# Shoulder/elbow load vs. pitch velocity, for benchmarking against another cohort.
#
# R port of examples/05_benchmark_shoulder_load.py (base R only, no packages).
#
# Builds a per-pitcher table of the Tier A variables used to benchmark an
# external study against OBP: peak shoulder internal-rotation moment, peak elbow
# varus moment, mean pitch velocity, and demographics.
#
# Definitions
# -----------
# * Window: start event -> ball release (BR_time). --start chooses the start
#   event: fp_10 = foot contact (10% BW, first contact; default) or
#   fp_100 = foot plant (100% BW). Pitches whose ball release precedes the start
#   event (or lack one) are dropped as bad tags.
# * POI shoulder_internal_rotation_moment is the peak of
#   shoulder_upper_arm_moment_z (upper-arm long-axis frame). The trunk-frame
#   equivalent (shoulder_thorax_moment_z) is also computed; the two are NOT
#   interchangeable (r ~ 0.63).
# * Moments are normalised as 100 * Nm / (mass_kg * 9.81 * height_m) -> %BW.H.
# * Per-pitch values are averaged within pitcher before summarising.
#
# Needs forces_moments in baseball_pitching/data/full_sig/ (scripts/download_data.sh).
#
# Run (from anywhere):
#   Rscript examples/05_benchmark_shoulder_load.R
#   Rscript examples/05_benchmark_shoulder_load.R --out pitcher_level.csv
#   Rscript examples/05_benchmark_shoulder_load.R --start fp_100 --min-mph 80 --max-mph 90

G     <- 9.81
BANDS <- c(0, 75, 80, 85, 90, 200)

# ---- args ------------------------------------------------------------------
args  <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1]
}
start   <- get_arg("--start", "fp_10")
out     <- get_arg("--out")
min_mph <- as.numeric(get_arg("--min-mph", NA))
max_mph <- as.numeric(get_arg("--max-mph", NA))
stopifnot(start %in% c("fp_10", "fp_100"))

# ---- paths -----------------------------------------------------------------
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
here <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg)))
} else {
  normalizePath("examples")
}
root <- dirname(here)
data_dir <- file.path(root, "baseball_pitching", "data")
fs_dir   <- file.path(data_dir, "full_sig")

# ---- helpers ---------------------------------------------------------------
read_cols <- function(csv, cols) {
  hdr <- names(read.csv(csv, nrows = 1, check.names = FALSE))
  cc  <- ifelse(hdr %in% cols, NA, "NULL")
  read.csv(csv, colClasses = cc, check.names = FALSE)
}

find_table <- function(name) {
  csv <- file.path(fs_dir, paste0(name, ".csv"))
  if (file.exists(csv)) return(csv)
  zip <- file.path(fs_dir, paste0(name, ".zip"))
  if (file.exists(zip)) {
    exdir <- file.path(tempdir(), name)
    unzip(zip, exdir = exdir)
    return(file.path(exdir, paste0(name, ".csv")))
  }
  cat(name, "not found in", fs_dir, "\nFetch it first:\n    scripts/download_data.sh\n")
  quit(save = "no", status = 0)
}

# Peak shoulder IR moment (thorax and upper-arm frame) per pitch, start -> BR.
trunk_frame_peaks <- function(csv, start) {
  start_col <- paste0(start, "_time")
  fm <- read_cols(csv, c("session_pitch", "time", start_col, "BR_time",
                         "shoulder_thorax_moment_z", "shoulder_upper_arm_moment_z"))
  ev  <- fm[!duplicated(fm$session_pitch), c("session_pitch", start_col, "BR_time")]
  bad <- ev$session_pitch[is.na(ev[[start_col]]) | ev$BR_time <= ev[[start_col]]]
  fm  <- fm[!fm$session_pitch %in% bad, ]
  fm  <- fm[fm$time >= fm[[start_col]] & fm$time <= fm$BR_time, ]
  data.frame(
    session_pitch       = sort(unique(fm$session_pitch)),
    shoulder_ir_trunk_nm = as.numeric(tapply(fm$shoulder_thorax_moment_z,
                                             fm$session_pitch, max)),
    shoulder_ir_uarm_nm  = as.numeric(tapply(fm$shoulder_upper_arm_moment_z,
                                             fm$session_pitch, max))
  )
}

# ---- load + join -----------------------------------------------------------
poi  <- read.csv(file.path(data_dir, "poi", "poi_metrics.csv"))
meta <- read.csv(file.path(data_dir, "metadata.csv"))
peaks <- trunk_frame_peaks(find_table("forces_moments"), start)

pitch <- merge(
  poi[, c("session_pitch", "session", "p_throws", "pitch_speed_mph",
          "shoulder_internal_rotation_moment", "elbow_varus_moment")],
  meta[, c("session_pitch", "user", "session_mass_kg", "session_height_m",
           "age_yrs", "playing_level")],
  by = "session_pitch")
pitch <- merge(pitch, peaks, by = "session_pitch", all.x = TRUE)

chk <- pitch[complete.cases(pitch[, c("shoulder_internal_rotation_moment",
                                      "shoulder_ir_uarm_nm")]), ]
cat(sprintf("POI vs recomputed upper-arm peak: r = %.4f  (n=%d)\n",
            cor(chk$shoulder_internal_rotation_moment, chk$shoulder_ir_uarm_nm), nrow(chk)))
cat("Window start:", start, "\n")
cat(sprintf("Pitches w/o a usable event window (no trunk-frame value): %d of %d\n\n",
            sum(is.na(pitch$shoulder_ir_trunk_nm)), nrow(pitch)))

norm <- 100 / (pitch$session_mass_kg * G * pitch$session_height_m)
pitch$shoulder_ir_poi_bwh   <- pitch$shoulder_internal_rotation_moment * norm
pitch$shoulder_ir_trunk_bwh <- pitch$shoulder_ir_trunk_nm * norm
pitch$elbow_varus_bwh       <- pitch$elbow_varus_moment * norm

# ---- one row per pitcher-session ------------------------------------------
means <- aggregate(
  cbind(age_yr = age_yrs, height_m = session_height_m, mass_kg = session_mass_kg,
        velo_mph = pitch_speed_mph,
        shoulder_ir_poi_nm = shoulder_internal_rotation_moment,
        shoulder_ir_poi_bwh, shoulder_ir_trunk_bwh, elbow_varus_bwh) ~ user + session,
  data = pitch, FUN = function(x) mean(x, na.rm = TRUE), na.action = na.pass)
info <- aggregate(session_pitch ~ user + session, data = pitch, FUN = length)
names(info)[3] <- "n_pitches"
lvl  <- pitch[!duplicated(pitch[, c("user", "session")]),
              c("user", "session", "p_throws", "playing_level")]
names(lvl)[3:4] <- c("throws", "level")
pitcher <- Reduce(function(a, b) merge(a, b, by = c("user", "session")),
                  list(info, lvl, means))

if (!is.na(min_mph)) pitcher <- pitcher[pitcher$velo_mph >= min_mph, ]
if (!is.na(max_mph)) pitcher <- pitcher[pitcher$velo_mph <= max_mph, ]

cat(sprintf("Pitcher-sessions: %d  (pitchers: %d, pitches: %d)\n",
            nrow(pitcher), length(unique(pitcher$user)), sum(pitcher$n_pitches)))

cat("\nCohort:\n")
vars <- c("age_yr", "height_m", "mass_kg", "velo_mph")
print(round(sapply(pitcher[vars], function(x)
  c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))), 2))

cat("\nBy playing level:\n")
lv <- aggregate(cbind(age = age_yr, velo = velo_mph) ~ level, pitcher, mean)
lv$n <- as.integer(table(pitcher$level)[lv$level])
print(format(lv[, c("level", "n", "age", "velo")], digits = 3), row.names = FALSE)

outcomes <- c("shoulder_ir_poi_bwh", "shoulder_ir_trunk_bwh", "elbow_varus_bwh")
pitcher$velo_band <- cut(pitcher$velo_mph, BANDS, right = FALSE)

cat("\nMean load by velocity band (%BW.H):\n")
band <- do.call(rbind, lapply(split(pitcher, pitcher$velo_band), function(d)
  if (nrow(d)) c(n = nrow(d), sapply(d[outcomes], mean, na.rm = TRUE))))
print(round(band, 2))

cat("\nLoad vs. velocity (per pitcher; slope in %BW.H per mph):\n")
for (col in outcomes) {
  d   <- pitcher[complete.cases(pitcher[, c("velo_mph", col)]), ]
  fit <- lm(d[[col]] ~ d$velo_mph)
  cat(sprintf("  %-24s slope=%6.3f  intercept=%7.2f  r=%5.2f  n=%d\n",
              col, coef(fit)[2], coef(fit)[1], cor(d$velo_mph, d[[col]]), nrow(d)))
}

if (!is.null(out)) {
  write.csv(pitcher[, setdiff(names(pitcher), "velo_band")], out, row.names = FALSE)
  cat("\nWrote", out, "\n")
}
