# Read a raw C3D motion-capture file in base R.
#
# R port of 02_read_c3d.py. The Python script uses ezc3d; there is no
# dependency-free C3D reader on CRAN, so this file includes a minimal one
# (read_c3d_points below) that handles what the OpenBiomechanics files use:
# Intel byte order, floating-point point data (POINT:SCALE < 0). It reads marker
# labels, rate, units and 3D trajectories; analog channels are skipped. For
# anything broader (integer-scaled files, analog data, other byte orders) use
# ezc3d from Python, or the `c3dr` package.
#
# Sample files ship at baseball_pitching/data/c3d/000822/ ; the full set comes
# from scripts/download_data.sh.
#
# Run:
#   Rscript examples/02_read_c3d.R

source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))),
                 "obp_helpers.R"))

# ---- minimal C3D reader ----------------------------------------------------
read_c3d_points <- function(path) {
  raw <- readBin(path, "raw", n = file.size(path))
  i16 <- function(pos, signed = TRUE)   # 1-indexed byte position
    readBin(raw[pos:(pos + 1)], "integer", size = 2, signed = signed, endian = "little")
  f32 <- function(pos) readBin(raw[pos:(pos + 3)], "numeric", size = 4, endian = "little")
  i8  <- function(pos) readBin(raw[pos], "integer", size = 1, signed = TRUE)

  param_block <- as.integer(raw[1])
  if (as.integer(raw[2]) != 80) stop("not a C3D file: ", path)

  # ---- header ----
  n_points  <- i16(3, FALSE)
  first_fr  <- i16(7, FALSE)
  last_fr   <- i16(9, FALSE)
  scale     <- f32(13)
  data_blk  <- i16(17, FALSE)
  n_analog  <- i16(5, FALSE)            # analog values per frame (all samples)
  rate      <- f32(21)
  if (scale >= 0) stop("integer-scaled C3D is not supported by this minimal reader")

  # ---- parameter section: walk the linked list of groups/parameters ----
  pstart <- (param_block - 1) * 512
  if (as.integer(raw[pstart + 4]) != 84) stop("only Intel (little-endian) C3D is supported")
  pos <- pstart + 5                      # first entry (1-indexed), after 4-byte header
  groups <- list(); params <- list()
  repeat {
    name_len <- i8(pos); gid <- i8(pos + 1)
    if (name_len == 0) break
    n    <- abs(name_len)
    name <- rawToChar(raw[(pos + 2):(pos + 1 + n)])
    off_pos <- pos + 2 + n
    nxt  <- i16(off_pos)
    if (gid < 0) {
      groups[[as.character(abs(gid))]] <- name
    } else {
      p <- off_pos + 2
      type <- i8(p); nd <- as.integer(raw[p + 1])
      dims <- if (nd > 0) as.integer(raw[(p + 2):(p + 1 + nd)]) else integer(0)
      total <- if (nd > 0) prod(dims) else 1
      dpos  <- p + 2 + nd
      val <- if (type == -1 && nd > 0) {            # character array
        len <- dims[1]; cnt <- if (nd > 1) prod(dims[-1]) else 1
        vapply(seq_len(cnt), function(k)
          trimws(rawToChar(raw[(dpos + (k - 1) * len):(dpos + k * len - 1)])), "")
      } else if (type == 2) {
        vapply(seq_len(total), function(k) i16(dpos + 2 * (k - 1)), 0L)
      } else if (type == 4) {
        vapply(seq_len(total), function(k) f32(dpos + 4 * (k - 1)), 0)
      } else if (type == 1) {
        as.integer(raw[dpos:(dpos + total - 1)])
      } else NULL
      g <- groups[[as.character(gid)]]
      if (!is.null(g)) params[[paste(g, name, sep = ":")]] <- val
    }
    if (nxt == 0) break
    pos <- off_pos + nxt
  }

  # ---- point data ----
  n_frames <- last_fr - first_fr + 1
  fr_pts   <- params[["POINT:FRAMES"]]
  if (!is.null(fr_pts) && fr_pts > 0) n_frames <- fr_pts
  used   <- params[["POINT:USED"]]
  if (!is.null(used)) n_points <- used
  floats_per_frame <- n_points * 4 + n_analog
  dstart <- (data_blk - 1) * 512 + 1
  vals <- readBin(raw[dstart:length(raw)], "numeric", size = 4, endian = "little",
                  n = floats_per_frame * n_frames)
  m <- matrix(vals, nrow = floats_per_frame)[seq_len(n_points * 4), , drop = FALSE]
  arr <- array(m, dim = c(4, n_points, n_frames))   # (x,y,z,residual) x marker x frame

  list(labels = params[["POINT:LABELS"]][seq_len(n_points)],
       points = arr, rate = params[["POINT:RATE"]],
       units  = params[["POINT:UNITS"]])
}

# ---- locate a sample file --------------------------------------------------
# athlete C3Ds live in per-athlete subfolders; static model files hold no motion.
c3d_files <- sort(list.files(c3d_dir("pitching"), pattern = "\\.c3d$",
                             recursive = TRUE, full.names = TRUE))
c3d_files <- c3d_files[!grepl("model", basename(c3d_files))]
stopifnot("No sample C3D files found. Run scripts/download_data.sh." = length(c3d_files) > 0)

path <- c3d_files[1]
cat("Opening", sub(paste0(normalizePath(REPO_ROOT, winslash = "/"), "/"), "",
                   normalizePath(path, winslash = "/"), fixed = TRUE), "\n")

c3d      <- read_c3d_points(path)
labels   <- c3d$labels
points   <- c3d$points
n_frames <- dim(points)[3]
rate     <- if (is.null(c3d$rate)) NA else c3d$rate
units    <- if (is.null(c3d$units) || !nzchar(c3d$units[1])) "units" else c3d$units[1]

cat(sprintf("Markers: %d   Frames: %d   Rate: %.0f Hz\n", length(labels), n_frames, rate))
cat("Marker names:", paste(labels, collapse = ", "), "\n")

# Plot the throwing-hand-adjacent RWRA marker if present, else the first marker.
marker <- if ("RWRA" %in% labels) "RWRA" else labels[1]
idx <- match(marker, labels)
xs <- points[1, idx, ]; ys <- points[2, idx, ]; zs <- points[3, idx, ]

out <- file.path(figures_dir(), "02_marker_trajectory_r.png")
png(out, width = 840, height = 720, res = 120)
# Base R has no native 3D line plot: draw an empty perspective box, then project
# the trajectory onto it with trans3d().
pmat <- persp(range(xs), range(ys), matrix(min(zs), 2, 2), zlim = range(zs),
              theta = -50, phi = 25, ticktype = "detailed", nticks = 4,
              xlab = sprintf("X (%s)", units), ylab = sprintf("Y (%s)", units),
              zlab = sprintf("Z (%s)", units),
              main = sprintf("Marker '%s' trajectory  (%d frames @ %.0f Hz)", marker, n_frames, rate),
              col = NA, border = "grey80")
lines(trans3d(xs, ys, zs, pmat), lwd = 1.5, col = "steelblue")
points(trans3d(xs[1], ys[1], zs[1], pmat), pch = 16, col = "green3", cex = 1.4)
points(trans3d(xs[n_frames], ys[n_frames], zs[n_frames], pmat), pch = 16, col = "red", cex = 1.4)
legend("topright", c("start", "end"), pch = 16, col = c("green3", "red"), bty = "n")
dev.off()
cat(sprintf("Wrote %s\n", out))
