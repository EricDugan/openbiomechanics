# Explore the pitching point-of-interest (POI) metrics.
#
# R port of 01_explore_poi.py (base R only). Loads the per-pitch POI table and
# the session/athlete metadata, prints a few summary statistics, and saves a
# scatter plot of pitch velocity against peak elbow varus moment.
#
# Run:
#   Rscript examples/01_explore_poi.R

source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))),
                 "obp_helpers.R"))

poi  <- load_poi("pitching")
meta <- load_metadata("pitching")

cat(sprintf("POI table:      %d pitches x %d metrics\n", nrow(poi), ncol(poi)))
cat(sprintf("Metadata table: %d pitches x %d columns\n", nrow(meta), ncol(meta)))

# Every pitch is keyed by session_pitch; both tables share it.
cat("\nPitch types thrown:\n")
print(sort(table(poi$pitch_type), decreasing = TRUE))
cat(sprintf("\nPitch speed (mph): mean=%.1f min=%.1f max=%.1f\n",
            mean(poi$pitch_speed_mph), min(poi$pitch_speed_mph), max(poi$pitch_speed_mph)))
cat(sprintf("Elbow varus moment (Nm): mean=%.1f max=%.1f\n",
            mean(poi$elbow_varus_moment), max(poi$elbow_varus_moment)))

# Correlation of the two headline metrics.
r <- cor(poi$pitch_speed_mph, poi$elbow_varus_moment, use = "complete.obs")
cat(sprintf("\nPearson r (pitch_speed_mph vs elbow_varus_moment): %.3f\n", r))

out <- file.path(figures_dir(), "01_speed_vs_varus_moment_r.png")
png(out, width = 840, height = 600, res = 120)
plot(poi$pitch_speed_mph, poi$elbow_varus_moment, pch = 16, cex = 0.6,
     col = adjustcolor("steelblue", 0.5),
     xlab = "Pitch speed (mph)", ylab = "Peak elbow varus moment (Nm)",
     main = sprintf("Arm stress vs. velocity  (n=%d, r=%.2f)", nrow(poi), r))
dev.off()
cat(sprintf("\nWrote %s\n", out))
