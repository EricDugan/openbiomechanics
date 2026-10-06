# Explore the high-performance (force-plate assessment) table.
#
# R port of 04_hp_assessment.py (base R only). Loads high_performance/data/
# hp_obp.csv and plots the distribution of countermovement-jump (CMJ) jump
# height across athletes -- a standard lower-body power screen.
#
# Run:
#   Rscript examples/04_hp_assessment.R

source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))),
                 "obp_helpers.R"))

hp <- load_hp()
cat(sprintf("HP table: %d assessments x %d columns\n", nrow(hp), ncol(hp)))

# Force-plate metrics are suffixed by test: _cmj (countermovement jump),
# _sj (squat jump), _imtp (isometric mid-thigh pull), etc.
metric <- "jump_height_(imp-mom)_[cm]_mean_cmj"
vals <- hp[[metric]]
vals <- vals[!is.na(vals)]

cat(sprintf("\n%s\n", metric))
cat(sprintf("  n=%d  mean=%.1f cm  sd=%.1f  min=%.1f  max=%.1f\n",
            length(vals), mean(vals), sd(vals), min(vals), max(vals)))
cat("\nAssessments by playing level:\n")
print(sort(table(hp$playing_level), decreasing = TRUE))

out <- file.path(figures_dir(), "04_cmj_jump_height_r.png")
png(out, width = 840, height = 600, res = 120)
hist(vals, breaks = 20, col = "steelblue", border = "white",
     xlab = "CMJ jump height (cm)", ylab = "Athletes",
     main = sprintf("Countermovement-jump height distribution  (n=%d)", length(vals)))
abline(v = mean(vals), col = "red", lty = 2, lwd = 2)
legend("topright", legend = sprintf("mean %.1f cm", mean(vals)),
       col = "red", lty = 2, lwd = 2, bty = "n")
dev.off()
cat(sprintf("\nWrote %s\n", out))
