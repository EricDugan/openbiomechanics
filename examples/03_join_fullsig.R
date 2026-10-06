# Join two full-signal time-series tables on session_pitch + time.
#
# R port of 03_join_fullsig.py (base R only). The full-signal tables are large
# and NOT stored in git; fetch them with scripts/download_data.sh. They land as
# .zip archives in data/full_sig/ -- this script reads the zips directly.
#
# Marker-derived tables share the 360 Hz clock and join cleanly on
# session_pitch + time. (Force-plate data are 1,080 Hz, so joining those to
# marker data can drop rows -- avoided here by joining two marker-derived tables.)
#
# Run:
#   Rscript examples/03_join_fullsig.R

source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))),
                 "obp_helpers.R"))

angles <- read_full_sig("pitching", "joint_angles")
velos  <- read_full_sig("pitching", "joint_velos")

if (is.null(angles) || is.null(velos)) {
  cat("Full-signal tables not found locally.\n")
  cat("Expected joint_angles and joint_velos (.csv or .zip) in:\n")
  cat(sprintf("    %s\n\n", full_sig_dir("pitching")))
  cat("Fetch them first:\n    scripts/download_data.sh\n")
  quit(save = "no", status = 0)
}

cat(sprintf("joint_angles: %d x %d\n", nrow(angles), ncol(angles)))
cat(sprintf("joint_velos:  %d x %d\n", nrow(velos), ncol(velos)))

# joint_velos repeats the shared event-time columns; keep only the join keys
# plus its measurement columns so the merge doesn't duplicate them.
shared    <- intersect(names(angles), names(velos))
velo_cols <- c("session_pitch", "time", setdiff(names(velos), shared))

merged <- merge(angles, velos[, velo_cols], by = c("session_pitch", "time"))
cat(sprintf("merged:       %d x %d\n", nrow(merged), ncol(merged)))
stopifnot("row count changed -- unexpected time misalignment" = nrow(merged) == nrow(angles))

cat(sprintf("\nPitches represented: %d\n", length(unique(merged$session_pitch))))
cat(sprintf("Columns after join:  %d\n", ncol(merged)))
