# Minimal R counterpart to the `obp` Python loader package (base R only).
#
# Each example script sources this file, then calls the loaders below. All paths
# resolve from the repo root, so scripts run from any working directory.

obp_script_dir <- function() {
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(f)) dirname(normalizePath(sub("^--file=", "", f))) else normalizePath("examples")
}

REPO_ROOT   <- dirname(obp_script_dir())
DISCIPLINES <- c(pitching = "baseball_pitching", hitting = "baseball_hitting")

.folder <- function(discipline) file.path(REPO_ROOT, DISCIPLINES[[discipline]])

load_poi      <- function(discipline) read.csv(file.path(.folder(discipline), "data", "poi", "poi_metrics.csv"))
load_metadata <- function(discipline) read.csv(file.path(.folder(discipline), "data", "metadata.csv"))
load_hittrax  <- function() read.csv(file.path(REPO_ROOT, "baseball_hitting", "data", "poi", "hittrax.csv"))
load_hp       <- function() read.csv(file.path(REPO_ROOT, "high_performance", "data", "hp_obp.csv"),
                                     check.names = FALSE)
c3d_dir       <- function(discipline) file.path(.folder(discipline), "data", "c3d")
full_sig_dir  <- function(discipline) file.path(.folder(discipline), "data", "full_sig")

# Read one full-signal table, from <name>.csv or <name>.zip (unzipped to tempdir).
# `cols` optionally restricts columns (far faster/lighter for the big tables).
# Returns NULL if the table has not been downloaded.
read_full_sig <- function(discipline, name, cols = NULL) {
  dir <- full_sig_dir(discipline)
  csv <- file.path(dir, paste0(name, ".csv"))
  zip <- file.path(dir, paste0(name, ".zip"))
  if (!file.exists(csv) && file.exists(zip)) {
    exdir <- file.path(tempdir(), name)
    unzip(zip, exdir = exdir)
    csv <- file.path(exdir, paste0(name, ".csv"))
  }
  if (!file.exists(csv)) return(NULL)
  cc <- NA
  if (!is.null(cols)) {
    hdr <- names(read.csv(csv, nrows = 1, check.names = FALSE))
    cc  <- ifelse(hdr %in% cols, NA, "NULL")
  }
  read.csv(csv, colClasses = cc, check.names = FALSE)
}

# Directory for figures (created on demand).
figures_dir <- function() {
  d <- file.path(obp_script_dir(), "figures")
  dir.create(d, showWarnings = FALSE)
  d
}
