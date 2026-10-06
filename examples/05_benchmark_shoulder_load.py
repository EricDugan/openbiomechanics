"""Shoulder/elbow load vs. pitch velocity, for benchmarking against another cohort.

Builds a per-pitcher table of the Tier A variables used to benchmark an external
study against OBP: peak shoulder internal-rotation moment, peak elbow varus
moment, mean pitch velocity, and demographics.

Notes on definitions
--------------------
* Window: start event -> ball release (``BR_time``). The start event is chosen
  with ``--start``: ``fp_10`` = foot contact (10% BW, first contact; default,
  closest to "stride foot contact") or ``fp_100`` = foot plant (100% BW).
  Pitches whose ball release precedes the start event are dropped as bad tags.
* The POI ``shoulder_internal_rotation_moment`` is the peak of
  ``shoulder_upper_arm_moment_z`` (upper-arm long-axis frame; + = internal rotation).
* ``shoulder_thorax_moment_z`` is NOT internal rotation: per the pitching README,
  the thorax-frame z moment is shoulder HORIZONTAL ADDUCTION (+) / abduction (-)
  (r ~ 0.63 with the IR moment). It is reported here as ``shoulder_hadd_thorax``.
* Moments are normalised as 100 * Nm / (mass_kg * 9.81 * height_m) -> %BW.H.
* Per-pitch values are averaged within pitcher before summarising.

Needs ``forces_moments`` in data/full_sig/ (obp.download(), or
scripts/download_data.sh). pandas reads the .zip directly.

Run:
    python3 examples/05_benchmark_shoulder_load.py
    python3 examples/05_benchmark_shoulder_load.py --out pitcher_level.csv
    python3 examples/05_benchmark_shoulder_load.py --min-mph 80 --max-mph 90
"""

import argparse
import sys
import pathlib

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[1]))

import numpy as np
import pandas as pd

import obp
from obp import full_sig_dir

G = 9.81
BANDS = [0, 75, 80, 85, 90, 200]


def find_table(name):
    fs = full_sig_dir("pitching")
    for p in (fs / f"{name}.csv", fs / f"{name}.zip"):
        if p.exists():
            return p
    print(f"{name} not found in {fs}. Fetch it first:")
    print("    scripts/download_data.sh   # or: python3 -c 'import obp; obp.download()'")
    sys.exit(0)


def trunk_frame_peaks(path, start):
    """Peak shoulder IR (upper-arm z) and horizontal-adduction (thorax z) moments per pitch."""
    start_col = f"{start}_time"
    cols = ["session_pitch", "time", start_col, "BR_time",
            "shoulder_thorax_moment_z", "shoulder_upper_arm_moment_z"]
    fm = pd.read_csv(path, usecols=cols)
    bad = fm.groupby("session_pitch")[[start_col, "BR_time"]].first()
    bad = bad.index[(bad["BR_time"] <= bad[start_col]) | bad[start_col].isna()]
    fm = fm[~fm["session_pitch"].isin(bad)]
    in_win = (fm["time"] >= fm[start_col]) & (fm["time"] <= fm["BR_time"])
    peaks = (
        fm[in_win]
        .groupby("session_pitch")[["shoulder_thorax_moment_z",
                                   "shoulder_upper_arm_moment_z"]]
        .max()
        .rename(columns={"shoulder_thorax_moment_z": "shoulder_hadd_thorax_nm",
                         "shoulder_upper_arm_moment_z": "shoulder_ir_uarm_nm"})
    )
    return peaks.reset_index()


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--out", help="write the per-pitcher table to this CSV")
    ap.add_argument("--start", choices=["fp_10", "fp_100"], default="fp_10",
                    help="window start event: fp_10 = foot contact (default), "
                         "fp_100 = foot plant")
    ap.add_argument("--min-mph", type=float, default=None,
                    help="keep pitchers with mean velocity >= this")
    ap.add_argument("--max-mph", type=float, default=None,
                    help="keep pitchers with mean velocity <= this")
    args = ap.parse_args()

    poi = obp.load_poi("pitching")
    meta = obp.load_metadata("pitching")
    peaks = trunk_frame_peaks(find_table("forces_moments"), args.start)

    pitch = (
        poi[["session_pitch", "session", "p_throws", "pitch_speed_mph",
             "shoulder_internal_rotation_moment", "elbow_varus_moment"]]
        .merge(meta[["session_pitch", "user", "session_mass_kg",
                     "session_height_m", "age_yrs", "playing_level"]],
               on="session_pitch", how="inner")
        .merge(peaks, on="session_pitch", how="left")
    )

    # Sanity check: recomputed upper-arm peak should reproduce the POI column.
    chk = pitch[["shoulder_internal_rotation_moment", "shoulder_ir_uarm_nm"]].dropna()
    print(f"POI vs recomputed upper-arm peak: r = "
          f"{chk.corr().iloc[0, 1]:.4f}  (n={len(chk)})")
    print(f"Window start: {args.start}")
    print(f"Pitches w/o a usable event window (no trunk-frame value): "
          f"{pitch['shoulder_hadd_thorax_nm'].isna().sum()} of {len(pitch)}\n")

    norm = 100.0 / (pitch["session_mass_kg"] * G * pitch["session_height_m"])
    pitch["shoulder_ir_poi_bwh"] = pitch["shoulder_internal_rotation_moment"] * norm
    pitch["shoulder_hadd_thorax_bwh"] = pitch["shoulder_hadd_thorax_nm"] * norm
    pitch["elbow_varus_bwh"] = pitch["elbow_varus_moment"] * norm

    # One row per pitcher-session: average throws within pitcher first.
    pitcher = (
        pitch.groupby(["user", "session"])
        .agg(n_pitches=("session_pitch", "size"),
             throws=("p_throws", "first"),
             level=("playing_level", "first"),
             age_yr=("age_yrs", "first"),
             height_m=("session_height_m", "first"),
             mass_kg=("session_mass_kg", "first"),
             velo_mph=("pitch_speed_mph", "mean"),
             shoulder_ir_poi_nm=("shoulder_internal_rotation_moment", "mean"),
             shoulder_ir_poi_bwh=("shoulder_ir_poi_bwh", "mean"),
             shoulder_hadd_thorax_bwh=("shoulder_hadd_thorax_bwh", "mean"),
             elbow_varus_bwh=("elbow_varus_bwh", "mean"))
        .reset_index()
    )
    if args.min_mph is not None:
        pitcher = pitcher[pitcher["velo_mph"] >= args.min_mph]
    if args.max_mph is not None:
        pitcher = pitcher[pitcher["velo_mph"] <= args.max_mph]

    print(f"Pitcher-sessions: {len(pitcher)}  "
          f"(pitchers: {pitcher['user'].nunique()}, pitches: {pitcher['n_pitches'].sum()})")
    print("\nCohort:")
    print(pitcher[["age_yr", "height_m", "mass_kg", "velo_mph"]]
          .agg(["mean", "std", "min", "max"]).round(2).to_string())
    print("\nBy playing level:")
    print(pitcher.groupby("level")
          .agg(n=("user", "size"), age=("age_yr", "mean"),
               velo=("velo_mph", "mean")).round(1).to_string())

    outcomes = ["shoulder_ir_poi_bwh", "shoulder_hadd_thorax_bwh", "elbow_varus_bwh"]
    pitcher["velo_band"] = pd.cut(pitcher["velo_mph"], BANDS, right=False)
    print("\nMean load by velocity band (%BW.H):")
    print(pitcher.groupby("velo_band", observed=True)[outcomes]
          .agg(["count", "mean"]).round(2).to_string())

    print("\nLoad vs. velocity (per pitcher; slope in %BW.H per mph):")
    for col in outcomes:
        d = pitcher[["velo_mph", col]].dropna()
        slope, intercept = np.polyfit(d["velo_mph"], d[col], 1)
        r = d.corr().iloc[0, 1]
        print(f"  {col:<24} slope={slope:6.3f}  intercept={intercept:7.2f}  "
              f"r={r:5.2f}  n={len(d)}")

    if args.out:
        pitcher.drop(columns="velo_band").to_csv(args.out, index=False)
        print(f"\nWrote {args.out}")


if __name__ == "__main__":
    main()
