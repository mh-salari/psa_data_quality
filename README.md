# The Effect of Pupil Size on Data Quality in Head-Mounted Eye Trackers

[![DOI](https://img.shields.io/badge/DOI-10.3758/s13428--025--02880--3-blue)](https://doi.org/10.3758/s13428-025-02880-3)
[![Paper](https://img.shields.io/badge/Paper-Behavior%20Research%20Methods-green)](https://rdcu.be/e49pe)

This repository contains the data, processing pipeline, analysis code, and experiment materials for the paper:

> Salari, M., Niehorster, D. C., Nystrom, M., & Bednarik, R. (2026). The effect of pupil size on data quality in head-mounted eye trackers. *Behavior Research Methods*, 58, 17. https://doi.org/10.3758/s13428-025-02880-3

## Repository Structure

```
psa_data_quality/
├── data/                       # Processed eye-tracking data
├── quality_metrics/            # Computed quality metrics and notebooks
├── analysis/                   # R scripts for statistical analysis and figures
├── process/                    # Python data processing pipeline
└── run_experiments/            # Experiment software and stimuli
```

## Data

### Per-Participant Data (`data/{participant_id}/{eye_tracker}/`)

Each participant directory contains data from the eye trackers they were recorded with. The eye trackers used in this study are:

- **EyeLink 1000 Plus** (desktop, reference device)
- **Pupil Core** (head-mounted)
- **Pupil Neon** (head-mounted)
- **SMI ETG 2w** (head-mounted)
- **Tobii Glasses 2** (head-mounted, available for 6 participants)

Each eye tracker directory contains:

| File | Description |
|------|-------------|
| `data.csv` | Final cleaned gaze data with positions converted to degrees of visual angle |
| `calibration.xml` | Camera calibration parameters (head-mounted trackers only) |
| `gazeData.tsv` | Raw gaze data in common format |
| `target.csv` | Detected target and corner reference point coordinates |
| `distance.csv` | Estimated viewing distance per frame |
| `undistorted.csv` | Gaze and target coordinates after lens distortion correction |
| `stabilized.csv` | Coordinates after head movement compensation |

The `data.csv` files contain the following columns: `eye_tracker`, `participant_id`, `trial_number`, `frame`, `trial_condition` (bright/dark), `target_x/y`, corner coordinates, `pup_diam_l/r` (pupil diameter left/right), `gaze_x/y`, `distance_average`, `gaze_angle_x/y`, `target_angle_x/y`, and `distance_to_target`.

### Aggregate Data Files (`data/`)

| File | Description |
|------|-------------|
| `pupil_size.csv` | Pupil diameter measurements across all participants and eye trackers |
| `hm_nan_statistics.csv` | Data loss statistics for head-mounted eye trackers |
| `eyelink1000plus_nan_statistics.csv` | Data loss statistics for EyeLink 1000 Plus |

## Quality Metrics (`quality_metrics/`)

Pre-computed quality metrics used by the analysis scripts:

| File | Description |
|------|-------------|
| `accuracy.csv` | Spatial accuracy per participant, trial, and condition |
| `apparent_gaze_shift.csv` | Apparent gaze shift (PSA magnitude) between bright and dark conditions |
| `rms_s2s.csv` | RMS sample-to-sample precision per trial |
| `std.csv` | Standard deviation precision per trial |
| `quality_metrics.ipynb` | Notebook that computes these metrics from `data.csv` files |

## Analysis (`analysis/`)

R scripts (R 4.4.2, tidyverse 2.0.0) that generate the figures and statistical results reported in the paper.

### Figures

| Script | Output | Paper Figure |
|--------|--------|-------------|
| `pupil_size_line_plot.R` | `output/participant_pupil_diameter_changes_bright_to_dark.png` | Figure 2 |
| `apparent_gaze_shift.R` | `output/apparent_gaze_shift.png` | Figure 3 |
| `accuracy_line_plot.R` | `output/participant_accuracy_changes_bright_to_dark.png` | Figure 4 |
| `std_line_plot.R` | `output/participant_std_changes_bright_to_dark.png` | Figure 5 |
| `rms_s2s_line_plot.R` | `output/participant_rms_s2s_changes_bright_to_dark.png` | Figure 6 |
| `data_loss_line_plot.R` | `output/participant_data_loss_changes_bright_to_dark.png` | Figure 7 |

### Statistical Analysis

| Script | Output |
|--------|--------|
| `accuracy_statistics.R` | `output/accuracy_descriptive_stats.csv`, `output/accuracy_statistical_analysis.csv` |
| `apparent_gaze_shift_statistics.R` | `output/apparent_gaze_shift_descriptive_stats.csv`, `output/apparent_gaze_shift_statistical_analysis.csv` |
| `rms_s2s_statistics.R` | `output/rms_s2s_descriptive_stats.csv`, `output/rms_s2s_statistical_analysis.csv` |
| `std_statistics.R` | `output/std_descriptive_stats.csv`, `output/std_statistical_analysis.csv` |
| `data_loss_statistics.R` | `output/data_loss_descriptive_stats.csv`, `output/data_loss_statistical_analysis.csv` |
| `pupil_size_statistics.R` | `output/pupil_size_descriptive_stats.csv` |

### Additional Plots

| Script | Output | Description |
|--------|--------|-------------|
| `accuracy.R` | `output/accuracy.png` | Accuracy box plot |
| `pupil_size.R` | `output/pupil_size.png` | Pupil size box plot |
| `rms_s2s.R` | `output/rms_s2s.png` | RMS-S2S box plot |
| `std.R` | `output/std.png` | STD box plot |
| `data_loss.R` | `output/data_loss_table.csv` | Data loss summary table |

## Data Processing Pipeline (`process/`)

Python scripts that transform raw eye-tracker recordings into the processed `data.csv` files. The pipeline runs in the following order:

1. **`hm_to_common_format.py`** -- Converts head-mounted eye tracker recordings to a common data format using [glassesTools](https://github.com/dcnieho/glassesTools)
2. **`target_detection/example.ipynb`** -- Detects target and corner reference points in scene camera video
3. **`hm_distance.py`** -- Estimates viewing distance using the pinhole camera model
4. **`hm_nan_undistort_stabilize.py`** -- Removes invalid samples, undistorts coordinates using camera calibration, and compensates for head movements
5. **`hm_to_deg.py`** -- Converts gaze coordinates to degrees of visual angle, applies time trimming (middle 75%), distance filtering (< 10 deg), and z-score outlier removal (z > 3)
6. **`eyelink1000plus_data_processor.py`** -- Processes EyeLink 1000 Plus data from raw recordings with the same filtering steps

## Experiment Software (`run_experiments/`)

Software used to run the experiment:

| File | Description |
|------|-------------|
| `display_stimulus.py` | Python script for stimulus presentation (head-mounted trackers) |
| `calibration.py` | Calibration target display script |
| `utils/generate_visual_stimulus.py` | Generates the visual stimuli (bright/dark backgrounds with target) |
| `utils/visual_angle_converter.py` | Visual angle conversion utilities |
| `resources/` | Stimulus images (bright/dark backgrounds, target, instruction pages) |
| `eyelink1000plus/display_stimulus_deploy/` | Compiled EyeLink experiment (Experiment Builder) |

## Citation

If you use this data or code, please cite:

```bibtex
@article{salari2025,
  title = {The Effect of Pupil Size on Data Quality in Head-Mounted Eye Trackers},
  author = {Salari, Mohammadhossein and Niehorster, Diederick C. and Nyström, Marcus and Bednarik, Roman},
  date = {2025-12-03},
  journaltitle = {Behavior Research Methods},
  volume = {58},
  number = {1},
  pages = {17},
  issn = {1554-3528},
  doi = {10.3758/s13428-025-02880-3}
}
```

## License

See [LICENSE](LICENSE) for details.
