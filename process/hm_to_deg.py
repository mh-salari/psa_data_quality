"""
Author:       Mohammadhossein Salari
Email:        mohammadhossein.salari@gmail.com
Last Modified: 2024/02/17
Description: This script converts eye-tracking coordinates to visual angles and
             performs data cleaning. The script calculates scaling factors based on physical targets'
             dimensions, transforms pixel coordinates to visual angles, and performs outlier removal
             using time-based trimming, distance thresholds, and z-score filtering.

Input structure:
 data/
 ├── participant1/
 │   ├── Pupil Core/
 │   │   ├── stabilized.csv
 │   │   └── distance.csv
 │   ├── SMI ETG/
 │   │   ├── stabilized.csv
 │   │   └── distance.csv
 │   └── ...
 ├── participant2/
 │   └── ...
 └── ...
Output structure:
 data/
 ├── participant1/
 │   ├── Pupil Core/
 │   │   └── data.csv
 │   ├── SMI ETG/
 │   │   └── data.csv
 │   └── ...
 ├── participant2/
 │   └── ...
 └── ...
"""

from pathlib import Path
from tqdm import tqdm

import numpy as np
import pandas as pd
from scipy import stats


def calculate_scaling_factor(row, real_width_mm, real_height_mm):
    """
    Calculate scaling factor (mm/pixel) using all four edges of the screen
    """
    # Calculate pixel distances for each edge using adjusted coordinates
    top_width_px = np.sqrt(
        (row["top_right_x"] - row["top_left_x"]) ** 2
        + (row["top_right_y"] - row["top_left_y"]) ** 2
    )

    bottom_width_px = np.sqrt(
        (row["bottom_right_x"] - row["bottom_left_x"]) ** 2
        + (row["bottom_right_y"] - row["bottom_left_y"]) ** 2
    )

    left_height_px = np.sqrt(
        (row["bottom_left_x"] - row["top_left_x"]) ** 2
        + (row["bottom_left_y"] - row["top_left_y"]) ** 2
    )

    right_height_px = np.sqrt(
        (row["bottom_right_x"] - row["top_right_x"]) ** 2
        + (row["bottom_right_y"] - row["top_right_y"]) ** 2
    )

    # Calculate scaling factors from each edge (mm/pixel)
    scale_top = real_width_mm / top_width_px
    scale_bottom = real_width_mm / bottom_width_px
    scale_left = real_height_mm / left_height_px
    scale_right = real_height_mm / right_height_px

    # Calculate average scaling factor
    scaling_factors = [scale_top, scale_bottom, scale_left, scale_right]
    average_scale = np.mean(scaling_factors)

    return average_scale


def convert_to_visual_angles(df):
    """
    Convert adjusted gaze and target coordinates to visual angles using scaling approach
    """

    # Initialize lists for results
    gaze_angles_x = []
    gaze_angles_y = []
    target_angles_x = []
    target_angles_y = []

    for idx, row in df.iterrows():

        eye_tracker = row["eye_tracker"]

        # Calculate scaling factor for this frame
        if eye_tracker == "Pupil Core":
            if (
                row["participant_id"] in [319, 460, 503, 772, 844]
                and row["trial_condition"] == "bright"
            ):
                real_width_mm = 476.64
                real_height_mm = 268.11
            else:
                real_width_mm = 346.31
                real_height_mm = 137.78
        elif eye_tracker == "Tobii Glasses 2":
            if row["trial_condition"] == "dark":
                real_width_mm = 346.31
                real_height_mm = 137.78
            else:
                real_width_mm = 476.64
                real_height_mm = 268.11
        elif eye_tracker == "SMI ETG" or eye_tracker == "Pupil Neon":
            real_width_mm = 346.31
            real_height_mm = 137.78
        else:
            print("Undifined eye tracker")

        scale = calculate_scaling_factor(row, real_width_mm, real_height_mm)  # mm/pixel

        # Find screen center
        center_x, center_y = row["target_x"], row["target_y"]

        # Convert gaze coordinates using adjusted values
        gaze_x_mm = (float(row["gaze_x"]) - center_x) * scale
        gaze_y_mm = (float(row["gaze_y"]) - center_y) * scale

        target_x_mm = (float(row["target_x"]) - center_x) * scale
        target_y_mm = (float(row["target_y"]) - center_y) * scale

        # Convert to visual angles (arctan of opposite/adjacent)
        distance_mm = float(row["distance_average"])

        gaze_angle_x = np.degrees(np.arctan(gaze_x_mm / distance_mm))
        gaze_angle_y = np.degrees(np.arctan(gaze_y_mm / distance_mm))

        target_angle_x = np.degrees(np.arctan(target_x_mm / distance_mm))
        target_angle_y = np.degrees(np.arctan(target_y_mm / distance_mm))

        # Store results
        gaze_angles_x.append(gaze_angle_x)
        gaze_angles_y.append(gaze_angle_y)
        target_angles_x.append(target_angle_x)
        target_angles_y.append(target_angle_y)

    # Add results to dataframe
    df["gaze_angle_x"] = gaze_angles_x
    df["gaze_angle_y"] = gaze_angles_y
    df["target_angle_x"] = target_angles_x
    df["target_angle_y"] = target_angles_y

    return df


def calculate_euclidean_distance(row):
    return np.sqrt(
        (row["gaze_angle_x"] - row["target_angle_x"]) ** 2
        + (row["gaze_angle_y"] - row["target_angle_y"]) ** 2
    )


def clean_trials(
    df,
    start_threshold=25 / 2,
    end_threshold=25 / 2,
    z_threshold=3,
    distance_threshold=10,
):
    df_cleaned = pd.DataFrame()
    for trial in df["trial_number"].unique():
        trial_data = df[df["trial_number"] == trial].copy()

        # Time trimming
        start_idx = trial_data.index.min()
        end_idx = trial_data.index.max()
        total_indices = end_idx - start_idx

        keep_start_idx = start_idx + int(total_indices * (start_threshold / 100))
        keep_end_idx = end_idx - int(total_indices * (end_threshold / 100))

        time_mask = (trial_data.index >= keep_start_idx) & (
            trial_data.index <= keep_end_idx
        )
        time_trimmed_data = trial_data[time_mask].copy()  # Adding explicit .copy()

        # Distance-based filtering using your existing function
        time_trimmed_data.loc[:, "distance_to_target"] = time_trimmed_data.apply(
            calculate_euclidean_distance, axis=1
        )
        distance_mask = time_trimmed_data["distance_to_target"] <= distance_threshold
        distance_filtered = time_trimmed_data[distance_mask]

        # Z-score filtering
        columns_to_check = ["gaze_angle_x", "gaze_angle_y"]
        valid_mask = pd.Series(True, index=distance_filtered.index)

        for col in columns_to_check:
            z_scores = np.abs(stats.zscore(distance_filtered[col], nan_policy="omit"))
            valid_mask &= z_scores < z_threshold

        cleaned_trial_data = distance_filtered[valid_mask]
        df_cleaned = pd.concat([df_cleaned, cleaned_trial_data])

    return df_cleaned


def main():

    dataset_dir_path = Path(__file__).resolve().parent.parent / "data"

    # Get all eye trackers data directories
    eye_trackers = ["Pupil Core", "SMI ETG", "Pupil Neon", "Tobii Glasses 2"]
    data_dirs = []
    for participant_dir in dataset_dir_path.iterdir():
        if participant_dir.is_dir():
            for eye_tracker in eye_trackers:

                data_path = participant_dir / eye_tracker
                if data_path.exists():
                    data_dirs.append(data_path)
    data_dirs[:3]

    for data_dir in tqdm(data_dirs[:]):

        df = pd.read_csv(data_dir / "stabilized.csv")
        distance_df = pd.read_csv(data_dir / "distance.csv")
        if len(df) == len(distance_df) and df["frame"].equals(distance_df["frame"]):
            df["distance_average"] = distance_df["distance_average"]
        df = convert_to_visual_angles(df)

        clean_df = clean_trials(df)
        clean_df.to_csv(data_dir / "data.csv", index=False)


if __name__ == "__main__":
    main()
