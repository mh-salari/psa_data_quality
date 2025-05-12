#!/usr/bin/env python3

import os
from pathlib import Path
from tqdm import tqdm

import numpy as np
import pandas as pd
from scipy import stats

# Constants for pupil size conversion
EYELINK_REFERENCE_VALUE = 11815
EYELINK_REFERENCE_DIAMETER_MM = 15


def load_and_preprocess_data(data_path):
    # Load raw data
    raw_df = pd.read_csv(
        data_path,
        sep="\t",
        encoding="utf-16",
        na_values=["."],
        dtype={"RECORDING_SESSION_LABEL": str},
    )

    # Calculate time from trial start
    min_timestamp = raw_df.groupby(["RECORDING_SESSION_LABEL", "TRIAL_INDEX"])[
        "TIMESTAMP"
    ].transform("min")
    raw_df["TIME_FROM_TRIAL_START_MS"] = raw_df["TIMESTAMP"] - min_timestamp

    # Clean participant IDs to only include digits
    raw_df["RECORDING_SESSION_LABEL"] = raw_df["RECORDING_SESSION_LABEL"].apply(
        lambda x: "".join(filter(str.isdigit, str(x)))
    )

    # Rename raw data into common data format
    processed_df = pd.DataFrame()
    processed_df["eye_tracker"] = ["EyeLink 1000 Plus"] * len(raw_df)
    processed_df["participant_id"] = raw_df["RECORDING_SESSION_LABEL"]
    processed_df["trial_number"] = raw_df["TRIAL_INDEX"]
    processed_df["trial_condition"] = [
        "dilated" if idx % 2 == 1 else "constricted" for idx in raw_df["TRIAL_INDEX"]
    ]

    # Set spatial coordinates and angles
    processed_df["target_x"] = 1920 / 2
    processed_df["target_y"] = 1080 / 2
    processed_df["gaze_x"] = raw_df["AVERAGE_GAZE_X"]
    processed_df["gaze_y"] = raw_df["AVERAGE_GAZE_Y"]
    processed_df["gaze_angle_x"] = (raw_df["AVERAGE_GAZE_X"] - 1920 / 2) / raw_df[
        "RESOLUTION_X"
    ]
    processed_df["gaze_angle_y"] = (raw_df["AVERAGE_GAZE_Y"] - 1080 / 2) / raw_df[
        "RESOLUTION_Y"
    ]
    processed_df["target_angle_x"] = 0
    processed_df["target_angle_y"] = 0

    # Convert pupil area to diameter in mm
    processed_df["pup_diam_l"] = (
        EYELINK_REFERENCE_DIAMETER_MM
        * np.sqrt(raw_df["LEFT_PUPIL_SIZE"])
        / np.sqrt(EYELINK_REFERENCE_VALUE)
    )
    processed_df["pup_diam_r"] = (
        EYELINK_REFERENCE_DIAMETER_MM
        * np.sqrt(raw_df["RIGHT_PUPIL_SIZE"])
        / np.sqrt(EYELINK_REFERENCE_VALUE)
    )

    processed_df["TIME_FROM_TRIAL_START_MS"] = raw_df["TIME_FROM_TRIAL_START_MS"]

    return processed_df


def process_and_clean_data(
    df, trial_duration_ms=5000, time_trim=25, distance_threshold=10, z_threshold=3
):
    """
    Process and clean eye tracking data.

    Args:
        df: Input DataFrame with eye tracking data
        trial_duration_ms: Duration to extract from each trial in milliseconds
        time_trim: Total percentage of time to trim (split equally between start and end)
        distance_threshold: Maximum allowed distance from target
        z_threshold: Z-score threshold for outlier removal
    """

    data_columns = [
        "gaze_x",
        "gaze_y",
        "gaze_angle_x",
        "gaze_angle_y",
        "pup_diam_l",
        "pup_diam_r",
    ]

    # Step 1: Time-based trimming
    time_trimmed_df = pd.DataFrame()
    time_trim_stats = []

    # Calculate trim percentage for each end (half of total trim)
    edge_trim_percentage = time_trim / 2

    for (participant_id, trial_num), group_df in tqdm(
        df.groupby(["participant_id", "trial_number"]), desc="Time trimming"
    ):
        # Count initial rows
        initial_rows = len(group_df)

        # Extract the relevant time segment (last 5 seconds by default)
        max_time = group_df["TIME_FROM_TRIAL_START_MS"].max()
        min_time = max_time - trial_duration_ms

        # Select only data from the last 5 seconds
        last_section = group_df[
            (group_df["TIME_FROM_TRIAL_START_MS"] >= min_time)
            & (group_df["TIME_FROM_TRIAL_START_MS"] <= max_time)
        ]

        five_second_rows = len(last_section)

        if five_second_rows == 0:
            print(
                f"Warning: No data in the last {trial_duration_ms}ms for participant {participant_id}, trial {trial_num}"
            )
            continue

        # Calculate additional trim within the 5-second window
        trimmed_data = last_section
        if time_trim > 0:
            time_range = (
                last_section["TIME_FROM_TRIAL_START_MS"].max()
                - last_section["TIME_FROM_TRIAL_START_MS"].min()
            )
            start_trim = last_section["TIME_FROM_TRIAL_START_MS"].min() + (
                time_range * edge_trim_percentage / 100
            )
            end_trim = last_section["TIME_FROM_TRIAL_START_MS"].max() - (
                time_range * edge_trim_percentage / 100
            )

            trimmed_data = last_section[
                (last_section["TIME_FROM_TRIAL_START_MS"] >= start_trim)
                & (last_section["TIME_FROM_TRIAL_START_MS"] <= end_trim)
            ]

        final_rows = len(trimmed_data)

        # Store trimming statistics
        time_trim_stats.append(
            {
                "participant_id": participant_id,
                "trial_number": trial_num,
                "initial_rows": initial_rows,
                "five_second_rows": five_second_rows,
                "final_rows": final_rows,
            }
        )

        # Add to our result dataframe
        time_trimmed_df = pd.concat([time_trimmed_df, trimmed_data])

    # Reset index of the combined dataframe
    time_trimmed_df = time_trimmed_df.reset_index(drop=True)

    # Step 2: Calculate NaN statistics by participant and condition
    nan_stats_list = []

    # Calculate NaN statistics for each participant and condition
    for participant_id, participant_data in time_trimmed_df.groupby("participant_id"):
        for condition in ["dilated", "constricted"]:
            condition_mask = participant_data["trial_condition"] == condition
            condition_data = participant_data[condition_mask]

            if len(condition_data) > 0:
                # Count rows with NaNs for this participant and condition
                nan_rows = condition_data[data_columns].isna().any(axis=1).sum()
                total_rows = len(condition_data)

                nan_stats_list.append(
                    {
                        "participant_id": participant_id,
                        "condition": condition,
                        "total_rows": total_rows,
                        "nan_rows": nan_rows,
                        "nan_percentage": (
                            (nan_rows / total_rows * 100) if total_rows > 0 else 0
                        ),
                    }
                )

    # Add overall statistics
    overall_nan_rows = time_trimmed_df[data_columns].isna().any(axis=1).sum()
    overall_total_rows = len(time_trimmed_df)

    nan_stats_list.append(
        {
            "participant_id": "ALL",
            "condition": "ALL",
            "total_rows": overall_total_rows,
            "nan_rows": overall_nan_rows,
            "nan_percentage": (
                (overall_nan_rows / overall_total_rows * 100)
                if overall_total_rows > 0
                else 0
            ),
        }
    )

    print(
        f"OVERALL: {overall_nan_rows} NaN rows out of {overall_total_rows} rows "
        f"({(overall_nan_rows / overall_total_rows * 100):.2f}%)"
    )

    # Create NaN statistics dataframe
    nan_stats_df = pd.DataFrame(nan_stats_list)

    # Remove rows with NaNs
    time_trimmed_df = time_trimmed_df.dropna(subset=data_columns)

    # Step 3: Remove timing column
    time_trimmed_df.drop(columns=["TIME_FROM_TRIAL_START_MS"], inplace=True)

    # Step 4: Apply spatial filtering
    filtered_df = pd.DataFrame()

    for participant_id, participant_data in tqdm(
        time_trimmed_df.groupby("participant_id"), desc="Spatial filtering"
    ):
        for trial in participant_data["trial_number"].unique():
            trial_data = participant_data[
                participant_data["trial_number"] == trial
            ].copy()

            # Distance-based filtering
            trial_data["distance_to_target"] = np.sqrt(
                (trial_data["gaze_angle_x"] - trial_data["target_angle_x"]) ** 2
                + (trial_data["gaze_angle_y"] - trial_data["target_angle_y"]) ** 2
            )
            distance_filtered = trial_data[
                trial_data["distance_to_target"] <= distance_threshold
            ]

            # Z-score filtering
            valid_mask = pd.Series(True, index=distance_filtered.index)
            for col in ["gaze_angle_x", "gaze_angle_y"]:
                z_scores = np.abs(
                    stats.zscore(distance_filtered[col], nan_policy="omit")
                )
                valid_mask &= z_scores < z_threshold

            cleaned_data = distance_filtered[valid_mask].drop(
                columns=["distance_to_target"]
            )

            filtered_df = pd.concat([filtered_df, cleaned_data])

    return filtered_df, nan_stats_df


def main():

    current_file_path = Path(__file__).resolve()
    project_dir_path = current_file_path.parent.parent
    data_path = (
        project_dir_path / "recordings/eyelink1000plus/Output/all_participants.xls"
    )
    output_dir = project_dir_path / "data"
    os.makedirs(output_dir, exist_ok=True)

    # Load and process data
    print("Loading and preprocessing eye tracking data...")
    raw_data = load_and_preprocess_data(data_path)

    # Process and clean data
    cleaned_data, nan_stats = process_and_clean_data(
        raw_data,
        trial_duration_ms=5000,
        time_trim=25,  # 25% total trim (12.5% from each end)
        distance_threshold=10,
        z_threshold=3,
    )

    # Save NaN statistics
    nan_stats_file = output_dir / "eyelink1000plus_nan_statistics.csv"
    nan_stats.to_csv(nan_stats_file, index=False)
    print(
        f"Detailed NaN statistics by participant and condition saved to {nan_stats_file}"
    )

    # Save data by participant
    print(f"Saving data for {cleaned_data['participant_id'].nunique()} participants...")
    for participant_id in tqdm(
        cleaned_data["participant_id"].unique(), desc="Saving data"
    ):
        participant_dir = output_dir / f"{participant_id}/eyelink1000plus/"
        os.makedirs(participant_dir, exist_ok=True)

        participant_data = cleaned_data[
            cleaned_data["participant_id"] == participant_id
        ]
        save_path = participant_dir / "data.csv"
        participant_data.to_csv(save_path, index=False)


if __name__ == "__main__":
    main()
