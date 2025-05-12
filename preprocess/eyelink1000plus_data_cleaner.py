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
    """Load and preprocess raw EyeLink data into a common data column names."""
    # Load raw data
    raw_df = pd.read_csv(
        data_path,
        sep="\t",
        encoding="utf-16",
        na_values=["."],
        dtype={"RECORDING_SESSION_LABEL": str},
    )

    # Calculate relative time from trial start
    min_timestamp = raw_df.groupby(["RECORDING_SESSION_LABEL", "TRIAL_INDEX"])["TIMESTAMP"].transform("min")
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
    processed_df["gaze_angle_x"] = (raw_df["AVERAGE_GAZE_X"] - 1920 / 2) / raw_df["RESOLUTION_X"]
    processed_df["gaze_angle_y"] = (raw_df["AVERAGE_GAZE_Y"] - 1080 / 2) / raw_df["RESOLUTION_Y"]
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


def process_and_clean_data(df, trial_duration_ms=5000, time_trim=25, distance_threshold=10, z_threshold=3):
    """
    Process and clean eye tracking data through time trimming and spatial filtering.
    
    Args:
        df: Input DataFrame with eye tracking data
        trial_duration_ms: Duration to extract from each trial in milliseconds
        time_trim: Total percentage of time to trim (split equally between start and end)
        distance_threshold: Maximum allowed distance from target
        z_threshold: Z-score threshold for outlier removal
    """
    # Step 1: Time-based trimming
    print("Applying time-based trimming...")
    time_trimmed_df = pd.DataFrame()
    
    # Calculate trim percentage for each end (half of total trim)
    edge_trim_percentage = time_trim / 2
    
    for (participant_id, trial_num), group_df in tqdm(
        df.groupby(["participant_id", "trial_number"]), desc="Time trimming"
    ):
        # Extract the relevant time segment (last 5 seconds by default)
        max_time = group_df["TIME_FROM_TRIAL_START_MS"].max()
        last_section = group_df[
            (group_df["TIME_FROM_TRIAL_START_MS"] >= max_time - trial_duration_ms)
            & (group_df["TIME_FROM_TRIAL_START_MS"] <= max_time)
        ]
        
        if len(last_section) == 0:
            continue
            
        # Trim specified percentage from start and end using the time_trim parameter
        time_range = last_section["TIME_FROM_TRIAL_START_MS"].max() - last_section["TIME_FROM_TRIAL_START_MS"].min()
        start_trim = last_section["TIME_FROM_TRIAL_START_MS"].min() + (time_range * edge_trim_percentage / 100)
        end_trim = last_section["TIME_FROM_TRIAL_START_MS"].max() - (time_range * edge_trim_percentage / 100)
        
        trimmed_data = last_section[
            (last_section["TIME_FROM_TRIAL_START_MS"] >= start_trim)
            & (last_section["TIME_FROM_TRIAL_START_MS"] <= end_trim)
        ]
        
        time_trimmed_df = pd.concat([time_trimmed_df, trimmed_data])
    
    time_trimmed_df = time_trimmed_df.reset_index(drop=True)
    
    # Step 2: Remove NaN values and calculate statistics
    data_columns = ['gaze_x', 'gaze_y', 'gaze_angle_x', 'gaze_angle_y', 'pup_diam_l', 'pup_diam_r']
    
    # Count rows with NaNs before removal
    rows_with_nan = time_trimmed_df[data_columns].isna().any(axis=1)
    
    # Get stats for each condition
    dilated_mask = time_trimmed_df['trial_condition'] == 'dilated'
    constricted_mask = time_trimmed_df['trial_condition'] == 'constricted'
    
    # Count total rows and rows with NaNs for each condition
    dilated_total = dilated_mask.sum()
    dilated_nan_rows = (dilated_mask & rows_with_nan).sum()
    
    constricted_total = constricted_mask.sum()
    constricted_nan_rows = (constricted_mask & rows_with_nan).sum()
    
    total_rows = len(time_trimmed_df)
    total_nan_rows = rows_with_nan.sum()
    
    # Print statistics
    print("\n---- NaN Statistics by Condition ----")
    print(f"Dilated: {dilated_nan_rows} rows with NaNs out of {dilated_total} rows ({dilated_nan_rows/dilated_total*100:.2f}%)")
    print(f"Constricted: {constricted_nan_rows} rows with NaNs out of {constricted_total} rows ({constricted_nan_rows/constricted_total*100:.2f}%)")
    print(f"Total: {total_nan_rows} rows with NaNs out of {total_rows} rows ({total_nan_rows/total_rows*100:.2f}%)")
    
    # Remove rows with NaNs
    time_trimmed_df = time_trimmed_df.dropna(subset=data_columns)
    
    # Step 3: Remove timing column
    time_trimmed_df.drop(columns=["TIME_FROM_TRIAL_START_MS"], inplace=True)
    
    # Step 4: Apply spatial filtering
    print("\nApplying spatial filtering...")
    filtered_df = pd.DataFrame()
    
    for participant_id, participant_data in tqdm(
        time_trimmed_df.groupby("participant_id"), desc="Spatial filtering"
    ):
        for trial in participant_data["trial_number"].unique():
            trial_data = participant_data[participant_data["trial_number"] == trial].copy()
            
            # Distance-based filtering
            trial_data["distance_to_target"] = np.sqrt(
                (trial_data["gaze_angle_x"] - trial_data["target_angle_x"])**2 + 
                (trial_data["gaze_angle_y"] - trial_data["target_angle_y"])**2
            )
            distance_filtered = trial_data[trial_data["distance_to_target"] <= distance_threshold]
            
            # Z-score filtering
            valid_mask = pd.Series(True, index=distance_filtered.index)
            for col in ["gaze_angle_x", "gaze_angle_y"]:
                z_scores = np.abs(stats.zscore(distance_filtered[col], nan_policy="omit"))
                valid_mask &= z_scores < z_threshold
            
            cleaned_data = distance_filtered[valid_mask].drop(columns=["distance_to_target"])
            filtered_df = pd.concat([filtered_df, cleaned_data])
    
    # Create a basic nan_stats dataframe for compatibility with the rest of the code
    nan_stats_df = pd.DataFrame([
        {'condition': 'dilated', 'total_rows': dilated_total, 'nan_rows': dilated_nan_rows, 'nan_percentage': dilated_nan_rows/dilated_total*100},
        {'condition': 'constricted', 'total_rows': constricted_total, 'nan_rows': constricted_nan_rows, 'nan_percentage': constricted_nan_rows/constricted_total*100},
        {'condition': 'ALL', 'total_rows': total_rows, 'nan_rows': total_nan_rows, 'nan_percentage': total_nan_rows/total_rows*100}
    ])
    
    return filtered_df, nan_stats_df

def main():
    """Process eye tracking data and save by participant."""
    # Setup paths
    current_file_path = Path(__file__).resolve()
    project_dir_path = current_file_path.parent.parent
    data_path = project_dir_path / "recordings/eyelink1000plus/Output/all_participants.xls"
    output_dir = project_dir_path / "data"
    os.makedirs(output_dir, exist_ok=True)

    # Load and process data
    print("Loading and preprocessing eye tracking data...")
    raw_data = load_and_preprocess_data(data_path)
    
    # Process and clean data with specified parameters
    cleaned_data, nan_stats = process_and_clean_data(
        raw_data, 
        trial_duration_ms=5000,
        time_trim=25,  # 25% total trim (12.5% from each end)
        distance_threshold=10, 
        z_threshold=3
    )
    
    
    # Save NaN statistics
    nan_stats_file = output_dir / "eyelink1000plus_nan_statistics.csv"
    nan_stats.to_csv(nan_stats_file, index=False)
    print(f"\nDetailed NaN statistics saved to {nan_stats_file}")
    
    # Save data by participant
    print(f"\nSaving data for {cleaned_data['participant_id'].nunique()} participants...")
    for participant_id in tqdm(cleaned_data['participant_id'].unique(), desc="Saving data"):
        participant_dir = output_dir / f"{participant_id}/eyelink1000plus/"
        os.makedirs(participant_dir, exist_ok=True)
        
        participant_data = cleaned_data[cleaned_data['participant_id'] == participant_id]
        save_path = participant_dir / "data.csv"
        participant_data.to_csv(save_path, index=False)
    
    # Calculate and print final statistics
    total_participants = cleaned_data['participant_id'].nunique()
    total_rows = len(cleaned_data)
    rows_by_condition = cleaned_data.groupby('trial_condition').size()
    
    print(f"\nProcessing complete.")
    print(f"Total participants: {total_participants}")
    print(f"Total rows in final dataset: {total_rows}")
    print(f"Rows by condition:")
    for condition, count in rows_by_condition.items():
        print(f"  {condition}: {count} rows ({count/total_rows*100:.2f}%)")


if __name__ == "__main__":
    main()