import os
from pathlib import Path
from tqdm import tqdm

import numpy as np
import pandas as pd
from scipy import stats

from glassesTools import ocv, transforms


def clean_trials(
    df,
    start_threshold=25 / 2,
    end_threshold=25 / 2,
    z_threshold=3,
    distance_threshold=10,
):
    df_cleaned = pd.DataFrame()
    for trial in df["segment"].unique():
        trial_data = df[df["segment"] == trial].copy()

        # Time trimming
        start_idx = trial_data.index.min()
        end_idx = trial_data.index.max()
        total_indices = end_idx - start_idx

        keep_start_idx = start_idx + int(total_indices * (start_threshold / 100))
        keep_end_idx = end_idx - int(total_indices * (end_threshold / 100))

        time_mask = (trial_data.index >= keep_start_idx) & (
            trial_data.index <= keep_end_idx
        )
        time_trimmed_data = trial_data[time_mask].copy()

        # Distance-based filtering using your existing function
        distance_mask = time_trimmed_data["gaze_target_angle"] <= distance_threshold
        distance_trimmed_data = time_trimmed_data[distance_mask]

        # Z-score filtering
        columns_to_check = ["gaze_target_angle"]
        valid_mask = pd.Series(True, index=distance_trimmed_data.index)

        for col in columns_to_check:
            z_scores = np.abs(
                stats.zscore(distance_trimmed_data[col], nan_policy="omit")
            )
            valid_mask &= z_scores < z_threshold

        cleaned_trial_data = distance_trimmed_data[valid_mask]
        df_cleaned = pd.concat([df_cleaned, cleaned_trial_data])

    return df_cleaned


def main():

    current_file_path = Path(__file__).resolve()
    project_dir_path = current_file_path.parent.parent

    dataset_dir_path = project_dir_path / "data"

    # List of eye trackers
    eye_trackers = ["Pupil Core", "SMI ETG", "Pupil Neon", "Tobii Glasses 2"]

    # Find participant directories for each eye tracker
    participant_dirs_by_tracker = {}
    for eye_tracker in eye_trackers:
        participant_dirs = [
            d
            for d in dataset_dir_path.iterdir()
            if d.is_dir() and (d / eye_tracker).exists()
        ]
        participant_dirs_by_tracker[eye_tracker] = participant_dirs

    # Create a list to store NaN removal statistics
    nan_stats = []

    # Outer progress bar for eye trackers
    for eye_tracker, participant_dirs in tqdm(
        participant_dirs_by_tracker.items(), desc="Processing eye trackers", position=0
    ):
        # Inner progress bar for participants
        for participant_dir in tqdm(
            participant_dirs,
            desc=f"Processing {eye_tracker} participants",
            position=1,
            leave=False,
        ):
            # Extract participant ID from directory path
            participant_id = os.path.basename(participant_dir)

            data_dir = Path(participant_dir, eye_tracker)
            target_df = pd.read_csv(data_dir / "target.csv")
            gaze_df = pd.read_csv(data_dir / "gazeData.tsv", sep="\t")
            cam = ocv.CameraParams.read_from_file(str(data_dir / "calibration.xml"))

            # Directly merge with gaze data
            merged_df = target_df[
                ["frame", "trial_condition", "segment", "target_x", "target_y"]
            ].merge(gaze_df, left_on="frame", right_on="frame_idx", how="left")

            # Column order
            base_columns = [
                "frame",
                "trial_condition",
                "segment",
                "target_x",
                "target_y",
            ]
            other_columns = [
                col
                for col in merged_df.columns
                if col not in base_columns + ["frame_idx"]
            ]
            merged_df = merged_df[base_columns + other_columns]

            # Process by lighting state - only check for gaze position NaNs
            gaze_columns = ["gaze_pos_vid_x", "gaze_pos_vid_y"]

            # Split by condition
            dilated_df = merged_df[merged_df["trial_condition"] == "dilated"]
            constricted_df = merged_df[merged_df["trial_condition"] == "constricted"]

            # Process dilated data
            dilated_count = dilated_df.shape[0]
            dilated_missing_mask = dilated_df[gaze_columns].isna().any(axis=1)
            dilated_nan_count = dilated_missing_mask.sum()
            dilated_percent = (
                (dilated_nan_count / dilated_count) * 100 if dilated_count > 0 else 0
            )
            clean_dilated_df = dilated_df[~dilated_missing_mask].copy()

            # Save dilated condition NaN statistics
            nan_stats.append(
                {
                    "eye_tracker": eye_tracker,
                    "participant_id": participant_id,
                    "condition": "dilated",
                    "total_rows": dilated_count,
                    "nan_rows": dilated_nan_count,
                    "nan_percentage": dilated_percent,
                }
            )

            # Process constricted data
            constricted_count = constricted_df.shape[0]
            constricted_missing_mask = constricted_df[gaze_columns].isna().any(axis=1)
            constricted_nan_count = constricted_missing_mask.sum()
            constricted_percent = (
                (constricted_nan_count / constricted_count) * 100
                if constricted_count > 0
                else 0
            )
            clean_constricted_df = constricted_df[~constricted_missing_mask].copy()

            # Save constricted condition NaN statistics
            nan_stats.append(
                {
                    "eye_tracker": eye_tracker,
                    "participant_id": participant_id,
                    "condition": "constricted",
                    "total_rows": constricted_count,
                    "nan_rows": constricted_nan_count,
                    "nan_percentage": constricted_percent,
                }
            )

            # Combine cleaned data
            df = pd.concat([clean_dilated_df, clean_constricted_df])

            # Continue with the rest of the processing
            target_positions = df[["target_x", "target_y"]].to_numpy()
            target = transforms.unproject_points(target_positions, cam)
            gaze_positions = df[["gaze_pos_vid_x", "gaze_pos_vid_y"]].to_numpy()
            gaze = transforms.unproject_points(gaze_positions, cam)

            angles = []
            for t, g in zip(target, gaze):
                angle = transforms.angle_between(t, g)
                angles.append(angle)

            df["gaze_target_angle"] = angles
            cleaned_df = clean_trials(df)

            # Add eye tracker as first column
            cleaned_df.insert(0, "eye_tracker", eye_tracker)

            # Save the cleaned DataFrame to the participant directory
            output_path = Path(data_dir) / "data.csv"
            cleaned_df.to_csv(output_path, index=False)

    # After all processing is complete, convert statistics to DataFrame and save
    nan_stats_df = pd.DataFrame(nan_stats)
    nan_stats_df.to_csv(
        dataset_dir_path / "head_mounted_nan_statistics.csv", index=False
    )


if __name__ == "__main__":
    main()
