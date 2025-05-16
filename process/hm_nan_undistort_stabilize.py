"""
Author:       Mohammadhossein Salari
Email:        mohammadhossein.salari@gmail.com
Last Modified: 2024/05/14
Description: The script undistorts camera points using calibration parameters, merges
             target and gaze data, filters out records with missing gaze data, and compensate
             for head movments by aligning targets.
             Statistics about missing data (NaNs) are collected for each participant and condition.

Input structure:
 data/
 ├── participant1/
 │   ├── Pupil Core/
 │   ├── SMI ETG/
 │   ├── Pupil Neon/
 │   └── Tobii Glasses 2/
 ├── participant2/
 │   └── ...
 └── participant3/
     └── ...
Output structure:
 data/
 ├── participant1/
 │   ├── Pupil Core/
 │   │   ├── undistorted.csv
 │   │   └── stabilized.csv
 │   └── ...
 ├── participant2/
 │   └── ...
 └── hm_nan_statistics.csv
"""

from pathlib import Path
import numpy as np
import pandas as pd
import xml.etree.ElementTree as ET
import cv2
from tqdm import tqdm


class CameraCalibration:
    def __init__(self, calibration_file):
        if not calibration_file.exists():
            raise FileNotFoundError(f"Calibration file not found: {calibration_file}")
        self.matrix, self.distortion_coeffs = self._read_camera_params(calibration_file)

    def _read_camera_params(self, xml_file):
        tree = ET.parse(xml_file)
        root = tree.getroot()

        matrix_elem = root.find(".//cameraMatrix/data")
        matrix_data = list(map(float, matrix_elem.text.split()))
        matrix = np.array(matrix_data).reshape(3, 3)

        distortion_elem = root.find(".//distCoeff/data")
        if distortion_elem is not None and distortion_elem.text:
            distortion_data = list(map(float, distortion_elem.text.split()))
            distortion_coeffs = np.array(distortion_data)
        else:
            raise ValueError("Distortion coefficients not found in calibration file")

        return matrix, distortion_coeffs

    @property
    def focal_length(self):
        return self.matrix[0, 0]


def undistort_points(camera_cal, points):
    points = np.array(points, dtype=np.float32).reshape(-1, 1, 2)
    undistorted = cv2.undistortPoints(
        points, camera_cal.matrix, camera_cal.distortion_coeffs, P=camera_cal.matrix
    )
    return undistorted.reshape(-1, 2)


def undistort_dataframe(df, camera_cal):
    undistorted_df = df.copy()

    coordinate_pairs = [
        ("target_x", "target_y"),
        ("top_left_x", "top_left_y"),
        ("top_right_x", "top_right_y"),
        ("bottom_left_x", "bottom_left_y"),
        ("bottom_right_x", "bottom_right_y"),
        ("gaze_x", "gaze_y"),
    ]

    for x_col, y_col in coordinate_pairs:
        undistorted_df[x_col] = undistorted_df[x_col].astype("float64")
        undistorted_df[y_col] = undistorted_df[y_col].astype("float64")

    for idx, row in undistorted_df.iterrows():
        points_to_undistort = []
        for x_col, y_col in coordinate_pairs:
            points_to_undistort.append([row[x_col], row[y_col]])

        undistorted_points = undistort_points(camera_cal, points_to_undistort)

        for i, (x_col, y_col) in enumerate(coordinate_pairs):
            undistorted_df.at[idx, x_col] = undistorted_points[i][0]
            undistorted_df.at[idx, y_col] = undistorted_points[i][1]

    return undistorted_df


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

    # Create a list to store NaN removal statistics
    nan_stats = []

    # Define the columns we want to keep
    columns_to_keep = [
        "segment",
        "frame",
        "trial_condition",
        "target_x",
        "target_y",
        "top_left_x",
        "top_left_y",
        "top_right_x",
        "top_right_y",
        "bottom_left_x",
        "bottom_left_y",
        "bottom_right_x",
        "bottom_right_y",
        "pup_diam_l",
        "pup_diam_r",
        "gaze_pos_vid_x",
        "gaze_pos_vid_y",
    ]

    for data_dir in tqdm(data_dirs[:]):
        eye_tracker = data_dir.name
        participant_id = data_dir.parent.name

        target_df = pd.read_csv(data_dir / "target.csv")
        gaze_df = pd.read_csv(data_dir / "gazeData.tsv", sep="\t")

        camera_cal = CameraCalibration(data_dir / "calibration.xml")

        # Merge with gaze data
        merged_df = target_df.merge(
            gaze_df, left_on="frame", right_on="frame_idx", how="left"
        )

        # Check for missing columns and add them with NaN values
        for col in columns_to_keep:
            if col not in merged_df.columns:
                merged_df[col] = float("nan")

        merged_df = merged_df[columns_to_keep]

        # Split by condition
        dark_df = merged_df[merged_df["trial_condition"] == "dark"]
        bright_df = merged_df[merged_df["trial_condition"] == "bright"]

        # Process dark data
        dark_count = dark_df.shape[0]
        dark_missing_mask = (
            dark_df[["gaze_pos_vid_x", "gaze_pos_vid_y"]].isna().any(axis=1)
        )
        dark_nan_count = dark_missing_mask.sum()

        # Save dark condition NaN statistics
        nan_stats.append(
            {
                "eye_tracker": eye_tracker,
                "participant_id": participant_id,
                "condition": "dark",
                "total_rows": dark_count,
                "nan_rows": dark_nan_count,
            }
        )

        # Process bright data
        bright_count = bright_df.shape[0]
        bright_missing_mask = (
            bright_df[["gaze_pos_vid_x", "gaze_pos_vid_y"]].isna().any(axis=1)
        )
        bright_nan_count = bright_missing_mask.sum()

        # Save bright condition NaN statistics
        nan_stats.append(
            {
                "eye_tracker": eye_tracker,
                "participant_id": participant_id,
                "condition": "bright",
                "total_rows": bright_count,
                "nan_rows": bright_nan_count,
            }
        )

        # Combine cleaned data
        df = pd.concat(
            [
                dark_df[~dark_missing_mask],
                bright_df[~bright_missing_mask],
            ]
        )
        df = df.sort_values(by="frame")
        df.insert(0, "eye_tracker", eye_tracker)
        df.insert(1, "participant_id", participant_id)

        df = df.rename(
            columns={
                "segment": "trial_number",
                "gaze_pos_vid_x": "gaze_x",
                "gaze_pos_vid_y": "gaze_y",
            }
        )

        undistorted_df = undistort_dataframe(df, camera_cal)
        undistorted_df.to_csv(data_dir / "undistorted.csv", index=False)

        # Get reference point from the first frame
        reference_x = undistorted_df["target_x"].iloc[0]
        reference_y = undistorted_df["target_y"].iloc[0]

        # Calculate offsets needed to align each frame's target with the reference
        offsets_x = reference_x - undistorted_df["target_x"]
        offsets_y = reference_y - undistorted_df["target_y"]

        # Apply offsets to all coordinate columns
        coordinate_cols = [
            col
            for col in undistorted_df.columns
            if col.endswith("_x") or col.endswith("_y")
        ]
        for col in coordinate_cols:
            if col.endswith("_x"):
                undistorted_df[col] = undistorted_df[col] + offsets_x
            elif col.endswith("_y"):
                undistorted_df[col] = undistorted_df[col] + offsets_y

        undistorted_df.to_csv(data_dir / "undistorted.csv", index=False)

        stabilized_df = undistorted_df.copy()

        # Get reference point from the first frame
        reference_x = stabilized_df["target_x"].iloc[0]
        reference_y = stabilized_df["target_y"].iloc[0]

        # Calculate offsets needed to align each frame's target with the reference
        offsets_x = reference_x - stabilized_df["target_x"]
        offsets_y = reference_y - stabilized_df["target_y"]

        # Apply offsets to all coordinate columns
        coordinate_cols = [
            col
            for col in stabilized_df.columns
            if col.endswith("_x") or col.endswith("_y")
        ]
        for col in coordinate_cols:
            if col.endswith("_x"):
                stabilized_df[col] = stabilized_df[col] + offsets_x
            elif col.endswith("_y"):
                stabilized_df[col] = stabilized_df[col] + offsets_y

        stabilized_df.to_csv(data_dir / "stabilized.csv", index=False)

    # After all processing is complete, convert statistics to DataFrame and save
    nan_stats_df = pd.DataFrame(nan_stats)
    nan_stats_df.to_csv(dataset_dir_path / "hm_nan_statistics.csv", index=False)


if __name__ == "__main__":
    main()
