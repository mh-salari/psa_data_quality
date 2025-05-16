"""
Author:       Mohammadhossein Salari
Email:        mohammadhossein.salari@gmail.com
Last Modified: 2024/05/14
Description: This script that calculates viewing distances for head-mounted eye-tracker
             data using camera calibration parameters and known physical dimensions of targets.

Input structure:
 data/
 ├── participant1/
 │   ├── Pupil Core/
 │   │   ├── stabilized.csv
 │   │   └── calibration.xml
 │   ├── SMI ETG/
 │   └── ...
 ├── participant2/
 │   └── ...
 └── ...
Output structure:
 data/
 ├── participant1/
 │   ├── Pupil Core/
 │   │   └── distance.csv
 │   ├── SMI ETG/
 │   │   └── distance.csv
 │   └── ...
 ├── participant2/
 │   └── ...
 └── ...
"""

import numpy as np
import pandas as pd
import xml.etree.ElementTree as ET
from pathlib import Path
from tqdm import tqdm


class CameraCalibration:
    def __init__(self, calibration_file):
        if not calibration_file.exists():
            raise FileNotFoundError(f"Calibration file not found: {calibration_file}")
        self.matrix = self._read_camera_params(calibration_file)

    def _read_camera_params(self, xml_file):
        tree = ET.parse(xml_file)
        root = tree.getroot()

        matrix_elem = root.find(".//cameraMatrix/data")
        matrix_data = list(map(float, matrix_elem.text.split()))

        return np.array(matrix_data).reshape(3, 3)

    @property
    def focal_length(self):
        return self.matrix[0, 0]


def calculate_euclidean_distance(x1, y1, x2, y2):
    return np.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2)


def calculate_distances(camera_cal, real_width_mm, real_height_mm, row):
    top_left = [row["top_left_x"], row["top_left_y"]]
    top_right = [row["top_right_x"], row["top_right_y"]]
    bottom_left = [row["bottom_left_x"], row["bottom_left_y"]]
    bottom_right = [row["bottom_right_x"], row["bottom_right_y"]]

    top_width = calculate_euclidean_distance(
        top_right[0], top_right[1], top_left[0], top_left[1]
    )
    bottom_width = calculate_euclidean_distance(
        bottom_right[0], bottom_right[1], bottom_left[0], bottom_left[1]
    )
    avg_width = (top_width + bottom_width) / 2

    left_height = calculate_euclidean_distance(
        bottom_left[0], bottom_left[1], top_left[0], top_left[1]
    )
    right_height = calculate_euclidean_distance(
        bottom_right[0], bottom_right[1], top_right[0], top_right[1]
    )
    avg_height = (left_height + right_height) / 2

    distance_from_width = (real_width_mm * camera_cal.focal_length) / avg_width
    distance_from_height = (real_height_mm * camera_cal.focal_length) / avg_height

    return distance_from_width, distance_from_height


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
        participant_id = int(data_dir.parent.name)
        eye_tracker = data_dir.name

        undistorted_df = pd.read_csv(data_dir / "stabilized.csv")
        camera_cal = CameraCalibration(data_dir / "calibration.xml")

        distances = []
        for _, row in undistorted_df.iterrows():
            if eye_tracker == "Pupil Core":
                if (
                    participant_id in [319, 460, 503, 772, 844]
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
                print("Undefined eye tracker")

            dist_width, dist_height = calculate_distances(
                camera_cal, real_width_mm, real_height_mm, row
            )

            distances.append(
                {
                    "frame": row["frame"],
                    "distance_from_width": dist_width,
                    "distance_from_height": dist_height,
                    "distance_average": (dist_width + dist_height) / 2,
                }
            )

        distances_df = pd.DataFrame(distances)
        distances_df.to_csv(data_dir / "distance.csv", index=False)


if __name__ == "__main__":
    main()
