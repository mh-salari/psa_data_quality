import pandas as pd
from pathlib import Path



dataset_dir_path = Path(__file__).resolve().parent.parent / "data"


eye_trackers = [
    "EyeLink 1000 Plus",
    "Pupil Core",
    "SMI ETG",
    "Pupil Neon",
    "Tobii Glasses 2",
]


columns_to_keep = [
    "eye_tracker",
    "participant_id",
    "trial_number",
    "trial_condition",
    "pup_diam_l",
    "pup_diam_r",
]


data_paths = []
for participant_dir in dataset_dir_path.iterdir():
    if participant_dir.is_dir():
        for eye_tracker in eye_trackers:
            data_path = participant_dir / eye_tracker / "data.csv"
            if data_path.exists():
                data_paths.append(data_path)


print(f"Found {len(data_paths)} CSV files.")


all_dfs = []

for file_path in data_paths:
    print(
        f"Reading {file_path.name} from {file_path.parent.parent.name}/{file_path.parent.name}"
    )

    df = pd.read_csv(file_path)

    existing_columns = [col for col in columns_to_keep if col in df.columns]

    df = df[existing_columns]
    all_dfs.append(df)


combined_df = pd.concat(all_dfs, ignore_index=True)

output_path = dataset_dir_path / "pupil_size.csv"
combined_df.to_csv(output_path, index=False)
print(f"Combined data saved to: {output_path}")
