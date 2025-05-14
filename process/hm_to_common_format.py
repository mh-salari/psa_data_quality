"""
Author:       Mohammadhossein Salari
Email:        mohammadhossein.salari@gmail.com
Last Modified: 2024/02/17
Description: This script that converts head mounted eye-trackers datato glassesTools
             common format.

             The script expects the input directory to be from one eye tracker
             (e.g., Pupil Labs, Tobii, etc.) containing participant subdirectories.

             To use this script, you must install the glassesTools library from
             https://github.com/dcnieho/glassesTools/.

Input structure:
    raw_data/  (eye tracker specific directory)
    ├── participant1/
    ├── participant2/
    └── participant3/

Output structure:
    export_data/
    ├── participant1/
    │   └── eye_tracker_name/
    ├── participant2/
    │   └── eye_tracker_name/
    └── participant3/
        └── eye_tracker_name/
"""

import argparse
from pathlib import Path
from pprint import pprint
from glassesTools.eyetracker import eye_tracker_names
from glassesTools.importing import do_import, get_recording_info


# ANSI color codes
class Colors:
    HEADER = "\033[95m"
    OKBLUE = "\033[94m"
    OKGREEN = "\033[92m"
    WARNING = "\033[93m"
    FAIL = "\033[91m"
    ENDC = "\033[0m"
    BOLD = "\033[1m"


def print_colored(text, color):
    print(f"{color}{text}{Colors.ENDC}")


def import_data(raw_data_dir, export_data_dir, eye_tracker):
    """
    Import eye tracking data from a single eye tracker directory containing multiple participant data.

    Args:
        raw_data_dir (str): Path to the eye tracker's raw data directory
        export_data_dir (str): Path where processed data should be exported
        eye_tracker (str): Name of the eye tracker (must be supported by glassesTools)
    """
    if eye_tracker not in eye_tracker_names:
        print_colored(
            f"\nError: Unsupported eye tracker type: {eye_tracker}",
            Colors.FAIL,
        )
        print_colored(
            f"Supported eye trackers: {', '.join(eye_tracker_names)}",
            Colors.WARNING,
        )
        return

    raw_data_path = Path(raw_data_dir)
    export_data_path = Path(export_data_dir)
    export_data_path.mkdir(parents=True, exist_ok=True)

    print_colored(f"\nImporting data from eye tracker: {eye_tracker}", Colors.HEADER)

    for participant_dir in raw_data_path.iterdir():
        if participant_dir.is_dir():
            participant_id = participant_dir.name
            print_colored(f"  Processing participant: {participant_id}", Colors.OKBLUE)

            source_dir = str(participant_dir)
            output_dir = str(export_data_path / participant_id / eye_tracker)
            Path(output_dir).mkdir(parents=True, exist_ok=True)

            try:
                rec_info = get_recording_info(source_dir, eye_tracker)[0]
                print_colored("  Recording info:", Colors.BOLD)
                pprint(rec_info.__dict__, indent=4, width=100)

                do_import(output_dir, source_dir, eye_tracker, rec_info)
                print_colored(
                    f"  Data imported successfully for {participant_id}",
                    Colors.OKGREEN,
                )
            except Exception as e:
                print_colored(
                    f"  Error importing data for {participant_id}: {str(e)}",
                    Colors.FAIL,
                )

    print_colored("\nImport complete.", Colors.BOLD)


if __name__ == "__main__":
    # Set up argument parser
    parser = argparse.ArgumentParser(
        description="Import eye tracking data from a single eye tracker directory"
    )
    parser.add_argument(
        "--input",
        "-i",
        type=str,
        required=True,
        help="Input directory path containing participant data for a specific eye tracker",
    )
    parser.add_argument(
        "--output",
        "-o",
        type=str,
        required=True,
        help="Output directory path for imported data",
    )
    parser.add_argument(
        "--eye-tracker",
        "-e",
        type=str,
        required=True,
        help=f"Eye tracker type. Supported types: {', '.join(eye_tracker_names)}",
    )

    # Parse arguments
    args = parser.parse_args()

    # Call the import function
    import_data(args.input, args.output, args.eye_tracker)
