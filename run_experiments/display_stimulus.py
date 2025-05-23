"""
Author:       Mohammadhossein Salari
Email:        mohammadhossein.salari@gmail.com
Last Modified: 2024/08/29
Description:  This Python script presents a sequence of black and white screens 
              with a centered stimulus image, interspersed with rest periods. 
              It also displays instructions and allows the user to control the 
              experiment flow using the space bar. The script is designed to be 
              run in full-screen mode on the secondary monitor (if available). 
              It loads experiment settings from an experiment_settings.json file, 
              and if the file doesn't exist, it uses default values.
"""

import cv2
import time
import numpy as np
import sys
from pathlib import Path
import screeninfo
import json
import ctypes


def get_secondary_monitor():
    screens = screeninfo.get_monitors()
    if len(screens) > 1:
        return screens[1]  # Assuming the second monitor is at index 1
    else:
        print("No secondary monitor detected. Using primary monitor.")
        return screens[0]


def load_parameters(settings_file_path):
    # Set default values

    screen_width = 1920
    screen_height = 1080
    bright_rest_duration = 5
    dark_rest_duration = 5
    recording_duration = 5

    # Define the path to the JSON file
    settings_file_path = Path(settings_file_path)

    # Check if the JSON file exists
    if settings_file_path.exists():
        try:
            # Load the parameters from the JSON file
            with settings_file_path.open("r") as file:
                settings = json.load(file)

            # Update the parameters with the values from the JSON file
            screen_height = settings.get("screen_height", screen_height)
            screen_width = settings.get("screen_width", screen_width)
            bright_rest_duration = settings.get(
                "bright_rest_duration", bright_rest_duration
            )
            dark_rest_duration = settings.get("dark_rest_duration", dark_rest_duration)
            recording_duration = settings.get("recording_duration", recording_duration)
        except (json.JSONDecodeError, FileNotFoundError):
            # Handle any errors that may occur while reading the JSON file
            print("Error reading the JSON file. Using default parameters.")

    return (
        screen_height,
        screen_width,
        bright_rest_duration,
        dark_rest_duration,
        recording_duration,
    )


def add_circles_to_image(image, circle_size=10, circle_color=(0, 0, 255), margin=50):
    height, width = image.shape[:2]
    radius = circle_size // 2

    # Top-left circle
    cv2.circle(image, (margin + radius, margin + radius), radius, circle_color, -1)

    # Top-right circle
    cv2.circle(
        image, (width - margin - radius, margin + radius), radius, circle_color, -1
    )

    # Bottom-left circle
    cv2.circle(
        image, (margin + radius, height - margin - radius), radius, circle_color, -1
    )

    # Bottom-right circle
    cv2.circle(
        image,
        (width - margin - radius, height - margin - radius),
        radius,
        circle_color,
        -1,
    )

    return image


def add_stimulus_to_image(stimulus_path, input_image):

    image = input_image.copy()
    # Load the stimulus image with alpha channel
    stimulus_image = cv2.imread(stimulus_path, cv2.IMREAD_UNCHANGED)

    # Check if stimulus image is loaded
    if stimulus_image is None:
        print(f"Error: Unable to load image at {stimulus_path}")
        return None

    # Set the image resolution
    image_width = image.shape[1]
    image_height = image.shape[0]

    # Get stimulus stimulus image dimensions
    stimulus_image_height, stimulus_image_width = stimulus_image.shape[:2]

    # Check if the stimulus image has an alpha channel
    if stimulus_image.shape[2] == 4:
        # Separate the RGB and alpha channels
        b, g, r, a = cv2.split(stimulus_image)
        rgb_image = cv2.merge((b, g, r))
        alpha_channel = a
    else:
        # If no alpha channel, use the image as is
        rgb_image = stimulus_image
        alpha_channel = (
            np.ones((stimulus_image_height, stimulus_image_width), dtype=np.uint8) * 255
        )

    # Calculate the top-left corner position to center the image
    top_left_x = (image_width - stimulus_image_width) // 2
    top_left_y = (image_height - stimulus_image_height) // 2

    # Create a mask and inverse mask from the alpha channel
    alpha_mask = alpha_channel / 255.0
    alpha_inv_mask = 1.0 - alpha_mask

    # Place the image on the image
    roi = image[
        top_left_y : top_left_y + stimulus_image_height,
        top_left_x : top_left_x + stimulus_image_width,
    ]
    for c in range(0, 3):
        roi[:, :, c] = alpha_mask * rgb_image[:, :, c] + alpha_inv_mask * roi[:, :, c]

    return image


def add_text_to_image(image, text, font_scale=1, color=(0, 0, 0), thickness=2):
    font = cv2.FONT_HERSHEY_SIMPLEX
    line_spacing = int(40 * font_scale)
    text_lines = text.split("\n")

    # Get total text height
    total_text_height = len(text_lines) * line_spacing

    # Calculate starting y position to center text vertically
    start_y = (image.shape[0] - total_text_height) // 2

    for i, line in enumerate(text_lines):
        # Get line size
        (text_width, text_height), _ = cv2.getTextSize(
            line, font, font_scale, thickness
        )

        # Calculate x position to center this line
        x = (image.shape[1] - text_width) // 2

        y = start_y + i * line_spacing + text_height

        cv2.putText(
            image,
            line,
            (x, y),
            font,
            font_scale,
            color,
            thickness,
            cv2.LINE_AA,
        )


def main():

    # Find the resources path
    resources_dir = Path(__file__).parent / "resources"

    # Load the experiment parameters
    settings_file_path = resources_dir / "experiment_settings.json"
    (
        screen_height,
        screen_width,
        bright_rest_duration,
        dark_rest_duration,
        recording_duration,
    ) = load_parameters(settings_file_path)

    # Create the full path of the stimulus
    stimulus_path = resources_dir / "stimulus.png"

    # make backgrounds (black and white)
    black_background = np.zeros((screen_height, screen_width, 3), dtype=np.uint8)
    gray_background = np.ones((screen_height, screen_width, 3), dtype=np.uint8) * 100
    white_background = np.ones((screen_height, screen_width, 3), dtype=np.uint8) * 255

    circle_size = 25
    circle_color = (0, 255, 0)
    circle_margin = 250

    # Add circles to backgrounds
    black_background = add_circles_to_image(
        black_background, circle_size, circle_color, circle_margin
    )
    cv2.imwrite(
        str(resources_dir / "black_background.png"),
        black_background,
        [cv2.IMWRITE_PNG_COMPRESSION, 0],
    )
    gray_background = add_circles_to_image(
        gray_background, circle_size, circle_color, circle_margin
    )
    cv2.imwrite(
        str(resources_dir / "gray_background.png"),
        gray_background,
        [cv2.IMWRITE_PNG_COMPRESSION, 0],
    )
    white_background = add_circles_to_image(
        white_background, circle_size, circle_color, circle_margin
    )
    cv2.imwrite(
        str(resources_dir / "white_background.png"),
        white_background,
        [cv2.IMWRITE_PNG_COMPRESSION, 0],
    )
    # Add stimulus image to backgrounds
    black_with_stimulus = add_stimulus_to_image(str(stimulus_path), black_background)
    cv2.imwrite(
        str(resources_dir / "black_with_stimulus.png"),
        black_with_stimulus,
        [cv2.IMWRITE_PNG_COMPRESSION, 0],
    )
    white_with_stimulus = add_stimulus_to_image(str(stimulus_path), white_background)
    cv2.imwrite(
        str(resources_dir / "white_with_stimulus.png"),
        white_with_stimulus,
        [cv2.IMWRITE_PNG_COMPRESSION, 0],
    )

    # Create instruction page
    instruction_page = gray_background.copy()
    instruction_text = (
        "Please fixate your eye on the stimulus and try not to blink.\n"
        "You can blink and rest after each stimulus when the gray screen is displayed.\n"
        "\nPress space to start."
    )
    add_text_to_image(instruction_page, instruction_text)
    cv2.imwrite(
        str(resources_dir / "instruction_page.png"),
        instruction_page,
        [cv2.IMWRITE_PNG_COMPRESSION, 0],
    )

    # Create rest page
    rest_page = gray_background.copy()
    rest_text = "You can rest now.\n\nPress space to continue."
    add_text_to_image(rest_page, rest_text)
    cv2.imwrite(
        str(resources_dir / "rest_page.png"),
        rest_page,
        [cv2.IMWRITE_PNG_COMPRESSION, 0],
    )

    # Create end of experiment page
    end_page = gray_background.copy()
    end_text = (
        "End of experiment.\nThank you for participating.\n\nPress space to exit."
    )
    add_text_to_image(end_page, end_text)
    cv2.imwrite(
        str(resources_dir / "end_page.png"),
        end_page,
        [cv2.IMWRITE_PNG_COMPRESSION, 0],
    )

    states = [
        # instruction
        (instruction_page, -1),
        # 1
        (black_background, dark_rest_duration),
        (black_with_stimulus, recording_duration),
        (rest_page, -1),
        (white_background, bright_rest_duration),
        (white_with_stimulus, recording_duration),
        # rest
        (rest_page, -1),
        # 2
        (black_background, dark_rest_duration),
        (black_with_stimulus, recording_duration),
        (rest_page, -1),
        (white_background, bright_rest_duration),
        (white_with_stimulus, recording_duration),
        # rest
        (rest_page, -1),
        # 3
        (black_background, dark_rest_duration),
        (black_with_stimulus, recording_duration),
        (rest_page, -1),
        (white_background, bright_rest_duration),
        (white_with_stimulus, recording_duration),
        # end
        (end_page, -1),
    ]

    # Window name
    window_name = "Stimulus Window"

    # Get the second monitor
    monitor = get_secondary_monitor()

    # Create a window with the size of the secondary monitor
    cv2.namedWindow(window_name, cv2.WINDOW_NORMAL)  # Create a resizable window
    cv2.resizeWindow(
        window_name, monitor.width, monitor.height
    )  # Set window size to monitor size

    # Move the window to the secondary monitor's position
    cv2.moveWindow(window_name, monitor.x, monitor.y)

    # Set the window to full screen
    cv2.setWindowProperty(window_name, cv2.WND_PROP_FULLSCREEN, cv2.WINDOW_FULLSCREEN)

    # Initialize variables
    current_state = 0

    # Main loop
    while current_state < len(states):
        image, duration_seconds = states[current_state]
        start_time = time.time()

        # Display the image
        cv2.imshow(window_name, image)

        # Loop until duration_seconds is elapsedd
        while True:
            if duration_seconds != -1:
                elapsed_time = time.time() - start_time
                remaining_time = max(
                    0, duration_seconds - elapsed_time
                )  # Ensure remaining time doesn't go negative

                # Print remaining time on the terminal
                sys.stdout.write(f"\rRemaining Time: {remaining_time:.2f} seconds")
                sys.stdout.flush()
            if (
                duration_seconds != -1
                and (time.time() - start_time) >= duration_seconds
            ):
                break

            key = cv2.waitKey(1) & 0xFF  # Ensure compatibility across platforms

            if key == 32 and duration_seconds == -1:
                break
            elif key == ord("q"):
                cv2.destroyAllWindows()
                exit()  # or break to exit only the inner loop
        current_state += 1
    # Clean up
    cv2.destroyAllWindows()


if __name__ == "__main__":
    # Hide the mouse pointer
    ctypes.windll.user32.ShowCursor(False)
    main()
