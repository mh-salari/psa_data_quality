"""
Author:       Mohammadhossein Salari
Email:        mohammadhossein.salari@gmail.com
Last Modified: 2024/08/29
Description:  A Python script to generate visual stimuli (cross and circles) with specific
              visual angles.
"""

from PIL import Image, ImageDraw
from pathlib import Path
from .visual_angle_converter import VisualAngleConverter


def generate_visual_stimulus(
    screen_width_mm,
    screen_height_mm,
    screen_width_px,
    screen_height_px,
    viewing_distance_mm,
    outer_circle_diameter_in_degrees,
    inner_circle_diameter_in_degrees,
    stimuli_color,
    save_path,
):
    """
    Generates a visual stimulus consisting of a cross and concentric circles based on
    the given screen parameters, viewing distance, and stimulus properties.

    Args:
        screen_width_mm (float): Width of the screen in millimeters.
        screen_height_mm (float): Height of the screen in millimeters.
        screen_width_px (int): Width of the screen in pixels.
        screen_height_px (int): Height of the screen in pixels.
        viewing_distance_mm (float): Distance between the viewer and the screen in millimeters.
        outer_circle_diameter_in_degrees (float): Diameter of the outer circle in degrees.
        inner_circle_diameter_in_degrees (float): Diameter of the inner circle in degrees.
        stimuli_color (tuple): Color of the stimuli in (R, G, B, A) format.
        save_path (str or Path): Path where the generated image will be saved.
    """

    # Initialize the visual angle converter
    converter = VisualAngleConverter(
        screen_width_pixels=screen_width_px,
        screen_height_pixels=screen_height_px,
        screen_width_mm=screen_width_mm,
        screen_height_mm=screen_height_mm,
        distance=viewing_distance_mm,
    )

    # Calculate the size of the target stimuli using the converter
    outer_diameter_px = int(
        converter.visual_angle_to_pixels(
            outer_circle_diameter_in_degrees, orientation="vertical"
        )
    )
    print(f"Outer circle:")
    print(f"  Diameter: {outer_diameter_px} pixels")
    print(f"  Diameter: {outer_circle_diameter_in_degrees:.2f} degrees visual angle")

    inner_diameter_px = int(
        converter.visual_angle_to_pixels(
            inner_circle_diameter_in_degrees, orientation="vertical"
        )
    )
    print(f"Inner circle:")
    print(f"  Diameter: {inner_diameter_px} pixels")
    print(f"  Diameter: {inner_circle_diameter_in_degrees:.2f} degrees visual angle")

    # Create a new image with a transparent background
    img_size = (
        outer_diameter_px + 2,
        outer_diameter_px + 2,
    )  # Add 1 pixel on each side
    img = Image.new("RGBA", img_size, (0, 0, 0, 0))
    print(f"Image size: {img.size[0]}x{img.size[1]} pixels")

    # Get drawing context
    draw = ImageDraw.Draw(img)

    # Calculate center coordinates
    cx, cy = img_size[0] // 2, img_size[1] // 2

    # Draw outer circle
    outer_radius = outer_diameter_px // 2
    draw.ellipse(
        [cx - outer_radius, cy - outer_radius, cx + outer_radius, cy + outer_radius],
        fill=stimuli_color,
    )

    # Create a mask for the cross
    cross_mask = Image.new("L", img_size, 0)
    cross_draw = ImageDraw.Draw(cross_mask)

    # Draw cross on the mask
    cross_length = outer_radius
    cross_width = inner_diameter_px
    cross_draw.line(
        [cx - cross_length, cy, cx + cross_length, cy], fill=255, width=cross_width
    )
    cross_draw.line(
        [cx, cy - cross_length, cx, cy + cross_length], fill=255, width=cross_width
    )

    # Use the mask to create a transparent cross in the main image
    transparent = Image.new("RGBA", img_size, (0, 0, 0, 0))
    img = Image.composite(transparent, img, cross_mask)

    # Draw inner circle
    inner_radius = inner_diameter_px // 2
    draw = ImageDraw.Draw(img)
    draw.ellipse(
        [cx - inner_radius, cy - inner_radius, cx + inner_radius, cy + inner_radius],
        fill=stimuli_color,
    )

    # Save the image in the 'resources' directory
    save_path = Path(save_path)
    save_path.mkdir(exist_ok=True)  # Create the directory if it doesn't exist
    save_path = save_path / "stimulus.png"

    img.save(save_path)
    print(f"Image saved as '{save_path}'")
