"""
Filename:     visual_angle_converter.py
Author:       Mohammadhossein Salari
Email:        mohammadhossein.salari@gmail.com
Last Modified: 2024/08/29
Description:  Utility module for converting between pixels, millimeters, and visual angles
              for display screens. Provides methods to compute visual angles for horizontal
              and vertical screen dimensions and conversions based on user-defined screen
              parameters and observer distance.
              
              The conversion results in this module have been verified using the SR Research Visual Angle Calculator:
              https://www.sr-research.com/visual-angle-calculator/
"""

import math


class VisualAngleConverter:
    def __init__(
        self,
        screen_width_pixels,
        screen_height_pixels,
        screen_width_mm,
        screen_height_mm,
        distance,
    ):
        # Screen parameters
        self.screen_width_pixels = screen_width_pixels
        self.screen_height_pixels = screen_height_pixels
        self.screen_width_mm = screen_width_mm
        self.screen_height_mm = screen_height_mm
        self.distance = distance

        # Calculating conversion factors
        self.width_pixels_per_mm = self.screen_width_pixels / self.screen_width_mm
        self.height_pixels_per_mm = self.screen_height_pixels / self.screen_height_mm
        self.width_mm_per_pixel = self.screen_width_mm / self.screen_width_pixels
        self.height_mm_per_pixel = self.screen_height_mm / self.screen_height_pixels

    def pixels_to_mm(self, pixels, orientation="horizontal"):
        """Converts pixels to millimeters for a given orientation (horizontal or vertical)."""
        if orientation == "horizontal":
            return pixels * self.width_mm_per_pixel
        elif orientation == "vertical":
            return pixels * self.height_mm_per_pixel
        else:
            raise ValueError(
                "Invalid orientation. Please use 'horizontal' or 'vertical'."
            )

    def mm_to_pixels(self, mm, orientation="horizontal"):
        """Converts millimeters to pixels for a given orientation (horizontal or vertical)."""
        if orientation == "horizontal":
            return mm * self.width_pixels_per_mm
        elif orientation == "vertical":
            return mm * self.height_pixels_per_mm
        else:
            raise ValueError(
                "Invalid orientation. Please use 'horizontal' or 'vertical'."
            )

    def pixels_to_visual_angle(self, pixels, orientation="horizontal"):
        """Converts a pixel size to a visual angle in degrees for a given orientation (horizontal or vertical)."""
        size_mm = self.pixels_to_mm(pixels, orientation)
        visual_angle = 2 * math.degrees(math.atan(size_mm / (2 * self.distance)))
        return visual_angle

    def visual_angle_to_pixels(self, angle, orientation="horizontal"):
        """Converts a visual angle in degrees to pixels for a given orientation (horizontal or vertical)."""
        angle_radians = math.radians(angle)
        size_mm = 2 * self.distance * math.tan(angle_radians / 2)
        pixels = self.mm_to_pixels(size_mm, orientation)
        return pixels


if __name__ == "__main__":

    # Example usage

    converter = VisualAngleConverter(
        screen_width_pixels=2560,
        screen_height_pixels=1440,
        screen_width_mm=596,
        screen_height_mm=335,
        distance=700,
    )

    print(converter.pixels_to_mm(380, orientation="horizontal"))
    print(converter.mm_to_pixels(100, orientation="vertical"))
    print(converter.pixels_to_visual_angle(380, orientation="horizontal"))
    print(converter.visual_angle_to_pixels(0.6, orientation="vertical"))
