import cv2
import numpy as np
import math


def create_calibration_image(
    current_width,
    current_height,
    native_width,
    native_height,
    ppi,
    background_color=(128, 128, 128),
):
    # Calculate scaling factor and effective DPI
    scaling_factor = current_width / native_width
    effective_dpi = int(ppi * scaling_factor)

    def mm_to_pixels(mm):
        return int(mm * effective_dpi / 25.4)

    # Set image size to match the current resolution
    image_size = (current_width, current_height)

    # Create background with specified color
    image = np.full((image_size[1], image_size[0], 3), background_color, dtype=np.uint8)

    # Calculate dimensions
    outer_diameter = mm_to_pixels(43)
    inner_diameter = mm_to_pixels(23)  # 43mm - (2 * 10mm)
    center_diameter = mm_to_pixels(3)  # Approximation for the center circle
    center = (image_size[0] // 2, image_size[1] // 2)

    # Calculate margin (2% of the outer circle diameter)
    margin = int(outer_diameter * 0.02)

    # Calculate the size of the white rectangle
    rect_size = outer_diameter + (2 * margin)
    rect_top_left = (center[0] - rect_size // 2, center[1] - rect_size // 2)
    rect_bottom_right = (center[0] + rect_size // 2, center[1] + rect_size // 2)

    # Draw white rectangle
    cv2.rectangle(image, rect_top_left, rect_bottom_right, (255, 255, 255), -1)

    # Draw outer black circle
    cv2.circle(image, center, outer_diameter // 2, (0, 0, 0), -1)

    # Draw inner white circle
    cv2.circle(image, center, inner_diameter // 2, (255, 255, 255), -1)

    # Draw center black circle
    cv2.circle(image, center, center_diameter // 2, (0, 0, 0), -1)

    return image


def calculate_ppi(width_pixels, height_pixels, diagonal_inches):
    # Using the Pythagorean theorem
    diagonal_pixels = (width_pixels**2 + height_pixels**2) ** 0.5

    # PPI = diagonal pixels / diagonal inches
    ppi = diagonal_pixels / diagonal_inches

    return round(ppi, 2)


# Example usage (can be commented out when importing the function)
if __name__ == "__main__":
    current_width, current_height = 1920, 1080
    native_width, native_height = 1920, 1080
    diagonal_inches = 21.5

    ppi = math.sqrt(current_width**2 + current_height**2) / diagonal_inches
    print(ppi)
    # Create calibration image with gray background
    calibration_image_gray = create_calibration_image(
        current_width,
        current_height,
        native_width,
        native_height,
        ppi,
        background_color=(255, 255, 255),
    )

    # Create a named window
    cv2.namedWindow("Full Screen Calibration", cv2.WINDOW_NORMAL)

    # Set the window to full screen
    cv2.setWindowProperty(
        "Full Screen Calibration", cv2.WND_PROP_FULLSCREEN, cv2.WINDOW_FULLSCREEN
    )

    while True:
        # Display the image
        cv2.imshow("Full Screen Calibration", calibration_image_gray)

        key = cv2.waitKey(1) & 0xFF  # Ensure compatibility across platforms

        if key == ord("q"):
            cv2.destroyAllWindows()
            break

    # # Save the image (for reference)
    # cv2.imwrite(
    #     "bullseye.png",
    #     calibration_image_gray,
    #     [cv2.IMWRITE_PNG_COMPRESSION, 0],
    # )
    # print("Calibration image saved as 'bullseye.png'")
