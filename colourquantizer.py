import numpy as np
from skimage.io import imread, imshow
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
import json

# Predefined colors (32 colors)
PREDEFINED_COLORS = np.array([
    [0, 0, 0], 			[0, 0, 255], 		[0, 255, 0], 		[255, 0, 0],
    [255, 255, 0], 		[255, 0, 255], 		[0, 255, 255], 		[255, 255, 255],
    [0, 170, 255], 		[85, 170, 255], 	[170, 170, 255],	[0, 85, 255],
    [85, 0, 255], 		[85, 85, 255], 		[170, 85, 255], 	[170, 0, 255],
    [0, 255, 170], 		[85, 255, 170], 	[170, 255, 170],	[0, 255, 85],
    [85, 255, 0],		[85, 255, 85],		[170, 255, 85],		[170, 255, 0],
    [255, 0, 170],	 	[255, 85, 170],		[255, 170, 170], 	[255, 0, 85],
    [255, 85, 0],		[255 ,85 ,85],		[255 ,170 ,85],		[255 ,170 ,0]
])

def median_cut_quantize(img_arr):
    # Reshape the image array to a list of pixels
    pixels = img_arr.reshape(-1, img_arr.shape[-1])
    
    # Combine image pixels with predefined colors
    combined_pixels = np.vstack((pixels.astype(int), PREDEFINED_COLORS))
    
    # Perform KMeans clustering to find dominant colors
    kmeans = KMeans(n_clusters=256 - len(PREDEFINED_COLORS)) # Remaining slots for image colors
    kmeans.fit(combined_pixels)
    
    # Get the cluster centers (dominant colors)
    dominant_colors = kmeans.cluster_centers_.astype(int)
    
    # Combine predefined colors with dominant colors from the image
    total_palette = np.vstack((PREDEFINED_COLORS.astype(int), dominant_colors))
    
    return total_palette

def quantize_image(image_path):
    img = imread(image_path)
    
    # Ensure the image is in RGB format
    if img.shape[-1] == 4: # Handle RGBA images by removing alpha channel
        img = img[:, :, :3]
    
    quantized_palette = median_cut_quantize(img)

    scaled_palette = (quantized_palette // 4).astype(np.uint8)

    # Create a mapping from original pixel values to quantized palette
    pixel_indices = np.zeros((img.shape[0], img.shape[1]), dtype=int)
    
    # Assign each pixel to the nearest color in the quantized palette
    for i in range(img.shape[0]):
        for j in range(img.shape[1]):
            distances = np.linalg.norm(scaled_palette - img[i][j] // 4, axis=1)
            pixel_indices[i][j] = np.argmin(distances)


    # Save the palette as an array in JSON format
    palette_output_path = 'palette.json'
    image_output_path = 'image.json'
    flattened_palette = scaled_palette.tolist()
    simple_palette = []
    for i in range(256 - 32):
        simple_palette += flattened_palette[i + 32]
    
    conversion_palette = []
    for i in range(672):
        conversion_palette += f'{(simple_palette[i]):06b}'
    
    conv2_palette = []
    for i in range(len(conversion_palette)):
        conv2_palette.append(int(conversion_palette[i]))
    
    final_palette = []
    for i in range(len(conv2_palette) // 8):
        final_palette.append((conv2_palette[8 * i + 0]<<7) | (conv2_palette[8 * i + 1]<<6) | (conv2_palette[8 * i + 2]<<5) | (conv2_palette[8 * i + 3]<<4) | (conv2_palette[8 * i + 4]<<3) | (conv2_palette[8 * i + 5]<<2) | (conv2_palette[8 * i + 6]<<1) | (conv2_palette[8 * i + 7]<<0)) 
    
    with open(palette_output_path, 'w') as f:
        json.dump(final_palette, f)

    # Create quantized image using the indices and palette
    quantized_image = (scaled_palette[pixel_indices] * 4).astype(np.uint8)

    with open(image_output_path, 'w') as f:
        json.dump(pixel_indices.astype(np.uint8).flatten().tolist(), f)

    # Display original and quantized images along with the palette
    plt.figure(figsize=(12,6))
    
    plt.subplot(1,3,1)
    plt.imshow(img)
    plt.title('Original Image')
    
    plt.subplot(1,3,2)
    plt.imshow(quantized_image.astype(np.uint8))
    plt.title('Quantized Image')

    plt.subplot(1,3,3)
    # Reshape the palette to display as pixels
    num_colors = len(scaled_palette) # Total number of colors
    palette_width = int(np.sqrt(num_colors)) + (num_colors % int(np.sqrt(num_colors)) > 0) # Ensure enough width
    palette_height = num_colors // palette_width + (num_colors % palette_width > 0) # Calculate height
    
    reshaped_palette = np.zeros((palette_height * palette_width ,3), dtype=np.uint8) # Create empty array for reshaping
    reshaped_palette[:num_colors] = scaled_palette * 4 # Fill with quantized colors
    
    reshaped_palette_image = reshaped_palette.reshape((palette_height,palette_width,-1)) # Reshape into grid format
    
    plt.imshow(reshaped_palette_image.astype(np.uint8), aspect='auto', origin='upper') # Display as pixels
    plt.title('Quantized Palette (256 Colors)')
    
    plt.axis('off')
    plt.show()

# Example usage	
#quantize_image('krazy.png')
#quantize_image('joke.png')
quantize_image('jumpscare.jpg')