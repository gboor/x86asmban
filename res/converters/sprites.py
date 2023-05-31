from PIL import Image
import struct

# Color for transparency and empty background
TRANSPARENT = (0xFF, 0x00, 0xFF)
BACKGROUND = (0x00, 0x00, 0x00)

# Open the image and the output file
image = Image.open('../tiles_and_char_8bit.png')
rgba = image.convert('RGBA')
output = open('../sprites.bin', 'wb')

# Get the palette and write it as unsigned ints
palette = image.getpalette()

if palette is None:
    print('Cannot use images without a palette')
    exit(1)

palette_count = len(palette)
expected_count = 254 * 3  # 256 colors, 3 bytes per color, with 2 reserved for background and transparency

if palette_count > expected_count:
    print('Cannot use palettes with more than 254 colors')
    exit(1)

if palette_count < expected_count:
    # Pad it with 0s
    palette.extend([0] * (expected_count - palette_count))

# Add transparent and background
palette.extend(TRANSPARENT)
palette.extend(BACKGROUND)

# Mode 13h uses a palette of 18 bits per pixel, or 6 bits per color
# Remove the 2 least significant bits from all entries in the palette
palette = [x >> 2 for x in palette]

# Write it as unsigned bytes
output.write(struct.pack('B' * len(palette), *palette))

# Write the index of the transparent and background colors
output.write(struct.pack('BB', 254, 255))

# Get data and set transparent pixels (from rgba) to transparent idx 254
data = list(image.getdata())
transparency_data = rgba.getchannel('A').getdata()
data = [v if transparency_data[k] != 0 else 254 for k, v in enumerate(data)]

# Write the data itself as unsigned bytes
output.write(struct.pack('B' * len(data), *data))

# Done
output.close()
