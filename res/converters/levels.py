import struct

# These values are just included for skipping levels that would be too large.
# It is the tile size in pixels (width and height) and the total level size will be compared against its pixel
# size to make sure levels fit within the screen we will draw on in ASM.
TILE_SIZE = 16
SCREEN_WIDTH = 320
SCREEN_HEIGHT = 200

level_data = open('../original.txt', 'r').readlines()

levels = []
cur_level = None
for line in level_data:
    # Strip whitespaces and newlines
    line = line.rstrip()

    # If it's numeric, it's the start of a new level
    if line.isnumeric():
        # Push previous level
        if cur_level is not None:
            levels.append(cur_level)

        # Start a new level
        cur_level = {'width': 0, 'height': 0, 'data': [], 'player': [], 'boxes': []}
    else:
        # Update known information and parse the line
        if len(line) > cur_level['width']:
            cur_level['width'] = len(line)
        cur_level['height'] += 1

        # Replace all spaces with -
        # We will be using - to signify "empty space" as opposed to "floor"
        # At a later stage, we will figure out where the floors go
        line = line.replace(' ', '-')

        # Add each line
        cur_level['data'].append(list(line))

# Add the final level
levels.append(cur_level)

# Process all levels
filtered_levels = []
for level_idx, level in enumerate(levels):
    # Check if the level fits within a 320x200 pixel screen
    if level['width'] * TILE_SIZE > SCREEN_WIDTH:
        print(f'Level {level_idx} is too wide to fit, skipping')
        continue

    if level['height'] * TILE_SIZE > SCREEN_HEIGHT:
        print(f'Level {level_idx} is too high to fit, skipping')
        continue

    for row_idx, row in enumerate(level['data']):
        # Loop the row for special objects (player, boxes)
        for char_idx, char in enumerate(row):
            if char == '@' or char == '+':
                level['player'] = [char_idx, row_idx]

                # Replace with empty or goal
                row[char_idx] = '-' if char == '@' else '.'
            elif char == '$' or char == '*':
                level['boxes'].append([char_idx, row_idx])

                # Replace with empty or goal
                row[char_idx] = '-' if char == '$' else '.'

        # Pad all rows until they are equal length
        if len(row) < level['width']:
            row += (level['width'] - len(row)) * '-'

    # Resolve the floors
    # We assume here that the position of the player will always be on a floor, that all levels are completely
    # locked in by walls and that empty sealed spaces outside the playing field are not floors, but empty.
    # Start from the player and find all - tiles around it, moving recursively outwards.
    # This is a very suboptimal way of handling this and many coordinates will be hit many times,
    # but since it's only run once, and it works, it's good enough.
    processed = []

    def resolve_floors(c):
        global level, processed

        if c in processed:
            # This coordinate was already hit
            return

        processed.append(c)

        x = c[0]
        y = c[1]

        tile = level['data'][y][x]

        # if c is not an empty space or a target, stop
        if tile != '-' and tile != '.':
            return

        # This tile is a floor, unless it's a target
        if tile != '.':
            level['data'][y][x] = ' '

        # Process all tiles around c except diagonal ones (which are not reachable and can be empty)
        for new_x in [x - 1, x + 1]:
            if new_x < 0 or new_x >= level['width']:
                continue

            resolve_floors([new_x, y])

        for new_y in [y - 1, y + 1]:
            if new_y < 0 or new_y >= level['height']:
                continue

            resolve_floors([x, new_y])

    resolve_floors(level['player'])
    filtered_levels.append(level)

# Convert all levels to a binary format that starts with an index of all 16-bit words
#
# NUM_LEVELS
# OFFSET 1
# OFFSET 2
# ...
# OFFSET X
#
# Then, for each level at the given offset, in bytes as we won't need > 255 (probably)
#
# WIDTH
# HEIGHT
# PLAYER_X
# PLAYER_Y
# NUM_BOXES
# [DATA] (char array)
# [BOX COORDS] (array of x,y repeated)

level_count = len(filtered_levels)

# Calculate offsets from the top of the data block, so including the header
cur_offset = level_count * 2 + 2  # 2 bytes for each offset + 2 bytes for the level count

# Turn all levels into packed structs and calculate offsets
packed_levels = []
offsets = []
max_boxes = 0
for level in filtered_levels:
    # Flatten the data array
    level_data = [tile.encode('ascii') for row in level['data'] for tile in row]

    print(level['boxes'])

    box_count = len(level['boxes'])
    if box_count > max_boxes:
        max_boxes = box_count

    # Flatten the box array
    box_data = [coord for box in level['boxes'] for coord in box]

    data = struct.pack('BBBBB' + 'c' * len(level_data) + 'B' * len(box_data),
                       level['width'], level['height'], *level['player'], box_count, *level_data, *box_data)

    packed_levels.append(data)

    # Add the offset for this level
    offsets.append(cur_offset)
    cur_offset += len(data)

# Create the header - force little endian for x86
header = struct.pack('<H' + 'H' * level_count, level_count, *offsets)

# Write the whole blob
output = open('../levels.bin', 'wb')
output.write(header)

for level in packed_levels:
    output.write(level)

output.close()

print(f'Max box count: {max_boxes}')
