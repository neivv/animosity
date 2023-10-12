Animosity is a program to convert to/from SC:R graphic formats.

## File support status

- images.dat .anim
    - anim\main_xxx.anim, HD2\main_xxx.anim, SD\mainSD.anim are fully supported
        (To current knowledge of what supporting them means)
    - The skin anims Carbot/ and Presale/ are semi-supported,
        Animosity is currently not able to handle the different scales at once if the files
        are not located in anim - HD2\anim - SD\mainSD.anim, causing importing not work
        properly
        See ".anim" section for more details
- Tileset\foliage.anim does not support importing new graphics (Maybe? May work nowadays but untested)
    Importing new foliage graphics will cause SC:R to crash due to Animosity not
    handling some foliage-specific data correctly.
- anim\main.lit (Sprite lighting)
    Fully supported when editing images.dat anims, though lacks a preview for changes
    See "Sprite lighting" section for mode details
- images.rel (HD anim file relation list)
    Fully supported?
- .dds.grp files
    Fully supported
    They will require some tedious work of importing HD/HD2/SD variants one by one when
    working on them
- Tileset .dds.vr4 files
    Fully supported
    They're effectively same as .dds.grp, as such, editing them is probably quite tedious

## Framedef .json files

Since all of the graphic files have at least some metadata values that cannot be represented as a
image pixels, animosity creates a "Frame definition" .json file that will contain any such data
so that it will not be lost when reimporting.

As such, you generally want to export frames first to get a compatible framedef file, even
if your plan is to completely replace the graphic. Some variables can be also modified by editing
the file with a text editor. Note that .json as a file format is very nitpicky about requiring
commas after every line of an object/array expect the last, which in turn must not have a comma.

Importing settings that are only editable through framedef.json:
- "frame_count" to import different amount of images from what was exported.
    You'll also need to edit "multi_frame_images" -> "frame_count" for each layer if you
    exported all frames in a single image.
- "offset_x" and "offset_y" to import all frames with an additional offset.
    You can just move the frames further right/down with an image editing software to increase
    the offset without changing this, but this supports negative offsets which some vanilla
    sprites have.
- "layers" allows importing more/less layers.
    You can only have the layers which are already displayed by Animosity.
    Also note that "multi_frame_images" has data for each layer as well when importing all frames
    in a single image.
- "frame_types" Allows editing some unknown value that is set on by-frame basis.
    See ".anim frame types" below for slightly more details.

## .anim

While .anim files may seem like self-contained sprite atlases of possibly several layers,
they end up having dependencies on other graphic files. As such, frame importing has to
handle several files at once.

Anim inter-file requirements:
1) HD and HD2 anim must contain same graphics, HD2 being scaled to 50% of width/height
    (25% pixels) of HD. All layers should match and contain same frames at same positions
    on the texture atlas.
2) The main "Dimensions" values don't actually seem to be used?
    In HD they match what you would expect a frame dimension to be, HD2 uses same value as HD,
    and SD doesn't have them set at all.
    SD is confirmed to use dimensions from the .grp specified in images.dat / images.tbl for
    centering frames, and it seems that HD/HD2 also end up using the GRP values, but scaled
    2x / 4x.
3) The .grp files are used also to determine which pixels of a sprite are clickable.
    The SD pixels are used even in HD mode, making some vanilla sprites have unintuitive
    clickable pixels.

Animosity handles 1) automatically when importing HD/HD2 files, as long as the files
are placed in anim\ and HD2\anim respectively. While it is possible to import into anim
files one by one when Animosity doesn't find the correct directory structure, it is quite
difficult, possibly impossible with current importing setting, to handle correctly and is
not recommended.

Similarly 2) and 3) can be automatically handled by choosing to "Create GRP" when importing to SD
and when mainSD.anim is saved ad SD\mainSD.anim.
Note that this linked .grp is selected by arr\images.dat field (which in turn uses arr\images.tbl).
The default images.dat shares .grps with main unit and its shadow in several cases, if you end
up using a shadow images.dat entry for an unrelated graphic, remember to edit images.dat to
refer to a new .grp file instead. (Animosity needs to reopen files in order to detect changed
arr\images.dat/tbl)

## .anim layers

.anim files can have at most 7 layers, most of them only relevant when Real-time Lighting (RTL)
is turned on. It is valid to only ipmort diffuse (or diffuse + teamcolor) to avoid the hassle with
RTL layers. Animosity is a bit of clunky and does not have a neat interface for adding layers,
but editing framedef.json to add/remove layers which get imported will work.

- diffuse
    Main layer when RTL is disabled. The colors/alpha in diffuse get drawn without any
    further modification in game, team color applied on top of that if the teamcolor layer exists.
    Not used at all if RTL is enabled and RTL layers exist.
    This layer should always exist.
- teamcolor
    Determines which pixels are become painted by player color. The default shader uses the
    background pixel to determine how much the color should be dimmed. #ffffff white pixel in
    the base graphic results in team color applied without changes, #00ff80 would not apply
    red channel of team color at all, green fully, and blue at 50%.
    The teamcolor layer itself is expected 1bit monochrome bitmap, black not applying team color
    and white applying it. The layer could probably be a full 8bit monochrome bitmap for extra
    color information, though Animosity currently forces it to 1bit when importing.
    The shaders applying team color could also be modified to modify how the layers are blended.
- bright
    Main colors when RTL is enabled. This layer is intended to represent the colors when the
    pixels of a sprite is fully light from any direction.
- normal
    Determines surface normals for each pixel of the sprite (That is, which direction each pixel
    is "facing").
    Used to determine which pixels are lit when different lights hit the sprite.
    There is always a "global light" coming from the direction of the camera which light up pixels
    as usual, but sprite lighting can be used to create additional light sources coming from
    ground level.
    Normal layer only encodes X and Y components of the normal vector, with Z being calculated
    from X and Y, as the normal length is assumed to be 1.0.
    Since normal maps are usually edited in a format where X/Y/Z are all stored in R/G/B channels
    of a image, animosity supports decoding to/from X/Y - R/A format from/to X/Y/Z - R/G/B when
    selecting "Decode normals" when exporting graphics. Similarly there is an option to render
    the raw encoded image or an image with decoded normals, decoded normals being what SC:R
    will use in the end.
    - Technical details:
        As normal components are generally considered to be values between -1.0 and 1.0 (with the
        additional expectation that the final vector's length is 1.0), they are stored in a image
        as color value 0 representing -1.0, and 255 representing 1.0, other values being linearly
        interpolated in that range (E.g. 128 would be ~0.0, 64 would be -0.5, etc.)
        SC:R stores the X component on red channel, and Y on alpha channel using the above encoding,
        and calculates Z as sqrt(1.0 - x^2 - y^2), with an extra shader rule to at least have
        Z = sqrt(0.1) ~ 0.31.
        Then the resulting vector is normalized (Divided by its length; keeping direction but
        modifying component values so that vector length is 1.0)
- specular
    Determines strength of specular reflection for each pixel.
    In practice, white areas of the specular map get a "bright shiny spot" to light reflecting
    at a very specific angle, while darker areas have the effect reduced or completely removed
    if black.
- ao_depth
    This layer contains two separate values, ambient occlusion and depth.
    Ambient occlusion is stored in green channel of the layer, representing which areas of the
    sprite are covered by the sprite itself, making lighting less effective on them.
    (255 causes 100% light effectiveness, only depending on angle/normal in that case,
    reducing AO value causes the effectiveness linearly decrease to 0%.
    Depth is stored in alpha channel, pixels with larger value being closer to the screen.  It is
    used to determine pixel's Z coordinate which is necessary when calculating the angle which
    light is coming from.
    Animosity supports splitting ao_depth to two separate images when exporting and merging them
    back when importing. Adding color/alpha information to these images is meaningless, they should
    be kept grayscale. You can also preview the layers separately in the program.
- emissive
    "Internal light" from the sprite itself which does not affect any other sprites and gets added
    on top of other colors without modification when RTL is enabled.
    Use #000000 black to mark areas that should not have emissive light, the alpha channel is not
    read here.
    This layer is optional even if other RTL layers are used.


## .anim frame types

The .anim files contain an unknown "frame type" value for each of the frames. Animosity does not
directly show those values at the moment, but they can be edited by opening the framedef .json file
and importing after editing it.

It is completely unknown if the frame type has any meaning whatsoever. Observed values are
0, 1, 8, and 9, which would imply there being one flag 1 and another flag 8 which may be combined.
SD sprites are almost always fully 0, exception being sprite#0 which is fully 1
HD sprites use 1, 8, and 9, with normal sprites usually being 1 outside burrowing frames, and
shadow/explosion/spell (Transparent?) sprites using 8/9 for all of their frames.

- 1 = Use width / height in anim?
- 2 = Related to player color?

## Sprite lighting

Each frame of a sprite can be set to generate lighting when RTL is enabled.
This data is stored in anim\main.lit file, which must be extracted in order to edit lighting.
By default this is mostly enabled for explosion effects, but it can be added to any other sprite.
X/Y coordinates in this case are relative to first pixel of frame that is included in the texture,
using HD coordinates. E.g. position 1,1 would be 1 pixel right and 1 pixel down not from the
top left corner of exported image, but from the first row/column which is not completely
transparent for this frame. This means that modifying a sprite which has lighting data to use
more of the blank space left/top of the sprite would misalign the lighting position.
