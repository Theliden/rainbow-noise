# rainbow-noise
Random generation of smooth, colourful graphics.

![example image](sample.png?raw=true)

## Notes on Usage

- the main Haskell file to compile is ```Main.hs```
- the program takes 3 integer arguments:
  - the height of the image in pixels
  - the width of the image in pixels
  - a random seed
- output is to standard output in the PPM image format
- it is recommended to redirect the output into a ppm file, then run another program to convert the result into a different format
- this process may be simplified in the future

## Planned Extensions

Support for looping gif animations is coming soon.
