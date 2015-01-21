# HASCII

HASCII is a command line tool to display images using plain old ascii characters. It supports wide variety of formats,
including BMP, JPG, PNG, GIF, and PSD.

The image gets converted to grayscale and then to a grid of characters that best represent the original image,
thanks to multi level image thresholding using Otsu's method.

# Usage

To view your image, simply type

    $ hascii myImage.png
  
The width of the image depends on the width of your terminal. Script makers can supply a width with the -w flag.
