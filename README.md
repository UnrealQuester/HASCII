# HASCII

HASCII is a command line tool to display images using plain old ascii characters. It supports a wide variety of formats, including BMP, JPG, PNG, GIF, and PSD.

The image gets converted to grayscale and then to a grid of characters that best represent the original image,
thanks to multi level image thresholding using Otsu's method.

# Usage

To view your image, simply type

    $ hascii myImage.png
```    
 ▒▒▒▒▒▒▒▒▒▒▒▒▒    ▒░░░░░░░░░░░░░                                           
  ▒▒▒▒▒▒▒▒▒▒▒▒▒     ░░░░░░░░░░░░░                                          
   ▒▒▒▒▒▒▒▒▒▒▒▒▒▒    ░░░░░░░░░░░░░                                         
     ▒▒▒▒▒▒▒▒▒▒▒▒▒    ░░░░░░░░░░░░░░                                       
      ▒▒▒▒▒▒▒▒▒▒▒▒▒     ░░░░░░░░░░░░░                                      
       ▒▒▒▒▒▒▒▒▒▒▒▒▒▓    ░░░░░░░░░░░░░                                     
         ▒▒▒▒▒▒▒▒▒▒▒▒▒    ░░░░░░░░░░░░░▓                                   
          ▒▒▒▒▒▒▒▒▒▒▒▒▒    ▓░░░░░░░░░░░░░                                  
           ▒▒▒▒▒▒▒▒▒▒▒▒▒     ░░░░░░░░░░░░░     ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
             ▒▒▒▒▒▒▒▒▒▒▒▒▒    ░░░░░░░░░░░░░▓    ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
              ▒▒▒▒▒▒▒▒▒▒▒▒▒    ░░░░░░░░░░░░░░    ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
               ▒▒▒▒▒▒▒▒▒▒▒▒▒     ░░░░░░░░░░░░░    ▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
                 ▒▒▒▒▒▒▒▒▒▒▒▒▒    ░░░░░░░░░░░░░                            
                  ▒▒▒▒▒▒▒▒▒▒▒▒▒    ░░░░░░░░░░░░░░                          
                ▒▒▒▒▒▒▒▒▒▒▒▒▒▒    ░░░░░░░░░░░░░░░░                         
               ▒▒▒▒▒▒▒▒▒▒▒▒▒     ░░░░░░░░░░░░░░░░░░     ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
              ▒▒▒▒▒▒▒▒▒▒▒▒▒    ░░░░░░░░░░░░░░░░░░░░░░    ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
             ▒▒▒▒▒▒▒▒▒▒▒▒▒    ░░░░░░░░░░░░░░░░░░░░░░░░    ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
           ▒▒▒▒▒▒▒▒▒▒▒▒▒     ░░░░░░░░░░░░░░░░░░░░░░░░░░     ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
          ▒▒▒▒▒▒▒▒▒▒▒▒▒    ▒░░░░░░░░░░░░░  ░░░░░░░░░░░░░                   
         ▒▒▒▒▒▒▒▒▒▒▒▒▒    ░░░░░░░░░░░░░      ░░░░░░░░░░░░░                 
       ▒▒▒▒▒▒▒▒▒▒▒▒▒     ░░░░░░░░░░░░░        ░░░░░░░░░░░░░                
      ▒▒▒▒▒▒▒▒▒▒▒▒▒     ░░░░░░░░░░░░░          ░░░░░░░░░░░░░               
     ▒▒▒▒▒▒▒▒▒▒▒▒▒    ░░░░░░░░░░░░░▓            ▓░░░░░░░░░░░░░             
   ▒▒▒▒▒▒▒▒▒▒▒▒▒▒    ░░░░░░░░░░░░░                ░░░░░░░░░░░░░            
  ▒▒▒▒▒▒▒▒▒▒▒▒▒     ░░░░░░░░░░░░░                  ░░░░░░░░░░░░░           
 ▒▒▒▒▒▒▒▒▒▒▒▒▒    ░░░░░░░░░░░░░░                    ░░░░░░░░░░░░░░         
```

# Options

    $ hascii [--characters=<chars>] [--width=<value>] [--height=<value>] [--fit-mode=<mode>] [FILENAME]
    
    
## Filename

The name of the file to open. Supported image formats are BMP, JPG, PNG, GIF, and PSD. If no filename is given, the tool reads from stdin instead. Images are completly loaded into memory before processing.

## characters

Short: w

Default: █▓▒░

What characters to use to display the image. Should be exactly 4. Missing chacaters will be replaced by spaces and extra characters will be ignores. Feel free to experiment. UTF-8 is supported as long as your terminal supports it.

## width

Short: w

Default: none

Sets the width of the displayed image in characters. If omitted, the width is set to the width of the terminal. When the terminal size can not be calculated (for example when it is part of a script or pipe) it is set to 75. See fit-mode for details. Please note that for most fonts and font sizes a character is about twice as high as it is wide. For this reason, the aspect ratio gets adjusted before it is displayed. A quadratic image will have twice as many columns as it has rows.

## height

Short: h

Default: none

Sets the height of the displayed image in characters. If omitted, the height is calculated automatically form the width and aspect ratio of the image. See fit-mode for details. Please note that for most fonts and font sizes a character is about twice as high as it is wide. For this reason, the aspect ratio gets adjusted before it is displayed. A quadratic image will have twice as many columns as it has rows.

## fit-mode

Possible values: width, height, original, smallest

Default: width

The image can be stretched to fit the entire width or height of the terminal with the fit-modes width and height respectively. With original, the image will not be resized at all. One character represents 1x2 pixels. The image will be strechted to that the aspect ratio does not change. In all three aforementioned modes, the width and height command line parameters will be ignored. When both the width and the height if given, the image will have exactly that size, regardless of the fit-mode. With smallest, the image will be made to fit the given width or height, depending on what is smaller, or the widht or height of the terminal, depending on what is smaller.
