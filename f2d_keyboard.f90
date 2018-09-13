!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_keyboard.f90 *
!  *                  *
!  ********************
 
!+ F2D_KEYBOARD - FIT 2-D KEY-BOARD interface
     Subroutine F2D_KEYBOARD (fit2d_version, gui, output_graphics, &
       shared_memory, memory_id, input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, log_file_open, xmaxdat, &
       ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       input_options, xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, &
       mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, &
       mzlabel, mx_pixel_size, my_pixel_size, results, status)
!       max_vec_values, max_vectors, &
!       num_vectors, STR_VECTORS, END_VECTORS, VECTOR_TITLES, status)
!  Description:
!    Interactive keyboard menu interface to FIT2D
!  Keywords:
!    FIT2D.Keyboard.Interface, Interface.Keyboard.FIT2D
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    09-Dec-2014: V1.21 Use result vectors data structure (Hammersley)
!    21-Sep-2011: V1.20 Add "main" and "memory" (Hammersley
!    05-May-2009: V1.19 Add "ZZ_TEST" command (Hammersley)
!    12-Jun-2008: V1.18 Investigating crash on Linux macros (Hammersley)
!    26-Sep-2007: V1.17 Add "LOAD MASK" command (Hammersley)
!    10-Nov-2006: V1.16 Add "STOP WATCH" command (Hammersley)
!    24-Apr-2006: V1.15 Changes to "FIO_INPUT" (Hammersley)
!    20-Mar-2006: V1.14 Add "INPUT OPTIONS" support (Hammersley)
!    10-Mar-2006: V1.13 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    12-Jan-2005: V1.12 Add "LOAD GEOMETRY" command (Hammersley)
!    11-Jan-2005: V1.11 Add "SAVE GEOMETRY" command (Hammersley)
!    26-Jul-2004: V1.10 Add "DRAW CAKE" command (Hammersley)
!    19-Jul-2004: V1.9 Add "SET PIXEL VALUE" command (Hammersley)
!    10-Mar-2004: V1.8 Change use of "VECTORS" array (Hammersley)
!    27-Feb-2004: V1.7 Add "BLOCK COPY" command to keyboard menu
!      for more efficient access to internal memories (Hammersley)
!    23-Feb-1999: V1.6 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V1.5 Change to use IO internal database routines (Hammersley)
!    29-Nov-1998: V1.4 Changes to "F2D_REFLECT" argument list (Hammersley)
!    28-Oct-1998: V1.3 Add support for "VECTORS" arrays (Hammersley)
!    20-Oct-1998: V1.2 Menu choices separated into "F2D_KEYSUB" (Hammersley)
!    19-Oct-1998: V1.1 "F2D_VARIABLE" replaced by "IO_VARIABLE" (Hammersley)
!    21-Aug-1998: V1.0 Change "SYMBOL" commands and calls to
!      "VARIABLE" commands and calls (Hammersley)
!    03-Jun-1998: V0.29 Add current input file name to argument list
!      of "F2D_OUTPUT" (Hammersley)
!    12-Mar-1998: V0.28 Add "EXTEND" command (Hammersley)
!    03-Mar-1998: V0.27 Use "F2D_ROTATELUT" to rotate look-up table (Hammersley)
!    22-Jan-1998: V0.26 Changes to the argument list of
!      "F2D_PRINT" and "IO_OPEN_OUTMACRO" (Hammersley)
!    21-Jan-1998: V0.25 Remove "Z-DIFFERENCES" command (Hammersley)
!    22-Dec-1997: V0.24 Correct text of "EXIT" confirmation message (Hammersley)
!    30-Nov-1997: V0.23 Add "HISTOGRAM" command (Hammersley)
!    29-Sep-1997: V0.22 Correct argument list for
!      "GS_INP_ROTATELUT" which lead to a core dump (Hammersley)
!    04-Sep-1997: V0.21 "TRANSPOSE" command now returns success
!      status of not (Hammersley)
!    13-Jun-1997: V0.20 Add "POLARISATION EFFECT" command (Hammersley)
!    13-Mar-1997: V0.19 Re-instate "POWER SPECTRUM" command
!      and add "AUTOCORRELATION" command (Hammersley)
!    01-Mar-1997: V0.18 Pass name of input file to calibration sub-menu 
!      (Hammersley)
!    20-Feb-1997: V0.17 Transfer name of input file between interfaces 
!      (Hammersley)
!    04-Feb-1997: V0.16 Make "RAISE TO A POWER" use the keyboard interface 
!      (Hammersley)
!    14-Jan-1997: V0.15 Add "GUI" command (Hammersley)
!    11-Jan-1997: V0.14 Add "PREDICTOR" and "ENTROPY" commands (Hammersley)
!    30-Nov-1996: V0.13 Re-instate "SURFACE INTERPOLATION" option.
!      Add "CHANGES" command (Hammersley)
!    16-Nov-1996: V0.12 Temporary remove "SURFACE INTERPOLATION" option 
!      (Hammersley)
!    21-Oct-1996: V0.11 Output information message when returning to
!      graphics window (Hammersley)
!    09-Oct-1996: V0.10 Add "I2C" and "BANNER" commands (Hammersley)
!    08-Oct-1996: V0.9 Add "CONCATENATION" and "DEDUCE FILE SEQUENCE" commands 
!      (Hammersley)
!    03-Oct-1996: V0.8 Set default print output type (Hammersley)
!    04-Sep-1996: V0.7 GUI option for "F2D_LOGARITHM". Add "MESSAGE" command 
!      (Hammersley)
!    29-Aug-1996: V0.6 Add "SLEEP" command (Hammersley)
!    17-Apr-1996: V0.5 Add "QUESTION" command (Hammersley)
!    11-Apr-1996: V0.4 Save change of colour table into internal data-base 
!      (Hammersley)
!    08-Mar-1996: V0.3 Add "AXES SCALES" command (Hammersley)
!    21-Feb-1996: V0.2 Changes to "F2D_MOVE" (Hammersley)
!    15-Feb-1996: V0.1 Original, based on old "fit2d" main (Hammersley)
!  Modules:
!  Use IO_LIB
     Use MA_LIB
     Use LG_LIB
     Use GS_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'io_db.inc' ! I/O data-base
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import:
     Character(Len = *), Intent(IN) :: fit2d_version ! Version of fit2d
!      being run
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used
     Logical, Intent(IN) :: output_graphics ! .True., if graphics are to be
!      output
     Logical, Intent(IN) :: shared_memory ! .True., if shared memory is to used
     Integer, Intent(IN) :: memory_id ! Identifier of shared memory (if used)
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file ! Name of current data
!      input file
     Logical, Intent(INOUT) :: data_defined ! .True., if data exists within
!      the program
     Logical, Intent(INOUT) :: memory_exist ! .True. if memory array exists
     Logical, Intent(INOUT) :: memory_defined ! .True. if the memory
!      contains data
     Logical, Intent(INOUT) :: variance_exist ! .True., if a data variance
!      array is created
     Logical, Intent(INOUT) :: mask_exist ! .True., if the mask array exists
     Logical, Intent(INOUT) :: log_file_open ! .True., if a log file is open
     Integer, Intent(INOUT) :: xmaxdat ! Dimension size in X-direction for
!      data arrays
     Integer, Intent(INOUT) :: ymaxdat ! Dimension size in Y-direction for
!      data arrays
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements defined in
!      Y-direction
!    Real X_AXIS(xmaxdat) ! X-axis values
!    Real Y_AXIS(ymaxdat) ! Y-axis values
!    Real DATA(xmaxdat, ymaxdat) ! The data values
!    Real VARIANCES(xmaxdat, ymaxdat) ! The estimated variance values
     Character(Len = *), Intent(INOUT) :: title ! Title for plot
     Character(Len = *), Intent(INOUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: zlabel ! Z-axis label for plot
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Type(INPUT_OPTIONS_STRUCTURE), Intent(INOUT) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
     Integer, Intent(INOUT) :: xstrelm ! The X-pixel for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! The Y-pixel for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! The X-pixel for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! The Y-pixel for the end of the ROI
     Integer, Intent(INOUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(INOUT) :: mynumdat ! Defines Y-extent of data region
!    Real MX_AXIS(xmaxdat) ! Array containing data X-coordinates
!    Real MY_AXIS(ymaxdat) ! Array containing data Y-coordinates
!    Real MDATA(xmaxdat, ymaxdat) ! Array containing data to be fitted
!    Real MVARIANCES(xmaxdat, ymaxdat) ! Array containing variances in
!    the data values
     Integer, Intent(INOUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(INOUT) :: mxstrelm ! Start X-element of memory data region
     Integer, Intent(INOUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(INOUT) :: mystrelm ! Start Y-element of memory data region
     Character(Len = *), Intent(INOUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(INOUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(INOUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(INOUT) :: mzlabel ! Z-axis label for data
!     Type(IMAGE_DATA), Intent(OUT) :: main ! Main data-set
!     Type(IMAGE_DATA), Intent(OUT) :: memory ! Memory data-set
     Real, Intent(INOUT) :: mx_pixel_size ! Size of a pixel in the memory
!      data in the X-direction (metres)
     Real, Intent(INOUT) :: my_pixel_size ! Size of a pixel in the memory
!      data in the Y-direction (metres)
     Type(RESULT_VECTORS), Intent(INOUT) :: results ! Result vectors
!     Integer, Intent(INOUT) :: max_vec_values ! First dimension of "VECTORS"
!      array (maximum number of values which can be stored in a vector)
!     Integer, Intent(INOUT) :: max_vectors ! Second dimension of "VECTORS"
!      array (maximum number of vectors which can be stored)
!     Integer, Intent(INOUT) :: num_vectors ! Number of values defined in the
!      "time"-series for each vector
!     Integer, Intent(INOUT) :: STR_VECTORS(max_vectors) ! Starting defined
!      element for "VECTORS"
!     Integer, Intent(INOUT) :: END_VECTORS(max_vectors) ! End defined
!      elements for "VECTORS"
!    Real VECTORS(max_vec_values, max_vectors) ! Multiple 1-D arrays of
!      vector values
!     Character(Len = *), Intent(INOUT) :: VECTOR_TITLES(max_vectors)
!      Titles for the 1-D data-sets
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V1.21' ! Version number
     Integer, Parameter :: Max_menu = 138 ! Number of instructions in the menu
!  Local Variables:
     Character(Len = 80) :: command ! Full command corresponding to user
!      request
     Character(Len = 20) :: display_command = 'IMAGE' ! Last used display
!      option ("IMAGE" or "PLOT")
     Character(Len = 20) :: print_type ! Type, of graphics to print:
!      supported types are:
!        "banner"
!        "image"
!        "contour"
!        "x/y graph"
     Integer :: item ! Loop variable for array items
     Integer :: num_menu ! Number of choices available in the menu
     Integer :: temp_status ! Temporary version of "status"
     Integer :: warning ! Type of warning:
!      0 = No warning, everything fine
!      1 = Data not defined
!      2 = Memory not defined at all
!      3 = Memory not defined throughout whole of ROI
!      4 = Memory arrays do not exist
!      5 = No graphics
!      6 = Variance arrays do not exist
     Logical :: continue ! .True., whilst the user wishes to continue
!      with the program
     Logical :: reset ! .True., if "status" is to be reset to the good
!      status value
     Logical :: resize ! .True., if the graphics output window has been
!      re-sized
!  Local Arrays:
     Character(Len = 80) :: ERROR(10) ! Buffer for error messages
     Character(Len = 40) :: MENU(Max_menu) ! Array containing menu choices
     Character(Len = 80) :: MENUTXT(Max_menu) ! Text to explain menu choices
!  External Functions:
!    Internal Functions:
!  Local Data:
     Data (MENU(item), item = 1, 10) / '?', 'ADD', 'ANNOTATION LABEL', &
       'ASPECT RATIO', 'AUTOCORRELATION', 'AXES SCALES', 'BANNER', &
       'BLOCK COPY', 'BLUR', 'BRAGGS'' EQUATION' /
     Data (MENU(item), item = 11, 20) / 'CADD', 'CALCULATOR', 'CALIBRATION', &
       'CDIVIDE', 'CHANGES', 'CLEAR DATA', 'CLOSE LOG', 'CMULTIPLY', &
       'COLOUR TABLE', 'CONCATENATION' /
     Data (MENU(item), item = 21, 30) / 'CONTOUR PLOT', 'CREATE DATA', &
       'CURVE STYLES', 'DEDUCE FILE SEQUENCE', 'DEFINE VARIABLE', &
       'DIFFRACTION PATTERN', 'DIMENSIONS', 'DISPLAY LIMITS', 'DIVIDE', &
       'DRAW CAKE' /
     Data (MENU(item), item = 31, 40) / 'END GRAPHICS FILE', 'ENTROPY', &
       'EXCHANGE', 'EXIT', 'EXTEND', 'FAST IMAGE', 'FILTER', 'FIT', 'FLIP', &
       'FONT' /
     Data (MENU(item), item = 41, 50) / 'FUJI LINEARISATION', 'FULL REGION', &
       'GAUSSIAN', 'GEOMETRY (EXPERIMENT)', 'GRID', 'GUI', 'HELP', &
       'HISTOGRAM', 'I2C', 'IMAGE' /
     Data (MENU(item), item = 51, 60) / 'INFORMATION', 'INPUT DATA', &
       'INTERNAL MEMORY', 'LINEARISE FILM', 'LIST VARIABLES', 'LOAD GEOMETRY', &
       'LOAD MASK', 'LOGARITHM', 'MACRO', 'MEDIAN FILTER' /
     Data (MENU(item), item = 61, 70) / 'MESSAGE', 'MOVE/ROTATE', 'MULTIPLY', &
       'NORMALISE', 'OFFSET/SCALE', 'OPEN LOG', 'OUTPUT DATA', &
       'PAGE POSITION', 'PAUSE', 'PEEP' /
     Data (MENU(item), item = 71, 80) / &
       'PIXEL REGION', &
       'PLOT DATA', &
       'POISSONIAN NOISE', &
       'POLARISATION EFFECT', &
       'POSTSCRIPT OPTIONS', &
       'POWER SPECTRUM', &
       'PREDICTOR', &
       'PRINT GRAPHICS', &
       'PUBLICATION QUALITY', &
       'QUESTION' /
     Data (MENU(item), item = 81, 90) / &
       'QUIT', &
       'RAISE TO A POWER', &
       'REBIN', &
       'RE-BIN', &
       'RECALL', &
       'REFLECT', &
       'REGION', &
       'RING (ADD POWDER RING)', &
       'ROI', &
       'ROTATE LUT' /
     Data (MENU(item), item = 91, 100) / &
       'RUN MACRO', &
       'SAVE GEOMETRY', &
       'SELECT PIXEL OPERATION', &
       'SEQUENCE', &
       'SET ANNOTATION STYLE', &
       'SET ARROW STYLE', &
       'SET AXES STYLE', &
       'SET BACKGROUND STYLE', &
       'SET COLOURS', &
       'SET CURVE STYLES' /
     Data (MENU(item), item = 101, 110) / &
       'SET ENUMERATION STYLE', &
       'SET FONT', 'SET GRID STYLE', &
       'SET LAYOUT STYLE', &
       'SET PIXEL VALUE', &
       'SET TICK POSITIONS', &
       'SET TITLE STYLE', &
       'SET X-LABEL STYLE', &
       'SET Y-LABEL STYLE', &
       'SLEEP' /
     Data (MENU(item), item = 111, 120) / &
       'SMOOTH', & 
       'SPATIAL FILTERING', &
       'START MACRO', &
       'STATISTICS', &
       'STOP MACRO', &
       'STOP WATCH', &
       'STORE', &
       'SUBTRACT', &
       'SURFACE INTERPOLATION', &
       'SYMBOL' /
     Data (MENU(item), item = 121, 130) / &
       'SYMMETRIC FUNCTION', &
       'TITLE', &
       'THRESHOLD', &
       'TRANSPOSE', &
       'UN-DEFINE VARIABLE', &
       'UNIT CELL PARAMETERS', &
       'VARIABLE', &
       'VARIANCES DEFINITION', &
       'V2C', &
       'WAIT' &
       /
     Data (MENU(item), item = 131, 138) / &
       'WEIGHTED AVERAGE', &
       'X-AXIS LABEL', &
       'Y-AXIS LABEL', &
       'Z-AXIS LABEL', &
       'Z-SCALE', &
       'ZZ_TEST', &
       '1-D INTERPOLATION', &
       '3-D SURFACE PLOT' /
 
     Data (MENUTXT(item), item = 1, 10) / &
       '?: List of available commands and functions', &
       'ADD: Add region of interest (ROI) of memory to current data', &
       'ANNOTATION LABEL: Define annotation labels for graphics', &
       'ASPECT RATIO: Control automatic aspect ratio display or not', &
       'AUTOCORRELATION: Calculate autocorrelation function of ROI', &
       'AXES SCALES: Change X and Y axis scales (start and increment)', &
       'BANNER: Draw fit2d banner', &
       'BLOCK COPY: Copy data block from and to internal memories', &
       'BLUR: Blur data by a top hat convolution', &
       'BRAGGS'' EQUATION: Variety of uses of Braggs equation' /
     Data (MENUTXT(item), item = 11, 20) / &
       'CADD: Add constant to region of interest (ROI)', &
       'CALCULATOR: Reverse Polish notation calculator', &
       'CALIBRATION: Calculate or apply calibration functions', &
       'CDIVIDE: Divide by a constant value the region of interest', &
       'CHANGES: Details of important changes in FIT2D versions', &
       'CLEAR DATA: Set region of interest (and variances) to zero', &
       'CLOSE LOG: Close log file', &
       'CMULTIPLY: Multiply region of interest by a constant', &
       'COLOUR TABLE: Choice of colour table and index range', &
       'CONCATENATION: Concatenate two strings and save in a variable' /
     Data (MENUTXT(item), item = 21, 30) / &
       'CONTOUR PLOT: Graphic display of region of interest', &
       'CREATE DATA: Define blank data region (for simulation)', &
       'CURVE STYLES: Set attributes for curve representations', &
       'DEDUCE FILE SEQUENCE: Deduce components of file sequence', &
       'DEFINE VARIABLE: Define variable and its corresponding value', &
       'DIFFRACTION PATTERN: Predict part of diffraction pattern', &
       'DIMENSIONS: Change dynamic array dimensions', &
       'DISPLAY LIMITS: Set maximum number of displayable pixels', &
       'DIVIDE: DIVIDE ROI of current data by the memory', &
       'DRAW CAKE: Draw cake on top of graphics' /
     Data (MENUTXT(item), item = 31, 40) / &
       'END GRAPHICS FILE: Close previously opened PostScript file', &
       'ENTROPY: Calculate image entropy and frequency statistics', &
       'EXCHANGE: Exchange current data with the memory contents', &
       'EXIT: Exit from program', &
       'EXTEND: Extend defined data region by user defined size', &
       'FAST IMAGE: Fast output of an image to PostScript', &
       'FILTER: Fourier Filtering of data with sharp cut-off filter', &
       'FIT: Model fitting, also detector tilt, and powder integration', &
       'FLIP: Reflect data through horizontal or vertical middle', &
       'FONT: Select text font to be used for all roman text' /
     Data (MENUTXT(item), item = 41, 50) / &
       'FUJI LINEARISATION: Convert intensities to linear scale', &
       'FULL REGION: Set region of interest to full data region', &
       'GAUSSIAN: Add 2-D Gaussian to data', &
       'GEOMETRY (EXPERIMENT): Define experimental geometry', &
       'GRID: Define graphics grid requirements', &
       'GUI: "Graphical user interface" scientific interfaces menu', &
       'HELP: User help and information', &
       'HISTOGRAM: Calculate pixel value frequency histogram', &
       'I2C: Convert integer to character representation (for macros)', &
       'IMAGE: Interactive control of image display' /
     Data (MENUTXT(item), item = 51, 60) / &
       'INFORMATION: Information on the internal state of FIT2D', &
       'INPUT DATA: Input data from data file', &
       'INTERNAL MEMORY: Store current ROI in special program memory', &
       'LINEARISE FILM: Non-linearity corrections to film data', &
       'LIST VARIABLES: List defined variables, types, and values', &
       'LOAD GEOMETRY: Input expt. geometry from a previous file', &
       'LOAD MASK: Input mask from previously saved mask file', &
       'LOGARITHM: Take logarithm base 10 of all elements in ROI', &
       'MACRO: Run previously saved macro definition file', &
       'MEDIAN FILTER: Filter by taking median value within window' /
     Data (MENUTXT(item), item = 61, 70) / &
       'MESSAGE: Define a message for output during a macro', &
       'MOVE/ROTATE: Move and or rotate image, output in memory', &
       'MULTIPLY: Multiply ROI of memory to the current data', &
       'NORMALISE: Normalise ROI; divide by maximum value within ROI', &
       'OFFSET/SCALE: Calculate offset and scale between two images', &
       'OPEN LOG: Open log file for record of input and output', &
       'OUTPUT DATA: Output data to named data file', &
       'PAGE POSITION: Set position of data display region on page', &
       'PAUSE: Wait for a user return (including within macros)', &
       'PEEP: Look at pixel coordinates and value' /
     Data (MENUTXT(item), item = 71, 80) / &
       'PIXEL REGION: Change region of interest using pixel limits', &
       'PLOT DATA: Plot data as 2-D image (or X-Y graph if 1-D)', &
       'POISSONIAN NOISE: Add Poissonian noise to data', &
       'POLARISATION EFFECT: Apply polarisation effect to intensities', &
       'POSTSCRIPT OPTIONS: Controls options affecting PostScript', &
       'POWER SPECTRUM: Calculate power spectrum of ROI', &
       'PREDICTOR: Applied a predictor algorithm to image', &
       'PRINT GRAPHICS: Output graphics to a file for printing', &
       'PUBLICATION QUALITY: Set attributes for high quality output', &
       'QUESTION: Ask an interactive question during a macro' /
     Data (MENUTXT(item), item = 81, 90) / &
       'QUIT: Exit from program', &
       'RAISE TO A POWER: Raise elements in ROI to specified power', &
       'REBIN: Rebin data, output in memory (same as "RE-BIN")', &
       'RE-BIN: Re-bin data, output in memory', &
       'RECALL: Recall data set from internal memory', &
       'REFLECT: Reflect data about input line, output in memory', &
       'REGION: Change region of interest', &
       'RING (ADD POWDER RING): Add powder diffraction ring to data', &
       'ROI: Defined Region Of Interest (Region Of Interest) (pixels)', &
       'ROTATE LUT: Interactive rotation of the colour table' /
     Data (MENUTXT(item), item = 91, 100) / &
       'RUN MACRO: Run previously saved macro definition file', &
       'SAVE GEOMETRY: Store current geometrical values in a file', &
       'SELECT PIXEL OPERATION: Operation on defined pixel value range', &
       'SEQUENCE: Run macro for a sequence of files (or not)', &
       'SET ANNOTATION STYLE: Set style of annotation label text', &
       'SET ARROW STYLE: Set style of an arrow; style, colour, etc.', &
       'SET AXES STYLE: Set style of axes, line width colour', &
       'SET BACKGROUND STYLE: Define background colour', &
       'SET COLOURS: Set colour of graph lines, text, and markers', &
       'SET CURVE STYLES: Set attributes for curve representations' /
     Data (MENUTXT(item), item = 101, 110) / &
       'SET ENUMERATION STYLE: Set style for axis numbering', &
       'SET FONT: Select text font to be used for all roman text', &
       'SET GRID STYLE: line type, colour, and width for grid', &
       'SET LAYOUT STYLE: Graphics title, and axis label distances', &
       'SET PIXEL VALUE: Set value of defined pixel', &
       'SET TICK POSITIONS: Positions of axes large tick marks', &
       'SET TITLE STYLE: text font, colour, character width, etc.', &
       'SET X-LABEL STYLE: text font, colour, character width, etc.', &
       'SET Y-LABEL STYLE: text font, colour, character width, etc.', &
       'SLEEP: Pause for a defined number of seconds' /
     Data (MENUTXT(item), item = 111, 120) / &
       'SMOOTH: Top-hat convolution smoothing of user input size', &
       'SPATIAL FILTERING: Filtering in Spatial domain', &
       'START MACRO: Save commands in macro definition file', &
       'STATISTICS: Calculate parameters of a region of the data', &
       'STOP MACRO: Close previously opened macro definition file', &
       'STOP WATCH: Interactive stop watch for external timing', &
       'STORE: Store present data set in internal memory', &
       'SUBTRACT: Subtract region of interest of memory from data', &
       'SURFACE INTERPOLATION: User defined surface in memory', &
       'SYMBOL: Same as "VARIABLE"' /
     Data (MENUTXT(item), item = 121, 130) / &
       'SYMMETRIC FUNCTION: Add circularly symmetric function to data', &
       'TITLE: Input new text for title', &
       'THRESHOLD: Set minimum and/or maximum values in ROI', &
       'TRANSPOSE: Transpose data arrays and variance arrays', &
       'UN-DEFINE VARIABLE: Remove variable from translation table', &
       'UNIT CELL PARAMETERS: Convert between real and reciprocal', &
       'VARIABLE: Define a named variable its type and value', &
       'VARIANCES DEFINITION: Define variances from current data', &
       'V2C: Variance to current and vice versa (special command)', &
       'WAIT: Wait for a user return (including within macros)' /
     Data (MENUTXT(item), item = 131, 138) / &
       'WEIGHTED AVERAGE: data and memory weighted by variances', &
       'X-AXIS LABEL: Enter X-axis for graphics', &
       'Y-AXIS LABEL: Enter Y-axis label for graphics', &
       'Z-AXIS LABEL: Enter Z-axis label (intensity) for graphics', &
       'Z-SCALE: Image scaling mode, automatic, minimum/ maximum', &
       'ZZ_TEST: Test log output etc.', &
       '1-D INTERPOLATION: Correct 1-D values by interpolation', &
       '3-D SURFACE PLOT: Graphics view of region of interest' /
 
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_KEYBOARD ' // Version)
        Return
     End If
 
!  Check that the data region is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_KEYBOARD ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Arguments would appear to be reasonable, go ahead.
 
!     Set up initial default values
        If (.Not. gui) Then
           print_type = 'banner'
        End If
        num_menu = max_menu
        command = 'INPUT DATA'
 
!     Create error message for the menu input
        ERROR(1) = 'Please enter one of the available menu choices'
        ERROR(2) = 'Enter ? for list of menu choices'
 
!     Start command input/action loop until EXIT requested
        continue = .True.
        Do While (continue)
 
!        By default no warnings
           warning = 0
 
!        Check for "null" command
           If (command .Eq. 'null') Then
              command = display_command
           End If
 
!        Get user to select between the available menu options
           Call IO_MENU (.True., 'Main menu: ENTER COMMAND', max_menu, &
             MENUTXT, 2, ERROR, max_menu, num_menu, MENU, command, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''command = '', a80)') command
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Check for re-sizing of graphics window
           Call LG_INP_RESIZE (Gs_wkid_terminal, resize, status)
 
!        If in sequence mode and "null" return status returned, set
!        default command
           If (status .Eq. St_nullreturn .And. io_sequence) Then
              Call ST_DEF_SYSTEM (status)
              command = 'SEQUENCE'
           Else
 
!           Do nothing
              Continue
 
           End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!        EXECUTE MENU CHOICES
 
           Call F2D_KEYSUB (max_menu, num_menu, MENUTXT, fit2d_version, gui, &
             output_graphics, shared_memory, memory_id, command, &
             display_command, print_type, input_file, data_defined, &
             memory_exist, memory_defined, variance_exist, mask_exist, &
             log_file_open, xmaxdat, ymaxdat, xnumdat, ynumdat, title, xlabel, &
             ylabel, zlabel, experiment, input_options, xstrelm, ystrelm, &
             xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, &
             mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
             mx_pixel_size, my_pixel_size, results, warning, continue, status)
!max_vec_values, max_vectors, &
!             num_vectors, STR_VECTORS, END_VECTORS, VECTOR_TITLES, warning, &
!             continue, status)
 
!        Output user message if error
           Call ST_OUT (status)
 
           If (status .Ne. St_goodvalue) Then
 
!           Use choice to EXIT or reset status
              reset = .True.
              temp_status = St_goodvalue
              Call IO_INPL (.True., 0, 1, .True., 'RESET "status"', 1, &
                'YES: to reset "status" value, other exit program', 1, &
                'Enter YES or NO', reset, temp_status)
 
              If (reset) Then
                 Call ST_DEF_SYSTEM (status)
              Else
                 continue = .False.
              End If
 
           End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!        Output warning messages if required
           If (warning .Eq. 1) Then
 
!           No data, issue message
              Call IO_WRITE ( 'WARNING: No data exists at the moment, you ' // &
                'must "INPUT DATA" or "CREATE DATA"', status)
              command = 'INPUT DATA'
           Else If (warning .Eq. 2) Then
 
!           No data in memory, issue message
              Call IO_WRITE ( 'WARNING: No data is stored in the memory at ' &
                // 'present, you must use "STORE"', status)
              Call IO_WRITE ('         or use "EXCHANGE" ' // &
                'to put data into the memory', status)
              command = 'STORE'
           Else If (warning .Eq. 3) Then
 
!           Memory not defined throughout ROI
              Call IO_WRITE ( 'WARNING: Memory is not defined throughout ' // &
                'region of interest (ROI)', status)
              Call IO_WRITE ('         you can use "REGION" ' // &
                'or "ROI" to re-define the ROI', status)
              command = 'REGION'
           Else If (warning .Eq. 4) Then
 
!           No memory, issue message
              Call IO_WRITE ( 'WARNING: Memory arrays do not exist you must ' &
                // 'create them with', status)
              Call IO_WRITE ('         "DIMENSIONS" (but ' // &
                'this will destroy any stored data)', status)
              command = 'DIMENSIONS'
           Else If (warning .Eq. 5) Then
 
!           No graphics, issue message
              Call IO_WRITE ( 'WARNING: Graphics system is turned off. You ' &
                // 'must "EXIT" and restart FIT2D', status)
              command = 'EXIT'
 
           Else If (warning .Eq. 6) Then
 
!           No variance arrays, issue message
              Call IO_WRITE ( 'WARNING: No variance arrays exist at the ' // &
                'moment, you must "EXIT" and', status)
              Call IO_WRITE ('         restart with error ' // &
                'arrays  defined', status)
              command = 'EXIT'
 
           End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!        Check for warning occuring during a macro
           If ((warning .Gt. 0 .And. warning .Lt. 6) .And. io_input_macro) &
             Then
 
              Call IO_WRITE ('WARNING---WARNING---WARNING---' // &
                'WARNING---WARNING---WARNING---WARNING--WARNING', status)
              Call IO_WRITE (' ', status)
              Call IO_WRITE ('ERROR: Bad "WARNING: occured ' // &
                'during a macro. Macro automatically terminated.', status)
              Call IO_WRITE (' ', status)
              Call IO_WRITE ('WARNING---WARNING---WARNING---' // &
                'WARNING---WARNING---WARNING---WARNING--WARNING', status)
              io_input_macro = .False.
 
!           Close macro file
              Close (Io_unit_inmacro)
 
           End If
 
!        Check if sequence is running
!        Check for warning occuring during a macro
           If ((warning .Gt. 0 .And. warning .Lt. 6) .And. io_sequence) Then
 
              Call IO_WRITE ('WARNING---WARNING---WARNING---' // &
                'WARNING---WARNING---WARNING---WARNING--WARNING', status)
              Call IO_WRITE (' ', status)
              Call IO_WRITE ('ERROR: Bad "WARNING" occured ' // &
                'during a sequence. Sequence terminated.', status)
              Call IO_WRITE (' ', status)
              Call IO_WRITE ('WARNING---WARNING---WARNING---' // &
                'WARNING---WARNING---WARNING---WARNING--WARNING', status)
              io_sequence = .False.
           End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
 
        End Do
 
!     Output information message
        If (gui) Then
           Call IO_WRITE ('INFO: Control transferred to graphics window', &
             status)
        End If
 
     End If
 
     End Subroutine F2D_KEYBOARD
!********1*********2*********3*********4*********5*********6*********7*********8
!********1*********2*********3*********4*********5*********6*********7*********8
 
!+ F2D_KEYSUB - FIT 2-D KEY-board SUB
     Subroutine F2D_KEYSUB (max_menu, num_menu, MENUTXT, fit2d_version, gui, &
       output_graphics, shared_memory, memory_id, command, display_command, &
       print_type, input_file, data_defined, memory_exist, memory_defined, &
       variance_exist, mask_exist, log_file_open, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, input_options, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, results, warning, continue, status)
!max_vec_values, max_vectors, num_vectors, &
!       STR_VECTORS, END_VECTORS, VECTOR_TITLES, warning, continue, status)
!  Description:
!    Menu choices of the keyboard menu
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    06-Jan-2015: V1.14 Changes to "F2D_EXTEND" (Hammersley)
!    09-Dec-2014: V1.13 Use result vectors data structure (Hammersley)
!    27-Sep-2007: V1.12 Add "LOAD MASK" command (Hammersley)
!    10-Nov-2006: V0.11 Add "STOP WATCH" command (Hammersley)
!    24-Apr-2006: V0.10 Changes to "FIO_INPUT" (Hammersley)
!    23-Mar-2006: V0.9 Use "LUT_STYLE" structure (Hammersley)
!    20-Mar-2006: V0.8 Add "INPUT OPTIONS" support (Hammersley)
!    10-Mar-2006: V0.7 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    12-Jan-2005: V0.6 Add "LOAD GEOMETRY" command (Hammersley)
!    11-Jan-2005: V0.5 Add "SAVE GEOMETRY" command (Hammersley)
!    10-Mar-2004: V0.4 Change use of "VECTORS" array (Hammersley)
!    27-Feb-2004: V0.3 Add "BLOCK COPY" command to keyboard menu
!      for more efficient access to internal memories (Hammersley)
!    28-Oct-1998: V0.2 Add support for "VECTORS" arrays (Hammersley)
!    20-Oct-1998: V0.1 Original, extracted from "F2D_KEYBOARD" (Hammersley)
!  Modules:
!  Use IO_LIB
!  Use MA_LIB
!  Use GS_LIB
!  Type Definitions:
     Implicit None
     Include 'gs.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'io_db.inc' ! I/O data-base
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import:
     Integer, Intent(IN) :: max_menu ! Number of instructions in the menu
     Integer, Intent(IN) :: num_menu ! Number of choices available in the menu
     Character(Len = *), Intent(IN) :: MENUTXT(Max_menu) ! Text to explain
!      menu choices
     Character(Len = *), Intent(IN) :: fit2d_version ! Version of fit2d
!      being run
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used
     Logical, Intent(IN) :: output_graphics ! .True., if graphics are to be
!      output
     Logical, Intent(IN) :: shared_memory ! .True., if shared memory is to used
     Integer, Intent(IN) :: memory_id ! Identifier of shared memory (if used)
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: command ! Full command
!      corresponding to user request
     Character(Len = *), Intent(INOUT) :: display_command ! Last used
!      display option ("IMAGE" or "PLOT")
     Character(Len = *), Intent(INOUT) :: print_type ! Type, of graphics to
!      print: supported types are:
!        "banner"
!        "image"
!        "contour"
!        "x/y graph"
     Character(Len = *), Intent(INOUT) :: input_file ! Name of current data
!      input file
     Logical, Intent(INOUT) :: data_defined ! .True., if data exists within
!      the program
     Logical, Intent(INOUT) :: memory_exist ! .True. if memory array exists
     Logical, Intent(INOUT) :: memory_defined ! .True. if the memory
!      contains data
     Logical, Intent(INOUT) :: variance_exist ! .True., if a data variance
!      array is created
     Logical, Intent(INOUT) :: mask_exist ! .True., if the mask array exists
     Logical, Intent(INOUT) :: log_file_open ! .True., if a log file is open
     Integer, Intent(INOUT) :: xmaxdat ! X-direction dimension for data arrays
     Integer, Intent(INOUT) :: ymaxdat ! Y-direction dimension for data arrays
     Integer, Intent(INOUT) :: xnumdat ! Number of elements in X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of elements in Y-direction
!    Real X_AXIS(xmaxdat) ! X-axis values
!    Real Y_AXIS(ymaxdat) ! Y-axis values
!    Real DATA(xmaxdat, ymaxdat) ! The data values
!    Real VARIANCES(xmaxdat, ymaxdat) ! The estimated variance values
     Character(Len = *), Intent(INOUT) :: title ! Title for plot
     Character(Len = *), Intent(INOUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: zlabel ! Z-axis label for plot
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Type(INPUT_OPTIONS_STRUCTURE), Intent(INOUT) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(INOUT) :: mynumdat ! Defines Y-extent of data region
!    Real MX_AXIS(xmaxdat) ! Array containing data X-coordinates
!    Real MY_AXIS(ymaxdat) ! Array containing data Y-coordinates
!    Real MDATA(xmaxdat, ymaxdat) ! Array containing data to be fitted
!    Real MVARIANCES(xmaxdat, ymaxdat) ! Array containing variances in
!    the data values
     Integer, Intent(INOUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(INOUT) :: mxstrelm ! Starting X-element of memory data
!      region
     Integer, Intent(INOUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(INOUT) :: mystrelm ! Starting Y-element of memory data
!      region
     Character(Len = *), Intent(INOUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(INOUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(INOUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(INOUT) :: mzlabel ! Z-axis label for data
!     Type(IMAGE_DATA), Intent(INOUT) :: main ! Main data-set
!     Type(IMAGE_DATA), Intent(INOUT) :: memory ! Memory data-set
     Real, Intent(INOUT) :: mx_pixel_size ! Size of a pixel in the memory
!      data in the X-direction (metres)
     Real, Intent(INOUT) :: my_pixel_size ! Size of a pixel in the memory
!      data in the Y-direction (metres)
     Type(RESULT_VECTORS), Intent(INOUT) :: results ! Result vectors
!     Integer, Intent(INOUT) :: max_vec_values ! First dimension of "VECTORS"
!      array (maximum number of values which can be stored in a vector)
!     Integer, Intent(INOUT) :: max_vectors ! Second dimension of "VECTORS"
!      array (maximum number of vectors which can be stored)
!     Integer, Intent(INOUT) :: num_vectors ! Number of values defined in the
!    "time"-series for each vector
!     Integer, Intent(INOUT) :: STR_VECTORS(max_vectors) ! Starting defined
!      element for "VECTORS"
!     Integer, Intent(INOUT) :: END_VECTORS(max_vectors) ! End defined
!      elements for "VECTORS"
!    Real VECTORS(max_vec_values, max_vectors) ! Multiple 1-D arrays of
!      vector values
!     Character(Len = *), Intent(INOUT) :: VECTOR_TITLES(max_vectors)
!      Titles for the 1-D data-sets
!  Export:
     Integer, Intent(OUT) :: warning ! Type of warning:
!      0 = No warning, everything fine
!      1 = Data not defined
!      2 = Memory not defined at all
!      3 = Memory not defined throughout whole of ROI
!      4 = Memory arrays do not exist
!      5 = No graphics
!      6 = Variance arrays do not exist
     Logical, Intent(OUT) :: continue ! .True., whilst the user wishes to
!      continue with the program
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V1.14' ! Version number
!  Local Variables:
     Character(Len = Len_name), Save :: inmacro_file = 'fit2d.mac'
!      File name of file for input of macro definitions
     Character(Len = Len_name), Save :: log_file_name = 'fit2d.log'
!      File name of file for output of program log
     Character(Len = Len_name), Save :: mask_file_name ! Name of input mask file
     Character(Len = 256) :: message ! User messages
     Character(Len = Len_name), Save :: outmacro_file = 'fit2d.mac'
!      File name of file for output of macro definitions
     Integer :: db_stat ! Data-store status return
     Integer :: i ! Loop variable
     Integer :: item ! Loop variable for array items
     Integer :: j ! Loop variable
     Integer :: len_string ! Defined length of string
     Integer :: pWORK ! Pointer to dynamically allocated work array
     Integer :: retstat ! Return status:
!      0 = Good status, operation performed correctly
!      1 = Bad status, operation not performed due to wrong ROI and array shape
     Logical :: confirm ! .True., if the user confirms that they want to
!      exit the program
!  Local Data Structures:
     Type(LUT_STYLE) :: style ! LUT Style (see "gs.inc")
!  Local Arrays:
     Character(Len = 20) :: COLOUR_TABLE(10) ! Colour table choices
!  External Functions:
!  Internal Functions:
!  Local Data:
     Data mask_file_name / 'fit2d.msk' /
     Data (COLOUR_TABLE(item), item = 1, 10) / 'COLOUR WHEEL', 'GEOGRAPHICAL', &
       'GREY-SCALE', 'INVERSE GREY-SCALE', 'ORIGINAL', 'PSYCHOLOGIAL', &
       'REPEATING', 'TEMPERATURE', 'TRIAL', 'UPSIDE DOWN' /
!--------1---------2---------3---------4---------5---------6---------7---------8

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Default = '', a)') mask_file_name
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Check user escape has not been returned
     If (status .Eq. St_escapevalue .Or. status .Eq. St_nullreturn) Then
 
!     Reset status system
        Call ST_DEF_SYSTEM (status)
 
!     Set default command to "EXIT"
        command = 'EXIT'
 
!     Otherwise call subroutine to perform requested operation.
 
     Else If (command .Eq. '?') Then
 
!     Output list of available commands
        Call IO_TEXT (num_menu, MENUTXT, status)
        command = 'HELP'
 
     Else If (command .Eq. 'ADD') Then
 
        If (memory_defined .And. data_defined) Then
 
!        Add memory to current data in region of interest
           If (xendelm .Le. mxnumdat .And. yendelm .Le. mynumdat) Then
 
              Call MA_RADD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, %val(pMDATA), xmaxdat, ymaxdat, %val(pDATA), status)
 
              If (variance_exist) Then
 
                 Call MA_RADD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                   yendelm, %val(pMVARIANCES), xmaxdat, ymaxdat, &
                   %val(pVARIANCES), status)
 
              End If
 
              command = 'FIT'
 
           Else
              warning = 3
           End If
 
        Else If (data_defined) Then
           warning = 2
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'ANNOTATION LABEL') Then
 
!     Input requirements for annotation labels
        Call GS_INP_ALABELS (status)
        command = display_command
 
     Else If (command .Eq. 'ASPECT RATIO') Then
 
!     User setting of automatic aspect ratio display, or not
        Call F2D_ASPECTRATIO (status)
        command = display_command
 
     Else If (command .Eq. 'AUTOCORRELATION') Then
 
        If (data_defined) Then
 
!        Calculate autocorrelation function of the ROI
           Call F2D_AUTOCORRELATION (.False., xmaxdat, ymaxdat, xstrelm, &
             ystrelm, xendelm, yendelm, %val(pXAXIS), %val(pYAXIS), &
             %val(pDATA), memory_defined, mxnumdat, mynumdat, mxstrelm, &
             mystrelm, mxendelm, myendelm, %val(pMXAXIS), %val(pMYAXIS), &
             %val(pMDATA), mtitle, mxlabel, mylabel, mzlabel, status)
           command = 'EXCHANGE'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'AXES SCALES') Then
 
        If (data_defined) Then
 
!        User setting of axes start and increment values
           Call F2D_AXESSCALES (.False., xmaxdat, ymaxdat, xnumdat, ynumdat, &
             xstrelm, ystrelm, xendelm, yendelm, %val(pXAXIS), %val(pYAXIS), &
             status)
           command = display_command
 
        Else
           warning = 1
        End If
 
        command = display_command
 
     Else If (command .Eq. 'BANNER') Then
 
!     Draw banner page
        Call F2D_BANNER (fit2d_version, gui, status)
        command = 'MESSAGE'
 
     Else If (command .Eq. 'BLOCK COPY') Then
 
        If (data_defined) Then
 
!        Copy a block of data to or from an internal memory
           Call F2D_BLOCKCOPY (.False., variance_exist, xmaxdat, ymaxdat, &
             xnumdat, ynumdat, %val(pDATA), %val(pVARIANCES), status)
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'BLUR' .Or. command .Eq. 'SMOOTH') Then
 
        If (data_defined .And. memory_exist) Then
 
!        Top-hat convolution blurring
           Call F2D_BLUR (.False., xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, variance_exist, %val(pDATA), %val(pVARIANCES), &
             %val(pMDATA), %val(pMVARIANCES), status)
 
           mxnumdat = xendelm
           mynumdat = yendelm
           mxstrelm = xstrelm
           mystrelm = ystrelm
           mxendelm = xendelm
           myendelm = yendelm
           mtitle = title
           mxlabel = xlabel
           mylabel = ylabel
           mzlabel = zlabel
 
           Call MA_RCOPY (xmaxdat, 1, %val(pXAXIS), 1, 1, xnumdat, 1, xmaxdat, &
             1, %val(pMXAXIS), status)
           Call MA_RCOPY (ymaxdat, 1, %val(pYAXIS), 1, 1, ynumdat, 1, ymaxdat, &
             1, %val(pMYAXIS), status)
           memory_defined = .True.
           command = 'EXCHANGE'
 
        Else If (data_defined) Then
           warning = 4
        Else
           warning = 2
        End If
 
     Else If (command .Eq. 'BRAGGS'' EQUATION') Then
 
        Call F2D_BRAGG (status)
        command = 'GEOMETRY (EXPERIMENT)'
 
     Else If (command .Eq. 'CADD') Then
 
        If (data_defined) Then
 
!***        Add constant to region of interest
           Call F2D_CADD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, %val(pDATA), status)
           command = 'CMULTIPLY'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'CALCULATOR') Then
 
!     Reverse Polish notation calculator
        Call F2D_CALCULATOR (status)
        command = 'CALIBRATION'
 
     Else If (command .Eq. 'CALIBRATION') Then
 
        If (data_defined) Then
 
!        Calculate or apply calibration functions
           Call F2D_CALIBRATION (memory_exist, variance_exist, input_file, &
             xmaxdat, ymaxdat, xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), &
             %val(pDATA), %val(pVARIANCES), xstrelm, ystrelm, xendelm, &
             yendelm, title, xlabel, ylabel, zlabel, &
             experiment%x_pixel_size, experiment%y_pixel_size, &
             mxnumdat, mynumdat, %val(pMXAXIS), %val(pMYAXIS), &
             %val(pMDATA), %val(pMVARIANCES), mxstrelm, mystrelm, mxendelm, &
             myendelm, mtitle, mxlabel, mylabel, mzlabel, memory_defined, &
             mx_pixel_size, my_pixel_size, status)
 
           command = 'EXCHANGE'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'CDIVIDE') Then
 
        If (data_defined) Then
 
!        Divide region of interest by a constant
           Call F2D_CDIV (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, variance_exist, %val(pDATA), %val(pVARIANCES), status)
           command = display_command
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'CHANGES') Then
 
!     Details of important changes in FIT2D versions
        Call F2D_CHANGES (.False., status)
 
     Else If (command .Eq. 'CLEAR DATA') Then
 
        If (data_defined) Then
 
!        Initialise data region
           Call MA_RVALUE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, 0.0, %val(pDATA), status)
 
           If (variance_exist) Then
 
!           Initialise data variances
              Call MA_RVALUE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, 0.0, %val(pVARIANCES), status)
 
           End If
 
           command = 'SYMMETRIC FUNCTION'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'CLOSE LOG') Then
 
        If (log_file_open) Then
 
!        Close log file:
           Call IO_CLOSE_LOGFILE (status)
           log_file_open = .False.
           command = 'EXIT'
        Else
           Call IO_WRITE ( 'WARNING: Log file is not open', status)
           command = 'OPEN LOG'
        End If
 
     Else If (command .Eq. 'CMULTIPLY') Then
 
        If (data_defined) Then
 
!        Multiply region of interest by a constant
           Call F2D_CMULT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, variance_exist, %val(pDATA), %val(pVARIANCES), status)
           command = 'CADD'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'COLOUR TABLE') Then
 
!     Interactive definition of colour table
        Call GS_INP_LUT (status)
 
!     Inquire colour table choice
        Call GS_INQ_LUTSTYLE (style, status)
 
!     Save colour table choice in internal data-store
        len_string = Len_trim(COLOUR_TABLE(style%colour_table))
        Call IO_SET_KEYVALUE ('COLOUR_TABLE', len_string, &
          COLOUR_TABLE(style%colour_table), db_stat, status)
 
!     Set new command
        command = display_command
 
     Else If (command .Eq. 'CONCATENATION') Then
 
!     Concatenate two strings
        Call F2D_CONCATENATION (status)
 
!     Set new command
        command = 'DEFINE VARIABLE'
 
     Else If (command .Eq. 'CONTOUR PLOT') Then
 
        If (data_defined) Then
 
           If (yendelm - ystrelm .Gt. 1) Then
 
!           PLOT DATA: Produce image display of data array
              Call GS_2DCONTOUR (xmaxdat, ymaxdat, %val(pDATA), %val(pXAXIS), &
                %val(pYAXIS), xstrelm, ystrelm, xendelm, yendelm, title, &
                xlabel, ylabel, zlabel, status)
              print_type = 'contour'
 
           Else If (ystrelm .Eq. 1) Then
 
!           Plot X-Y graph
              Call GS_XYSGRAPH (xmaxdat, xstrelm, xendelm, %val(pXAXIS), &
                %val(pDATA), title, xlabel, zlabel, status)
              print_type = 'x/y graph'
 
           End If
 
!        Force up-dating of screen
           Call GS_UPDATE (status)
 
           command = 'PRINT GRAPHICS'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'CREATE DATA') Then
 
!     Set data and size of data
        Call F2D_CREATEDATA (variance_exist, xmaxdat, ymaxdat, xnumdat, &
          ynumdat, xstrelm, ystrelm, xendelm, yendelm, %val(pXAXIS), &
          %val(pYAXIS), %val(pDATA), %val(pVARIANCES), title, xlabel, ylabel, &
          zlabel, status)
        data_defined = .True.
        command = 'GAUSSIAN'
 
     Else If (command .Eq. 'CURVE STYLES' .Or. command .Eq. &
       'SET CURVE STYLES') Then
 
!     Interactive setting of curve attributes
        Call GS_INP_CURVESTYLES (.False., status)
        command = display_command
 
     Else If (command .Eq. 'DEFINE VARIABLE' .Or. command .Eq. 'SYMBOL' .Or. &
       command .Eq. 'VARIABLE') Then
 
!     Define program variable
        Call IO_VARIABLE (status)
        command = 'MACRO'
 
     Else If (command .Eq. 'DEDUCE FILE SEQUENCE') Then
 
!     Define components of a file sequence
        Call F2D_FILESEQUENCE (status)
 
!     Set new command
        command = 'CONCATENATION'
 
     Else If (command .Eq. 'DIFFRACTION PATTERN') Then
 
!     Calculate diffraction pattern
        Call F2D_DIFPATTERN (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, %val(pDATA), %val(pXAXIS), %val(pYAXIS), title, xlabel, &
          ylabel, zlabel, experiment, status)
        command = 'PRINT GRAPHICS'
 
     Else If (command .Eq. 'DIFFERENTIATE') Then
 
        Call IO_WRITE ('WARNING: Not yet implemented', status)
 
!     If (data_defined) Then
!
!***           Calculate numerical differential in region of interest
!     Call MA_2DDIF (xmaxdat,ymaxdat,%val(pDATA),
!     :                   xstrelm,ystrelm,xendelm,yendelm,
!     :                   xmaxdat,ymaxdat,%val(pMDATA),status)
!
!***           Copy axis data
!     Call MA_RCOPY (xmaxdat,1,xnumdat,1,%val(pXAXIS),
!     :                   xmaxdat,1,%val(pMXAXIS),status)
!     Call MA_RCOPY (ymaxdat,1,ynumdat,1,%val(pYAXIS),
!     :                   ymaxdat,1,%val(pMYAXIS),status)
!
!     mxnumdat=xendelm
!     mynumdat=yendelm
!     mxstrelm=xstrelm
!     mystrelm=ystrelm
!     mxendelm=xendelm
!     myendelm=yendelm
!     mtitle=title
!     mxlabel=xlabel
!     mylabel=ylabel
!     mzlabel=zlabel
!     memory_defined=.True.
!     command='EXCHANGE'
!
!     Else
!     warning = 1
!     End If
 
     Else If (command .Eq. 'DIMENSIONS') Then
 
!     Input new dimensions for internal dynamic arrays
        Call IO_INPI (.True., 1, 100000, .True., 'ARRAY X DIMENSION', 1, &
          'Enter X dimension for internal data arrays', 1, &
          'Must be within specified bounds', xmaxdat, status)
        Call IO_INPI (.True., 1, 100000, .True., 'ARRAY Y DIMENSION', 1, &
          'Enter Y dimension for internal data arrays', 1, &
          'Must be within specified bounds', ymaxdat, status)
 
!     Free existing internal arrays
        Call F2D_FREEARRAYS (memory_exist, variance_exist, mask_exist, &
          shared_memory, memory_id, pDATA, pXAXIS, pYAXIS, pVARIANCES, pMASK, &
          pMDATA, pMXAXIS, pMYAXIS, pMVARIANCES, results, status)
 
!**OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE
!     Call IO_FREE (pDATA, status)
!     Call IO_FREE (pXAXIS, status)
!     Call IO_FREE (pYAXIS, status)
!     If (memory_exist) Then
!     Call IO_FREE (pMDATA, status)
!     Call IO_FREE (pMXAXIS, status)
!     Call IO_FREE (pMYAXIS, status)
!     End If
!     If (variance_exist) Then
!     Call IO_FREE (pVARIANCES, status)
!     If (memory_exist) Then
!     Call IO_FREE (pMVARIANCES, status)
!     End If
!     End If
!     If (mask_exist) Then
!     Call IO_FREE (pMASK, status)
!     End If
!**OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE
 
!     Ask whether or not memory is necessary
        Call IO_INPL (.True., 0, 1, .True., 'CREATE MEMORY', 1, &
          'YES is required for many operations, NO saves memory', 1, &
          'Must enter YES or NO', memory_exist, status)
 
!     Ask whether or not variance arrays are necessary
        Call IO_INPL (.True., 0, 1, .True., 'CREATE VARIANCE ARRAYS', 1, &
          'YES if error analysis is required, NO saves memory', 1, &
          'Must enter YES or NO', variance_exist, status)
 
!     Now recreate dynamic arrays of specified size
        Call F2D_ARRAYS (xmaxdat, ymaxdat, & ! max_vec_values, max_vectors, &
          memory_exist, variance_exist, shared_memory, memory_id, &
          mask_exist, &
          pDATA, pMASK, pXAXIS, pYAXIS, pVARIANCES, pMDATA, pMXAXIS, pMYAXIS, &
          pMVARIANCES, results, status)
 
!     Data and memory now no longer defined
        data_defined = .False.
        memory_defined = .False.
 
        command = 'INPUT DATA'
 
     Else If (command .Eq. 'DISPLAY LIMITS') Then
 
!     User control of maximum number of displayed pixels in either direction
        Call F2D_DISPLAYLIMITS (status)
        command = display_command
 
     Else If (command .Eq. 'DIVIDE') Then
 
        If (memory_defined .And. data_defined) Then
 
!        Divide current data by the memory in region of interest
           If (xendelm .Le. mxnumdat .And. yendelm .Le. mynumdat) Then
 
              Call F2D_DIVIDE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, variance_exist, %val(pMDATA), %val(pMVARIANCES), &
                %val(pMASK), %val(pDATA), %val(pVARIANCES), status)
              command = 'FIT'
 
           Else
              warning = 3
           End If
 
        Else If (data_defined) Then
           warning = 2
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'DRAW CAKE') Then
 
!     Draw "cake" according to current limits
        Call F2D_DRAW_CAKE_CMD (experiment, status)
 
     Else If (command .Eq. 'END GRAPHICS FILE') Then
 
!     Close PostScript output file
        Call GS_CLOSE_PS (status)
        command = 'PRINT GRAPHICS'
 
     Else If (command .Eq. 'ENTROPY') Then
 
!     Calculate entropy of image and pixel value frequency
!     statistics
        If (data_defined) Then
 
           Call IO_MALLOC (131072 * 4, pWORK, status)
           Call F2D_ENTROPY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, %val(pDATA), %val(pWORK), status)
           Call IO_FREE (pWORK, status)
           command = 'INPUT DATA'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'EXCHANGE') Then
 
        If (data_defined .And. memory_exist) Then
 
           Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
             xlabel, ylabel, zlabel, variance_exist, data_defined, &
             experiment%x_pixel_size, experiment%y_pixel_size, &
             xstrelm, ystrelm, xendelm, yendelm, &
             mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, &
             mtitle, mxlabel, mylabel, mzlabel, memory_defined, mx_pixel_size, &
             my_pixel_size, status)
 
           command = display_command
 
        Else If (data_defined) Then
           warning = 4
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'EXIT' .Or. command .Eq. 'QUIT') Then
 
        If (.Not. gui) Then
 
!        EXIT/QUIT: Confirm request
           confirm = .False.
           Call IO_INPL (.True., 0, 1, .True., 'CONFIRM EXIT', 1, &
             'YES: to exit program, NO: to continue', 1, 'Enter YES or NO', &
             confirm, status)
        Else
           confirm = .True.
        End If
 
        If (.Not. confirm) Then
           command = 'INPUT DATA'
        Else
           continue = .False.
        End If
 
     Else If (command .Eq. 'EXTEND') Then
 
        If (data_defined) Then
 
!        Extend currently defined data region of image
           Call F2D_EXTEND (.False., xmaxdat, ymaxdat, variance_exist, &
             xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
             %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pMASK), &
             %val(pVARIANCES), status)
 
        End If
        command = display_command
 
     Else If (command .Eq. 'FAST IMAGE') Then
 
        Call IO_WRITE ('WARNING: The command "FAST ' // &
          'IMAGE" has been removed as the normal PostScript', status)
        Call IO_WRITE ('         printing is now much ' // &
          'faster.  You should not  use this command and', status)
        Call IO_WRITE ('         remove it from any macros.', status)
 
!     !!! NOT INDENTED PROPERLY AFTER HERE !!!!!!!!!!!!!
     Else If (command .Eq. 'FILTER') Then
 
        Call IO_WRITE ('WARNING: Not yet implemented', status)
 
!     If (data_defined) Then
!
!     xnumtmp=xendelm-xstrelm+1
!     ynumtmp=yendelm-ystrelm+1
!
!     xlowpass=0
!     MESSAGE(1)='Low pass frequency for X direction'
!     ERROR(1)='Must be within specified bounds'
!     Call IO_INPI (.True.,0,xnumtmp/2,.True.,'X LOW PASS',
!     :            fullhed,1,MESSAGE,1,ERROR,xlowpass,status)
!
!     ylowpass=0
!     MESSAGE(1)='Low pass frequency for Y direction'
!     ERROR(1)='Must be within specified bounds'
!     Call IO_INPI (.True.,0,ynumtmp/2,.True.,'Y LOW PASS',
!     :            fullhed,1,MESSAGE,1,ERROR,ylowpass,status)
!
!     xhighpass=xnumtmp/2
!     MESSAGE(1)='Highest pass frequency for X direction'
!     ERROR(1)='Must be within specified bounds'
!     Call IO_INPI (.True.,0,xnumtmp/2,.True.,'X HIGH PASS',
!     :            fullhed,1,MESSAGE,1,ERROR,xhighpass,status)
!
!     yhighpass=ynumtmp/2
!     MESSAGE(1)='Highest pass frequency for Y direction'
!     ERROR(1)='Must be within specified bounds'
!     Call IO_INPI (.True.,0,ynumtmp/2,.True.,'Y HIGH PASS',
!     :            fullhed,1,MESSAGE,1,ERROR,yhighpass,status)
!
!     filtmode=1
!     MESSAGE(1)='Mode for filitering data :'
!     MESSAGE(2)='  0 = Block removal of frequencies'
!     MESSAGE(3)='  1 = Elliptical removal of frequencies'
!     ERROR(1)='Must be within specified bounds'
!     Call IO_INPI (.True.,0,1,.True.,'FILTER MODE',
!     :            fullhed,3,MESSAGE,1,ERROR,filtmode,status)
!
!***           Copy ROI to work array
!     Call MA_RMOVE (xmaxdat,ymaxdat,%val(pDATA),
!     :            xstrelm,ystrelm,xendelm,yendelm,
!     :            xmaxdat,ymaxdat,1,1,%val(pWORK),status)
!
!     Call MA_FFILT (xmaxdat,ymaxdat,xnumtmp,ynumtmp,
!     :            xlowpass,ylowpass,xhighpass,yhighpass,filtmode,
!     :            %val(pWORK),status)
!
!***           Copy work array back to ROI
!     Call MA_RMOVE (xmaxdat,ymaxdat,%val(pWORK),
!     :            1,1,xnumtmp,ynumtmp,
!     :            xmaxdat,ymaxdat,xstrelm,ystrelm,
!     :            %val(pDATA),status)
!
!     command = display_command
!     Else
!     warning = 1
!     End If
 
     Else If (command .Eq. 'FIT') Then
 
!     Enter fitting sub-menu
        If (data_defined .And. memory_exist) Then
 
           If (.Not. mask_exist) Then
 
!           Create mask array
              Call IO_MALLOC (xmaxdat * ymaxdat, pMASK, status)
 
              If (status .Eq. St_goodvalue) Then
 
!              Initialise mask to all good elements
                 Call MA_L1VALUE (xmaxdat, ymaxdat, 1, 1, xmaxdat, ymaxdat, &
                   .False., %val(pMASK), status)
                 mask_exist = .True.
 
              End If
 
           End If
 
!        Enter fit menu
!        N.B. Pass program arrays pointer through common
           Call F2D_FIT (variance_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, &
             title, xlabel, ylabel, zlabel, experiment, &
             xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
             mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
             memory_defined, mx_pixel_size, my_pixel_size, status)
 
           command = 'EXCHANGE'
 
        Else If (data_defined) Then
           warning = 4
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'FLIP') Then
 
        If (data_defined) Then
 
!        Flip the data in the horizontal or vertical
           Call F2D_FLIP (variance_exist, xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pDATA), %val(pVARIANCES), status)
           command = display_command
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'FONT' .Or. command .Eq. 'SET FONT') Then
 
!     Set the text font for all text
        Call GS_INP_FONT (status)
        command = display_command
 
     Else If (command .Eq. 'FUJI LINEARISATION') Then
 
        If (data_defined) Then
 
!        Convert fuji intensities to a linear scale
           Call F2D_FUJI (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, variance_exist, %val(pDATA), %val(pVARIANCES), status)
           command = display_command
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'FULL REGION') Then
 
        If (data_defined) Then
 
!        Define maximum data display region
           xstrelm = 1
           ystrelm = 1
           xendelm = xnumdat
           yendelm = ynumdat
           command = 'OUTPUT DATA'
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'GAUSSIAN') Then
 
        If (data_defined) Then
 
!        Add user specified 2-D Gaussian to data
           Call F2D_GAUSSIAN (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), status)
           command = display_command
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'GEOMETRY (EXPERIMENT)') Then
 
        If (data_defined) Then
 
!        Input experimental geometry and store in internal database
           Call F2D_GEOMETRY (.False., xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pDATA), %val(pXAXIS), %val(pYAXIS), title, &
             xlabel, ylabel, zlabel, experiment, status)
           command = 'DIFFRACTION PATTERN'
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'GRID') Then
 
!     Input users grid requirements
        Call GS_INP_GRID (status)
        command = display_command
 
     Else If (command .Eq. 'GUI') Then
 
!     Enter graphics interface
        Call F2D_GUI (fit2d_version, .False., output_graphics, shared_memory, &
          memory_id, input_file, data_defined, memory_exist, memory_defined, &
          variance_exist, mask_exist, log_file_open, f2d_xmaxdat, f2d_ymaxdat, &
          f2d_xnumdat, f2d_ynumdat, title, xlabel, ylabel, zlabel, &
          experiment, input_options, f2d_xstrelm, f2d_ystrelm, f2d_xendelm, &
          f2d_yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
          myendelm, mtitle, mxlabel, mylabel, mzlabel, mx_pixel_size, &
          my_pixel_size, results, status)
!max_vec_values, max_vectors, num_vectors, &
!          STR_VECTORS, END_VECTORS, VECTOR_TITLES, status)
        command = 'EXIT'
 
     Else If (command .Eq. 'HELP') Then
 
!     Interactive help system
        Call F2D_HELP (.False., status)
 
     Else If (command .Eq. 'HISTOGRAM') Then
 
        If (data_defined) Then
 
           Call F2D_HISTOGRAM (.False., xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pDATA), title, mxstrelm, mystrelm, &
             mxendelm, myendelm, %val(pMDATA), %val(pMXAXIS), %val(pMYAXIS), &
             mtitle, mxlabel, mylabel, mzlabel, status)
           mxnumdat = mxendelm
           mynumdat = myendelm
           memory_defined = .True.
           command = 'EXCHANGE'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'IMAGE') Then
 
        If (data_defined .And. output_graphics) Then
 
!        Interactive image display and manipulation
           Call F2D_IMAGE (.False., xmaxdat, ymaxdat, xnumdat, ynumdat, &
             %val(pXAXIS), %val(pYAXIS), %val(pMASK), %val(pDATA), &
             %val(pVARIANCES), title, xlabel, ylabel, zlabel, variance_exist, &
             experiment, xstrelm, ystrelm, xendelm, yendelm, mxnumdat, &
             mynumdat, %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), &
             %val(pMVARIANCES), mxstrelm, mystrelm, mxendelm, myendelm, &
             mtitle, mxlabel, mylabel, mzlabel, memory_defined, mx_pixel_size, &
             my_pixel_size, print_type, status)
 
           command = 'EXIT'
           display_command = 'IMAGE'
 
        Else If (output_graphics) Then
           warning = 1
        Else
           warning = 5
        End If
 
     Else If (command .Eq. 'I2C') Then
 
!     Convert input integer to character representation
        Call F2D_I2C (status)
        command = 'CONCATENATION'
 
     Else If (command .Eq. 'INFORMATION') Then
 
!     Output information on the internal state of the program
        Call F2D_INFORMATION (xmaxdat, ymaxdat, data_defined, variance_exist, &
          xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
          memory_defined, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
          myendelm, status)
        command = 'DIMENSIONS'
 
     Else If (command .Eq. 'INPUT DATA') Then
 
!     Input data from choice of file formats
        Call FIO_INPUT (input_options, xmaxdat, ymaxdat, &
          input_file, variance_exist, &
          data_defined, xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), &
          %val(pDATA),  %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
          experiment%x_pixel_size, experiment%y_pixel_size, &
          experiment, status)
        xstrelm = 1
        ystrelm = 1
        xendelm = xnumdat
        yendelm = ynumdat
        command = display_command
 
!     Set default values for data display region
        Call GS_SET_IDR (0.0, 0.0, Real(xnumdat), Real(ynumdat), status)
 
     Else If (command .Eq. 'INTERNAL MEMORY') Then
 
!     Store or recover data from internal memory
        Call F2D_INTERNALMEMORY (xmaxdat, ymaxdat, variance_exist, title, &
          data_defined, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
          %val(pXAXIS), %val(pYAXIS), %val(pDATA),  %val(pVARIANCES), status)
        command = display_command
 
     Else If (command .Eq. 'LOAD GEOMETRY') Then
 
!     Load experiment geometry from a file
        Call F2D_LOADGEOMETRY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, experiment%x_pixel_size, experiment%y_pixel_size, status)
 
     Else If (command .Eq. 'LOAD MASK') Then
 
!     Load experiment geometry from a file
        Call FIO_IN_MASK (.False., xmaxdat, ymaxdat, xnumdat, ynumdat, &
          mask_file_name, retstat, %val(pMASK), status)
 
     Else If (command .Eq. 'LINEARISE FILM') Then
 
        If (data_defined) Then
 
!        Apply non-linearity film corrections
           Call F2D_LINEARISE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, variance_exist, %val(pDATA), %val(pVARIANCES), status)
           command = display_command
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'LIST VARIABLES') Then
 
!     Output table of currently defined program variables and values
        Call F2D_LISTVARIABLES (status)
        command = 'DEFINE VARIABLE'
 
     Else If (command .Eq. 'LOGARITHM') Then
 
        If (data_defined) Then
 
!        Take logarithm of ROI
           Call F2D_LOGARITHM (.False., xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, variance_exist, %val(PMASK), %val(pDATA), &
             %val(pVARIANCES), status)
           command = display_command
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'MACRO' .Or. command .Eq. 'RUN MACRO') Then
 
!     Run macro file
        Call IO_OPEN_INMACRO (.True., inmacro_file, status)
        command = display_command
 
     Else If (command .Eq. 'MEDIAN FILTER') Then
 
        If (data_defined .And. memory_exist) Then
 
           Call F2D_MEDIANFILTER (.False., xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pDATA), %val(pMDATA), status)
 
           memory_defined = .True.
           mxnumdat = xendelm
           mynumdat = yendelm
           mxstrelm = xstrelm
           mystrelm = ystrelm
           mxendelm = xendelm
           myendelm = yendelm
           mtitle = title
           mxlabel = xlabel
           mylabel = ylabel
           mzlabel = zlabel
 
           Call MA_RCOPY (xmaxdat, 1, %val(pXAXIS), 1, 1, xnumdat, 1, xmaxdat, &
             1, %val(pMXAXIS), status)
           Call MA_RCOPY (ymaxdat, 1, %val(pYAXIS), 1, 1, ynumdat, 1, ymaxdat, &
             1, %val(pMYAXIS), status)
           command = 'EXCHANGE'
 
        Else If (data_defined) Then
           warning = 4
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'MESSAGE') Then
 
!     Define interactive message for macro output
        Call F2D_MESSAGE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, %val(pDATA), %val(pXAXIS), %val(pYAXIS), title, xlabel, &
          ylabel, zlabel, status)
        command = 'SLEEP'
 
     Else If (command .Eq. 'MOVE/ROTATE') Then
 
        If (data_defined .And. memory_exist) Then
 
!        Move/rotate the data
           Call F2D_MOVE (xmaxdat, ymaxdat, %val(pXAXIS), %val(pYAXIS), &
             %val(pDATA), %val(pVARIANCES), xstrelm, ystrelm, xendelm, &
             yendelm, variance_exist, &
             experiment%x_pixel_size, experiment%y_pixel_size, &
             %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), %val(pMVARIANCES), &
             mxstrelm, mystrelm, mxendelm, myendelm, status)
 
           If (mxnumdat .Lt. mxendelm) Then
              mxnumdat = mxendelm
           End If
 
           If (mynumdat .Lt. myendelm) Then
              mynumdat = myendelm
           End If
 
           mtitle = title
           mxlabel = xlabel
           mylabel = ylabel
           mzlabel = zlabel
           mx_pixel_size = experiment%x_pixel_size
           my_pixel_size = experiment%y_pixel_size
           memory_defined = .True.
           command = 'EXCHANGE'
 
        Else If (data_defined) Then
           warning = 4
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'MULTIPLY') Then
 
        If (memory_defined .And. data_defined) Then
 
!        Multiply memory to current data in region of interest
           If (xendelm .Le. mxnumdat .And. yendelm .Le. mynumdat) Then
 
!           Variance calculation must take place before
!           multiplication
              If (variance_exist) Then
 
                 Call MA_RVARMULT (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, %val(pMDATA), %val(pMVARIANCES), xmaxdat, &
                   ymaxdat, %val(pDATA), %val(pVARIANCES), status)
 
              End If
 
              Call MA_RMULTIPLY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, %val(pMDATA), xmaxdat, ymaxdat, %val(pDATA), status)
 
              command = 'FIT'
 
           Else
              warning = 3
           End If
 
        Else If (data_defined) Then
           warning = 2
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'NORMALISE') Then
 
        If (data_defined) Then
 
!        Normalise region of interest, by dividing all elements by
!        the maximum value
           Call F2D_NORMALISE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, variance_exist, %val(pMASK), %val(pDATA), &
             %val(pVARIANCES), status)
           command = 'OUTPUT DATA'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'OFFSET/SCALE') Then
 
        If (data_defined .And. memory_defined) Then
 
!        Calculate offset and scaling between two images
           Call F2D_OFFSET  (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, variance_exist, %val(pDATA), %val(pVARIANCES), &
             %val(pMDATA), %val(pMVARIANCES), status)
 
           command = 'CMULTIPLY'
 
        Else If (data_defined) Then
           warning = 2
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'OPEN LOG') Then
 
        If (log_file_open) Then
 
           Call IO_WRITE ('WARNING: Log file is already open', status)
        Else
 
!        Open log file:
           log_file_name = 'fit2d.log'
           Call IO_OPEN_LOGFILE (.True., log_file_name, status)
           log_file_open = .True.
        End If
        command = 'INPUT DATA'
 
     Else If (command .Eq. 'OUTPUT DATA') Then
 
        If (data_defined) Then
 
!        Output data to file
           Call FIO_OUTPUT (input_file, xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
             %val(pVARIANCES), title, xlabel, ylabel, zlabel, variance_exist, &
             experiment%x_pixel_size, experiment%y_pixel_size, status)
           command = 'INPUT DATA'
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'PAGE POSITION') Then
 
!     Interactive setting of graph page position
        Call GS_INP_GPP (status)
        command = display_command
 
     Else If (command .Eq. 'PAUSE' .Or. command .Eq. 'WAIT') Then
 
!     Wait until a user return to continue
        Call IO_WRITENA ( 'WAIT: Enter <RETURN> to continue: ', status)
        Read (*, *)
        command = display_command
 
     Else If (command .Eq. 'PEEP') Then
 
        If (data_defined) Then
 
!        User graphical input coordinate, output value of pixel
           Call F2D_PEEP (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
             %val(pVARIANCES), title, xlabel, ylabel, zlabel, variance_exist, &
             experiment, status)
           command = display_command
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'PIXEL REGION' .Or. command .Eq. 'ROI') Then
 
        If (data_defined) Then
 
!        Define region of interest using pixel limits
           Call F2D_PIXELREGION (xmaxdat, ymaxdat, xnumdat, ynumdat, xstrelm, &
             ystrelm, xendelm, yendelm, status)
           command = display_command
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'PLOT DATA') Then
 
        If (data_defined .And. output_graphics) Then
 
!        PLOT IMAGE: Produce image or 1-D X-Y graph
           Call GS_PLOT (xmaxdat, ymaxdat, %val(pDATA), %val(pXAXIS), &
             %val(pYAXIS), xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
             ylabel, zlabel, status)
           print_type = 'image'
 
!        Force up-dating of screen
           Call GS_UPDATE (status)
 
           command = 'Z-SCALE'
           display_command = 'PLOT DATA'
 
        Else If (output_graphics) Then
           warning = 1
        Else
           warning = 5
        End If
 
     Else If (command .Eq. 'POISSONIAN NOISE') Then
 
!     Add Poissonian noise statistics to data
 
        If (data_defined) Then
 
!        Apply Poisson noise to data values
           Call MA_2DPOISSON (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, -1.0, %val(pDATA), status)
 
           If (variance_exist) Then
 
!           Copy data values to variance array
              Call MA_RCOPY (xmaxdat, ymaxdat, %val(pDATA), xstrelm, ystrelm, &
                xendelm, yendelm, xmaxdat, ymaxdat, %val(pVARIANCES), status)
 
           End If
 
           command = display_command
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'POLARISATION EFFECT') Then
 
        If (data_defined) Then
 
!        Apply polarisation effect to image intensities
           Call F2D_POLARISATION (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, experiment, &
             variance_exist, %val(pDATA), %val(pVARIANCES), status)
 
           command = display_command
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'POSTSCRIPT OPTIONS') Then
 
!     Allow user to change options affecting PostScript output
        Call F2D_POSTSCRIPT (status)
        command = 'PRINT GRAPHICS'
 
     Else If (command .Eq. 'POWER SPECTRUM') Then
 
        If (data_defined) Then
 
!        Calculate power spectrum of the ROI
           Call F2D_POWERSPEC (.False., xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
             memory_defined, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
             myendelm, %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), mtitle, &
             mxlabel, mylabel, mzlabel, status)
           command = 'EXCHANGE'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'PREDICTOR') Then
 
        If (data_defined) Then
 
!        Calculate values after predictor in the memory
           Call F2D_PREDICTOR (.False., xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
             mxstrelm, mystrelm, mxendelm, myendelm, %val(pMXAXIS), &
             %val(pMYAXIS), %val(pMDATA), status)
 
!        Set data maximums
           mxnumdat = mxendelm
           mynumdat = myendelm
           mtitle = title
           mxlabel = xlabel
           mylabel = ylabel
           mzlabel = zlabel
           memory_defined = .True.
           command = 'EXCHANGE'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'PRINT GRAPHICS') Then
 
!     Output graphics to Hardcopy file
        Call F2D_PRINT (.False., print_type, mask_exist, xmaxdat, ymaxdat, &
          %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pVARIANCES), &
          %val(pMASK), title, xlabel, ylabel, zlabel, variance_exist, xstrelm, &
          ystrelm, xendelm, yendelm, status)
        command = 'EXIT'
 
     Else If (command .Eq. 'PUBLICATION QUALITY') Then
 
!     Set graphics attributes to publication quality values
        Call GS_MSET_PUBLICATION (status)
        command = display_command
 
     Else If (command .Eq. 'QUESTION') Then
 
!     Define interactive question for a macro
        Call F2D_QUESTION (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, %val(pDATA), %val(pXAXIS), %val(pYAXIS), title, xlabel, &
          ylabel, zlabel, status)
 
     Else If (command .Eq. 'RAISE TO A POWER') Then
 
        If (data_defined) Then
 
!        Raise data to specified power
           Call F2D_POWER (.False., xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, variance_exist, %val(pMASK), %val(pDATA), &
             %val(pVARIANCES), status)
           command = display_command
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'REBIN' .Or. command .Eq. 'RE-BIN') Then
 
        If (data_defined .And. memory_exist) Then
 
!        Rebin the data
           Call F2D_REBIN (.False., .False., &
             experiment%x_pixel_size, experiment%y_pixel_size, xmaxdat, &
             ymaxdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pMASK), &
             %val(pVARIANCES), xstrelm, ystrelm, xendelm, yendelm, &
             variance_exist, retstat, %val(pMXAXIS), %val(pMYAXIS), &
             %val(pMDATA), %val(pMVARIANCES), mxstrelm, mystrelm, mxendelm, &
             myendelm, mxnumdat, mynumdat, mx_pixel_size, my_pixel_size, &
             status)
 
!        Set memory labels
           mtitle = title
           mxlabel = xlabel
           mylabel = ylabel
           mzlabel = zlabel
 
           If (retstat .Eq. 0) Then
              memory_defined = .True.
              command = 'EXCHANGE'
           End If
 
        Else If (data_defined) Then
           warning = 4
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'RECALL') Then
 
        If (memory_defined) Then
 
!        Transfer data from memory arrays
           xnumdat = mxnumdat
           ynumdat = mynumdat
           xstrelm = mxstrelm
           ystrelm = mystrelm
           xendelm = mxendelm
           yendelm = myendelm
           title = mtitle
           xlabel = mxlabel
           ylabel = mylabel
           zlabel = mzlabel
           experiment%x_pixel_size = mx_pixel_size
           experiment%y_pixel_size = my_pixel_size
           Call MA_RCOPY (xmaxdat, ymaxdat, %val(pMDATA), 1, 1, xnumdat, &
             ynumdat, xmaxdat, ymaxdat, %val(pDATA), status)
           Call MA_RCOPY (xmaxdat, 1, %val(pMXAXIS), 1, 1, xnumdat, 1, &
             xmaxdat, 1, %val(pXAXIS), status)
           Call MA_RCOPY (ymaxdat, 1, %val(pMYAXIS), 1, 1, ynumdat, 1, &
             ymaxdat, 1, %val(pYAXIS), status)
 
           If (variance_exist) Then
              Call MA_RCOPY (xmaxdat, ymaxdat, %val(pMVARIANCES), 1, 1, &
                xnumdat, ynumdat, xmaxdat, ymaxdat, %val(pVARIANCES), status)
           End If
 
           data_defined = .True.
           command = 'FIT'
 
        Else
           warning = 2
        End If
 
     Else If (command .Eq. 'REFLECT') Then
 
        If (data_defined) Then
 
!        Reflect the data
           Call F2D_REFLECT (.False., xmaxdat, ymaxdat, %val(pXAXIS), &
             %val(pYAXIS), %val(pDATA), %val(pVARIANCES), xnumdat, ynumdat, &
             xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
             zlabel, variance_exist, &
             experiment%x_pixel_size, experiment%y_pixel_size, retstat, &
             memory_defined, %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), &
             %val(pMVARIANCES), mxnumdat, mynumdat, mxstrelm, mystrelm, &
             mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, status)
 
           command = 'EXCHANGE'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'REGION') Then
 
        If (data_defined) Then
 
!        Define data display region
           Call GS_INP_2DREGION ( xmaxdat, xnumdat, %val(pXAXIS), ymaxdat, &
             ynumdat, %val(pYAXIS), xstrelm, ystrelm, xendelm, yendelm, &
             status)
 
           command = display_command
        Else
           warning = 1
           command = 'INPUT DATA'
        End If
 
     Else If (command .Eq. 'RING (ADD POWDER RING)') Then
 
        If (data_defined) Then
 
!        User definition of powder diffraction ring to be added to the data
           Call F2D_POWDERRING (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, experiment, %val(pDATA), status)
 
           command = display_command
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'ROTATE LUT') Then
 
!     Interactive rotation of the colour look-up table
        Call F2D_ROTATELUT ( xmaxdat, ymaxdat, xnumdat, ynumdat, %val(pXAXIS), &
          %val(pYAXIS), %val(pDATA), %val(pVARIANCES), title, xlabel, ylabel, &
          zlabel, variance_exist, xstrelm, ystrelm, xendelm, yendelm, status)
 
     Else If (command .Eq. 'SAVE GEOMETRY') Then
 
!     Save experiment geometry parameters to a file
        Call F2D_SAVEGEOMETRY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, experiment%x_pixel_size, experiment%y_pixel_size, status)
 
     Else If (command .Eq. 'SELECT PIXEL OPERATION') Then
 
        If (data_defined) Then
 
!        Perform operation on limited pixel range
           Call F2D_SELECTPIXEL (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, %val(pDATA), status)
           command = display_command
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'SEQUENCE') Then
 
!     Set-up a processing sequence or carry out one part of the processing
        Call F2D_SEQUENCE (inmacro_file, status)
        command = 'EXIT'
 
     Else If (command .Eq. 'SET ANNOTATION STYLE') Then
 
!     Input requirements for annotation label styles
        Call GS_INP_ALABELSTYLES (status)
        command = display_command
 
     Else If (command .Eq. 'SET ARROW STYLE') Then
 
!     Input requirements for arrow style
        Call GS_INP_ARROWSTYLES (status)
        command = display_command
 
     Else If (command .Eq. 'SET AXES STYLE') Then
 
!     User input to define axes style
        Call GS_INP_AXESSTYLES (status)
        command = display_command
 
     Else If (command .Eq. 'SET BACKGROUND STYLE') Then
 
!     Input requirements for background style
        Call GS_INP_BACKGROUND (status)
        command = display_command
 
     Else If (command .Eq. 'SET ENUMERATION STYLE') Then
 
!     Input style attributes for axis numbering
        Call GS_INP_ENUMERATIONSTYLE (status)
        command = display_command
 
     Else If (command .Eq. 'SET COLOURS') Then
 
!     Input colour for graph lines, text, and markers
        Call GS_INP_ACOLOURS (status)
        command = display_command
 
     Else If (command .Eq. 'SET GRID STYLE') Then
 
!     User input to define grid line style
        Call GS_INP_GRIDSTYLE (status)
        command = display_command
 
     Else If (command .Eq. 'SET LAYOUT STYLE') Then
 
!     User input to define graphics layout distances
        Call GS_INP_LAYOUT (status)
        command = display_command
 
     Else If (command .Eq. 'SET PIXEL VALUE') Then
 
!     User input to define the value of a pixel
        Call F2D_PIXEL_VALUE (xmaxdat, ymaxdat, xnumdat, ynumdat, %val(pDATA), &
          status)
        command = display_command
 
     Else If (command .Eq. 'SET TICK POSITIONS') Then
 
!     User input to define method of calculating large tick mark
!     positions
        Call GS_INP_LTICKS (status)
        command = display_command
 
     Else If (command .Eq. 'SET TITLE STYLE') Then
 
!     User input to define title style
        Call GS_INP_TITLESTYLE (status)
        command = display_command
 
     Else If (command .Eq. 'SET X-LABEL STYLE') Then
 
!     User input to define X-LABEL style
        Call GS_INP_XLABELSTYLE (status)
        command = display_command
 
     Else If (command .Eq. 'SET Y-LABEL STYLE') Then
 
!     User input to define Y-LABEL style
        Call GS_INP_YLABELSTYLE (status)
        command = display_command
 
     Else If (command .Eq. 'SLEEP') Then
 
!     Pause for a user input number of seconds
        Call F2D_SLEEP (status)
        command = 'MESSAGE'
 
     Else If (command .Eq. 'SPATIAL FILTERING') Then
 
        If (data_defined) Then
 
           Call F2D_SPATIALFILTER (xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pDATA), %val(pMDATA), status)
           mxnumdat = xendelm
           mynumdat = yendelm
           mxstrelm = xstrelm
           mystrelm = ystrelm
           mxendelm = xendelm
           myendelm = yendelm
           mtitle = title
           mxlabel = xlabel
           mylabel = ylabel
           mzlabel = zlabel
           Call MA_RCOPY (xmaxdat, 1, %val(pXAXIS), 1, 1, xnumdat, 1, xmaxdat, &
             1, %val(pMXAXIS), status)
           Call MA_RCOPY (ymaxdat, 1, %val(pYAXIS), 1, 1, ynumdat, 1, ymaxdat, &
             1, %val(pMYAXIS), status)
           memory_defined = .True.
           command = 'EXCHANGE'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'START MACRO') Then
 
!     Open macro output file
        Call IO_OPEN_OUTMACRO (.False., .True., outmacro_file, status)
        command = 'INPUT DATA'
 
     Else If (command .Eq. 'STATISTICS') Then
 
!     Output statistics of ROI
        If (data_defined) Then
 
           Call F2D_STATISTICS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), status)
           command = 'CLOSE LOG'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'STOP MACRO') Then
 
!     Close macro output file
        Call IO_CLOSE_OUTMACRO (status)
 
!     Set default input macro file to the output macro file
        inmacro_file = outmacro_file
        command = 'SEQUENCE'
 
     Else If (command .Eq. 'STOP WATCH') Then

!     Interactive timing
        Call F2D_STOP_WATCH (status)

     Else If (command .Eq. 'STORE') Then
 
        If (data_defined .And. memory_exist) Then
 
!        Transfer data to memory arrays
           mxnumdat = xnumdat
           mynumdat = ynumdat
           mxstrelm = xstrelm
           mystrelm = ystrelm
           mxendelm = xendelm
           myendelm = yendelm
           mtitle = title
           mxlabel = xlabel
           mylabel = ylabel
           mzlabel = zlabel
           mx_pixel_size = experiment%x_pixel_size
           my_pixel_size = experiment%y_pixel_size
           Call MA_RCOPY (xmaxdat, ymaxdat, %val(pDATA), 1, 1, xnumdat, &
             ynumdat, xmaxdat, ymaxdat, %val(pMDATA), status)
           Call MA_RCOPY (xmaxdat, 1, %val(pXAXIS), 1, 1, xnumdat, 1, xmaxdat, &
             1, %val(pMXAXIS), status)
           Call MA_RCOPY (ymaxdat, 1, %val(pYAXIS), 1, 1, ynumdat, 1, ymaxdat, &
             1, %val(pMYAXIS), status)
 
           If (variance_exist) Then
              Call MA_RCOPY (xmaxdat, ymaxdat, %val(pVARIANCES), 1, 1, &
                xnumdat, ynumdat, xmaxdat, ymaxdat, %val(pMVARIANCES), status)
           End If
 
           memory_defined = .True.
           command = 'INPUT DATA'
 
        Else If (data_defined) Then
           warning = 4
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'SUBTRACT') Then
 
        If (memory_defined .And. data_defined) Then
 
!        Subtract memory from current data in region of interest
           If (xendelm .Le. mxnumdat .And. yendelm .Le. mynumdat) Then
 
              Call MA_RSUBTRACT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, %val(pMDATA), xmaxdat, ymaxdat, %val(pDATA), status)
 
              If (variance_exist) Then
 
                 Call MA_RADD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                   yendelm, %val(pMVARIANCES), xmaxdat, ymaxdat, &
                   %val(pVARIANCES), status)
 
              End If
 
              command = display_command
 
           Else
              warning = 3
           End If
 
        Else If (data_defined) Then
           warning = 2
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'SURFACE INTERPOLATION') Then
 
        If (data_defined .And. memory_exist) Then
 
!        Calculate interpolated surface
           Call F2D_SURFACE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, %val(pDATA), %val(pXAXIS), %val(pYAXIS), title, xlabel, &
             ylabel, zlabel, memory_defined, %val(pMDATA), status)
 
!        Transfer ROI values, labels, etc. from main data
           mxnumdat = xendelm
           mynumdat = yendelm
           mxstrelm = xstrelm
           mystrelm = ystrelm
           mxendelm = xendelm
           myendelm = yendelm
           mtitle = 'INTERPOLATED SURFACE'
           mxlabel = xlabel
           mylabel = ylabel
           mzlabel = zlabel
           Call MA_RCOPY (xmaxdat, 1, %val(pXAXIS), 1, 1, xnumdat, 1, xmaxdat, &
             1, %val(pMXAXIS), status)
           Call MA_RCOPY (ymaxdat, 1, %val(pYAXIS), 1, 1, ynumdat, 1, ymaxdat, &
             1, %val(pMYAXIS), status)
 
           command = 'EXCHANGE'
 
        Else If (data_defined) Then
           warning = 4
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'SYMMETRIC FUNCTION') Then
 
        If (data_defined .And. memory_defined) Then
 
!        Add circularly symmetric function to data
           Call F2D_SYMFUN (mx_pixel_size, xmaxdat, mxnumdat, %val(pMDATA), &
             experiment%x_pixel_size, experiment%y_pixel_size, &
             xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pDATA), status)
           command = display_command
 
        Else If (data_defined) Then
           warning = 2
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'THRESHOLD') Then
 
        If (data_defined) Then
 
!        Threshold data with user input values
           Call F2D_THRESHOLD (.False., xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pMASK), %val(pDATA), status)
           command = display_command
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'TITLE') Then
 
!     Input new title
        Call IO_INPC (.True., 'NEW TITLE', 1, 'Enter new title', 1, &
          'Enter valid characters', 1, title, status)
        command = display_command
 
     Else If (command .Eq. 'TRANSPOSE') Then
 
        If (data_defined) Then
 
!        Transpose data arrays
           Call F2D_TRANSPOSE (.False., xmaxdat, ymaxdat, %val(pXAXIS), &
             %val(pYAXIS), %val(pDATA), %val(pVARIANCES), xnumdat, ynumdat, &
             xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
             zlabel, variance_exist, retstat, memory_defined, %val(pMXAXIS), &
             %val(pMYAXIS), %val(pMDATA), %val(pMASK), %val(pMVARIANCES), &
             mxnumdat, mynumdat, mxstrelm, mystrelm, &
             mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, status)
 
           If (retstat .Eq. 0) Then
              mx_pixel_size = experiment%x_pixel_size
              my_pixel_size = experiment%y_pixel_size
              command = 'EXCHANGE'
           Else
              command = 'ROI'
           End If
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'UN-DEFINE VARIABLE') Then
 
!     Un-define program variable
        Call F2D_UNDEFINE (status)
        command = 'MACRO'
 
     Else If (command .Eq. 'UNIT CELL PARAMETERS') Then
 
!     Interactive definition of unit cell, and output of
!     corresponding reciprocal cell parameters
        Call F2D_RECIPROCAL (status)
 
     Else If (command .Eq. 'VARIANCES DEFINITION') Then
 
        If (data_defined .And. variance_exist) Then
 
!        Estimate variances from current data
           Call F2D_VARIANCES (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, %val(pDATA), %val(pVARIANCES), status)
 
           command = 'OUTPUT DATA'
 
        Else If (data_defined) Then
           warning = 6
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'V2C') Then
 
        If (data_defined .And. variance_exist) Then
 
!        Swap variance array with current data array
           Call IO_ISWAP (pDATA, pVARIANCES, status)
           command = display_command
 
        Else If (data_defined) Then
           warning = 6
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'WEIGHTED AVERAGE') Then
 
        If (data_defined .And. memory_defined .And. variance_exist) Then
 
!        Calculate weighted average
           Call MA_WAVERAGE  (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, %val(pMDATA), %val(pMVARIANCES), xmaxdat, ymaxdat, &
             %val(pDATA), %val(pVARIANCES), status)
 
           command = 'OUTPUT DATA'
 
        Else If (data_defined) Then
           warning = 6
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'X-AXIS LABEL') Then
 
!     Enter X-axis label for graphics
        Call IO_INPC (.True., 'LABEL TEXT', 1, &
          'Enter X-axis label text for graphics', 1, 'Enter text or return', &
          1, xlabel, status)
        command = display_command
 
     Else If (command .Eq. 'Y-AXIS LABEL') Then
 
!     Enter Y-axis label for graphics
        Call IO_INPC (.True., 'LABEL TEXT', 1, &
          'Enter Y-axis label text for graphics', 1, 'Enter text or return', &
          1, ylabel, status)
        command = display_command
 
     Else If (command .Eq. 'Z-AXIS LABEL') Then
 
!     Enter Z-axis label for graphics
        Call IO_INPC (.True., 'LABEL TEXT', 1, &
          'Enter Z-axis (intensity) label text for graphics', 1, &
          'Enter text or return', 1, zlabel, status)
        command = display_command
 
     Else If (command .Eq. 'Z-SCALE') Then
 
!     Set image scaling mode
        Call F2D_ZSCALE (status)
        command = display_command
 
     Else If (command .Eq. 'ZZ_TEST') Then

        Do j = 1, 1000000

           Do i = 0, 9999

              Write (message, &
                '(''INFO: Writing canna_mesh2_'', i4.4, ''.edf '', i8)') i, j
              Call IO_WRITE (message, status)

           End Do

        End Do

     Else If (command .Eq. '1-D INTERPOLATION') Then
 
        If (data_defined) Then
 
!        Issue temporary warning message since option has been added as a quick 
!        fix
           Call IO_WRITE ('WARNING: This option has ' // &
             'been added as a quick fix. In the future it', status)
           Call IO_WRITE ('         will be thought ' // &
             'out more properly and the user questions and', status)
           Call IO_WRITE ('         the action will change.', status)
 
!        Apply 1-D linear interpolation correct to data
           Call F2D_1DINTERPOLATE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
             status)
           command = display_command
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. '3-D SURFACE PLOT') Then
 
        If (data_defined) Then
 
           If (yendelm-ystrelm .Gt. 1) Then
 
!           3-D plot: Produce 3-d surface display of data array
              Call F2D_3DSURFACE (print_type, xmaxdat, ymaxdat, %val(pDATA), &
                %val(pXAXIS), %val(pYAXIS), xstrelm, ystrelm, xendelm, &
                yendelm, title, xlabel, ylabel, zlabel, status)
 
           Else If (ystrelm .Eq. 1) Then
 
!           Plot X-Y graph
              Call F2D_XYGRAPH (.True., xmaxdat, ymaxdat, xstrelm, ystrelm, &
                xendelm, yendelm, variance_exist, %val(pDATA), &
                %val(pVARIANCES), %val(pXAXIS), %val(pYAXIS), title, xlabel, &
                ylabel, zlabel, status)
 
           End If
 
           command = 'PRINT GRAPHICS'
 
        Else
           warning = 1
        End If
 
     Else If (command .Eq. 'null') Then
 
!     Null command returned do nothing
        Continue
 
     Else
 
!     Unknown command
        Call IO_WRITE ('WARNING: Unknown command, please re-enter', status)
        command = 'INPUT DATA'
 
     End If
 
     End Subroutine F2D_KEYSUB
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
