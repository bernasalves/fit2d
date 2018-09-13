!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_composite.f90 *
!  *                   *
!  *********************
 
!+ F2D_COMPOSITE - FIT 2-D COMPOSITE image
     Subroutine F2D_COMPOSITE (input_options, &
       data_defined, memory_exist, memory_defined, &
       variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
       xlabel, ylabel, zlabel, experiment, xstrelm, ystrelm, &
       xendelm, yendelm, status) ! X_AXIS, Y_AXIS, DATA, VARIANCES,
!  Description:
!    Create composite image from a series of files
!  Keywords:
!    Composite.Image~Display, Display~Image.Composite
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Jun-2009: V0.20 Put back rebinning code, as problem seems to be
!      compiler / system related (works fine on Linux) (Hammersley)
!    18-May-2009: V0.19 Temporarily remove rebinning code for test (Hammersley)
!    12-May-2009: V0.18 Checking possible memory overwrite (Hammersley)
!    04-May-2009: V0.17 Debugging crash (Hammersley)
!    25-Apr-2006: V0.16 Use Fortran-90 dynamically allocated arrays (Hammersley)
!    24-Apr-2006: V0.15 Add "INPUT_OPTIONS" structure (Hammersley)
!    14-Mar-2006: V0.14 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    05-Mar-2004: V0.13 Output warning message when file is missing (Hammersley)
!    21-Dec-1999: V0.12 Correct problem of not being able to input
!      background image after ROI selection with a TIFF image (Hammersley)
!    10-Jun-1999: V0.11 Checking input of zoomed in images (Hammersley)
!    17-Feb-1999: V0.10 "F2D_GUI_INPUT" replaced by "FIO_GUI_INPUT" (Hammersley)
!    09-Jul-1998: V0.9 Replace call to "Io_nopath" with "IO_NODIRPATH" 
!      (Hammersley)
!    02-Jul-1998: V0.8 Correct placement of images when there is only
!      1 image per row (Hammersley)
!    30-Jun-1998: V0.7 Make "dummy" into a dummy array, and declare
!      unused character string for call to "GS_FORM" (Hammersley)
!    22-Feb-1998: V0.6 Use "IO_FILENAME" to generate file names (Hammersley)
!    20-Feb-1998: V0.5 Call to "F2D_DEF_FS" (Hammersley)
!    27-Jan-1998: V0.4 Output size of new internal arrays (Hammersley)
!    26-Jan-1998: V0.3 Reduce size of temporary arrays, and set
!      composite title to be only based on the file names and not the
!      full paths (Hammersley)
!    23-Jan-1998: V0.2 Changes to "F2D_GUI_INPUT" so that the
!      user prompt text is set by the calling routine (Hammersley)
!    23-Dec-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined ! .True., if data exists within
!      the program
     Logical, Intent(INOUT) :: memory_exist ! .True. if memory array exists
     Logical, Intent(INOUT) :: memory_defined ! .True. if the memory
!      contains data
     Logical, Intent(INOUT) :: variances_exist ! .True., if a data variance
!      array is created
     Logical, Intent(INOUT) :: mask_exist ! .True., if the mask array exists
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
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.20' ! Version number
     Integer, Parameter :: Max_chars = 42 ! Maximum number of characters in
!      a line
     Integer, Parameter :: Max_lines = 55 ! Number of lines in message
     Integer, Parameter :: Max_choices = 10 ! Maximum number of choices
!      (need 5 extra spaces)
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection prompt 
!      text
!  Local Variables:
     Character(Len = 256) :: back_file_name = ' ' ! Full name of background file
     Character(Len = 256) :: extension ! File name extension
     Character(Len = 256) :: file_name ! Name of file in the series
     Character(Len = 80) :: file1 ! First file name without directory path
     Character(Len = 80) :: file2 ! Second file name without directory path
     Character(Len = 256) :: first_file ! Full name of first file
     Character(Len = 256) :: last_file ! Full name of last file
     Character(Len = 256) :: postfix ! Fixed end of file names
     Character(Len = 256) :: prefix ! Fixed start of file names
     Character(Len = 20) :: print_type ! Type, of graphics to print
     Integer :: end_value ! Value at end of sequence
     Integer, Save :: increment = 1 ! Step value in sequence
     Integer :: file ! Loop variable for files
     Integer :: first_image ! First image in series (not used)
     Integer :: last_image ! Last image in series (not used)
     Integer :: num_characters ! Number of characters in numerical part
     Integer :: num_choices ! Number of choices in form
     Integer :: num_images ! Number of images of input
     Integer :: num_horizontally ! Number of images horizontally
     Integer :: num_vertically ! Number of images vertically
     Integer, Save :: rebin_factor = 1 ! Factor to rebin data
     Integer :: retstat ! Return status variable
     Integer stat ! Status return variable for "Allocate"
     Integer :: start_value ! Value at start of sequence
     Integer :: xendout ! X-end of the rebinned output region
     Integer :: xmax_new ! Necessary first dimension size of program arrays
     Integer :: xnumcom ! Number of defined X-pixels in composite array
     Integer :: xrebin ! Number of X-pixels in a rebinned image
     Integer :: xstrout ! X-start of the rebinned output region
     Integer :: yendout ! Y-end of the rebinned output region
     Integer :: ymax_new ! Necessary second dimension size of program arrays
     Integer :: ynumcom ! Number of defined Y-pixels in composite array
     Integer :: yrebin ! Number of Y-pixels in a rebinned image
     Integer :: ystrout ! Y-start of the rebinned output region
     Logical :: file_ok ! .True., when the input file is O.K.
     Logical :: recreate_arrays ! .True., if the program array are to
!      be destroyed and re-allocated
     Logical, Save :: roi = .True. ! .True., if a region of interest is to
!      selected from each input image
     Logical, Save :: subtract = .True. ! .True., if background is to
!      subtracted from each image
     Logical :: variable_characters ! .True., if the number of characters
!      in the numerical part of the names changes
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 70) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 80) :: MESSAGE(7) ! User messages
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = Max_chars) :: TX(Max_lines) ! Text array
     Integer :: DUMMY(2) ! Dummy array, for un-used arguments
     Integer :: INTEGERS(Max_choices) ! Integer variables to be input
     Integer :: INTS_LOWER(Max_choices) ! Lower bound of integer variables
     Integer :: INTS_UPPER(Max_choices) ! Upper bound of integer variables
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
     Logical :: BOUND(Max_choices) ! .True., if variable range is bound
     Logical :: LOGICALS(Max_choices) ! Logical variables to be input
     Real, Allocatable :: BACK(:, :) ! Dynamic array to contain background
     Real, Allocatable :: TMP(:, :) ! Dynamic array to contain input data
     Real, Allocatable :: TMPBACK(:, :) ! Dynamic array to contain temporary
!      background
!  External Functions:
!  Local Data:
     Data TX(   1) / ' ' /
     Data TX(   2) / '      ---------------------------'/
     Data TX(   3) / '      COMPOSITE COMMAND HELP TEXT'/
     Data TX(   4) / '      ---------------------------'/
     Data TX(   5) / ' ' /
     Data TX(   6) / 'The composite command allows a series of'/
     Data TX(   7) / 'images to be combined together into a'/
     Data TX(   8) / 'composite image. The series is defined by'/
     Data TX(   9) / 'the first and the last files in the'/
     Data TX(  10) / 'series, and by the incremental jump from'/
     Data TX(  11) / 'one image to the next e.g. it is possible'/
     Data TX(  12) / 'to input every other file in a series.'/
     Data TX(  13) / ' ' /
     Data TX(  14) / 'Optionally a background e.g. dark current,'/
     Data TX(  15) / 'image may be selected and subtracted from'/
     Data TX(  16) / 'each input image in the series, and a'/
     Data TX(  17) / 'region of interest (ROI) may be selected'/
     Data TX(  18) / 'and used to form the composite image. The'/
     Data TX(  19) / 'number of images per row of the composite'/
     Data TX(  20) / 'image may be set, which automatically'/
     Data TX(  21) / 'determines the number of lines. Since the'/
     Data TX(  22) / 'composite image could be very large the'/
     Data TX(  23) / 'option to re-bin the input prior to'/
     Data TX(  24) / 'assembing exists.'/
     Data TX(  25) / ' ' /
     Data TX(  26) / 'The following control "buttons" are'/
     Data TX(  27) / 'available:'/
     Data TX(  28) / ' ' /
     Data TX(  29) / '"SUBTRACT": "YES" to select a background'/
     Data TX(  30) / 'image which will be subtracted from each'/
     Data TX(  31) / 'input image prior to further operations.'/
     Data TX(  32) / ' ' /
     Data TX(  33) / '"ROI": "YES" to select a "REGION OF'/
     Data TX(  34) / 'INTEREST" e.g. zoom in one selected'/
     Data TX(  35) / 'region and form the composite image'/
     Data TX(  36) / 'from the defined region from each image'/
     Data TX(  37) / 'in the series.'/
     Data TX(  38) / ' ' /
     Data TX(  39) / '"INCREMENT": The step from one file in'/
     Data TX(  40) / 'the series to be used to the next e.g.'/
     Data TX(  41) / '1 = use every file, 2 = use every'/
     Data TX(  42) / 'second file, etc.'/
     Data TX(  43) / ' ' /
     Data TX(  44) / '"NO. PER ROW": Number of raw images to'/
     Data TX(  45) / 'be asembled together on one line of the'/
     Data TX(  46) / 'composite image.'/
     Data TX(  47) / ' ' /
     Data TX(  48) / '"RE-BIN NO.": Re-bin factor of the raw'/
     Data TX(  49) / 'images prior to assembling in the'/
     Data TX(  50) / 'composite image. This re-binning'/
     Data TX(  51) / 'applies to both directions.'/
     Data TX(  52) / ' ' /
     Data TX(  53) / '---------------'/
     Data TX(  54) / 'END OF HELP TEXT'/
     Data TX(  55) / '---------------'/
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_COMPOSITE ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_COMPOSITE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Define file series
        Call F2D_DEF_FS (input_options, &
          .True., variances_exist, xmaxdat, ymaxdat, retstat, &
          xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
          %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
          xstrelm, ystrelm, xendelm, yendelm, first_file, &
          first_image, last_file, last_image, start_value, end_value, prefix, &
          variable_characters, num_characters, postfix, extension, increment, &
          experiment, status)
 
!     Check return status
        If (retstat .Ne. 0) Then
           Return
        End If
 
!     Add the "postfix" to the extension if it exists
        If (Len_trim(postfix) .Gt. 0) Then
 
           If (Len_trim(extension) .Gt. 0) Then
              extension = Trim(postfix) // Trim(extension)
           Else
              extension = Trim(postfix)
           End If
 
        End If
 
!     Remove directory path from full file name
        Call IO_NODIRPATH (first_file, file1, status)
 
!     Check file sequence
        num_images = 1
        Do file = start_value + increment, end_value, increment
           num_images = num_images + 1
        End Do
 
!     Calculate appropriate (?) number horizontally
        num_horizontally = Int(Sqrt(Real(num_images) / 2.0) + 0.99)
        rebin_factor = Min(8, num_horizontally)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Control form
 
!     Allow user of choose number of output bins and control various other 
!     options
        num_choices = 4
        PROMPT(1) = 'CONTROL OF COMPOSITE IMAGE'
        PROMPT(2) = 'DISPLAY PARAMETERS'
        BUTTONS(1) = 'SUBTRACT'
        BUTTONS(2) = 'ROI'
        BUTTONS(3) = 'NO. PER ROW'
        BUTTONS(4) = 'RE-BIN NO.'
 
        TYPES(1) = Gs_logical
        TYPES(2) = Gs_logical
        TYPES(3) = Gs_integer
        TYPES(4) = Gs_integer
 
        TEXT(1) = 'SUBTRACT BACKGROUND IMAGE FROM DATA'
        TEXT(2) = 'SELECT REGION OF INTEREST'
        TEXT(3) = 'NUMBER OF IMAGES PER ROW'
        TEXT(4) = 'FACTOR BY WHICH TO RE-BIN'
        FULL_PROMPTS(1) = 'Enter "YES" to input an image' // &
          'which will subtracted from all the input images'
        FULL_PROMPTS(2) = 'Enter "YES" to be able to zoom in' // &
          'on a region of interest for the composite image'
        FULL_PROMPTS(3) = 'Enter number of images which will be ' // &
          'displayed on one line of the composite image'
        FULL_PROMPTS(4) = 'Enter the factor by which input ' // &
          'pixels will be re-binned (in both directions)'
        BOUND(1) = .True.
        BOUND(2) = .True.
        BOUND(3) = .True.
        BOUND(4) = .True.
 
        INTS_LOWER(3) = 1
        INTS_UPPER(3) = num_images
        INTS_LOWER(4) = 1
        INTS_UPPER(4) = Max(32, num_horizontally)
 
        LOGICALS(1) = subtract
        LOGICALS(2) = roi
        INTEGERS(3) = num_horizontally
        INTEGERS(4) = rebin_factor
 
!     Output interactive graphical form
        Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, Max_choices, &
          num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, INTS_LOWER, &
          INTS_UPPER, DUMMY, DUMMY, INTEGERS, LOGICALS, DUMMY, STRINGS, &
          retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           Return
        End If
 
!     Set resulting values
        subtract = LOGICALS(1)
        roi = LOGICALS(2)
        num_horizontally = INTEGERS(3)
        rebin_factor = INTEGERS(4)
 
!     Count number of images to display
        num_images = 1
        Do file = start_value + increment, end_value, increment
           num_images = num_images + 1
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Number of image lines
        num_vertically = Int( Real(num_images) / Real(num_horizontally) + 0.999)
 
        If (roi) Then
 
!        Select region of interest
           Call F2D_GUI_ROI (xmaxdat, ymaxdat, xnumdat, ynumdat, %val(pXAXIS), &
             %val(pYAXIS), %val(pDATA), %val(pVARIANCES), title, xlabel, &
             ylabel, zlabel, variances_exist, experiment, &
             xstrelm, ystrelm, xendelm, yendelm, print_type, status)
 
        End If
 
        If (subtract) Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''F2D_COMPOSITE: before allocate TMPBACK'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!        Create temporary array to contain background image
           Allocate (TMPBACK(xnumdat, ynumdat), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_COMPOSITE ' // Version)
              Return
           End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''F2D_COMPOSITE: before allocate BACK'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!        Create temporary array to contain background image
           Allocate (BACK(xendelm, yendelm), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_COMPOSITE ' // Version)
              Return
           End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''F2D_COMPOSITE: after allocate BACK'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!        Input background image
           file_ok = .False.
           Do While (.Not. file_ok)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''F2D_COMPOSITE: before input background'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

              PROMPT(1) = 'SELECT FILE CONTAINING BACKGROUND IMAGE'
              PROMPT(2) = '(click on "HELP" for list of formats)'
              Call FIO_GUI_INPUT (&
                Max_prompt, 2, PROMPT, 0, input_options, xnumdat, ynumdat, &
                variances_exist, data_defined, back_file_name, xnumdat, &
                ynumdat, %val(pXAXIS), %val(pYAXIS), TMPBACK, &
                %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                experiment, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''F2D_COMPOSITE: after input background'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!           Display image
              Call GS_PLOT (xnumdat, ynumdat, TMPBACK, %val(pXAXIS), &
                %val(pYAXIS), xstrelm, ystrelm, xendelm, yendelm, &
                'Background', xlabel, ylabel, zlabel, status)
 
!           Check that the background file is O.K.
              Call GS_INPL (.True., 0, 1, .True., 'FILE O.K.', 1, &
                'Enter "YES" if the file is O.K.', 1, 'Enter "YES" or "NO"', &
                file_ok, status)
 
!           Check for user escape
              If (status .Ne. St_goodvalue) Then
                 Return
              End If
 
           End Do
 
!        Copy background to temporary array
           Call MA_RMOVE (xnumdat, ynumdat, TMPBACK, xstrelm, ystrelm, &
             xendelm, yendelm, xendelm, yendelm, xstrelm, ystrelm, BACK, status)
 
!        Free temporary background array
           Deallocate (TMPBACK)
 
        End If
 
!     Create temporary array to contain input data
        Allocate (TMP(xendelm, yendelm), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_COMPOSITE ' // Version)
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Automatic part of sequence processing
 
!     Copy input data to temporary array
        Call MA_RCOPY (xmaxdat, ymaxdat, %val(pDATA), xstrelm, ystrelm, &
          xendelm, yendelm, xendelm, yendelm, TMP, status)
 
        If (subtract) Then
 
!        Subtract background
           Call MA_RSUBTRACT (xendelm, yendelm, xstrelm, ystrelm, xendelm, &
             yendelm, BACK, xendelm, yendelm, TMP, status)
 
        End If
 
!     Calculate necessary program dimensions
        xmax_new = (xendelm - xstrelm + 1) / rebin_factor * num_horizontally
        ymax_new = (yendelm - ystrelm + 1) / rebin_factor * num_vertically
 
        If (xmax_new .Gt. xmaxdat .Or. ymax_new .Gt. ymaxdat) Then
 
           Write (MESSAGE(1), '(''INFO: Required size for internal arrays ' // &
             '= '', i6, '' * '', i6, '' pixels'')') xmax_new, ymax_new
           Call IO_WRITE (MESSAGE(1), status)
 
           recreate_arrays = .True.
           MESSAGE(1) = 'The program arrays need to be bigger, this'
           MESSAGE(2) = 'will destroy any existing data in the '
           MESSAGE(3) = '"memory".'
           MESSAGE(4) = '   Enter "YES" to create new arrays.'
           Call GS_INPL (.True., 0, 1, .True., &
             'DESTROY AND CREATE BIGGER PROGRAM ARRAYS', 4, MESSAGE, 1, &
             'Enter "YES" or "NO"', recreate_arrays, status)
 
           If (recreate_arrays) Then
 
!           Set new array sizes
              xmaxdat = xmax_new
              ymaxdat = ymax_new
 
!           Free existing internal arrays
              Call IO_FREE (pDATA, status)
              Call IO_FREE (pXAXIS, status)
              Call IO_FREE (pYAXIS, status)
 
              If (memory_exist) Then
                 Call IO_FREE (pMDATA, status)
                 Call IO_FREE (pMXAXIS, status)
                 Call IO_FREE (pMYAXIS, status)
              End If
 
              If (variances_exist) Then
                 Call IO_FREE (pVARIANCES, status)
 
                 If (memory_exist) Then
                    Call IO_FREE (pMVARIANCES, status)
                 End If
 
              End If
 
              If (mask_exist) Then
                 Call IO_FREE (pMASK, status)
              End If
 
!           Now recreate dynamic arrays of specified size
              Call IO_MALLOC (xmaxdat * ymaxdat * 4, pDATA, status)
              Call IO_MALLOC (xmaxdat * 4, pXAXIS, status)
              Call IO_MALLOC (ymaxdat * 4, pYAXIS, status)
 
              If (memory_exist) Then
                 Call IO_MALLOC (xmaxdat * ymaxdat * 4, pMDATA, status)
                 Call IO_MALLOC (xmaxdat * 4, pMXAXIS, status)
                 Call IO_MALLOC (ymaxdat * 4, pMYAXIS, status)
              End If
 
              If (variances_exist) Then
                 Call IO_MALLOC (xmaxdat * ymaxdat * 4, pVARIANCES, status)
 
                 If (memory_exist) Then
                    Call IO_MALLOC (xmaxdat * ymaxdat * 4, pMVARIANCES, status)
                 End If
 
              End If
 
              If (mask_exist) Then
 
!              Re-create mask array
                 Call IO_MALLOC (xmaxdat * ymaxdat, pMASK, status)
 
              End If
 
!           Data and memory now no longer exists
              data_defined = .False.
              memory_defined = .False.
 
           Else
 
!           No new arrays, exit
              Return
 
           End If
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        xrebin = (xendelm - xstrelm + 1) / rebin_factor
        yrebin = (yendelm - ystrelm + 1) / rebin_factor
 
        xnumcom = xrebin * num_horizontally
        ynumcom = yrebin * num_vertically

        If (xnumcom .Gt. xmaxdat .Or. ynumcom .Gt. ymaxdat) Then
           MESSAGE(1) = 'Total output size exceeds defined array sizes'
           MESSAGE(2) = 'Choose less files, smaller ROI, or greater rebinning'
           MESSAGE(3) = 'or define larger program arrays'
           Call GS_FWARNING (3, 3, MESSAGE, status)
           Return
        End If

!     Initialise program array
        Call MA_RVALUE (xmaxdat, ymaxdat, 1, 1, xnumcom, ynumcom, 0.0, &
          %val(pDATA), status)
 
        If (mask_exist) Then
 
!        Initialise mask to all good elements
           Call MA_L1VALUE (xmaxdat, ymaxdat, 1, 1, xnumcom, ynumcom, &
             .False., %val(pMASK), status)

        End If

!     Rebin and transfer first image to composite array
        Call MA_RREBIN (xendelm, yendelm, TMP, xstrelm, ystrelm, &
          xendelm, yendelm, rebin_factor, rebin_factor, .True., 1, yrebin * &
          (num_vertically - 1) + 1, xmaxdat, ymaxdat, %val(pDATA), xendout, &
          yendout, status)
 
        xstrout = xrebin + 1
        ystrout = yrebin * (num_vertically - 1) + 1
 
        If (xstrout .Gt. xrebin * num_horizontally) Then
           xstrout = 1
           ystrout = ystrout - yrebin
        End If
 

!     User progress macro
        Call GS_BACKGROUND (status)
        Call GS_FPROMPT (1, 1, 'AUTOMATIC SEQUENCE INPUT STARTED', status)
        Call GS_UPDATE (status)
        Call IO_WRITE ('INFO: Start of creating composite image', status)
 
        Do file = start_value + increment, end_value, increment
 
!        Generate input file name
           Call IO_FILENAME (prefix, file, .Not. variable_characters, &
             num_characters, extension, retstat, file_name, status)
 
!        Input file
           data_defined = .False.
           Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, -2147483647, &
             input_options, xendelm, yendelm, variances_exist, data_defined, &
             file_name, xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), TMP, &
             %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
             experiment, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''F2D_COMPOSITE: After FIO_GUI_INPUT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG


           If (data_defined) Then
 
              Call IO_WRITE ('INFO: Input ' // file_name, status)
 
              If (subtract) Then
 
!              Subtract background
                 Call MA_RSUBTRACT (xendelm, yendelm, xstrelm, ystrelm, &
                   xendelm, yendelm, BACK, xendelm, yendelm, TMP, status)
 
              End If
 
!           Rebin and transfer image to composite array

! TEMPORARILY REMOVE REBINNING CODE
              Call MA_RREBIN (xendelm, yendelm, TMP, xstrelm, ystrelm, &
                xendelm, yendelm, rebin_factor, rebin_factor, .True., xstrout, &
                ystrout, xmaxdat, ymaxdat, %val(pDATA), xendout, yendout, &
                status)
 
           Else
 
!           File not input
              Call IO_WRITE ('WARNING: File not input: ' // file_name, status)
 
           End If
 
           xstrout = xstrout + xrebin
           If (xstrout .Gt. xrebin * num_horizontally) Then
              xstrout = 1
              ystrout = ystrout - yrebin
           End If
 
        End Do
 
!     Free dynamic memory
        Deallocate (TMP)
 
        If (subtract) Then
           Deallocate (BACK)
        End If
 
        data_defined = .True.
 
!     Set defined data region
        xstrelm = 1
        ystrelm = 1
        xendelm = xnumcom
        yendelm = ynumcom
        xnumdat = xnumcom
        ynumdat = ynumcom
 
!     Set combined title
        Call IO_NODIRPATH (last_file, file2, status)
        title = Trim(file1) // ' to ' // Trim(file2)
 
!     Set axis values
        Call F2D_AXES (xmaxdat, xnumdat, 0.0, 1.0 / Real(rebin_factor), &
          %val(pXAXIS), status)
        Call F2D_AXES (ymaxdat, ynumdat, 0.0, 1.0 / Real(rebin_factor), &
          %val(pYAXIS), status)
 
     End If
 
     End Subroutine F2D_COMPOSITE
!********1*********2*********3*********4*********5*********6*********7*********8
