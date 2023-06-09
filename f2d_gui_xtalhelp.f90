!********1*********2*********3*********4*********5*********6*********7*********8

!               f2d_gui_xtalhelp.f90

! DO NOT ALTER THIS FILE

!  This file has been created automatically by "maketext"
!  from the ASCII text file:  f2d_gui_xtalhelp.text 
!  which should be edited and maketext should be re-run to 
!  create a new version of the subroutine.

!+ F2D_GUI_XTALHELP interactive user help/message display facility
    Subroutine F2D_GUI_XTALHELP (gui, status)

! Type Definitions:
    Implicit None

! Usage:
!   Private

! Import:
    Logical gui ! .True., if GUI is to be used
! Status:
    Integer status ! Status return variable

! Local Constants:
    Integer, Parameter :: Max_chars =  48 ! Maximum characters per line 
    Integer, Parameter :: Max_lines  =  119 ! Number of lines in message
! Local Variables:
     Integer font_index ! Saves current font index
! Local Arrays:
    Character*(Max_chars) TX(Max_lines) ! Text array
! External Functions:
    Integer, External :: St_good ! Returns good status value
! Local Data:
    Data TX(   1) / &
      ' ' /
    Data TX(   2) / &
      '         ------------------------------------'/
    Data TX(   3) / &
      '         Welcome to the FIT2D On-Line'/
    Data TX(   4) / &
      '         Crystallography Graphical Menu'/
    Data TX(   5) / &
      '         ------------------------------------'/
    Data TX(   6) / &
      ' ' /
    Data TX(   7) / &
      'The commands here allow you to monitor data'/
    Data TX(   8) / &
      'quality from diffraction images on-line'/
    Data TX(   9) / &
      'with automatic input and display of data as'/
    Data TX(  10) / &
      'they are available.'/
    Data TX(  11) / &
      ' ' /
    Data TX(  12) / &
      'The peak search option identifies peaks and'/
    Data TX(  13) / &
      'performs a simple box integration. This allows'/
    Data TX(  14) / &
      'estimates of the number peaks with different'/
    Data TX(  15) / &
      'I/Sigma(I) to be displayed, and if the'/
    Data TX(  16) / &
      'diffraction geometry has been defined, the'/
    Data TX(  17) / &
      'number of peaks in different resolution'/
    Data TX(  18) / &
      'shells, and the average I/Sigma(I) in each'/
    Data TX(  19) / &
      'shell is displayed.'/
    Data TX(  20) / &
      ' ' /
    Data TX(  21) / &
      'The "AUTO INPUT" button allows a sequence'/
    Data TX(  22) / &
      'of images to be input, displayed, and optionally'/
    Data TX(  23) / &
      'searched for peaks, and statistics output.'/
    Data TX(  24) / &
      'Similarly the "NEXT FILE" and "PREV FILE"'/
    Data TX(  25) / &
      'allow easy input of following or previous'/
    Data TX(  26) / &
      'files in a sequence. (All files are assumed'/
    Data TX(  27) / &
      'to be of the same format and sizes as the'/
    Data TX(  28) / &
      'a file input using the "INPUT" button.)'/
    Data TX(  29) / &
      ' ' /
    Data TX(  30) / &
      'Commands'/
    Data TX(  31) / &
      '----------------'/
    Data TX(  32) / &
      ' ' /
    Data TX(  33) / &
      'EXIT: Exit Interface and return to main'/
    Data TX(  34) / &
      'interfaces menu'/
    Data TX(  35) / &
      ' ' /
    Data TX(  36) / &
      '?: Brief description of commands'/
    Data TX(  37) / &
      ' ' /
    Data TX(  38) / &
      'HELP: This help text'/
    Data TX(  39) / &
      ' ' /
    Data TX(  40) / &
      'AUTO INPUT: Automatically input files from'/
    Data TX(  41) / &
      'a sequence with option of performing a peak'/
    Data TX(  42) / &
      'search on input, and of setting an alarm if'/
    Data TX(  43) / &
      'a new image is not available after a defined'/
    Data TX(  44) / &
      'interval.'/
    Data TX(  45) / &
      ' ' /
    Data TX(  46) / &
      'DISPLAY: Graphical display possibilities'/
    Data TX(  47) / &
      ' ' /
    Data TX(  48) / &
      'EXCHANGE: Swap current data with the "memory"'/
    Data TX(  49) / &
      ' ' /
    Data TX(  50) / &
      'INPUT: Input data from a file on disk. This'/
    Data TX(  51) / &
      'defines the file type, and may be the size'/
    Data TX(  52) / &
      'for further input using the "NEXT FILE" or'/
    Data TX(  53) / &
      '"PREV FILE" commands.'/
    Data TX(  54) / &
      ' ' /
    Data TX(  55) / &
      'FULL: View image of full data; set "Region'/
    Data TX(  56) / &
      'Of Interest" (ROI) to be the whole of the'/
    Data TX(  57) / &
      'defined data.'/
    Data TX(  58) / &
      ' ' /
    Data TX(  59) / &
      'OPTIONS: Display style control menu.'/
    Data TX(  60) / &
      ' ' /
    Data TX(  61) / &
      'MOVEMENT: Easily controlled movement around'/
    Data TX(  62) / &
      'an image.'/
    Data TX(  63) / &
      ' ' /
    Data TX(  64) / &
      'PRINT: Output current graphics to a'/
    Data TX(  65) / &
      'PostScript file.'/
    Data TX(  66) / &
      ' ' /
    Data TX(  67) / &
      'UN-ZOOM: Zoom out to see more of the data.'/
    Data TX(  68) / &
      '(Also changes the ROI.)'/
    Data TX(  69) / &
      ' ' /
    Data TX(  70) / &
      'ZOOM IN: View a smaller region in greater'/
    Data TX(  71) / &
      'detail. (Also changes the ROI.)'/
    Data TX(  72) / &
      ' ' /
    Data TX(  73) / &
      'Z-SCALING: Automatic or fixed user control of'/
    Data TX(  74) / &
      'intensity display range. It is important to'/
    Data TX(  75) / &
      'consider whether fixed range scaling, or'/
    Data TX(  76) / &
      'automatic re-scaling is more suitable when'/
    Data TX(  77) / &
      'displaying a sequence of images.'/
    Data TX(  78) / &
      ' ' /
    Data TX(  79) / &
      'NEXT FILE: Input "next" file in a file'/
    Data TX(  80) / &
      'sequence.'/
    Data TX(  81) / &
      ' ' /
    Data TX(  82) / &
      'PREV FILE: Input "previous" file in a file'/
    Data TX(  83) / &
      'sequence.'/
    Data TX(  84) / &
      ' ' /
    Data TX(  85) / &
      'SET GEOMETRY: Define diffraction geometry'/
    Data TX(  86) / &
      'i.e. beam centre, wavelength, sample to'/
    Data TX(  87) / &
      'detector distance. This must be defined if'/
    Data TX(  88) / &
      'the corresponding d-spacings of diffraction'/
    Data TX(  89) / &
      'peaks are to be calculated.'/
    Data TX(  90) / &
      ' ' /
    Data TX(  91) / &
      'PEAK SEARCH: Find, display, and calculate'/
    Data TX(  92) / &
      'statistics on diffraction peaks. A number'/
    Data TX(  93) / &
      'of control parameters allow the peak search'/
    Data TX(  94) / &
      'to be optimised to find the maximum number'/
    Data TX(  95) / &
      'of genuine peaks, whilst rejecting noise'/
    Data TX(  96) / &
      'and other defects. The found peaks and'/
    Data TX(  97) / &
      'esitmates of integrated intensities and'/
    Data TX(  98) / &
      'sigmas may optionally be saved in an'/
    Data TX(  99) / &
      'ASCII file.'/
    Data TX( 100) / &
      ' ' /
    Data TX( 101) / &
      'SPY-GLASS: Cursor controlled "real-time"'/
    Data TX( 102) / &
      'zoom window.'/
    Data TX( 103) / &
      ' ' /
    Data TX( 104) / &
      ' ' /
    Data TX( 105) / &
      'Further help is available within the FIT2D'/
    Data TX( 106) / &
      'Reference Manual. This manual is available'/
    Data TX( 107) / &
      'by the web from the FIT2D home page URL:'/
    Data TX( 108) / &
      ' ' /
    Data TX( 109) / &
      'http://www.esrf.fr/computing/scientific/FIT2D/'/
    Data TX( 110) / &
      ' ' /
    Data TX( 111) / &
      '------------------------'/
    Data TX( 112) / &
      'END OF HELP TEXT'/
    Data TX( 113) / &
      '------------------------'/
    Data TX( 114) / &
      ' ' /
    Data TX( 115) / &
      ' ' /
    Data TX( 116) / &
      ' ' /
    Data TX( 117) / &
      ' ' /
    Data TX( 118) / &
      ' ' /
    Data TX( 119) / &
      ' ' /
!--------1---------2---------3---------4---------5---------6---------7---------8
! Check status value
    If (status .Eq. St_good()) Then
       If (gui) Then
 
          Call LG_INQ_TEXTFONT (font_index, status)
 
          Call LG_TEXTFONT (2, status)
 
          Call GS_MESSAGE (Max_lines, Max_lines, TX, status)
 
          Call LG_TEXTFONT (font_index, status)
 
       Else
          Call IO_TEXT (Max_lines, TX, status)
       End If
    End If

    End Subroutine F2D_GUI_XTALHELP
!********1*********2*********3*********4*********5*********6*********7*********8
