!********1*********2*********3*********4*********5*********6*********7*********8

!               f2d_guihelp.f90

! DO NOT ALTER THIS FILE

!  This file has been created automatically by "maketext"
!  from the ASCII text file:  f2d_guihelp.text 
!  which should be edited and maketext should be re-run to 
!  create a new version of the subroutine.

!+ F2D_GUIHELP interactive user help/message display facility
    Subroutine F2D_GUIHELP (gui, status)

! Type Definitions:
    Implicit None

! Usage:
!   Private

! Import:
    Logical gui ! .True., if GUI is to be used
! Status:
    Integer status ! Status return variable

! Local Constants:
    Integer, Parameter :: Max_chars =  46 ! Maximum characters per line 
    Integer, Parameter :: Max_lines  =  105 ! Number of lines in message
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
      '-----------------------------'/
    Data TX(   3) / &
      'Welcome to the FIT2D Main graphical menu'/
    Data TX(   4) / &
      '-----------------------------'/
    Data TX(   5) / &
      ' ' /
    Data TX(   6) / &
      'The commands here allow you to select from'/
    Data TX(   7) / &
      'a choice of interfaces to different areas'/
    Data TX(   8) / &
      'of mathematical or scientific speciality.'/
    Data TX(   9) / &
      'Many of the same possibilities are available'/
    Data TX(  10) / &
      'in different interfaces, but they are grouped'/
    Data TX(  11) / &
      'differently to try to make the interfaces as'/
    Data TX(  12) / &
      'simple and as obvious to use as possible.'/
    Data TX(  13) / &
      ' ' /
    Data TX(  14) / &
      'It should be noted that the FIT2D "Graphical'/
    Data TX(  15) / &
      'User Interface" is presently under development'/
    Data TX(  16) / &
      'and more commands and file formats for input'/
    Data TX(  17) / &
      'and output are available in the "KEYBOARD"'/
    Data TX(  18) / &
      'interface. This interface, is exactly the same'/
    Data TX(  19) / &
      'as the older FIT2D main keyboard menu.'/
    Data TX(  20) / &
      ' ' /
    Data TX(  21) / &
      '                               INTERFACES'/
    Data TX(  22) / &
      '                               ----------'/
    Data TX(  23) / &
      ' ' /
    Data TX(  24) / &
      '"?": The question mark in this menu, and other'/
    Data TX(  25) / &
      'menus gives a brief description of each of'/
    Data TX(  26) / &
      'the available choices.'/
    Data TX(  27) / &
      ' ' /
    Data TX(  28) / &
      '"HELP": The "HELP" button enters the "context"'/
    Data TX(  29) / &
      'related help associated with the menu. In'/
    Data TX(  30) / &
      'this case there is information describing'/
    Data TX(  31) / &
      'the different available interfaces. Clicking'/
    Data TX(  32) / &
      'on "HELP" enters a "pager" which allows the'/
    Data TX(  33) / &
      'user to scroll down and up the help'/
    Data TX(  34) / &
      'information.'/
    Data TX(  35) / &
      ' ' /
    Data TX(  36) / &
      '"FILE SERIES": Is an interface for performing'/
    Data TX(  37) / &
      'operations on a whole series of files, or'/
    Data TX(  38) / &
      'images as single commands. This is a newly'/
    Data TX(  39) / &
      'available interface and much functionality'/
    Data TX(  40) / &
      'is yet to be added.'/
    Data TX(  41) / &
      ' ' /
    Data TX(  42) / &
      '"IMAGE PROCESSING (GENERAL)": Is an interface'/
    Data TX(  43) / &
      'for general purpose image processing and'/
    Data TX(  44) / &
      'display operations on data input from a file.'/
    Data TX(  45) / &
      ' ' /
    Data TX(  46) / &
      '"KEYBOARD INTERFACE": This enters the'/
    Data TX(  47) / &
      '"keyboard" command-line interface to FIT2D.'/
    Data TX(  48) / &
      'Certain functionality is presently only'/
    Data TX(  49) / &
      'available from this interface.'/
    Data TX(  50) / &
      ' ' /
    Data TX(  51) / &
      '"MACROS / LOG FILE": Commands to create and'/
    Data TX(  52) / &
      'run macros, and to open and close log records'/
    Data TX(  53) / &
      'of the anaylsis session.'/
    Data TX(  54) / &
      ' ' /
    Data TX(  55) / &
      '"MFIT (MULTIPLE 1-D FITTING)": This interface'/
    Data TX(  56) / &
      'allows 1-D "peak" functions such as Gaussian,'/
    Data TX(  57) / &
      'Lorentzian, and Voigt functions to be fitted'/
    Data TX(  58) / &
      'to data along with other functions such as a'/
    Data TX(  59) / &
      'polynomial "background" function, and'/
    Data TX(  60) / &
      'exponential decay or trigonometric functions.'/
    Data TX(  61) / &
      'The fit model may be fitted repeatedly to a'/
    Data TX(  62) / &
      'whole series of 1-D datasets. (This provides'/
    Data TX(  63) / &
      'the same functionality as was previously'/
    Data TX(  64) / &
      'available through the program MFIT.)'/
    Data TX(  65) / &
      ' ' /
    Data TX(  66) / &
      '"ON-LINE CRYSTALLOGRAPHY": This interface'/
    Data TX(  67) / &
      'allows on-line display and examination of'/
    Data TX(  68) / &
      'crystallographic data, with the option of'/
    Data TX(  69) / &
      'automatic updating of new image data as'/
    Data TX(  70) / &
      'they are obtained. Simple statistics to'/
    Data TX(  71) / &
      'evaluate data quality are produced.'/
    Data TX(  72) / &
      ' ' /
    Data TX(  73) / &
      '"POWDER DIFFRACTION (2-D)": This interface'/
    Data TX(  74) / &
      'specialises in the integration of Debye-'/
    Data TX(  75) / &
      'Scherrer rings from 2-D detectors, to 1-D'/
    Data TX(  76) / &
      '"2-theta" scans, and to other scans. This'/
    Data TX(  77) / &
      'is useful for standard powder refinement,'/
    Data TX(  78) / &
      'but is also useful for texture analysis and'/
    Data TX(  79) / &
      'other types of integrations and "scans", and'/
    Data TX(  80) / &
      'is used for small angle scattering data.'/
    Data TX(  81) / &
      ' ' /
    Data TX(  82) / &
      '"RECIPROCAL SPACE MAPPING": This inferface'/
    Data TX(  83) / &
      'allows a series of diffraction images to'/
    Data TX(  84) / &
      'be input and transformed to build up a'/
    Data TX(  85) / &
      'reciprocal space lattice.'/
    Data TX(  86) / &
      ' ' /
    Data TX(  87) / &
      '"TEST": The interface is for testing'/
    Data TX(  88) / &
      'FIT2D and generates artificial data.'/
    Data TX(  89) / &
      ' ' /
    Data TX(  90) / &
      '"EXIT FIT2D": Allows FIT2D to be exited,'/
    Data TX(  91) / &
      'after confirmation.'/
    Data TX(  92) / &
      ' ' /
    Data TX(  93) / &
      '---------------'/
    Data TX(  94) / &
      'END OF HELP TEXT'/
    Data TX(  95) / &
      '---------------'/
    Data TX(  96) / &
      ' ' /
    Data TX(  97) / &
      ' ' /
    Data TX(  98) / &
      ' ' /
    Data TX(  99) / &
      ' ' /
    Data TX( 100) / &
      ' ' /
    Data TX( 101) / &
      ' ' /
    Data TX( 102) / &
      ' ' /
    Data TX( 103) / &
      ' ' /
    Data TX( 104) / &
      ' ' /
    Data TX( 105) / &
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

    End Subroutine F2D_GUIHELP
!********1*********2*********3*********4*********5*********6*********7*********8
