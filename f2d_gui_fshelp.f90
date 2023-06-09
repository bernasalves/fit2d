!********1*********2*********3*********4*********5*********6*********7*********8

!               f2d_gui_fshelp.f90

! DO NOT ALTER THIS FILE

!  This file has been created automatically by "maketext"
!  from the ASCII text file:  f2d_gui_fshelp.text 
!  which should be edited and maketext should be re-run to 
!  create a new version of the subroutine.

!+ F2D_GUI_FSHELP interactive user help/message display facility
    Subroutine F2D_GUI_FSHELP (gui, status)

! Type Definitions:
    Implicit None

! Usage:
!   Private

! Import:
    Logical gui ! .True., if GUI is to be used
! Status:
    Integer status ! Status return variable

! Local Constants:
    Integer, Parameter :: Max_chars =  52 ! Maximum characters per line 
    Integer, Parameter :: Max_lines  =   68 ! Number of lines in message
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
      '        ----------------------------------'/
    Data TX(   3) / &
      '        Welcome to the FIT2D File'/
    Data TX(   4) / &
      '        Series Processing Graphical Menu'/
    Data TX(   5) / &
      '        ----------------------------------'/
    Data TX(   6) / &
      ' ' /
    Data TX(   7) / &
      ' ' /
    Data TX(   8) / &
      'The commands here allow you to input and average'/
    Data TX(   9) / &
      'data from a series of files (AVERAGE), to create'/
    Data TX(  10) / &
      'a composite image from a series of files (COMPOSITE)'/
    Data TX(  11) / &
      'and to view the results (DISPLAY), or output them'/
    Data TX(  12) / &
      '(OUTPUT). Some of the buttons will start sub-menus.'/
    Data TX(  13) / &
      ' ' /
    Data TX(  14) / &
      '                         COMMANDS'/
    Data TX(  15) / &
      '                         ----------------'/
    Data TX(  16) / &
      ' ' /
    Data TX(  17) / &
      '"EXIT": exit "FILE SERIES" interface and'/
    Data TX(  18) / &
      'return to top Interface/Mode menu.'/
    Data TX(  19) / &
      ' ' /
    Data TX(  20) / &
      '"DISPLAY": starts a sub-menu which allows various'/
    Data TX(  21) / &
      'different formas of display of the image e.g.'/
    Data TX(  22) / &
      '3-D views, contour plot, as well as allowing'/
    Data TX(  23) / &
      'slices, and projections through the data to be'/
    Data TX(  24) / &
      'defined. Number fields may be extracted.'/
    Data TX(  25) / &
      ' ' /
    Data TX(  26) / &
      '"EXCHANGE": transfers an image from the "main'/
    Data TX(  27) / &
      'program array" to the "memory array", and the'/
    Data TX(  28) / &
      'inverse if the "memory array" already contains'/
    Data TX(  29) / &
      'data. This is useful to "charge" the "memory"'/
    Data TX(  30) / &
      'with data for binary operations.'/
    Data TX(  31) / &
      ' ' /
    Data TX(  32) / &
      '"?": Brief explanation of available commands.'/
    Data TX(  33) / &
      ' ' /
    Data TX(  34) / &
      '"HELP": This help text.'/
    Data TX(  35) / &
      ' ' /
    Data TX(  36) / &
      '"OPTIONS": starts a sub-menu which allows'/
    Data TX(  37) / &
      'various display options to be modified, and'/
    Data TX(  38) / &
      'control of the graphical output style.'/
    Data TX(  39) / &
      ' ' /
    Data TX(  40) / &
      '"OUTPUT": allows the current "region of'/
    Data TX(  41) / &
      'interest (roi)" to be saved in a file.'/
    Data TX(  42) / &
      'The user is given a choice of file formats'/
    Data TX(  43) / &
      'in which to save the data.'/
    Data TX(  44) / &
      ' ' /
    Data TX(  45) / &
      '"PRINT" may used to save the currently'/
    Data TX(  46) / &
      'displayed data image as a PostScript'/
    Data TX(  47) / &
      'file. You are prompted for the name of'/
    Data TX(  48) / &
      'the output file to create. After the'/
    Data TX(  49) / &
      'file is fully written, it may be sent'/
    Data TX(  50) / &
      'to a PostScript printer for printing.'/
    Data TX(  51) / &
      'Note: It can take some time to create'/
    Data TX(  52) / &
      'a file of a large 2-D data-set, and'/
    Data TX(  53) / &
      'often even longer for the file to be'/
    Data TX(  54) / &
      'printed.'/
    Data TX(  55) / &
      ' ' /
    Data TX(  56) / &
      ' ' /
    Data TX(  57) / &
      'Further help is available within the FIT2D'/
    Data TX(  58) / &
      'Reference Manual. This manual is available'/
    Data TX(  59) / &
      'by the web from the FIT2D home page URL:'/
    Data TX(  60) / &
      ' ' /
    Data TX(  61) / &
      'http://www.esrf.fr/computing/scientific/FIT2D/'/
    Data TX(  62) / &
      ' ' /
    Data TX(  63) / &
      '----------------------------'/
    Data TX(  64) / &
      'END OF HELP TEXT'/
    Data TX(  65) / &
      '----------------------------'/
    Data TX(  66) / &
      ' ' /
    Data TX(  67) / &
      ' ' /
    Data TX(  68) / &
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

    End Subroutine F2D_GUI_FSHELP
!********1*********2*********3*********4*********5*********6*********7*********8
