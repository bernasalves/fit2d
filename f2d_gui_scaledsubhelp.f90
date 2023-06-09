!********1*********2*********3*********4*********5*********6*********7*********8

!               f2d_gui_scaledsubhelp.f90

! DO NOT ALTER THIS FILE

!  This file has been created automatically by "maketext"
!  from the ASCII text file:  f2d_gui_scaledsubhelp.text 
!  which should be edited and maketext should be re-run to 
!  create a new version of the subroutine.

!+ F2D_GUI_SCALEDSUBHELP interactive user help/message display facility
    Subroutine F2D_GUI_SCALEDSUBHELP (gui, status)

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
      '        -----------------------------------'/
    Data TX(   3) / &
      '        Interactive Scaled Subtraction Menu'/
    Data TX(   4) / &
      '        -----------------------------------'/
    Data TX(   5) / &
      ' ' /
    Data TX(   6) / &
      'The commands here allow to interactively'/
    Data TX(   7) / &
      'adjust a scale factor which is applied to'/
    Data TX(   8) / &
      'the "memory" array prior to it being subtracted'/
    Data TX(   9) / &
      'element by element from the main data array.'/
    Data TX(  10) / &
      ' ' /
    Data TX(  11) / &
      ' ' /
    Data TX(  12) / &
      '                                      COMMANDS'/
    Data TX(  13) / &
      '                                      ----------'/
    Data TX(  14) / &
      ' ' /
    Data TX(  15) / &
      '"O.K": Exit subtracting the scaled memory from'/
    Data TX(  16) / &
      'the current data array.'/
    Data TX(  17) / &
      ' ' /
    Data TX(  18) / &
      '"CANCEL": Quit menu without any change to the'/
    Data TX(  19) / &
      'data.'/
    Data TX(  20) / &
      ' ' /
    Data TX(  21) / &
      '"FACTOR": Enter a new scale factor value by'/
    Data TX(  22) / &
      'keyboard input.'/
    Data TX(  23) / &
      ' ' /
    Data TX(  24) / &
      '"Z-SCALING" starts a sub-menu which'/
    Data TX(  25) / &
      'allows the false colour/ grey scale'/
    Data TX(  26) / &
      'used to display image intensities to'/
    Data TX(  27) / &
      'be changed. Different automatic and'/
    Data TX(  28) / &
      'fixed scaling methods can be selected.'/
    Data TX(  29) / &
      ' ' /
    Data TX(  30) / &
      '"?": Brief explanation of available commands.'/
    Data TX(  31) / &
      ' ' /
    Data TX(  32) / &
      '"INCREASE 1%": Increase the value of the'/
    Data TX(  33) / &
      'scale factor by 1%'/
    Data TX(  34) / &
      ' ' /
    Data TX(  35) / &
      '"INCREASE 10%": Increase the value of the'/
    Data TX(  36) / &
      'scale factor by 10%'/
    Data TX(  37) / &
      ' ' /
    Data TX(  38) / &
      '"INCREASE 50%": Increase the value of the'/
    Data TX(  39) / &
      'scale factor by 50%'/
    Data TX(  40) / &
      ' ' /
    Data TX(  41) / &
      '"HELP": This help text.'/
    Data TX(  42) / &
      ' ' /
    Data TX(  43) / &
      '"DECREASE 1%": Decrease the value of the'/
    Data TX(  44) / &
      'scale factor by 1%. (In fact this is the'/
    Data TX(  45) / &
      'inverse of "INCREASE 1%")'/
    Data TX(  46) / &
      ' ' /
    Data TX(  47) / &
      '"DECREASE 9%": Decrease the value of the'/
    Data TX(  48) / &
      'scale factor by 9%. (In fact this is the'/
    Data TX(  49) / &
      'inverse of "INCREASE 10%")'/
    Data TX(  50) / &
      ' ' /
    Data TX(  51) / &
      '"DECREASE 33%": Decrease the value of the'/
    Data TX(  52) / &
      'scale factor by 33%. (In fact this is the'/
    Data TX(  53) / &
      'inverse of "INCREASE 50%")'/
    Data TX(  54) / &
      ' ' /
    Data TX(  55) / &
      ' ' /
    Data TX(  56) / &
      'Further help is available within the FIT2D'/
    Data TX(  57) / &
      'Reference Manual. This manual is available'/
    Data TX(  58) / &
      'by the web from the FIT2D home page URL:'/
    Data TX(  59) / &
      ' ' /
    Data TX(  60) / &
      'http://www.esrf.fr/computing/expg/'/
    Data TX(  61) / &
      'subgroups/data_analysis/FIT2D'/
    Data TX(  62) / &
      ' ' /
    Data TX(  63) / &
      '---------------'/
    Data TX(  64) / &
      'END OF HELP TEXT'/
    Data TX(  65) / &
      '---------------'/
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

    End Subroutine F2D_GUI_SCALEDSUBHELP
!********1*********2*********3*********4*********5*********6*********7*********8
