!********1*********2*********3*********4*********5*********6*********7*********8

!               f2d_gui_roihelp.f90

! DO NOT ALTER THIS FILE

!  This file has been created automatically by "maketext"
!  from the ASCII text file:  f2d_gui_roihelp.text 
!  which should be edited and maketext should be re-run to 
!  create a new version of the subroutine.

!+ F2D_GUI_ROIHELP interactive user help/message display facility
    Subroutine F2D_GUI_ROIHELP (gui, status)

! Type Definitions:
    Implicit None

! Usage:
!   Private

! Import:
    Logical gui ! .True., if GUI is to be used
! Status:
    Integer status ! Status return variable

! Local Constants:
    Integer, Parameter :: Max_chars =  41 ! Maximum characters per line 
    Integer, Parameter :: Max_lines  =   35 ! Number of lines in message
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
      '        -----------------------------'/
    Data TX(   3) / &
      '        Welcome to the FIT2D "Region Of'/
    Data TX(   4) / &
      '        Interest (ROI) Graphical Menu'/
    Data TX(   5) / &
      '        -----------------------------'/
    Data TX(   6) / &
      ' ' /
    Data TX(   7) / &
      ' ' /
    Data TX(   8) / &
      'The commands here allow you to select a'/
    Data TX(   9) / &
      'region of interest (ROI) from an image.'/
    Data TX(  10) / &
      'This can be done graphically or pixel'/
    Data TX(  11) / &
      'values can be entered using the'/
    Data TX(  12) / &
      '"KEYBOARD" button'/
    Data TX(  13) / &
      ' ' /
    Data TX(  14) / &
      '             COMMANDS'/
    Data TX(  15) / &
      '             ----------'/
    Data TX(  16) / &
      ' ' /
    Data TX(  17) / &
      '"EXIT": exit the menu once the required'/
    Data TX(  18) / &
      'ROI has been selected.'/
    Data TX(  19) / &
      ' ' /
    Data TX(  20) / &
      '"?": Brief explanation of available'/
    Data TX(  21) / &
      'commands.'/
    Data TX(  22) / &
      ' ' /
    Data TX(  23) / &
      '"HELP": This help text.'/
    Data TX(  24) / &
      ' ' /
    Data TX(  25) / &
      '"UN-ZOOM": Increase size of current ROI.'/
    Data TX(  26) / &
      ' ' /
    Data TX(  27) / &
      '"ZOOM": Select a smaller ROI. Defined by'/
    Data TX(  28) / &
      'two graphically coordiantes.'/
    Data TX(  29) / &
      ' ' /
    Data TX(  30) / &
      '"FULL": Set the ROI to be all the defined'/
    Data TX(  31) / &
      'data.'/
    Data TX(  32) / &
      ' ' /
    Data TX(  33) / &
      '---------------'/
    Data TX(  34) / &
      'END OF HELP TEXT'/
    Data TX(  35) / &
      '---------------'/
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

    End Subroutine F2D_GUI_ROIHELP
!********1*********2*********3*********4*********5*********6*********7*********8
