!********1*********2*********3*********4*********5*********6*********7*********8

!               f2d_1dmaskhelp.f90

! DO NOT ALTER THIS FILE

!  This file has been created automatically by "maketext"
!  from the ASCII text file:  f2d_1dmaskhelp.text 
!  which should be edited and maketext should be re-run to 
!  create a new version of the subroutine.

!+ F2D_1DMASKHELP interactive user help/message display facility
    Subroutine F2D_1DMASKHELP (gui, status)

! Type Definitions:
    Implicit None

! Usage:
!   Private

! Import:
    Logical gui ! .True., if GUI is to be used
! Status:
    Integer status ! Status return variable

! Local Constants:
    Integer, Parameter :: Max_chars =  50 ! Maximum characters per line 
    Integer, Parameter :: Max_lines  =   51 ! Number of lines in message
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
      '           --------------------'/
    Data TX(   3) / &
      '           1-D DATA MASKING'/
    Data TX(   4) / &
      '           Graphical Menu'/
    Data TX(   5) / &
      '           --------------------'/
    Data TX(   6) / &
      ' ' /
    Data TX(   7) / &
      'This menu allows you to "mask-off" data-points'/
    Data TX(   8) / &
      'so that they will not be used for model'/
    Data TX(   9) / &
      'fitting or similar operations. Data points'/
    Data TX(  10) / &
      'may be masked-off by using the "MASK REGION"'/
    Data TX(  11) / &
      'button, and specifying a rectangle by clicking'/
    Data TX(  12) / &
      'on two opposite corners. All data pooints'/
    Data TX(  13) / &
      'inside the rectangle will be masked-off.'/
    Data TX(  14) / &
      'Masked-off data points are displayed in the'/
    Data TX(  15) / &
      'maksing colour, which is red by default.'/
    Data TX(  16) / &
      'The "UN-MASK REGION" button allows the'/
    Data TX(  17) / &
      'reverse operation i.e. all data points'/
    Data TX(  18) / &
      'within the defined rectangle are set'/
    Data TX(  19) / &
      'un-masked.'/
    Data TX(  20) / &
      ' ' /
    Data TX(  21) / &
      '   Command Summary'/
    Data TX(  22) / &
      '   ---------------'/
    Data TX(  23) / &
      ' ' /
    Data TX(  24) / &
      'EXIT: Return to upper menu'/
    Data TX(  25) / &
      ' ' /
    Data TX(  26) / &
      'CLEAR MASK: Set all points in 1-D data-set'/
    Data TX(  27) / &
      '  un-masked'/
    Data TX(  28) / &
      ' ' /
    Data TX(  29) / &
      'ZOOM IN: Display sub-region in greater detail'/
    Data TX(  30) / &
      ' ' /
    Data TX(  31) / &
      '?: List of buttons choices and brief explanation'/
    Data TX(  32) / &
      '   of command'/
    Data TX(  33) / &
      ' ' /
    Data TX(  34) / &
      'UN-ZOOM: Show all the 1-D data-set'/
    Data TX(  35) / &
      ' ' /
    Data TX(  36) / &
      'MASK REGION: Define rectangle in which data points'/
    Data TX(  37) / &
      '  will be masked out'/
    Data TX(  38) / &
      ' ' /
    Data TX(  39) / &
      'HELP: This help text'/
    Data TX(  40) / &
      ' ' /
    Data TX(  41) / &
      'UN-MASK RECTANGLE: Define rectangle in which data'/
    Data TX(  42) / &
      '  points will un-masked'/
    Data TX(  43) / &
      ' ' /
    Data TX(  44) / &
      ' ' /
    Data TX(  45) / &
      '   ----------------'/
    Data TX(  46) / &
      '   END OF HELP TEXT'/
    Data TX(  47) / &
      '   ----------------'/
    Data TX(  48) / &
      ' ' /
    Data TX(  49) / &
      ' ' /
    Data TX(  50) / &
      ' ' /
    Data TX(  51) / &
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

    End Subroutine F2D_1DMASKHELP
!********1*********2*********3*********4*********5*********6*********7*********8
