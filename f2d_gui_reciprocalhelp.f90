!********1*********2*********3*********4*********5*********6*********7*********8

!               f2d_gui_reciprocalhelp.f90

! DO NOT ALTER THIS FILE

!  This file has been created automatically by "maketext"
!  from the ASCII text file:  f2d_gui_reciprocalhelp.text 
!  which should be edited and maketext should be re-run to 
!  create a new version of the subroutine.

!+ F2D_GUI_RECIPROCALHELP interactive user help/message display facility
    Subroutine F2D_GUI_RECIPROCALHELP (gui, status)

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
    Integer, Parameter :: Max_lines  =   23 ! Number of lines in message
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
      '        ----------------------------------------'/
    Data TX(   3) / &
      '        Welcome to the FIT2D Reciprocal'/
    Data TX(   4) / &
      '        Space Mapping Graphical Menu'/
    Data TX(   5) / &
      '        ----------------------------------------'/
    Data TX(   6) / &
      ' ' /
    Data TX(   7) / &
      ' ' /
    Data TX(   8) / &
      '!!!!!!!!!!!! NOTE: THIS INTERFACE AND HELP TEXT'/
    Data TX(   9) / &
      '!!!!!!!!!!!!       IS UNDER DEVELOPMENT AND'/
    Data TX(  10) / &
      '                   CHANGES ARE VERY LIKELY'/
    Data TX(  11) / &
      ' ' /
    Data TX(  12) / &
      'Further help is available within the FIT2D'/
    Data TX(  13) / &
      'Reference Manual. This manual is available'/
    Data TX(  14) / &
      'by the web from the FIT2D home page URL:'/
    Data TX(  15) / &
      ' ' /
    Data TX(  16) / &
      'http://www.esrf.fr/computing/scientific/FIT2D/'/
    Data TX(  17) / &
      ' ' /
    Data TX(  18) / &
      '---------------------------'/
    Data TX(  19) / &
      'END OF HELP TEXT'/
    Data TX(  20) / &
      '---------------------------'/
    Data TX(  21) / &
      ' ' /
    Data TX(  22) / &
      ' ' /
    Data TX(  23) / &
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

    End Subroutine F2D_GUI_RECIPROCALHELP
!********1*********2*********3*********4*********5*********6*********7*********8
