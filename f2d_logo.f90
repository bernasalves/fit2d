!********1*********2*********3*********4*********5*********6*********7*********8

!               f2d_logo.f90

! DO NOT ALTER THIS FILE

!  This file has been created automatically by "maketext"
!  from the ASCII text file:  f2d_logo.text 
!  which should be edited and maketext should be re-run to 
!  create a new version of the subroutine.

!+ F2D_LOGO interactive user help/message display facility
    Subroutine F2D_LOGO (gui, status)

! Type Definitions:
    Implicit None

! Usage:
!   Private

! Import:
    Logical gui ! .True., if GUI is to be used
! Status:
    Integer status ! Status return variable

! Local Constants:
    Integer, Parameter :: Max_chars =  68 ! Maximum characters per line 
    Integer, Parameter :: Max_lines  =   16 ! Number of lines in message
! Local Variables:
     Integer font_index ! Saves current font index
! Local Arrays:
    Character*(Max_chars) TX(Max_lines) ! Text array
! External Functions:
    Integer, External :: St_good ! Returns good status value
! Local Data:
    Data TX(   1) / &
      '                FFFFFF           T         22                      D'/
    Data TX(   2) / &
      '             FFFF               TT        2222                    DD'/
    Data TX(   3) / &
      '          FFFF         I        TT       22  22                   DD'/
    Data TX(   4) / &
      '         FFF          II       TTT TTT  22   222                 DDD'/
    Data TX(   5) / &
      '         FFF     FF   I        TTTTT    2     222                DDD'/
    Data TX(   6) / &
      '        FFFF  FFFF   I      TTTTTT     2       222              DDD'/
    Data TX(   7) / &
      '       FFFFFFFFF    II   TTTTTTTT     2         222          DDDDDD'/
    Data TX(   8) / &
      '   FFFFFFFFF       II   TT    TTT              222         DDD  DDD'/
    Data TX(   9) / &
      '  FF    FFF       III         TTT              22         DD    DDD'/
    Data TX(  10) / &
      '        FFF       II         TTT              22         DD    DDD'/
    Data TX(  11) / &
      '       FFF       II          TTT             22         D     DDD'/
    Data TX(  12) / &
      '       FF        II           TT            22         D     DDD'/
    Data TX(  13) / &
      '      FF        II             TT          2222         DDDDDD'/
    Data TX(  14) / &
      '      F         I                T        22222222222    DDD'/
    Data TX(  15) / &
      ' ' /
    Data TX(  16) / &
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

    End Subroutine F2D_LOGO
!********1*********2*********3*********4*********5*********6*********7*********8
