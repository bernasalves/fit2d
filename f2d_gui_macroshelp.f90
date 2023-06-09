!********1*********2*********3*********4*********5*********6*********7*********8

!               f2d_gui_macroshelp.f90

! DO NOT ALTER THIS FILE

!  This file has been created automatically by "maketext"
!  from the ASCII text file:  f2d_gui_macroshelp.text 
!  which should be edited and maketext should be re-run to 
!  create a new version of the subroutine.

!+ F2D_GUI_MACROSHELP interactive user help/message display facility
    Subroutine F2D_GUI_MACROSHELP (gui, status)

! Type Definitions:
    Implicit None

! Usage:
!   Private

! Import:
    Logical gui ! .True., if GUI is to be used
! Status:
    Integer status ! Status return variable

! Local Constants:
    Integer, Parameter :: Max_chars =  47 ! Maximum characters per line 
    Integer, Parameter :: Max_lines  =   75 ! Number of lines in message
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
      '        ----------------------------'/
    Data TX(   3) / &
      '        Welcome to the FIT2D'/
    Data TX(   4) / &
      '        Macrosand Log File Menu'/
    Data TX(   5) / &
      '        ----------------------------'/
    Data TX(   6) / &
      ' ' /
    Data TX(   7) / &
      ' ' /
    Data TX(   8) / &
      'The commands here allow you to create a macro'/
    Data TX(   9) / &
      'file (CREATE MACRO) and (STOP MACRO) and to'/
    Data TX(  10) / &
      'run a macro on a sequence of files'/
    Data TX(  11) / &
      '(RUN SEQUENCE).'/
    Data TX(  12) / &
      ' ' /
    Data TX(  13) / &
      'The easist manner in which to create a simple'/
    Data TX(  14) / &
      'macro is:'/
    Data TX(  15) / &
      ' ' /
    Data TX(  16) / &
      '1. Use the "CREATE MACRO" button to open a file'/
    Data TX(  17) / &
      ' ' /
    Data TX(  18) / &
      '2. Complete the series of analysis operations'/
    Data TX(  19) / &
      '   on one file (INPUT, INTEGRATE, OUTPUT, etc.)'/
    Data TX(  20) / &
      '   When prompted for text or values always'/
    Data TX(  21) / &
      '   enter the value or command, unless you'/
    Data TX(  22) / &
      '   are absolutely sure that the default value'/
    Data TX(  23) / &
      '   is the one you want. (Defaults can change.)'/
    Data TX(  24) / &
      ' ' /
    Data TX(  25) / &
      '3. Use the "STOP MACRO" button to close the'/
    Data TX(  26) / &
      '   macro.'/
    Data TX(  27) / &
      ' ' /
    Data TX(  28) / &
      '4. Use your favourite editor to replace the'/
    Data TX(  29) / &
      '   name of the input file by #IN, and the'/
    Data TX(  30) / &
      '   name of any output file by #OUT. (Other'/
    Data TX(  31) / &
      '   more complicated changes are also'/
    Data TX(  32) / &
      '   possible.)'/
    Data TX(  33) / &
      ' ' /
    Data TX(  34) / &
      'The macro is now ready to be used.'/
    Data TX(  35) / &
      ' ' /
    Data TX(  36) / &
      ' ' /
    Data TX(  37) / &
      '                          COMMANDS'/
    Data TX(  38) / &
      '                          ----------------'/
    Data TX(  39) / &
      ' ' /
    Data TX(  40) / &
      '"EXIT": exit "MACROS / LOG FILE" interface and'/
    Data TX(  41) / &
      'return to top Interface/Mode menu.'/
    Data TX(  42) / &
      ' ' /
    Data TX(  43) / &
      '"?": Brief explanation of available commands.'/
    Data TX(  44) / &
      ' ' /
    Data TX(  45) / &
      '"HELP": This help text.'/
    Data TX(  46) / &
      ' ' /
    Data TX(  47) / &
      '"CREATE MACRO": Create a named file to'/
    Data TX(  48) / &
      'contain a recording of a series of'/
    Data TX(  49) / &
      'operations: a macro.'/
    Data TX(  50) / &
      ' ' /
    Data TX(  51) / &
      '"STOP MACRO": Stop recording the macro'/
    Data TX(  52) / &
      'and close the file.'/
    Data TX(  53) / &
      ' ' /
    Data TX(  54) / &
      '"RUN SEQUENCE": Run a macro repeatedly'/
    Data TX(  55) / &
      'in a series e.g. on a file sequence'/
    Data TX(  56) / &
      ' ' /
    Data TX(  57) / &
      '"OPEN LOG": Start saving a log of program'/
    Data TX(  58) / &
      'output and user input to a named file.'/
    Data TX(  59) / &
      ' ' /
    Data TX(  60) / &
      '"CLOSE LOG": Close a previously openned'/
    Data TX(  61) / &
      'log file.'/
    Data TX(  62) / &
      ' ' /
    Data TX(  63) / &
      ' ' /
    Data TX(  64) / &
      'Further help is available within the FIT2D'/
    Data TX(  65) / &
      'Reference Manual. This manual is available'/
    Data TX(  66) / &
      'by the web from the FIT2D home page URL:'/
    Data TX(  67) / &
      ' ' /
    Data TX(  68) / &
      'http://www.esrf.fr/computing/scientific/FIT2D/'/
    Data TX(  69) / &
      ' ' /
    Data TX(  70) / &
      '------------------------'/
    Data TX(  71) / &
      'END OF HELP TEXT'/
    Data TX(  72) / &
      '------------------------'/
    Data TX(  73) / &
      ' ' /
    Data TX(  74) / &
      ' ' /
    Data TX(  75) / &
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

    End Subroutine F2D_GUI_MACROSHELP
!********1*********2*********3*********4*********5*********6*********7*********8
