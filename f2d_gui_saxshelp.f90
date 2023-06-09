!********1*********2*********3*********4*********5*********6*********7*********8

!               f2d_gui_saxshelp.f90

! DO NOT ALTER THIS FILE

!  This file has been created automatically by "maketext"
!  from the ASCII text file:  f2d_gui_saxshelp.text 
!  which should be edited and maketext should be re-run to 
!  create a new version of the subroutine.

!+ F2D_GUI_SAXSHELP interactive user help/message display facility
    Subroutine F2D_GUI_SAXSHELP (gui, status)

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
    Integer, Parameter :: Max_lines  =  114 ! Number of lines in message
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
      '      ---------------------------------------'/
    Data TX(   3) / &
      '      Welcome to the FIT2D SAXS and'/
    Data TX(   4) / &
      '      GISAXS Graphical Menu'/
    Data TX(   5) / &
      '      ---------------------------------------'/
    Data TX(   6) / &
      ' ' /
    Data TX(   7) / &
      'The commands here allow you to input data from'/
    Data TX(   8) / &
      'files  ("INPUT"),  to  integrate data to a'/
    Data TX(   9) / &
      'choice of scans,  with  large  flexibility in'/
    Data TX(  10) / &
      'choosing different 2-theta and azimuth ranges.'/
    Data TX(  11) / &
      ' ' /
    Data TX(  12) / &
      'Generally data analysis will start by using the'/
    Data TX(  13) / &
      '"INPUT" command to input data from file.'/
    Data TX(  14) / &
      '(Most of the options will not work until data'/
    Data TX(  15) / &
      'has been input or otherwise created.)'/
    Data TX(  16) / &
      ' ' /
    Data TX(  17) / &
      'Occasionally the "IMAGE PROCESSING'/
    Data TX(  18) / &
      '(GENERAL)" interface will be useful to'/
    Data TX(  19) / &
      'allow such operations as subtracting one'/
    Data TX(  20) / &
      'image from another prior to further analysis'/
    Data TX(  21) / &
      'in this menu.'/
    Data TX(  22) / &
      ' ' /
    Data TX(  23) / &
      'To help see and interact with the data, the'/
    Data TX(  24) / &
      'button "Z-SCALING" allows the intensity'/
    Data TX(  25) / &
      'limits of displayed data to be changed. A'/
    Data TX(  26) / &
      'number of different fixed or automatically'/
    Data TX(  27) / &
      're-scaling methods are available and a'/
    Data TX(  28) / &
      'choice of linear or logarithm intensity'/
    Data TX(  29) / &
      'representation is available.'/
    Data TX(  30) / &
      ' ' /
    Data TX(  31) / &
      'Data from spatially distorted detector systems'/
    Data TX(  32) / &
      'may be corrected using the "CORRECTION"'/
    Data TX(  33) / &
      'button and prior to integration unwanted areas'/
    Data TX(  34) / &
      'of the image may be "masked-out" using the'/
    Data TX(  35) / &
      'mask menu which is selected through the'/
    Data TX(  36) / &
      '"MASK" button.'/
    Data TX(  37) / &
      ' ' /
    Data TX(  38) / &
      'The "BEAM CENTRE" command allows a choice of'/
    Data TX(  39) / &
      'methods for defining the beam centre. An'/
    Data TX(  40) / &
      'alternative might be to use a powder sample'/
    Data TX(  41) / &
      'and the "POWDER DIFFRACTION" interface commands'/
    Data TX(  42) / &
      '"TILT" or "CALIBRANT" to refine the beam centre'/
    Data TX(  43) / &
      'and optionally the detector non-orthogonality'/
    Data TX(  44) / &
      'to the direct beam.'/
    Data TX(  45) / &
      ' ' /
    Data TX(  46) / &
      'Three forms of integration are available:  the'/
    Data TX(  47) / &
      '"INTEGRATE" button integrates all of the un-'/
    Data TX(  48) / &
      'masked data to a choice of simple 1-D scans.'/
    Data TX(  49) / &
      'Alternatively, arbitrary 2-theta and azimuthal'/
    Data TX(  50) / &
      'regions may be integrated to a defined number'/
    Data TX(  51) / &
      'of "2-theta" scans using the "CAKE" menu.'/
    Data TX(  52) / &
      'This is an interactive menu allowing easy'/
    Data TX(  53) / &
      'control of start and end azimuthal and radial'/
    Data TX(  54) / &
      'regions. This can be useful for samples with'/
    Data TX(  55) / &
      'order/disorder and other non-symmetric'/
    Data TX(  56) / &
      'diffraction. The "PROJECTION" command allows'/
    Data TX(  57) / &
      'arbitrary rectangular regions to be defined'/
    Data TX(  58) / &
      'and integrated to a choice of 1-D scans.'/
    Data TX(  59) / &
      ' ' /
    Data TX(  60) / &
      'Having integrated 2-D data the resulting scan'/
    Data TX(  61) / &
      'or  scans  can  be  saved to output files in'/
    Data TX(  62) / &
      'a variety of different file formats using the'/
    Data TX(  63) / &
      '"OUTPUT"  button.'/
    Data TX(  64) / &
      ' ' /
    Data TX(  65) / &
      'Once the original data has been transformed to'/
    Data TX(  66) / &
      'a 1-D or series of 1-D 2-theta or other scans'/
    Data TX(  67) / &
      'it replaces the original data in the main'/
    Data TX(  68) / &
      'program array (i.e. the data that is displayed).'/
    Data TX(  69) / &
      'The original data is still stored within an'/
    Data TX(  70) / &
      'internal "memory". To recover the original'/
    Data TX(  71) / &
      'data the "EXCHANGE" command can be used to'/
    Data TX(  72) / &
      'swap the two data-sets. The previously'/
    Data TX(  73) / &
      'displayed data, e.g. a Q-scan, will'/
    Data TX(  74) / &
      'now be in the "memory". Using "EXCHANGE"'/
    Data TX(  75) / &
      'twice is an "identity operation".'/
    Data TX(  76) / &
      ' ' /
    Data TX(  77) / &
      '"ZOOM-IN" allows a sub-region of the data'/
    Data TX(  78) / &
      'to be selected and displayed. It should be'/
    Data TX(  79) / &
      'noted that ALL OPERATIONS INCLUDING'/
    Data TX(  80) / &
      '"OUTPUT" ONLY WORK ON THE CURRENT'/
    Data TX(  81) / &
      'SELECTED REGION. Care needs to be taken'/
    Data TX(  82) / &
      'when using "ZOOM-IN" that subsequent'/
    Data TX(  83) / &
      'operations are intended for only the'/
    Data TX(  84) / &
      'current sub-region. The full available'/
    Data TX(  85) / &
      'data region may be selected by using'/
    Data TX(  86) / &
      'the "FULL" button.'/
    Data TX(  87) / &
      ' ' /
    Data TX(  88) / &
      '"PRINT" may used to save the currently'/
    Data TX(  89) / &
      'displayed data image as a PostScript'/
    Data TX(  90) / &
      'file. You are prompted for the name of'/
    Data TX(  91) / &
      'the output file to create. After the'/
    Data TX(  92) / &
      'file is fully written, it may be sent'/
    Data TX(  93) / &
      'to a PostScript printer for printing.'/
    Data TX(  94) / &
      'Note: It can take some time to create'/
    Data TX(  95) / &
      'a file of a large 2-D data-set, and'/
    Data TX(  96) / &
      'often even longer for the file to be'/
    Data TX(  97) / &
      'printed.'/
    Data TX(  98) / &
      ' ' /
    Data TX(  99) / &
      ' ' /
    Data TX( 100) / &
      'Further help is available within the FIT2D'/
    Data TX( 101) / &
      'Reference Manual. This manual is available'/
    Data TX( 102) / &
      'by the web from the FIT2D home page URL:'/
    Data TX( 103) / &
      ' ' /
    Data TX( 104) / &
      'http://www.esrf.fr/computing/scientific/FIT2D/'/
    Data TX( 105) / &
      ' ' /
    Data TX( 106) / &
      '-----------------------------'/
    Data TX( 107) / &
      'END OF HELP TEXT'/
    Data TX( 108) / &
      '-----------------------------'/
    Data TX( 109) / &
      ' ' /
    Data TX( 110) / &
      ' ' /
    Data TX( 111) / &
      ' ' /
    Data TX( 112) / &
      ' ' /
    Data TX( 113) / &
      ' ' /
    Data TX( 114) / &
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

    End Subroutine F2D_GUI_SAXSHELP
!********1*********2*********3*********4*********5*********6*********7*********8
