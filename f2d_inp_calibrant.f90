!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_inp_calibrant.f90 *
!  *                       *
!  *************************
 
!+ F2D_INP_CALIBRANT -  INPut CALIBRANT choice
     Subroutine F2D_INP_CALIBRANT (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, mask_data, XAXIS, YAXIS, DATA, MASK, title, xlabel, &
       ylabel, zlabel, experiment, calibrant_sample, status)
!  Description:
!    Inputs choice of calibrant sample
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Nov-2014: V0.6 Changes to "F2D_CLICK" (Hammersley)
!    20-Mar-2013: V0.5 Add Silver Behenate to list of calibrants (Hammersley)
!    17-Mar-2006: V0.4 Support for arbitrary aspect ratio windows (Hammersley)
!    13-Mar-2006: V0.3 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.2 Alter menu lay-out for landscape windows (Hammersley)
!    12-Dec-2003: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: xendelm ! The X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! The Y-pixel number for the end of the ROI
     Logical, Intent(IN) :: mask_data ! .True., if data is to be masked
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Axis data for the Y-axis
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
!  Export:
     Character(Len = *), Intent(OUT) :: calibrant_sample ! Sample used for
!      calibration
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
     Integer, Parameter :: Max_help = 28 ! Dimension size of "HELP" text
     Integer, Parameter :: Max_menu = 11 ! Dimension size of menu
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection
!      prompt text
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Integer :: font_index ! Index of font to save
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Logical :: continue ! .True., until user wants to exit
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 60) :: HELP(Max_help) ! Help text
     Character(Len = 20), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command
!      explanation
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 11) / 'CANCEL', 'ALUMINA (Al2O3)', &
       'CERIUM DIOXIDE', '?', 'LANTHANUM HEXABORIDE', 'PARAFFIN WAX', 'HELP', &
       'SODIUM CHLORIDE', 'SILICON', 'USER DEFINED', 'SILVER BEHENATE' /
     Data (MENUTXT(item), item = 1, 11) / &
       'CANCEL: Cancel "CALIBRANT" command and return to menu', &
       'ALUMINA (Al2O3): Al_2O_3 powder sample', &
       'CERIUM DIOXIDE: Ceria powder sample e.g. NIST standard', &
       '?: This list of menu choice explanations', &
       'LANTHANUM HEXABORIDE: LaB_6 powder sample e.g. NIST standard', &
       'SODIUM CHLORIDE: NaCl Common salt (beware of moisture)', &
       'HELP: More detailed help text on menu choices', &
       'PARAFFIN WAX: MM Xtallographers "standard" calibration wax', &
       'SILICON: Silicon powder sample e.g. NIST standard', &
       'USER DEFINED: User input of D-spacings for arbitrary sample', &
       'SILVER BEHENATE: [CH_3(CH_2)_20 COOAg] powder e.g. NIST standard' /
     Data (HELP(item), item = 1, 10) / &
       'Select the type of calibrant sample you have', &
       'used. The choice includes the commonly used', &
       'samples from the American National Institute of', &
       'Standards and Technology.', &
       ' ', &
       ' "PARAFFIN WAX" is included since many xtall-', &
       'ographers use this for distance calibration.', &
       'However don''t expect high accuracy, and if only', &
       'low angle data is available don''t try to', &
       'refine both wavelength and distance together.' /
     Data (HELP(item), item = 11, 20) / &
       ' ', &
       'The "USER DEFINED" choice inputs an ASCII file', &
       'with the D-spacings to be used for calibration.', &
       'With one value per line, in units of Angstroms.', &
       ' e.g. The first 6 D-spacings for silicon would be defined:', &
       ' ', & 
       '3.1355', &
       '1.9201', &
       '1.6375', &
       '1.3577' /
       Data (HELP(item), item = 21, 28) / &
       '1.2459', &
       '1.1086', &
       ' ', &
       'The "USER DEFINED" choice allows you to also use', &
       'different D-spacing for standard samples e.g. for', &
       'use at non-standard temperatures, or to delibrately', &
       'miss certain lines owing to contamination or other', &
       'effects, or to treat mixed samples.' /
!  Local Data:
 
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_CALIBRANT ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Lt. 1 .Or. xstrelm .Gt. xendelm .Or. xendelm .Gt. &
       xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Lt. 1 .Or. ystrelm .Gt. yendelm .Or. yendelm .Gt. &
       ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_INP_CALIBRANT ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Inquire window format
        Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!     Loop inputting menu commands until requested to stop
        input_mode = 2
        continue = .True.
        update_image = .False.
        update_menu = .True.
        Do While (continue)
 
!        Draw or re-draw display
           If (update_image) Then
 
!           Redraw image
              Call GS_MPLOT (mask_data, xmaxdat, ymaxdat, DATA, MASK, XAXIS, &
                YAXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                ylabel, zlabel, status)
 
           End If
 
           If (update_menu) Then
 
!           Set menu layout style
              num_menu = Max_menu
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 17, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 3, 20, 20, status)
              End If
 
!           Re-draw menu
              Call GS_FMENU (1, 1, 'CALIBRATION CHOICE', Max_menu, num_menu, &
                MENU, status)
 
           End If
 
!        By default don't update graphics
           update_image = .False.
           update_menu = .False.
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, input_mode, &
             input_type, command, x_coordinate, y_coordinate, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
           If (input_type .Eq. Gs_resize) Then
 
              update_image = .True.
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_motion) Then
 
!           Pointer motion, don't update
              update_image = .False.
              update_menu = .False.
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Output the coordinate position and intensity
              Call F2D_CLICK (1, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, XAXIS, YAXIS, DATA, title, xlabel, ylabel, zlabel, &
                x_coordinate, y_coordinate, experiment, &
                update_image, update_menu, status)
 
           Else
 
!           Menu choice input
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''command = '', a)') command
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!           Carry out menu choices
              If (command .Eq. 'null') Then
 
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the PD menu
                 Call LG_INQ_TEXTFONT (font_index, status)
                 Call LG_TEXTFONT (2, status)
                 Call GS_MESSAGE (Max_help, Max_help, HELP, status)
                 Call LG_TEXTFONT (font_index, status)
                 update_image = .True.
                 update_menu = .True.
 
              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
                 update_image = .True.
                 update_menu = .True.
 
              Else If (command .Eq. 'CANCEL' .Or. command .Eq. &
                'ALUMINA (Al2O3)' .Or. command .Eq. 'CERIUM DIOXIDE' .Or. &
                command .Eq. 'LANTHANUM HEXABORIDE' .Or. command .Eq. &
                'SODIUM CHLORIDE' .Or. command .Eq. 'PARAFFIN WAX' .Or. &
                command .Eq. 'SILICON' .Or. command .Eq. 'USER DEFINED' .Or. &
                command .Eq. 'SILVER BEHENATE') Then
 
                 calibrant_sample = command
                 continue = .False.
 
              End If
 
           End If
 
        End Do
 
     End If
 
     End Subroutine F2D_INP_CALIBRANT
!********1*********2*********3*********4*********5*********6*********7*********8
