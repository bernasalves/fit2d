!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_colours.f90 *
!  *                 *
!  *******************
 
!+ F2D_COLOURS - FIT 2-D COLOURS for image display
     Subroutine F2D_COLOURS (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
       variances_exist, xstrelm, ystrelm, xendelm, yendelm, status)
!  Description:
!    Graphical menu choice of colour table for image display
!  Keywords:
!    Colour~Table.Choice, Choose.Colour~Table, LUT.Choice
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    22-Mar-2006: V0.16 Use "LUT_STYLE" structure (Hammersley)
!    17-Mar-2006: V0.15 Support for arbitrary aspect ratio windows (Hammersley)
!    25-Feb-2004: V0.14 Alter menu lay-out for landscape windows (Hammersley)
!    02-Jun-2003: V0.13 Tidy up code (Hammersley)
!    23-Feb-1999: V0.12 All data-base saving and recovering routines
!      renamed (Hammersley)
!    15-Dec-1998: V0.11 Change to use IO internal database routines (Hammersley)
!    25-Nov-1998: V0.10 Add 'PURPLE-WHITE' colour table (Hammersley)
!    03-Mar-1998: V0.9 Re-draw image, if colour mapping is static (Hammersley)
!    19-Dec-1997: V0.8 Add "TRIAL" colour table choice. Restructure code 
!      (Hammersley)
!    10-Apr-1996: V0.7 Change menu layout to increase size of text and make 
!      option order more standard (Hammersley)
!    19-Mar-1996: V0.6 Allow output and control of 1-D X/Y graphs (Hammersley)
!    29-Feb-1996: V0.5 Save colour table choice in internal data-store 
!      (Hammersley)
!    21-Jan-1996: V0.4 Inquire current prompt region (Hammersley)
!    20-Jun-1995: V0.3 Change to use GS graphics library (Hammersley)
!    22-Jun-1994: V0.2 Add choice of upside down geographical colour table 
!      (Hammersley)
!    20-Sep-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements in Y-direction
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! Variance values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Logical, Intent(IN) :: variances_exist ! .True., if the variances exist
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.15' ! Version number
     Integer, Parameter :: Max_menu = 14 ! Dimension size of menu
!  Local Variables:
     Character(Len = 20) :: command ! User entered command
     Integer :: bit_planes ! Number of bit planes in colour map
     Integer :: colour_mapping ! Type of colour mapping:
!      1 = 'PseudoColor': Variable colour map
!      2 = 'TrueColor': Fixed colour map
     Integer :: db_stat ! Data-store status return
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: len_string ! Defined length of a string
     Integer lut_choice ! Number of chosen LUT
     Integer :: num_menu ! Number of choices in menu
     Logical :: continue ! .True., until user wants to exit
     Logical :: static_colours ! .True., if the display colour map is static
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 20), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanations
!  Local Data Structures:
     Type(LUT_STYLE) :: STYLE ! LUT Style (see "gs.inc")
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 14) / 'EXIT', '?', 'HELP', 'COLOUR WHEEL', &
       'GEOGRAPHICAL', 'GREY-SCALE', 'INVERSE GREY-SCALE', 'ORIGINAL', &
       'PSYCHOLOGIAL', 'REPEATING', 'TEMPERATURE', 'TRIAL', 'UPSIDE DOWN', &
       'BLACK-PURPLE-WHITE' /
     Data (MENUTXT(item), item = 1, 14) / 'EXIT: Return to higher menu level', &
       '?: List of commands with brief description', &
       'HELP: Explanatory help text for the menu', &
       'COLOUR WHEEL: magenta-red-yellow-green-cyan-blue', &
       'GEOGRAPHICAL: black-blue-green-grey-yellow-brown-magenta-white', &
       'GREY-SCALE: black-white', 'INVERSE GREY-SCALE: white-black', &
       'ORIGINAL: black-magenta-red-yellow-green-cyan-blue-white', &
       'PSYCHOLOGIAL: "cold" colours = low intensity', &
       'REPEATING: as colour wheel, but goes black between colours', &
       'TEMPERATURE: black-red-yellow-white', &
       'TRIAL: Experimental for developing new colour tables', &
       'UPSIDE DOWN: white-magenta-brown-yellow-grey-green-blue-black', &
       'BLACK-PURPLE-WHITE: Black-Purple-Magenta-Red-Orange-Yellow-White' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_COLOURS ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm .Gt. xmaxdat .Or. xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Gt. ymaxdat .Or. yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_COLOURS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Inquire current lut parameters
        Call GS_INQ_LUTSTYLE (STYLE, status)
 
!     Inquire colour mapping method
        Call LG_INQ_COLOURMAPPING (colour_mapping, bit_planes, status)
 
        static_colours = colour_mapping .Ne. 1
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputing menu commands until requested to stop
 
 
!     By default update menu
        update_image = .False.
        update_menu = .True.
 
        continue = .True.
        Do While (continue)
 
           If (update_image) Then
 
!           Redraw image
              Call GS_PLOT (xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, xstrelm, &
                ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
                status)
 
              update_image = .False.
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout style
              num_menu = Max_menu
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 15, 20, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_vertical, 3, 15, 20, status)
              End If
 
!           Draw menu
              Call GS_FMENU (1, 1, 'COLOUR TABLES', Max_menu, num_menu, MENU, &
                status)
 
              update_menu = .False.
           End If
 
!        Get user to select between the available menu options
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 1, input_type, &
             command, x_coordinate, y_coordinate, status)
 
           If (input_type .Eq. Gs_resize) Then
 
              update_image = .True.
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_choice) Then
 
!           Carry out menu choices
              If (command .Eq. 'EXIT') Then
 
                 continue = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display help message
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
                 update_image = .True.
                 update_menu = .True.
 
              Else If (command .Eq. 'HELP') Then
 
!              Display help message
                 Call F2D_COLOURSHELP (.True., status)
                 update_image = .True.
                 update_menu = .True.
 
              Else
 
!              One of the colour table choices has been chosen
 
!              Convert choice to number
                 Do item = 4, num_menu
 
                    If (MENU(item) .Eq. command) Then
                       lut_choice = item - 3
                    End If
 
                 End Do
 
!              Set new lut parameters
                 STYLE%colour_table = lut_choice
                 Call GS_SET_LUTSTYLE (STYLE, status)
 
!              Change workstation colour table
                 Call GS_COLOURS (status)
 
!              If the colour mapping method is fixed, we need to
!              update the image
                 update_image = static_colours
 
              End If
 
           End If
 
!        Check status
           If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
!     Save colour table choice in internal data-store
        len_string = Len_trim(MENU(lut_choice + 3))
        Call IO_SET_KEYVALUE ('COLOUR_TABLE', len_string, &
          MENU(lut_choice + 3), db_stat, status)
 
     End If
 
     End Subroutine F2D_COLOURS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
