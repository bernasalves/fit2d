!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************************
!  *                          *
!  * f2d_gui_extreme_xtal.f90 *
!  *                          *
!  ****************************
 
!+ F2D_GUI_EXTREME_XTAL - FIT 2-D GUI EXTREME_XTAL
     Subroutine F2D_GUI_EXTREME_XTAL (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    FIT2D GUI for crystallography
!  Keywords:
!    Image~Display, Display~Image
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    25-Sep-2009: V0.7 Add "INTEGRATE" command (Hammersley)
!    27-Jan-2009: V0.6 Remove temporarily (Hammersley)
!    23-Jan-2009: V0.5 Modify options (Hammersley)
!    02-Jun-2008: V0.4 Remove temporarily (Hammersley)
!    26-Oct-2007: V0.3 Add "LOAD PEAKS" command (Hammersley)
!    25-Oct-2007: V0.2 Add "SAVE PEAKS" command (Hammersley)
!    15-Nov-2006: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
!  Use GS_LIB
!  Use FIO_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file ! Name of current data
!      input file
     Logical, Intent(INOUT) :: data_defined ! .True., if data exists within
!      the program
     Logical, Intent(INOUT) :: memory_exist ! .True. if memory array exists
     Logical, Intent(INOUT) :: memory_defined ! .True. if the memory
!      contains data
     Logical, Intent(INOUT) :: variances_exist ! .True., if a data variance
!      array is created
     Logical, Intent(INOUT) :: mask_exist ! .True., if the mask array exists
     Integer, Intent(INOUT) :: xmaxdat ! Dimension size in X-direction for
!      data arrays
     Integer, Intent(INOUT) :: ymaxdat ! Dimension size in Y-direction for
!      data arrays
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements defined in
!      Y-direction
!    Real X_AXIS(xmaxdat) ! X-axis values
!    Real Y_AXIS(ymaxdat) ! Y-axis values
!    Real DATA(xmaxdat, ymaxdat) ! The data values
!    Real VARIANCES(xmaxdat, ymaxdat) ! The estimated variance values
     Character(Len = *), Intent(INOUT) :: title ! Title for plot
     Character(Len = *), Intent(INOUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: zlabel ! Z-axis label for plot
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
!  Export:
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
!    Real MX_AXIS(xmaxdat) ! Array containing data X-coordinates
!    Real MY_AXIS(ymaxdat) ! Array containing data Y-coordinates
!    Real MDATA(xmaxdat, ymaxdat) ! Array containing data to be fitted
!    Real MVARIANCES(xmaxdat, ymaxdat) ! Array containing variances in
!    the data values
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Real, Intent(OUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(OUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.7' ! Version number
     Integer, Parameter :: Max_menu = 17 ! Dimension size of menu
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection
!      prompt text
!  Local Variables:
     Character(Len = 1) :: bell ! ASCII character to ring a bell !
     Character(Len = 80) :: command ! User entered command
     Character(Len = 256) :: next_file ! Full name of next input file
     Character(Len = 20) :: print_type ! Type, of graphics to print:
!      supported types are:
!        "banner"
!        "image"
!        "contour"
!        "x/y graph"
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: max_peaks ! Dimension of peak arrays
     Integer :: num_coordinates ! Number of return coordinates
     Integer :: num_menu ! Number of choices in menu
     Integer :: num_peaks ! Number of found peaks
     Integer :: retstat ! Return status for "IO_NEWFILENAME"
!      0 = Good status
     Integer stat ! Status return variable for "Allocate"
     Logical :: continue ! .True., until user wants to exit
     Logical :: data_warning ! .True., if there is no data, and the user tries 
!      an operation which requires data
     Logical, Save :: draw_bad_weak = .False. ! .True., if bad but non-saturated
!      "peaks" are to be drawn
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 12), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanation
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
     Type(PEAK_STRUCTURE), Allocatable :: PEAKS(:) ! Dynamically allocated array
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 17) / &
       'EXIT', &
       '?', &
       'HELP', &
       'DISPLAY', &
       'EXCHANGE', &
       'FULL', &
       'OPTIONS', &
       'PRINT', &
       'UN-ZOOM', &
       'ZOOM IN', &
       'Z-SCALING', &
       'SET GEOMETRY', &
       'PEAK SEARCH', &
       'MASK', &
       'VIEW PEAKS', &
       'SAVE PEAKS', &
       'LOAD PEAKS' /
     Data (MENUTXT(item), item = 1, 17) / &
       'EXIT: Exit Interface and return to main interfaces menu', &
       '?: This help on the menu choices', &
       'HELP: Help text on this grphical menu', &
       'DISPLAY: Further graphical display possibilities', &
       'EXCHANGE: Swap current data with the "memory"', &
       'FULL: View image of full data', &
       'OPTIONS: Further display control menu', &
       'PRINT: Output current graphics to PostScript file', &
       'UN-ZOOM: Zoom out to see more of the data', &
       'ZOOM IN: Graphical region definition', &
       'Z-SCALING: Automatic or user control of intensity display range', &
       'SET GEOMETRY: Define diffraction geometry', &
       'PEAK SEARCH: Find, display, and calculate statistics on peaks', &
       'MASK: Defined regions to be ignored in the peak search, etc.', &
       'VIEW PEAKS: Interactive viewer for peak lattice ivestigation', &
       'SAVE PEAKS: Save peak positions, intensity, etc. to a text file', &
       'LOAD PEAKS: Load peak positions, intensity, etc. from a text file' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_EXTREME_XTAL ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_GUI_EXTREME_XTAL ' // Version)
     Else

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Temporarily unavailable
        Call GS_BACKGROUND (status)
        Call GS_FWARNING (1, 1, 'NOT AVAILABLE YET', status)
        Return

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Allocate memory for peaks array
        max_peaks = 100000
        num_peaks = 0
        Allocate (PEAKS(max_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_GUI_EXTREME_XTAL ' // Version)
           Return
        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputting menu commands until requested to stop
        num_menu = Max_menu
        input_mode = 2
        continue = .True.
        update_image = .True.
        update_menu = .True.
        Do While (continue)
 
!        Draw or re-draw display
           If (update_image) Then
 
              If (data_defined) Then
 
!              Redraw image
                 Call GS_MPLOT (.True., xmaxdat, ymaxdat, %val(pDATA), &
                   %val(pMASK), %val(pXAXIS), %val(pYAXIS), xstrelm, ystrelm, &
                   xendelm, yendelm, title, xlabel, ylabel, zlabel, status)
                 print_type = 'masked_image'
 
!              Draw peaks
                 If (num_peaks .Gt. 0) Then
                    Call F2D_DRAW_PEAKS (draw_bad_weak, max_peaks, num_peaks, &
                      PEAKS, status)
                 End If
 
              Else
 
!              Re-draw welcome message
                 MESSAGE(1) = ' '
                 MESSAGE(2) = 'WELCOME TO THE SINGLE'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'CRYSTAL INTERFACE'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = 'PRESS "PEAK SEARCH" TO'
                 MESSAGE(7) = ' '
                 MESSAGE(8) = 'INPUT A SERIES OF IMAGES'
                 MESSAGE(9) = ' '
                 MESSAGE(10) = ' '
                 Call GS_PPROMPT (Max_message, 10, MESSAGE, status)
 
              End If
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Reset menu layout
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 9, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_vertical, 4, 12, 9, status)
              End If
 
!           Re-draw menu
              Call GS_FMENU (1, 0, 'not used', Max_menu, num_menu, MENU, status)
 
           End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Start of event loop'')')
!        Write (*, '(''auto_input = '' l2)') auto_input
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        By default update graphics
           update_image = .True.
           update_menu = .True.
           data_warning = .False.
           command = 'null'
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, input_mode, &
             input_type, command, x_coordinate, y_coordinate, status)
 
           If (input_type .Eq. Gs_resize) Then
 
              update_image = .True.
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_motion) Then
 
!           Pointer motion, don't update
              update_image = .False.
              update_menu = .False.
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Output the coordinate position and intensity
              If (data_defined) Then
 
                 Call F2D_CLICK (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                   yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, &
                   xlabel, ylabel, zlabel, x_coordinate, y_coordinate, &
                   experiment, update_image, update_menu, status)
 
              End If
 
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
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'EXCHANGE') Then
 
!              Swap current data with "memory"
                 Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
                   xlabel, ylabel, zlabel, variances_exist, data_defined, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   xstrelm, ystrelm, xendelm, &
                   yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   memory_defined, mx_pixel_size, my_pixel_size, status)
 
              Else If (command .Eq. 'FULL') Then
 
!              Set ROI to cover all data
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
 
              Else If (command .Eq. 'HELP') Then
 
                 Call F2D_GUI_XTALHELP (.True., status)
 
              Else If (command .Eq. 'MASK') Then
 
                 If (data_defined) Then
 
                    Call F2D_MASK (.True., xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pDATA), %val(pXAXIS), %val(pYAXIS), title, xlabel, &
                      ylabel, zlabel, experiment, xstrelm, &
                      ystrelm, xendelm, yendelm, %val(pMASK), status)
 
!                 Draw peaks
                    If (num_peaks .Gt. 0) Then
                       Call F2D_DRAW_PEAKS (draw_bad_weak, max_peaks, &
                         num_peaks, PEAKS, status)
                    End If
 
                    update_image = .False.
                 Else
                    data_warning = .True.
                 End If
                 update_menu = .True.
 
              Else If (command .Eq. 'SET GEOMETRY') Then
 
!              Input geometry
                 If (data_defined) Then
 
!                 Input experimental geometry and store in internal data-base
                    Call F2D_GEOMETRY (.True., xmaxdat, ymaxdat, xstrelm, &
                      ystrelm, xendelm, yendelm, %val(pDATA), %val(pXAXIS), &
                      %val(pYAXIS), title, xlabel, ylabel, zlabel, &
                      experiment, status)
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .True.
                 update_menu = .True.
 
              Else If (command .Eq. 'PEAK SEARCH') Then
 
!              Input images and find peak positions in reciprocal space
                 Call F2D_EXT_PEAKSEARCH (input_options, xmaxdat, ymaxdat, &
                   max_peaks, xnumdat, ynumdat, &
                   xstrelm, ystrelm, xendelm, yendelm, &
                   experiment, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pVARIANCES), %val(pMXAXIS), %val(pMYAXIS), &
                   %val(pMDATA), %val(pMVARIANCES), %val(pMASK), &
                   title, xlabel, ylabel, zlabel, variances_exist, &
                   data_defined, num_peaks, PEAKS, draw_bad_weak, status)
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'LOAD PEAKS') Then

!              Input peak positions, intensity, etc. from an ASCII file
                 Call F2D_EXT_LOAD_PEAKS (max_peaks, num_peaks, PEAKS, status)
                 update_image = .True.
                 update_menu = .True.

              Else If (command .Eq. 'OPTIONS') Then
 
!              Further options menu
                 Call F2D_OPTIONS (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pVARIANCES), &
                   title, xlabel, ylabel, zlabel, experiment, &
                   variances_exist, xstrelm, ystrelm, xendelm, yendelm, &
                   print_type, status)
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'OUTPUT') Then
 
                 If (data_defined) Then
                    Call FIO_GUI_OUTPUT (input_file, xmaxdat, ymaxdat, &
                      xstrelm, ystrelm, xendelm, yendelm, %val(pXAXIS), &
                      %val(pYAXIS), %val(pDATA), %val(pVARIANCES), title, &
                      xlabel, ylabel, zlabel, variances_exist, &
                      experiment%x_pixel_size, experiment%y_pixel_size, status)
                    update_image = .True.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'SAVE PEAKS') Then

!              Output peak positions, intensity, etc. to an ASCII file
                 Call F2D_EXT_SAVE_PEAKS (max_peaks, num_peaks, PEAKS, status)
                 update_image = .True.
                 update_menu = .True.

              Else If (command .Eq. 'UN-ZOOM') Then
 
!              Increase size of 1-D region or ROI
                 Call GS_INP_UNZOOM (xnumdat, ynumdat, xstrelm, ystrelm, &
                   xendelm, yendelm, status)
 
              Else If (command .Eq. 'SPY-GLASS') Then
 
!              Interactive cursor controlled zoom window
                 If (data_defined) Then
 
!                 Input dummy coordinate
                    num_coordinates = 1
                    MESSAGE(1) = 'Move the cursor into the displayed image.'
                    MESSAGE(2) = 'The "spy-glass" will show a zoomed in'
                    MESSAGE(3) = 'region of the image centred around the'
                    MESSAGE(4) = 'cursor position. As the cursor moves so'
                    MESSAGE(5) = 'should the spy-glass image.'
                    Call GS_INPS_FCOORDINATES (mask_exist, .True., xmaxdat, &
                      ymaxdat, xstrelm, ystrelm, xendelm, yendelm, &
                      %val(pDATA), %val(pMASK), %val(pXAXIS), %val(pYAXIS), &
                      title, xlabel, ylabel, zlabel, 'CLICK HERE TO END', 5, &
                      MESSAGE, .False., 1, num_coordinates, x_coordinate, &
                      y_coordinate, status)
 
!                 Check for user escape
                    If (status .Eq. St_escapevalue) Then
                       status = St_goodvalue
                    Else If (status .Ne. St_goodvalue) Then
                       Return
                    End If
 
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two points
                 If (data_defined) Then
                    Call F2D_ZOOMIN ( xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, xlabel, &
                      ylabel, zlabel, xstrelm, ystrelm, xendelm, yendelm, &
                      status)
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'DISPLAY') Then
 
!              Further display options menu
                 If (data_defined) Then
                    Call F2D_DISPLAY (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                      variances_exist, experiment, xstrelm, &
                      ystrelm, xendelm, yendelm, mxnumdat, mynumdat, &
                      %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), &
                      %val(pMVARIANCES), mxstrelm, mystrelm, mxendelm, &
                      myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                      memory_defined, mx_pixel_size, my_pixel_size, &
                      print_type, status)
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'PRINT') Then
 
!              Output current graphics to file, prompting for file name if 
!              necessary
                 If (data_defined) Then
 
                    Call F2D_PRINT (.True., print_type, mask_exist, xmaxdat, &
                      ymaxdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pVARIANCES), %val(pMASK), title, xlabel, ylabel, &
                      zlabel, variances_exist, xstrelm, ystrelm, xendelm, &
                      yendelm, status)
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'VIEW PEAKS') Then

                 If (num_peaks .Gt. 0) Then

!              Simulate some peaks at present
                 Call F2D_EXT_3DVIEW (max_peaks, num_peaks, PEAKS, experiment, &
                   status)
                 update_image = .True.
                  update_menu = .True.

                 Else
                    Call GS_FWARNING (1, 1, 'NO PEAKS HAVE BEEN FOUND', status)
                 End If

              Else If (command .Eq. 'Z-SCALING') Then
 
!              User defined Z-scaling mode
                 If (data_defined) Then
                    Call F2D_GUI_ZSCALE (.True., xmaxdat, ymaxdat, xnumdat, &
                      ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pMASK), title, xlabel, ylabel, zlabel, xstrelm, &
                      ystrelm, xendelm, yendelm, experiment, &
                      .False., x_coordinate, y_coordinate, status)
 
!                 Draw peaks
                    If (num_peaks .Gt. 0) Then
                       Call F2D_DRAW_PEAKS (draw_bad_weak, max_peaks, &
                         num_peaks, PEAKS, status)
                    End If
 
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              End If
 
           End If
 
!        Output warning message if required
           If (data_warning) Then
 
              Call GS_FWARNING (1, 1, 'DATA NEEDED, BUT NONE IS DEFINED', &
                status)
              update_menu = .True.

           End If
 
!        Check status
           If (status .Eq. St_escapevalue) Then
 
!           Reset status system
              Call ST_DEF_SYSTEM (status)
 
           Else If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
!     Free memory for peaks array
        Deallocate (PEAKS)
 
     End If
 
     End Subroutine F2D_GUI_EXTREME_XTAL
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 
 
