!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************************
!  *                          *
!  * f2d_gui_xtallography.f90 *
!  *                          *
!  ****************************
 
!+ F2D_GUI_XTALLOGRAPHY - FIT 2-D GUI XTALLOGRAPHY
     Subroutine F2D_GUI_XTALLOGRAPHY (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
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
!    26-Nov-2014: V0.35 Changes to "F2D_CLICK" (Hammersley)
!    15-Nov-2006: V0.34 Use "PEAK_STRUCTURE" to hold results (Hammersley)
!    25-Apr-2006: V0.33 Use Fortran-90 dynamically allocated arrays (Hammersley)
!    24-Apr-2006: V0.32 Add "input_options" structure (Hammersley)
!    17-Mar-2006: V0.31 Support for arbitrary aspect ratio windows (Hammersley)
!    14-Mar-2006: V0.30 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.29 Alter menu lay-out for landscape windows (Hammersley)
!    26-Mar-1999: V0.28 Output text message when there is no data (Hammersley)
!    17-Feb-1999: V0.27 "F2D_GUI_INPUT" replaced by "FIO_GUI_INPUT" (Hammersley)
!    26-Jun-1998: V0.26 Add "PEAKS_DSPACINGS" dynamic array to store peak 
!      spacings (Hammersley)
!    27-Jan-1998: V0.25 Correct bugs introduced in "NEXT FILE"
!      and "PREV FILE" commands at previous version (Hammersley)
!    23-Jan-1998: V0.24 Changes to "F2D_GUI_INPUT" so that the
!      user prompt text is set by the calling routine (Hammersley)
!    22-Jan-1998: V0.23 Changes to the argument list of "F2D_PRINT" (Hammersley)
!    09-Mar-1997: V0.22 Add mask creation and "MASK" option (Hammersley)
!    20-Feb-1997: V0.21 Transfer name of input file between interfaces 
!      (Hammersley)
!    19-Feb-1997: V0.20 Add option to "PEAK SEARCH" to output results to an 
!      ASCII file (Hammersley)
!    25-Jan-1997: V0.19 Cater for "CANCEL" buttons (Hammersley)
!    07-Nov-1996: V0.18 Correct menu layout after "ZOOM IN" command (Hammersley)
!    08-Oct-1996: V0.17 Use "GS_INP_UNZOOM" to un-zoom 1-D and/or 2-D regions 
!      (Hammersley)
!    26-Aug-1996: V0.16 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    23-Aug-1996: V0.15 Changes to "GS_INPS_FCOORDINATES" (Hammersley)
!    19-Mar-1996: V0.14 Allow output and control of 1-D X/Y graphs (Hammersley)
!    25-Feb-1996: V0.13 Style changes removed to "f2d_gui" (Hammersley)
!    15-Feb-1996: V0.12 Renamed from "f2d_gui" (Hammersley)
!    11-Feb-1996: V0.11 Add "SET GEOMETRY" command (Hammersley)
!    10-Feb-1996: V0.10 Default automatic inverse grey-scale, weak diffraction 
!      peak scaling (Hammersley)
!    09-Feb-1996: V0.9 Add intensity and intensity standard deviation arrays 
!      (Hammersley)
!    08-Feb-1996: V0.8 Add peak overlay (Hammersley)
!    04-Feb-1996: V0.7 Program arrays passed through common to allow "EXCHANGE" 
!      command to work properly (Hammersley)
!    28-Jan-1996: V0.6 Add peak search command (Hammersley)
!    25-Jan-1996: V0.5 Add "AUTO INPUT" command (Hammersley)
!    23-Jan-1996: V0.4 Add "NEXT" button for input next file (Hammersley)
!    21-Jan-1996: V0.3 Add warning messages (Hammersley)
!    20-Jan-1996: V0.2 Draw FIT2D banner (Hammersley)
!    14-Jan-1996: V0.1 Original (Hammersley)
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
     Logical, Intent(INOUT) :: variance_exist ! .True., if a data variance
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
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.35' ! Version number
     Integer, Parameter :: Max_menu = 20 ! Dimension size of menu
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
     Integer, Save :: alarm_seconds = 200 ! Maximum number of seconds
!      between image input before the alarm goes off
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
     Logical, Save :: alarm = .False. ! .True., if a time-out alarm is to
!      specified
     Logical :: alarm_sounded ! .True., if a the time-out alarm sounded
     Logical :: auto_input ! .True., if files are to be automatically input as 
!      they occur
     Logical :: auto_peak_search ! .True., if images are to be peak searched 
!      automatically on input
     Logical :: continue ! .True., until user wants to exit
     Logical :: data_warning ! .True., if there is no data, and the user tries 
!      an operation which requires data
     Logical, Save :: draw_bad_weak = .False. ! .True., if bad but non-saturated
!      "peaks" are to be drawn
     Logical :: no_events ! .True., if there are no events to input
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Real :: time_cpu ! CPU time
     Real :: time_event ! Time when event occurs
     Real :: time_file ! Time when a file was input
     Real :: time_last ! Last stored time in seconds
     Real :: time_now ! Time in seconds
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
     Type(PEAK_STRUCTURE), Allocatable :: PEAKS(:) ! Peak search results
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 10) / 'EXIT', '?', 'HELP', 'AUTO INPUT', &
       'DISPLAY', 'EXCHANGE', 'FULL', 'INPUT', 'OPTIONS', 'MOVEMENT' /
     Data (MENU(item), item = 11, 20) / 'PRINT', 'UN-ZOOM', 'ZOOM IN', &
       'Z-SCALING', 'NEXT FILE', 'PREV FILE', 'SET GEOMETRY', 'PEAK SEARCH', &
       'SPY-GLASS', 'MASK' /
     Data (MENUTXT(item), item = 1, 10) / &
       'EXIT: Exit Interface and return to main interfaces menu', &
       '?: This help on the menu choices', &
       'HELP: Help text on this grphical menu', &
       'AUTO INPUT: Automatically input files from a sequence', &
       'DISPLAY: Further graphical display possibilities', &
       'EXCHANGE: Swap current data with the "memory"', &
       'INPUT: Input data from a file on disk', &
       'FULL: View image of full data', &
       'OPTIONS: Further display control menu', &
       'MOVEMENT: Easily controlled movement around an image' /
     Data (MENUTXT(item), item = 11, 20) / &
       'PRINT: Output current graphics to PostScript file', &
       'UN-ZOOM: Zoom out to see more of the data', &
       'ZOOM IN: Graphical region definition', &
       'Z-SCALING: Automatic or user control of intensity display range', &
       'NEXT FILE: Input "next" file in a file sequence', &
       'PREV FILE: Input "previous" file in a file sequence', &
       'SET GEOMETRY: Define diffraction geometry', &
       'PEAK SEARCH: Find, display, and calculate statistics on peaks', &
       'SPY-GLASS: Cursor controlled "real-time" zoom window', &
       'MASK: Defined regions to be ignored in the peak search, etc.' /
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_XTALLOGRAPHY ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_XTALLOGRAPHY ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Arguments would appear to be reasonable, go ahead.
 
!     Allocate memory for peak arrays
        max_peaks = 20000
        num_peaks = 0
        Allocate (PEAKS(max_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_GUI_XTALLOGRAPHY ' // Version)
           Return
        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Initialise variables
        bell = Char(7)
 
!     Set menu layout style
        num_menu = Max_menu
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Find reference time
        Call IO_TIMES (time_last, time_cpu, status)
 
!     Loop inputting menu commands until requested to stop
        input_mode = 2
        no_events = .False.
        auto_input = .False.
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
                 MESSAGE(2) = 'WELCOME TO ON-LINE'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'CRYSTALLOGRAPHY GUI'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = 'PRESS "INPUT" TO '
                 MESSAGE(7) = ' '
                 MESSAGE(8) = 'SELECT A FILE'
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
           alarm_sounded = .False.
           update_image = .True.
           update_menu = .True.
           data_warning = .False.
           command = 'null'
 
!        Find current time
           Call IO_TIMES (time_now, time_cpu, status)
 
!        If required check time since last file was found
           If (alarm .And. no_events .And. Int(time_now - time_file) .Gt. &
             alarm_seconds) Then
 
              alarm_sounded = .True.
 
!           Make a noise
              Do item = 1, 20
                 Write (*, '(a1)') bell
              End Do
 
!           Issue warning message
              Call GS_FWARNING (1, 1, &
                'TIME-OUT ALARM: NEW FILE DOESN''T EXIST YET', status)
 
!           Stop automatic updating of files
              alarm = .False.
              auto_input = .False.
              input_mode = 2
              MENU(4) = 'AUTO INPUT'
              MENUTXT(4) = &
                'AUTO INPUT: Automatically input files from a sequence'
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''Time since last check = '', f10.3)')
!           :          time_now - time_last
!           Write (*, '(''no_events = '' l2)') no_events
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Only check maximum once every second
           Else If (auto_input .And. no_events .And. (time_now - time_last) &
             .Gt. 1.0) Then
 
              time_last = time_now
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''Check file exists'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Check if next file in sequence exists
              Call IO_NEWFILENAME (input_file, 1, 1, retstat, next_file, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''retstat = '' i4)') retstat
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              If (retstat .Eq. 0) Then
 
!              File exists, so input it
                 command = 'NEXT FILE'
              Else
                 command = 'null'
              End If
 
!           Force checking of events queue
              no_events = .False.
 
           Else
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''Graphical input'')')
!           Write (*, '(''input_mode = '', i4)') input_mode
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Get user to select between the available menu options
              command = 'null'
              Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, input_mode, &
                input_type, command, x_coordinate, y_coordinate, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''input_type = '', i4)') input_type
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Set flag if no input
              no_events = input_type .Eq. Gs_none
 
              If (no_events) Then
 
!              Find time
                 Call IO_TIMES (time_now, time_cpu, status)
 
!              Check time since last event
                 If ((time_now - time_event) .Gt. 1.0) Then
 
!                 Sleep a little before checking again
                    Call IO_SLEEP (1.0, status)

                 End If
 
                 command = 'null'
 
              Else
 
!              Store time of event
                 Call IO_TIMES (time_event, time_cpu, status)

              End If
 
           End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
           If (alarm_sounded) Then
 
              update_image = .False.
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_resize) Then
 
              update_image = .True.
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_motion) Then
 
!           Pointer motion, don't update
              update_image = .False.
              update_menu = .False.
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Output the coordinate position and intensity
              If (data_defined) Then
 
                 Call F2D_CLICK (1, xmaxdat, ymaxdat, &
                   xstrelm, ystrelm, xendelm, &
                   yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, &
                   xlabel, ylabel, zlabel, x_coordinate, y_coordinate, &
                   experiment, update_image, update_menu, status)
 
              End If
 
           Else
 
!           Menu choice input
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''command = '', a)') command
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
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
 
              Else If (command .Eq. 'AUTO INPUT') Then
 
                 If (data_defined) Then
 
!                 Automatic updating of file sequence requested
                    auto_input = .True.
                    input_mode = 10
                    MENU(4) = 'STOP INPUT'
                    MENUTXT(4) = 'STOP INPUT: Cancel automatic ' // &
                      'input of files from a file sequence'
 
!                 Ask if an alarm should be set
                    MESSAGE(1) = 'You may choose to set a ' // &
                      'time-out alarm, so that if there is not a'
                    MESSAGE(2) = 'new file after a set number ' // &
                      'of seconds the alarm will notify you.'
                    MESSAGE(3) = 'This is to help spot ' // &
                      'failures in data collection.'
                    MESSAGE(4) = ' '
                    MESSAGE(5) = 'Enter "YES" to set the ' // &
                      'alarm, and the number of seconds will be'
                    MESSAGE(6) = 'requested.'
                    Call GS_INPL (.True., 0, 1, .True., 'SET TIME-OUT ALARM', &
                      6, MESSAGE, 1, 'Enter "YES" or "NO"', alarm, status)
 
                    If (alarm) Then
 
                       MESSAGE(1) = 'Enter number of seconds ' // &
                         'between data input, prior to'
                       MESSAGE(2) = 'issuing a warning alarm.'
                       Call GS_INPI (.True., 5, 3600, .True., &
                         'ALARM TIME-OUT (SECONDS)', 2, MESSAGE, 1, &
                         'Enter number of seconds', alarm_seconds, status)
 
!                    Set time of "last file"
                       Call IO_TIMES (time_file, time_cpu, status)
 
                    End If
 
                    MESSAGE(1) = 'The peak search algorithm ' // &
                      'may be applied automatically to each'
                    MESSAGE(2) = 'image on input. Enter "YES" ' // &
                      'to have automatic peak searching.'
                    MESSAGE(3) = 'Normal peaks are displayed ' // &
                      'as red crosses, and saturated peaks'
                    MESSAGE(4) = 'are displayed with yellow ' // &
                      'crosses. Yellow asterisks denoted'
                    MESSAGE(5) = '"bad" saturated peaks.'
                    Call GS_INPL (.True., 0, 1, .True., 'AUTO PEAK SEARCH', 5, &
                      MESSAGE, 1, 'Enter "YES" or "NO"', auto_peak_search, &
                      status)
 
                 Else
 
                    Call GS_FWARNING ( 1, 1, 'NO FILES DEFINED: USE "INPUT" ' &
                      // 'TO SELECT A FILE', status)
 
                 End If
 
                 update_menu = .True.
 
              Else If (command .Eq. 'STOP INPUT') Then
 
!              Automatic updating of file sequence requested
                 auto_input = .False.
                 input_mode = 2
                 MENU(4) = 'AUTO INPUT'
                 MENUTXT(4) = 'AUTO INPUT: Automatically input ' // &
                   'files from a sequence'
 
                 update_menu = .True.
 
              Else If (command .Eq. 'EXCHANGE') Then
 
!              Swap current data with "memory"
                 Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
                   xlabel, ylabel, zlabel, variance_exist, data_defined, &
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
 
              Else If (command .Eq. 'INPUT') Then
 
!              Input data
                 PROMPT(1) = 'SELECT FILE TO INPUT IMAGE DATA'
                 PROMPT(2) = '(click on "HELP" for list of formats)'
                 Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 0, &
                   input_options, xmaxdat, &
                   ymaxdat, variance_exist, data_defined, input_file, xnumdat, &
                   ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                   experiment, status)
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
                 num_peaks = 0
 
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
 
              Else If (command .Eq. 'MOVEMENT') Then
 
                 If (data_defined) Then
 
!                 Interactive image display and manipulation
                    Call F2D_IMAGE (.True., xmaxdat, ymaxdat, xnumdat, &
                      ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pMASK), &
                      %val(pDATA), &
                      %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                      variance_exist, experiment, xstrelm, &
                      ystrelm, xendelm, yendelm, mxnumdat, mynumdat, &
                      %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), &
                      %val(pMVARIANCES), mxstrelm, mystrelm, mxendelm, &
                      myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                      memory_defined, mx_pixel_size, my_pixel_size, &
                      print_type, status)
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'NEXT FILE') Then
 
!              Input data
                 Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 1, &
                   input_options, xmaxdat, &
                   ymaxdat, variance_exist, data_defined, input_file, xnumdat, &
                   ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                   experiment, status)
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
                 num_peaks = 0
 
!              Perform automatic peak search if required
                 If (auto_input .And. auto_peak_search) Then
 
                    num_peaks = 0
                    Call F2D_GUI_PEAKSEARCH (.False., input_file, .False., &
                      xmaxdat, ymaxdat, xnumdat, ynumdat, %val(pDATA), &
                      %val(pMASK), %val(pXAXIS), %val(pYAXIS), xstrelm, &
                      ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
                      zlabel, max_peaks, num_peaks, experiment, &
                      %val(pMDATA), PEAKS, draw_bad_weak, status)
 
                 End If
 
!              Record time
                 Call IO_TIMES (time_file, time_cpu, status)
 
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
 
!              Calculate and display peak positions
                 If (data_defined) Then
 
                    num_peaks = 0
                    Call F2D_GUI_PEAKSEARCH (.True., input_file, .True., &
                      xmaxdat, ymaxdat, xnumdat, ynumdat, %val(pDATA), &
                      %val(pMASK), %val(pXAXIS), %val(pYAXIS), xstrelm, &
                      ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
                      zlabel, max_peaks, experiment, num_peaks, &
                      %val(pMDATA), PEAKS, draw_bad_weak, status)
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'PREV FILE') Then
 
!              Input data
                 Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, -1, &
                   input_options, xmaxdat, &
                   ymaxdat, variance_exist, data_defined, input_file, xnumdat, &
                   ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                   experiment, status)
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
                 num_peaks = 0
 
              Else If (command .Eq. 'OPTIONS') Then
 
!              Further options menu
                 Call F2D_OPTIONS (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pVARIANCES), &
                   title, xlabel, ylabel, zlabel, experiment, &
                   variance_exist, xstrelm, ystrelm, xendelm, yendelm, &
                   print_type, status)
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'OUTPUT') Then
 
                 If (data_defined) Then
                    Call FIO_GUI_OUTPUT (input_file, xmaxdat, ymaxdat, &
                      xstrelm, ystrelm, xendelm, yendelm, %val(pXAXIS), &
                      %val(pYAXIS), %val(pDATA), %val(pVARIANCES), title, &
                      xlabel, ylabel, zlabel, variance_exist, &
                      experiment%x_pixel_size, experiment%y_pixel_size, status)
                    update_image = .True.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
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
                    Call GS_INPS_FCOORDINATES ( mask_exist, .True., xmaxdat, &
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
                      %val(pXAXIS), %val(pYAXIS), %val(pMASK), %val(pDATA), &
                      %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                      variance_exist, experiment, xstrelm, &
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
                      zlabel, variance_exist, xstrelm, ystrelm, xendelm, &
                      yendelm, status)
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
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
 
              Call GS_FWARNING ( 1, 1, 'DATA NEEDED, BUT NONE IS DEFINED', &
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
 
     End Subroutine F2D_GUI_XTALLOGRAPHY
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 
 
