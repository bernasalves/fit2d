!********1*********2*********3*********4*********5*********6*********7*********8

!  ***********************
!  *                     *
!  * f2d_gui_vectors.f90 *
!  *                     *
!  ***********************

!+ F2D_GUI_VECTORS - FIT 2-D GUI VECTORS (viewing etc.)
     Subroutine F2D_GUI_VECTORS (input_options, experiment, results, &
!       max_vec_values, max_vectors, num_vectors, &
!       STR_VECTORS, END_VECTORS, VECTORS, VECTOR_TITLES, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
       DATA, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, status)
!  Description:
!    FIT2D GUI for vector viewing, input etc.
!  Keywords:
!    Vector.Operation, Vector.Input, Vector.Output
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    12-Dec-2014: V0.10 "SET SIZES" command (Hammersley)
!    09-Dec-2014: V0.9 Use result vectors data structure (Hammersley)
!    08-Dec-2014: V0.8 Add "TRANSFER" and "SET VECTOR" commands (Hammersley)
!    05-Dec-2014: V0.7 Add "TEXT OUTPUT" command (Hammersley)
!    24-Apr-2006: V0.6 Add "INPUT_OPTIONS" structure (Hammersley)
!    16-Mar-2006: V0.5 Set data to be defined (Hammersley)
!    14-Mar-2006: V0.4 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    17-Feb-1999: V0.3 "F2D_GUI_INPUT" replaced by "FIO_GUI_INPUT" (Hammersley)
!    02-Nov-1998: V0.2 Make into a proper menu and add "OUTPUT" command 
!      (Hammersley)
!    29-Oct-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!     Integer, Intent(IN) :: max_vec_values ! First dimension of "VECTORS"
!      array (maximum number of values which can be stored in a vector)
!     Integer, Intent(IN) :: max_vectors ! Second dimension of "VECTORS"
!      array (maximum number of vectors which can be stored)
!  Import/Export:
     Type(RESULT_VECTORS), Intent(INOUT) :: results ! Result vectors
!     Integer, Intent(INOUT) :: num_vectors ! Number of values defined in the
!      "time"-series for each vector
!     Integer, Intent(INOUT) :: STR_VECTORS(max_vectors) ! Starting defined
!      element for "VECTORS"
!     Integer, Intent(INOUT) :: END_VECTORS(max_vectors) ! End defined
!      elements for "VECTORS"
!     Real, Intent(INOUT) :: VECTORS(max_vec_values, max_vectors)
!      Multiple 1-D arrays of vector values
!     Character(Len = *), Intent(INOUT) :: VECTOR_TITLES(max_vectors)
!      Titles for the 1-D data-sets
     Integer, Intent(IN) :: xmaxdat ! First Dimension size for "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second Dimension size for "DATA"
!  Export:
     Integer, Intent(OUT) :: xnumdat ! Number of defined elements in X-direction
     Integer, Intent(OUT) :: ynumdat ! Number of defined elements in X-direction
     Integer, Intent(OUT) :: xstrelm ! First X-element of defined region
     Integer, Intent(OUT) :: ystrelm ! First Y-element of defined region
     Integer, Intent(OUT) :: xendelm ! Last X-element of defined region
     Integer, Intent(OUT) :: yendelm ! Last Y-element of defined region
     Real, Intent(OUT) :: DATA(xmaxdat, ymaxdat) ! Main data array
     Real, Intent(OUT) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(OUT) :: YAXIS(ymaxdat) ! Y-axis values
     Character(Len = *), Intent(OUT) :: title ! Title for the graph
     Character(Len = *), Intent(OUT) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: zlabel ! Z-axis label for data
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.10' ! Version number
     Integer, Parameter :: Max_menu = 13 ! Dimension size of menu
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection prompt 
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Character(Len = 256), Save :: input_file = 'vector.dat'
!      Name of current vector input file
     Character(Len = 20) :: print_type ! Type, of graphics to print:
!      supported types are:
!        "banner"
!        "image"
!        "contour"
!        "x/y graph"
     Character(Len = 80), Save :: vec_xlabel = 'Samples' ! X-axis label
     Character(Len = 80), Save :: vec_ylabel = 'N.A.' ! Y-axis label
     Character(Len = 80), Save :: vec_zlabel = 'Value' ! Z-axis label
     Integer i ! Loop variable 
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Integer :: stat ! Status return variable for "Allocate"
     Integer, Save :: vec_num = 0 ! Number of vector to use
     Logical :: continue ! .True., until user wants to exit
     Logical :: data_defined ! .True., if vector is defined
     Logical :: data_warning ! .True., if there is no data, and the
!      user tries an operation which requires data
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
!    Logical x_error ! .True., means draw X-direction error bars
!    Logical xmax_auto ! .True., means X-maximum is automatically set
!    Logical xmin_auto ! .True., means X-minimum is automatically set
!    Logical y_error ! .True., means draw Y-direction error bars
!    Logical ymax_auto ! .True., means Y-maximum is automatically set
!    Logical ymin_auto ! .True., means Y-minimum is automatically set
     Real :: vec_pixel_size ! Size of bin in the vector
     Real :: x_pixel_size ! Temporary storage of X-pixel size
     Real :: y_pixel_size ! Dummy argument
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Real, Allocatable :: AXIS(:) ! Array to store axis values
     Character(Len = 20), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command
!      explanation
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
     Real :: DUMMY(1) ! Unused dummy argument
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 13) / &
       'EXIT',  'ZOOM IN',    'INPUT',       'SET SIZES', &
       '?',     'FULL',       'OUTPUT',      'OPTIONS', & 
       'HELP',  'SET VECTOR', 'TEXT OUTPUT', 'TRANSFER', &
       'PRINT' /
     Data (MENUTXT(item), item = 1, 13) / &
       'EXIT: Exit menu', &
       'ZOOM IN: Define smaller graphical display region', &
       'INPUT: Input data from a file on disk', &
       'SET SIZES: Define number of vectors and values (destroys old values)', &
       '?: This help on the menu choices', &
       'FULL: View image of full data', &
       'OUTPUT: Save data in an output file', &
       'OPTIONS: Graphics display control menu', &
       'HELP: Help text on this graphical menu', &
       'SET VECTOR: Input number of current vector (display, transfer, etc.)', &
       'TEXT OUTPUT: Output all vector values to a text file', &
       'TRANSFER: Transfer current vector to main data array', &
       'PRINT: Output current graphics to PostScript file' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_GUI_VECTORS'')')
!     Write (*, '(''results%max_values = '', i8)') results%max_values
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_VECTORS ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (results%max_values .Le. 0) Then
        status = St_bad_dim1
     Else If (results%max_vectors .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_GUI_VECTORS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Check that one or more vectors have been defined
        If (results%num_vectors .Le. 0) Then
 
           Call GS_FWARNING (1, 1, 'No vectors have been defined', status)
           Return
 
        End If
 
!     Allocate memory for axis values
!        Call IO_MALLOC (max_vec_values * 4, pAXIS, status)
        Allocate (AXIS(results%max_values), Stat = stat)

!     Check status
        If (stat .Ne. 0) Then
           status = St_mod_ma + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_GUI_VECTORS ' // Version)
           Return
        End If
 
!     Set axis values
        Call F2D_AXES (results%max_values, results%max_values, 1.0, 1.0, AXIS, &
          status)
 
!     Number of vector to use
        Call F2D_INP_NUMVECTOR (results, vec_num, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''STR_VECTORS(vec_num), END_VECTORS(vec_num) = '', &
!       2i6)') STR_VECTORS(vec_num), END_VECTORS(vec_num)
!     Write (*, '(a)') VECTOR_TITLES(vec_num)
!     Do i = STR_VECTORS(vec_num), END_VECTORS(vec_num)
!     Write (*, '(i3, g14.7)') i, VECTORS(i, vec_num)
!     End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputting menu commands until requested to stop
        data_defined = .True.
        num_menu = Max_menu
        input_mode = 2
        continue = .True.
        update_image = .True.
        update_menu = .True.
 
        Do While (continue)
 
!        Draw or re-draw display
           If (update_image) Then

              If (results%num_vectors .Gt. 0) Then

!              Redraw image or 1-D plot
                 Call GS_XYSGRAPH (results%max_values, &
                   results%STARTS(vec_num), &
                   results%ENDS(vec_num), AXIS, results%VECTORS(1, vec_num), &
                   results%TITLES(vec_num), 'Samples', 'Values', status)
                 print_type = 'x/y graph'
              End If

           End If
 
           If (update_menu) Then
 
!           Set menu layout style
              Call GS_SET_MENULAYOUT (0, 4, 14, 9, status)
 
!           Re-draw menu
              Call GS_FMENU (1, 1, 'VECTORS MENU', Max_menu, num_menu, MENU, &
                status)
 
           End If
 
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
 
!              Output X/Y position
                 Call GS_XYCLICK (x_coordinate, y_coordinate, status)
 
                 update_image = .False.
                 update_menu = .False.
 
              End If
 
           Else
 
!           Menu choice input
 
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
 
              Else If (command .Eq. 'FULL') Then
 
!              Set scaling in all directions to full automatic
                 Call GS_SET_AUTODDR (.True., .True., .True., .True., status)
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the PD menu
                 Call F2D_GUI_VECHELP (.True., status)
 
              Else If (command .Eq. 'INPUT') Then
 
!              Input data
                 PROMPT(1) = 'SELECT FILE TO INPUT (1-D vector)'
                 PROMPT(2) = '(click on "HELP" for list of formats)'
                 Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 0, &
                   INPUT_OPTIONS, results%max_values, 1, .False., &
                   data_defined, input_file, results%ENDS(vec_num), &
                   1, AXIS, YAXIS, results%VECTORS(1, vec_num), DUMMY, &
                   results%TITLES(vec_num), vec_xlabel, vec_ylabel, &
                   vec_zlabel, experiment, status)
                 vec_pixel_size = experiment%x_pixel_size
                 y_pixel_size = experiment%y_pixel_size

                 results%STARTS(vec_num) = 1
                 print_type = ' '
 
              Else If (command .Eq. 'OPTIONS') Then
 
!              Further options menu
                 x_pixel_size = experiment%X_pixel_size
                 experiment%x_pixel_size = vec_pixel_size
                 Call F2D_OPTIONS (results%max_values, 1, &
                   results%ENDS(vec_num), 1, AXIS, YAXIS, &
                   results%VECTORS(1, vec_num), DUMMY, &
                   results%TITLES(vec_num), vec_xlabel, vec_ylabel, &
                   vec_zlabel, experiment, .False., 1, 1, &
                   results%ENDS(vec_num), 1, print_type, status)
 
                 experiment%x_pixel_size = x_pixel_size
                 update_image = .False.
                 update_menu = .True.
                 print_type = 'image'
 
              Else If (command .Eq. 'OUTPUT') Then
 
                 Call FIO_GUI_OUTPUT (input_file, results%max_values, 1, &
                   results%STARTS(vec_num), 1, results%ENDS(vec_num), 1, &
                   AXIS, YAXIS, results%VECTORS(1, vec_num), DUMMY, &
                   results%TITLES(vec_num), vec_xlabel, vec_ylabel, &
                   vec_zlabel, .False., vec_pixel_size, y_pixel_size, status)
 
                 update_image = .True.
                 update_menu = .True.
 

              Else If (command .Eq. 'PRINT') Then
 
!              Output current graphics to file, prompting for file
!              name if necessary
                 If (data_defined) Then
 
                    Call F2D_PRINT (.True., print_type, .False., &
                      results%max_values, 1, AXIS, YAXIS, results%VECTORS(1, &
                      vec_num), DUMMY, DUMMY, results%TITLES(vec_num), &
                      vec_xlabel, vec_ylabel, vec_zlabel, .False., &
                      results%STARTS(vec_num), 1, results%ENDS(vec_num), 1, &
                      status)
 
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'SET SIZES') Then

                 Call GS_INPI (.True., 10, 10000, .True., &
                   'NUMBER OF RESULTS VECTORS', 1, &
                   'Maximum number of results vectors which can be used', 1, &
                   'Enter a valid integer within given range', &
                   results%max_vectors, status)
                 Call GS_INPI (.True., 10, 10000, .True., &
                   'MAXIMUM NUMBER OF VALUES IN EACH VECTOR', 1, &
                   'Maximum number of values in each vector', 1, &
                   'Enter a valid integer within given range', &
                   results%max_values, status)

!              Check for cancel
                 If (status .Eq. St_escapevalue) Then
                    Call ST_DEF_SYSTEM (status)
                 Else

                    results%num_vectors = 0

                    Deallocate (results%VECTORS)
                    Deallocate (results%STARTS)
                    Deallocate (results%ENDS)
                    Deallocate (results%TITLES)

                    Allocate (results%VECTORS(results%max_values, &
                      results%max_vectors), Stat = stat)
                    If (stat .Ne. 0) Then
                       status = St_mod_ma + St_bad_malloc
                       Call ST_SAVE ('Subroutine F2D_GUI_VECTORS ' // Version)
                       Return
                    End If
                    
                    Allocate (results%STARTS(results%max_vectors), Stat = stat)
                    If (stat .Ne. 0) Then
                       status = St_mod_ma + St_bad_malloc
                       Call ST_SAVE ('Subroutine F2D_GUI_VECTORS ' // Version)
                       Return
                    End If
                    
                    Allocate (results%ENDS(results%max_vectors), Stat = stat)
                    If (stat .Ne. 0) Then
                       status = St_mod_ma + St_bad_malloc
                       Call ST_SAVE ('Subroutine F2D_GUI_VECTORS ' // Version)
                       Return
                    End If

                    Allocate (results%TITLES(results%max_vectors), Stat = stat)
                    If (stat .Ne. 0) Then
                       status = St_mod_ma + St_bad_malloc
                       Call ST_SAVE ('Subroutine F2D_GUI_VECTORS ' // Version)
                       Return
                    End If

!                 Initialise vectors array
                    results%VECTORS = 0.0
                    results%num_vectors = 0
 
!                 Set out of range limits to defined vector ranges
                    Do i = 1, results%max_vectors
                       results%STARTS(i) = results%max_values + 1
                       results%ENDS(i) = 0
                    End Do
 
                 End If

              Else If (command .Eq. 'SET VECTOR') Then

!              Number of vector to use
                 Call F2D_INP_NUMVECTOR (results, vec_num, status)
                 update_image = .True.
                 update_menu = .True.
 
              Else If (command .Eq. 'TEXT OUTPUT') Then
 
                 Call FIO_OUT_VECTORS (input_file, results, status) 
!                   max_vec_values, max_vectors, num_vectors, &
!                   STR_VECTORS, END_VECTORS, VECTORS, VECTOR_TITLES, &
!                   status)
 
                 update_image = .True.
                 update_menu = .True.
 
              Else If (command .Eq. 'TRANSFER') Then

!              Transfer current vector to main data array
                 Call F2D_VECTOR2DATA (results, &
!max_vec_values, max_vectors, &
!                   num_vectors, STR_VECTORS, END_VECTORS, VECTORS, &
!                   VECTOR_TITLES, 
                   AXIS, vec_num, &
                   xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   xstrelm, ystrelm, xendelm, yendelm, DATA, XAXIS, YAXIS, &
                   title, xlabel, ylabel, zlabel, status)
                 data_defined = .True.
 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two points
                 If (data_defined) Then
 
!                 Input zoom region as two coordinates and change display region
                    Call GS_INP_XYREGION (status)
 
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
 
!     Free memory
!        Call IO_FREE (pAXIS, status)
        Deallocate (AXIS)

     End If
 
     End Subroutine F2D_GUI_VECTORS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
