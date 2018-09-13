!********1*********2*********3*********4*********5*********6*********7**
 
!  ***************************
!  *                         *
!  * f2d_rmap_inubmatrix.f90 *
!  *                         *
!  ***************************
 
!+ F2D_RMAP_INUBMATRIX: Reciprocal MAP INput UB MATRIX
     Subroutine F2D_RMAP_INUBMATRIX (INPUT_OPTIONS, xmaxdat, ymaxdat, &
       xnumdat, ynumdat, XAXIS, YAXIS, DATA, title, xlabel, ylabel, zlabel, &
       experiment, retstat, status)
!  Description:
!    Choice of input methods for UB matrix elements
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    24-Apr-2006: V0.9 Add "INPUT_OPTIONS" structure (Hammersley)
!    14-Mar-2006: V0.8 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    14-Nov-2005: V0.7 Add option to input from an ASCII file (Hammersley)
!    14-Oct-2005: V0.6 Add experiment geometry parameters (Hammersley)
!    13-Oct-2005: V0.5 Save values in internal data-base (Hammersley)
!    05-Oct-2005: V0.4 Changes to argument list (Hammersley)
!    06-Sep-2005: V0.3 Choice of clicking on diffraction images (Hammersley)
!    10-Jun-2005: V0.2 Continue implementation (Hammersley)
!    09-Jun-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     TYPE(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: INPUT_OPTIONS ! Control of 
!      input of auxiliary experimental information (see "io.inc")
     Integer, Intent(IN) :: xmaxdat ! First dimension of array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array
     Integer, Intent(IN) :: xnumdat ! Defines X-extent of defined data
     Integer, Intent(IN) :: ynumdat ! Defines Y-extent of defined data
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Array to contain X-coordinate grid 
!      data
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Array to contain Y-coordinate grid 
!      data
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data values
     Character(Len = *), Intent(INOUT) :: title ! Title for data
     Character(Len = *), Intent(INOUT) :: xlabel ! Label for X-axis of data
     Character(Len = *), Intent(INOUT) :: ylabel ! Label for Y-axis of data
     Character(Len = *), Intent(INOUT) :: zlabel ! Label for Z-axis of data
!  Import/Export:
     TYPE(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
     Integer, Intent(OUT) :: retstat ! Status return:
!      0 = Good status
!      1 = Cancel used
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
     Integer, Parameter :: Max_menu = 6 ! Dimension size of menu
!  Local Variables:
     Character(Len = 47) :: command ! User entered command
     Character(Len = 256) :: file_name ! Name of user selected file
     Integer :: item ! Loop variable
     Integer :: input_type ! Type of graphical input
     Integer :: num_menu ! Number of menu items
     Integer :: numdat ! Number of input values
     Logical :: continue ! .True., until the menu is exited
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 47), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanations
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, Max_menu) / 'CANCEL', '?', &
       'ENTER UB MATRIX ELEMENT VALUES', &
       'INPUT UB MATRIX ELEMENT FROM TEXT FILE', &
       'UNIT CELL; ANGLES AND INDICES OF TWO REFECTIONS', &
       'UNIT CELL; CLICK ON IMAGES WITH TWO REFECTIONS' /
     Data (MENUTXT(item), item = 1, Max_menu) / &
       'CANCEL: Exit without calculating UB matrix', &
       '?: List of available formats and commands', &
       'ENTER UB ...: Form input of individual matrix element values', &
       'TEXT FILE: Input 9 co-efficients from an ASCII text file', &
       'UNIT CELL; ANGLES ...: Input of unit cell and two refections', &
       'UNIT CELL; CLICK ...: Input from diffraction images' /
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_INUBMATRIX ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Clear file selection graphics
        Call GS_BACKGROUND (status)
 
!     Choose method for defining UB matrix
        continue = .True.
        update_menu = .True.
        Do While (continue)
 
           If (update_menu) Then
 
!           Set menu layout style
              num_menu = Max_menu
              Call GS_SET_MENULAYOUT (gs_horizontal, 1, 30, 30, status)
 
!           Draw menu
              Call GS_FMENU (1, 1, 'UB MATRIX INPUT METHOD:', Max_menu, &
                num_menu, MENU, status)
 
           End If
 
!        By default no update and exit menu
           update_menu = .False.
           continue = .False.
 
!        Get user to select between the available menu options
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 1, input_type, &
             command, x_coordinate, y_coordinate,status)
 
           If (input_type .Eq. Gs_resize) Then
 
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_choice) Then
 
!           Carry out menu choices
              If (command .Eq. 'CANCEL') Then
 
!              Exit input routine
                 retstat = 1
                 Return
 
              Else If (command .Eq. '?') Then
 
!              Format menu choices
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
                 continue = .True.
                 update_menu = .True.
 
              Else
                 continue = .False.
              End If
 
!           Check status
              If (status .Eq. St_escapevalue) Then
 
!              Re-set status system
                 Call ST_DEF_SYSTEM (status)
 
              Else If (status .Ne. St_goodvalue) Then
                 continue = .False.
              End If
 
           End If
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input UB matrix according to user chosen method
        If (command .Eq. 'ENTER UB MATRIX ELEMENT VALUES') Then
 
           Call F2D_RMAP_UBMATRIX (experiment%UB_MATRIX, status)
 
        Else If (command .Eq. 'INPUT UB MATRIX ELEMENT FROM TEXT FILE') Then
 
!        Select file for input of UB matrix
           Call GS_FILESELECTION (1, 1, &
             'SELECT FILE CONTAINING UB MATRIX ELEMENTS', 1, 1, &
             'Select ASCII text file with 9 UB matrix elements', 1, .False., &
             retstat, file_name, status)
 
           If (retstat .Eq. 0) Then
 
!           Input 9 element values from a text file
              Call FIO_IN_1DASCII (file_name, 9, retstat, numdat, &
                experiment%UB_MATRIX, status)
 
              If (retstat .Ne. 0 .Or. numdat .Lt. 9) Then
 
                 Call GS_FWARNING (1, 1, &
                   'The UB matrix has not been input correctly', status)

              End If
 
           End If
 
        Else If (command .Eq. &
          'UNIT CELL; ANGLES AND INDICES OF TWO REFECTIONS') Then
 
!        Calculate UB matrix from unit cell and two reflections
           Call F2D_RMAP_CAL_UBMATRIX (experiment, status)
 
        Else If (command .Eq. &
          'UNIT CELL; CLICK ON IMAGES WITH TWO REFECTIONS') Then
 
           Call F2D_RMAP_IN_REFLECTIONS (INPUT_OPTIONS, xmaxdat, ymaxdat, &
             xnumdat, ynumdat, XAXIS, YAXIS, DATA, &
             title, xlabel, ylabel, zlabel, experiment, retstat, status)
 
        End If
 
!     Calculate inverse of UB matrix
        Call MA_MATINVERSE (3, 3, experiment%UB_MATRIX, 3, retstat, &
          experiment%INV_UB, status)
 
        If (retstat .Ne. 0) Then
           Call GS_FWARNING (1, 1, 'The UB matrix could not ' // &
             'be inverted; re-enter values', status)
        Else
           experiment%ub_matrix_set = .True.
        End If
 
     End If
 
     End Subroutine F2D_RMAP_INUBMATRIX
!********1*********2*********3*********4*********5*********6*********7**
 
 
