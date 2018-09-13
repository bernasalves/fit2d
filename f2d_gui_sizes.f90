!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_gui_sizes.f90 *
!  *                   *
!  *********************
 
!+ F2D_GUI_SIZES: GUI SIZES of data FIT2D arrays file formats
     Subroutine F2D_GUI_SIZES (xmaxdat, ymaxdat, memory_exist, &
       variance_arrays, status)
!  Description:
!    Sizes of data arrays and existence or not of memory and variance arrays.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    20-Sep-1999: V0.5 Increase maximum size of X-dimension (Hammersley)
!    30-Jan-1998: V0.4 Declare unused character string for call to "GS_FORM" 
!      (Hammersley)
!    08-Mar-1996: V0.3 Recognise cancel button from form (Hammersley)
!    07-Feb-1996: V0.2 Add extra to "BUTTONS" array (Hammersley)
!    20-Jan-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
     Integer, Intent(INOUT) :: xmaxdat ! First dimension of array
     Integer, Intent(INOUT) :: ymaxdat ! Second dimension of array
     Logical, Intent(INOUT) :: memory_exist ! .True., if memory array are to
!      be created
     Logical, Intent(INOUT) :: variance_arrays ! .True., if variance arrays
!      are to be created
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
     Integer, Parameter :: Max_choices = 9 ! Maximum number of choices
!  Local Variables:
     Integer :: dummy ! Dumy variable, not used
     Integer :: num_choices ! Number of choices
     Integer :: retstat ! Return status variable for "GS_FORM"
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 60) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 70) :: HELP(23) ! User help text on the form
     Character(Len = 70) :: PROMPT(3) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Integer :: INTEGERS(Max_choices) ! Integer variables to be input
     Integer :: INTS_LOWER(Max_choices) ! Lower bound of integer variables
     Integer :: INTS_UPPER(Max_choices) ! Upper bound of integer variables
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
     Logical :: BOUND(Max_choices) ! .True., if variable range is bound
     Logical :: LOGICALS(Max_choices) ! Logical variables to be input
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_SIZES ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
 
        PROMPT(1) = 'DIMENSIONS OF PROGRAM ARRAYS'
        PROMPT(2) = '(need to be big enough to store'
        PROMPT(3) = 'and work on data)'
        HELP(1) = 'This size form allows you  to select the ' // &
          'sizes of internal'
        HELP(2) = 'program arrays for storing 2-D detector ' // &
          'and  other data.'
        HELP(3) = 'Normally you will want the arrays to be ' // &
          'at least as large'
        HELP(4) = 'as your data, but it is possible to require ' // &
          'arrays which'
        HELP(5) = 'are bigger, and it is also possible to work ' // &
          'with arrays'
        HELP(6) = 'which are smaller by only inputting a ' // &
          'sub-region of the'
        HELP(7) = 'data, or by re-binning the data on input.'
        HELP(8) = ' '
        HELP(9) = 'The "X-DIMENSION" and "Y-DIMENSION" buttons '
        HELP(10) = 'allow these number of pixels to be changed.'
        HELP(11) = ' '
        HELP(12) = 'Normally memory arrays should be created ' // &
          'as they are'
        HELP(13) = 'necessary for many operations, however if ' // &
          'you need to'
        HELP(14) = 'limit the computer memory usage, they do not ' // &
          'need to be'
        HELP(15) = 'created for simple display.'
        HELP(16) = ' '
        HELP(17) = 'The "MEMORY" button controls the creation ' // &
          'of the memory'
        HELP(18) = 'arrays, or not.'
        HELP(19) = ' '
        HELP(20) = 'Normally variance (error) propagation ' // &
          'is not necessary.'
        HELP(21) = 'If  it  is  required this option should be ' // &
          'selected,  but  the'
        HELP(22) = 'requirements for computer memory for ' // &
          'array space will '
        HELP(23) = 'double and mathematical operations will take ' // &
          'twice as long.'
        num_choices = 4
        BUTTONS(1) = 'X-DIMENSION'
        BUTTONS(2) = 'Y-DIMENSION'
        BUTTONS(3) = 'MEMORY'
        BUTTONS(4) = 'VARIANCES'
        TYPES(1) = 1
        TYPES(2) = 1
        TYPES(3) = 2
        TYPES(4) = 2
        TEXT(1) = 'FIRST DIMENSION OF ARRAYS'
        TEXT(2) = 'SECOND DIMENSION OF ARRAYS'
        TEXT(3) = 'CREATE MEMORY ARRAYS'
        TEXT(4) = 'CREATE VARIANCE ARRAYS'
        FULL_PROMPTS(1) = 'Enter number of pixels "horizontally" ' // &
          'for arrays'
        FULL_PROMPTS(2) = 'Enter number of pixels "vertically" ' // &
          'for arrays'
        FULL_PROMPTS(3) = 'Enter "YES" to be able to perform ' // &
          'maths, etc.'
        FULL_PROMPTS(4) = 'Enter "YES" for error propagation (slower)'
        BOUND(1) = .True.
        BOUND(2) = .True.
        BOUND(3) = .False.
        BOUND(4) = .False.
        INTS_LOWER(1) = 1
        INTS_LOWER(2) = 1
        INTS_UPPER(1) = 100000
        INTS_UPPER(2) = 100000
        INTEGERS(1) = xmaxdat
        INTEGERS(2) = ymaxdat
        LOGICALS(3) = memory_exist
        LOGICALS(4) = variance_arrays
 
!     Output interactive graphical form
        Call GS_FORM (3, 3, PROMPT, 23, 23, HELP, Max_choices, num_choices, &
          BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, INTS_LOWER, INTS_UPPER, &
          dummy, dummy, INTEGERS, LOGICALS, dummy, STRINGS, retstat, status)
 
!     Check return status
        If (retstat .Eq. -1) Then
 
!        The cancel button was pressed
           status = St_escapevalue
 
        Else
 
!        Set resulting values
           xmaxdat = INTEGERS(1)
           ymaxdat = INTEGERS(2)
           memory_exist = LOGICALS(3)
           variance_arrays = LOGICALS(4)
 
        End If
 
     End If


!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''retstat = '', i12)') retstat
!     Write (*, '(''status = '', i12)') status
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_GUI_SIZES
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
