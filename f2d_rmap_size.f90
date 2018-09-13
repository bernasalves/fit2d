!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_rmap_size.f90 *
!  *                   *
!  *********************
 
!+ F2D_RMAP_SIZE: Reciprocal MAP SIZE form
     Subroutine F2D_RMAP_SIZE (xmaxmap, ymaxmap, zmaxmap, retstat, status)
!  Description:
!    User input of reciprocal map size
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    20-Mar-2006: V0.2 Change "MAP" and "NORMALISE" to allocatable arrays
!      (Hammersley)
!    19-Apr-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     Integer, Intent(INOUT) :: xmaxmap ! First dimension of reciprocal map
     Integer, Intent(INOUT) :: ymaxmap ! Second dimension of reciprocal map
     Integer, Intent(INOUT) :: zmaxmap ! Third dimension of reciprocal map
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status variable:
!      0 = Good status
!      -1 = Failed
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
     Integer, Parameter :: Max_chars =  43 ! Maximum number of characters
!      in a line
     Integer, Parameter :: Max_lines =   15 ! Number of lines in message
     Integer, Parameter :: Max_choices = 8 ! Maximum number of choices:
!      Note: Must of at least 5 more than "num_choices" to allow for general
!      form buttons
!  Local Variables:
     Integer :: dummy ! Dummy variable, not used
     Integer :: num_choices ! Number of choices
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 60) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 70) :: PROMPT(2) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = Max_chars) :: TX(Max_lines) ! Text array
     Integer :: INTEGERS(Max_choices) ! Integer variables to be input
     Integer :: INTEGERS_LOWER(Max_choices) ! Lower bound of integer
!      variables
     Integer :: INTEGERS_UPPER(Max_choices) ! Upper bound of integer
!      variables
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
     Logical :: BOUND(Max_choices) ! .True., if variable range is bound
!  External Functions:
!  Local Data:
     Data TX(   1) / ' ' /
     Data TX(   2) / '      -------------------------------'/
     Data TX(   3) / '      Welcome to the FIT2D Reciprocal'/
     Data TX(   4) / '             Map size form'/
     Data TX(   5) / '      -------------------------------'/
     Data TX(   6) / ' ' /
     Data TX(   7) / 'You can set the size of the map to produce,'/
     Data TX(   8) / 'but there must be enough virtual memory to'/
     Data TX(   9) / 'produce the map and a normalising array of'/
     Data TX(  10) / 'the same size.'/
     Data TX(  11) / ' ' /
     Data TX(  12) / '---------------'/
     Data TX(  13) / 'END OF HELP TEXT'/
     Data TX(  14) / '---------------'/
     Data TX(  15) / ' ' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_SIZE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_RMAP_SIZE'')')
!***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Try to get default values from internal data-base
        Call IO_INQ_IKEYVALUE ('RMAP_X_DIMENSION', xmaxmap, retstat, status)
        Call IO_INQ_IKEYVALUE ('RMAP_Y_DIMENSION', ymaxmap, retstat, status)
        Call IO_INQ_IKEYVALUE ('RMAP_Z_DIMENSION', zmaxmap, retstat, status)
 
        num_choices = 3
        PROMPT(1) = 'RECIPROCAL MAP SIZE'
        PROMPT(2) = 'CONTROL FORM'
 
        BUTTONS(1) = 'NUMBER PIXELS X'
        BUTTONS(2) = 'NUMBER PIXELS Y'
        BUTTONS(3) = 'NUMBER PIXELS Z'
 
        TYPES(1) = Gs_integer
        TYPES(2) = Gs_integer
        TYPES(3) = Gs_integer
 
        TEXT(1) = 'NUMBER OF PIXELS FIRST DIMENSION'
        TEXT(2) = 'NUMBER OF PIXELS SECOND DIMENSION'
        TEXT(3) = 'NUMBER OF PIXELS THIRD DIMENSION'
 
        FULL_PROMPTS(1) = 'Enter number of pixels ' // &
          'for first dimension of the reciprocal map'
        FULL_PROMPTS(2) = 'Enter number of pixels ' // &
          'for second dimension of the reciprocal map'
        FULL_PROMPTS(3) = 'Enter number of pixels ' // &
          'for third dimension of the reciprocal map'
 
        BOUND(1) = .True.
        BOUND(2) = .True.
        BOUND(3) = .True.
 
        INTEGERS_LOWER(1) = 2
        INTEGERS_UPPER(1) = 10000
        INTEGERS_LOWER(2) = 2
        INTEGERS_UPPER(2) = 10000
        INTEGERS_LOWER(3) = 2
        INTEGERS_UPPER(3) = 10000
        
        INTEGERS(1) = xmaxmap
        INTEGERS(2) = ymaxmap
        INTEGERS(3) = zmaxmap
 
!     Output interactive graphical form
        Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, Max_choices, &
          num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, &
          INTEGERS_LOWER, INTEGERS_UPPER, dummy, dummy, INTEGERS, dummy, &
          dummy, STRINGS, retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           Return
        End If
 
!     Set resulting values
        xmaxmap = INTEGERS(1)
        ymaxmap = INTEGERS(2)
        zmaxmap = INTEGERS(3)
 
!     Save values in internal data-base
        Call IO_SET_IKEYVALUE ('RMAP_X_DIMENSION', xmaxmap, retstat, status)
        Call IO_SET_IKEYVALUE ('RMAP_Y_DIMENSION', ymaxmap, retstat, status)
        Call IO_SET_IKEYVALUE ('RMAP_Z_DIMENSION', zmaxmap, retstat, status)
 
     End If
 
     End Subroutine F2D_RMAP_SIZE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
