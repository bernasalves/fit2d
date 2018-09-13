!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_rmap_ubmatrix.f90 *
!  *                       *
!  *************************
 
!+ F2D_RMAP_UBMATRIX: Reciprocal MAP input UB MATRIX
     Subroutine F2D_RMAP_UBMATRIX (UB, status)
!  Description:
!    User input of UB matrix elements
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    27-Apr-2006: V0.4 Remove inquire from internal data-base of UB-matrix 
!      values (now loaded at start-up) (Hammersley)
!    27-Oct-2005: V0.3 Remove calculation of inverse UB matrix to calling
!      routine (Hammersley)
!    13-Oct-2005: V0.2 UB matrix values save by calling rountine now 
!      (Hammersley)
!    21-Apr-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: UB (3, 3) ! UB matrix of crystal orientation
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
     Integer, Parameter :: Max_chars = 43 ! Maximum number of characters in
!      a line
     Integer, Parameter :: Max_lines = 13 ! Number of lines in message
     Integer, Parameter :: Max_choices = 14 ! Maximum number of choices:
!      Note: Must of at least 5 more than 'num_choices' to allow for general
!      form buttons
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: dummy ! Dummy variable, not used
     Integer :: i ! Loop variable for dimensions
     Integer :: num_choices ! Number of choices
     Integer :: retstat ! Return status variable
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 60) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 70) :: PROMPT(2) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to 'GS_FORM'
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = Max_chars) :: TX(Max_lines) ! Text array
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
     Logical :: BOUND(Max_choices) ! .True., if variable range is bound
     Real :: REALS(Max_choices) ! Real variables to be input
     Real :: REALS_LOWER(Max_choices) ! Lower bound of real variables
     Real :: REALS_UPPER(Max_choices) ! Upper bound of real variables
!  External Functions:
!  Local Data:
     Data TX(   1) / ' ' /
     Data TX(   2) / '      -------------------------------'/
     Data TX(   3) / '      Welcome to the FIT2D Reciprocal'/
     Data TX(   4) / '      Map UB Matrix Input'/
     Data TX(   5) / '      -------------------------------'/
     Data TX(   6) / ' ' /
     Data TX(   7) / 'You can set the individual elements of'/
     Data TX(   8) / 'the UB matrix.'/
     Data TX(   9) / ' ' /
     Data TX(  10) / '---------------'/
     Data TX(  11) / 'END OF HELP TEXT'/
     Data TX(  12) / '---------------'/
     Data TX(  13) / ' ' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_UBMATRIX ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
 
        num_choices = 9
        PROMPT(1) = 'UB MATRIX ELEMENTS'
        PROMPT(2) = 'CONTROL FORM'
 
        BUTTONS(1) = 'UB(1, 1)'
        BUTTONS(2) = 'UB(1, 2)'
        BUTTONS(3) = 'UB(1, 3)'
        BUTTONS(4) = 'UB(2, 1)'
        BUTTONS(5) = 'UB(2, 2)'
        BUTTONS(6) = 'UB(2, 3)'
        BUTTONS(7) = 'UB(3, 1)'
        BUTTONS(8) = 'UB(3, 2)'
        BUTTONS(9) = 'UB(3, 3)'
 
        TYPES(1) = Gs_real
        TYPES(2) = Gs_real
        TYPES(3) = Gs_real
        TYPES(4) = Gs_real
        TYPES(5) = Gs_real
        TYPES(6) = Gs_real
        TYPES(7) = Gs_real
        TYPES(8) = Gs_real
        TYPES(9) = Gs_real
 
        TEXT(1) = 'Element (1, 1) of UB matrix'
        TEXT(2) = 'Element (1, 2) of UB matrix'
        TEXT(3) = 'Element (1, 3) of UB matrix'
        TEXT(4) = 'Element (2, 1) of UB matrix'
        TEXT(5) = 'Element (2, 2) of UB matrix'
        TEXT(6) = 'Element (2, 3) of UB matrix'
        TEXT(7) = 'Element (3, 1) of UB matrix'
        TEXT(8) = 'Element (3, 2) of UB matrix'
        TEXT(9) = 'Element (3, 3) of UB matrix'
 
        FULL_PROMPTS(1) = 'Enter element (1, 1) of UB matrix'
        FULL_PROMPTS(2) = 'Enter element (1, 2) of UB matrix'
        FULL_PROMPTS(3) = 'Enter element (1, 3) of UB matrix'
        FULL_PROMPTS(4) = 'Enter element (2, 1) of UB matrix'
        FULL_PROMPTS(5) = 'Enter element (2, 2) of UB matrix'
        FULL_PROMPTS(6) = 'Enter element (2, 3) of UB matrix'
        FULL_PROMPTS(7) = 'Enter element (3, 1) of UB matrix'
        FULL_PROMPTS(8) = 'Enter element (3, 2) of UB matrix'
        FULL_PROMPTS(9) = 'Enter element (3, 3) of UB matrix'
 
        BOUND(1) = .False.
        BOUND(2) = .False.
        BOUND(3) = .False.
        BOUND(4) = .False.
        BOUND(5) = .False.
        BOUND(6) = .False.
        BOUND(7) = .False.
        BOUND(8) = .False.
        BOUND(9) = .False.
 
        REALS(1) = UB(1, 1)
        REALS(2) = UB(1, 2)
        REALS(3) = UB(1, 3)
        REALS(4) = UB(2, 1)
        REALS(5) = UB(2, 2)
        REALS(6) = UB(2, 3)
        REALS(7) = UB(3, 1)
        REALS(8) = UB(3, 2)
        REALS(9) = UB(3, 3)
 
!     Output interactive graphical form
        Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, Max_choices, &
          num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, dummy, &
          dummy, REALS_LOWER, REALS_UPPER, dummy, dummy, REALS, STRINGS, &
          retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           status = St_escapevalue
           Return
        End If
 
!     Set resulting values
        UB(1, 1) = REALS(1)
        UB(1, 2) = REALS(2)
        UB(1, 3) = REALS(3)
        UB(2, 1) = REALS(4)
        UB(2, 2) = REALS(5)
        UB(2, 3) = REALS(6)
        UB(3, 1) = REALS(7)
        UB(3, 2) = REALS(8)
        UB(3, 3) = REALS(9)
 
     End If
 
     End Subroutine F2D_RMAP_UBMATRIX
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
