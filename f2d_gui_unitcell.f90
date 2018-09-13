!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_gui_unitcell.f90 *
!  *                      *
!  ************************
 
!+ F2D_GUI_UNITCELL: Reciprocal MAP input UNIT CELL
     Subroutine F2D_GUI_UNITCELL (experiment, status)
!  Description:
!    User input of unit cell parameters
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Mar-2006: V0.2 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    09-Jun-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
!  Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
     Integer, Parameter :: Max_chars = 43 ! Maximum number of characters in
!      a line
     Integer, Parameter :: Max_lines = 13 ! Number of lines in message
     Integer, Parameter :: Max_choices = 11 ! Maximum number of choices:
!      Note: Must be of at least 5 more than 'num_choices' to allow for general
!      form buttons
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: dummy ! Dummy variable, not used
     Integer :: i ! Loop variable for dimensions
     Integer :: num_choices ! Number of choices
     Integer :: retstat ! Return status variable
     Logical :: finished ! .True., when the input is finished O.K.
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 70) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 70) :: PROMPT(2) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
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
     Data TX(   3) / '      Welcome to the FIT2D Unit Cell'/
     Data TX(   4) / '      Parameter Input Form'/
     Data TX(   5) / '      -------------------------------'/
     Data TX(   6) / ' ' /
     Data TX(   7) / 'You can set the individual elements of'/
     Data TX(   8) / 'a general triclinic cell.'/
     Data TX(   9) / ' ' /
     Data TX(  10) / '---------------'/
     Data TX(  11) / 'END OF HELP TEXT'/
     Data TX(  12) / '---------------'/
     Data TX(  13) / ' ' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_UNITCELL ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
 
!     Try to get default values from internal data-base
        Call IO_INQ_RKEYVALUE ('A_UNIT_CELL', experiment%cell_length_a, &
          retstat, status)
        Call IO_INQ_RKEYVALUE ('B_UNIT_CELL', experiment%cell_length_b, &
          retstat, status)
        Call IO_INQ_RKEYVALUE ('C_UNIT_CELL', experiment%cell_length_c, &
          retstat, status)
        Call IO_INQ_RKEYVALUE ('ALPHA_UNIT_CELL', experiment%cell_alpha, &
          retstat, status)
        Call IO_INQ_RKEYVALUE ('BETA_UNIT_CELL', experiment%cell_beta, &
          retstat, status)
        Call IO_INQ_RKEYVALUE ('GAMMA_UNIT_CELL', experiment%cell_gamma, &
          retstat, status)
 
        num_choices = 6
        PROMPT(1) = 'UNIT CELL PARAMETERS'
        PROMPT(2) = 'CONTROL FORM'
 
        BUTTONS(1) = 'a'
        BUTTONS(2) = 'b'
        BUTTONS(3) = 'c'
        BUTTONS(4) = 'alpha'
        BUTTONS(5) = 'beta'
        BUTTONS(6) = 'gamma'
 
        TYPES(1) = Gs_real
        TYPES(2) = Gs_real
        TYPES(3) = Gs_real
        TYPES(4) = Gs_real
        TYPES(5) = Gs_real
        TYPES(6) = Gs_real
 
        TEXT(1) = 'Length (Angstroms) of a'
        TEXT(2) = 'Length (Angstroms) of b'
        TEXT(3) = 'Length (Angstroms) of c'
        TEXT(4) = 'Angle (Degrees) of alpha'
        TEXT(5) = 'Angle (Degrees) of beta'
        TEXT(6) = 'Angle (Degrees) of gamma'
 
        FULL_PROMPTS(1) = 'Enter length in Angstroms of ' // &
          'the a basis vector of the unit cell'
        FULL_PROMPTS(2) = 'Enter length in Angstroms of ' // &
          'the b basis vector of the unit cell'
        FULL_PROMPTS(3) = 'Enter length in Angstroms of ' // &
          'the c basis vector of the unit cell'
        FULL_PROMPTS(4) = 'Enter the angle in degrees of ' // &
          'alpha (between b and c vectors) of the unit cell'
        FULL_PROMPTS(5) = 'Enter the angle in degrees of ' // &
          'beta (between a and c vectors) of the unit cell'
        FULL_PROMPTS(6) = 'Enter the angle in degrees of ' // &
          'gamma (between a and b vectors) of the unit cell'
 
        BOUND(1) = .True.
        BOUND(2) = .True.
        BOUND(3) = .True.
        BOUND(4) = .True.
        BOUND(5) = .True.
        BOUND(6) = .True.
 
        REALS(1) = experiment%cell_length_a
        REALS(2) = experiment%cell_length_b
        REALS(3) = experiment%cell_length_c
        REALS(4) = experiment%cell_alpha
        REALS(5) = experiment%cell_beta
        REALS(6) = experiment%cell_gamma
 
        REALS_LOWER(1) = 0.1
        REALS_UPPER(1) = 10000.0
        REALS_LOWER(2) = 0.1
        REALS_UPPER(2) = 10000.0
        REALS_LOWER(3) = 0.1
        REALS_UPPER(3) = 10000.0
        REALS_LOWER(4) = 0.1
        REALS_UPPER(4) = 180.0
        REALS_LOWER(5) = 0.1
        REALS_UPPER(5) = 180.0
        REALS_LOWER(6) = 0.1
        REALS_UPPER(6) = 180.0
 
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
        experiment%cell_length_a = REALS(1)
        experiment%cell_length_b = REALS(2)
        experiment%cell_length_c = REALS(3)
        experiment%cell_alpha = REALS(4)
        experiment%cell_beta = REALS(5)
        experiment%cell_gamma = REALS(6)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Set default values from internal data-base
        Call IO_SET_RKEYVALUE ('A_UNIT_CELL', experiment%cell_length_a, &
          retstat, status)
        Call IO_SET_RKEYVALUE ('B_UNIT_CELL', experiment%cell_length_b, &
          retstat, status)
        Call IO_SET_RKEYVALUE ('C_UNIT_CELL', experiment%cell_length_c, &
          retstat, status)
        Call IO_SET_RKEYVALUE ('ALPHA_UNIT_CELL', experiment%cell_alpha, &
          retstat, status)
        Call IO_SET_RKEYVALUE ('BETA_UNIT_CELL', experiment%cell_beta, &
          retstat, status)
        Call IO_SET_RKEYVALUE ('GAMMA_UNIT_CELL', experiment%cell_gamma, &
          retstat, status)
 
     End If
 
     End Subroutine F2D_GUI_UNITCELL
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
