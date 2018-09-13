!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_1dconstraints.f90 *
!  *                       *
!  *************************
 
!+ F2D_1DCONSTRAINTS - FIT2D: 1-D fit parameter CONSTRAINTS
     Subroutine F2D_1DCONSTRAINTS (max_parameters, num_parameters, PARNAMES, &
       CONSTRAINTS, PARAMS, status)
!  Description:
!    Graphical menus to constrain or unconstrain parameters
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Jan-1998: V0.2 Declare unused character string for call to "GS_FORM"
!      (Hammersley)
!    15-Dec-1996: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use GS_LIB
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: max_parameters ! The dimension size of the
!      parameter arrays, PARAMS, PARNAMES, and CONSTRAINTS
     Integer, Intent(IN) :: num_parameters ! The total number of parameters
!      in the fit model
     Character(Len = *), Intent(IN) :: PARNAMES(max_parameters)
!      The names given to describe each of the parameters
!  Import/Export:
     Logical, Intent(INOUT) :: CONSTRAINTS(max_parameters) ! Constraints array,
!      .True. if a model parameter is fixed and not to be fitted
     Real, Intent(INOUT) :: PARAMS(max_parameters) ! The values of the
!      parameters
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
     Integer, Parameter :: Max_per_menu = 12 ! Dimension of internal array,
!      and maximum number of parameters to be displayed together
!  Local Variables:
     Integer :: menu ! Loop variable for menus
     Integer :: num_choices ! Number of choices in a menu
     Integer :: num_menus ! Number of menus of values to output
     Integer :: retstat ! Return status variable
     Integer :: par ! Loop variable for parameters
     Integer :: start_par ! First parameter of a menu
!  Local Arrays:
     Character(Len = 6) :: BUTTONS(Max_per_menu + 5) ! Choice buttons
     Character(Len = 50) :: HELP(23) ! User help text on the form
     Character(Len = 50) :: PROMPT(2) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_per_menu + 5) ! Text for form
     Integer :: INTEGERS_UNUSED(1) ! Dummy array for "GS_FORM"
     Integer :: TYPES(Max_per_menu) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
     Logical :: LOGICALS_UNUSED(1) ! Dummy array for "GS_FORM"
     Real :: REALS_UNUSED(1) ! Dummy array for "GS_FORM"
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_1DCONSTRAINTS ' // Version)
        Return
     End If
 
!  Number of menus to display
     num_menus = (num_parameters / Max_per_menu) + 1
 
!  Define form title prompt
     PROMPT(1) = 'FIT PARAMETERS STATES:'
     PROMPT(2) = 'constrained (YES) or unconstrained (NO)'
 
!  Define help text
     HELP(1) = 'This size form allows you to constrain'
     HELP(2) = 'or unconstrain fit model parameters.'
     HELP(3) = ' '
     HELP(4) = 'The "buttons" show the number of the '
     HELP(5) = 'parameter, whilst the text box shows'
     HELP(6) = 'the type of model feature and parameter:'
     HELP(7) = ' '
     HELP(8) = 'GAU = Gaussian, LOR = Lorentzian,'
     HELP(9) = 'VOI = Voigtian peak.'
     HELP(10) = ' '
     HELP(11) = 'POS = Central position, MAX = Maximum'
     HELP(12) = 'peak intensity, SD = Standard deviation'
     HELP(13) = 'width of a Gaussian peak or component, '
     HELP(14) = 'HWHM = Half-Width Half-Maximum of'
     HELP(15) = 'a Lorentzian peak or component.'
     HELP(16) = ' '
     HELP(17) = 'POL = Chebyshev polynomial coefficient,'
     HELP(18) = 'followed by the order of the coefficient.'
 
!  Set all types to be logical
     Do par = 1, Max_per_menu
        TYPES(par) = 2
     End Do
 
     start_par = 1
     Do menu = 1, num_menus
 
        num_choices = Min(Max_per_menu, num_parameters - start_par + 1)
 
!     Create buttons text
        Do par = 1, num_choices
           Write (BUTTONS(par), '(i3)') par + start_par - 1
           TEXT(par) = PARNAMES(par + start_par - 1)
        End Do
 
!     Output interactive graphical form
        Call GS_FORM (2, 2, PROMPT, 18, 18, HELP, Max_per_menu + 5, &
          num_choices, BUTTONS, TYPES, TEXT, TEXT, LOGICALS_UNUSED, &
          INTEGERS_UNUSED, INTEGERS_UNUSED, REALS_UNUSED, REALS_UNUSED, &
          INTEGERS_UNUSED, CONSTRAINTS(start_par), REALS_UNUSED, STRINGS, &
          retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           Return
        End If
 
        start_par = start_par + Max_per_menu
 
     End Do
 
     End Subroutine F2D_1DCONSTRAINTS
!********1*********2*********3*********4*********5*********6*********7*********8
