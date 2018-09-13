!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_mfitsetup.f90 *
!  *                   *
!  *********************
 
!+ F2D_MFITSETUP: MFIT fitting command SET-UP form
     Subroutine F2D_MFITSETUP (variances_exist, alpha, fitting_info, &
       itsperpar, display_frequency, weighted_fit, model_evolution, status)
!  Description:
!    Graphical form to allow control of variables
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Oct-1998: V0.4 Add control of "model_evolution" variable (Hammersley)
!    30-Jan-1998: V0.3 Declare unused character string for call to "GS_FORM" 
!      (Hammersley)
!    17-Dec-1996: V0.2 Add "INFORMATION LEVEL" control (Hammersley)
!    15-Dec-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: variances_exist ! .True., if variances exist
!  Import/Export:
     Real, Intent(INOUT) :: alpha ! Scaling factor for required accuracy
     Integer, Intent(INOUT) :: fitting_info ! Level of user information output:
!      0: no information
!      >= 1" output iteration numbers
!      >= 2: output sum of squared residuals
     Real, Intent(INOUT) :: itsperpar ! Number of iterations per parameter
!      required to minimise function
     Integer, Intent(INOUT) :: display_frequency ! Frequency with which to
!      display fitted model results
     Logical, Intent(INOUT) :: weighted_fit ! .True., if weighted fitting is
!      required
     Logical, Intent(INOUT) :: model_evolution ! .True., if the fitting model 
!      parameters "evolve" during a multiple row fit, or always start from the
!      initialisation values
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
     Integer, Parameter :: Max_choices = 11 ! Maximum number of choices
!  Local Variables:
     Integer :: dummy ! Dummy variable, not used
     Integer :: num_choices ! Number of choices
     Integer :: retstat ! Return status variable for "GS_FORM"
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 80) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 70) :: HELP(23) ! User help text on the form
     Character(Len = 70) :: PROMPT(3) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Integer :: INTS_LOWER(Max_choices) ! Lower bounds on integer input
     Integer :: INTS_UPPER(Max_choices) ! Upper bounds on integer input
     Integer :: INTEGERS(Max_choices) ! Integer variables to be input
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
     Logical :: BOUND(Max_choices) ! .True., if variable range is bound
     Logical :: LOGICALS(Max_choices) ! Logical variables to be input
     Real :: REALS(Max_choices) ! Real variables to be input
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MFITSETUP ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
 
        If (variances_exist) Then
           num_choices = 5
        Else
           num_choices = 4
        End If
 
        PROMPT(1) = 'FITTING CONTROL PARAMETERS'
        HELP(1) = 'This size form allows you to adjust aspects '
        HELP(2) = 'of the fit and graphical display of'
        HELP(3) = 'results.'
        BUTTONS(1) = 'INFO. LEVEL'
        BUTTONS(2) = 'ITER / PAR'
        BUTTONS(3) = 'DIS. FREQ.'
        BUTTONS(4) = 'EVOLUTION'
        BUTTONS(5) = 'WEIGHTED'
        TYPES(1) = 1
        TYPES(2) = 3
        TYPES(3) = 1
        TYPES(4) = 2
        TYPES(5) = 2
        TEXT(1) = 'LEVEL OF FITTING INFORMATION'
        TEXT(2) = 'No. OF ITERATIONS PER PARAMETER'
        TEXT(3) = 'DISPLAY FREQUENCY OF RESULTS'
        TEXT(4) = 'ALLOW FIT PARAMETER VALUES TO EVOLVE'
        TEXT(5) = 'WEIGHTED FITTING'
        FULL_PROMPTS(1) = 'Fitting information level: ' // &
          '0 = minimum, 2 = maximum'
        FULL_PROMPTS(2) = 'Enter maximum number of iterations ' // &
          'per fit parameter'
        FULL_PROMPTS(3) = 'Enter number of rows bewteen ' // &
          'graphical display of fitted results'
        FULL_PROMPTS(4) = 'Yes to allow the fitted values of' // &
          'one fit to be used for the next'
        FULL_PROMPTS(5) = 'Select fitted weighted by variance' // &
          'exists or not'
        BOUND(1) = .True.
        BOUND(2) = .False.
        BOUND(3) = .False.
        BOUND(4) = .False.
        BOUND(4) = .False.
        INTEGERS(1) = fitting_info
        REALS(2) = itsperpar
        INTEGERS(3) = display_frequency
        LOGICALS(4) = model_evolution
        LOGICALS(5) = weighted_fit
        INTS_LOWER(1) = 0
        INTS_UPPER(1) = 2
 
!     Output interactive graphical form
        Call GS_FORM (1, 1, PROMPT, 3, 3, HELP, Max_choices, num_choices, &
          BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, INTS_LOWER, INTS_UPPER, &
          dummy, dummy, INTEGERS, LOGICALS, REALS, STRINGS, retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           status = St_escapevalue
           Return
        End If
 
!     Set resulting values
        fitting_info = INTEGERS(1)
        itsperpar = REALS(2)
        display_frequency = INTEGERS(3)
        model_evolution = LOGICALS(4)
        If (variances_exist) Then
           weighted_fit = LOGICALS(5)
        End If
 
     End If
 
     End Subroutine F2D_MFITSETUP
!********1*********2*********3*********4*********5*********6*********7*********8
