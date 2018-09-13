!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_constrain.f90 *
!  *                   *
!  *********************
 
!+ F2D_CONSTRAIN - FIT2D: CONSTRAIN fit parameters
     Subroutine F2D_CONSTRAIN (max_parameters, num_parameters, PARNAMES, &
       CONSTRAINTS, PARAMS, status)
!  Description:
!    Offers menu selection to user so that the user can select certain 
!    parameters to be constrained
!  Method:
!    Allows the various parameters to be constrained if required
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Nov-1996: V0.4 Constraints now defined in a separate logical array 
!      (Hammersley)
!    03-Jan-1996: V0.3 Changes for IBM AIX "xlf" compiler (Hammersley)
!    21-Feb-1993: V0.2 Allow several parameters to be modified at the same time
!      (Hammersley)
!    25-Jan-1993: V0.1 Original, based on "FIT2CON" (Hammersley)
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
     Real, Intent(INOUT) :: PARAMS(max_parameters) ! Parameters values
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
     Integer, Parameter :: Max_menu = 3 ! Number of instructions in the menu
!  Local Variables:
     Character(Len = 80) :: command ! Full command corresponding to user request
     Character(Len = 13) :: constate ! The state of a parameter
     Character(Len = 132) :: format ! Output format statement
     Character(Len = 80) :: subcom ! A command given to the sub-menu
     Integer :: high_par ! Highest parameter in range to be modified
     Integer :: low_par ! Lowest parameter in range to be modified
     Integer :: num_menu ! Number of choices available in the menu
     Integer :: par ! The number of the parameter being considered
     Real :: temppar ! Temporary storage of parameter value
!  Local Arrays:
     Character(Len = 10) :: MENU(Max_menu) ! Array containing menu choices
     Character(Len = 15) :: MENU2(3) ! Sub-menu
     Character(Len = 80) :: MENUTXT(Max_menu) ! Text to explain menu choices
     Character(Len = 80) :: MESSAGE(3) ! Text to be output to the user
!  External Functions:
!  Local Data:
     Data MENU / 'EXIT', 'MODIFY', 'VIEW' /
     Data MENUTXT / 'EXIT - Exit from menu', &
       'MODIFY - Change parameter state', &
       'VIEW - See all parameters and their states' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CONSTRAIN ' // Version)
        Return
     End If
 
!  Initialise values
     num_menu = 3
     command = 'VIEW'
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Start command input/action loop until EXIT requested
     Do While (command .Ne. 'EXIT')
 
!     Get user to select between the available menu options
        Call IO_MENU (.True., 'Contraints sub-menu: enter command', Max_menu, &
          MENUTXT, 1, 'Enter one of the available commands', Max_menu, &
          num_menu, MENU, command, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     EXECUTE MENU CHOICES
 
!     Call subroutine to perform requested operation.
        If (command .Eq. 'MODIFY') Then
 
!        Find out which parameters are to be modified
           low_par = 0
           Call IO_INPI (.True., 0, num_parameters, .True., &
             'LOWEST PARAMETER NUMBER TO MODIFY', 1, &
             'Enter number of lowest parameter to be modified '// &
             '(0 for no modification)', 1, 'Must be within set limits', &
             low_par, status)
 
           If (low_par .Gt. 0) Then
 
              high_par = low_par
              Call IO_INPI (.True., low_par, num_parameters, .True., &
                'HIGHEST PARAMETER NUMBER TO MODIFY', 1, &
                'Enter number of highest parameter to be '// &
                'modified (0 for no modification)', 1, &
                'Must be within set limits', high_par, status)
 
              If (CONSTRAINTS(low_par)) Then
                 constate = 'CONSTRAINED'
                 subcom = 'UNCONSTRAIN'
              Else
                 constate = 'UNCONSTRAINED'
                 subcom = 'CONSTRAIN'
              End If
              temppar = PARAMS(low_par)
 
!           Output details of parameter to the user
              Write (MESSAGE(1), '(''PARAMETER NUMBER '', i3, 1x, a, 1x, a,' &
                // ''' Value = '',g14.5)') low_par, Trim(PARNAMES(low_par)), &
                constate, temppar
              Call IO_WRITE (MESSAGE(1), status)
 
!           Select modification of parameter
              MENU2(1) = 'CONSTRAIN'
              MENU2(2) = 'SET'
              MENU2(3) = 'UNCONSTRAIN'
              MESSAGE(1) = 'CONSTRAIN - Constrain value to a ' &
                //'constant value'
              MESSAGE(2) = 'SET - Set to new value'
              MESSAGE(3) = 'UNCONSTRAIN - Allow value to be varied '
              Call IO_MENU (.True.,'ENTER MODIFICATION', 3, MESSAGE, 1, &
                'Enter one of the avialable choices', 3, 3, MENU2, subcom, &
                status)
 
!           Modify parameter state is required
              If (subcom .Eq. 'CONSTRAIN') Then
 
                 Do par = low_par, high_par
                    CONSTRAINTS(par) = .True.
                 End Do
 
              Else If (subcom .Eq. 'UNCONSTRAIN') Then
 
                 Do par = low_par, high_par
                    CONSTRAINTS(par) = .False.
                 End Do
 
              Else If (subcom .Eq. 'SET') Then
 
!              Prompt for new value if required
                 temppar = PARAMS(low_par)
 
                 Call IO_INPR (.False., 0.0, 0.0, .True., &
                   'ENTER NEW PARAMETER VALUE', 1, &
                   'Enter new value for parameter', 1, 'Enter valid number', &
                   temppar, status)
 
!              Set range of parameter values
                 Do par = low_par, high_par
                    PARAMS(par) = temppar
                 End Do
 
              End If
 
           End If
 
        Else If (command .Eq. 'VIEW') Then
 
!        Range of parameters to view
           low_par = 1
           Call IO_INPI (.True., 1, num_parameters, .True., &
             'LOWEST PARAMETER NUMBER TO VIEW', 1, &
             'Enter number of lowest parameter to view ', 1, &
             'Must be within set limits', low_par, status)
           high_par = num_parameters
           Call IO_INPI (.True., low_par, num_parameters, .True., &
             'HIGHEST PARAMETER NUMBER TO VIEW', 1, &
             'Enter number of highest parameter to view ', 1, &
             'Must be within set limits', high_par, status)
 
!        Output details of parameters to the user
           Do par = low_par, high_par
 
!           Decode the constrained/unconstrained state of the parameter
              If (CONSTRAINTS(par)) Then
                 constate = 'CONSTRAINED'
              Else
                 constate = 'UNCONSTRAINED'
              End If
 
              temppar = PARAMS(par)
 
              format = '(''PARAMETER NUMBER '', i3, 1x, a, 1x, a, ' // &
                ''' Value = '', g14.5)'
              Write (MESSAGE(1), format) par, Trim(PARNAMES(par)), constate, &
                temppar
              Call IO_WRITE (MESSAGE(1), status)
           End Do
 
           command = 'MODIFY'
 
        Else If (command .Eq. 'EXIT') Then
 
!        EXIT : The program will automatically exit from the loop
           Continue
 
        Else
 
!        Unknown command
           Call IO_WRITE ('WARNING: Unknown command, please type again', &
             status)
 
        End If
 
     End Do
 
     End Subroutine F2D_CONSTRAIN
!********1*********2*********3*********4*********5*********6*********7*********8
