!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************
!  *               *
!  * f2d_scale.f90 *
!  *               *
!  *****************
 
!+ F2D_SCALE - FIT2D: SCALE size settings
     Subroutine F2D_SCALE (max_parameters, num_parameters, PARNAMES, PARAMS, &
       SCALE_FACTORS, status)
!  Description:
!    Offers menu selection to user so that the user can select
!    certain parameters scale sizes to reset
!  Keywords:
!    Scales.Fit~Parameters, Fit~Parameters.Scales
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Nov-1996: V0.3 Change variable names to be consistent with
!      "FIT" sub-menu (Hammersley)
!    04-Jan-1996: V0.2 Changes for IBM AIX "xlf" compiler: Doesn't like "g" 
!      format descriptors without width specifiers (Hammersley)
!    04-Feb-1993: V0.1 Original, based on "FIT2SCALE" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: max_parameters ! The dimension size of the
!      parameter arrays, "PARAMS", "PARNAMES", and "SCALE_FACTORS"
     Integer, Intent(IN) :: num_parameters ! The total number of parameters
!      in the fit model
     Character(Len = *), Intent(IN) :: PARNAMES(max_parameters)
!      The names given to describe each of the parameters
     Real, Intent(IN) :: PARAMS(max_parameters) ! The values of the parameters
!  Import/Export:
     Real, Intent(INOUT) :: SCALE_FACTORS(max_parameters) ! The typical
!      expected variation in the parameter values
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Integer, Parameter :: Maxinstr = 3 ! Number of instructions in the menu
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Character(Len = 80) :: command ! Full command corresponding to user request
     Character(Len = 132) :: format ! Format string for I/O statements
     Integer :: numinstr ! Number of choices available in the menu
     Integer :: par ! The number of the parameter being considered
!  Local Arrays:
     Character(Len = 80) :: ERROR(10) ! Buffer for error messages
     Character(Len = 10) :: MENU(Maxinstr) ! Array containing menu choices
     Character(Len = 80) :: MENUTXT(Maxinstr) ! Text to explain menu choices
     Character(Len = 80) :: MESSAGE(10) ! Text to be output to the user
!  External Functions:
     Integer, External :: Ma_exdigit ! Extract digit from an integer number
!  Local Data:
     Data MENU / 'EXIT', 'MODIFY', 'VIEW' /
     Data MENUTXT / 'EXIT - Exit from menu', &
       'MODIFY - Change parameter state', &
       'VIEW - See all parameters and their states' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SCALE ' // Version)
        Return
     End If
 
!  Initialise values
     numinstr = 3
     command = 'VIEW'
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Start command input/action loop until EXIT requested
     Do While (command .Ne. 'EXIT')
 
!     Get user to select between the available menu options
        Call IO_MENU (.True., 'ENTER COMMAND', Maxinstr, MENUTXT, 1, ERROR, &
          Maxinstr, numinstr, MENU, command, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     EXECUTE MENU CHOICES
 
!     Call subroutine to perform requested operation.
        If (command .Eq. 'MODIFY') Then
 
!        Find out which parameter is to be modified
           par = 0
           MESSAGE(1) = 'Enter number of parameter to be modified,'
           MESSAGE(2) = '(0 if no parameter is to be modified)'
           Write (ERROR,'(''Must be within set bounds'')')
           Call IO_INPI (.True., 0, num_parameters, .True., &
             'PARAMETER NUMBER', 2, MESSAGE, 1, ERROR, par, status)
 
!        Output details of parameter to the user
           format = '(''PARAMETER NUMBER '', i3, 1x, a, ' // &
             ''' Value = '', g12.5, ''Scale = '', g12.5)'
           Write (MESSAGE, format) par, Trim(PARNAMES(par)), PARAMS(par), &
             SCALE_FACTORS(par)
           Call IO_WRITE (MESSAGE(1), status)
 
           Write (MESSAGE,'(''Enter new scale size for parameter'')')
           Write (ERROR,'(''Please give an acceptable input'')')
           Call IO_INPR (.False., 0.0, 0.0, .True., 'NEW SCALE SIZE', 1, &
             MESSAGE, 1, ERROR, SCALE_FACTORS(par), status)
 
        Else If (command .Eq. 'VIEW') Then
 
!        Output details of parameters to the user
           Do par = 1, num_parameters
              format = '(''PARAMETER NUMBER '', i3, 1x, a, ' // &
                ''' Value = '', g12.5, ''Scale = '', g12.5)'
              Write (MESSAGE, format) par, Trim(PARNAMES(par)), PARAMS(par), &
                SCALE_FACTORS(par)
              Call IO_WRITE (MESSAGE(1), status)
           End Do
 
        Else If (command .Eq. 'EXIT') Then
 
!        EXIT : The program will automatically exit from the loop
           Continue
 
        Else
 
!        Unknown command.
           Write (MESSAGE,'(''ILLEGAL COMMAND, PLEASE RE-ENTER'')')
           Call IO_WRITE ('WARNING: Unknown command, please re-enter', status)
 
        End If
 
!     Check value of status
        Call ST_OUT (status)
 
     End Do
 
     End Subroutine F2D_SCALE
!********1*********2*********3*********4*********5*********6*********7*********8
