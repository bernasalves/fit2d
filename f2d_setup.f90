!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************
!  *               *
!  * f2d_setup.f90 *
!  *               *
!  *****************
 
!+ F2D_SETUP - FIT 2-D:  SET UP parameters
     Subroutine F2D_SETUP (poisson, mlm, weight, alpha, itsperpar, evolve, &
       disfreq, contint, reverse, fastdis, haltcrit, haltval, status)
!  Description:
!    Offers menu selection to user so that the user can setup the values of 
!    certain control parameters for minimisation.
!  Keywords:
!    Fitting.Control.Parameters, Parameters.Fit.Control
!  Method:
!    Menu controlled subroutine
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    04-Jan-1996: V0.2 Changes for IBM AIX "xlf" compiler: Doesn't like "i" and 
!      "g" format descriptors without width specifiers, and doesn't like 
!      certain formats (Hammersley)
!    04-Feb-1993: V0.1 Original, based on "FITSET" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
!  Import/Export:
     Logical, Intent(INOUT) :: poisson ! .True., if Poissonian statistics are 
!      to be taken into account when fitting the data
     Logical, Intent(INOUT) :: mlm ! .True., if the Maximum Likelihood method 
!      is to be applied
     Logical, Intent(INOUT) :: weight ! .True. if weighting by the errors is
!      to be applied to the fit
     Real, Intent(INOUT) :: alpha ! The average number of standard deviations on
!      each data point for the aimed goodness of fit
     Real, Intent(INOUT) ::  itsperpar ! The number iterations per
!      unconstrained parameter for each fit
     Logical, Intent(INOUT) :: evolve ! .True., if the starting model for the
!      fit is to evolve i.e. the previous fit is used as the start for the new
!      fit, otherwise the same initial model is used all the time
     Integer, Intent(INOUT) :: disfreq ! The frequency at which the fit is
!      displayed graphically to the user, 0 = no display
     Integer, Intent(INOUT) :: contint ! Interval of demanding whether to
!      continue or to stop: 0 = No request
     Logical, Intent(INOUT) :: reverse ! .True., is the frames are to be fitted 
!      in reverse order
     Logical, Intent(INOUT) :: fastdis ! .True., if only minimal text is to be 
!      displayed to speed up graphical output
     Integer, Intent(INOUT) :: haltcrit ! The halting criterion to be used
!      for the fitting:
!        0 = No halting
!        1 = Upper limit on the goodness of fit
     Real, Intent(INOUT) :: haltval ! The value used to halt the fit
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Integer, Parameter :: Maxinstr = 13 ! Number of instructions in the menu
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Character(Len = 80) :: command ! Full command corresponding to user request
     Character(Len = 132) :: format ! Format string for I/O
     Logical :: finish ! .True., when the user wants to leave the menu
     Integer :: lentext ! Length of text to be output
     Integer :: numinstr ! Number of choices available in the menu
!  Local Arrays:
     Character(Len = 80) :: ERROR(10) ! Buffer for error messages
     Character(Len = 20) :: MENU(Maxinstr) ! Array containing menu choices
     Character(Len = 80) :: MENUTXT(Maxinstr) ! Text to explain menu choices
     Character(Len = 80) :: MESSAGE(15) ! Text to be output to the user
!  External Functions:
!  Local Data:
     Data MENU / 'ACCURACY', 'CHANGE FIT ORDER', 'DISPLAY FREQUENCY', 'EXIT', &
       'FAST DISPLAY', 'HALT CRITERION', 'MAXIMUM LIKELIHOOD', &
       'MODEL EVOLUTION', 'NUMBER ITERATIONS', 'POISSON STATISTICS', &
       'REQUEST CONTINUATION', 'VIEW', 'WEIGHTED FIT' /
     Data MENUTXT / 'ACCURACY - Aimed accuracy in fit', &
       'CHANGE FIT ORDER - Reverse order of frame fitting', &
       'DISPLAY FREQUENCY - Frequency of graphical output', &
       'EXIT - Exit from menu', &
       'FAST DISPLAY - Minimal text or full text on graphs', &
       'HALT CRITERION - Permature stopping of fit criteria', &
       'MAXIMUM LIKELIHOOD - Fitting by Maximum Likelihood method', &
       'MODEL EVOLUTION - Constant or evolving initial fit model', &
       'NUMBER ITERATIONS - Number of iterations per fit parameter', &
       'POISSON STATISTICS - Fitting using model derived weighting', &
       'REQUEST CONTINUATION - at defined intervals during fitting', &
       'VIEW - See values of all control parameters', &
       'WEIGHTED FIT - Fit weighted by errors or unweighted ' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SETUP ' // Version)
        Return
     End If
 
!  Initialise values
     numinstr = Maxinstr
     command = 'VIEW'
     finish = .False.
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Start command input/action loop until EXIT requested
     Do While (.Not. finish)
 
!     Get user to select between the available menu options
        Call IO_MENU (.True., 'FITTING SETUP SUB-MENU: ENTER COMMAND', &
          Maxinstr, MENUTXT, 1, ERROR, Maxinstr, numinstr, MENU, command, &
          status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     EXECUTE MENU CHOICES
 
!     Call subroutine to perform requested operation.
        If (command .Eq. 'ACCURACY') Then
 
           MESSAGE(1) = 'Required accuracy: 1.0 = An one sigma ' // &
             'fit (on average) throughout'
           MESSAGE(2) = 'the data region.  ( < 1.0 higher ' // &
             'accuracy, closer fit to data '
           MESSAGE(3) = ' > 1.0 Lower accuracy, looser fit to data )'
           Write (ERROR,'(''Must be within set limits'')')
           Call IO_INPR (.True., 0.1e-6, 100., .True., 'ACCURACY LEVEL', 3, &
             MESSAGE, 1, ERROR, alpha, status)
 
           command = 'EXIT'
 
        Else If (command .Eq. 'DISPLAY FREQUENCY') Then
 
!        Input frequency of graphical output during multiple fitting
           MESSAGE(1) = 'How often should the result of the ' // &
             'fit be displayed.'
           MESSAGE(2) = '(0 = No display)'
           ERROR(1) = 'Must be within set limits'
           Call IO_INPI (.True., 0, 1024, .True., 'GRAPH FREQUENCY', 2, &
             MESSAGE, 1, ERROR, disfreq, status)
 
           command = 'EXIT'
 
        Else If (command .Eq. 'CHANGE FIT ORDER') Then
 
!        Toggle fitting order for frames
           reverse = .Not. reverse
 
           If (reverse) Then
              MESSAGE(1) = 'Multiple frame fitting will now be in' // &
                ' reverse order'
           Else
              MESSAGE(1) = 'Multiple frame fitting will now be in' // &
                ' normal order'
           End If
 
           Call IO_WRITE (MESSAGE(1), status)
 
           command = 'EXIT'
 
        Else If (command .Eq. 'EXIT') Then
 
!        EXIT : Set 'finish' to exit the loop
           finish = .True.
 
        Else If (command .Eq. 'FAST DISPLAY') Then
 
!        Toggle fast/normal display mode
           fastdis = .Not. fastdis
 
           If (fastdis) Then
              MESSAGE(1) = 'Minimal text will be added to ' // &
                'fit graphical output'
           Else
              MESSAGE(1) = 'Full text will be added to ' // &
                'fit graphical output'
           End If
 
           Call IO_WRITE (MESSAGE(1), status)
 
           command = 'EXIT'
 
        Else If (command .Eq. 'HALT CRITERION') Then
 
!        Input halt criterion
           MESSAGE(1) = 'What halt criterion do you want to ' // &
             'use to permaturely stop the fitting'
           MESSAGE(2) = '          0 = No halt criterion'
           MESSAGE(3) = '          1 = Upper limit on goodness of fit'
           ERROR(1) = 'Must be within set limits'
           Call IO_INPI (.True., 0, 1, .True., 'HALT CRITERION', 3, MESSAGE, &
             1, ERROR, haltcrit, status)
 
           If (haltcrit .Eq. 1) Then
 
!           Must input upper limit for the goodness of fit
              MESSAGE(1) = 'Enter upper limit for the goodness of fit'
              ERROR(1) = 'Must be within set limits'
              Call IO_INPR (.True., 0.1e-6, 100.0, .True., 'MAXIMUM GOOD FIT', &
                1, MESSAGE, 1, ERROR, haltval, status)
 
           End If
 
           command = 'EXIT'
 
        Else If (command .Eq. 'MAXIMUM LIKELIHOOD') Then
 
!        Toggle Maximum Likelihood method criterion
           mlm = .Not. mlm
 
           If (mlm) Then
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
              MESSAGE(1) = 'The Maximum Likelihood method ' // &
                '(MLM) will be applied'
           Else
              MESSAGE(1) = 'Least Squares fitting will be applied'
           End If
 
           Call IO_WRITE (MESSAGE(1), status)
 
           command = 'EXIT'
 
        Else If (command .Eq. 'MODEL EVOLUTION') Then
 
!        Toggle evolution/static initial fit model parameter flag
           evolve = .Not. evolve
 
           If (evolve) Then
              MESSAGE(1) = 'The minimised fit parameters for one ' // &
                'will be used as the'
              MESSAGE(2) = 'starting parameters for the next fit'
           Else
              MESSAGE(1) = 'The user input fit parameters ' // &
                'will be used as the'
              MESSAGE(2) = 'starting parameters for all the fits'
           End If
 
           Call IO_TEXT (2, MESSAGE, status)
 
           command = 'EXIT'
 
        Else If (command .Eq. 'NUMBER ITERATIONS') Then
 
!        Input number of iterations per unconstrained parameter
           MESSAGE(1) = 'Maximum number of iterations ' // &
             'per fit per unconstrained parameter'
           ERROR(1) = 'Must be within set limits'
           Call IO_INPR (.True., 0.1, 200.0, .True., 'NUMBER PER PARAMETER', &
             1, MESSAGE, 1, ERROR, itsperpar, status)
 
           command = 'EXIT'
 
        Else If (command .Eq. 'POISSON STATISTICS') Then
 
!        Toggle Poissonian/ Gaussian statistics assumption
           poisson = .Not. poisson
 
           If (poisson) Then
              MESSAGE(1) = 'Poissonian statistics will be taken ' // &
                'into account by model derived weighting'
           Else
              MESSAGE(1) = 'Gaussian statistics weighted by ' // &
                'entered data errors'
           End If
 
           Call IO_WRITE (MESSAGE(1), status)
 
           command = 'EXIT'
 
        Else If (command.Eq.'REQUEST CONTINUATION') Then
 
!        Input number of frames to fit between user prompts
           MESSAGE(1) = 'How many frames are to be fitted ' // &
             'between continuation requests'
           MESSAGE(2) = 'Return "0" for no requests'
           ERROR(1) = 'Must be within defined limits'
           Call IO_INPI (.True., 0, 1024, .True., 'REQUEST INTERVAL', 2, &
             MESSAGE, 1, ERROR, contint, status)
 
           command = 'EXIT'
 
        Else If (command .Eq. 'VIEW') Then
 
!        Output details of control parameters
           format = '(''ACCURACY : '', g12.5, '' (Average '',' // &
             '''standard deviations per data point)'')'
           Write (MESSAGE(1), format) alpha
           format = '(''DISPLAY FREQUENCY : '', i6,' // &
             ''' (Frequency of graphical output)'')'
           Write (MESSAGE(2), format) disfreq
 
           If (fastdis) Then
              MESSAGE(3) = 'FAST DISPLAY : TRUE (Minimal text ' // &
                'will be added to fit graphical output)'
           Else
              MESSAGE(3) = 'FAST DISPLAY : FALSE (Full text ' // &
                'will be added to fit graphical output)'
           End If
 
           If (reverse) Then
              MESSAGE(4) = 'REVERSE : TRUE (Multiple frame ' // &
                ' fitting in reverse order)'
           Else
              MESSAGE(4) = 'REVERSE : FALSE (Multiple frame ' // &
                ' fitting in normal order)'
           End If
 
           If (haltcrit .Eq. 0) Then
 
              format = '(''HALT CRITERION : '', i1, '' ('',' // &
                '''No halt criterion)'')'
              Write (MESSAGE(5), format) haltcrit
 
              lentext = 5
           Else
 
              format = '(''HALT CRITERION : '', i1 ,'' ('',' // &
                '''Upper limit on goodness of fit)'')'
              Write (MESSAGE(5), format) haltcrit
 
              format = '(''HALT VALUE : '', g12.5,'' ('',' // &
                '''Meaning defined by HALT CRITERION)'')'
              Write (MESSAGE(6), format) haltval
              lentext = 6
           End If
 
           If (evolve) Then
              MESSAGE(lentext + 1) = 'MODEL EVOLUTION : TRUE (' // &
                'Minimised parameters used for next fit)'
           Else
              MESSAGE(lentext + 1) = 'MODEL EVOLUTION : FALSE (' // &
                'Initial parameters used for all fits)'
           End If
 
           format = '(''NUMBER ITERATIONS : '', g12.5, ' // &
             ''' (Per unconstrained parameter)'')'
           Write (MESSAGE(lentext + 2), format) itsperpar
 
           If (weight) Then
              MESSAGE(lentext + 3) = 'WEIGHTED FIT : TRUE (Fit ' // &
                'weighted by error values)'
           Else
              MESSAGE(lentext + 3) = 'WEIGHTED FIT : FALSE (Fit ' // &
                'unweighted)'
           End If
 
           If (poisson) Then
              MESSAGE(lentext + 4) = 'POISSON STATISTICS : TRUE ( ' // &
                'Model derived weighting)'
           Else
              MESSAGE(lentext + 4)='POISSON STATISTICS : FALSE ( '// &
                'Weighting by ERROR array)'
           End If
 
           If (mlm) Then
              MESSAGE(lentext + 5) = 'MAXIMUM LIKELIHOOD METHOD : ' // &
                'TRUE (Applied to fitting)'
           Else
              MESSAGE(lentext + 5) = 'MAXIMUM LIKELIHOOD METHOD : ' // &
                'FALSE (Fit by Least Squares Method)'
           End If
 
           If (contint .Eq. 0) Then
              MESSAGE(lentext + 6) = 'No user CONTINUATION requests'
           Else
              format = '(''User CONTINUATION ''' // &
                '''prompts every '', i4, '' frames'')'
              Write (MESSAGE(lentext + 6), format) contint
           End If
 
           Call IO_TEXT (lentext + 6, MESSAGE, status)
 
        Else If (command .Eq. 'WEIGHTED FIT') Then
 
!        Toggle weighted/non-weighted fit flag
           weight = .Not. weight
 
           If (weight) Then
              MESSAGE(1) = 'Error weighting will now to be used ' // &
                'for the fit minimisation'
           Else
              MESSAGE(1) = 'No weighting will now to be used ' // &
                'for the fit minimisation'
           End If
 
           Call IO_WRITE (MESSAGE(1), status)
 
           command = 'EXIT'
 
        Else
 
!        Unknown command.
           Call IO_WRITE ('WARNING: Unknown command, please re-enter', status)
 
        End If
 
!     Check value of status
        Call ST_OUT (status)
 
     End Do
 
     End Subroutine F2D_SETUP
!********1*********2*********3*********4*********5*********6*********7*********8
