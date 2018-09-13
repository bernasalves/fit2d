!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************
!  *               *
!  * f2d_bragg.f90 *
!  *               *
!  *****************
 
!+ F2D_BRAGG - FIT 2-D BRAGGs' equation
     Subroutine F2D_BRAGG (status)
!  Description:
!    Variety of uses of Braggs equation
!  Method:
!    Menu driven loop
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    10-Jan-1998: V0.4 Correction to second call to
!      "F2D_INP_WAVELENGTH" which could have resulted in a crash (Hammersley)
!    11-Feb-1996: V0.3 Changes to "F2D_INP_WAVELENGTH" (Hammersley)
!    21-Apr-1995: V0.2 Add option to calculate wavelength (Hammersley)
!    02-Mar-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
     Integer, Parameter :: Max_menu = 5 ! Number of instructions in the menu
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Character(Len = 80) :: command ! Full command corresponding to user request
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of menu choices
     Integer :: temp_status ! Temporary version of 'status'
     Logical :: continue ! .True., if the menu loop is to continue
     Logical :: reset ! .True., if 'status' system is to be reset
     Real, Save :: d_spacing = 39.0e-10 ! D-spacing between crystal planes
!      in metres
     Real, Save :: two_theta = 0.3490658 ! Two theta angle (radians)
     Real, Save :: wavelength = 1.540614e-10 ! Wavelength (metres)
!  Local Arrays:
     Character(Len = 27) :: MENU(Max_menu) ! Array containing menu choices
     Character(Len = 80) :: MENUTXT(Max_menu) ! Text to explain menu choices
!    Internal Functions:
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 5) / 'D-SPACING', 'EXIT', 'QUIT', &
       'TWO THETA ANGLE', 'WAVELENGTH/ENERGY' /
     Data (MENUTXT(item), item = 1, 5) / &
       'D-SPACING: Calculate D-spacing from two theta angle/ wavelength', &
       'EXIT: Return to main menu', 'QUIT: Return to main menu', &
       'TWO THETA ANGLE: 2 theta angle from d-spacing/ wavelength', &
       'WAVELENGTH/ENERGY: Calculate from d-spacing/ 2 theta angle' /
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_BRAGG'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_BRAGG ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
     command = 'D-SPACING'
     num_menu = Max_menu ! Number of instructions available in the menu
 
!  Start command input/action loop until EXIT requested
     continue = .True.
     Do While (continue)
 
!     Get user to select between the available menu options
        Call IO_MENU (.True., 'Bragg equation sub-menu: ENTER COMMAND', &
          Max_menu, MENUTXT, 1, 'Enter one of the available commands', &
          Max_menu, num_menu, MENU, command, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     EXECUTE MENU CHOICES
 
!     Call subroutine to perform requested operation.
        If (command .Eq. 'D-SPACING') Then
 
!        Input wavelength
           Call F2D_INP_WAVELENGTH (.False., wavelength, status)
 
!        Input two theta angle
           two_theta = two_theta * 180.0 /  Pi
           Call IO_INPR (.True., 0.0, 180.0, .True., &
             'TWO THETA ANGLE (Degrees)', 1, &
             'Enter two theta angle in degrees', 1, &
             'Must be valid real number', two_theta, status)
           two_theta = two_theta * Pi / 180.0
 
!        Calculate D-spacings
           d_spacing = wavelength / (2.0 * Sin(two_theta / 2.0))
 
!        Output results
           Write (message, '(''INFO: D-spacing (Angstroms) = '', ' // &
             'f12.5)') d_spacing * 1.0e10
           Call IO_WRITE (message, status)
 
           command = 'TWO THETA ANGLE'
 
        Else If (command .Eq. 'EXIT' .Or. command .Eq. 'QUIT') Then
 
!        EXIT: Return to main menu
           continue = .False.
 
        Else If (command .Eq. 'TWO THETA ANGLE') Then
 
!        Input wavelength
           Call F2D_INP_WAVELENGTH (.False., wavelength, status)
 
!        Input D-spacing
           d_spacing = d_spacing * 1.0e10
           Call IO_INPR (.True., 0.001, 1.0e6, .True., &
             'D-SPACING (Angstroms)', 1, &
             'Enter d-spacing in Anstroms (1 Angstrom = 0.1nm)', 1, &
             'Must be valid real number', d_spacing, status)
           d_spacing = d_spacing / 1.0e10
 
!        Calculate 2 theta angle
           two_theta = 2.0 * Asin (wavelength / (2.0 * d_spacing))
 
!        Output results
           Write (message, '(''INFO: Two theta angle (Degrees) ' // &
             '= '', f12.5)') two_theta * 180.0 / Pi
           Call IO_WRITE (message, status)
 
           command = 'D-SPACING'
 
        Else If (command .Eq. 'WAVELENGTH/ENERGY') Then
 
!        Input D-spacing
           d_spacing = d_spacing * 1.0e10
           Call IO_INPR (.True., 0.001, 1.0e6, .True., &
             'D-SPACING (Angstroms)', 1, &
             'Enter d-spacing in Anstroms (1 Angstrom = 0.1nm)', 1, &
             'Must be valid real number', d_spacing, status)
           d_spacing = d_spacing / 1.0e10
 
!        Input two theta angle
           two_theta = two_theta * 180.0 /  Pi
           Call IO_INPR (.True., 0.0, 180.0, .True., &
             'TWO THETA ANGLE (Degrees)', 1, &
             'Enter two theta angle in degrees', 1, &
             'Must be valid real number', two_theta, status)
           two_theta = two_theta * Pi / 180.0
 
!        Calculate wavelength
           wavelength = d_spacing * 2.0 * Sin(two_theta / 2.0)
 
!        Output results
           Write (message, '(''INFO: Wavelength (Angstroms) ' // &
             '= '', f12.5)') wavelength * 1.0e10
           Call IO_WRITE (message, status)
           Write (message, '(''INFO: Energy (keV) = '', f12.5)') &
             12.3984193 / (wavelength * 1.0e10)
           Call IO_WRITE (message, status)
 
           command = 'D-SPACING'
 
        Else
 
!        Unknown command
           Call IO_WRITE ( &
             'WARNING: Unknown command, please enter new command', status)
 
        End If
 
!     Output user message if error
        Call ST_OUT (status)
 
        If (status .Ne. St_goodvalue) Then
 
!        Use choice to EXIT or reset status
           reset = .True.
           temp_status = St_goodvalue
           Call IO_INPL (.True., 0, 1, .True., 'RESET "status"', 1, &
             'YES: to reset "status" value, other exit program', 1, &
             'Enter YES or NO', reset, temp_status)
 
           If (reset) Then
              Call ST_DEF_SYSTEM (status)
           Else
              continue = .False.
           End If
 
        End If
 
     End Do
 
     End Subroutine F2D_BRAGG
!********1*********2*********3*********4*********5*********6*********7*********8
