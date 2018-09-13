!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_inp_wavelength.f90 *
!  *                        *
!  **************************
 
!+ F2D_INP_WAVELENGTH -  INPut WAVELENGTH (photon)
     Subroutine F2D_INP_WAVELENGTH (gui, wavelength, status)
!  Description:
!    User input of experimental (photon) wavelength.
!  Keywords:
!    Wavelength
!  Method:
!    Uses "IO_INPR" or "GS_INPR"
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Dec-1996: V0.4 Avoid open strings crossing lines (Hammersley)
!    11-Feb-1996: V0.3 Option of GUI (Hammersley)
!    03-Jan-1996: V0.2 Changes made for IBM AIX "xlf" compiler (Hammersley)
!    02-Mar-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is being 
!      used
!  Import/Export:
     Real, Intent(INOUT) :: wavelength ! Wavelength in metres
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Real :: angstroms ! Wavelength in Angstroms
     Real :: energy ! Energy of X-rays in keV
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(4) ! User text
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_WAVELENGTH ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        MESSAGE(1) = 'Enter the wavelength of the X-ray ' // &
          'radiation in Angstroms. (1 Angstrom ='
        MESSAGE(2) = '0.1 nm). Some typical values (Angstroms) are:'
        MESSAGE(3) = 'Cu K alpha 1 = 1.540614 (8.04778 keV)'
        MESSAGE(4) = 'Mo K alpha 1 = 0.7093243 (17.47934 keV)'
        angstroms = wavelength * 1.0e10
 
        If (gui) Then
           Call GS_INPR (.True., 0.0, 100000.0, .True., &
             'WAVELENGTH (Angstroms) (0.0 for keV )', 4, MESSAGE, 1, &
             'Must be valid real number', angstroms, status)
        Else
           Call IO_INPR (.True., 0.0, 100000.0, .True., &
             'WAVELENGTH (Angstroms) (0.0 for keV )', 4, MESSAGE, 1, &
             'Must be valid real number', angstroms, status)
        End If
 
        If (angstroms .Ne. 0.0) Then
           wavelength = angstroms * 1.0e-10
           Write (message, '(''INFO: Energy = '', g14.5, '' keV'')') 12.39852 &
             / angstroms
           Call IO_WRITE (message, status)
 
        Else
 
!        Input energy of X-rays
           energy = (12.3984193 / wavelength) * 1.0e-10
 
           If (gui) Then
              Call GS_INPR (.True., 0.001, 10000.0, .True., &
                'ENERGY OF RADIATION (keV)', 1, &
                'Enter energy of X-rays in keV (an average will do)', 1, &
                'Must be valid real number', energy, status)
           Else
              Call IO_INPR (.True., 0.001, 10000.0, .True., &
                'ENERGY OF RADIATION (keV)', 1, &
                'Enter energy of X-rays in keV (an average will do)', 1, &
                'Must be valid real number', energy, status)
           End If
 
           wavelength = 12.3984193 / energy
           Write (message, '(''INFO: Wavelength = '', g14.5, '' Angstroms'')') &
             wavelength
           Call IO_WRITE (message, status)
           wavelength = wavelength * 1.0e-10
 
        End If
 
     End If
 
     End Subroutine F2D_INP_WAVELENGTH
!********1*********2*********3*********4*********5*********6*********7*********8
