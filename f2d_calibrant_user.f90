!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_calibrant_user.f90 *
!  *                        *
!  **************************
 
!+ F2D_CALIBRANT_USER - CALIBRANT fit of wavelength / distance / etc.
     Subroutine F2D_CALIBRANT_USER (max_angles, retstat, num_cali_rings, &
       D_SPACINGS, status)
!  Description:
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    18-Feb-2003: V0.2 Force screen to be updated, if cancel (Hammersley)
!    24-Jul-2001: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: max_angles ! Dimension of "ANGLES" array
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status: 
!      0 = Good status
!      1 = Cancel was chosen (no file)
!      2 = Bad status (no file)
     Integer, Intent(OUT) :: num_cali_rings ! Number of calibration powder
!      rings defined
     Real, Intent(OUT) :: D_SPACINGS(max_angles) ! Calibrant diffraction peak
!      d-spacings in order of distance in metres
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: len_string ! Number of defined characters in string
     Integer :: ring ! Loop variable
     Character(Len = 256) :: file_name = 'sample.Ds' ! Name of ASCII file
!      containing D-spacings
!  Local Arrays:
     Character(Len = 80) :: PROMPT(2) ! Main prompt for required file
     Character(Len = 80) :: INFO(10) ! Help Information
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CALIBRANT_USER ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (Max_angles .Le. 0) Then
        status = St_bad_dim1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_CALIBRANT_USER ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Obtain default file name from data-base
        Call IO_INQ_KEYVALUE ('CALIBRANT_USER_FILE', len_string, file_name, &
          retstat, status)
 
!     Name of file containing list of D-spacings in Angstroms
        PROMPT(1) = 'Select file containing list of D-spacings'
        PROMPT(2) = '(In Angstroms, and in decreasing order)'
        INFO(1) = 'Select ASCII file with D-spacings to be used'
        INFO(2) = 'for calibration. One value per line, in units'
        INFO(3) = 'of Angstroms. e.g. The first 6 D-spacings for'
        INFO(4) = 'Silicon would be defined:'
        INFO(5) = '3.1355'
        INFO(6) = '1.9201'
        INFO(7) = '1.6375'
        INFO(8) = '1.3577'
        INFO(9) = '1.2459'
        INFO(10) = '1.1086'
        Call GS_FILESELECTION (2, 2, PROMPT, 10, 10, INFO, 1, .False., &
          retstat, file_name, status)
 
!     Check status
        If (retstat .Ne. 0) Then
 
!        Force screen to be re-drawn
           Call GS_UPDATE (status)
           Return
 
        End If
 
!     Input values from file
        Call FIO_IN_1DASCII (file_name, max_angles, retstat, num_cali_rings, &
          D_SPACINGS, status)
 
!     Convert from Angstroms to metres
        Do ring = 1, num_cali_rings
           D_SPACINGS(ring) = D_SPACINGS(ring) * 1.0e-10
        End Do
 
!     Save default file name in data-base
        Call IO_SET_KEYVALUE ('CALIBRANT_USER_FILE', Len_trim(file_name), &
          file_name, retstat, status)
 
     End If
 
     End Subroutine F2D_CALIBRANT_USER
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
