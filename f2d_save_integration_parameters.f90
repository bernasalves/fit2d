!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************************************
!  *                                     *
!  * f2d_save_integration_parameters.f90 *
!  *                                     * 
!  ***************************************
 
!+ F2D_SAVE_INTEGRATION_PARAMETERS - FIT2D: SAVE INTEGRATION PARAMETERS
     Subroutine F2D_SAVE_INTEGRATION_PARAMETERS (input_file, &
       experiment, xstrelm, ystrelm, &
       xendelm, yendelm, outer_limit, scan_type, maximum_d, &
       conserve_intensity, correct_geometry, outer_angle, &
       num_2theta, lorentz_geometry, rad_pixel_size, outer_q, &
       parameter_file, status)
!  Description:
!    Writes to an output file the integration parameters
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    14-Oct-2011: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use GS_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Character(Len = *), Intent(IN) :: input_file ! Name of input file
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: xendelm ! The X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! The Y-pixel number for the end of the ROI
     Real, Intent(IN) :: outer_limit ! Outer radius in metres
     Integer, Intent(IN) :: scan_type ! Type of 1-D scan to be produced:
!      0 = Radial: equal distance scan
!      1 = "2-theta": equal angle element scan
!      2 = Q-space: Equal Q element scan
!          (Q = (4 * Pi / wavelength) Sin(theta/2) )
!      3 = D-spacing scan
     Real, Intent(IN) :: maximum_d ! Maximum D-spacing to calculate for
!      D-spacing scans (Angstroms)
     Logical, Intent(IN) :: conserve_intensity ! .True., if the total
!      intensity in to be conserved in the re-binning operation
     Logical, Intent(IN) :: correct_geometry ! .True., if the scan
!      intensities are to be corrected for the 1/Cos**3(angle) drop owing 
!      to the geometrical difference between a 2-theta scan and a flat detector
     Real, Intent(IN) :: outer_angle ! Maximum angle of 2-theta scan
     Integer, Intent(IN) :: num_2theta ! Number of 2-theta angle or radial
!      bins in rebinned array
     Integer, Intent(IN) :: lorentz_geometry ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the equivalent of a 
!            2-theta scan i.e. detector at equal distance
     Real, Intent(IN) :: rad_pixel_size ! Size of pixel to be used for the
!      2-theta, radial, or q-space bin sizes. If an equal radial
!      distance scan is to be calculated ("scan_type = 0") the units
!      are metres, and for an equal angle pixel scan ("scan_type = 1")
!      the units are radians, and for a Q-space scan the units are
!      inverse Angstroms (?)
     Real, Intent(IN) :: outer_q ! Outer q value
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: parameter_file ! Name of output file
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Integer, Parameter :: Io_out = 17 ! Unit number for output
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Character(Len = Len_name) :: message ! User output
     Integer :: retstat ! Return status variable:
!      0 = Good status
!      1 = File not openned
!      2 = Problem during writing of the file
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SAVE_INTEGRATION_PARAMETERS ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Arguments would appear to be reasonable, go ahead.

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_SAVE_INTEGRATION_PARAMETERS: Start'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Open output file
     Call IO_OPEN_ASCIIFILE (.False., 'WRITE', Io_out, parameter_file, &
       retstat, status)
 
     If (retstat .Ne. 0) Then
        Return
     End If

     Write (message, '(''NOTE: Writing '', a)') Trim(parameter_file)
     Call IO_WRITE (message, status)

     Write (Io_out, '(''INTEGRATION PARAMETERS'')')
     Write (Io_out, '('' '')')

     Write (Io_out, '(''Last input file = '', a)') Trim(input_file)
     Write (Io_out, '('' '')')

     Write (Io_out, '(''Region of interest from ('', i5,'','' i5, ' // & 
       ''') to ('', i5, '','', i5, '')'')') xstrelm, ystrelm, xendelm, yendelm
     Write (Io_out, '('' '')')

     Write (Io_out, '(''X/Y pixel sizes = '', f8.2, '','', f8.2,'' microns'')')&
       experiment%x_pixel_size * 1.0e6, experiment%y_pixel_size * 1.0e6
     Write (Io_out, '(''Sample to detector distance = '', f10.4, '' mm'')') &
       experiment%detector_distance * 1.0e3
     Write (Io_out, '(''Wavelength = '', f12.6, '' Angstroms'')')&
       experiment%wavelength * 1.0e10
     Write (Io_out, '(''X/Y pixel position of beam = '', f10.3, '', '', ' // &
       'f10.3)') experiment%x_beam, experiment%y_beam
     Write (Io_out, '(''Angle of tilt plane = '', f10.4, '' degrees'')') &
       experiment%tilt_plane_rotation * 180.0 / Pi
     Write (Io_out, '(''Angle of tilt = '', f12.6, '' degrees'')') &
       experiment%tilt_angle * 180.0 / Pi
     Write (Io_out, '(''Detector rotation angle = '', f12.6, '' degrees'')') &
       experiment%detector_rotation * 180.0 / Pi
     Write (Io_out, '(''Polarisation factor = '', f12.4)') &
       experiment%polarisation

     Write (Io_out, '('' '')')
     If (scan_type .Eq. 0) Then
        Write (Io_out, '(''Scan type = RADIAL'')')
     Else If (scan_type .Eq. 1) Then
        Write (Io_out, '(''Scan type = 2-THETA'')')
     Else If (scan_type .Eq. 2) Then
        Write (Io_out, '(''Scan type = Q-SPACE'')')
     Else If (scan_type .Eq. 3) Then
        Write (Io_out, '(''Scan type = D-SPACING'')')
     End If

     If (conserve_intensity) Then
        Write (Io_out, '(''Conserve Intensity = YES'')')
     Else
        Write (Io_out, '(''Conserve Intensity = NO'')')
     End If

     If (experiment%correct_polarisation) Then
        Write (Io_out, '(''Correct Polarisation = YES'')')
     Else
        Write (Io_out, '(''Correct Polarisation = NO'')')
     End If

     If (correct_geometry) Then
        Write (Io_out, '(''Geometric Correction = YES'')')
     Else
        Write (Io_out, '(''Geometric Correction = NO'')')
     End If

     Write (Io_out, '(''Maximum 2-theta Angle = '', f10.5, '' degrees'')') &
       outer_angle * 180.0 / Pi
     Write (Io_out, '(''Mumber of scan bins = '', i8)') num_2theta
     Write (Io_out, '(''Maximum D-Spacing = '', f10.5, '' Angstroms'')') &
       maximum_d

!  Close file
     Close (Io_out)
 
     End Subroutine F2D_SAVE_INTEGRATION_PARAMETERS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 

