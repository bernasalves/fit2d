!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_rtheta.f90 *
!  *                *
!  ******************
 
!+ F2D_RTHETA -  Radial/THETA re-binning
     Subroutine F2D_RTHETA (title, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MASK, experiment, max_angular, &
       max_radial, lorentz_geometry, num_azimuthal, &
       num_radial, ANGULAR_AXIS, RADIAL_AXIS, R_THETA, azimuth_pixel_size, &
       radial_pixel_size, mtitle, mxlabel, mylabel, status)
!  Description:
!  Keywords:
!    Radial~Profile.Calculation, Calculate.Radial~Profile,
!    Profile.Radial.Calculation, Powder~Diffraction.Radial~Profile,
!    Diffraction.Powder.Radial~Profile
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    28-Mar-2006: V0.11 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    14-Mar-2006: V0.10 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    16-Dec-1996: V0.9 Avoid open strings crossing lines (Hammersley)
!    11-Feb-1996: V0.8 Changes to "F2D_INP_SAMPLEDISTANCE" (Hammersley)
!    02-Feb-1996: V0.7 Change to "F2D_INP_BEAMCENTRE" arguments (Hammersley)
!    23-Oct-1995: V0.6 Lorentz correction to an uncorrected 2-theta scan 
!      added (Hammersley)
!    01-Sep-1995: V0.5 Separate 2-theta and azimuthly polarisation corrections 
!      and add correction for Lorentz (Hammersley)
!    28-Aug-1995: V0.4 Take account of polarisation (Hammersley)
!    03-May-1995: V0.3 Include choice of equal 2 theta angle bins (Hammersley)
!    03-Mar-1995: V0.2 Intensity conservation option (Hammersley)
!    08-Feb-1995: V0.1 Original, based on "F2D_POWDERDIFFRACTION" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: max_angular ! First dimension of 'R_THETA'
     Integer, Intent(IN) :: max_radial ! Second dimension of 'R_THETA'
!  Import/Export:
     Integer, Intent(INOUT) :: lorentz_geometry ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the equivalent of a 
!            2-theta scan i.e. detector at equal distance
!  Export:
     Integer, Intent(OUT) :: num_azimuthal ! Number of angular bins in
!      rebinned array
     Integer, Intent(OUT) :: num_radial ! Number of radial bins in rebinned
!      array
     Real, Intent(OUT) :: ANGULAR_AXIS(max_angular) ! Horizontal (angular)
!      axis information
     Real, Intent(OUT) :: RADIAL_AXIS(max_radial) ! Vertical (radial) axis
!      information
     Real, Intent(OUT) :: R_THETA(max_angular, max_radial) ! Re-binned
!      angular/radial pixel data
     Real, Intent(OUT) :: radial_pixel_size ! Size of pixel to be used for
!      the 2-theta or radial bin sizes. If an equal radial distance scan
!      is to be calculated ('angular_scan = .False.') the units are
!      metres, and for an equal angle pixel scan ('angular_scan =
!      .True.') the units are radians
     Real, Intent(OUT) :: azimuth_pixel_size ! Size of pixel to be used for
!      the azimuth bin sizes. The units are radians
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.11' ! Version number
!  Local Variables:
     Integer :: pixel ! Loop variable for pixels
     Integer stat ! Status return variable for "Allocate"
     Logical, Save :: angular_scan = .True. ! .True., if the 1-D scan is to
!      be produced in equal angle elements as opposed to equal radial distance
!      elements
     Logical, Save :: change_values = .False. ! .True., if refined parameters 
!      values for the beam centre and/or the tilt are to be edited by the user
     Logical, Save :: first = .True. ! .True., if the subroutine is being
!      called for the first time
     Logical, Save :: conserve_intensity = .True. ! .True., if the total
!      intensity in to be conserved in the re-binning operation
!    Logical spatial_distortion ! .True., if spatial distortion is to
!      be taken into account
     Real, Save :: maximum_2theta ! Maximum 2 theta angle for re-binning to
!      a 1-D scan (radians)
     Real, Save :: maximum_radius ! Maximum radius in metres for re-binned
!      region
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(9) ! User text
     Real, Allocatable :: WORK(:, :) ! Dynamic array for "F2D_RTHETA2" used to 
!      store fractions of pixels contributing to each angular/radial pixel
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RTHETA ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0 .Or. max_radial .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Lt. 1 .Or. xstrelm .Gt. xendelm .Or. xendelm .Gt. &
       xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Lt. 1 .Or. ystrelm .Gt. yendelm .Or. yendelm .Gt. &
       ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_RTHETA ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Output WARNING MESSAGE
        Call IO_WRITE (' ', status)
        Call IO_WRITE ('WARNING : This option is still in ' // &
          'a developmental stage ', status)
        Call IO_WRITE ('          User input will change, ' // &
          'check results carefully', status)
        Call IO_WRITE (' ', status)
 
!     Output current beam centre and detector tilt values
        Write (MESSAGE(1), '(''INFO: The current pixel ' // &
          'coordinates for the beam centre = '', 2f10.3)') &
          experiment%x_beam, experiment%y_beam
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''INFO: The current pixel ' // &
          'sizes (microns) = '', 2f10.3)') experiment%x_pixel_size * 1.0e6, &
          experiment%y_pixel_size * 1.0e6
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''INFO: The current sample to ' // &
          'detector distance (millimetres) = '', f10.3)') &
          experiment%detector_distance * 1000.0
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''INFO: The rotation angle of the ' // &
          'tilt plane = '', f8.3)') experiment%tilt_plane_rotation * 180.0 / Pi
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''INFO: The tilt angle of the ' // &
          'detector = '', f8.3)') experiment%tilt_angle * 180.0 / Pi
        Call IO_WRITE (MESSAGE(1), status)
 
!     Chance to change beam centre and tilt
        Call IO_INPL (.True., 0, 1, .True., &
          'CHANGE BEAM CENTRE AND/OR TILT VALUES', 1, &
          'YES: to specific new values for parameters', 1, &
          'Enter "YES" or "NO"', change_values, status)
 
!     Input new values for beam centre and detector tilt if
!     required
        If (change_values) Then
 
!        Input X/Y pixel coordinate of beam centre
           Call F2D_INP_BEAMCENTRE (.False., experiment%x_beam, &
             experiment%y_beam, status)
        experiment%beam_centre_set = .True.
 
!        Input X/Y pixel sizes
           Call F2D_INP_PIXELSIZES (.False., experiment%x_pixel_size, &
             experiment%y_pixel_size, status)
        experiment%pixel_sizes_set = .True.
 
!        Input sample to detector distance
           Call F2D_INP_SAMPLEDISTANCE (.False., &
             experiment%detector_distance, status)
         experiment%detector_distance_set = .True.

!        Input detector tilt angles
           Call F2D_INP_DETECTORTILT (.False., experiment%tilt_plane_rotation, &
             experiment%tilt_angle, status)
        experiment%tilt_set = .True.
 
        End If
 
!     Input beam polarisation (at sample)
        Call F2D_INP_POLARISATION (experiment%correct_polarisation, &
          experiment%polarisation, lorentz_geometry, status)
        experiment%polarisation_set = .True.

!     Set-up default values
        If (first) Then
           first = .False.
           num_azimuthal = 180
           num_radial = Nint( Sqrt( ((xendelm - xstrelm + 1) / 2.0)**2 + &
             ((yendelm - ystrelm + 1) / 2.0)**2 ) )
           maximum_radius = num_radial * experiment%x_pixel_size
           maximum_2theta = Atan2(maximum_radius, experiment%detector_distance)
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Radial distance or equal 2 theta angle scan
        MESSAGE(1) = 'Enter: "YES" if you want to produce a 1-D ' // &
          'scan as a function of equal'
        MESSAGE(2) = '       angle pixels.'
        MESSAGE(3) = '       "NO" if you want to produce a 1-D ' // &
          'scan as a function of equal'
        MESSAGE(4) = '        radial distance pixels (on an ' // &
          'orthogonal detector).'
        MESSAGE(5) = '(The two are not the same.)'
        MESSAGE(6) = 'The best choice will depend on the ' // &
          'capabilities on any further'
        MESSAGE(7) = 'processing software. e.g. If a Rietveld ' // &
          'program requires equal angle'
        MESSAGE(8) = 'pixels then this is clearly the choice. ' // &
          'If either type of scan may be'
        MESSAGE(9) = 'treated then the equal radial re-binning ' // &
          'is probably perferable.'
        Call IO_INPL (.True., 0, 1, .True., 'PRODUCE EQUAL ANGLE PIXEL SCAN', &
          9, MESSAGE, 1, 'Enter "YES" or "NO"', angular_scan, status)
 
!     Number of azimuth angular and 2-theta/radial bins
        MESSAGE(1) = 'The region of interest, with the exception ' // &
          'of masked-off areas is'
        MESSAGE(2) = 'rebinned to a azimuth/radial binned array. ' // &
          'Where each pixel represents'
        MESSAGE(3) = 'an equal angular/radial area. You may ' // &
          'chose the number of azimuth and'
        MESSAGE(4) = 'radial bins to be used.'
        Call IO_INPI (.True., 1, xmaxdat, .True., 'NUMBER OF AZIMUTH BINS', 4, &
          MESSAGE, 1, 'Must be within given range', num_azimuthal, status)
 
        If (angular_scan) Then
           Call IO_INPI (.True., 1, ymaxdat, .True., &
             'NUMBER OF 2-THETA ANGLE BINS', 4, MESSAGE, 1, &
             'Must be within given range', num_radial, status)
        Else
           Call IO_INPI (.True., 1, ymaxdat, .True., &
             'NUMBER OF RADIAL DISTANCE BINS', 4, MESSAGE, 1, &
             'Must be within given range', num_radial, status)
        End If
 
        If (angular_scan) Then
 
!        Maximum 2-theta angle in pixels
           maximum_2theta = maximum_2theta * 180.0 / Pi
           MESSAGE(1) = 'Enter the maximum 2-theta angle (degrees)'
           MESSAGE(2) = 'for the output re-binned region.'
           Call IO_INPR (.True., 0.0, 90.0, .True., &
             'MAXIMUM 2-THETA LIMIT (DEGREES)', 2, MESSAGE, 1, &
             'Must be within given range', maximum_2theta, status)
           maximum_2theta = maximum_2theta * Pi / 180.0
 
        Else
 
!        Maximum radial distance in pixels
           maximum_radius = maximum_radius / experiment%x_pixel_size
           MESSAGE(1) = 'Enter the maximum ' // &
             'radial distance (X-pixel units)'
           MESSAGE(2) = 'for the output re-binned region.'
           Call IO_INPR (.True., 1.0, Real(Max(xendelm - xstrelm + 1, &
             yendelm - ystrelm+1)), .True., 'MAXIMUM RADIAL LIMIT (X PIXELS)', &
             2, MESSAGE, 1, 'Must be within given range', maximum_radius, &
             status)
           maximum_radius = maximum_radius * experiment%x_pixel_size
        End If
 
!     Set output pixel sizes
        azimuth_pixel_size = 2.0 * Pi / Real(num_azimuthal)
        If (angular_scan) Then
           radial_pixel_size = maximum_2theta / Real(num_radial)
        Else
           radial_pixel_size = maximum_radius / Real(num_radial)
        End If
 
!     Correct for spatial distortion
        MESSAGE(1) = 'YES: if a the detector spatial distortion ' // &
          'has been characterised and is'
        MESSAGE(2) = 'to be taken into account. If you answer ' // &
          '"YES" you will be required to'
        MESSAGE(3) = 'input the name of a valid spatial ' // &
          'distortion interpolation file.'
!     Call IO_INPL (.True., 0, 1, .True.,
!     :       'TAKE ACCOUNT OF SPATIAL DISTORTION', 3, MESSAGE,
!     :       1, 'Enter "YES" or "NO"',
!     :       spatial_distortion, status)
 
        MESSAGE(1) = 'The total integrated intensity in the ' // &
          'transformed region can be'
        MESSAGE(2) = 'preserved by answering "YES". The ' // &
          'integrated intensity in defined'
        MESSAGE(3) = 'azimuthal/radial regions should also be ' // &
          'approximately preserved.'
        MESSAGE(4) = 'However, for producing a number of 2 ' // &
          'theta scans, it is probably better'
        MESSAGE(5) = 'not to preserve total intensity. ' // &
          'Answering "NO" means that each output'
        MESSAGE(6) = 'pixel is normalised by the number of input pixels'
        Call IO_INPL (.True., 0, 1, .True., 'CONSERVE INTEGRATED INTENSITY', &
          6, MESSAGE, 1, 'Enter "YES" or "NO"', conserve_intensity, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Get dynamic work array space
        Allocate (WORK(num_azimuthal, num_radial), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_RTHETA ' // Version)
           Return
        End If

!     Calculate angular / radial re-binning
        Call F2D_RTHETA2 (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, MASK, conserve_intensity, angular_scan, experiment, &
          lorentz_geometry, maximum_2theta, maximum_radius, max_angular, &
          max_radial, num_azimuthal, num_radial, num_azimuthal, num_radial, &
          WORK, R_THETA, status)
 
!     Free dynamic array space
        Deallocate (WORK)
 
!     Define axis data
        If (angular_scan) Then
 
           Do pixel = 1, num_radial
              RADIAL_AXIS(pixel) = (Real(pixel) - 0.5) * radial_pixel_size * &
                180.0 / Pi
           End Do
 
        Else
 
           Do pixel = 1, num_radial
              RADIAL_AXIS(pixel) = (Real(pixel) - 0.5) * radial_pixel_size * &
                1000.0
           End Do
 
        End If
 
        Do pixel = 1, num_azimuthal
           ANGULAR_AXIS(pixel) = (Real(pixel) - 0.5) * 360.0 / &
             Real(num_azimuthal)
        End Do
 
     End If
 
!  Set title and labels
     If (angular_scan) Then
        mtitle = Trim(title) // ': Azimuth/2-theta'
        mxlabel = 'Azimuth (Degrees)'
        mylabel = '2-Theta Angle (Degrees)'
     Else
        mtitle = Trim(title) // ': Azimuth/Radial Distance'
        mxlabel = 'Azimuth (Degrees)'
        mylabel = 'Radial Distance (mm)'
     End If
 
     End Subroutine F2D_RTHETA
!********1*********2*********3*********4*********5*********6*********7*********8
 
