!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************************
!  *                           *
!  * f2d_powderdiffraction.f90 *
!  *                           *
!  *****************************
 
!+ F2D_POWDERDIFFRACTION -  POWDER DIFFRACTION fitting
     Subroutine F2D_POWDERDIFFRACTION (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, XAXIS, YAXIS, DATA, MASK, variances_exist, title, &
       max_radial, experiment, lorentz_geometry, num_radial, RADAXIS, &
       RPROFILE, PROERRS, radial_pixel_size, rtitle, rxlabel, rzlabel, status)
!  Description:
!    The user has the option of changing the beam centre / tilt parameters and
!    calculating a radial profile. Spatial distortion may be accounted for in 
!    fitting the centre and tilt and in calculating the radial profile.
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
!    28-Mar-2006: V0.20 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    13-Mar-2006: V0.19 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    16-Dec-1996: V0.18 Avoid open strings crossing lines (Hammersley)
!    11-Feb-1996: V0.17 Changes to "F2D_INP_SAMPLEDISTANCE" (Hammersley)
!    02-Feb-1996: V0.16 Change to "F2D_INP_BEAMCENTRE" arguments (Hammersley)
!    06-Nov-1995: V0.15 Set default angular pixel size to be in
!      units of 1000th of a degree (Hammersley)
!    23-Oct-1995: V0.14 Lorentz correction to an uncorrected 2-theta scan 
!      added (Hammersley)
!    01-Sep-1995: V0.13 Separate 2-theta and azimuthly polarisation
!      corrections and add correction for Lorentz (Hammersley)
!    28-Aug-1995: V0.12 Take account of polarisation (Hammersley)
!    20-Apr-1995: V0.11 Output radial pixel size (Hammersley)
!    05-Apr-1995: V0.10 Allow pixel sizes to be changed (Hammersley)
!    15-Mar-1995: V0.9 Choice of spatial distortion correction (Hammersley)
!    28-Feb-1995: V0.8 Make mask elements single bytes (Hammersley)
!    22-Feb-1995: V0.7 Option of 2 theta scan and to save data in
!      "POWDER DIFFRACTION STANDARD" data file (Hammersley)
!    19-Jan-1995: V0.6 Only perform 1-D radial scan here, the tilt and beam 
!      centre calculation has been move to "F2D_TILTCENTRE" (Hammersley)
!    17-Jan-1995: V0.5 Store spline coefficients in double precision arrays 
!      (Hammersley)
!    30-Sep-1994: V0.4 Separate tilt fitting code into "F2D_FINDTILT"
!      (Hammersley)
!    28-Sep-1994: V0.3 Calculate 1-D radial profile taking account tilt 
!      (Hammersley)
!    30-Aug-1994: V0.2 Check alternative tilt angle solutions (Hammersley)
!    23-Aug-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'io.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Array containing data Y-coordinates
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Logical, Intent(IN) :: variances_exist ! .True., if the variances array
!      exists
     Character(Len = *), Intent(IN) :: title ! Title for original 2-D data
     Integer, Intent(IN) :: max_radial ! Dimension of array "RAD_AXIS",
!      "PROFILE" and "NUMPIXELS"
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(INOUT) :: lorentz_geometry ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the
!            equivalent of a 2-theta scan i.e. detector at equal distance
!  Export:
     Integer, Intent(OUT) :: num_radial ! Number of radial profile pixels
     Real, Intent(OUT) :: RADAXIS(max_radial) ! The radial positions at
!      which the radial profile has been calculated
     Real, Intent(OUT) :: RPROFILE(max_radial) ! The averaged radial profile
!      calculated from a user input centre
     Real, Intent(OUT) :: PROERRS(max_radial) ! The estimated standard
!      variances in the radial profile
     Real, Intent(OUT) :: radial_pixel_size ! Size of pixel to be used for
!      the radial 1-D histogram. If an equal radial distance scan is to
!      be calculated ("angular_scan = .False.") the units are metres, and for 
!      an equal angle pixel scan ("angular_scan = .True.") the units are radians
     Character(Len = *), Intent(OUT) :: rtitle ! Title label for 1-D profile
     Character(Len = *), Intent(OUT) :: rxlabel ! X-axis label for 1-D profile
     Character(Len = *), Intent(OUT) :: rzlabel ! Z-axis label for 1-D profile
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.20' ! Version number
!  Local Variables:
     Integer stat ! Status return variable for "Allocate"
     Logical, Save :: angular_scan = .True. ! .True., if the 1-D scan is to
!      be produced in equal angle elements as opposed to equal radial distance
!      elements
     Logical, Save :: change_values = .False. ! .True., if refined parameters 
!      values for the beam centre and/or the tilt are to be edited by the user
     Logical, Save :: spatial_distortion = .False. ! .True., if spatial
!      distortion is to be taken into account
     Logical, Save :: save_data = .True. ! .True., if the data is to be
!      saved into an output file (ASCII) according to the "POWDER DIFFRACTION
!      STANDARD" (PDS) format
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(9) ! User text
     Real, Allocatable :: WORK(:) ! Dynamic array: Work array for 
!      "F2D_TILTRADIAL" used to store fractions of pixels contributing to each 
!      radial pixel
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_POWDERDIFFRACTION ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_POWDERDIFFRACTION ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
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
 
!     Input new values for beam centre and detector tilt if required
        If (change_values) Then
 
!        Input X/Y pixel coordinate of beam centre
           Call F2D_INP_BEAMCENTRE (.False., &
             experiment%x_beam, experiment%y_beam, status)
 
!        Input X/Y pixel sizes
           Call F2D_INP_PIXELSIZES (.False., &
             experiment%x_pixel_size, experiment%y_pixel_size, status)
 
!        Input sample to detector distance
           Call F2D_INP_SAMPLEDISTANCE (.False., experiment%detector_distance, &
             status)
 
!        Input detector tilt angles
           Call F2D_INP_DETECTORTILT (.False., &
             experiment%tilt_plane_rotation, experiment%tilt_angle, status)
 
        End If
 
!     Input beam polarisation (at sample)
        Call F2D_INP_POLARISATION (experiment%correct_polarisation, &
          experiment%polarisation, lorentz_geometry, status)
 
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
 
        If (angular_scan) Then
 
!        Input equal angular step spacing between pixels for the
!        calculation of the radial profile in radians
           radial_pixel_size = Atan2( &
             (experiment%x_pixel_size + experiment%y_pixel_size) / 2.0, &
             experiment%detector_distance) * 180.0 / Pi
 
!        Round radial pixel size to nearest 1000th of a degree
!        (this is for the PDS file format and subsequent programs)
           radial_pixel_size = (Nint(radial_pixel_size * 1000.0)) / 1000.0
           MESSAGE(1) = 'Enter required angle step between bins ' // &
             'for the calculation of the 1-D'
           MESSAGE(2) = '2 theta scan. (The default value ' // &
             'corresponds to the angular size of'
           MESSAGE(3) = 'the average pixel size at the beam centre.)'
           Call IO_INPR (.True., 0.001, 10000.0, .True., &
             '2 THETA SCAN ANGULAR PIXEL STEP (DEGREES)', 3, MESSAGE, 1, &
             'Must be within given range', radial_pixel_size, status)
           radial_pixel_size = radial_pixel_size * Pi / 180.0
 
        Else
 
!        Input world coordinate system spacing between pixels for the
!        calculation of the radial profile in metres
           radial_pixel_size = &
             (experiment%x_pixel_size + experiment%y_pixel_size) / 2.0 * 1.0e6
           MESSAGE(1) = 'Enter required spacing between pixel ' // &
             'positions for the calculation of'
           MESSAGE(2) = 'the radial profile i.e. Each pixel in ' // &
             'the radial profile should correspond'
           MESSAGE(3) = 'to what pixel distance from the image.'
           Call IO_INPR (.True., 0.001, 10000.0, .True., &
             'RADIAL PIXEL SIZE (MICRONS)', 3, MESSAGE, 1, &
             'Must be within given range', radial_pixel_size, status)
           radial_pixel_size = radial_pixel_size / 1.0e6
 
        End If
 
!     Correct for spatial distortion
        MESSAGE(1) = 'YES: if a the detector spatial distortion ' // &
          'has been characterised and is'
        MESSAGE(2) = 'to be taken into account. If you answer ' // &
          '"YES" you will be required to'
        MESSAGE(3) = 'input the name of a valid spatial ' // &
          'distortion interpolation file.'
        Call IO_INPL (.True., 0, 1, .True., &
          'TAKE ACCOUNT OF SPATIAL DISTORTION', 3, MESSAGE, 1, &
          'Enter "YES" or "NO"', spatial_distortion, status)
 
!     Get dynamic work array space
        Allocate (WORK(max_radial), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_POWDERDIFFRACTION ' // Version)
           Return
        End If

!     Special integration if spatial distortion is to be corrected
        If (spatial_distortion) Then
 
!        Calculate 1-D radial profile of data
           Call F2D_SRADIAL (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, XAXIS, YAXIS, DATA, MASK, variances_exist, experiment, &
             lorentz_geometry, angular_scan, radial_pixel_size, max_radial, &
             num_radial, RADAXIS, RPROFILE, PROERRS, WORK, status)
 
        Else
 
!        Calculate 1-D radial profile of data
           Call F2D_TILTRADIAL (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, DATA, MASK, variances_exist, experiment, &
             lorentz_geometry, angular_scan, radial_pixel_size, max_radial, &
             num_radial, RADAXIS, RPROFILE, PROERRS, WORK, status)
 
        End If
 
!     Apply any Lorentz corrections to the 1-D intensities
        Call F2D_LORENTZ (lorentz_geometry, variances_exist, max_radial, &
          num_radial, RADAXIS, RPROFILE, PROERRS, status)
 
        If (angular_scan) Then
 
!        Find out if data is to be saved in an output file
           MESSAGE(1) = 'If the data is to be Rietveld refined ' // &
             'it is probably appropriate to'
           MESSAGE(2) = 'output it now into a "PDS" file ' // &
             '(Powder Diffraction Standard) file.'
           MESSAGE(3) = '(The scan will be saved in the ' // &
             'memory, but the number of pixels'
           MESSAGE(4) = 'contributing to each scan bin will ' // &
             'be lost if the file is not created now.)'
           Call IO_INPL (.True., 0, 1, .True., &
             'SAVE DATA IN "POWDER DIFFRACTION STANDARD" FORMAT', 4, MESSAGE, &
             1, 'Enter "YES" or "NO"', save_data, status)
 
           If (save_data) Then
 
!           Save data to ASCII file
              Call FIO_OUT_PDS (experiment%detector_gain, title, 0.0, &
                radial_pixel_size, &
                max_radial, num_radial, RPROFILE, WORK, status)
 
           End If
 
        End If
 
!     Free dynamic array space
        Deallocate (WORK)
 
!     Set text information
        If (angular_scan) Then
           rtitle = Trim(title) // ': Angular Profile'
           rxlabel = 'Two-Theta Angle (degrees)'
        Else
           rtitle = Trim(title) // ': Radial Profile'
           rxlabel = 'Radial distance (mm)'
        End If
 
        rzlabel = 'Intensity (normalised)'
 
     End If
 
     End Subroutine F2D_POWDERDIFFRACTION
!********1*********2*********3*********4*********5*********6*********7*********8
