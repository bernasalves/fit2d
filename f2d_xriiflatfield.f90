!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_xriiflatfield.f90 *
!  *                       *
!  *************************
 
!+ F2D_XRIIFLATFIELD: FLAT FIELD calibration correction
     Subroutine F2D_XRIIFLATFIELD (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, x_pixel_size, y_pixel_size, DATA, status)
!  Description:
!    Corrects data from an X-ray image intensifier system (or similar
!    system) which has a spherical detection surface which curves
!    away from the X-ray source. Similarly, the system may have a
!    spherical protective layer typically made out of aluminium or
!    Beryllium which attenuates the X-rays before they can be
!    absorbed in the detection layer. The program calculates the
!    theoretical absorption in both layers, which will change as a
!    function of angle, source position, etc. From the theoretical
!    absorption in the detection layer a normalisation curve can be
!    calculated and the input image can be corrected, based on a
!    central position. At present the source position must be on-axis.
!    An input "raw" flat-field image is thus corrected (output
!    in the current data array i.e. the original data is
!    over-writen).
!
!    Note: A real system is very likely to suffer from other
!    non-uniform response owing to uneven deposition of the detection
!    layer. Clearly, this simple theoretical model does not account
!    for this. If a good flat field calibration measured has been
!    performed, it should be used instead.
!  Keywords:
!    Calibrate.Flat-Field.Theoretical, Flat-Field.Calibration
!  Method:
!    1. Input sample to detector distance, pixel sizes, centre of
!    flat-field measurement, etc.
!    2. For each angle calculate intersection of ray from source
!    position to detection position.
!    3. The equation of the line (ray) can now be calculated.
!    4. The intersection points of the ray with the other inner and
!    outer spheres can be calculated by solving the two simultaneous
!    equations.
!    5. From the intersection positions the path length through the
!    two different layers may be calculated.
!    6. The absorption in the two different layers may be calculated
!    to give the fractional absorption in the detection layer.
!    7. The radial correction profile may be calculated by
!    normalising to the on-axis absorption.
!    8. The radial correction profile can be used to correct the
!    entire image.
!
!    Note: The mathematics of the method are described more fully in
!    14:97.
!  Deficiencies:
!    UNFINISHED WORK, DOESN'T CORRECT THE "DATA" ARRAY
!    Doesn't handle off-axis source positions
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    28-Mar-2006: V0.2 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    24-Jun-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
!  Import/Export:
     Real, Intent(INOUT) :: x_pixel_size ! Pixel size in metres in X-direction
     Real, Intent(INOUT) :: y_pixel_size ! Pixel size in metres in Y-direction
!  Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Corrected flat-field
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: max_pixels ! Dimension size for profile array
     Integer stat ! Status return variable for "Allocate"
     Logical, Save :: first = .True. ! .True., if the subroutine is being
!      used for the first time
     Real, Save :: absorb_detect = 0.0 ! Absorption coefficient of the
!      detective layer
     Real, Save :: absorb_protect = 0.0 ! Absorption coefficient of the
!      protective layer
     Real, Save :: radius_detect = 0.4 ! Outer radius of detection surface
     Real, Save :: radius_protect = 0.45 ! Outer radius of protection layer
     Real, Save :: source_distance = 0.30 ! Distance from source to outer
!      radius of protective layer
     Real, Save :: width_detect = 0.0005 ! Radial width of detection layer
!      in metres
     Real, Save :: width_protect = 0.0008 ! Radial width of protection
!      layer in metres
     Real, Save :: x_centre ! X-position of centre of symmetry of detector
     Real, Save :: y_centre ! Y-position of centre of symmetry of detector
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(5) ! User messages
     Real, Allocatable :: PROFILE(:) ! Dynamically allocated array
!  Internal Functions:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_XRIIFLATFIELD ' // Version)
     Else
 
!     Default values for experimental geometry
        If (first) Then
           x_centre = Real(xstrelm + xendelm) / 2.0
           y_centre = Real(ystrelm + yendelm) / 2.0
           first = .False.
        End If
 
!     Set default pixel sizes if not defined
        If (x_pixel_size .Eq. 0.0 .And. y_pixel_size .Eq. 0.0) Then
           x_pixel_size = 150.0e-6
           y_pixel_size = 150.0e-6
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Inform user of basic detector model
        Call IO_WRITE ('INFO: The detector system is ' // &
          'modelled by two spheres of finite thicknesses:', status)
        Call IO_WRITE ('      an inner sphere where the ' // &
          'X-rays are detected, and an outer sphere', status)
        Call IO_WRITE ('      (vacuum vessel or the ' // &
          'detector) which attenuates the X-ray prior', status)
        Call IO_WRITE ('      to possible detection. ' // &
          'The source position is assumed to be on-axis.', status)
        Call IO_WRITE (' ', status)
 
!     Enter geometry of detector system
 
!     Radius of detection layer
        MESSAGE(1) = 'Enter radius of curvature of the outer ' // &
          'surface of the X-ray detection'
        MESSAGE(2) = 'layer. Units are in metres. (For the ' // &
          'ESRF XRII/CCD Beryllium system'
        MESSAGE(3) = 'radius = 0.4m???, and the Aluminium ' // &
          'system radius = 0.5m???)'
        Call IO_INPR (.True., 0.001, 10.0, .True., &
          'RADIUS OF DETECTION LAYER (OUTER SURFACE: METRES) ', 3, MESSAGE, 1, &
          'Must be valid real number within given range', radius_detect, &
          status)
 
!     Thickness of detection layer
        Call IO_INPR (.True., 1.0e-9, 1.0e-1, .True., &
          'RADIAL THICKNESS OF DETECTION LAYER (METRES) ', 1, &
          'Enter thickness of X-ray detection layer (metres)', 1, &
          'Must be valid real number within given range', width_detect, &
          status)
 
!     Aborption coefficient for detection layer
        MESSAGE(1) = 'Enter absorption coefficient for the ' // &
          'detection layer at the photon'
        MESSAGE(2) = 'wavelength/energy you wish to correct. ' // &
          'The units are per metre and NOT'
        MESSAGE(3) = 'per centimetre. If you have a value in ' // &
          'per centimetre units you should'
        MESSAGE(4) = 'multiply it by 100. Values for the ESRF ' // &
          'XRII/CCD system at different'
        MESSAGE(5) = 'photon energies/wavelengths are: (to be ' // 'defined)'
        Call IO_INPR (.True., 1.0e-9, 1.0e-1, .True., &
          'DETECTION LAYER ABSORPTION COEFFICIENT (PER METRE)', 5, MESSAGE, 1, &
          'Must be valid real number within given range', absorb_detect, &
          status)
 
!     Radius of protection layer
        MESSAGE(1) = 'Enter radius of curvature of the outer ' // &
          'surface of the vacuum vessel'
        MESSAGE(2) = 'layer. Units are in metres. (For the ' // &
          'ESRF XRII/CCD Beryllium system'
        MESSAGE(3) = 'radius = 0.4m???, and the Aluminium ' // &
          'system radius = 0.5m???)'
        Call IO_INPR (.True., 0.001, 10.0, .True., &
          'RADIUS OF VACUUM LAYER (OUTER SURFACE: METRES) ', 3, MESSAGE, 1, &
          'Must be valid real number within given range', radius_protect, &
          status)
 
!     Thickness of vaccum protection layer
        Call IO_INPR (.True., 1.0e-9, 1.0e-1, .True., &
          'RADIAL THICKNESS OF VACCUM/PROTECTION LAYER (METRES)', 1, &
          'Enter thickness of vacuum/protection layer (metres)', 1, &
          'Must be valid real number within given range', width_protect, &
          status)
 
!     Aborption coefficient for detection layer
        MESSAGE(1) = 'Enter absorption coefficient for the ' // &
          'detection layer at the photon'
        MESSAGE(2) = 'wavelength/energy you wish to correct. ' // &
          'The units are per metre and NOT'
        MESSAGE(3) = 'per centimetre. If you have a value in ' // &
          'per centimetre units you should'
        MESSAGE(4) = 'multiply it by 100. Values for the ESRF ' // &
          'XRII/CCD system at different'
        MESSAGE(5) = 'photon energies/wavelengths are: (to be defined)'
        Call IO_INPR (.True., 1.0e-9, 1.0e-1, .True., &
          'DETECTION LAYER ABSORPTION COEFFICIENT (PER METRE)', 5, MESSAGE, 1, &
          'Must be valid real number within given range', absorb_protect, &
          status)
 
!     Input centre of flat field on image
        Call IO_INPR (.False., 0.0, 0.0, .True., &
          'FLAT FIELD CENTRE X-COORDINATE ', 1, &
          'Enter X-coordinate of centre flat field measurement', 1, &
          'Must be valid real number', x_centre, status)
        Call IO_INPR (.False., 0.0, 0.0, .True., &
          'FLAT FIELD CENTRE Y-COORDINATE ', 1, &
          'Enter Y-coordinate of centre of flat field measurement', 1, &
          'Must be valid real number', y_centre, status)
 
!     Input sample to detector distance
        MESSAGE(1) = 'Enter the distance from the centre of ' // &
          'the sample to the outer edge of'
        MESSAGE(2) = 'the detector vaccum vessel in metres'
        Call IO_INPR (.True., 0.0001, 100000.0, .True., &
          'SAMPLE TO DETECTOR (OUTER VACUUM VESSEL) DISTANCE (METRES)', &
          2, MESSAGE, 1, 'Must be valid real number', source_distance, status)
 
!     Input pixel sizes
        x_pixel_size = x_pixel_size * 1.0e6
        Call IO_INPR (.True., 0.001, 10000.0, .True., &
          'RAW PIXEL X-SIZE (MICRONS)', 1, &
          'Enter X-size of one raw pixel in microns ', 1, &
          'Must be valid real number', x_pixel_size, status)
        x_pixel_size = x_pixel_size * 1.0e-6
        y_pixel_size = y_pixel_size * 1.0e6
        Call IO_INPR (.True., 0.001, 10000.0, .True., &
          'RAW PIXEL Y-SIZE (MICRONS)', 1, &
          'Enter Y-size of one raw pixel in microns ', 1, &
          'Must be valid real number', y_pixel_size, status)
        y_pixel_size = y_pixel_size * 1.0e-6
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate maximum distance from flat field centre to edge of
!     flat field image (in pixels)
        max_pixels = Int(Max( Sqrt((x_centre - Real(xstrelm))**2 + (y_centre - &
          Real(ystrelm))**2), Sqrt((Real(xendelm) - x_centre)**2 + &
          (y_centre-Real(ystrelm))**2), Sqrt((x_centre - Real(xstrelm))**2 + &
          (Real(yendelm) - y_centre)**2), Sqrt((Real(xendelm) - x_centre)**2 + &
          (Real(yendelm) - y_centre) **2))) + 1
 
!     Create dynamic array to hold 1-D source distribution and geometry 
!     correction profile
        Allocate (PROFILE(max_pixels + 1), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_XRIIFLATFIELD ' // Version)
           Return
        End If

!     Calculate 1-D source distribution and geometry correction profile
        Call F2D_CALABSORPTION ( radius_detect, width_detect, absorb_detect, &
          radius_protect, width_protect, absorb_protect, source_distance, &
          x_pixel_size, max_pixels, PROFILE, status)
 
!     Apply correcting profile to input flat field measurement
!     Call F2D_CALFLATFIELD (max_pixels, PROFILE,
!     :       x_centre, y_centre, x_pixel_size, y_pixel_size,
!     :       xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, yendelm,
!     :       DATA, status)
 
!     Free profile array
        Deallocate (PROFILE)
 
     End If
 
     End Subroutine F2D_XRIIFLATFIELD
!********1*********2*********3*********4*********5*********6*********7*********8
