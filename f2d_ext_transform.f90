!********1*********2*********3*********4*********5*********6*********7********8
 
!  *************************
!  *                       *
!  * f2d_ext_transform.f90 *
!  *                       *
!  *************************
 
!+ F2D_EXT_TRANSFORM - Transform peak positions to reciprocal space
     Subroutine F2D_EXT_TRANSFORM (experiment, two_theta, chi, phi, omega, &
       max_peaks, start_peak, num_peaks, PEAKS, status)
!  Description:
!    Transform diffraction image peak positions to reciprocal space.
!  Keywords:
!    Reciprocal~Space.Transformation.Peak~Positions, 
!    Transformation.Reciprocal~Space.Peak~Positions,
!    Peak~Positions.Transformation.Reciprocal~Space
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Nov-2006: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Double Precision, Intent(IN) :: two_theta ! Two theta angle of detector 
!      (radians)
     Double Precision, Intent(IN) :: chi ! Crystal chi rotation angle (radians)
     Double Precision, Intent(IN) :: phi ! Crystal phi rotation angle (radians)
     Double Precision, Intent(IN) :: omega ! Crystal omega rotation angle 
!      (radians)
     Integer, Intent(IN) :: max_peaks ! Dimension of "PEAKS"
     Integer, Intent(IN) :: start_peak ! Starting peak for transformation
     Integer, Intent(IN) :: num_peaks ! Number of defined peaks
!  Import/Export:
     Type(PEAK_STRUCTURE), Intent(INOUT) :: PEAKS(max_peaks) ! Peak information
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Character(Len = 160) message ! User messages
     Real :: cos_detector ! Cosine of detector rotation
     Real :: cos_gamma ! Cosine of gamma
     Real :: cos_nu ! Cosine of nu
     Real :: inv_wave_angstroms ! Reciprocal of wavelength in Angstroms
!      i.e. 1.0 / (wavelength * 1.0e10)
     Real :: d ! Distance from sample to X-component of pixel position
     Real :: gamma ! Gamma (two theta) of pixel (radians)
     Integer :: peak ! Loop variable for peaks
     Real :: nu ! Nu of pixel (radians)
     Real :: proj ! Projection of a scattering vector in plane
     Real :: sin_detector ! Sine of detector rotation
     Real :: sin_gamma ! Sine of gamma
     Real :: sin_nu ! Sine of nu
     Real :: temp ! Temporary variable for swapping x and Y
     Real x_rotate ! Rotated X-coordinate
     Real :: xwc ! X-component of distance vector from beam centre
     Real y_rotate ! Rotated Y-coordinate
     Real :: ywc ! Y-component of distance vector from beam centre
!  Local Arrays:
     Real :: CHIMAT(3, 3) ! Chi rotation matrix
     Real :: CHIMATT(3, 3) ! Transpose of Chi rotation matrix
     Real :: OMEGAMAT(3, 3) ! Omega rotation matrix
     Real :: OMEGAMATT(3, 3) ! Transpose of Omega rotation matrix
     Real :: PHIMAT(3, 3) ! Phi rotation matrix
     Real :: PHIMATT(3, 3) ! Transpose of Phi rotation matrix
     Real :: ROT(9) ! Combined rotation matrix
     Real :: WORK(3, 3) ! Work matrix for calculating combined rotation matrix
     Real :: Z(3) ! Scattering vector of a pixel position
!  Internal Functions:
     Double Precision :: Degrees
     Double Precision :: rad
     Degrees(rad) = rad * 180.0 / Pi_d
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_EXT_TRANSFORM ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (max_peaks .Le. 0) Then
        status = St_bad_dim1
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_EXT_TRANSFORM ' // Version)
     Else

!      Output diffractometer angles
        If (experiment%vertical_2theta) Then
           Write (message, &
             '(''INFO: Vertical 2-theta rotation: angle = '', f9.4)') &
             Degrees(two_theta)
        Else
           Write (message, &
             '(''INFO: Horizontal 2-theta rotation: angle = '', f9.4)') &
             Degrees(two_theta)
        End If
        Call IO_WRITE (message, status)
        Write (message, '(''      chi = '', f9.4, '' phi = '', f9.4, ' // &
          ''' omega = '', g9.4, '' (Degrees)'')') &
          Degrees(chi), Degrees(phi), Degrees(omega)
        Call IO_WRITE (message, status)

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.

!     Calculate the reciprocal of the wavength in Angstrom units
        inv_wave_angstroms = 1.0 / (experiment%wavelength * 1.0e10)

!     Define Tranpose of chi, phi, omega rotation matrices
        Call F2D_CHIMAT (chi, CHIMAT)
        Call F2D_GMTRA (CHIMAT, CHIMATT, 3, 3)
        Call F2D_PHIMAT (phi, PHIMAT)
        Call F2D_GMTRA (PHIMAT, PHIMATT, 3, 3)
        Call F2D_PHIMAT (omega, OMEGAMAT)
        Call F2D_GMTRA (OMEGAMAT, OMEGAMATT, 3, 3)
 
!     Form combined rotation matrix
        Call F2D_GMPRD (CHIMATT, OMEGAMATT, WORK, 3, 3, 3)
        Call F2D_GMPRD (PHIMATT, WORK, ROT, 3, 3, 3)
 
        cos_detector = Cos(experiment%detector_rotation)
        sin_detector = Sin(experiment%detector_rotation)

!     Loop over peak
        Do peak = start_peak, num_peaks
 
!        Swap X and Y to cope with (default) vertical 2-theta
           xwc = (PEAKS(peak)%y_centre - experiment%y_beam) * &
             experiment%y_pixel_size
           ywc = (PEAKS(peak)%y_centre - experiment%x_beam) * &
             experiment%x_pixel_size

!        ??? Ideally need to correct for beam tilt

!        Rotate about beam
           x_rotate = cos_detector * xwc - sin_detector * ywc
           y_rotate = sin_detector * xwc + cos_detector * ywc

!        Mirror horizontal image displacement if required
           If (experiment%view_from_sample) Then
              y_rotate  = -y_rotate 
           End If
 
!       Swap x and y if horizontal 2-theta movement
           If (.Not. experiment%vertical_2theta) Then
              temp = x_rotate
              x_rotate = ywc
              y_rotate  = temp
           End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''x, y = '', 2i6)') x, y
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
 !        Find pixel gamma and nu
           d = Sqrt(x_rotate**2 + experiment%detector_distance**2)
           gamma = two_theta + &
             Atan2(x_rotate, experiment%detector_distance)
           nu = Atan2(y_rotate, d)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''gamma, nu = '', 2g12.5)') gamma, nu
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Find the scattering vector in cartesian coordinates
!        from gamma, nu, chi, phi and omega
           sin_gamma = Sin(gamma)
           cos_gamma = Cos(gamma)
           sin_nu = Sin(nu)
           cos_nu = Cos(nu)
           Z(1) = (sin_gamma * cos_nu) * inv_wave_angstroms
           Z(2) = (cos_gamma * cos_nu - 1.0) * inv_wave_angstroms
           Z(3) = sin_nu * inv_wave_angstroms
 
!        Apply combined rotation matrix
           PEAKS(peak)%a_star = ROT(1) * Z(1) + ROT(4) * Z(2) + ROT(7) * Z(3)
           PEAKS(peak)%b_star = ROT(2) * Z(1) + ROT(5) * Z(2) + ROT(8) * Z(3)
           PEAKS(peak)%c_star = ROT(3) * Z(1) + ROT(6) * Z(2) + ROT(9) * Z(3)
 
        End Do
 
     End If
 
     End Subroutine F2D_EXT_TRANSFORM
!********1*********2*********3*********4*********5*********6*********7********8
 
