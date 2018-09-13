!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************************
!  *                            *
!  * f2d_rmap_cal_cartesian.f90 *
!  *                            *
!  ******************************
 
!+ F2D_RMAP_CAL_CARTESIAN - Calculate cartesian coordinate position in 
!  reciprocal space
     Subroutine F2D_RMAP_CAL_CARTESIAN (info, x_coordinate, y_coordinate, &
       experiment, Z, status)
!  Description:
!    Calculate cartesian reciprocal space position of a given detector 
!    coordinate
!  Keywords:
!    Reciprocal~Space.Cartesian.Transformation, 
!    Cartesian.Transformation.Reciprocal~Space
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    22-Jun-2006: V0.2 Reverse image sense (Hammersley)
!    18-Apr-2006: V0.1 Original based on "F2D_RMAP_CAL_COORDINATE" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: info ! level of output information:
!      0 = None
!      1 = Output D-spacing etc.
     Real, Intent(IN) :: x_coordinate ! X-pixel coordinate on detector
     Real, Intent(IN) :: y_coordinate ! Y-pixel coordinate on detector
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: Z(3) ! Scattering vector in cartesian coordinates
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: i ! Loop variable
     Integer :: j ! Loop variable
     Real :: inv_wave_angstroms ! Inverse of wavelength in Angstrom units
     Real :: cos_gamma ! Cosine of gamma
     Real :: cos_nu ! Cosine of nu
     Real :: sin_gamma ! Sine of gamma
     Real :: sin_nu ! Sine of nu
     Real :: d ! Distance from sample to X-component of pixel position
     Real :: d_spacing ! Equivalent d-spacing
     Real :: gamma ! Gamma (two theta) of pixel (radians)
     Real :: nu ! Nu of pixel (radians)
     Real :: proj ! Projection of a scattering vector in plane
     Real :: rad ! Dummy variable
     Real :: temp ! Temporary variable for swapping x and Y
     Real :: xwc ! X-component of distance vector from beam centre
     Real :: ywc ! Y-component of distance vector from beam centre
!  Local Arrays:
     Real :: CHIMAT(3, 3) ! Chi rotation matrix
     Real :: CHIMATT(3, 3) ! Transpose of Chi rotation matrix
     Real :: OMEGAMAT(3, 3) ! Omega rotation matrix
     Real :: OMEGAMATT(3, 3) ! Transpose of Omega rotation matrix
     Real :: PHIMAT(3, 3) ! Phi rotation matrix
     Real :: PHIMATT(3, 3) ! Transpose of Phi rotation matrix
     Real :: ROTMAT(9) ! Combined rotation matrix
     Real :: WORK(3, 3) ! Work matrix for calculating combined rotation matrix
     Real :: Z1(3) ! Scattering vector of a pixel position
!  Internal Functions:
     Real :: Degrees
     Degrees(rad) = rad * 180.0 / 3.1415926
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_CAL_CARTESIAN ' // Version)
        Return
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_RMAP_CAL_CARTESIAN ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Calculate the reciprocal of the wavength in Angstrom units
        inv_wave_angstroms = 1.0 / (experiment%wavelength * 1.0e10)
 
!     Define Tranpose of chi, phi, omega rotation matrices
        Call F2D_CHIMAT (experiment%chi_start, CHIMAT)
        Call F2D_GMTRA (CHIMAT, CHIMATT, 3, 3)
        Call F2D_PHIMAT (experiment%phi_start, PHIMAT)
        Call F2D_GMTRA (PHIMAT, PHIMATT, 3, 3)
        Call F2D_PHIMAT (experiment%omega_start, OMEGAMAT)
        Call F2D_GMTRA (OMEGAMAT, OMEGAMATT, 3, 3)
 
!     Form combined rotation matrix
        Call F2D_GMPRD (CHIMATT, OMEGAMATT, WORK, 3, 3, 3)
        Call F2D_GMPRD (PHIMATT, WORK, ROTMAT, 3, 3, 3)

        If (info .Ge. 1) Then
           Write (message, '(''x, y = '', 2g12.5)') x_coordinate, y_coordinate
           Call IO_WRITE (message, status)
        End If

!     Swap X and Y to cope with (default) vertical 2-theta
        xwc = (y_coordinate - experiment%y_beam) * experiment%y_pixel_size
        ywc = (x_coordinate - experiment%x_beam) * experiment%x_pixel_size

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''experiment%view_from_sample = '', l1)') &
!          experiment%view_from_sample
!        Write (*, '(''experiment%vertical_2theta = '', l1)') &
!          experiment%vertical_2theta
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Mirror horizontal image displacement if required
        If (experiment%view_from_sample) Then
           ywc = -ywc
        End If
 
!     Swap x and y if horizontal 2-theta movement
        If (.Not. experiment%vertical_2theta) Then
           temp = xwc
           xwc = ywc
           ywc = temp
        End If
 
!     Find pixel gamma and nu
        d = Sqrt(xwc**2 + experiment%detector_distance**2)
        gamma = experiment%two_theta + Atan2(xwc, experiment%detector_distance)
        nu = Atan2(ywc, d)
 
        If (info .Ge. 1) Then
           Write (message, '(''gamma, nu = '', 2g12.5)') Degrees(gamma), &
             Degrees(nu)
           Call IO_WRITE (message, status)
        End If

        d_spacing = experiment%wavelength / (2.0 * Sin(gamma / 2.0))

!        If (info .Ge. 2) Then
 
!           Write (message, '(''INFO: D-spacing Gamma (Angstroms) = '', f12.5)')&
!             d_spacing * 1.0e10
!           Call IO_WRITE (message, status)
!        End If
        d_spacing = experiment%wavelength / (2.0 * Sin(nu / 2.0))
 
!        If (info .Ge. 2) Then
!           Write (message, '(''INFO: D-spacing Nu (Angstroms) = '', f12.5)') &
!             d_spacing * 1.0e10
!           Call IO_WRITE (message, status)
!        End If

!     Find the scattering vector in cartesian coordinates from gamma, nu, chi, 
!     phi and omega
        sin_gamma = Sin(gamma)
        cos_gamma = Cos(gamma)
        sin_nu = Sin(nu)
        cos_nu = Cos(nu)
        Z1(1) = (sin_gamma * cos_nu) * inv_wave_angstroms
        Z1(2) = (cos_gamma * cos_nu - 1.0 ) * inv_wave_angstroms
        Z1(3) = sin_nu * inv_wave_angstroms
 
        If (info .Ge. 1) Then
           Write (message, '(''Cartesian coordinate = '', 3g12.4)') &
             Z1(1), Z1(2), Z1(3)
           Call IO_WRITE (message, status)
        End If

!     Apply combined rotation matrix
!     OLD-CODE
!     Call F2D_GMPRD (ROTMAT, Z, Z1, 3, 3, 1)
!     NEW-CODE
        Z(1) = ROTMAT(1) * Z1(1) + ROTMAT(4) * Z1(2) + ROTMAT(7) * Z1(3)
        Z(2) = ROTMAT(2) * Z1(1) + ROTMAT(5) * Z1(2) + ROTMAT(8) * Z1(3)
        Z(3) = ROTMAT(3) * Z1(1) + ROTMAT(6) * Z1(2) + ROTMAT(9) * Z1(3)
!     END OF NEW CODE

     End If

     End Subroutine F2D_RMAP_CAL_CARTESIAN
!********1*********2*********3*********4*********5*********6*********7*********8
 
