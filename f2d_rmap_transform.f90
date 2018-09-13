!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_rmap_transform.f90 *
!  *                        *
!  **************************
 
!+ F2D_RMAP_TRANSFORM - Transform a diffraction image to reciprocal space
     Subroutine F2D_RMAP_TRANSFORM (xmaxmap, ymaxmap, zmaxmap, xnummap, &
       ynummap, znummap, C1, C2C1, c2c1s, C3C1, c3c1s, C5, CN, step, &
       thickness, two_theta, chi, phi, omega, &
       xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, yendelm, &
       experiment, inv_wave_angstroms, DATA, MASK, MAP, NORMALISE, status)
!  Description:
!    Transform a diffraction image to reciprocal map array.
!  Keywords:
!    Reciprocal~Space.Transformation, Transformation.Reciprocal~Space
!  Method:
!  Deficiencies:
!    Doesn't take acount of detector axes being rotated relative to 
!    diffractometer axes
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    27-Nov-2014: V0.15 Add extra figures to output to please Intel compiler
!      (Hammersley)
!    11-Sep-2006: V0.14 Add support for monitor and attenuation normalisation
!      (Hammersley)
!    29-Aug-2006: V0.13 Add rotation about beam (Hammersley)
!    22-Jun-2006: V0.12 Remove redundant argument variables (Hammersley)
!    27-Mar-2006: V0.11 Add masking (Hammersley)
!    23-Mar-2006: V0.10 Divide message into two lines (Hammersley)
!    20-Mar-2006: V0.9 Formalise angle output message (Hammersley)
!    10-Mar-2006: V0.8 Use "EXPERIMENTAL_DETAILS" structure. Angles are now 
!      in double precision (Hammersley)
!    28-Oct-2005: V0.7 Add option to mirror images (Hammersley)
!    25-Oct-2005: V0.6 Change the way detector horizontal or vertical
!      position is handled (Hammersley)
!    14-Oct-2005: V0.5 Correct (swap) vertical and horizontal two theta
!      values (Hammersley)
!    08-Jun-2005: V0.4 Add detector 2-theta angles horizontal as
!      well as vertical (Hammersley)
!    13-May-2005: V0.3 Convert all angles internally to radians,
!      and input inverse wavelength in Angstroms (Hammersley)
!    10-May-2005: V0.2 Debugging (Hammersley)
!    21-Apr-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import:
     Integer, Intent(IN) :: xmaxmap ! First dimension of reciprocal map
     Integer, Intent(IN) :: ymaxmap ! Second dimension of reciprocal map
     Integer, Intent(IN) :: zmaxmap ! Third dimension of reciprocal map
     Integer, Intent(IN) :: xnummap ! Number of defined pixels in first
!      dimension of map
     Integer, Intent(IN) :: ynummap ! Number of defined pixels in second
!      dimension of map
     Integer, Intent(IN) :: znummap ! Number of defined pixels in third
!      dimension of map
     Real, Intent(IN) :: C1(3) ! Starting coordinate of first section
     Real, Intent(IN) :: C2C1(3) ! Difference vector C2 - C1
     Real, Intent(IN) :: c2c1s ! Square of length of difference vector C2 - C1
     Real, Intent(IN) :: C3C1(3) ! Difference vector C3 - C1
     Real, Intent(IN) :: c3c1s ! Square of length of difference vector C3 - C1
     Real, Intent(IN) :: C5(3) ! Difference vector between sections
     Real, Intent(IN) :: CN(3) ! Normalised difference vector
     Real, Intent(IN) :: step ! Step between sections
     Real, Intent(IN) :: thickness ! Thickness of a single section
     Double Precision, Intent(IN) :: two_theta ! Two theta angle of detector 
!      (radians)
     Double Precision, Intent(IN) :: chi ! Crystal chi rotation angle (radians)
     Double Precision, Intent(IN) :: phi ! Crystal phi rotation angle (radians)
     Double Precision, Intent(IN) :: omega ! Crystal omega rotation angle 
!      (radians)
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Real, Intent(IN) :: inv_wave_angstroms ! Inverse of wavelength in
!      Angstrom units
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: MAP(xmaxmap, ymaxmap, zmaxmap) ! Reciprocal space map
     Real, Intent(OUT) :: NORMALISE(xmaxmap, ymaxmap, zmaxmap)
!      Normalisation array
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.14' ! Version number
!  Local Variables:
     Character(Len = 160) message ! User messages
     Integer :: k ! Loop variable for dimensions
     Integer :: xm ! X-pixel number in map
     Integer :: x ! Loop variable for 1st dimension
     Integer :: y ! Loop variable for 2nd dimension
     Integer :: ym ! Y-pixel number in map
     Integer :: z ! Loop variable for 3rd dimension
     Integer :: zm ! Z-pixel number in map
     Real :: c5dist ! Length of "C5"
     Real :: cos_detector ! Cosine of detector rotation
     Real :: cos_gamma ! Cosine of gamma
     Real :: cos_nu ! Cosine of nu
     Real :: half_thickness ! Half the thickness between slices
     Real :: monitor ! Monitor value for frame
     Real :: scale_factor ! Scale factor for scaling intensities
     Real :: sin_detector ! Sine of detector rotation
     Real :: sin_gamma ! Sine of gamma
     Real :: sin_nu ! Sine of nu
     Real :: d ! Distance from sample to X-component of pixel position
     Real :: gamma ! Gamma (two theta) of pixel (radians)
     Real :: nu ! Nu of pixel (radians)
     Real :: proj ! Projection of a scattering vector in plane
     Real :: temp ! Temporary variable for swapping x and Y
     Real x_rotate ! Rotated X-coordinate
     Real :: xwc ! X-component of distance vector from beam centre
     Real y_rotate ! Rotated Y-coordinate
     Real :: ywc ! Y-component of distance vector from beam centre
     Real :: z_r ! Z real coordinate in output volume
!  Local Arrays:
     Real :: CHIMAT(3, 3) ! Chi rotation matrix
     Real :: CHIMATT(3, 3) ! Transpose of Chi rotation matrix
     Real :: OMEGAMAT(3, 3) ! Omega rotation matrix
     Real :: OMEGAMATT(3, 3) ! Transpose of Omega rotation matrix
     Real :: PHIMAT(3, 3) ! Phi rotation matrix
     Real :: PHIMATT(3, 3) ! Transpose of Phi rotation matrix
     Real :: ROTMAT(9) ! Combined rotation matrix
     Real :: WORK(3, 3) ! Work matrix for calculating combined rotation matrix
!    Real DUM1(3, 3) ! Work arrays
!    Real DUM2(3, 3) ! Work arrays
     Real :: Z1(3) ! Scattering vector of a pixel position
     Real :: Z1C1(3) ! Z1 C1 product
     Real :: Z2(3) ! Work vector for calculating scattering vector
     Real :: Z3(3) ! Work vector for calculating scattering vector
     Real :: Z4(3) ! Work vector for calculating scattering vector
!  Internal Functions:
     Double Precision :: Degrees
     Double Precision :: rad
     Degrees(rad) = rad * 180.0 / Pi_d
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_TRANSFORM ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_RMAP_TRANSFORM ' // Version)
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
        Write (message, '(''      chi = '', f11.4, '' phi = '', f11.4, ' // &
          ''' omega = '', g11.4, '' (Degrees)'')') &
          Degrees(chi), Degrees(phi), Degrees(omega)
        Call IO_WRITE (message, status)

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.

!     Calculate "scale_factor" to apply to intensities
        If (experiment%attenuation_set) Then
           scale_factor = experiment%attenuation
        Else
           scale_factor = 1.0
        End If

        If (experiment%monitor_set) Then
           monitor = experiment%monitor
        Else
           monitor = 1.0
        End If

        half_thickness = thickness / 2.0

        c5dist = Sqrt(C5(1)**2 + C5(2)**2 + C5(3)**2)

!     Define Tranpose of chi, phi, omega rotation matrices
        Call F2D_CHIMAT (chi, CHIMAT)
        Call F2D_GMTRA (CHIMAT, CHIMATT, 3, 3)
        Call F2D_PHIMAT (phi, PHIMAT)
        Call F2D_GMTRA (PHIMAT, PHIMATT, 3, 3)
        Call F2D_PHIMAT (omega, OMEGAMAT)
        Call F2D_GMTRA (OMEGAMAT, OMEGAMATT, 3, 3)
 
!     Form combined rotation matrix
        Call F2D_GMPRD (CHIMATT, OMEGAMATT, WORK, 3, 3, 3)
        Call F2D_GMPRD (PHIMATT, WORK, ROTMAT, 3, 3, 3)
 
        cos_detector = Cos(experiment%detector_rotation)
        sin_detector = Sin(experiment%detector_rotation)

!     Loop over each pixel in the ROI
        Do y = ystrelm, yendelm
 
!        Swap X and Y to cope with (default) vertical 2-theta
           xwc = (Real(y) - 0.5 - experiment%y_beam) * experiment%y_pixel_size
 
           Do x = xstrelm, xendelm

              If (.Not. MASK(x, y)) Then

                 ywc = (Real(x) - 0.5 - experiment%x_beam) * &
                   experiment%x_pixel_size

!              Rotate about beam
                 x_rotate = cos_detector * xwc - sin_detector * ywc
                 y_rotate = sin_detector * xwc + cos_detector * ywc

!              Mirror horizontal image displacement if required
                 If (experiment%view_from_sample) Then
                    y_rotate  = -y_rotate 
                 End If
 
!              Swap x and y if horizontal 2-theta movement
                 If (.Not. experiment%vertical_2theta) Then
                    temp = x_rotate
                    x_rotate = ywc
                    y_rotate  = temp
                 End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''x, y = '', 2i6)') x, y
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Find pixel gamma and nu
                 d = Sqrt(x_rotate**2 + experiment%detector_distance**2)
                 gamma = two_theta + &
                   Atan2(x_rotate, experiment%detector_distance)
                 nu = Atan2(y_rotate, d)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''gamma, nu = '', 2g12.5)') gamma, nu
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Find the scattering vector in cartesian coordinates
!              from gamma, nu, chi, phi and omega
                 sin_gamma = Sin(gamma)
                 cos_gamma = Cos(gamma)
                 sin_nu = Sin(nu)
                 cos_nu = Cos(nu)
                 Z4(1) = (sin_gamma * cos_nu) * inv_wave_angstroms
                 Z4(2) = (cos_gamma * cos_nu - 1.0) * inv_wave_angstroms
                 Z4(3) = sin_nu * inv_wave_angstroms
 
!              Call F2D_GMPRD (OMEGAMATT, Z4, Z3, 3, 3, 1)
 
!              Calculate Z1 = [PHI]T.[CHI]T.Z3
!              Call F2D_GMPRD (CHIMATT, Z3, Z2, 3, 3, 1)
!              Call F2D_GMPRD (PHIMATT, Z2, Z1, 3, 3, 1)
 
!              Apply combined rotation matrix
!              OLD-CODE
!              Call F2D_GMPRD (ROTMAT, Z4, Z1, 3, 3, 1)
!              NEW-CODE
                 Z1(1) = ROTMAT(1) * Z4(1) + ROTMAT(4) * Z4(2) + &
                   ROTMAT(7) * Z4(3)
                 Z1(2) = ROTMAT(2) * Z4(1) + ROTMAT(5) * Z4(2) + &
                   ROTMAT(8) * Z4(3)
                 Z1(3) = ROTMAT(3) * Z4(1) + ROTMAT(6) * Z4(2) + &
                   ROTMAT(9) * Z4(3)
!              END OF NEW CODE
 
!              Vector relative to start
                 Do k = 1, 3
                    Z1C1(k) = Z1(k) - C1(k)
                 End Do

!              Find projection of this vector on the normal to the section
                 proj = Z1C1(1) * CN(1) + Z1C1(2) * CN(2) + Z1C1(3) * CN(3)

                 z_r = (proj / c5dist) + 1.0

                 zm = Nint(z_r)

                 If (zm .Gt. 0 .And. zm .Le. znummap) Then

                    If (Abs(z_r - Real(zm)) .Le. half_thickness) Then

!                    Find the vector relative to one point on the section
                       Do k = 1, 3
                          Z1C1(k) = Z1(k) - (C1(k) + (zm - 1) * C5(k))
                       End Do
 
                       xm = Int(((Z1C1(1) * C2C1(1) + Z1C1(2) * C2C1(2) + &
                         Z1C1(3) * C2C1(3)) / c2c1s) * Real(xnummap) + 0.5)
 
                       If (xm .Gt. 0 .And. xm .Le. xnummap) Then
 
                          ym = Int(((Z1C1(1) * C3C1(1) + Z1C1(2) * C3C1(2) + &
                            Z1C1(3) * C3C1(3)) / c3c1s) * Real(ynummap) + 0.5)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                    Write (*, '(''x, y = '', 2i6)') x, y
!                    Write (*, '(''gamma, nu = '', 2g12.5)') gamma, nu
!                    Write (*, '(''Z1(1:3) = '', 3g12.5)') Z1
!                    Write (*, '(''Z1C1(1:3) = '', 3g12.5)') Z1C1
!                    Write (*, '(''C2C1(1:3) = '', 3g12.5)') C2C1
!                    Write (*, '(''C3C1(1:3) = '', 3g12.5)') C3C1
!                    Write (*, '(''c2c1s = '', g12.5)') c2c1s
!                    Write (*, '(''c3c1s = '', g12.5)') c3c1s
!                    Write (*, '(''proj, step = '', 2g12.5)')
!                    :                   proj, step
!                    Write (*, '(''xm, ym = '', 2i14)') xm, ym
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                          If (ym .Gt. 0 .And. ym .Le. ynummap) Then
 
                             MAP(xm, ym, zm) = MAP(xm, ym, zm) + DATA(x, y) * &
                               scale_factor
                             NORMALISE(xm, ym, zm) = NORMALISE(xm, ym, zm) + &
                               monitor
 
                          End If
 
                       End If
 
                    End If
 
                 End If

              End If
 
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_RMAP_TRANSFORM
!********1*********2*********3*********4*********5*********6*********7*********8
 
