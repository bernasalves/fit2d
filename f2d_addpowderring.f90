!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_addpowderring.f90 *
!  *                       *
!  *************************
 
!+ F2D_ADDPOWDERRING - ADD POWDER diffraction RING to data
     Subroutine F2D_ADDPOWDERRING (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, experiment, angle_cone, intensity, sigma, DATA, status)
!  Description:
!    Adds a 2-D "diffraction" powder ring to the region of "DATA(xmaxdat, 
!    ymaxdat)" between "(xstrelm, ystrelm)" and "(xendelm, yendelm)". The 
!    maximum intensity of the ring is "intensity". The beam-centre is at 
!    "(x_beam, y_beam)", and the detector (beam-centre) to sample distance is
!    "distance". The 2-theta angle of the diffraction ring is "angle_cone". 
!    "sigma" is the standard deviation of the (Gaussian) radial profile of the 
!    ring in pixels. The detector may be tilted with respect to the plane 
!    orthogonal to the beam. The rotation angle of this tilt axis with respect 
!    to the X-axis is given by "rotation" about the Z-axis, and the tilt from 
!    the vertical is given by "tilt", which is a rotation about the rotated 
!    Y-axis. The image may have non-square pixels, their dimensions are 
!    "x_pixel_size" and "y_pixel_size".
!  Keywords:
!    Powder~Ring.Calculate, Diffraction~Ring.Calculate
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    14-Mar-2006: V0.4 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    21-Jan-1995: V0.3 Convert distance to metres, and enter pixel dimensions 
!      in metres (Hammersley)
!    23-Aug-1994: V0.2 Include rotation about the Z-axis for the tilt plane 
!      (Hammersley)
!    19-Aug-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of "DATA" array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of "DATA" array
     Integer, Intent(IN) :: xstrelm ! Starting X-element for calculation of
!      Gaussian
     Integer, Intent(IN) :: ystrelm ! Starting Y-element for calculation of
!      Gaussian
     Integer, Intent(IN) :: xendelm ! End X-element for calculation of Gaussian
     Integer, Intent(IN) :: yendelm ! End Y-element for calculation of Gaussian
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Real, Intent(IN) :: angle_cone ! Opening angle of diffraction cone
!      (radians)
     Real, Intent(IN) :: intensity ! Maximum intensity of ring
     Real, Intent(IN) :: sigma ! Standard deviation width (pixels) of radial
!      profile (metres)
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The data array to have
!      the Gaussian peak added
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Real :: cos_rotation ! The cosine of the rotation angle of the tilt plane
     Real :: cos_tilt_sqr ! Square of cosine of "y_tilt"
     Real :: gaussian_pos ! Relative position on a Gaussian
     Real :: radius ! Radius of untilted circular powder ring on detector
     Real :: sin_rotation ! The sine of the rotation angle of the tilt plane
     Real :: sin_tilt ! Sine of "y_tilt"
     Real :: sin_tilt_sqr ! Square of sine of "y_tilt"
     Real :: tan_cone_sqr ! Square of tangent of "angle_cone"
     Real :: term_c ! Constant term in formula for the intersection of a cone
!      with an inclined plane
     Real :: term_x ! The term in x, of the formula for the intersection of a
!      cone with an inclined plane
     Real :: term_xx ! The term in x^2, of the formula for the intersection of a
!      cone with an inclined plane
     Real :: x_dc ! X-data coordinate of centre of a pixel
     Real :: x_transform ! X-coordinate in rotated tilt frame
     Real :: y_dc ! Y-data coordinate of centre of a pixel
     Real :: y_transform ! Y-coordinate in rotated tilt frame
!  Local Arrays:
!  External Functions:
     Real, External :: Ma_gaussian ! Calculate a Gaussian curve position value
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ADDPOWDERRING ' // Version)
        Return
     End If
 
!  Check that the data region is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm .Lt. xstrelm .Or. xendelm .Gt. xmaxdat) Then
        status = St_bad_adr1
     Else If (yendelm .Lt. ystrelm .Or. yendelm .Gt. ymaxdat) Then
        status = St_bad_adr2
     Else If (sigma .Le. 0.0 .Or. experiment%x_pixel_size .Le. 0.0 .Or. &
       experiment%y_pixel_size .Le. 0.0 ) Then
        status = St_bad_real1
     End If
 
!  Re-check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ADDPOWDERRING ' // Version)
        status = St_mod_fit2d + status
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Initialise variables
        cos_rotation = Cos(-experiment%tilt_plane_rotation)
        sin_rotation = Sin(-experiment%tilt_plane_rotation)
        radius = experiment%detector_distance * Tan(angle_cone)
        tan_cone_sqr = (Tan(angle_cone))**2
        cos_tilt_sqr = (Cos(experiment%tilt_angle))**2
        sin_tilt = Sin(experiment%tilt_angle)
        sin_tilt_sqr = sin_tilt**2
        term_c = experiment%detector_distance**2 * tan_cone_sqr
        term_xx = sin_tilt_sqr * tan_cone_sqr  - cos_tilt_sqr
        term_x = 2.0 * experiment%detector_distance * sin_tilt * tan_cone_sqr
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''term_c, term_xx, term_x = '', 3g)')
!     :       term_c, term_xx, term_x
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Loop through active data region
        Do y = ystrelm, yendelm
 
           y_dc = (Real(y) - 0.5 - experiment%y_beam) * experiment%y_pixel_size
 
           Do x = xstrelm, xendelm
 
!           Relative position
              x_dc = (Real(x) - 0.5 - experiment%x_beam) * &
                experiment%x_pixel_size
 
!           Convert coordinate to rotated coordinate frame
 
!           Rotate coordinates by "-rotation", such that the transformed
!           coordinates are rotated to the orientation where the tilt in around 
!           the rotated Y-axis.
              x_transform = cos_rotation * x_dc - sin_rotation * y_dc
              y_transform = sin_rotation * x_dc + cos_rotation * y_dc
 
!           Calculate relative position on radial profile
              gaussian_pos = Abs(Sqrt(Abs(term_xx * x_transform**2 + term_x * &
                x_transform - y_transform**2)) - radius) / sigma
 
!           Convert Gaussian position to value
              If (gaussian_pos .Le. 5.0) Then
                 DATA(x, y) = DATA(x, y) + intensity * &
                   Ma_gaussian(gaussian_pos, 1, status)
 
              End If
 
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_ADDPOWDERRING
!********1*********2*********3*********4*********5*********6*********7*********8
