!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_powderring.f90 *
!  *                    *
!  **********************
 
!+ F2D_POWDERRING: add POWDER diffraction RING to data
     Subroutine F2D_POWDERRING (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, experiment, DATA, status)
!  Description:
!    User specifies a powder diffraction ring to be added to the ROI
!  Keywords:
!    Powder~Ring.Add, Add.Powder~Ring, Input.Powder~Ring,
!    Diffraction~Ring.Add, Simulate.Powder~Diffraction.Ring
!  Method:
!  Deficiencies:
!    The detector tilt accounts for the change in position of the
!    radial profile, BUT NOT the change in profile shape as
!    measured on the detector
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    14-Mar-2006: V0.8 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    15-Feb-1996: V0.7 Correction to call to "F2D_INP_PIXELSIZES" (Hammersley)
!    11-Feb-1996: V0.6 Changes to "F2D_INP_SAMPLEDISTANCE" (Hammersley)
!    02-Feb-1996: V0.5 Change to "F2D_INP_BEAMCENTRE" arguments (Hammersley)
!    24-Jan-1995: V0.4 Changes to "F2D_INQ_GEOMETRY" (Hammersley)
!    21-Jan-1995: V0.3 Take into account non-square pixels (Hammersley)
!    23-Aug-1994: V0.2 Include rotation about the Z-axis for the tilt plane 
!      (Hammersley)
!    19-Aug-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array
     Integer, Intent(IN) :: xstrelm ! Defines start of ROI in X-direction
     Integer, Intent(IN) :: ystrelm ! Defines start of ROI in Y-direction
     Integer, Intent(IN) :: xendelm ! Defines end of ROI in X-direction
     Integer, Intent(IN) :: yendelm ! Defines end of ROI in Y-direction
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Array contains data
!      values, on output the ring is added to the data values
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
!  Local Variables:
     Logical, Save :: first = .True. ! .True., if the first call to the routine
     Real, Save :: angle_cone = .087266460 ! Opening angle of diffraction cone
     Real, Save :: intensity = 500.0 ! Maximum intensity of ring
     Real, Save :: sigma = 2.0 ! Standard deviation width (pixels) of
!      radial profile
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_POWDERRING '// Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Lt. 1 .Or. xstrelm .Gt. xendelm .Or. &
       xendelm .Gt. xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Lt. 1 .Or. ystrelm .Gt. yendelm .Or. &
       yendelm .Gt. ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_POWDERRING ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input X/Y pixel dimensions
        Call F2D_INP_PIXELSIZES (.False., experiment%x_pixel_size, &
          experiment%y_pixel_size, status)
         experiment%pixel_sizes_set = .True.

!     Input X/Y pixel coordinate of beam centre
        Call F2D_INP_BEAMCENTRE (.False., experiment%x_beam, &
          experiment%y_beam, status)
         experiment%beam_centre_set = .True.

!     Input sample to detector distance
        Call F2D_INP_SAMPLEDISTANCE (.False., experiment%detector_distance, &
          status)
        experiment%detector_distance_set = .True.
 
!     Rotation angle of tilt plane and tilt angle of detector
        Call F2D_INP_DETECTORTILT (.False., experiment%tilt_plane_rotation, &
          experiment%tilt_angle, status)
        experiment%tilt_set = .True.

!     Opening angle of diffraction ring
        angle_cone = angle_cone * 180.0 / Pi
        Call IO_INPR (.True., 0.0, 90.0, .True., &
          'OPENING ANGLE OF DIFFRACTION RING (DEGREES)', 1, &
          'Enter 2 theta angle of diffraction ring (degrees)', 1, &
          'Must be valid real number', angle_cone, status)
        angle_cone = angle_cone * Pi / 180.0
 
!     Input peak intensity
        Call IO_INPR (.False., 0.0, 0.0, .True., 'PEAK MAXIMUM INTENSITY', 1, &
          'Enter maximum intensity of powder diffraction ring', 1, &
          'Must be valid real number', intensity, status)
 
!     Standard deviation of diffraction ring radial profile
        Call IO_INPR (.True., 0.0, 1.7e38, .True., &
          'STANDARD DEVIATION WIDTH (X-PIXELS) OF RADIAL PROFILE', 1, &
          'Radial peak width of diffraction ring in pixel units', 1, &
          'Must be valid real number', sigma, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Add Powder ring to data
        Call F2D_ADDPOWDERRING (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, experiment, angle_cone, intensity, &
          sigma * experiment%x_pixel_size, DATA, status)
 
     End If
 
     End Subroutine F2D_POWDERRING
!********1*********2*********3*********4*********5*********6*********7*********8
