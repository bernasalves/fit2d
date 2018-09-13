!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_ringoutliers.f90 *
!  *                      *
!  ************************
 
!+ F2D_RINGOUTLIERS - FIT2D: RING OUTLIERS rejection
!    (Note: Coordinate values passed through common)
     Subroutine F2D_RINGOUTLIERS (full_info, Max_angles, num_rings, &
       sample_distance, x_beam, y_beam, ANGLE_CONES, tilt_plane_rotation, &
       tilt_angle, radial_error, reject_value, status)
!  Description:
!    Calculates if coordinates are within a limited number of standard 
!    deviations of the fitted position. Those which are not are removed from 
!    the coordinates list.
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Jul-2003: V0.2 Correct code to reject outliers. Previously, the 
!      outliers were not being rejected, but the number of coordinates was 
!      being reduced (Hammersley)
!    21-Feb-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'f2d_fitrings.inc' ! Powder least squares fitting coordinate data
     Include 'f2d_lsqpowder.inc' ! Powder ring fitting control parameters
!  Import:
     Logical, Intent(IN) :: full_info ! .True., if full information is
!      required by the user
     Integer, Intent(IN) :: max_angles ! Dimension size of "ANGLES" array
     Integer, Intent(IN) :: num_rings ! Number of powder rings for which
!      coordinates are available for fitting
     Real, Intent(IN) :: reject_value ! If a coordinate is more than this
!      number of standard deviations from the predicted ring position (radially)
!      than it will be removed from the fit
     Real, Intent(IN) :: sample_distance ! Distance (metres) from sample to
!      detector (beam centre on detector)
     Real, Intent(IN) :: x_beam ! Best fit X-coordinate of beam centre (metres)
     Real, Intent(IN) :: y_beam ! Best fit Y-coordinate of beam centre (metres)
     Real, Intent(IN) :: ANGLE_CONES(max_angles) ! Opening angles of cones
!      (radians)
     Real, Intent(IN) :: tilt_plane_rotation ! Angle of rotation for the
!      tilt plane in the Z = 0 plane (radians)
     Real, Intent(IN) :: tilt_angle ! Angle of detector tilt in the rotated
!      Y-direction (radians)
     Real, Intent(IN) :: radial_error ! Estimated average coordinate radial
!      position error from best fit parameters (metres)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: num_reject ! Total number of rejected coordinates
     Integer :: ring ! Loop variable for the powder rings
     Real :: cos_rotation ! The cosine of "-tilt_plane_rotation"
     Real :: cos_tilt_sqr ! Square of cosine of "tilt_angle"
     Real :: radius ! "Radius" of cone
     Real :: residual ! Radial error in position for a coordinate
     Real :: sin_rotation ! The sine of "-rotation"
     Real :: sin_tilt ! Sine of "tilt_angle"
     Real :: sin_tilt_sqr ! Square of sine of "tilt_angle"
     Real :: tan_cone_sqr ! Square of tangent of "angle_cone"
     Real :: term_c ! Constant term in formula for the
!      intersection of a cone with an inclined plane
     Real :: term_x ! The term in x, of the formula for the
!      intersection of a cone with an inclined plane
     Real :: term_xx ! The term in x^2, of the formula for the
!      intersection of a cone with an inclined plane
     Real :: x_transform ! Transformed X-coordinates
     Real :: y_transform ! Transformed Y-coordinates
!  Local Arrays:
!  Local Data:
!  External Functions:
!    Common:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''ENTERED F2D_RINGOUTLIERS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RINGOUTLIERS ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (max_angles .Le. 0) Then
        status = St_bad_dim1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_RINGOUTLIERS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Define rotation cosines and sines
        cos_rotation = Cos (-tilt_plane_rotation)
        sin_rotation = Sin (-tilt_plane_rotation)
        cos_tilt_sqr = (Cos(tilt_angle))**2
        sin_tilt = Sin(tilt_angle)
        sin_tilt_sqr = sin_tilt**2
 
!     Check each coordinate of each ring
        Do ring = 1, num_rings
 
!        Define common values
           tan_cone_sqr = (Tan(ANGLE_CONES(ring)))**2
           radius = sample_distance * Tan(ANGLE_CONES(ring))
           term_c = sample_distance**2 * tan_cone_sqr
           term_xx = sin_tilt_sqr * tan_cone_sqr  - cos_tilt_sqr
           term_x = 2.0 * sample_distance * sin_tilt * tan_cone_sqr
 
           Do coordinate = 1, F2D_NUM_RCOORDINATES(ring)
 
!           Rotate coordinates by "-tilt_plane_rotation", such that the
!           transformed coordinates are compared to an powder ring whose
!           tilt plane is in the Y=0 plane.
              x_transform = cos_rotation * (F2D_X_RCOORDINATES(coordinate, &
                ring) - x_beam) - sin_rotation * &
                (F2D_Y_RCOORDINATES(coordinate, ring) - y_beam)
              y_transform = sin_rotation * (F2D_X_RCOORDINATES(coordinate, &
                ring) - x_beam) + cos_rotation * &
                (F2D_Y_RCOORDINATES(coordinate, ring) - y_beam)
 
              residual = Sqrt(-term_xx * x_transform**2 - term_x * x_transform &
                + y_transform**2) - radius
 
              If (residual .Gt. radial_error * reject_value) Then
 
!              The coordinate needs to be rejected. This is signaled
!              through setting a bad intensity value
                 F2D_RINTENSITIES(coordinate, ring) = -1.0
 
              End If
 
           End Do
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Go through coordinate arrays looking for rejected coordinates
!     and compact lists
        Do ring = 1, num_rings
 
           num_reject = 0
           Do coordinate  = 1, F2D_NUM_RCOORDINATES(ring)
 
              If (F2D_RINTENSITIES(coordinate, ring) .Lt. -0.1) Then
 
!              Bad coordinate
                 num_reject = num_reject + 1
 
              Else
 
!              Only need to transfer coordinates if some have been
!              removed
                 If (num_reject .Gt. 0) Then
                    F2D_X_RCOORDINATES(coordinate - num_reject, ring) = &
                      F2D_X_RCOORDINATES(coordinate, ring)
                    F2D_Y_RCOORDINATES(coordinate - num_reject, ring) = &
                      F2D_Y_RCOORDINATES(coordinate, ring)
                    F2D_RINTENSITIES(coordinate - num_reject, ring) = &
                      F2D_RINTENSITIES(coordinate, ring)
                 End If
 
              End If
 
           End Do
 
!        Set number of coordinates remaining in ring
           F2D_NUM_RCOORDINATES(ring) = F2D_NUM_RCOORDINATES(ring) - &
             num_reject
 
        End Do
 
!     Output message to user
        If (full_info) Then
           Write (message, '(''INFO: Number of rejected ' // &
             'coordinates = '', i5)') num_reject
           Call IO_WRITE (message, status)
 
        End If
 
     End If
 
     End Subroutine F2D_RINGOUTLIERS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
