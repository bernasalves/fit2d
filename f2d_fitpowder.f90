!********1*********2*********3*********4*********5*********6*********7**
 
!  *******************
!  *                 *
!  * f2d_fitpowder.f *
!  *                 *
!  *******************
 
!+ F2D_FITPOWDER: FIT POWDER rings (Note: coordinates are
!    passed through 'f2d_fitrings.inc' common block)
     Subroutine F2D_FITPOWDER (full_info, max_angles, num_rings, &
       weighted_fitting, detector_gain, reject_outliers, reject_value, &
       refine_beam_centre, refine_sample_distance, refine_tilt, &
       sample_distance, x_beam, y_beam, ANGLE_CONES, tilt_plane_rotation, &
       tilt_angle, radial_error, status)
!  Description:
!    Fits beam centre and detector tilt parameters to one or more
!    powder rings. If 'reject_outliers' then coordinates which are
!    further than 'reject_value' from the predicted (fitted) ring
!    position are removed from the coordinate list and the data is
!    refitted.
!  Keywords:
!    Powder.Centre/Radius, Least~Squares.Powder.Centre/Radius
!  Method:
!    'F2D_LSQPOWDER' is used to fit the powder rings, and
!    'F2D_RINGOUTLIERS' is used to remove outliers from the
!    coordinate lists.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    21-Feb-1995: V0.1 Original, (Hammersley)
!    21-Jul-1995: V0.2 Debugging 'IO_MALLOC' problem (Hammersley)
!    01-Apr-1996: V0.3 Reduce tilt plane angle to +-Pi range (Hammersley)
!    22-Aug-1996: V0.4 Add option to not fit tilt (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc' ! Coordinate arrays for fitting
     Include 'f2d_lsqpowder.inc' ! Used to pass control
!    variables to 'F2D_FUNPOWDER'
!  Import:
     Logical, Intent(IN) :: full_info ! .True., if full information is
!      required by
!    the user
     Integer, Intent(IN) :: max_angles ! Dimension size of 'ANGLES' array
     Integer, Intent(IN) :: num_rings ! Number of powder rings for which
!    coordinates are available for fitting
     Logical, Intent(IN) :: weighted_fitting ! .True., if the fitting of
!      tilt and
!    beam centre is to be weighted by the intensities
     Real, Intent(IN) :: detector_gain ! The gain of the detector i.e. the
!      intensity
!    values divided by the gain should produce counts
     Logical, Intent(IN) :: reject_outliers ! .True., if outliers are to be
!    rejected
     Real, Intent(IN) :: reject_value ! If a coordinate is more than this
!      number
!    of standard deviations from the predicted ring position (radially)
!    than it will be removed from the fit
     Logical, Intent(IN) :: refine_beam_centre ! .True., if the beam centre
!      position
!    is to be refined
     Logical, Intent(IN) :: refine_sample_distance ! .True., if the sample
!      to detector
!    distance is to be refined
     Logical, Intent(IN) :: refine_tilt ! .True., if the detector
!      non-orthogonality
!    is to be refined
!  Import/Export:
     Real, Intent(INOUT) :: sample_distance ! Distance (metres) from sample
!      to detector
!    (beam centre on detector)
     Real, Intent(INOUT) :: x_beam ! Best fit X-coordinate of beam centre
     Real, Intent(INOUT) :: y_beam ! Best fit Y-coordinate of beam centre
     Real, Intent(INOUT) :: ANGLE_CONES(max_angles) ! Opening angles of
!      cones
     Real, Intent(INOUT) :: tilt_plane_rotation ! Angle of rotation for the
!      tilt plane
!    in the Z = 0 plane (radians)
     Real, Intent(INOUT) :: tilt_angle ! Angle of detector tilt in the
!      rotated
!    Y-direction (radians)
!  Export:
     Real, Intent(OUT) :: radial_error ! Estimated average coordinate radial
!    position error from best fit parameters. If only 5
!    coordinates have been entered, this is set to -1.7e38
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FITPOWDER ' // Version)
        Return
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''ENTERED F2D_FITPOWDER'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check that the region to be examined is reasonably defined
     If (max_angles .Le. 0) Then
        status = St_bad_dim1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_FITPOWDER ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Fit all coordinates on all rings to obtain fitted estimates
!     of beam centre/ tilt
        Call F2D_LSQPOWDER (full_info, Max_angles, num_rings, &
          weighted_fitting, detector_gain, refine_beam_centre, &
          refine_sample_distance, refine_tilt, sample_distance, x_beam, &
          y_beam, ANGLE_CONES, tilt_plane_rotation, tilt_angle, radial_error, &
          status)
 
!     Remove out-lying coordinates and re-fit if required
        If (reject_outliers) Then
 
           Call F2D_RINGOUTLIERS (full_info, Max_angles, num_rings, &
             sample_distance, x_beam, y_beam, ANGLE_CONES, &
             tilt_plane_rotation, tilt_angle, radial_error, reject_value, &
             status)
 
!        Re-fit data
           Call F2D_LSQPOWDER (full_info, Max_angles, num_rings, &
             weighted_fitting, detector_gain, refine_beam_centre, &
             refine_sample_distance, refine_tilt, sample_distance, x_beam, &
             y_beam, ANGLE_CONES, tilt_plane_rotation, tilt_angle, &
             radial_error, status)
 
!        Reduce tilt plane rotation angle to +-Pi
           Do While (Abs(tilt_plane_rotation) .Gt. Pi)
 
              If (tilt_plane_rotation .Gt. Pi) Then
                 tilt_plane_rotation = tilt_plane_rotation - 2.0 * Pi
              Else
                 tilt_plane_rotation = tilt_plane_rotation + 2.0 * Pi
              End If
 
           End Do
 
!        Convert tilt rotation angles to range 0 - Pi radians
           If (tilt_plane_rotation .Lt. 0.0) Then
              tilt_plane_rotation = tilt_plane_rotation + Pi
              tilt_angle = -tilt_angle
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_FITPOWDER
!********1*********2*********3*********4*********5*********6*********7**
 
 
