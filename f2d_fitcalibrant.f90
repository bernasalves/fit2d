!********1*********2*********3*********4*********5*********6*********7*********8

!  ************************
!  *                      *
!  * f2d_fitcalibrant.f90 *
!  *                      *
!  ************************
 
!+ F2D_FITCALIBRANT: FIT CALIBRANT rings (Note: coordinates are passed through
!  "f2d_fitrings.inc" common block)
     Subroutine F2D_FITCALIBRANT (full_info, max_rings, num_rings, D_SPACINGS, &
       weighted_fitting, reject_outliers, reject_value, &
       refine_beam_centre, refine_sample_distance, refine_wavelength, &
       refine_tilt, experiment, radial_error, status)
!  Description:
!    Fits wavelength, detector to sample distance, beam centre and
!    detector tilt parameters to one or more calibrant powder rings.
!    If "reject_outliers" then coordinates which are
!    further than "reject_value" from the predicted (fitted) ring
!    position are removed from the coordinate list and the data is
!    refitted.
!  Keywords:
!    Powder.Centre/Radius, Least~Squares.Powder.Centre/Radius
!  Method:
!    "F2D_LSQCALIBRANT" is used to fit the powder rings, and
!    "F2D_RINGOUTLIERS" is used to remove outliers from the
!    coordinate lists.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    10-Mar-2006: V0.6 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    17-Apr-1998: V0.5 Correct position of code to ensure that
!      the tilt plane angle is always output between +-Pi, previously
!      this only worked when outliers where being rejected (Hammersley)
!    24-Feb-1997: V0.4 Rename from "F2D_FITSILICON" (Hammersley)
!    15-Nov-1996: V0.3 Allow both NAG based fitting routine and
!      new "MA_MODELFIT" fitting routine to be called to
!      allow verification of results (Hammersley)
!    26-Aug-1996: V0.2 Checking cos and sin "TOTAL LOSS OF PRECISION"
!      problem (Hammersley)
!    12-Jun-1996: V0.1 Original, based on "F2D_FITPOWDER" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc' ! Coordinate arrays for fitting
!  Import:
     Logical, Intent(IN) :: full_info ! .True., if full information is
!      required by the user
     Integer, Intent(IN) :: max_rings ! Dimension size of "D_SPAPINGS" array
     Integer, Intent(IN) :: num_rings ! Number of powder rings for which
!      coordinates are available for fitting
     Real, Intent(IN) :: D_SPACINGS(max_rings) ! D-spacings for the powder lines
     Logical, Intent(IN) :: weighted_fitting ! .True., if the fitting of
!      tilt and beam centre is to be weighted by the intensities
     Logical, Intent(IN) :: reject_outliers ! .True., if outliers are to be
!      rejected
     Real, Intent(IN) :: reject_value ! If a coordinate is more than this
!      number of standard deviations from the predicted ring position (radially)
!      than it will be removed from the fit
     Logical, Intent(IN) :: refine_beam_centre ! .True., if the beam centre
!      positionis to be refined
     Logical, Intent(IN) :: refine_sample_distance ! .True., if the sample
!      to detector distance is to be refined
     Logical, Intent(IN) :: refine_wavelength ! .True., if the wavelength is
!      to be refined
     Logical, Intent(IN) :: refine_tilt ! .True., if the detector tilt is to
!      be refined
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
     Real, Intent(OUT) :: radial_error ! Estimated average coordinate radial
!      position error from best fit parameters. If only 5 coordinates have
!      been entered, this is set to -1.7e38
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
!  Local Variables:
     Integer :: ring ! Loop variable for powder rings
!  Local Arrays:
     Real :: ANGLE_CONES(F2d_max_rings) ! Opening angles of diffraction
!    cones at the refined wavelength
!  External Functions:
!    Common:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FITCALIBRANT ' // Version)
        Return
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''ENTERED F2D_FITCALIBRANT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check that the region to be examined is reasonably defined
     If (max_rings .Le. 0) Then
        status = St_bad_dim1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_FITCALIBRANT ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Fit all coordinates on all rings to obtain fitted estimates of beam 
!     centre / tilt
        Call F2D_LSQCALIBRANT (full_info, max_rings, num_rings, D_SPACINGS, &
          weighted_fitting, refine_beam_centre, &
          refine_sample_distance, refine_wavelength, refine_tilt, &
          experiment, radial_error, status)
 
!     Remove out-lying coordinates and re-fit if required
        If (reject_outliers) Then
 
!        Calculate angles of the diffraction cones
           Do ring = 1, num_rings
              ANGLE_CONES(ring) = 2.0 * Asin (experiment%wavelength / &
                (2.0 * D_SPACINGS(ring)))
           End Do
 
!        Check coordinates for rejects
           Call F2D_RINGOUTLIERS (full_info, max_rings, num_rings, &
             experiment%detector_distance, &
             experiment%x_beam * experiment%x_pixel_size, &
             experiment%y_beam * experiment%y_pixel_size, ANGLE_CONES, &
             experiment%tilt_plane_rotation, experiment%tilt_angle, &
             radial_error, reject_value, status)
 
!        Re-fit data
           Call F2D_LSQCALIBRANT (full_info, max_rings, num_rings, D_SPACINGS, &
             weighted_fitting, refine_beam_centre, &
             refine_sample_distance, refine_wavelength, refine_tilt, &
             experiment, radial_error, status)
 
        End If
 
!        Write (*, '(''experiment%tilt_plane_rotation = '', f14.5)') &
!          experiment%tilt_plane_rotation

!     Reduce tilt plane rotation angle to +-Pi
        Do

          If (experiment%tilt_plane_rotation .Le. Pi) Exit
           experiment%tilt_plane_rotation = &
             experiment%tilt_plane_rotation - 2.0 * Pi
        End Do

        Do 

          If (experiment%tilt_plane_rotation .Gt. -Pi) Exit
          experiment%tilt_plane_rotation = &
             experiment%tilt_plane_rotation + 2.0 * Pi
        End Do

!        Write (*, '(''Reduced experiment%tilt_plane_rotation = '', f14.5)') &
!          experiment%tilt_plane_rotation

!     Convert tilt rotation angles to range 0 - Pi radians
        If (experiment%tilt_plane_rotation .Lt. 0.0) Then
           experiment%tilt_plane_rotation = experiment%tilt_plane_rotation + Pi
           experiment%tilt_angle = -experiment%tilt_angle
        End If
 
     End If
 
     End Subroutine F2D_FITCALIBRANT
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
