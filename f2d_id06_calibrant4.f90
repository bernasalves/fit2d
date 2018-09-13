!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************************
!  *                         *
!  * f2d_id06_calibrant4.f90 *
!  *                         *
!  ***************************
 
!+ F2D_ID06_CALIBRANT4 -  CALIBRANT fit of wavelength / distance / etc.
     Subroutine F2D_ID06_CALIBRANT4 (xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, XAXIS, YAXIS, DATA, MASK, &
       title, xlabel, ylabel, zlabel, azimuth_start, azimuth_end, &
       calibrant_sample, max_cali_rings, num_cali_rings, D_SPACINGS, &
       refine_detector_offset, refine_beam_centre, refine_detector_distance, &
       refine_tilt, refine_wavelength, reject_outliers, experiment, status)
!  Description:
!    Calibrates sample to detector distance, wavelength, beam centre,
!    and detector non-orthoganality using a high quality calibrant
!    calibration pattern. Starts with estimates of first 2 Bragg lines.
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Oct-2014: V0.4 Add "detector_offset" parameter and allow beam centre to
!      be different from rotation centre (Hammersley)
!    15-Sep-2014: V0.3 Add "refine_wavelength" (Hammersley)
!    03-Sep-2014: V0.2 Add fitting (Hammersley)
!    12-Aug-2014: V0.1 Original, based on "F2D_ID06_CALIBRANT2" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fitrings.inc' ! Powder rings least squares
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements defined in
!      Y-direction
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Axis data for the Y-axis
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates  masked data point (i.e. point not considered in fitting)
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Real, Intent(IN) :: azimuth_start ! Start azimuth for full data region
     Real, Intent(IN) :: azimuth_end ! End azimuth for full data region
     Character(Len = *), Intent(IN) :: calibrant_sample ! Sample used for 
!      calibration
     Integer :: max_cali_rings ! Dimension for "D_SPACINGS"
     Integer :: num_cali_rings ! Number of calibration powder rings defined
     Real, Intent(IN) :: D_SPACINGS(max_cali_rings) ! Calibrant diffraction peak
!      d-spacings in order of distance in metres
     Logical, Intent(IN) :: refine_detector_offset ! .True., if the detector
!      offset from the rotation axis to start of the detector is to be refined
     Logical, Intent(IN) :: refine_beam_centre ! .True., if the beam
!      centre position is to be refined
     Logical, Intent(IN) :: refine_detector_distance ! .True., if the
!      sample to detector  distance is to be refined
     Logical, Intent(IN) :: refine_tilt ! .True., if the detector tilt
!      is to be refined
     Logical, Intent(IN) :: refine_wavelength ! .True., if the
!      wavelength is to be refined
     Logical, Intent(IN) :: reject_outliers ! .True., if outliers are to
!      be rejected
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Real :: beam_centre ! Position of the beam centre
     Character(Len = 60) :: user_prompt ! User prompt text
     Integer :: coordinate ! Loop variable for coordinates
     Logical :: correct_parallax = .False. ! .True., if the effect of parallax
!      on angular position is to be corrected
     Real :: distance_centre ! Distance to the beam centre from the diffraction 
!      line
     Integer :: i ! Loop variable
     Logical :: low_centre ! .True., if beam centre is low in X-direction
     Integer :: half_pos ! Position halfway between min and max
     Integer :: half_pos2 ! 2nd cyclic position halfway between min and max
     Integer :: max_pos ! Position of maximum difference in same azimuth 
!      coordinates
     Integer :: min_pos ! Position of minimum difference in same azimuth 
!      coordinates
     Integer :: num_choices ! Number of choices
     Integer :: num_help ! Number of lines of help text
     Integer :: num_pairs ! Number of paired coordinates found
     Integer :: num_rings ! Number of powder rings to search
     Integer, Save :: num_sections = 90 ! Number of angular sections to
!      calculate coordinates on powder rings
     Integer :: radii ! Loop variable for radii
     Integer :: retstat ! Status return variable
     Integer :: ring ! Loop variable for powder ring d-spacings
     Logical, Save :: extra_fits = .False. ! .True., if the pattern
!      requires extra fitting stages i.e. 4, 7, 11, 15 rings
     Logical, Save :: first = .True. ! .True., if first call to routine
     Logical, Save :: full_info = .True. ! .True., if full output
!      information is to be produced
     Logical, Save :: save_refine_detector_distance ! Saved version of
!      refine sample to detector distance
     Logical, Save :: weighted_fitting = .False. ! .True., if the fitting
!      of tilt and beam centre is to be weighted by the intensities
     Real :: angle ! Orientation angle of first axis of best fit ellipse
     Real :: distance ! Distance on detector between first two reflections
     Real :: est_distance ! Estimated sample to detector distance
     Real, Save :: half_search_distance ! Distance of half the width for the
!      search region (pixels)
     Real :: radial_error ! Estimated average error (radially) in
!      coordinate positions (metres)
     Real :: radius1 ! Longer radius of ellipse (metres)
     Real :: radius2 ! Shorter radius of ellipse (metres)
     Real, Save :: reject_value = 4.0 ! If a coordinate is more than this
!      number of standard deviations from the predicted ring position (radially)
!      than it will be removed from the fit
     Real :: rotation ! Angle of rotation of the tilt plane (about the Z-axis)
     Real :: tilt ! Angle of detector tilt in the X-direction
     Real :: two_theta1 ! Two theta angle of first reflection
     Real :: two_theta2 ! Two theta angle of second reflection
     Real :: x1 ! Position of first Bragg line
     Real :: x2 ! Position of second Bragg line
     Real :: x_ellipse ! X-coordinate of centre of fitted ellipse (metres)
     Real :: y_ellipse ! Y-coordinate of centre of fitted ellipse (metres)
!  Local Arrays:
     Character(Len = 70) :: PROMPT(3) ! User prompt text
     Character(Len = 80) :: MESSAGE(7) ! User messages
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Real :: AZI_PAIRS(F2d_max_rcoordinates) ! Azimuths of paired coordinates
     Real :: PAIR_DIFS(F2d_max_rcoordinates) ! Differences betwen pairs of
!      coordinates at the same azimuth
     Real :: X_PAIRS(F2d_max_rcoordinates) ! X-coordinate of paired coordinates
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ID06_CALIBRANT4 ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
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
        Call ST_SAVE ('Subroutine F2D_ID06_CALIBRANT4 ' // Version)
     Else
 
!     Need to draw image
        Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, XAXIS, YAXIS, &
          xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
          status)
 
!     By default set good return status
        retstat = 0
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_ID06_CALIBRANT4: After GS_2DIMAGE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input coordinate on first Bragg line
        F2D_NUM_RCOORDINATES(1) = 0
        If (calibrant_sample .Eq. 'ALUMINA (Al2O3') Then
           MESSAGE(1) = 'Click on the 1 0 -2 reflection line '
           MESSAGE(2) = '(the closest to the "centre").'
        Else If (calibrant_sample .Eq. 'LANTHANUM HEXABORIDE') Then
           MESSAGE(1) = 'Click on the 100 reflection line'
           MESSAGE(2) = '(the closest to the "centre").'
        Else If (calibrant_sample .Eq. 'PARAFFIN WAX') Then
           MESSAGE(1) = 'Click on the first reflection line'
           MESSAGE(2) = '(the closest to the "centre").'
        Else If (calibrant_sample .Eq. 'SODIUM CHLORIDE') Then
           MESSAGE(1) = 'Click on the 200 reflection line'
           MESSAGE(2) = '(the second from the "centre").'
        Else If (calibrant_sample .Eq. 'SILVER BEHENATE') Then
           MESSAGE(1) = 'Click on the 001 reflection line'
           MESSAGE(2) = '(the first from the "centre").'
        Else
           MESSAGE(1) = 'Click on 111 reflection line'
           MESSAGE(2) = '(the closest to the "centre").'
        End If
 
        If (calibrant_sample .Eq. 'SODIUM CHLORIDE') Then
           user_prompt = 'CLICK ON THE 200 (SECOND) SAMPLE LINE'
        Else
           user_prompt = 'INPUT COORDINATE ON INNER SAMPLE LINE'
        End If
 
        F2D_NUM_RCOORDINATES(1) = 1
        Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, MASK, XAXIS, YAXIS, title, xlabel, &
          ylabel, zlabel, user_prompt, 2, MESSAGE, .False., &
          F2d_max_rcoordinates, F2D_NUM_RCOORDINATES, &
          F2D_X_POLAR, F2d_Y_POLAR, status)
        x1 = F2D_X_POLAR(1, 1)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_ID06_CALIBRANT3: After GS_INPS_FCOORDINATES'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           status = St_goodvalue
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If

!     Input coordinate on second Bragg line
        If (calibrant_sample .Eq. 'SODIUM CHLORIDE') Then
           user_prompt = 'CLICK ON THE THIRD SAMPLE LINE'
        Else
           user_prompt = 'INPUT COORDINATE ON SECOND INNER MOST LINE'
        End If
 
        F2D_NUM_RCOORDINATES(1) = 0
        MESSAGE(1) = 'Click on the second reflection line'
 
        F2D_NUM_RCOORDINATES(1) = 1
        Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, MASK, XAXIS, YAXIS, title, xlabel, &
          ylabel, zlabel, user_prompt, 2, MESSAGE, .False., &
          F2d_max_rcoordinates, F2D_NUM_RCOORDINATES, &
          F2D_X_POLAR, F2D_Y_POLAR, status)
        x2 = F2D_X_POLAR(1, 1)

!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           status = St_goodvalue
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Determine if beam centre is low or high
        low_centre = x1 .Le. x2

!     Estimate sample to detector distance
        two_theta1 = 2.0 * Asin(experiment%wavelength / (2.0 * D_SPACINGS(1)))
        two_theta2 = 2.0 * Asin(experiment%wavelength / (2.0 * D_SPACINGS(2)))
        distance = (x2 - x1) * experiment%x_pixel_size

        If (refine_detector_distance) Then
           experiment%detector_distance = distance / &
             (Tan(two_theta2) - Tan(two_theta1)) 

           Write (message, &
             '(''INFO: Detector distance from 2 reflections = '', 1pe14.5)') &
             experiment%detector_distance
           Call IO_WRITE (message, status)

        End If

        If (refine_detector_offset) Then

!        Estimate position of beam-centre in X-direction
           distance_centre = Tan(two_theta1) * experiment%detector_distance 
           If (low_centre) Then
              beam_centre = x1 - distance_centre / experiment%x_pixel_size
           Else
              beam_centre = x1 + distance_centre / experiment%x_pixel_size
           End If
           experiment%detector_offset = -beam_centre * experiment%x_pixel_size

           Write (message, &
             '(''INFO: Detector offset (m) from 2 reflections = '', 1pe14.5)') &
             experiment%detector_offset
           Call IO_WRITE (message, status)
           Write (message, '(''INFO: Detector offset (pixels) from ' // &
             '2 reflections = '', 1pe14.5)') &
             experiment%detector_offset / experiment%x_pixel_size
           Call IO_WRITE (message, status)

        End If

        If (refine_beam_centre) Then
           experiment%x_beam = 0.0
           experiment%y_beam = 0.0
        End If

        save_refine_detector_distance = refine_detector_distance
!        refine_detector_distance = .False.

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Search up to first 2 Bragg rings
        Call IO_WRITE (' ', status)
        Call IO_WRITE ('INFO: Finding Coordinates on first 2 Bragg Reflections',&
          status)

        half_search_distance = Abs(x2 - x1) / 4.0
        num_rings = Min(2, num_cali_rings)
        Call F2D_FIND_LINES (full_info, xmaxdat, ymaxdat, xnumdat, ynumdat, &
          xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, experiment, &
          low_centre, &
          F2d_max_rings, num_rings, D_SPACINGS, half_search_distance, &
          azimuth_start, azimuth_end, &
          num_sections, .False., F2d_max_rcoordinates, F2D_NUM_RCOORDINATES, &
          F2D_X_POLAR, F2D_Y_POLAR, F2D_RINTENSITIES, &
          F2D_AZIMUTHS, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Do coordinate = 1, F2D_NUM_RCOORDINATES(1)
!           Write (*, '(''coordinate, X_POLAR (mm) = '', i4, f12.3)') &
!             coordinate, F2D_X_POLAR(coordinate, 1)
!        End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Output found coordinates on image
        Call F2D_ID06_DISPLAY (F2d_max_rcoordinates, F2d_max_rings, num_rings, &
          F2D_NUM_RCOORDINATES, F2D_X_POLAR, F2D_Y_POLAR, status)

!     Find pairs of found line positions 
        Call F2D_ID06_LINEPAIRS (F2d_max_rcoordinates, F2d_max_rings, num_rings,&
          F2D_NUM_RCOORDINATES, F2D_X_POLAR, F2D_AZIMUTHS, &
          num_pairs, X_PAIRS, PAIR_DIFS, AZI_PAIRS, min_pos, max_pos, &
          status)

        half_pos = (max_pos + min_pos) / 2
        half_pos2 = Mod(half_pos + num_pairs / 2 - 1, num_pairs) + 1

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''num_pairs = '', i6)') num_pairs
!        Write (*, '(''min_pos = '', i6, '' angle(deg) = '', f12.5)') &
!          min_pos, AZI_PAIRS(min_pos) * 180.0 / Pi
!        Write (*, '(''max_pos = '', i6, '' angle(deg) = '', f12.5)') &
!          max_pos, AZI_PAIRS(max_pos) * 180.0 / Pi
!        Write (*, '(''half_pos = '', i6, '' angle(deg) = '', f12.5)') &
!          half_pos, AZI_PAIRS(half_pos) * 180.0 / Pi
!        Write (*, '(''half_pos2 = '', i6, '' angle(deg) = '', f12.5)')&
!           half_pos2, AZI_PAIRS(half_pos2) * 180.0 / Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Initial detector distance and beam centre
        two_theta1 = 2.0 * Asin(experiment%wavelength / (2.0 * D_SPACINGS(1)))
        two_theta2 = 2.0 * Asin(experiment%wavelength / (2.0 * D_SPACINGS(2)))
        distance = (PAIR_DIFS(half_pos) + PAIR_DIFS(half_pos2)) / 2.0 &
          * experiment%x_pixel_size
        experiment%detector_distance = distance / &
          (Tan(two_theta2) - Tan(two_theta1)) 
        Write (message, &
          '(''INFO: Detector distance from 2 reflections = '', 1pe14.5)') &
          experiment%detector_distance
        Call IO_WRITE (message, status)

!     Estimate distance from rotation axis to start of detector (metres)
        distance_centre = Tan(two_theta1) * experiment%detector_distance 
        experiment%detector_offset  = distance_centre - &
          (X_PAIRS(half_pos) + X_PAIRS(half_pos2)) / 2.0 * &
          experiment%x_pixel_size 
        
        Write (message, '(''INFO: Detector offset from 2 reflections = ''' // &
          ', 1pe14.5, '' metres'')') experiment%detector_offset
        Call IO_WRITE (message, status)
        Write (message, '(''INFO: Detector offset from 2 reflections = '',' // &
           '1pe14.5, '' pixels'')') &
           experiment%detector_offset / experiment%x_pixel_size
        Call IO_WRITE (message, status)

!     Set tilt
        experiment%tilt_plane_rotation = Mod(AZI_PAIRS(max_pos) + &
           Mod(AZI_PAIRS(min_pos) + Pi, 2 * Pi) / 2.0, 2 * Pi)
        Write (message, &
          '(''INFO: Tilt plane rotation angle (degrees) = '', 1pe14.5)') &
          experiment%tilt_plane_rotation * 180.0 / Pi
        Call IO_WRITE (message, status)


!      Approximate tilt angle
        experiment%tilt_angle = 0.1
!          Acos((PAIR_DIFS(half_pos) + PAIR_DIFS(half_pos2)) / &
!          (PAIR_DIFS(max_pos) + PAIR_DIFS(min_pos)))
!          Acos(PAIR_DIFS(half_pos) / PAIR_DIFS(max_pos)) / 2.0
!        Write (message, &
!          '(''INFO: Approximate tilt (degrees) = '', 1pe14.5)') &
!          experiment%tilt_angle * 180.0 / Pi
!        Call IO_WRITE (message, status)

!     Refine detector geometry parameters based on two line coordinates
        Call F2D_ID06_LSQCALIBRANT (full_info, F2d_max_rings, num_rings, &
          D_SPACINGS, &
          weighted_fitting, refine_detector_offset, refine_beam_centre, &
          refine_detector_distance, .False., refine_tilt, &
          experiment, radial_error, status)

        Write (message, &
          '(''INFO: Detector offset (metres) from 2 rings = '', 1pe14.5)') &
          experiment%detector_offset
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: Detector offset (pixels) from 2 rings = '', 1pe14.5)') &
          experiment%detector_offset / experiment%x_pixel_size
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: X beam centre (pixels) from 2 rings = '', 1pe14.5)') &
          experiment%x_beam
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: Y beam centre (pixels) from 2 rings = '', 1pe14.5)') &
          experiment%y_beam
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: Tilt plane rotation angle (degrees) = '', 1pe14.5)') &
          experiment%tilt_plane_rotation * 180.0 / Pi
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: Tilt angle (degrees) = '', 1pe14.5)') &
          experiment%tilt_angle * 180.0 / Pi
        Call IO_WRITE (message, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Return
!        Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Use 4 lines
        half_search_distance = Abs(x2 - x1) / 6.0
        num_rings = Min(4, num_cali_rings)
        Call F2D_FIND_LINES (full_info, xmaxdat, ymaxdat, xnumdat, ynumdat, &
          xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, experiment, &
          low_centre, &
          F2d_max_rings, num_rings, D_SPACINGS, half_search_distance, &
          azimuth_start, azimuth_end, &
          num_sections, .False., F2d_max_rcoordinates, F2D_NUM_RCOORDINATES, &
          F2D_X_POLAR, F2D_Y_POLAR, F2D_RINTENSITIES, &
          F2D_AZIMUTHS, status)

!     Re-draw image
        Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, XAXIS, YAXIS, &
          xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
          status)

!     Output found coordinates on image
        Call F2D_ID06_DISPLAY (F2d_max_rcoordinates, F2d_max_rings, num_rings, &
          F2D_NUM_RCOORDINATES, F2D_X_POLAR, F2D_Y_POLAR, status)

!     Refine detector geometry parameters based on four line coordinates
        Call F2D_ID06_LSQCALIBRANT (full_info, F2d_max_rings, num_rings, &
          D_SPACINGS, &
          weighted_fitting, refine_detector_offset, refine_beam_centre, &
          refine_detector_distance, .False., refine_tilt, &
          experiment, radial_error, status)

        Write (message, &
          '(''INFO: Detector offset (metres) from 4 rings = '', 1pe14.5)') &
          experiment%detector_offset
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: Detector offset (pixels) from 4 rings = '', 1pe14.5)') &
          experiment%detector_offset / experiment%x_pixel_size
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: X beam centre (pixels) from 4 rings = '', 1pe14.5)') &
          experiment%x_beam
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: Y beam centre (pixels) from 4 rings = '', 1pe14.5)') &
          experiment%y_beam
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: Detector distance from 4 rings = '', 1pe14.5)') &
          experiment%detector_distance
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: Tilt plane rotation angle (degrees) = '', 1pe14.5)') &
          experiment%tilt_plane_rotation * 180.0 / Pi
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: Tilt angle (degrees) = '', 1pe14.5)') &
          experiment%tilt_angle * 180.0 / Pi
        Call IO_WRITE (message, status)

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Use 8 lines
        half_search_distance = Abs(x2 - x1) / 6.0
        num_rings = Min(8, num_cali_rings)
        Call F2D_FIND_LINES (full_info, xmaxdat, ymaxdat, xnumdat, ynumdat, &
          xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, experiment, &
          low_centre, &
          F2d_max_rings, num_rings, D_SPACINGS, half_search_distance, &
          azimuth_start, azimuth_end, &
          num_sections, .False., F2d_max_rcoordinates, F2D_NUM_RCOORDINATES, &
          F2D_X_POLAR, F2D_Y_POLAR, F2D_RINTENSITIES, &
          F2D_AZIMUTHS, status)

!     Re-draw image
        Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, XAXIS, YAXIS, &
          xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
          status)

!     Output found coordinates on image
        Call F2D_ID06_DISPLAY (F2d_max_rcoordinates, F2d_max_rings, num_rings, &
          F2D_NUM_RCOORDINATES, F2D_X_POLAR, F2D_Y_POLAR, status)

!     Refine detector geometry parameters based on four line coordinates
        Call F2D_ID06_LSQCALIBRANT (full_info, F2d_max_rings, num_rings, &
          D_SPACINGS, &
          weighted_fitting, refine_detector_offset, refine_beam_centre, &
          refine_detector_distance, .False., refine_tilt, &
          experiment, radial_error, status)

        Write (message, &
          '(''INFO: Detector offset (metres) from 8 rings = '', 1pe14.5)') &
          experiment%detector_offset
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: Detector offset (pixels) from 8 rings = '', 1pe14.5)') &
          experiment%detector_offset / experiment%x_pixel_size
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: X beam centre (pixels) from 8 rings = '', 1pe14.5)') &
          experiment%x_beam
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: Y beam centre (pixels) from 8 rings = '', 1pe14.5)') &
          experiment%y_beam
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: Detector distance from 8 rings = '', 1pe14.5)') &
          experiment%detector_distance
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: Tilt plane rotation angle (degrees) = '', 1pe14.5)') &
          experiment%tilt_plane_rotation * 180.0 / Pi
        Call IO_WRITE (message, status)
        Write (message, &
          '(''INFO: Tilt angle (degrees) = '', 1pe14.5)') &
          experiment%tilt_angle * 180.0 / Pi
        Call IO_WRITE (message, status)

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Use all lines
        If (num_cali_rings .Gt. 8) Then

           half_search_distance = Abs(x2 - x1) / 6.0
           num_rings = num_cali_rings
           Call F2D_FIND_LINES (full_info, xmaxdat, ymaxdat, xnumdat, ynumdat, &
             xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, experiment, &
             low_centre, &
             F2d_max_rings, num_rings, D_SPACINGS, half_search_distance, &
             azimuth_start, azimuth_end, &
             num_sections, .False., F2d_max_rcoordinates, F2D_NUM_RCOORDINATES, &
             F2D_X_POLAR, F2D_Y_POLAR, F2D_RINTENSITIES, &
             F2D_AZIMUTHS, status)

!        Re-draw image
           Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, XAXIS, YAXIS,&
             xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
             status)

!        Output found coordinates on image
           Call F2D_ID06_DISPLAY (F2d_max_rcoordinates, F2d_max_rings, &
             num_rings, F2D_NUM_RCOORDINATES, F2D_X_POLAR, F2D_Y_POLAR, status)

!        Refine detector geometry parameters based on four line coordinates
           Call F2D_ID06_LSQCALIBRANT (full_info, F2d_max_rings, num_rings, &
             D_SPACINGS, &
             weighted_fitting, refine_detector_offset, refine_beam_centre, &
             refine_detector_distance, .False., refine_tilt, &
             experiment, radial_error, status)

           Write (message, &
             '(''INFO: Detector offset (metres) from all rings = '', 1pe14.5)') &
             experiment%detector_offset
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: Detector offset (pixels) from all rings = '', 1pe14.5)') &
             experiment%detector_offset / experiment%x_pixel_size
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: X beam centre (pixels) from all rings = '', 1pe14.5)') &
             experiment%x_beam
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: Y beam centre (pixels) from all rings = '', 1pe14.5)') &
             experiment%y_beam
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: Detector distance from all rings = '', 1pe14.5)') &
             experiment%detector_distance
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: Tilt plane rotation angle (degrees) = '', 1pe14.5)') &
             experiment%tilt_plane_rotation * 180.0 / Pi
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: Tilt angle (degrees) = '', 1pe14.5)') &
             experiment%tilt_angle * 180.0 / Pi
           Call IO_WRITE (message, status)

        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Use all lines and refine wavelength
        If (num_cali_rings .Gt. 8 .And. refine_wavelength) Then

!        Refine detector geometry parameters based on four line coordinates
           Call F2D_ID06_LSQCALIBRANT (full_info, F2d_max_rings, num_rings, &
             D_SPACINGS, &
             weighted_fitting, refine_detector_offset, refine_beam_centre, &
             refine_detector_distance, refine_wavelength, refine_tilt, &
             experiment, radial_error, status)

           Write (message, &
             '(''INFO: Detector offset (metres) from all rings = '', 1pe14.5)') &
             experiment%detector_offset
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: Detector offset (pixels) from all rings = '', 1pe14.5)') &
             experiment%detector_offset / experiment%x_pixel_size
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: X beam centre (pixels) from all rings = '', 1pe14.5)') &
             experiment%x_beam
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: Y beam centre (pixels) from all rings = '', 1pe14.5)') &
             experiment%y_beam
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: Detector distance from all rings = '', 1pe14.5)') &
             experiment%detector_distance
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: Wavelength from all rings (Angstroms) = '', 1pe14.5)') &
             experiment%wavelength * 1.0e10
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: Tilt plane rotation angle (degrees) = '', 1pe14.5)') &
             experiment%tilt_plane_rotation * 180.0 / Pi
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: Tilt angle (degrees) = '', 1pe14.5)') &
             experiment%tilt_angle * 180.0 / Pi
           Call IO_WRITE (message, status)
           Write (MESSAGE(1), '(''INFO: ROT X = '', f12.3, ' // &
             '''   ROT Y = '', f12.3, '' degrees'')') &
             Cos(experiment%tilt_plane_rotation) &
             * experiment%tilt_angle * 180.0 / Pi, &
             Sin(experiment%tilt_plane_rotation) * experiment%tilt_angle &
             * 180.0 / Pi
           Call IO_WRITE (MESSAGE(1), status)

        End If

     End If

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_ID06_CALIBRANT4: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_ID06_CALIBRANT4
!********1*********2*********3*********4*********5*********6*********7*********8
