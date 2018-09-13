!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_id06_calibrant.f90 *
!  *                        *
!  **************************
 
!+ F2D_ID06_CALIBRANT -  CALIBRANT fit of wavelength / distance / etc.
     Subroutine F2D_ID06_CALIBRANT (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, XAXIS, YAXIS, DATA, MASK, &
       title, xlabel, ylabel, zlabel, azimuth_start, azimuth_end, experiment, &
       status)
!  Description:
!    Calibrates sample to detector distance, wavelength, beam centre,
!    and detector non-orthoganality using a high quality calibrant
!    calibration pattern.
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Jun-2014: V0.9 Testing with beam centre data (Hammersley)
!    06-Jun-2014: V0.8 Testing (Hammersley)
!    04-Jun-2014: V0.8 Test both possible focii and tilts (Hammersley)
!    30-Apr-2014: V0.7 De-bugging and testing (Hammersley)
!    29-Apr-2014: V0.6 Allow beam centre on the left-hand side (Hammersley)
!    01-Apr-2014: V0.5 Stop changes to Y-beam centre; Add azimuth limits
!      (Hammersley)
!    04-Mar-2014: V0.4 Debugging (Hammersley)
!    24-Feb-2014: V0.3 Debugging (Hammersley)
!    17-Jan-2014: V0.2 Adding code (Hammersley)
!    16-Jan-2014: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fitrings.inc' ! Powder rings least squares
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used
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
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.9' ! Version number
     Integer, Parameter :: Max_angles = F2d_max_rings ! Dimension of "ANGLES"
!      array
     Integer, Parameter :: Max_choices = 20 ! Maximum number of choices
!      (need 5 extra spaces)
     Integer, Parameter :: Max_chars =  60 ! Maximum number of characters
!      in a line
     Integer, Parameter :: Max_help = 10 ! Dimension of "HELP" array
     Integer, Parameter :: Max_lines =  2 ! Number of lines in message
!  Local Variables:
     Real :: beam_centre ! Position of the beam centre
     Character(Len = 20) :: calibrant_sample ! Sample used for calibration
     Character(Len = 60) :: user_prompt ! User prompt text
     Integer :: coordinate ! Loop variable for coordinates
!    Integer method_used ! The input method that was used to
!      specify the beam centre:
!        1 = Keyboard,            only "x_beam, y_beam" are defined
!        2 = Single cursor point,   "      "       "     "     "
!        3 = Symmetric points,      "      "       "     "     "
!        4 = Points on circle,  "radius1" and "radial_error" are defined
!        5 = Points on ellipse, "radius1", radius2","angle" and
!            "radial_error" are defined
!        6 = Fitted 2-D Gaussian
     Logical :: correct_parallax = .False. ! .True., if the effect of parallax
!      on angular position is to be corrected
     Real :: distance_centre ! Distance to the beam centre from the diffraction 
!      line
     Integer :: i ! Loop variable
     Logical :: low_centre ! .True., if beam centre is low in X-direction
     Integer :: num_cali_rings ! Number of calibration powder rings defined
     Integer :: num_choices ! Number of choices
     Integer :: num_help ! Number of lines of help text
     Integer :: num_rings ! Number of powder rings to search
     Integer, Save :: num_sections = 90 ! Number of angular sections to
!      calculate coordinates on powder rings
     Integer :: radii ! Loop variable for radii
     Integer :: retstat ! Status return variable:
     Integer :: ring ! Loop variable for powder ring d-spacings
     Logical, Save :: extra_fits = .False. ! .True., if the pattern
!      requires extra fitting stages i.e. 4, 7, 11, 15 rings
     Logical, Save :: first = .True. ! .True., if first call to routine
     Logical, Save :: full_info = .True. ! .True., if full output
!      information is to be produced
     Logical, Save :: refine_xbeam_centre = .True. ! .True., if the beam
!      centre position is to be refined
     Logical, Save :: refine_polarisation = .False. ! .True., if the
!      polarisation factor is to be refined
     Logical, Save :: refine_sample_distance = .True. ! .True., if the
!      sample to detector  distance is to be refined
     Logical, Save :: refine_tilt = .True. ! .True., if the detector tilt
!      is to be refined
     Logical, Save :: refine_wavelength = .True. ! .True., if the
!      wavelength is to be refined
     Logical, Save :: reject_outliers = .True. ! .True., if outliers are to
!      be rejected
     Logical, Save :: weighted_fitting = .False. ! .True., if the fitting
!      of tilt and beam centre is to be weighted by the intensities
     Real :: angle ! Orientation angle of first axis of best fit ellipse
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
     Real :: two_theta ! Two theta angle of a reflection
     Real :: x_ellipse ! X-coordinate of centre of fitted ellipse (metres)
     Real :: y_ellipse ! Y-coordinate of centre of fitted ellipse (metres)
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 20) :: CALIBRATION_CHOICES(8) ! Choice of known samples
!      used for calibration
     Character(Len = 70) :: EXPLANATION(8) ! Explanation of calibration
!      samples
     Character(Len = 70) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 60) :: HELP(Max_help) ! Help text
     Character(Len = 70) :: PROMPT(3) ! User prompt text
     Character(Len = 80) :: MESSAGE(7) ! User messages
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = Max_chars) :: TX(Max_lines) ! Text array
     Integer :: INTEGERS(Max_choices) ! Integer variables to be input
     Integer :: INTS_LOWER(Max_choices) ! Lower bound of integer variables
     Integer :: INTS_UPPER(Max_choices) ! Upper bound of integer variables
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
     Logical :: BOUND(Max_choices) ! .True., if variable range is bound
     Logical :: LOGICALS(Max_choices) ! Logical variables to be input
     Real, Save :: AL_2_O_3(Max_angles) ! Aluminium oxide diffraction peak
!      d-spacings in order of distance in metres
     Real, Save :: CE_O_2(Max_angles) ! Cerium dioxide diffraction peak
!      d-spacings in order of distance in metres
     Real, Save :: D_SPACINGS(Max_angles) ! Calibrant diffraction peak
!      d-spacings in order of distance in metres
     Real, Save :: LA_B_6(Max_angles) ! Lanthanum Hexaboride diffraction peak
!      d-spacings in order of distance in metres
     Real, Save :: PARAFFIN_WAX(2) ! Paraffin wax diffraction peak
!      d-spacings in order of distance in metres
     Real :: REALS_LOWER(Max_choices) ! Lower bounds on reals
     Real :: REALS_UPPER(Max_choices) ! Upper bounds on reals
     Real :: REALS(Max_choices) ! Reals to be changed
     Real, Save :: SILICON(Max_angles) ! Silicon powder diffraction peak
!      d-spacings in order of distance in metres
     Real, Save :: SILVER_BEHENATE(13) ! Silver Behenate powder 
!      diffraction peak d-spacings in order of distance in metres
     Real, Save :: SODIUM_CHLORIDE(Max_angles) ! Salt diffraction peak
!      d-spacings in order of distance in metres
     Real :: X_POLAR(F2d_max_rcoordinates, F2d_max_rings)  ! X-coordinate of
!      line polar coordinates
     Real :: Y_POLAR(F2d_max_rcoordinates, F2d_max_rings)  ! Y-coordinate of
!      line polar coordinates
!  External Functions:
!  Local Data:
     Data CALIBRATION_CHOICES / 'ALUMINA (Al2O3)', 'CERIUM DIOXIDE', &
       'LANTHANUM HEXABORIDE', 'SODIUM CHLORIDE', 'PARAFFIN WAX', 'SILICON', &
       'SILVER BEHENATE', 'USER DEFINED' /
     Data EXPLANATION / 'ALUMINA (Al2O3): Al_2O_3 powder sample', &
       'CERIUM DIOXIDE: Ceria powder sample e.g. NIST standard', &
       'LANTHANUM HEXABORIDE: LaB_6 powder sample e.g. NIST standard', &
       'SODIUM CHLORIDE: NaCl Common salt (beware of moisture)', &
       'PARAFFIN WAX: MM Xtallographers "standard" calibration wax', &
       'SILICON: Silicon powder sample e.g. NIST standard', &
       'SILVER BEHENATE: [CH_3(CH_2)_20 COOAg] powder e.g. NIST standard', &
       'USER DEFINED: User input of D-spacings for arbitrary sample' /
!    Al2O3(a = 4.7564e-10, b = 12.9894e-10 R3 c hexagonal setting)
     Data AL_2_O_3 / &!     h  k  l
       3.479e-10, &   !  1  1  0 -2
       2.550e-10, &   !  2  1  0  4
       2.378e-10, &   !  3  2 -1  0
       2.165e-10, &   !  4  0  0  6
       2.084e-10, &   !  5  2 -1  3
       1.963e-10, &   !  6  2  0  2
       1.739e-10, &   !  7  2  0 -4
       1.601e-10, &   !  8  2 -1  6
       1.546e-10, &   !  9  3 -1  1
       1.514e-10, &   ! 10  3 -1 -2
       1.511e-10, &   ! 11  1  0 -8
       1.404e-10, &   ! 12  3 -1  4
       1.373e-10, &   ! 13  3  0  0
       1.335e-10, &   ! 14  3 -1 -5
       1.275e-10  &   ! 15  2  0  8
      /

!    :    1.239e-10,    ! 16  1  0 10
!    :    1.234e-10,    ! 17  2 -1  9
!    :    1.193e-10,    ! 18  3 -1  7
!    :    1.189e-10,    ! 19  4 -2  0
!    :    1.160e-10,    ! 20  3  0  6
!    :    1.147e-10,    ! 21  4 -2  3
!    :    1.138e-10,    ! 22  4 -1 -1
!    :    1.125e-10,    ! 23  4 -1  2
!    :    1.124e-10,    ! 24  3 -1 -8
!    :    1.099e-10,    ! 25  2  0 -10
!    :    1.082e-10,    ! 26  0  0  12
!    :    1.078e-10,    ! 27  4 -1 -4
!    :    1.046e-10,    ! 28  4 -1  5
!    :    1.042e-10,    ! 29  4 -2  6
!    :    1.017e-10,    ! 30  4  0 -2
!    :    0.997e-10,    ! 31  3 -1 10
!  CeO2(a=5.411102e-10 [m], fcc)
     Data CE_O_2 /   &   !     hkl
       3.124101e-10, &   !  1  111
       2.705551e-10, &   !  2  200
       1.913113e-10, &   !  3  220
       1.631509e-10, &   !  4  311
       1.562051e-10, &   !  5  222
       1.352775e-10, &   !  6  400
       1.241392e-10, &   !  7  331
       1.209959e-10, &   !  8  420
       1.104537e-10, &   !  9  422
       1.041367e-10, &   ! 10  511
       0.956557e-10, &   ! 11  440
       0.914643e-10, &   ! 12  531
       0.901850e-10, &   ! 13  600
       0.901850e-10, &   ! 14  442
       0.855570e-10  &   ! 15  620
       /
!    :    0.825185e-10   ! 16  533
!    :    0.815754e-10   ! 17  622
!    :    0.781025e-10   ! 18  444
!    :    0.757706e-10   ! 19  711
!    :    0.750385e-10   ! 20  640
!    :    0.723089e-10   ! 21  642
!    :    0.704465e-10   ! 22  731
!    :    0.676388e-10   ! 23  800
!    :    0.661071e-10   ! 24  733
!    :    0.656193e-10   ! 25  820
!    :    0.656193e-10   ! 26  644
!    :    0.637704e-10   ! 27  822
!    :    0.624820e-10   ! 28  751
!    :    0.620696e-10   ! 29  662
!  LaB6(a=4.15695e-10 [m], primitive cubic)
     Data LA_B_6 /   &   !     hkl
       4.156950e-10, &   !  1  100
       2.939408e-10, &   !  2  110
       2.400016e-10, &   !  3  111
       2.078475e-10, &   !  4  200
       1.859045e-10, &   !  5  210
       1.697068e-10, &   !  6  211
       1.469704e-10, &   !  7  220
       1.385650e-10, &   !  8  221 / 300
       1.314543e-10, &   !  9  310
       1.253368e-10, &   ! 10  311
       1.200008e-10, &   ! 11  222
       1.110992e-10, &   ! 12  321
       1.039237e-10, &   ! 13  400
       1.008209e-10, &   ! 14  322 / 410
       0.9798025e-10 &   ! 15  411
       /
!  Paraffin Wax (a = ? [m], ?)
     Data PARAFFIN_WAX / & !     hkl
       4.14e-10,         & !  1  ?
       3.72e-10          & !  2  ?
       /
     Data SILICON / &  !     hkl
       3.1355e-10,  &  !  1  111
       1.9201e-10,  &  !  2  220
       1.6375e-10,  &  !  3  311
       1.3577e-10,  &  !  4  400
       1.2459e-10,  &  !  5  331
       1.1086e-10,  &  !  6  422
       1.0452e-10,  &  !  7  511
       0.9600e-10,  &  !  8  440
       0.9180e-10,  &  !  9  531
       0.8587e-10,  &  ! 10  620
       0.8282e-10,  &  ! 11  533
       0.7839e-10,  &  ! 12  444
       0.7605e-10,  &  ! 13  711
       0.7257e-10,  &  ! 14  642
       0.7070e-10   &  ! 15  553
       /
     Data SILVER_BEHENATE / &  !     hkl
       58.380e-10,  &          !  1  001
       29.190e-10,  &          !  2  002
       19.460e-10,  &          !  3  003
       14.595e-10,  &          !  4  004
       11.676e-10,  &          !  5  005
       09.730e-10,  &          !  6  006
       08.340e-10,  &          !  7  007
       07.2975e-10, &          !  8  008
       06.4867e-10, &          !  9  009
       05.838e-10,  &          ! 10  00 10
       05.3073e-10, &          ! 11  00 11
       04.865e-10,  &          ! 12  00 12
       04.4908e-10  &          ! 13  00 13
       /

!  Sodium Chloride (a = ?, Fm3m)
     Data SODIUM_CHLORIDE /& ! hkl
!      3.260e-10,            !  1  111
       2.821e-10,	   & !  2  200
       1.994e-10,	   & !  3  220
       1.701e-10,	   & !  4  311
       1.628e-10,	   & !  5  222
       1.410e-10,	   & !  6  400
       1.294e-10,	   & !  7  331
       1.261e-10,	   & !  8  420
       1.1515e-10,         & !  9  422
       1.0855e-10,	   & ! 10  511
       0.9969e-10,	   & ! 11  440
       0.9533e-10,	   & ! 12  531
       0.9401e-10,         & ! 13  600
       0.8917e-10,	   & ! 14  620
       0.8601e-10,	   & ! 15  533
       0.8503e-10	   & ! 16  622
       /
!      0.8141e-10,	     ! 17  444
     Data TX(   1) / 'HELP TEXT TO BE ADDED WHEN OPTION COMPLETE ' /
     Data TX(   2) / '(PRESENTLY UNDER DEVELOPMENT)'/
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ID06_CALIBRANT ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_ID06_CALIBRANT ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_ID06_CALIBRANT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Transfer current beam centre
        x_ellipse = experiment%x_beam * experiment%x_pixel_size
        y_ellipse = 0.0
        experiment%y_beam = 0.0 ! By definition
 
!     Select calibrant powder sample
        Call F2D_INP_CALIBRANT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, .True., XAXIS, YAXIS, DATA, MASK, title, xlabel, ylabel, &
          zlabel, experiment, calibrant_sample, status)
 
!     Set D-spacings of required sample
        If (calibrant_sample .Eq. 'CANCEL') Then
 
           Return
 
        Else If (calibrant_sample .Eq. 'ALUMINA (Al2O3)') Then
 
           num_cali_rings = 15
           Do ring = 1, num_cali_rings
              D_SPACINGS(ring) = AL_2_O_3(ring)
           End Do
 
        Else If (calibrant_sample .Eq. 'CERIUM DIOXIDE') Then
 
           num_cali_rings = 15
           Do ring = 1, num_cali_rings
              D_SPACINGS(ring) = CE_O_2(ring)
           End Do
 
        Else If (calibrant_sample .Eq. 'LANTHANUM HEXABORIDE') Then
 
           num_cali_rings = 15
           Do ring = 1, num_cali_rings
              D_SPACINGS(ring) = LA_B_6(ring)
           End Do
 
        Else If (calibrant_sample .Eq. 'PARAFFIN WAX') Then
 
           num_cali_rings = 2
           Do ring = 1, num_cali_rings
              D_SPACINGS(ring) = PARAFFIN_WAX(ring)
           End Do
 
        Else If (calibrant_sample .Eq. 'SILICON') Then
 
           num_cali_rings = 15
           Do ring = 1, num_cali_rings
              D_SPACINGS(ring) = SILICON(ring)
           End Do
 
        Else If (calibrant_sample .Eq. 'SILVER BEHENATE') Then
 
           num_cali_rings = 13
           Do ring = 1, num_cali_rings
              D_SPACINGS(ring) = SILVER_BEHENATE(ring)
           End Do
 
        Else If (calibrant_sample .Eq. 'SODIUM CHLORIDE') Then
 
           num_cali_rings = 15
           Do ring = 1, num_cali_rings
              D_SPACINGS(ring) = SODIUM_CHLORIDE(ring)
           End Do
 
        Else If (calibrant_sample .Eq. 'USER DEFINED') Then
 
!        Input D-spacings from user
           Call F2D_CALIBRANT_USER (Max_angles, retstat, num_cali_rings, &
             D_SPACINGS, status)
 
!        Check file has been input
           If (retstat .Ne. 0) Then
 
!           Redraw image
              Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, XAXIS, &
                YAXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                ylabel, zlabel, status)
              Return
 
           End If
 
        End If
 
        If (first) Then
 
!        Try to input default parameters from the data-base
           Call IO_INQ_IKEYVALUE ('CALI_NUMBER_SECTIONS', num_sections, &
             retstat, status)
           Call IO_INQ_LKEYVALUE ('CALI_REJECT_OUTLIERS', reject_outliers, &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('CALI_REJECT_LIMIT', reject_value, retstat, &
             status)
           Call IO_INQ_LKEYVALUE ('CALI_FULL_INFO', full_info, retstat, &
             status)
           Call IO_INQ_LKEYVALUE ('CALI_REFINE_BEAM', refine_xbeam_centre, &
             retstat, status)
           Call IO_INQ_LKEYVALUE ('CALI_REFINE_DISTANCE', &
             refine_sample_distance, retstat, status)
           Call IO_INQ_LKEYVALUE ('CALI_REFINE_WAVELENGTH', refine_wavelength, &
             retstat, status)
           Call IO_INQ_LKEYVALUE ('CALI_REFINE_TILT', refine_tilt, retstat, &
             status)
           Call IO_INQ_LKEYVALUE ('CALI_EXTRA_ITERATIONS', extra_fits, &
             retstat, status)
           Call IO_INQ_LKEYVALUE ('CORRECT_PARALLAX', correct_parallax, &
             retstat, status)
           Call IO_INQ_LKEYVALUE ('CALI_REFINE_POLARISATION', &
             refine_polarisation, retstat, status)
 
           first = .False.
 
        End If
 
!     Use graphical form to input changes to default values. Allow user to
!     choose number of output bins and control various other options
        num_choices = 15 !!!! DISABLE "REFINE POLARISATION" AT PRESENT
        PROMPT(1) = 'CALIBRANT PATTERN REFINEMENT'
        PROMPT(2) = 'OF DISTANCE WAVELENGTH ETC.'
        BUTTONS(1) = 'DISTANCE'
        BUTTONS(2) = 'BEAM X'
        BUTTONS(3) = 'WAVELENGTH'
        BUTTONS(4) = 'X-PIXEL SIZE'
        BUTTONS(5) = 'Y-PIXEL SIZE'
        BUTTONS(6) = 'ANGULAR SECTIONS'
        BUTTONS(7) = 'REJECT OUTLIERS'
        BUTTONS(8) = 'REJECT LIMIT'
        BUTTONS(9) = 'FULL INFO'
        BUTTONS(10) = 'REFINE BEAM X'
        BUTTONS(11) = 'REFINE DISTANCE'
        BUTTONS(12) = 'REFINE WAVELENGTH'
        BUTTONS(13) = 'REFINE TILT'
        BUTTONS(14) = 'EXTRA ITERATIONS'
        BUTTONS(15) = 'CORRECT PARALLAX'
        BUTTONS(16) = 'REFINE POLARISATION'
        TYPES(1) = Gs_real
        TYPES(2) = Gs_real
        TYPES(3) = Gs_real
        TYPES(4) = Gs_real
        TYPES(5) = Gs_real
        TYPES(6) = Gs_integer
        TYPES(7) = Gs_logical
        TYPES(8) = Gs_real
        TYPES(9) = Gs_logical
        TYPES(10) = Gs_logical
        TYPES(11) = Gs_logical
        TYPES(12) = Gs_logical
        TYPES(13) = Gs_logical
        TYPES(14) = Gs_logical
        TYPES(15) = Gs_logical
        TYPES(16) = Gs_logical
        TEXT(1) = 'SAMPLE TO DETECTOR DISTANCE (MM) (STARTING)'
        TEXT(2) = 'X COORDINATE FOR BEAM CENRE (PIXELS)'
        TEXT(3) = 'WAVELENGTH (ANGSTROMS) (STARTING)'
        TEXT(4) = 'SIZE OF HORIZONTAL PIXELS (MICRONS)'
        TEXT(5) = 'SIZE OF VERTICAL PIXELS (MICRONS)'
        TEXT(6) = 'NUMBER OF AZIMUTHAL SECTIONS'
        TEXT(7) = 'REJECT OUT-LYING POSITIONS AND RE-REFINE'
        TEXT(8) = 'REJECT LIMIT FROM IDEAL (STANDARD DEVIATIONS)'
        TEXT(9) = 'OUTPUT FULL INFORMATION'
        TEXT(10) = 'REFINE X BEAM CENTRE'
        TEXT(11) = 'REFINE SAMPLE TO DETECTOR DISTANCE'
        TEXT(12) = 'REFINE X-RAY WAVELENGTH'
        TEXT(13) = 'REFINE DETECTOR NON-ORTHOGONALITY'
        TEXT(14) = 'FIT INTERMEDIATE NUMBER OF RINGS'
        TEXT(15) = 'CORRECT PARALLAX EFFECT ON ANGULAR POSITION'
        TEXT(16) = 'REFINE VALUE OF POLARISATION FACTOR'
        FULL_PROMPTS(1) = 'Enter approximate sample to ' // &
          'detector distance (mm)'
        FULL_PROMPTS(2) = 'Enter X-coordinate of beam centre in pixels '
        FULL_PROMPTS(3) = 'Enter approximate X-ray ' // &
          'wavelength (Angstroms)'
        FULL_PROMPTS(4) = 'Enter dimension of pixels ' // &
          'horizontally, as displayed (mm)'
        FULL_PROMPTS(5) = 'Enter dimension of pixels ' // &
          'vertically, as displayed (mm)'
        FULL_PROMPTS(6) = 'Enter number of azimuthal ' // &
          'divisions to find average ring positions'
        FULL_PROMPTS(7) = 'Enter "YES" to reject badly fitting' // &
          'positions and re-fit without them'
        FULL_PROMPTS(8) = 'Enter number of standard deviations' // &
          ' from ideal before rejection'
        FULL_PROMPTS(9) = 'Enter "YES" to output full ' // &
          'diagnostics information in terminal window'
        FULL_PROMPTS(10) = 'Refine x beam centre'
        FULL_PROMPTS(11) = 'Refine sample to detector distance'
        FULL_PROMPTS(12) = 'Refine x-ray wavelength'
        FULL_PROMPTS(13) = 'Refine detector non-orthogonality'
        FULL_PROMPTS(14) = 'Enter "YES" to add extra partial' // &
          'fitting iterations for difficult problems'
        FULL_PROMPTS(15) = 'Enter "YES" to account for the effect of ' // &
          'parallax on angular position'
        FULL_PROMPTS(16) = 'Enter "YES" to refine value' // &
          'of the polarisation factor from the data intensities'
        BOUND(1) = .True.
        BOUND(2) = .True.
        BOUND(3) = .True.
        BOUND(4) = .True.
        BOUND(5) = .True.
        BOUND(6) = .True.
        BOUND(7) = .True.
        BOUND(8) = .True.
        BOUND(9) = .True.
        REALS_LOWER(1) = 0.0
        REALS_UPPER(1) = 10000.0
        REALS_LOWER(2) = -100000.0
        REALS_UPPER(2) = 100000.0
        REALS_LOWER(3) = 0.0001
        REALS_UPPER(3) = 10000.0
        REALS_LOWER(4) = 0.01
        REALS_UPPER(4) = 10000.0
        REALS_LOWER(5) = 0.01
        REALS_UPPER(5) = 10000.0
        INTS_LOWER(6) = 10
        INTS_UPPER(6) = 360
        REALS_LOWER(8) = 2.0
        REALS_UPPER(8) = 100.0
 
        REALS(1) = experiment%detector_distance * 1000.0
        REALS(2) = experiment%x_beam
        REALS(3) = experiment%wavelength * 1.0e10
        REALS(4) = experiment%x_pixel_size * 1.0e6
        REALS(5) = experiment%y_pixel_size * 1.0e6
        INTEGERS(6) = num_sections
        LOGICALS(7) = reject_outliers
        REALS(8) = reject_value
        LOGICALS(9) = full_info
        LOGICALS(10) = refine_xbeam_centre
        LOGICALS(11) = refine_sample_distance
        LOGICALS(12) = refine_wavelength
        LOGICALS(13) = refine_tilt
        LOGICALS(14) = extra_fits
        LOGICALS(15) = correct_parallax
        LOGICALS(16) = refine_polarisation
 
!     Output interactive graphical form
        Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, Max_choices, &
          num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, INTS_LOWER, &
          INTS_UPPER, REALS_LOWER, REALS_UPPER, INTEGERS, LOGICALS, REALS, &
          STRINGS, retstat, status)
 
!     Check if "CANCEL" entered
        If (retstat .Eq. -1) Then
 
!        Redraw image
           Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, XAXIS, &
             YAXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
             zlabel, status)
           Return
 
        End If
 
!     Set resulting values
        experiment%detector_distance = REALS(1) / 1000.0
        experiment%x_beam = REALS(2)
        experiment%wavelength = REALS(3) / 1.0e10
        experiment%x_pixel_size =  REALS(4) / 1.0e6
        experiment%y_pixel_size =  REALS(5) / 1.0e6
        num_sections =  INTEGERS(6)
        reject_outliers = LOGICALS(7)
        reject_value = REALS(8)
        full_info = LOGICALS(9)
        refine_xbeam_centre = LOGICALS(10)
        refine_sample_distance = LOGICALS(11)
        refine_wavelength = LOGICALS(12)
        refine_tilt = LOGICALS(13)
        extra_fits = LOGICALS(14)
        correct_parallax = LOGICALS(15)
        refine_polarisation = LOGICALS(16)
 
!     Save parameter values in the data-base
        Call IO_SET_IKEYVALUE ('CALI_NUMBER_SECTIONS', num_sections, retstat, &
          status)
        Call IO_SET_LKEYVALUE ('CALI_REJECT_OUTLIERS', reject_outliers, &
          retstat, status)
        Call IO_SET_RKEYVALUE ('CALI_REJECT_LIMIT', reject_value, retstat, &
          status)
        Call IO_SET_LKEYVALUE ('CALI_FULL_INFO', full_info, retstat, status)
        Call IO_SET_LKEYVALUE ('CALI_REFINE_BEAM', refine_xbeam_centre, &
          retstat, status)
        Call IO_SET_LKEYVALUE ('CALI_REFINE_DISTANCE', refine_sample_distance, &
          retstat, status)
        Call IO_SET_LKEYVALUE ('CALI_REFINE_WAVELENGTH', refine_wavelength, &
          retstat, status)
        Call IO_SET_LKEYVALUE ('CALI_REFINE_TILT', refine_tilt, retstat, &
          status)
        Call IO_SET_LKEYVALUE ('CALI_EXTRA_ITERATIONS', extra_fits, retstat, &
          status)
        Call IO_SET_LKEYVALUE ('CORRECT_PARALLAX', correct_parallax, &
          retstat, status)
        Call IO_SET_LKEYVALUE ('CALI_REFINE_POLARISATION', &
          refine_polarisation, retstat, status)
 
        If (correct_parallax) Then

!        Input detector characteristics
           Call F2D_INP_DETECTOR (experiment, status)

!        Alter D-spacings to take into account parallax
           Call F2D_PARALLAX_DSPACINGS (experiment, max_angles, &
             num_cali_rings, D_SPACINGS, status)

        End If

!     Save beam centre, if not to be refined
        If (.Not. refine_xbeam_centre) Then
           x_ellipse = experiment%x_beam * experiment%x_pixel_size
           y_ellipse = 0.0
        End If
 
!     Need to draw image
        Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, XAXIS, YAXIS, &
          xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
          status)
 
!     By default set good return status
        retstat = 0
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_ID06_CALIBRANT: After GS_2DIMAGE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input coordinate
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
          X_POLAR, Y_POLAR, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_ID06_CALIBRANT: After GS_INPS_FCOORDINATES'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           status = St_goodvalue
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If

        If (refine_xbeam_centre) Then

!        Determine if beam centre is low or high
           low_centre = X_POLAR(1, 1) .Le. (xendelm - xstrelm) / 2.0

!        Estimate position of beam-centre in X-direction
           two_theta = 2.0 * Asin(experiment%wavelength / (2.0 * D_SPACINGS(1)))
           distance_centre = Tan(two_theta) * experiment%detector_distance
           If (low_centre) Then
              beam_centre = X_POLAR(1, 1) - &
                distance_centre / experiment%x_pixel_size
           Else
              beam_centre = X_POLAR(1, 1) + &
                distance_centre / experiment%x_pixel_size
           End If
           experiment%x_beam = beam_centre

        End If

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!       Write (*, '(''two_theta = '', f8.2, '' degrees'')') two_theta * 180.0/Pi
!        Write (*, '(''distance centre = '', f10.4, '' metres'')') &
!          distance_centre
!        Write (*, '(''distance centre = '', f8.2, '' pixels'')') &
!          distance_centre / experiment%x_pixel_size
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (MESSAGE(1), &
          '(''INFO: Estimated Beam centre = '', f10.2, '' pixels'')') &
          beam_centre
        Call IO_WRITE (MESSAGE(1), status)
        
!     Find coordinate positions on line
        num_rings = 1
        half_search_distance = 40
        Call F2D_FIND_LINES (full_info, xmaxdat, ymaxdat, xnumdat, ynumdat, &
          xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, experiment, &
          low_centre, &
          F2d_max_rings, num_rings, D_SPACINGS, half_search_distance, &
          azimuth_start, azimuth_end, &
          num_sections, .False., F2d_max_rcoordinates, F2D_NUM_RCOORDINATES, &
          X_POLAR, Y_POLAR, F2D_RINTENSITIES, &
          F2D_AZIMUTHS, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_ID06_CALIBRANT: After F2D_FIND_LINE_COORDS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Output found coordinates on image
        Call F2D_ID06_DISPLAY (F2d_max_rcoordinates, F2d_max_rings, num_rings, &
          F2D_NUM_RCOORDINATES, X_POLAR, Y_POLAR, status)

!     Convert polar coordinates to cartesian coordinates
        Call F2D_POLAR_CARTESIAN (F2d_max_rings, F2d_max_rcoordinates, &
          num_rings, F2D_NUM_RCOORDINATES, experiment%x_beam, &
          experiment%x_pixel_size, F2D_AZIMUTHS, &
          X_POLAR, Y_POLAR, F2D_X_RCOORDINATES, F2D_Y_RCOORDINATES, status)

!     Fit ellipse to all inner ring coordinates to obtain better estimates for
!     the beam centre, radius, etc.
        If (full_info) Then
           Call IO_WRITE ('INFO: Fitting ellipse to ' // &
             'centre of gravity coordinates', status)
        End If

        If (F2D_NUM_RCOORDINATES(1) .Ge. 3) Then

           Call F2D_LSQELLIPSE (F2d_max_rcoordinates, F2D_NUM_RCOORDINATES(1), &
             F2D_X_RCOORDINATES, F2D_Y_RCOORDINATES, &
             x_ellipse, y_ellipse, radius1, &
             radius2, angle, radial_error, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''x_ellipse = '', f10.5)') &
!             x_ellipse / experiment%x_pixel_size
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

        Else
           Call IO_WRITE ('WARNING: Not enough coordinates found on ring', &
             status)
        End If

!     Output information to user
        If (full_info) Then
           Call F2D_TILTINFO ( &
             experiment%x_pixel_size, experiment%y_pixel_size, &
             F2D_NUM_RCOORDINATES(1), x_ellipse, y_ellipse, &
             radius1, radius2, angle, radial_error, status)
        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Search up to first 2 Bragg rings
        num_rings = Min(2, num_cali_rings)
        Call F2D_FIND_LINES (full_info, xmaxdat, ymaxdat, xnumdat, ynumdat, &
          xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, experiment, &
          low_centre, &
          F2d_max_rings, num_rings, D_SPACINGS, half_search_distance, &
          azimuth_start, azimuth_end, &
          num_sections, .False., F2d_max_rcoordinates, F2D_NUM_RCOORDINATES, &
          X_POLAR, Y_POLAR, F2D_RINTENSITIES, &
          F2D_AZIMUTHS, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Ring,   Coordinate, AZIMUTH'')')
!        Do ring = 1, num_rings

!           Do i = 1, F2D_NUM_RCOORDINATES(ring)
!              Write (*, '(i2, i4, '' '', 4g14.7)') ring, i, &
!                F2D_AZIMUTHS(i, ring) * 180.0 / Pi
!           End Do

!        End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Output found coordinates on image
        Call F2D_ID06_DISPLAY (F2d_max_rcoordinates, F2d_max_rings, num_rings, &
          F2D_NUM_RCOORDINATES, X_POLAR, Y_POLAR, status)

        experiment%tilt_angle = Acos(radius2 / radius1) ! Approximate +- tilt
        experiment%tilt_plane_rotation = angle

        If (refine_xbeam_centre) Then
           experiment%x_beam = x_ellipse / experiment%x_pixel_size + &
             Sqrt(radius1**2 - radius2**2) * Cos(angle) / experiment%x_pixel_size
        End If

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''x_ellipse = '', f10.5, '' x_foci = '', f10.5)') &
          x_ellipse / experiment%x_pixel_size, experiment%x_beam
        Write (*, '(''Tilt = '', f12.5, '' degrees'')') &
          experiment%tilt_angle * 180.0 / Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Convert polar coordinates to cartesian coordinates
        Call F2D_POLAR_CARTESIAN (F2d_max_rings, F2d_max_rcoordinates, &
          num_rings, F2D_NUM_RCOORDINATES, experiment%x_beam, &
          experiment%x_pixel_size, F2D_AZIMUTHS, &
          X_POLAR, Y_POLAR, F2D_X_RCOORDINATES, F2D_Y_RCOORDINATES, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Ring,   Coordinate,     X,     Y,  AZIMUTH, INTENSITY'')')
!        Do ring = 1, num_rings

!           Do i = 1, F2D_NUM_RCOORDINATES(ring)
!              Write (*, '(i2, i4, 4g14.7)') ring, i, &
!                F2D_X_RCOORDINATES(i, ring), F2D_Y_RCOORDINATES(i, ring), &
!                F2D_AZIMUTHS(i, ring) * 180.0 / Pi, F2D_RINTENSITIES(i, ring)
!           End Do

!        End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Refine tilt to first two diffraction rings
        Call F2D_FITCALIBRANT (full_info, F2d_max_rings, num_rings, &
          D_SPACINGS, weighted_fitting, reject_outliers, reject_value, &
          refine_xbeam_centre, .False., refine_sample_distance, .False., &
          refine_tilt, experiment, radial_error, status)

        If (full_info .Or. num_cali_rings .Le. 4) Then
            Write (MESSAGE(1), '(''INFO: Refined ' // &
             'Beam centre = '', 2f12.3, '' (pixels)'')') &
             experiment%x_beam, experiment%y_beam
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'Beam centre = '', 2f12.3, '' (mm)'')') &
             experiment%x_beam * experiment%x_pixel_size * 1000.0, &
             experiment%y_beam * experiment%y_pixel_size * 1000.0
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'sample to detector distance = '', f12.3, '' mm'')') &
             experiment%detector_distance * 1000.0
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'tilt plane rotation angle = '', f12.3, '' degrees'')') &
             experiment%tilt_plane_rotation * 180.0 / Pi
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'tilt angle = '', f12.3, '' degrees'')') &
             experiment%tilt_angle * 180.0 / Pi
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: ROT X = '', f12.3, ' // &
             '''   ROT Y = '', f12.3, '' degrees'')') &
             Cos(experiment%tilt_plane_rotation) &
               * experiment%tilt_angle * 180.0 / Pi, &
             Sin(experiment%tilt_plane_rotation) * experiment%tilt_angle &
               * 180.0 / Pi
           Call IO_WRITE (MESSAGE(1), status)
        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        If (refine_xbeam_centre) Then
        
!        Try 2nd foci
           experiment%tilt_angle = -Acos(radius2 / radius1) ! Approximate +- tilt
           experiment%tilt_plane_rotation = angle
           experiment%x_beam = x_ellipse / experiment%x_pixel_size - &
             Sqrt(radius1**2 - radius2**2) * Cos(angle) / experiment%x_pixel_size

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
           Write (*, '(''x_ellipse = '', f10.5, '' x_foci = '', f10.5)') &
             x_ellipse / experiment%x_pixel_size, experiment%x_beam
           Write (*, '(''Tilt = '', f12.5, '' degrees'')') &
             experiment%tilt_angle * 180.0 / Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!        Convert polar coordinates to cartesian coordinates
           Call F2D_POLAR_CARTESIAN (F2d_max_rings, F2d_max_rcoordinates, &
             num_rings, F2D_NUM_RCOORDINATES, experiment%x_beam, &
             experiment%x_pixel_size, F2D_AZIMUTHS, &
             X_POLAR, Y_POLAR, F2D_X_RCOORDINATES, F2D_Y_RCOORDINATES, status)

!        Refine tilt to first two diffraction rings
           Call F2D_FITCALIBRANT (full_info, F2d_max_rings, num_rings, &
             D_SPACINGS, weighted_fitting, reject_outliers, reject_value, &
             refine_xbeam_centre, .False., refine_sample_distance, .False., &
             refine_tilt, experiment, radial_error, status)

           If (full_info .Or. num_cali_rings .Le. 4) Then
              Write (MESSAGE(1), '(''INFO: Refined ' // &
                'Beam centre = '', 2f12.3, '' (pixels)'')') &
                experiment%x_beam, experiment%y_beam
              Call IO_WRITE (MESSAGE(1), status)
              Write (MESSAGE(1), '(''INFO: Refined ' // &
                'Beam centre = '', 2f12.3, '' (mm)'')') &
                experiment%x_beam * experiment%x_pixel_size * 1000.0, &
                experiment%y_beam * experiment%y_pixel_size * 1000.0
              Call IO_WRITE (MESSAGE(1), status)
              Write (MESSAGE(1), '(''INFO: Refined ' // &
                'sample to detector distance = '', f12.3, '' mm'')') &
                experiment%detector_distance * 1000.0
              Call IO_WRITE (MESSAGE(1), status)
              Write (MESSAGE(1), '(''INFO: Refined ' // &
                'tilt plane rotation angle = '', f12.3, '' degrees'')') &
                experiment%tilt_plane_rotation * 180.0 / Pi
              Call IO_WRITE (MESSAGE(1), status)
              Write (MESSAGE(1), '(''INFO: Refined ' // &
                'tilt angle = '', f12.3, '' degrees'')') &
                experiment%tilt_angle * 180.0 / Pi
              Call IO_WRITE (MESSAGE(1), status)
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
!     For testing
!        Return
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Search up to first 4 Bragg rings
        num_rings = Min(4, num_cali_rings)
        Call F2D_FIND_LINES (full_info, xmaxdat, ymaxdat, xnumdat, ynumdat, &
          xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, experiment, &
          low_centre, &
          F2d_max_rings, num_rings, D_SPACINGS, half_search_distance, &
          azimuth_start, azimuth_end, &
          num_sections, .False., F2d_max_rcoordinates, F2D_NUM_RCOORDINATES, &
          X_POLAR, Y_POLAR, F2D_RINTENSITIES, &
          F2D_AZIMUTHS, status)

!     Re-draw image
        Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, XAXIS, YAXIS, &
          xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
          status)
 
!     Output found coordinates on image
        Call F2D_ID06_DISPLAY (F2d_max_rcoordinates, F2d_max_rings, num_rings, &
          F2D_NUM_RCOORDINATES, X_POLAR, Y_POLAR, status)

!     Convert polar coordinates to cartesian coordinates
        Call F2D_POLAR_CARTESIAN (F2d_max_rings, F2d_max_rcoordinates, &
          num_rings, F2D_NUM_RCOORDINATES, experiment%x_beam, &
          experiment%x_pixel_size, F2D_AZIMUTHS, &
          X_POLAR, Y_POLAR, F2D_X_RCOORDINATES, F2D_Y_RCOORDINATES, status)

!     Refine tilt to first four diffraction rings
        Call F2D_FITCALIBRANT (full_info, F2d_max_rings, num_rings, &
          D_SPACINGS, weighted_fitting, reject_outliers, reject_value, &
          refine_xbeam_centre, .False., refine_sample_distance, .False., &
          refine_tilt, experiment, radial_error, status)

        If (full_info .Or. num_cali_rings .Le. 4) Then
            Write (MESSAGE(1), '(''INFO: Refined ' // &
             'Beam centre = '', 2f12.3, '' (pixels)'')') &
             experiment%x_beam, experiment%y_beam
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'Beam centre = '', 2f12.3, '' (mm)'')') &
             experiment%x_beam * experiment%x_pixel_size * 1000.0, &
             experiment%y_beam * experiment%y_pixel_size * 1000.0
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'sample to detector distance = '', f12.3, '' mm'')') &
             experiment%detector_distance * 1000.0
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'tilt plane rotation angle = '', f12.3, '' degrees'')') &
             experiment%tilt_plane_rotation * 180.0 / Pi
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'tilt angle = '', f12.3, '' degrees'')') &
             experiment%tilt_angle * 180.0 / Pi
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: ROT X = '', f12.3, ' // &
             '''   ROT Y = '', f12.3, '' degrees'')') &
             Cos(experiment%tilt_plane_rotation) &
             * experiment%tilt_angle * 180.0 / Pi, &
             Sin(experiment%tilt_plane_rotation) * experiment%tilt_angle &
             * 180.0 / Pi
           Call IO_WRITE (MESSAGE(1), status)
        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Search up to first 7 Bragg rings
        num_rings = Min(7, num_cali_rings)
        Call F2D_FIND_LINES (full_info, xmaxdat, ymaxdat, xnumdat, ynumdat, &
          xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, experiment, &
          low_centre, &
          F2d_max_rings, num_rings, D_SPACINGS, half_search_distance, &
          azimuth_start, azimuth_end, &
          num_sections, .False., F2d_max_rcoordinates, F2D_NUM_RCOORDINATES, &
          X_POLAR, Y_POLAR, F2D_RINTENSITIES, F2D_AZIMUTHS, status)

!     Re-draw image
        Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, XAXIS, YAXIS, &
          xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
          status)
 
!     Output found coordinates on image
        Call F2D_ID06_DISPLAY (F2d_max_rcoordinates, F2d_max_rings, num_rings, &
          F2D_NUM_RCOORDINATES, X_POLAR, Y_POLAR, status)

!     Convert polar coordinates to cartesian coordinates
        Call F2D_POLAR_CARTESIAN (F2d_max_rings, F2d_max_rcoordinates, &
          num_rings, F2D_NUM_RCOORDINATES, experiment%x_beam, &
          experiment%x_pixel_size, F2D_AZIMUTHS, &
          X_POLAR, Y_POLAR, F2D_X_RCOORDINATES, F2D_Y_RCOORDINATES, status)

!     Refine tilt to first 7 diffraction rings
        Call F2D_FITCALIBRANT (full_info, F2d_max_rings, num_rings, &
          D_SPACINGS, weighted_fitting, reject_outliers, &
          reject_value, refine_xbeam_centre, .False., refine_sample_distance, &
          refine_wavelength, refine_tilt, experiment, radial_error, status)

        If (full_info .Or. num_cali_rings .Le. 4) Then
            Write (MESSAGE(1), '(''INFO: Refined ' // &
             'Beam centre = '', 2f12.3, '' (pixels)'')') &
             experiment%x_beam, experiment%y_beam
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'Beam centre = '', 2f12.3, '' (mm)'')') &
             experiment%x_beam * experiment%x_pixel_size * 1000.0, &
             experiment%y_beam * experiment%y_pixel_size * 1000.0
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'sample to detector distance = '', f12.3, '' mm'')') &
             experiment%detector_distance * 1000.0
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'wavelength = '', f12.5, '' Angstroms'')') &
             experiment%wavelength * 1.0e10
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''      Energy (keV) = '', f12.5)') &
             12.3984193 / (experiment%wavelength * 1.0e10)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'tilt plane rotation angle = '', f12.3, '' degrees'')') &
             experiment%tilt_plane_rotation * 180.0 / Pi
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'tilt angle = '', f12.3, '' degrees'')') &
             experiment%tilt_angle * 180.0 / Pi
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: ROT X = '', f12.3, ' // &
             '''   ROT Y = '', f12.3, '' degrees'')') &
             Cos(experiment%tilt_plane_rotation) &
             * experiment%tilt_angle * 180.0 / Pi, &
             Sin(experiment%tilt_plane_rotation) * experiment%tilt_angle &
             * 180.0 / Pi
           Call IO_WRITE (MESSAGE(1), status)
        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Search up to first 15 Bragg rings
        num_rings = Min(15, num_cali_rings)
        Call F2D_FIND_LINES (full_info, xmaxdat, ymaxdat, xnumdat, ynumdat, &
          xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, experiment, &
          low_centre, &
          F2d_max_rings, num_rings, D_SPACINGS, half_search_distance, &
          azimuth_start, azimuth_end, &
          num_sections, .False., F2d_max_rcoordinates, F2D_NUM_RCOORDINATES, &
          X_POLAR, Y_POLAR, F2D_RINTENSITIES, F2D_AZIMUTHS, status)

!     Re-draw image
        Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, XAXIS, YAXIS, &
          xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
          status)
 
!     Output found coordinates on image
        Call F2D_ID06_DISPLAY (F2d_max_rcoordinates, F2d_max_rings, num_rings, &
          F2D_NUM_RCOORDINATES, X_POLAR, Y_POLAR, status)

!     Convert polar coordinates to cartesian coordinates
        Call F2D_POLAR_CARTESIAN (F2d_max_rings, F2d_max_rcoordinates, &
          num_rings, F2D_NUM_RCOORDINATES, experiment%x_beam, &
          experiment%x_pixel_size, F2D_AZIMUTHS, &
          X_POLAR, Y_POLAR, F2D_X_RCOORDINATES, F2D_Y_RCOORDINATES, status)

!     Refine tilt to first <= 15 diffraction rings
        Call F2D_FITCALIBRANT (full_info, F2d_max_rings, num_rings, &
          D_SPACINGS, weighted_fitting, reject_outliers, &
          reject_value, refine_xbeam_centre, .False., refine_sample_distance, &
          refine_wavelength, refine_tilt, experiment, radial_error, status)

!     Draw positions of un-tilted lines on image
        Call F2D_DRAW_ID06_LINES (experiment, Max_angles, num_cali_rings, &
          D_SPACINGS, status)

        If (full_info .Or. num_cali_rings .Le. 4) Then
            Write (MESSAGE(1), '(''INFO: Refined ' // &
             'Beam centre = '', 2f12.3, '' (pixels)'')') &
             experiment%x_beam, experiment%y_beam
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'Beam centre = '', 2f12.3, '' (mm)'')') &
             experiment%x_beam * experiment%x_pixel_size * 1000.0, &
             experiment%y_beam * experiment%y_pixel_size * 1000.0
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'sample to detector distance = '', f12.3, '' mm'')') &
             experiment%detector_distance * 1000.0
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'wavelength = '', f12.5, '' Angstroms'')') &
             experiment%wavelength * 1.0e10
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''      Energy (keV) = '', f12.5)') &
             12.3984193 / (experiment%wavelength * 1.0e10)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'tilt plane rotation angle = '', f12.3, '' degrees'')') &
             experiment%tilt_plane_rotation * 180.0 / Pi
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: Refined ' // &
             'tilt angle = '', f12.3, '' degrees'')') &
             experiment%tilt_angle * 180.0 / Pi
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: ROT X = '', f12.3, ' // &
             '''   ROT Y = '', f12.3, '' degrees'')') &
             Cos(experiment%tilt_plane_rotation) &
             * experiment%tilt_angle * 180.0 / Pi, &
             Sin(experiment%tilt_plane_rotation) * experiment%tilt_angle &
             * 180.0 / Pi
           Call IO_WRITE (MESSAGE(1), status)
        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8

     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_ID06_CALIBRANT: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_ID06_CALIBRANT
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
