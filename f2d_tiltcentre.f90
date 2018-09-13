!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_tiltcentre.f90 *
!  *                    *
!  **********************
 
!+ F2D_TILTCENTRE -  TILT/ beam CENTRE determination
     Subroutine F2D_TILTCENTRE (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, X_AXIS, Y_AXIS, DATA, MASK, title, xlabel, ylabel, &
       zlabel, experiment, status)
!  Description:
!    Determines detector tilt and beam centre from a powder ring
!    from an image of regular rectangular pixels
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Mar-2006: V0.27 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    23-Oct-1998: V0.26 Remove call to "F2D_INQ_GEOMETRY" (Hammersley)
!    30-Jan-1998: V0.25 Declare unused character string for call to "GS_FORM" 
!      (Hammersley)
!    03-Feb-1998: V0.24 Cope with "CANCEL" from geometry form (Hammersley)
!    11-Jul-1997: V0.23 Correct argument list of second call to
!      "F2D_FINDRINGS" (Hammersley)
!    08-Jul-1997: V0.22 Add "F2D_AZIMUTHS" array for "F2D_FINDRINGS" 
!      (Hammersley)
!    16-Dec-1996: V0.21 Avoid open strings crossing lines (Hammersley)
!    17-Oct-1996: V0.20 Output more figures for results of tilt fitting 
!      (Hammersley)
!    26-Aug-1996: V0.19 Return if user escape is input (Hammersley)
!    23-Aug-1996: V0.18 Changes to "F2D_INPRADIA" (Hammersley)
!    22-Aug-1996: V0.17 Add option to not fit tilt (Hammersley)
!    25-Apr-1996: V0.16 Add detailed help text to form (Hammersley)
!    23-Apr-1996: V0.15 Add beam centre X and Y to GUI form (Hammersley)
!    29-Mar-1996: V0.14 Immediate return after "CANCEL" from form (Hammersley)
!    08-Mar-1996: V0.13 Check for "CANCEL" from form (Hammersley)
!    15-Feb-1996: V0.12 Option of GUI (Hammersley)
!    11-Feb-1996: V0.11 Changes to "F2D_INP_SAMPLEDISTANCE" (Hammersley)
!    10-Feb-1996: V0.10 Make initialisation more robust (Hammersley)
!    04-Jan-1996: V0.9 Changes for IBM AIX "xlf" compiler: Doesn't
!      like "g" format descriptors without width specifiers (Hammersley)
!    26-Oct-1995: V0.8 Better user messages (Hammersley)
!    05-Apr-1995: V0.7 Allow pixel sizes to be changed (Hammersley)
!    22-Feb-1995: V0.6 Make "detector_gain" an import/export argument
!      (Hammersley)
!    21-Feb-1995: V0.5 Only produce alternative solution if the
!      beam centre is refined (Hammersley)
!    17-Feb-1995: V0.4 Set-up peak coordinates intensity arrays
!      and enable weighted fitting (Hammersley)
!    27-Jan-1995: V0.3 Pass coordinates through common to
!      "F2D_LSQPOWDER" (Hammersley)
!    21-Jan-1995: V0.2 Output graphically the centre of gravity
!      coordinates calculated for the fitted ring (Hammersley)
!    18-Jan-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'f2d_fitrings.inc' ! Powder rings least squares
!      fitting common, passes coordinate values to "F2D_LSQPOWDER"
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: xendelm ! The X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! The Y-pixel number for the end of the ROI
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Axis data for the Y-axis
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.27' ! Version number
     Integer, Parameter :: Max_angles = F2d_max_rings ! Dimension of "ANGLES" 
!      array
     Integer, Parameter :: Max_chars =  49 ! Maximum number of characters
!      in a line
     Integer, Parameter :: Max_lines =  111 ! Number of lines in message
     Integer, Parameter :: Max_choices = 11 ! Maximum number of choices
!      (need one extra space)
!  Local Variables:
     Integer :: num_choices ! Number of choices
     Integer :: num_rings ! Number of radia of powder rings which have
!      been input (including initial ring), which is the number of
!      rings which will be used for fitting the tilt/beam centre
     Integer, Save :: num_sections = 90 ! Number of angular sections to be
!      used to calculate ring centre coordinates
     Integer :: retstat ! "F2D_INP_ELLIPSE"  status return variable:
!      0 = Good status, return arguments defined
!      -1 = User escape or other 'status' problem
     Integer :: ring ! Loop variable for powder rings
     Integer :: solution ! Choice of solution 1 or 2
     Logical, Save :: full_info = .True. ! .True., if full information is
!      required by the user
     Logical, Save :: refine_beam_centre = .True. ! .True., if the beam
!      centre position is to be refined
     Logical, Save :: refine_sample_distance = .False. ! .True., if the
!      sample to detector distance is to be refined
     Logical, Save :: refine_tilt = .True. ! .True., if the detector
!      non-orthogonality is to be refined
     Logical, Save :: reject_outliers = .True. ! .True., if outliers are to
!      be rejected
     Logical, Save :: weighted_fitting = .False. ! .True., if the fitting
!      of tilt and beam centre is to be weighted by the intensities
     Real :: angle1 ! Orientation angle of first axis of best fit ellipse
     Real :: half_search_distance ! Distance of half the width for the
!      search region (metres)
     Real :: radial_error1 ! Estimated average error (radially) in
!      coordinate positions (metres)
     Real :: radial_error2 ! Estimated average error (radially) in
!      coordinate positions (metres)
     Real :: radius1 ! Longer radius of ellipse (metres)
     Real :: radius2 ! Shorter radius of ellipse (metres)
     Real, Save :: reject_value = 4.0 ! If a coordinate is more than this
!      number of standard deviations from the predicted ring position (radially)
!      than it will be removed from the fit
     Real :: rotation1 ! Angle of rotation of the tilt plane (about the Z-axis)
     Real :: rotation2 ! Angle of rotation of the tilt plane (about the Z-axis)
     Real :: tilt1 ! Angle of detector tilt in the X-direction
     Real :: tilt2 ! Angle of detector tilt in the X-direction
     Real :: x_beam ! X-coordinate of beam centre (metres)
     Real :: x_beam1 ! X-centre of fitted beam centre (metres)
     Real :: x_beam2 ! X-centre of fitted beam centre (metres)
     Real :: y_beam ! Y-coordinate of beam centre (metres)
     Real :: y_beam1 ! Y-centre of fitted beam centre (metres)
     Real :: y_beam2 ! Y-centre of fitted beam centre (metres)
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 70) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 80) :: MESSAGE(8) ! User text
     Character(Len = 70) :: PROMPT(3) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = Max_chars), Save :: TX(Max_lines) ! Text array
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
     Real :: ANGLE_CONES1(Max_angles) ! Half opening angle of diffraction
!      cones for first solution
     Real :: ANGLE_CONES2(Max_angles) ! Half opening angle of diffraction
!      cones for second solution
     Real, Save :: RADIA(Max_angles) ! The radius of powder rings to be
!      used for fitting the tilt and beam centre (metres)
     Real :: REALS_LOWER(Max_choices) ! Lower bounds on reals
     Real :: REALS_UPPER(Max_choices) ! Upper bounds on reals
     Real :: REALS(Max_choices) ! Reals to be changed
!  External Functions:
!  Local Data:
     Data TX(   1) / ' ' /
     Data TX(   2) / '      -------------------'/
     Data TX(   3) / '      TILT Command Form Help'/
     Data TX(   4) / '      -------------------'/
     Data TX(   5) / ' ' /
     Data TX(   6) / 'To correct for non-orthogonality of the detector'/
     Data TX(   7) / 'to the direct beam, the "TILT" button allows'/
     Data TX(   8) / 'the non-orthogonality (tilt) to be determined'/
     Data TX(   9) / 'as well as the beam centre from the shape of'/
     Data TX(  10) / 'recorded powder rings.'/
     Data TX(  11) / ' ' /
     Data TX(  12) / 'One or more powder rings are used to refine'/
     Data TX(  13) / 'beam centre and non-orthogonality parameters.'/
     Data TX(  14) / 'This form allows a number of choices in the'/
     Data TX(  15) / 'manner in which this works.'/
     Data TX(  16) / ' ' /
     Data TX(  17) / 'This operation works on the current selected'/
     Data TX(  18) / 'data region, and does not take into account'/
     Data TX(  19) / '"masked-off" data. You may want to use the'/
     Data TX(  20) / '"MASK" option to mask-off data prior to using'/
     Data TX(  21) / 'the "TILT" option. Or, if problems are encount-'/
     Data TX(  22) / 'ered during the "TILT" evaluation, problem data'/
     Data TX(  23) / 'regions may be identified and masked-out prior'/
     Data TX(  24) / 'to another try with "TILT".'/
     Data TX(  25) / ' ' /
     Data TX(  26) / 'An initial approximate beam centre is obtained'/
     Data TX(  27) / 'from a choice of methods. This beam centre is'/
     Data TX(  28) / 'then used to define a search region around one'/
     Data TX(  29) / 'powder ring. This ring should be strong and'/
     Data TX(  30) / 'well defined. The whole of the ring should be'/
     Data TX(  31) / 'within the search region through the defined'/
     Data TX(  32) / 'azimuthal range.'/
     Data TX(  33) / ' ' /
     Data TX(  34) / 'Other rings may also be selected to be used to'/
     Data TX(  35) / 'refine the beam centre and tilt parameters.'/
     Data TX(  36) / 'Ideally two or more rings should be used, but'/
     Data TX(  37) / 'one will work. Complete rings at high angle'/
     Data TX(  38) / 'will have the largest effect.'/
     Data TX(  39) / ' ' /
     Data TX(  40) / 'From the starting ring coordinates a best fit'/
     Data TX(  41) / 'circle is found, followed at an ellipse. The'/
     Data TX(  42) / 'powder ring section centres are re-calculated'/
     Data TX(  43) / 'for all the rings. The beam centre and tilt'/
     Data TX(  44) / 'angles are refined to these positions. There'/
     Data TX(  45) / 'is the option of rejecting badly fitting'/
     Data TX(  46) / 'positions and re-fitting the data.'/
     Data TX(  47) / ' ' /
     Data TX(  48) / 'For a given set of ellipses, two possible'/
     Data TX(  49) / 'beam centre / tilt angle combinations are'/
     Data TX(  50) / 'theoretically possible and indistinghable.'/
     Data TX(  51) / 'Only if the beam centre and/ or the tilt'/
     Data TX(  52) / 'angles are fixed is there an unique solution.'/
     Data TX(  53) / 'If the beam centre and the tilt are being'/
     Data TX(  54) / 'fitted you will be asked to chosen between'/
     Data TX(  55) / 'the two solutions. (In practice owing to'/
     Data TX(  56) / 'the uncertainty in the data the same solution'/
     Data TX(  57) / 'may be produced for both minimisations.)'/
     Data TX(  58) / 'If there are two solutions you may be able'/
     Data TX(  59) / 'to choose the correct solution by other'/
     Data TX(  60) / 'knowledge from the data e.g. scatter from'/
     Data TX(  61) / 'the beam-stop. If you can''t choose between'/
     Data TX(  62) / 'the solutions, do not worry; the calculated'/
     Data TX(  63) / 'two-theta angles from the data will be the'/
     Data TX(  64) / 'same.'/
     Data TX(  65) / ' ' /
     Data TX(  66) / ' ' /
     Data TX(  67) / 'The following "buttons" are available:'/
     Data TX(  68) / ' ' /
     Data TX(  69) / '"ANGULAR SECTIONS": This the number of angular'/
     Data TX(  70) / 'sections around 360 degrees of the powder rings'/
     Data TX(  71) / 'which are used to calculate the centre of the'/
     Data TX(  72) / 'rings. The beam centre and tilt are fitted to'/
     Data TX(  73) / 'these positions on each ring. If the data is'/
     Data TX(  74) / 'very noisy or shows poor powder avaraging then'/
     Data TX(  75) / 'a smaller number may be better. (No theoretical'/
     Data TX(  76) / 'criterion exists to set an optimum value, so'/
     Data TX(  77) / 'trail and error is recommended. Clearly, this'/
     Data TX(  78) / 'value should not be too small.)'/
     Data TX(  79) / ' ' /
     Data TX(  80) / '"REJECT OUTLIERS": This option allows badly'/
     Data TX(  81) / 'fitting "ring" positions to be rejected, and'/
     Data TX(  82) / 'the data re-fitted. This is to make the fit'/
     Data TX(  83) / 'procedure more robust and to allow for'/
     Data TX(  84) / 'erroneous positions owing to comtaminating'/
     Data TX(  85) / 'Bragg peaks, etc.'/
     Data TX(  86) / ' ' /
     Data TX(  87) / '"REJECT LIMIT": If "REJECT OUTLIERS" is "YES"'/
     Data TX(  88) / 'then this is the number of standard deviations'/
     Data TX(  89) / 'away from the best fit precition position after'/
     Data TX(  90) / 'which "outliers" are rejected.'/
     Data TX(  91) / ' ' /
     Data TX(  92) / '"FULL INFO": "YES" for terminal screen output of'/
     Data TX(  93) / 'information relating every stage of the fitting'/
     Data TX(  94) / 'of the powder rings.'/
     Data TX(  95) / ' ' /
     Data TX(  96) / '"REFINE BEAM": "YES" to fit the beam centre'/
     Data TX(  97) / 'position, as well as the tilt angles. "NO" to'/
     Data TX(  98) / 'refine only the tilt angles (if variable). This'/
     Data TX(  99) / 'is usually used when the beam centre has been'/
     Data TX( 100) / 'determined from a direct beam mark.'/
     Data TX( 101) / ' ' /
     Data TX( 102) / '"REFINE TILT": "YES" to fit the detector non-'/
     Data TX( 103) / 'orthogonality to the beam (the tilt). "NO" will'/
     Data TX( 104) / 'keep the tilt angles fixed and only fit the beam'/
     Data TX( 105) / 'centre (if variable). "NO" is often used when the'/
     Data TX( 106) / 'tilt angles have been determined accurately from'/
     Data TX( 107) / 'a high quality calibrant measurement.'/
     Data TX( 108) / ' ' /
     Data TX( 109) / '---------------'/
     Data TX( 110) / 'END OF HELP TEXT'/
     Data TX( 111) / '---------------'/
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_TILTCENTRE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_TILTCENTRE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        If (.Not. gui) Then
 
!        Output usage warning to users
           Call IO_WRITE (' ', status)
           Call IO_WRITE ('WARNING: This is a development ' // &
             'routine, user prompts are likely to change', status)
           Call IO_WRITE (' ', status)
        End If
 
        rotation1 = experiment%tilt_plane_rotation
        tilt1 = experiment%tilt_angle
 
 
        If (gui) Then
 
!        Allow experimental geometry to be changed
           Call F2D_GUI_EXPERIMENT (.False., experiment, status)
 
!        Check status
           If (status .Ne. St_goodvalue) Then
 
              If (status .Eq. St_escapevalue) Then
                 status = St_goodvalue
 
!              Redraw image
                 Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, &
                   X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, &
                   xlabel, ylabel, zlabel, status)
 
              End If
 
              Return
 
           End If
 
!        Try to input default parameters from the data-base
           Call IO_INQ_IKEYVALUE ('CALI_NUMBER_SECTIONS', num_sections, &
             retstat, status)
           Call IO_INQ_LKEYVALUE ('CALI_REJECT_OUTLIERS', reject_outliers, &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('CALI_REJECT_LIMIT', reject_value, retstat, &
             status)
           Call IO_INQ_LKEYVALUE ('CALI_FULL_INFO', full_info, retstat, &
             status)
           Call IO_INQ_LKEYVALUE ('CALI_REFINE_BEAM', refine_beam_centre, &
             retstat, status)
           Call IO_INQ_LKEYVALUE ('CALI_REFINE_TILT', refine_tilt, retstat, &
             status)

!        Allow user of choose number of output bins and control various other
!        options
           num_choices = 6
           PROMPT(1) = 'TILT / BEAM CENTRE REFINEMENT'
           PROMPT(2) = '(FITTING TO POWDER RINGS)'
 
           BUTTONS(1) = 'ANGULAR SECTIONS'
           BUTTONS(2) = 'REJECT OUTLIERS'
           BUTTONS(3) = 'REJECT LIMIT'
           BUTTONS(4) = 'FULL INFO'
           BUTTONS(5) = 'REFINE BEAM'
           BUTTONS(6) = 'REFINE TILT'
 
           TYPES(1) = Gs_integer
           TYPES(2) = Gs_logical
           TYPES(3) = Gs_real
           TYPES(4) = Gs_logical
           TYPES(5) = Gs_logical
           TYPES(6) = Gs_logical
 
           TEXT(1) = 'NUMBER OF AZIMUTHAL SECTIONS'
           TEXT(2) = 'REJECT OUT-LYING POSITIONS AND RE-REFINE'
           TEXT(3) = 'REJECT LIMIT FROM IDEAL (STANDARD DEVIATIONS)'
           TEXT(4) = 'OUTPUT FULL INFORMATION'
           TEXT(5) = 'FIND BEST FIT BEAM CENTRE'
           TEXT(6) = 'FIND BEST DETECTOR TILT ANGLES'
           FULL_PROMPTS(1) = 'Enter number of azimuthal ' // &
             'divisions to find average ring positions'
           FULL_PROMPTS(2) = 'Enter "YES" to reject badly fitting' // &
             'positions and re-fit without them'
           FULL_PROMPTS(3) = 'Enter number of standard deviations' // &
             ' from ideal before rejection'
           FULL_PROMPTS(4) = 'Enter "YES" to output full ' // &
             'diagnostics information in terminal window'
           FULL_PROMPTS(5) = 'Enter "YES" to refine beam centre position'
           FULL_PROMPTS(6) = 'Enter "YES" to refine angle ' // &
             'of detector tilt'
 
           BOUND(1) = .True.
           BOUND(2) = .True.
           BOUND(3) = .True.
           BOUND(4) = .True.
           BOUND(5) = .True.
           BOUND(6) = .True.
 
           INTS_LOWER(1) = 10
           INTS_UPPER(1) = 360
           REALS_LOWER(3) = 2.0
           REALS_UPPER(3) = 100.0
 
           INTEGERS(1) = num_sections
           LOGICALS(2) = reject_outliers
           REALS(3) = reject_value
           LOGICALS(4) = full_info
           LOGICALS(5) = refine_beam_centre
           LOGICALS(6) = refine_tilt
 
!        Output interactive graphical form
           Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, Max_choices, &
             num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, &
             INTS_LOWER, INTS_UPPER, REALS_LOWER, REALS_UPPER, INTEGERS, &
             LOGICALS, REALS, STRINGS, retstat, status)
 
!        Check if "CANCEL" entered
           If (retstat .Eq. -1) Then
 
!           Redraw image
              Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, X_AXIS, &
                Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                ylabel, zlabel, status)
              Return
           End If
 
!        Set resulting values
           num_sections =  INTEGERS(1)
           reject_outliers = LOGICALS(2)
           reject_value = REALS(3)
           full_info = LOGICALS(4)
           refine_beam_centre = LOGICALS(5)
           refine_tilt = LOGICALS(6)

!        Save parameter values in the data-base
           Call IO_SET_IKEYVALUE ('CALI_NUMBER_SECTIONS', num_sections, &
             retstat, status)
           Call IO_SET_LKEYVALUE ('CALI_REJECT_OUTLIERS', reject_outliers, &
             retstat, status)
           Call IO_SET_RKEYVALUE ('CALI_REJECT_LIMIT', reject_value, retstat, &
             status)
           Call IO_SET_LKEYVALUE ('CALI_FULL_INFO', full_info, retstat, status)
           Call IO_SET_LKEYVALUE ('CALI_REFINE_BEAM', refine_beam_centre, &
             retstat, status)
           Call IO_SET_LKEYVALUE ('CALI_REFINE_TILT', refine_tilt, retstat, &
             status)
 
        Else
 
!        Input sample to detector distance
           Call F2D_INP_SAMPLEDISTANCE (.False., experiment%detector_distance, &
             status)
           experiment%detector_distance_set = .True.

!        Input X/Y pixel sizes
           Call F2D_INP_PIXELSIZES (.False., &
             experiment%x_pixel_size, experiment%y_pixel_size, status)
           experiment%pixel_sizes_set = .True.
 
!        Number of sections into which to divide powder ring
           MESSAGE(1) = 'The powder ring is divided into a ' // &
             'number of equal angle sections for'
           MESSAGE(2) = 'calculating the radial centre of each ' // &
             'section. From the radial centre'
           MESSAGE(3) = 'and the average angle two cartesian ' // &
             'coordinates are calculated. These'
           MESSAGE(4) = 'coordinates are used to fit optimum ' // &
             'beam centre and detector plane tilt'
           MESSAGE(5) = 'angles. Enter required number of ' // &
             'sections for calculations.'
           Call IO_INPI (.True., 10, 360, .True., &
             'NUMBER OF ANGULAR SECTIONS', 5, MESSAGE, 1, &
             'Enter an integer within the given range', num_sections, status)
 
!        Weighted fitting or not
           MESSAGE(1) = 'The fitting of tilt angle and beam ' // &
             'centre to the estimated ring centres'
           MESSAGE(2) = 'may be performed using weighted fitting ' // &
             'or unweighted fitting. If'
           MESSAGE(3) = 'weighted fitting is chosen then the ' // &
             'average intensity (3 pixels) around'
           MESSAGE(4) = 'the calculated centre of each radial ' // &
             'profile is used to weight the fit.'
           MESSAGE(5) = 'This means that strong rings and strong ' // &
             'angular regions of rings will'
           MESSAGE(6) = 'have more influence than weaker ones. ' // &
             'This should make the fitting more'
           MESSAGE(7) = 'robust when the data has weak rings and ' // &
             'noisy background.'
!        Call IO_INPL (.True., 0, 1, .True.,
!        :       'WEIGHTED FITTING', 7, MESSAGE,
!        :       1, 'Enter "YES" or "NO"',
!        :       weighted_fitting, status)
 
!        Input detector gain for weighted fitting
           If (weighted_fitting) Then
              MESSAGE(1) = 'As you are using weighted fitting ' // &
                'the weight applied to each estimated'
              MESSAGE(2) = 'ring coordinate will depend on the ' // &
                'square root of the estimated number'
              MESSAGE(3) = 'of counts for that coordinate. The ' // &
                'number of counts is calculated from'
              MESSAGE(4) = 'the averaged pixel intensity divided ' // &
                'by the detector gain. Normally'
              MESSAGE(5) = 'the detector gain should be set to ' // &
                'the best estimate of the gain, but'
              MESSAGE(6) = 'it may be useful to vary it ' // &
                'artifically to change the weighting between'
              MESSAGE(7) = 'strong and weak rings. If the gain ' // &
                'is set artifically high, then the'
              MESSAGE(8) = 'fitting will be weighted more ' // &
                'towards the weak rings.'
              Call IO_INPR (.True., 0.01, 100.0, .True., 'DETECTOR GAIN', 8, &
                MESSAGE, 1, 'Enter a real value within the given range', &
                experiment%detector_gain, status)
 
           End If
 
!        Reject outliers or not
           MESSAGE(1) = 'Outlying coordinate positions which are ' // &
             'more than an input number of'
           MESSAGE(2) = 'standard deviations radially from the ' // &
             'fitted ring positions may be'
           MESSAGE(3) = 'rejected from the coordinate lists. The ' // &
             'beam centre/tilt can then be'
           MESSAGE(4) = 're-fitted without these coordinates. If ' // &
             'erroneous coordinate positions'
           MESSAGE(5) = 'are infuencing the fit, this option may ' // &
             'allow them to be removed.'
           Call IO_INPL (.True., 0, 1, .True., 'REJECT OUTLYING COORDINATES', &
             7, MESSAGE, 1, 'Enter "YES" or "NO"', reject_outliers, status)
 
           If (reject_outliers) Then
              MESSAGE(1) = 'Enter the limit of number of ' // &
                'standard deviations after which coordinate'
              MESSAGE(2) = 'positions are to be rejected from ' // &
                'the coordinate lists. A three sigma'
              MESSAGE(3) = 'limit should be reasonable for ' // &
                'less than about 200 coordinates (assuming'
              MESSAGE(4) = 'they are normally distributed.)'
              Call IO_INPR (.True., 1.0, 10.0, .True., &
                'REJECT LIMIT (NUMBER OF STANDARD DEVIATIONS)', 4, MESSAGE, 1, &
                'Enter a real value within the given range', reject_value, &
                status)
 
           End If
 
!        Full information on fit procedure
           Call IO_INPL (.True., 0, 1, .True., 'FULL ALGORITHM INFORMATION', &
             1, 'YES: if you want step by step diagnostics information', 1, &
             'Enter "YES" or "NO"', full_info, status)
 
!        User information
           Call IO_WRITE ('INFO: Control passed to graphics window', status)
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input initial estimate of beam centre and one powder ring
        Call F2D_INP_ELLIPSE (full_info, xmaxdat, ymaxdat, xstrelm, ystrelm, &
          xendelm, yendelm, X_AXIS, Y_AXIS, DATA, MASK, title, xlabel, ylabel, &
          zlabel, experiment, retstat, &
          radius1, radius2, angle1, half_search_distance, status)
 
!     Check return status is good
        If (retstat .Ne. 0) Then
           Return
        End If
 
!     Check status
        If (status .Eq. St_escapevalue) Then
           status = St_goodvalue
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Enter radia of other powder rings to use in the tilt refinement
        Call F2D_INPRADIA (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, MASK, X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
          full_info, experiment%x_pixel_size, experiment%y_pixel_size, &
          experiment%x_beam, experiment%y_beam, &
          radius1, radius2, angle1, half_search_distance, Max_angles, &
          num_rings, RADIA, status)
 
!     Check status
        If (status .Eq. St_escapevalue) Then
           status = St_goodvalue
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If
 
        If (.Not. gui) Then
 
!        Output information message
           Call GS_FPROMPT (1, 1, 'CONTROL RETURNED TO TERMINAL WINDOW', status)
           Call GS_UPDATE (status)
 
!        Refine beam centre or not
           MESSAGE(1) = 'The entered beam centre position may ' // &
             'be kept fixed or may be refined'
           MESSAGE(2) = 'along with the tilt and powder ring ' // &
             'opening angles. If the beam centre'
           MESSAGE(3) = 'is well known e.g. by using a ' // &
             'semi-transparant beam-stop then it is'
           MESSAGE(4) = 'probably better not to refine the position.'
           MESSAGE(5) = '   Enter "YES" for the beam centre ' // &
             'position to be refined, and "NO" if'
           MESSAGE(6) = 'the present values are to be kept fixed.'
           Call IO_INPL (.True., 0, 1, .True., 'REFINE BEAM CENTRE', 6, &
             MESSAGE, 1, 'Enter "YES" or "NO"', refine_beam_centre, status)
 
!        Refine detector tilt
           MESSAGE(1) = 'The non-orthogonality of the detector ' // &
             'plane to the beam may be'
           MESSAGE(2) = 'refined using the shape of the powder ' // &
             'rings. If you want to'
           MESSAGE(3) = 'refine this non-orthogonality or ' // &
             '"detector tilt" angles, enter "YES".'
           MESSAGE(4) = 'Entering "NO" uses the existing tilt ' // &
             'angles as constant in any'
           MESSAGE(5) = 'refinement of the beam centre.'
           Call IO_INPL (.True., 0, 1, .True., 'REFINE DETECTOR TILT', 5, &
             MESSAGE, 1, 'Enter "YES" or "NO"', refine_tilt, status)
 
           MESSAGE(1) = 'Enter "YES" to refine the sample to ' // &
             'detector distance. Normally it is'
           MESSAGE(2) = 'probably best initially to keep this ' // &
             'fixed, so answer "NO". Having'
           MESSAGE(3) = 'refined all other parameters, on a ' // &
             'second iteration the distance can'
           MESSAGE(4) = 'also be simultaneously refined.'
           Call IO_INPL (.True., 0, 1, .True., 'REFINE SAMPLE DISTANCE', 4, &
             MESSAGE, 1, 'Enter "YES" or "NO"', refine_sample_distance, &
             status)
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Check that something is to be refined
        If ((.Not. refine_beam_centre) .And. (.Not. refine_tilt) .And. &
          (.Not. refine_sample_distance)) Then
 
!        Nothing to refine !!!
           Call IO_WRITE ('WARNING: There is nothing left to refine !!!', &
             status)
 
           If (gui) Then
              Call GS_FWARNING ( 1, 1, 'NOTHING LEFT TO REFINE !!!', status)
           End If
 
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     If the beam-stop position is to be refined use initial centred powder 
!     ring to refine it
        If (refine_beam_centre) Then
 
!        Use fitted ellipse parameters to calculate centre of gravity
!        in segments around the initially input powder ring
           Call F2D_FINDRINGS (full_info, xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, DATA, MASK, experiment, &
             F2d_max_rings, 1, RADIA, &
             half_search_distance, num_sections, .False., &
             F2d_max_rcoordinates, F2D_NUM_RCOORDINATES, F2D_X_RCOORDINATES, &
             F2D_Y_RCOORDINATES, F2D_RINTENSITIES, F2D_AZIMUTHS, status)
 
!        Output number of accepted coordinates
           If (full_info) Then
              Write (MESSAGE(1), '(''INFO: Number of ' // &
                'acceptable coordinates on first ring = '', i3)') &
                F2D_NUM_RCOORDINATES(1)
              Call IO_WRITE (MESSAGE(1), status)
           End If
 
!        Display centre of gravity coordinates on top of graphics
           Call F2D_TILTDISPLAY ( &
             experiment%x_pixel_size, experiment%y_pixel_size, &
             F2d_max_rcoordinates, F2d_max_rings, 1, F2D_NUM_RCOORDINATES, &
             F2D_X_RCOORDINATES, F2D_Y_RCOORDINATES, status)
 
!        Fit ellipse to first input ring to obtain initial parameters
!        for tilt and beam centre
           If (full_info) Then
              Call IO_WRITE ('INFO: Fitting ellipse to ' // &
                'centre of gravity coordinates', status)
           End If
           Call F2D_LSQELLIPSE (F2d_max_rcoordinates, F2D_NUM_RCOORDINATES(1), &
             F2D_X_RCOORDINATES, F2D_Y_RCOORDINATES, x_beam, y_beam, &
             radius1, radius2, angle1, radial_error1, status)
 
!        Output information to user
           If (full_info) Then
              Call F2D_TILTINFO ( &
                experiment%x_pixel_size, experiment%y_pixel_size, &
                F2D_NUM_RCOORDINATES(1), x_beam, y_beam, radius1, &
                radius2, angle1, radial_error1, status)
           End If
 
           experiment%x_beam = x_beam / experiment%x_pixel_size
           experiment%y_beam = y_beam / experiment%y_pixel_size

        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Use fitted ellipse parameters to calculate centre of gravity
!     in segments around the required powder rings
        Call F2D_FINDRINGS (full_info, xmaxdat, ymaxdat, xstrelm, ystrelm, &
          xendelm, yendelm, DATA, MASK, experiment, &
          F2d_max_rings, num_rings, RADIA, half_search_distance, &
          num_sections, .False., F2d_max_rcoordinates, F2D_NUM_RCOORDINATES, &
          F2D_X_RCOORDINATES, F2D_Y_RCOORDINATES, F2D_RINTENSITIES, &
          F2D_AZIMUTHS, status)
 
!     Display centre of gravity coordinates on top of graphics
        Call F2D_TILTDISPLAY ( &
          experiment%x_pixel_size, experiment%y_pixel_size, &
          F2d_max_rcoordinates, F2d_max_rings, num_rings, &
          F2D_NUM_RCOORDINATES, F2D_X_RCOORDINATES, F2D_Y_RCOORDINATES, &
          status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Set "reasonable" initial estimates of powder diffraction ring
!     parameters based on fitted ellipse
        Do ring = 1, num_rings
           ANGLE_CONES1(ring) = Atan2(RADIA(ring), experiment%detector_distance)
        End Do
 
        If (refine_tilt) Then
           rotation1 = angle1
           tilt1 = Acos(radius2 / radius1) ! Approximate +- tilt
 
!        +- 10 degrees maximum initial tilt angle
           If (tilt1 .Gt. Pi / 18.0) Then
              tilt1 = Pi / 18.0
           Else If (tilt1 .Lt. -Pi / 18.0) Then
              tilt1 = -Pi / 18.0
           End If
 
        Else
           rotation1 = experiment%tilt_plane_rotation
           tilt1 = experiment%tilt_angle
        End If

 
        If (refine_beam_centre) Then
 
           x_beam1 = experiment%x_beam * experiment%x_pixel_size &
             - 2.0 * (radius1 - radius2) * Cos(rotation1)
           y_beam1 = experiment%y_beam * experiment%y_pixel_size &
             - 2.0 * (radius1 - radius2) * Sin(rotation1)
 
        Else
           x_beam1 = experiment%x_beam * experiment%x_pixel_size
           y_beam1 = experiment%y_beam * experiment%x_pixel_size
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Initial tilt angle = '', f12.3)')
!     :       tilt1 * 180.0 / Pi
!     Write (*, '(''Initial beam centre = '', 2f12.3)')
!     :       x_beam1 / x_pixel_size, y_beam1 / y_pixel_size
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        If (full_info) Then
           Call IO_WRITE (' ', status)
           Call IO_WRITE ('INFO: Fitting powder ' // &
             'rings to centre of gravity coordinates', status)
        End If
 
!     Least squares fit of tilt/beam centre to powder diffraction
!     rings (coordinates passed through "f2d_fitrings.inc")
        Call F2D_FITPOWDER (full_info, Max_angles, num_rings, &
          weighted_fitting, experiment%detector_gain, reject_outliers, &
          reject_value, &
          refine_beam_centre, refine_sample_distance, refine_tilt, &
          experiment%detector_distance, x_beam1, y_beam1, ANGLE_CONES1, &
          rotation1, tilt1, radial_error1, status)
 
!     Output information to user
        If (full_info) Then
           Call IO_WRITE ('INFO: Fit of powder ring to inclined detector', &
             status)
           Call F2D_TILT2INFO ( &
             experiment%x_pixel_size, experiment%y_pixel_size, Max_angles, &
             num_rings, x_beam1, y_beam1, ANGLE_CONES1, rotation1, tilt1, &
             radial_error1, status)
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Alternative solution is only necessary if the beam centre is not known
        If (refine_beam_centre .And. refine_tilt) Then
 
!        Set alternative "reasonable" initial estimates of powder
!        diffraction ring parameters based on fitted ellipse
           Do ring = 1, num_rings
              ANGLE_CONES2(ring) = Atan2(RADIA(ring), &
                experiment%detector_distance)
           End Do
           rotation2 = angle1
           tilt2 = -Acos(radius2 / radius1) ! Approximate +- tilt
 
!        +- 10 degrees maximum initial tilt angle
           If (tilt2 .Gt. Pi / 18.0) Then
              tilt2 = Pi / 18.0
           Else If (tilt2 .Lt. -Pi / 18.0) Then
              tilt2 = -Pi / 18.0
           End If
 
           If (refine_beam_centre) Then
              x_beam2 = experiment%x_beam * experiment%x_pixel_size &
                - 2.0 * (radius1 - radius2) * Cos(rotation2)
              y_beam2 = experiment%y_beam * experiment%y_pixel_size &
                - 2.0 * (radius1 - radius2) * Sin(rotation2)
 
           Else
              x_beam2 = experiment%x_beam * experiment%x_pixel_size
              y_beam2 = experiment%y_beam * experiment%y_pixel_size
           End If
 
           If (full_info) Then
              Call IO_WRITE (' ', status)
              Call IO_WRITE ('INFO: Alternative fit of ' // &
                'powder rings to centre of gravity coordinates', status)
           End If
 
!        Least squares fit of tilt/beam centre to powder diffraction rings
           Call F2D_FITPOWDER (full_info, Max_angles, num_rings, &
             weighted_fitting, experiment%detector_gain, reject_outliers, &
             reject_value, &
             refine_beam_centre, refine_sample_distance, refine_tilt, &
             experiment%detector_distance, x_beam2, y_beam2, ANGLE_CONES2, &
             rotation2, tilt2, radial_error2, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''radial_error2 = '', f12.5)') radial_error2
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Output information to user
           If (full_info) Then
              Call IO_WRITE ('INFO: Fit of powder ring to inclined detector', &
                status)
              Call F2D_TILT2INFO ( &
                experiment%x_pixel_size, experiment%y_pixel_size, max_angles, &
                num_rings, x_beam2, y_beam2, ANGLE_CONES2, rotation2, tilt2, &
                radial_error2, status)
           End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!        Output two possible solutions to the user
           Call IO_WRITE (' ', status)
           Call IO_WRITE ('INFO: In the absence of any ' // &
             'other information, there are two equally', status)
           Call IO_WRITE ('      valid solutions to the ' // &
             'tilt angle and beam centre which could', status)
           Call IO_WRITE ('      have formed a powder ring ' // &
             'on an inclined detector. Theoretically', status)
           Call IO_WRITE ('      these should both give the ' // &
             'same goodness of fit, so both solutions', status)
           Call IO_WRITE ('      are output. If the ' // &
             'position of the beam-stop is known, then the', status)
           Call IO_WRITE ('      "correct" solution may be selected.', status)
 
           Call IO_WRITE (' ', status)
           Call IO_WRITE ('INFO: SOLUTION 1', status)
        Else
           Call IO_WRITE (' ', status)
           Call IO_WRITE ('INFO: SOLUTION', status)
        End If
 
        Call F2D_TILT2INFO ( &
          experiment%x_pixel_size, experiment%y_pixel_size, max_angles, &
          num_rings, x_beam1, y_beam1, ANGLE_CONES1, rotation1, tilt1, &
          radial_error1, status)
 
 
        If (refine_beam_centre .And. refine_tilt) Then
           Call IO_WRITE (' ', status)
           Call IO_WRITE ('INFO: SOLUTION 2', status)
           Call F2D_TILT2INFO ( &
             experiment%x_pixel_size, experiment%y_pixel_size, max_angles, &
             num_rings, x_beam2, y_beam2, ANGLE_CONES2, rotation2, tilt2, &
             radial_error2, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!        Which solution to keep
           If (radial_error1 .Le. radial_error2) Then
              solution = 1
           Else
              solution = 2
           End If
 
           If (gui) Then
              Call GS_INPI (.True., 1, 2, .True., &
                'WHICH SOLUTION (SEE TERMINAL WINDOW)', 1, &
                'Enter "1" or "2" to choose the solution to save', 1, &
                'You can only input "1" or "2"', solution, status)
           Else
              Call IO_INPI (.True., 1, 2, .True., 'WHICH SOLUTION', 1, &
                'Enter "1" or "2" to chose the solution to save', 1, &
                'You can only input "1" or "2"', solution, status)
           End If
 
        Else
           solution = 1
        End If
 
        If (solution .Eq. 1) Then
           experiment%x_beam = x_beam1 / experiment%x_pixel_size
           experiment%y_beam = y_beam1 / experiment%y_pixel_size
           experiment%tilt_plane_rotation = rotation1
           experiment%tilt_angle = tilt1
        Else
           experiment%x_beam = x_beam2 / experiment%x_pixel_size
           experiment%y_beam = y_beam2 / experiment%y_pixel_size
           experiment%tilt_plane_rotation = rotation2
           experiment%tilt_angle = tilt2
        End If
 
     End If
 
     End Subroutine F2D_TILTCENTRE
!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_tiltinfo.f90 *
!  *                  *
!  ********************
 
!+ F2D_TILTINFO -  TILT INFOrmation output (utility routine)
     Subroutine F2D_TILTINFO (x_pixel_size, y_pixel_size, num_coordinates, &
       x_ellipse, y_ellipse, radius1, radius2, angle1, radial_error1, status)
!  Description:
!    Outputs useful value information to user
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    17-Oct-1996: V0.3 Output more figures for results of tilt fitting 
!      (Hammersley)
!    21-Jan-1995: V0.2 Add "num_coordinates", and only output error
!      estimate if more than 5 coordinates have been entered (Hammersley)
!    18-Jan-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Real, Intent(IN) :: x_pixel_size ! Size of a corrected pixel in the
!      X-direction (metres)
     Real, Intent(IN) :: y_pixel_size ! Size of a corrected pixel in the
!      Y-direction (metres)
     Integer, Intent(IN) :: num_coordinates ! Number of coordinates which
!      were used to refine the ellipse parameters
     Real, Intent(IN) :: x_ellipse ! X-coordinate of centre of fitted ellipse
!      (metres)
     Real, Intent(IN) :: y_ellipse ! Y-coordinate of centre of fitted ellipse
!      (metres)
     Real, Intent(IN) :: radius1 ! First radius of ellipse (metres)
     Real, Intent(IN) :: radius2 ! Second radius of ellipse (metres)
     Real, Intent(IN) :: angle1 ! Orientation angle of first axis of best
!      fit ellipse
     Real, Intent(IN) :: radial_error1 ! Estimated average error (radially)
!      in coordinate positions (metres)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
!  Local Arrays:
     Character(Len = 80) :: message ! User text
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_TILTINFO ' // Version)
        Return
     End If
 
!  Output information
     Write (message, '(''INFO: Number of coordinates = '', i3)') &
       num_coordinates
     Call IO_WRITE (message, status)
     Write (message, '(''INFO: Best fit ' // &
       'ellipse centre (X/Y mm) = '', 2g14.7)') x_ellipse * 1000.0, y_ellipse &
       * 1000.0
     Call IO_WRITE (message, status)
     Write (message, '(''INFO: Best fit ' // &
       'ellipse centre (X/Y pixels) = '', 2g14.7)') x_ellipse / x_pixel_size, &
       y_ellipse / y_pixel_size
     Call IO_WRITE (message, status)
     Write (message, '(''INFO: Best fit ' // &
       'radius 1, radius 2 (mm) = '', 2g14.7)') radius1 * 1000.0, radius2 * &
       1000.0
     Call IO_WRITE (message, status)
     Write (message, '(''INFO: Best fit ' // &
       'radius 1 (X pixels) = '', g14.7)') radius1 / x_pixel_size
     Call IO_WRITE (message, status)
     Write (message, '(''INFO: Best fit ' // &
       'radius 2 (Y pixels) = '', g14.7)') radius2 / y_pixel_size
     Call IO_WRITE (message, status)
     Write (message, '(''INFO: Best fit angle of ' // &
       'axis 1 (degrees) = '', g14.7)') angle1 * 180.0 / Pi
     Call IO_WRITE (message, status)
 
     If (num_coordinates .Gt. 5) Then
        Write (message, '(''INFO: Estimated coordinate ' // &
          'radial position error (mm) = '', g14.7)') radial_error1 * 1000.0
        Call IO_WRITE (message, status)
        Write (message, '(''INFO: Estimated coordinate ' // &
          'radial position error (X pixels) = '', g14.7)') radial_error1 / &
          x_pixel_size
        Call IO_WRITE (message, status)
     Else
        Call IO_WRITE ('INFO: The coordinate radial ' // &
          'position error is totally undetermined', status)
 
     End If
 
     End Subroutine F2D_TILTINFO
!********1*********2*********3*********4*********5*********6*********7*********8
!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_tilt2info.f90 *
!  *                   *
!  *********************
 
!+ F2D_TILT2INFO -  TILT (2) INFOrmation output (utility routine)
     Subroutine F2D_TILT2INFO (x_pixel_size, y_pixel_size, max_rings, &
       num_rings, x_beam, y_beam, ANGLE_CONES, rotation, tilt, radial_error, &
       status)
!  Description:
!    Outputs useful value information to user
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    17-Oct-1996: V0.3 Output more figures for results of tilt fitting 
!      (Hammersley)
!    26-Jan-1995: V0.2 Output angles from multiple cones (Hammersley)
!    18-Jan-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Real, Intent(IN) :: x_pixel_size ! Size of a corrected pixel in the
!      X-direction (metres)
     Real, Intent(IN) :: y_pixel_size ! Size of a corrected pixel in the
!      Y-direction (metres)
     Integer, Intent(IN) :: max_rings ! Dimension of "ANGLE_CONES"
     Integer, Intent(IN) :: num_rings ! Number of rings
     Real, Intent(IN) :: x_beam ! X-coordinate of centre of fitted beam (metres)
     Real, Intent(IN) :: y_beam ! Y-coordinate of centre of fitted beam (metres)
     Real, Intent(IN) :: ANGLE_CONES(max_rings) ! Opening angle (2-theta) of
!      cone radians
     Real, Intent(IN) :: rotation ! Angle of rotation of the tilt plane
!      (about the Z-axis)
     Real, Intent(IN) :: tilt ! Angle of detector tilt in the X-direction
     Real, Intent(IN) :: radial_error ! Estimated average error (radially)
!      in coordinate positions (metres)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer :: ring ! Loop variable for rings
!  Local Arrays:
     Character(Len = 80) :: message ! User text
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_TILT2INFO ' // Version)
        Return
     End If
 
!  Output information
     Write (message, '(''INFO: Best fit ' // &
       'beam centre (X/Y mm) = '', 2g14.7)') x_beam * 1000.0, y_beam * 1000.0
     Call IO_WRITE (message, status)
     Write (message, '(''INFO: Best fit beam centre (X/Y pixels) ' // &
       '= '', 2g14.7)') x_beam / x_pixel_size, y_beam / y_pixel_size
     Call IO_WRITE (message, status)
 
     Do ring = 1, num_rings
        Write (message, '(''INFO: Cone '', i2, '' best fit 2 theta angle ' // &
          '(degrees) = '', g14.7)') ring, ANGLE_CONES(ring) * 180.0 / Pi
        Call IO_WRITE (message, status)
     End Do
 
     Write (message, '(''INFO: Best fit angle of ' // &
       'tilt plane rotation (degrees) = '', g14.7)') rotation * 180.0 / Pi
     Call IO_WRITE (message, status)
     Write (message, '(''INFO: Best fit angle of ' // &
       'tilt (degrees) = '', g14.7)') tilt * 180.0 / Pi
     Call IO_WRITE (message, status)
     Write (message, '(''INFO: Estimated coordinate ' // &
       'radial position error (mm) = '', g14.7)') radial_error * 1000.0
     Call IO_WRITE (message, status)
     Write (message, '(''INFO: Estimated coordinate radial position ' // &
       'error (X pixels) = '', g14.7)') radial_error / x_pixel_size
     Call IO_WRITE (message, status)
 
     End Subroutine F2D_TILT2INFO
!********1*********2*********3*********4*********5*********6*********7*********8
