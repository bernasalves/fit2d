!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_cal_cake.f90 *
!  *                  *
!  ********************
 
!+ F2D_CAL_CAKE -  CALculate "CAKE" region: radial/azimuth re-binning
     Subroutine F2D_CAL_CAKE (title, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MASK, X_AXIS, Y_AXIS, experiment, &
       lorentz_geometry, start_azimuth, &
       end_azimuth, inner_limit, outer_limit, &
       memory_defined, mxnumdat, mynumdat, MXAXIS, MYAXIS, MDATA, mxstrelm, &
       mystrelm, mxendelm, myendelm, az_pixel_size, rad_pixel_size, mtitle, &
       mxlabel, mylabel, retstat, status)
!  Description:
!  Keywords:
!    Radial~Profile.Calculation, Calculate.Radial~Profile,
!    Profile.Radial.Calculation, Powder~Diffraction.Radial~Profile,
!    Diffraction.Powder.Radial~Profile
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    21-Sep-2012: V0.20 Option to correct parallax effect (Hammersley)
!    28-Sep-2011: V0.19 Time re-binning (Hammersley)
!    31-Mar-2006: V0.18 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    10-Mar-2006: V0.17 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    24-Mar-2004: V0.16 Add option to default to ~1 degree azimuthal bins
!      (Hammersley)
!    05-Mar-2004: V0.15 Trap "CANCEL" from geometry form (Hammersley)
!    22-Jul-2003: V0.14 Avoid user set azimuth values changing (Hammersley)
!    21-Jul-2003: V0.13 Checking problem with negative azimuth input.
!      Convert azimuth angles to -Pi to Pi range (Hammersley)
!    27-May-2003: V0.12 Use fixed prompt calls (Hammersley)
!    23-Feb-1999: V0.11 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.10 Change to use IO internal database routines (Hammersley)
!    24-Jul-1998: V0.9 Add "X_AXIS" and "Y_AXIS" arguments and convert beam 
!      centre to pixel coordinates (Hammersley)
!    30-Jan-1998: V0.8 Declare unused character string for call to "GS_FORM" 
!      (Hammersley)
!    25-Feb-1998: V0.7 Use Experimental geometry form and option to set maximum
!      D-spacing (Hammersley)
!    20-Feb-1998: V0.6 Swap X/Y output re-binning sense, so that the 2-theta 
!      scan direction is X-direction and the azimuthal scan direction is Y 
!      (Hammersley)
!    28-Jan-1998: V0.5 Add option of Q-space re-binning; change definition of 
!      "angular_scan" argument (Hammersley)
!    29-Mar-1996: V0.4 Add return status variable (Hammersley)
!    08-Mar-1996: V0.3 Check for "CANCEL" from form (Hammersley)
!    14-Feb-1996: V0.2 Add option to change start/stop azimuths and
!      inner and outer radial limits (Hammersley)
!    03-Feb-1996: V0.1 Original, based on "F2D_RTHETA" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Real, Intent(INOUT) :: start_azimuth ! Angle of azimuth of start of
!      region in radians
     Real, Intent(INOUT) :: end_azimuth ! Angle of azimuth of end of region
!      in radians
     Real, Intent(INOUT) :: inner_limit ! Inner radius in metres
     Real, Intent(INOUT) :: outer_limit ! Outer radius in metres
     Integer, Intent(INOUT) :: lorentz_geometry ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the equivalent of a 
!            2-theta scan i.e. detector at equal distance
!  Export:
     Logical, Intent(OUT) :: memory_defined ! .True., if the memory has been
!      defined
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of memory data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of meomory data region
     Real, Intent(OUT) :: MXAXIS(xmaxdat) ! Horizontal (angular) axis
!       information
     Real, Intent(OUT) :: MYAXIS(ymaxdat) ! Vertical (radial) axis
!      information
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Re-binned angular/radial
!      pixel data
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
     Real, Intent(OUT) :: az_pixel_size ! Size of pixel to be used for the
!      azimuth bin sizes. The units are radians
     Real, Intent(OUT) :: rad_pixel_size ! Size of pixel to be used for the
!      2-theta, q-spacing, or radial bin sizes. If an equal radial
!      distance scan is to be calculated ("scan_type = 0") the units
!      are metres, and for an equal angle pixel scan ("scan_type = 1")
!      the units are radians
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Integer, Intent(OUT) :: retstat ! Status return variable:
!      0 = Good status, transformation output
!      -1 = Operation cancelled, no transformation output
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.20' ! Version number
     Integer, Parameter :: Max_choices = 19 ! Maximum number of choices
!      (need one extra space)
!  Local Variables:
     Logical, Save :: conserve_intensity = .False. ! .True., if the total
!      intensity in to be conserved in the re-binning operation
     Logical :: continue ! .True., whilst the input is not good
     Logical :: correct_geometry ! .True., if the scan intensities are
!      to be corrected for the 1/Cos**3(angle) drop owing to the
!      geometrical difference between a 2-theta scan and a flat detector
     Logical, Save :: correct_parallax = .False. ! .True., if the effect of 
!      parallax on angular position is to be corrected
     Real :: end_cpu ! CPU time at end of re-binning
     Real :: end_elapse ! Elapse time at end of re-binning
     Logical, Save :: first = .True. ! .True., if first call
     Real :: inner_2theta ! 2-theta angle of inside edge of region
     Real, Save :: maximum_d = 20.0 ! Maximum of range for D-spacings scans
     Real :: mod_end_azimuth ! Modified end azimuth angle, 2*Pi is
!      added if the angle is less than the "start_azimuth"
     Real :: mod_start_azimuth ! Modified start azimuth angle in range -Pi to Pi
     Integer, Save :: num_2theta ! Number of 2-theta angle or radial bins
!      in rebinned array
     Integer, Save :: num_azimuthal ! Number of angular bins in rebinned array
     Integer :: num_choices ! Number of choices
     Logical, Save :: one_degree = .False. ! .True., if default azimuth bin
!      number should correspond to approximately 1 degree bins
     Integer :: pixel ! Loop variable
     Integer, Save :: scan_type = 1 ! Type of 1-D scan to be produced:
!      0 = Radial: equal distance scan
!      1 = "2-theta": equal angle element scan
!      2 = Q-space: Equal Q element scan
!          (Q = (4 * Pi / wavelength) Sin(theta / 2) )
!      3 = D-spacings
     Real :: start_cpu ! CPU time at start of re-binning
     Real :: start_elapse ! Elapse time at start of re-binning
     Integer stat ! Status return variable for "Allocate"
     Real :: x_beampc ! Beam centre X-pixel coordinates
     Real :: y_beampc ! Beam centre in Y-pixel coordinates
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 70) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 70) :: HELP(57) ! User help text on the form
     Character(Len = 80) :: MESSAGE(2) ! User messages
     Character(Len = 70) :: PROMPT(3) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
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
     Real :: REALS_LOWER(Max_choices) ! Lower bounds on reals
     Real :: REALS_UPPER(Max_choices) ! Upper bounds on reals
     Real :: REALS(Max_choices) ! Reals to be changed
     Real, Allocatable :: WORK(:, :) ! Dynamic work array for "F2D_CAL2_CAKE"
!      used to store fractions of pixels  contributing to each angular/radial 
!      pixel
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CAL_CAKE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_CAL_CAKE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Find any previously set user preferences
        Call IO_INQ_IKEYVALUE ('SCAN_TYPE', scan_type, retstat, status)
        Call IO_INQ_RKEYVALUE ('MAXIMUM_D', maximum_d, retstat, status)
        Call IO_INQ_LKEYVALUE ('CAKE_DEFAULT_1_DEGREE', one_degree, retstat, &
          status)
 
!     Convert start azimuth to -Pi to Pi range
        If (start_azimuth .Lt. -Pi) Then
           mod_start_azimuth = start_azimuth + 2.0 * Pi
        Else If (start_azimuth .Gt. Pi) Then
           mod_start_azimuth = start_azimuth - 2.0 * Pi
        Else
           mod_start_azimuth = start_azimuth
        End If
 
!     Make sure that the end azimuth is greater than the start azimuth
        If (end_azimuth .Lt. mod_start_azimuth) Then
           mod_end_azimuth = end_azimuth + 2.0 * Pi
        Else
           mod_end_azimuth = end_azimuth
        End If
 
!     Set appropriate default values
        If (first .Or. one_degree) Then
 
!        Set to 1 azimuthal bin per degree
           If (mod_end_azimuth .Gt. mod_start_azimuth) Then
              num_azimuthal = Max(1, Nint(((mod_end_azimuth - &
                mod_start_azimuth) * 180.0 / Pi)))
           Else
              num_azimuthal = Max(1, Nint(((2.0 * Pi + mod_end_azimuth - &
                mod_start_azimuth) * 180.0 / Pi)))
           End If
 
        End If
 
        If (first) Then
 
           first = .False.
 
!        Set to 1 2-theta bin per pixel (approximately)
           num_2theta = Max(1, Nint((outer_limit - inner_limit) / &
             Min(experiment%x_pixel_size, experiment%y_pixel_size)))
 
        End If
 
!     Allow experimental geometry to be changed
        Call F2D_GUI_EXPERIMENT (.False., experiment, status)
 
!     Check status
        If (status .Ne. St_goodvalue) Then
 
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
           End If
 
           retstat = -1
           Return
 
        End If
 
!     Set geometrical correction variable depending on value of
!     "lorentz_geometry"
        If (lorentz_geometry .Eq. 0) Then
           correct_geometry = .False.
        Else If (lorentz_geometry .Eq. 1) Then
           correct_geometry = .True.
        Else
           Call IO_WRITE ('PANIC: The type of ' // &
             'Lorentz/geometrical correction is unknown', status)
        End If
 
!     Allow user of choose number of output bins and control various other 
!     options
        num_choices = 14
        PROMPT(1) = 'TYPE OF AZIMUTH/RADIAL OR '
        PROMPT(2) = '2-THETA TRANSFORMATION'
        HELP(1) = 'This form allows you to control various ' // &
          'options in the re-binning'
        HELP(2) = 'from the 2-D detector data to an azimuthal ' // &
          'versus 2-theta or '
        HELP(3) = 'radial distance coordinate system.'
        HELP(4) = ' '
        HELP(5) = 'The region to be integrated is defined by ' // &
          'it''s limits.'
        HELP(6) = 'The number of output bins in the azimuthal ' // &
          'and radial/2-theta direction are defined.'
        HELP(7) = 'The region is defined in the anti-clockwise sense'
        HELP(8) = ' '
        HELP(9) = '"START AZIMUTH" defines the start of the ' // &
          'region to be integrated.'
        HELP(10) = 'The default is the value that was previously ' // &
          'defined graphically.'
        HELP(11) = ' '
        HELP(12) = '"END AZIMUTH" defines the end of the ' // &
          'region to be integrated.'
        HELP(13) = 'The default is the value that was previously ' // &
          'defined graphically.'
        HELP(14) = ' '
        HELP(15) = '"INNER RADIUS" defines the distance in ' // &
          'pixels from the beam centre'
        HELP(16) = 'for the start of the region to be ' // &
          'integrated. The default is the'
        HELP(17) = 'value that was previously defined graphically.'
        HELP(18) = ' '
        HELP(19) = '"OUTER RADIUS" defines the distance in ' // &
          'pixels from the beam centre'
        HELP(20) = 'for the end of the region to be ' // &
          'integrated. The default is the'
        HELP(21) = 'value that was previously defined graphically.'
        HELP(22) = ' '
 
        HELP(23) = '"SCAN TYPE" defines the type of integrated ' // &
          'output data:'
        HELP(24) = 'Linear radial distance, 2-theta angle, ' // &
          'Q-space, or D-spacings.'
        HELP(25) = ' '
        HELP(26) = '"1 DEGREE AZ" if set to "YES" will mean that ' // &
          'future integrations will propose'
        HELP(27) = 'a default number of azimuthal bin ' // 'corresponding to'
        HELP(28) = 'approxmately 1 degree size bins.'
        HELP(29) = ' '
        HELP(30) = '"AZIMUTH BINS" defines the number of output ' // &
          'bins around the azimuth.'
        HELP(31) = ' '
        HELP(32) = '"RADIAL BINS" defines the number of output ' // &
          'bins in the radial / '
        HELP(33) = 'angular direction.'
        HELP(34) = ' '
        HELP(35) = '"CONSERVE INT." if set to "YES" will ' // &
          'conserve total intensity'
        HELP(36) = 'in the output scan. For normal 2-theta / ' // &
          'Q-space scan output'
        HELP(37) = 'this should set to "NO".'
        HELP(38) = ' '
        HELP(39) = '"POLARISATION" if set to "YES" will apply a ' // &
          'polarisation effect'
        HELP(40) = 'intensity correction. Note: this is a 2-D ' // &
          'correction which depends'
        HELP(41) = 'both on 2-theta angle and azimuth.'
        HELP(42) = ' '
        HELP(43) = '"FACTOR" sets the polarisation factor to ' // &
          'be used when the'
        HELP(44) = 'polarisation effect is corrected. It is defined as'
        HELP(45) = '(I_h - I_v) / (I_h + I_v), where I_h is the ' // &
          'horizontal component and'
        HELP(46) = ' I_v is the vertical component. (Horizontal ' // &
          'should normally'
        HELP(47) = 'correspond to the X-direction of the image.)'
        HELP(48) = ' '
        HELP(49) = '"MAX. D-SPACING" is the maximum D-spacing ' // &
          'for the output scan.'
        HELP(50) = 'This is necessary as the beam-centre is ' // &
          'theoretically at'
        HELP(51) = 'an infinite D-spacing.'
        HELP(52) = ' '
        HELP(53) = '"GEOMETRY COR." is set to "YES" to apply a ' // &
          'geometrically correction'
        HELP(54) = 'to the output intensities to be equivalent ' // &
          'to a 2-theta scan.'
        HELP(55) = 'This corrects for the change in relative ' // &
          'angular size and'
        HELP(56) = 'obliqueness for off-axis pixels.'
        HELP(57) = ' '
        BUTTONS(1) = 'START AZIMUTH'
        BUTTONS(2) = 'END AZIMUTH'
        BUTTONS(3) = 'INNER RADIUS'
        BUTTONS(4) = 'OUTER RADIUS'
        BUTTONS(5) = 'SCAN TYPE'
        BUTTONS(6) = '1 DEGREE AZ'
        BUTTONS(7) = 'AZIMUTH BINS'
        BUTTONS(8) = 'RADIAL BINS'
        BUTTONS(9) = 'CONSERVE INT.'
        BUTTONS(10) = 'POLARISATION'
        BUTTONS(11) = 'FACTOR'
        BUTTONS(12) = 'MAX. D-SPACING'
        BUTTONS(13) = 'GEOMETRY COR.'
        BUTTONS(14) = 'CORRECT PARALLAX'
        TYPES(1) = Gs_real
        TYPES(2) = Gs_real
        TYPES(3) = Gs_real
        TYPES(4) = Gs_real
        TYPES(5) = Gs_scan_type
        TYPES(6) = Gs_logical
        TYPES(7) = Gs_integer
        TYPES(8) = Gs_integer
        TYPES(9) = Gs_logical
        TYPES(10) = Gs_logical
        TYPES(11) = Gs_real
        TYPES(12) = Gs_real
        TYPES(13) = Gs_logical
        TYPES(14) = Gs_logical
        TEXT(1) = 'STARTING AZIMUTH ANGLE (DEGREES)'
        TEXT(2) = 'END AZIMUTH ANGLE (DEGREES)'
        TEXT(3) = 'INNER RADIAL LIMIT (PIXELS)'
        TEXT(4) = 'OUTER RADIAL LIMIT (PIXELS)'
        TEXT(5) = 'SCAN TYPE (RADIAL, 2-THETA, Q-SPACE)'
        TEXT(6) = 'DEFAULT TO APPROX. 1 DEGREE SIZE AZIMUTHAL BINS'
        TEXT(7) = 'NUMBER OF AZIMUTHAL BINS'
        TEXT(8) = 'NUMBER OF RADIAL/2-THETA BINS'
        TEXT(9) = 'INTENSITY CONSERVATION'
        TEXT(10) = 'APPLY POLARISATION CORRECTION'
        TEXT(11) = 'POLARISATION FACTOR'
        TEXT(12) = 'MAXIMUM FOR D-SPACINGS SCANS (ANGSTROMS)'
        TEXT(13) = 'GEOMETRICAL CORRECTION TO INTENSITIES'
        TEXT(14) = 'CORRECT PARALLAX EFFECT ON ANGULAR POSITION'
        FULL_PROMPTS(1) = 'Enter azimuth angle for start of ' // &
          'region to be integrated (degrees)'
        FULL_PROMPTS(2) = 'Enter azimuth angle for end of ' // &
          'region to be integrated (degrees)'
        FULL_PROMPTS(3) = 'Enter radius for inner radial ' // &
          'limit of region to be integrated (pixels)'
        FULL_PROMPTS(4) = 'Enter radius for outer radial ' // &
          'limit of region to be integrated (pixels)'
        FULL_PROMPTS(5) = 'Select type of output 1-D scan: ' // &
          '"RADIAL", "2-THETA", or "Q-SPACE"'
        FULL_PROMPTS(6) = 'Enter "YES" if the default number of ' // &
          'should correspond to 1 degree azimuth bins'
        FULL_PROMPTS(7) = 'Number of bins azimuthally'
        FULL_PROMPTS(8) = 'Number of bins in ' // &
          'the 2-theta/radial direction'
        FULL_PROMPTS(9) = 'Enter "YES" to conserve total ' // &
          'intensity, "NO" for "2-theta scans"' // '(integers only)'
        FULL_PROMPTS(10) = 'Enter "YES" to apply polarisation ' // &
          'correction'
        FULL_PROMPTS(11) = 'Enter ratio of horizontal to ' // &
          'vertical polarisation'
        FULL_PROMPTS(12) = 'Enter maximum value of range for ' // &
          'calculation of D-spacing scans (Angstroms)'
        FULL_PROMPTS(13) = 'Enter "YES" if scan intensities are ' // &
          'corrected for geometry'
        FULL_PROMPTS(14) = 'Enter "YES" to correct the effect of parallax ' // &
          'on angular position'
        BOUND(1) = .True.
        BOUND(2) = .True.
        BOUND(3) = .True.
        BOUND(4) = .True.
        BOUND(5) = .True.
        BOUND(6) = .True.
        BOUND(7) = .True.
        BOUND(8) = .True.
        BOUND(9) = .True.
        BOUND(10) = .True.
        BOUND(11) = .True.
        BOUND(12) = .True.
        BOUND(13) = .True.
        BOUND(14) = .True.
 
        REALS_LOWER(1) = -360.0
        REALS_UPPER(1) = 360.0
        REALS_LOWER(2) = -360.0
        REALS_UPPER(2) = 360.0
        REALS_LOWER(3) = 0.0
        REALS_UPPER(3) = 10000.0
        REALS_LOWER(4) = 0.0
        REALS_UPPER(4) = 10000.0
        INTS_LOWER(7) = 1
        INTS_UPPER(7) = ymaxdat
        INTS_LOWER(8) = 1
        INTS_UPPER(8) = xmaxdat
        REALS_LOWER(11) = -1.0
        REALS_UPPER(11) = 1.0
        REALS_LOWER(12) = 1.0
        REALS_UPPER(12) = 10000.0
        REALS(1) = start_azimuth * 180.0 / Pi
        REALS(2) = end_azimuth * 180.0 / Pi
        REALS(3) = inner_limit / experiment%x_pixel_size
        REALS(4) = outer_limit / experiment%x_pixel_size
        INTEGERS(5) = scan_type
        LOGICALS(6) = one_degree
        INTEGERS(7) = Min(num_azimuthal, ymaxdat)
        INTEGERS(8) = Min(num_2theta, xmaxdat)
        LOGICALS(9) = conserve_intensity
        LOGICALS(10) = experiment%correct_polarisation
        REALS(11) = experiment%polarisation
        REALS(12) = maximum_d
        LOGICALS(13) = correct_geometry
        LOGICALS(14) = correct_parallax
 
        continue = .True.
        Do While (continue)
 
!        Output interactive graphical form
           Call GS_FORM (2, 2, PROMPT, 57, 57, HELP, Max_choices, num_choices, &
             BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, INTS_LOWER, &
             INTS_UPPER, REALS_LOWER, REALS_UPPER, INTEGERS, LOGICALS, REALS, &
             STRINGS, retstat, status)
 
!        Check for "CANCEL"
           If (retstat .Eq. -1) Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*,
!           :             '(''returning from F2D_CAL_CAKE: retstat = '', i6)')
!           :             retstat
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              Return
           End If
 
!        Set resulting values
           start_azimuth = REALS(1) * Pi / 180.0
           end_azimuth = REALS(2) * Pi / 180.0
           inner_limit = REALS(3) * experiment%x_pixel_size
           outer_limit = REALS(4) * experiment%x_pixel_size
           scan_type = INTEGERS(5)
           one_degree = LOGICALS(6)
           num_azimuthal = INTEGERS(7)
           num_2theta = INTEGERS(8)
           conserve_intensity = LOGICALS(9)
           experiment%correct_polarisation = LOGICALS(10)
           experiment%polarisation = REALS(11)
           maximum_d = REALS(12)
           correct_geometry = LOGICALS(13)
           correct_parallax = LOGICALS(14)
 
!        Modify start azimuth to -Pi to Pi range
           mod_start_azimuth = start_azimuth
           Do While (mod_start_azimuth .Gt. Pi)
              mod_start_azimuth = mod_start_azimuth - 2.0 * Pi
           End Do
           Do While (mod_start_azimuth .Lt. -Pi)
              mod_start_azimuth = mod_start_azimuth + 2.0 * Pi
           End Do
 
!        Modify end azimuth to be sure that it is greater than start azimuth
           mod_end_azimuth = end_azimuth
           Do While (mod_end_azimuth - mod_start_azimuth .Lt. 0.0)
              mod_end_azimuth = mod_end_azimuth + 2.0 * Pi
           End Do
           Do While (mod_end_azimuth - mod_start_azimuth .Gt. 2.0 * Pi)
              mod_end_azimuth = mod_end_azimuth - 2.0 * Pi
           End Do
 
!        Store user preferences in data base
           Call IO_SET_IKEYVALUE ('SCAN_TYPE', scan_type, retstat, status)
           Call IO_SET_RKEYVALUE ('MAXIMUM_D', maximum_d, retstat, status)
           Call IO_SET_LKEYVALUE ('CAKE_DEFAULT_1_DEGREE', one_degree, &
             retstat, status)
 
!        Set Lorentz / Geometircal correction type
           If (correct_geometry) Then
              lorentz_geometry = 1
           Else
              lorentz_geometry = 0
           End If
 
           If (outer_limit .Le. inner_limit) Then
 
!           Output warning
              Call GS_FWARNING (1, 1, &
                '! OUTER LIMIT MUST BE GREATER THAN INNER LIMIT !', status)
 
           Else
              continue = .False.
           End If
 
        End Do
 
!     Inform user of calculation and swapping of data to the memory
        MESSAGE(1) = 'WORKING: CALCULATING TRANSFORM'
        MESSAGE(2) = 'NOTE: ORIGINAL DATA STORED IN THE MEMORY'
        Call GS_FPROMPT (2, 2, MESSAGE, status)
 
!     Force output
        Call GS_UPDATE (status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Get dynamic work array space
        Allocate (WORK(num_azimuthal, num_2theta), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CAL_CAKE ' // Version)
           Return
        End If
 
!     Convert beam centre in data coordinates to pixel coordinates
!     (this accounts for axes starting at offset values)
!     MODIFIED: This did not work properly when the beam-centre was outside
!     the detector image, as "MA_DC2PIXC" limited values to the axis region
        x_beampc = experiment%x_beam
        y_beampc = experiment%y_beam
 
!**OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD
!     Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, x_beam, x_beampc,
!     :      status)
!     Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, y_beam, y_beampc,
!     :      status)
!**OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD

!     Get start time
        Call IO_TIMES (start_elapse, start_cpu, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Read (*, *)
!        Call GS_INPL (.False., 0, 1, .True., 'TEST INPUT', 1, &
!          'TEST INPUT', 1, 'HOW?', testl, status)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

        Call F2D_CAL2_CAKE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, MASK, conserve_intensity, scan_type, maximum_d, &
          experiment, mod_start_azimuth, mod_end_azimuth, inner_limit, &
          outer_limit, num_2theta, num_azimuthal, &
          lorentz_geometry, correct_parallax, xmaxdat, &
          ymaxdat, num_2theta, num_azimuthal, az_pixel_size, rad_pixel_size, &
          inner_2theta, WORK, MDATA, status)

!     Get end time
        Call IO_TIMES (end_elapse, end_cpu, status)


!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_CAL_CAKE: After F2D_CAL2_CAKE'')')
!        Call GS_INPL (.False., 0, 1, .True., 'TEST INPUT', 1, &
!          'TEST INPUT', 1, 'HOW?', testl, status)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Output time for re-binning
        Write (MESSAGE(1), '(''INFO: Elapse time for operation = '', f6.2 ' // &
          ''' seconds'')')  end_elapse - start_elapse
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''      CPU Time = '', f6.2, '' seconds'')') &
          end_cpu - start_cpu
        Call IO_WRITE (MESSAGE(1), status)

!     Free dynamic array space
        Deallocate (WORK)
 
!     Define axis data !!! SHOULD BE IMPROVED
        Do pixel = 1, num_azimuthal
           MYAXIS(pixel) = (mod_start_azimuth + (Real(pixel) - 0.5) * &
             az_pixel_size) * 180.0 / Pi
        End Do
 
        If (scan_type .Eq. 1) Then
 
           Do pixel = 1, num_2theta
              MXAXIS(pixel) = (inner_2theta + (Real(pixel) - 0.5) * &
                rad_pixel_size) * 180.0 / Pi
           End Do
 
        Else If (scan_type .Eq. 0) Then
 
           Do pixel = 1, num_2theta
              MXAXIS(pixel) = (inner_limit + (Real(pixel) - 0.5) * &
                rad_pixel_size) * 1000.0
           End Do
 
        Else If (scan_type .Eq. 2) Then
 
           Do pixel = 1, num_2theta
              MXAXIS(pixel) = (inner_2theta + (Real(pixel) - 0.5) * &
                rad_pixel_size)
           End Do
 
        Else If (scan_type .Eq. 3) Then
 
           Do pixel = 1, num_2theta
              MXAXIS(pixel) = (inner_2theta + (Real(pixel) - 0.5) * &
                rad_pixel_size)
           End Do
 
        End If
 
!     Set title and labels
        If (scan_type .Eq. 0) Then
           mtitle = Trim(title) // ': Azimuth/Radial Distance'
           mxlabel = 'Radial Distance (mm)'
        Else If (scan_type .Eq. 1) Then
           mtitle = Trim(title) // ': Azimuth/2-theta'
           mxlabel = '2-Theta Angle (Degrees)'
        Else If (scan_type .Eq. 2) Then
           mtitle = Trim(title) // ': Q-Space Scan'
           mxlabel = 'Q (Inverse Nanometres)'
        Else If (scan_type .Eq. 3) Then
           mtitle = Trim(title) // ': D-Spacings Scan'
           mxlabel = 'D-spacing (Angstroms)'
        End If
        mylabel = 'Azimuth (Degrees)'
 
!     Set memory to exist and set memory ROI
        memory_defined = .True.
        mxstrelm = 1
        mystrelm = 1
        mxendelm = num_2theta
        myendelm = num_azimuthal
        mxnumdat = mxendelm
        mynumdat = myendelm
  
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_CAL_CAKE: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_CAL_CAKE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
