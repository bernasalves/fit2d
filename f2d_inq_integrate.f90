!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_inq_integrate.f90 *
!  *                       *
!  *************************
 
!+ F2D_INQ_INTEGRATE - FIT2D: INQuire INTEGRATE parameters
     Subroutine F2D_INQ_INTEGRATE (input_file, xmaxdat, ymaxdat, &
       xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, experiment, &
       outer_limit, scan_type, maximum_d, conserve_intensity, &
       correct_geometry, outer_angle, &
       num_2theta, lorentz_geometry, rad_pixel_size, outer_q, &
       save_parameters, parameter_file, use_lut, correct_parallax, status)
!  Description:
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    20-Sep-2012: V0.9 Option to correct parallax effect (Hammersley)
!    23-Aug-2012: V0.8 Option of look-up table calculation (Hammersley)
!    14-Oct-2011: V0.7 Option to output integration parameters to a file
!      (Hammersley)
!    13-Mar-2006: V0.6 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    23-Feb-1999: V0.5 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.4 Change to use IO internal database routines (Hammersley)
!    30-Jan-1998: V0.3 Declare unused character string for call to "GS_FORM"
!      (Hammersley)
!    25-Feb-1998: V0.2 Specify maximum D-spacing for D-spacing scans
!      (Hammersley)
!    20-Feb-1998: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use GS_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Character(Len = *), Intent(IN) :: input_file ! Name of input file
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements defined in
!      Y-direction
     Integer, Intent(IN) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: xendelm ! The X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! The Y-pixel number for the end of the ROI
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(INOUT) :: lorentz_geometry ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the equivalent of a 
!            2-theta scan i.e. detector at equal distance
     Real, Intent(INOUT) :: outer_limit ! Outer radius in metres
     Integer, Intent(INOUT) :: scan_type ! Type of 1-D scan to be produced:
!      0 = Radial: equal distance scan
!      1 = "2-theta": equal angle element scan
!      2 = Q-space: Equal Q element scan
!          (Q = (4 * Pi / wavelength) Sin(theta/2) )
!      3 = D-spacing scan
     Real, Intent(INOUT) :: maximum_d ! Maximum D-spacing to calculate for
!      D-spacing scans
     Logical, Intent(INOUT) :: conserve_intensity ! .True., if the total
!      intensity in to be conserved in the re-binning operation
     Logical, Intent(INOUT) :: correct_geometry ! .True., if the scan
!      intensities are to be corrected for the 1/Cos**3(angle) drop owing 
!      to the geometrical difference between a 2-theta scan and a flat detector
!  Export:
     Real, Intent(OUT) :: rad_pixel_size ! Size of pixel to be used for the
!      2-theta, radial, or q-space bin sizes. If an equal radial
!      distance scan is to be calculated ('scan_type = 0') the units
!      are metres, and for an equal angle pixel scan ('scan_type = 1')
!      the units are radians, and for a Q-space scan the units are
!      inverse Angstroms (?)
     Real, Intent(OUT) :: outer_angle ! Maximum angle of 2-theta scan
     Integer, Intent(OUT) :: num_2theta ! Number of 2-theta angle or radial
!      bins in rebinned array
     Real, Intent(OUT) :: outer_q ! Outer q value
     Logical, Intent(OUT) :: save_parameters ! .True., if integration parameters
!      are to be saved to an output file
     Character(Len = *), Intent(OUT) :: parameter_file ! Name of file to save
!      integration parameters
     Logical, Intent(OUT) :: use_lut ! .True., if a look-up table is to be
!      used for the re-binning operation
     Logical, Intent(OUT) :: correct_parallax ! .True., if the effect of 
!      parallax on angular position is to be corrected
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.9' ! Version number
     Integer, Parameter :: Max_chars =  48 ! Maximum number of characters
!      in a line
     Integer, Parameter :: Max_lines =   82 ! Number of lines in message
     Integer, Parameter :: Max_choices = 17 ! Maximum number of choices
!      (need 5 extra spaces)
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
!  Local Variables:
     Integer :: num_choices ! Number of choices
     Integer :: retstat ! Return status variable
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 70) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 70) :: PROMPT(3) ! User prompt text
     Character(Len = Len_name) :: STRINGS(Max_choices) ! String for call to 
!      "GS_FORM"
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
     Real :: REALS_LOWER(Max_choices) ! Lower bounds on reals
     Real :: REALS_UPPER(Max_choices) ! Upper bounds on reals
     Real :: REALS(Max_choices) ! Reals to be changed
!  External Functions:
!  Local Data:
     Data TX(   1) / ' ' /
     Data TX(   2) / '      ------------------------'/
     Data TX(   3) / '      INTEGRATE Command Form Help'/
     Data TX(   4) / '      ------------------------'/
     Data TX(   5) / ' ' /
     Data TX(   6) / 'The "INTEGRATE" command allows integration'/
     Data TX(   7) / 'of 2-D data around the azimuth to 1-D'/
     Data TX(   8) / '"2-theta" equal angle, equal radial distance'/
     Data TX(   9) / 'scans, or equal Q-space binned scans.'/
     Data TX(  10) / 'Detector non-orthogonality is accounted for,'/
     Data TX(  11) / 'as is the effect of polarisation on'/
     Data TX(  12) / 'intensities. Different types of intensity'/
     Data TX(  13) / 'normalisation are possible.'/
     Data TX(  14) / ' ' /
     Data TX(  15) / 'Normally the beam centre and tilt angles will'/
     Data TX(  16) / 'have calculated by a previous call to the'/
     Data TX(  17) / '"TILT" command.'/
     Data TX(  18) / ' ' /
     Data TX(  19) / ' ' /
     Data TX(  20) / 'The following control "buttons" are available:'/
     Data TX(  21) / ' ' /
     Data TX(  22) / '"SCAN TYPE": Allows one of 3 different types'/
     Data TX(  23) / 'of integrated scans to be selected:'/
     Data TX(  24) / ' ' /
     Data TX(  25) / '   "2-THETA": This is an equal angle scan,'/
     Data TX(  26) / '   equivalent to a "2-THETA" scan on a'/
     Data TX(  27) / '   powder diffractometer. The scale is in'/
     Data TX(  28) / '   degrees.'/
     Data TX(  29) / ' ' /
     Data TX(  30) / '   "Q-SPACE": This is similar to the "2-THETA"'/
     Data TX(  31) / '   scan, but the output elements are in equal'/
     Data TX(  32) / '   Q-range bins. The scale is in inverse'/
     Data TX(  33) / '   Angstroms.'/
     Data TX(  34) / ' ' /
     Data TX(  35) / '   "RADIAL": This is an equal radial distance'/
     Data TX(  36) / '   element scan. The scale is in millimetres.'/
     Data TX(  37) / ' ' /
     Data TX(  38) / '"CONSERVE INT.": "NO" means that the output'/
     Data TX(  39) / 'intensities are normalised by the number of'/
     Data TX(  40) / 'contributing pixels (although geometrical'/
     Data TX(  41) / 'corrections may be applied). This is appropriate'/
     Data TX(  42) / 'for producing the equivalent of a 2-theta scan,'/
     Data TX(  43) / 'and for a Q-space scan, but does not converse'/
     Data TX(  44) / 'total intensity. For applications which require'/
     Data TX(  45) / 'integrated intensities to be conserved, this'/
     Data TX(  46) / 'should be set to "YES".'/
     Data TX(  47) / ' ' /
     Data TX(  48) / '"POLARISATION": "YES" to apply a polarisation'/
     Data TX(  49) / 'correction to the intenisities during the'/
     Data TX(  50) / 'integration.'/
     Data TX(  51) / ' ' /
     Data TX(  52) / '"FACTOR": This is the polarisation factor which'/
     Data TX(  53) / 'is used if the polarisation correction is'/
     Data TX(  54) / 'applied.'/
     Data TX(  55) / ' ' /
     Data TX(  56) / 'The polarisation factor is defined as'/
     Data TX(  57) / '(I_h - I_v) / (I_h + I_v), where I_h is the'/
     Data TX(  58) / 'horizontal component and I_v is the vertical'/
     Data TX(  59) / 'component. (Horizontal should normally'/
     Data TX(  60) / 'correspond to the X-direction of the image.)'/
     Data TX(  61) / ' ' /
     Data TX(  62) / '"GEOMETRY COR.": "YES" corrects a flat "scan" to'/
     Data TX(  63) / 'the equivalent of a 2-theta scan, or a Q-space'/
     Data TX(  64) / 'scan. ("CONSERVE INT. should be "NO"). These are'/
     Data TX(  65) / 'the effect of change of distance and obiqueness'/
     Data TX(  66) / 'at higher angles for the flat image plate'/
     Data TX(  67) / 'compared to a detector on a 2-theta arm, always'/
     Data TX(  68) / 'facing the sample.'/
     Data TX(  69) / ' ' /
     Data TX(  70) / '"MAX ANGLE": Maximum angle of the output scan'/
     Data TX(  71) / 'in degrees.'/
     Data TX(  72) / ' ' /
     Data TX(  73) / '"SCAN BINS": Number of bins in the output scan.'/
     Data TX(  74) / 'This may be increased up to the size of the'/
     Data TX(  75) / 'program array in the first dimension. Larger'/
     Data TX(  76) / 'values lead to over-sampling with may help'/
     Data TX(  77) / 'obtain better profiles.'/
     Data TX(  78) / ' ' /
     Data TX(  79) / '---------------'/
     Data TX(  80) / 'END OF HELP TEXT'/
     Data TX(  81) / '---------------'/
     Data TX(  82) / ' ' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INQ_INTEGRATE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_INQ_INTEGRATE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Allow experimental geometry to be changed
        Call F2D_GUI_EXPERIMENT (.False., experiment, status)
 
!     Check status
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Calculate number of 2-theta bins
        outer_limit = Max( Sqrt(((Real(xstrelm) - experiment%x_beam) * &
          experiment%x_pixel_size)**2 + ((Real(ystrelm) - experiment%y_beam) * &
          experiment%y_pixel_size)**2), Sqrt(((Real(xendelm) &
          - experiment%x_beam) * experiment%x_pixel_size)**2 + &
          ((Real(ystrelm) - experiment%y_beam) * experiment%y_pixel_size)**2), &
          Sqrt(((Real(xstrelm) - experiment%x_beam) * &
          experiment%x_pixel_size)**2 &
          + ((Real(yendelm) - experiment%y_beam) * &
          experiment%y_pixel_size)**2), &
          Sqrt(((Real(xendelm) - experiment%x_beam) * &
          experiment%x_pixel_size)**2 + &
          ((Real(yendelm) - experiment%y_beam) * experiment%y_pixel_size)**2))
 
!     Set to 1 bin per pixel (approximately)
        num_2theta = Min(Max(1, Nint(outer_limit / &
          Min(experiment%x_pixel_size, experiment%y_pixel_size))), xmaxdat)
 
!     Calculate outer angle of scan
        outer_angle = Atan2(outer_limit, experiment%detector_distance)
 
!     Calculate transformation
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

!     Create default parameter file name based on input file
        Call IO_OUTFILE (0, input_file, '.par', 'fit2d.par', parameter_file, &
          status)

!     Allow user of choose number of output bins and control various other 
!     options
        num_choices = 12
        PROMPT(1) = 'CONTROL OF RADIAL, 2-THETA, OR Q'
        PROMPT(2) = 'SCAN RE-BINNING PARAMETERS'
        BUTTONS(1) = 'SCAN TYPE'
        BUTTONS(2) = 'CONSERVE INT.'
        BUTTONS(3) = 'POLARISATION'
        BUTTONS(4) = 'FACTOR'
        BUTTONS(5) = 'GEOMETRY COR.'
        BUTTONS(6) = 'MAX. ANGLE'
        BUTTONS(7) = 'SCAN BINS'
        BUTTONS(8) = 'MAX. D-SPACING'
        BUTTONS(9) = 'SAVE PARAMETERS'
        BUTTONS(10) = 'PARAMETER FILE'
        BUTTONS(11) = 'USE LOOK-UP TABLE'
        BUTTONS(12) = 'CORRECT PARALLAX'
 
        TYPES(1) = Gs_scan_type
        TYPES(2) = Gs_logical
        TYPES(3) = Gs_logical
        TYPES(4) = Gs_real
        TYPES(5) = Gs_logical
        TYPES(6) = Gs_real
        TYPES(7) = Gs_integer
        TYPES(8) = Gs_real
        TYPES(9) = Gs_logical
        TYPES(10) = Gs_output_file
        TYPES(11) = Gs_logical
        TYPES(12) = Gs_logical
        TEXT(1) = 'SCAN TYPE (D, RADIAL, 2-THETA, Q-SPACE)'
        TEXT(2) = 'INTENSITY CONSERVATION'
        TEXT(3) = 'APPLY POLARISATION CORRECTION'
        TEXT(4) = 'POLARISATION FACTOR'
        TEXT(5) = 'GEOMETRICAL CORRECTION TO INTENSITIES'
        TEXT(6) = 'MAXIMUM 2-THETA ANGLE OF SCAN (DEGREES)'
        TEXT(7) = 'NUMBER OF BINS IN OUTPUT SCAN'
        TEXT(8) = 'MAXIMUM FOR D-SPACINGS SCANS (ANGSTROMS)'
        TEXT(9) = 'SAVE INTEGRATION PARAMETERS TO A FILE'
        TEXT(10) = 'NAME OF PARAMETERS FILE'
        TEXT(11) = 'USE LOOK-UP TABLE TO SAVE CALCULATIONS'
        TEXT(12) = 'CORRECT PARALLAX EFFECT ON ANGULAR POSITION'
        FULL_PROMPTS(1) = 'Select type of output 1-D scan: ' // &
          '"RADIAL", "2-THETA", or "Q-SPACE"'
        FULL_PROMPTS(2) = 'Enter "YES" to conserve total ' // &
          'intensity, "NO" for "2-theta scans" (integers only)'
        FULL_PROMPTS(3) = 'Enter "YES" to apply polarisation correction'
        FULL_PROMPTS(4) = 'Enter ratio of horizontal to ' // &
          'vertical polarisation'
        FULL_PROMPTS(5) = 'Enter "YES" if scan intensities are ' // &
          'corrected for geometry'
        FULL_PROMPTS(6) = 'Enter maximum 2-theta angle of scan ' // &
          '(degrees)'
        FULL_PROMPTS(7) = 'Enter number of 2-theta or radial ' // &
          'bins in output scan'
        FULL_PROMPTS(8) = 'Enter maximum of range for ' // &
          'calculation of D-spacing scans (Angstroms)'
        FULL_PROMPTS(9) = 'Enter "YES" to save the integration parameter ' // &
          'to a file'
        FULL_PROMPTS(10) = 'Select name of file to contain integration ' // &
          'parameter values'
        FULL_PROMPTS(11) = 'Enter "YES" to use a look-up table to ' // &
          'save calculations for repeat integrations'
        FULL_PROMPTS(12) = 'Enter "YES" to correct the effect of parallax ' // &
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
        BOUND(10) = .False.
        BOUND(11) = .True.
        BOUND(12) = .True.
 
        INTEGERS(1) = scan_type
        LOGICALS(2) = conserve_intensity
        LOGICALS(3) = experiment%correct_polarisation
        REALS(4) = experiment%polarisation
        REALS_LOWER(4) = -1.0
        REALS_UPPER(4) = 1.0
        LOGICALS(5) = correct_geometry
        REALS(6) = outer_angle * 180.0 / Pi
        REALS_LOWER(6) = 0.001
        REALS_UPPER(6) = 90.0
        INTEGERS(7) = num_2theta
        INTS_LOWER(7) = 2
        INTS_UPPER(7) = xmaxdat
        REALS(8) = maximum_d
        REALS_LOWER(8) = 1.0
        REALS_UPPER(8) = 10000.0
        LOGICALS(9) = save_parameters
        STRINGS(10) = parameter_file
        LOGICALS(11) = use_lut
        LOGICALS(12) = correct_parallax

!     Output interactive graphical form
        Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, Max_choices, &
          num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, INTS_LOWER, &
          INTS_UPPER, REALS_LOWER, REALS_UPPER, INTEGERS, LOGICALS, REALS, &
          STRINGS, retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           status = St_escapevalue
           Return
        End If
 
!     Set resulting values
        scan_type = INTEGERS(1)
        conserve_intensity = LOGICALS(2)
        experiment%correct_polarisation = LOGICALS(3)
        experiment%polarisation = REALS(4)
        correct_geometry = LOGICALS(5)
        outer_angle = REALS(6) * Pi / 180.0
        num_2theta = INTEGERS(7)
        maximum_d = REALS(8)
        save_parameters = LOGICALS(9)
        parameter_file = STRINGS(10)
        use_lut = LOGICALS(11)
        correct_parallax = LOGICALS(12)

        experiment%polarisation_set = .True.
 
!     Store user preferences in data base
        Call IO_SET_IKEYVALUE ('SCAN_TYPE', scan_type, retstat, status)
 
!     Set Lorentz/Geometircal correction type
        If (correct_geometry) Then
           lorentz_geometry = 1
        Else
           lorentz_geometry = 0
        End If
 
!     Calculate new outer limits
        outer_limit = experiment%detector_distance * Tan(outer_angle)
        outer_angle = Atan2(outer_limit, experiment%detector_distance)
 
        If (scan_type .Eq. 1) Then
 
!        Calculate angular pixel size
           rad_pixel_size = (outer_angle / Real(num_2theta)) * 180.0 / Pi
 
!        Find closest 1000th of a degree
           rad_pixel_size = ((Nint(rad_pixel_size * 1000.0)) / 1000.0) * Pi / &
             180.0
 
!        Calculate new outer limit for scan
           outer_angle = rad_pixel_size * Real(num_2theta)
 
        Else If (scan_type .Eq. 2) Then
 
           outer_q = 4.0 * Pi / (experiment%wavelength * 1.0e10) * &
             Sin(outer_angle / 2.0)
           rad_pixel_size = (outer_q / Real(num_2theta))
 
        End If
 
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
     If (correct_parallax) Then

!     Input detector characteristics
        Call F2D_INP_DETECTOR (experiment, status)
     End If

     End Subroutine F2D_INQ_INTEGRATE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 
