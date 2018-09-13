!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_gui_experiment.f90 *
!  *                        *
!  **************************
 
!+ F2D_GUI_EXPERIMENT: GUI experimental GEOMETRY
     Subroutine F2D_GUI_EXPERIMENT (detector_offset, experiment, status)
!  Description:
!    Graphical form to set experimental geometry parameters
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    27-Nov-2014: V0.12 Add option to input detector offset (Hammersley)
!    30-Aug-2006: V0.11 Add rotation about beam (Hammersley)
!    16-Mar-2006: V0.10 Don't save values in data-base (Hammersley)
!    10-Mar-2006: V0.9 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    31-Mar-2005: V0.8 Decrease minimum wavelength to 0.00001 Angstroms
!      as previous limit was too high for TEM 2D diffraction work (Hammersley)
!    11-Apr-2003: V0.7 Increase maximum allowed distance to 100m (Hammersley)
!    24-Feb-1999: V0.6 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.5 Change to use IO internal database routines (Hammersley)
!    27-Oct-1998: V0.4 Save pixel sizes in internal data-base (Hammersley)
!    31-Aug-1998: V0.3 Save geometry values in internal data-base (Hammersley)
!    30-Jan-1998: V0.2 Declare unused character string for call to "GS_FORM"
!      (Hammersley)
!    28-Jan-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: detector_offset ! .True., if the detector offset 
!      parameter is presented 
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.12' ! Version number
     Integer, Parameter :: Max_chars =  42 ! Maximum characters in a line
     Integer, Parameter :: Max_lines =   95 ! Number of lines in message
     Integer, Parameter :: Max_choices = 15 ! Maximum number of choices:
!      Note: Must of at least 5 more than "num_choices" to allow for general
!      form buttons
!  Local Variables:
     Integer :: dummy ! Dummy variable, not used
     Integer :: num_choices ! Number of choices
     Integer :: retstat ! Return status variable for "GS_FORM"
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 128) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 70) :: PROMPT(2) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = Max_chars) :: TX(Max_lines) ! Text array
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
     Logical :: BOUND(Max_choices) ! .True., if variable range is bound
     Real :: REALS(Max_choices) ! Real variables to be input
     Real :: REALS_LOWER(Max_choices) ! Lower bound of real variables
     Real :: REALS_UPPER(Max_choices) ! Upper bound of real variables
!  External Functions:
!  Local Data:
     Data TX(   1) / ' ' /
     Data TX(   2) / '      -------------------------------'/
     Data TX(   3) / '      Welcome to the FIT2D Experimental'/
     Data TX(   4) / '             Geometry Graphical Menu'/
     Data TX(   5) / '      -------------------------------'/
     Data TX(   6) / ' ' /
     Data TX(   7) / 'This Experimental Geometry form allows you'/
     Data TX(   8) / 'to change the variables which describe the'/
     Data TX(   9) / 'geometry of the experiment. (Usually a'/
     Data TX(  10) / 'diffraction experiment.)'/
     Data TX(  11) / ' ' /
     Data TX(  12) / 'The experiment is assumed to consist of'/
     Data TX(  13) / 'a flat detector mounted roughly orthogonal'/
     Data TX(  14) / 'to a beam (usually X-rays, but could be'/
     Data TX(  15) / 'electrons, neutrons, etc.) which passes'/
     Data TX(  16) / 'through a sample.'/
     Data TX(  17) / ' ' /
     Data TX(  18) / 'Non-flat detectors and spatial distorted'/
     Data TX(  19) / 'detectors should be corrected to a regular'/
     Data TX(  20) / 'flat grid detector prior to any operations'/
     Data TX(  21) / 'which rely on these parameters.'/
     Data TX(  22) / ' ' /
     Data TX(  23) / 'NOTE: Depending on the context, not all'/
     Data TX(  24) / 'of the parameters may need setting. e.g.'/
     Data TX(  25) / 'The "TILT" command will deduce the'/
     Data TX(  26) / '"TILT ROTATION" and "TILT ANGLE" so'/
     Data TX(  27) / 'these will probably not be set here.'/
     Data TX(  28) / ' ' /
     Data TX(  29) / 'The available parameters are:'/
     Data TX(  30) / ' ' /
     Data TX(  31) / '"X-PIXEL SIZE": Sets the dimension of the'/
     Data TX(  32) / 'pixels in the horizontal direction (as'/
     Data TX(  33) / 'displayed on the screen). The units are'/
     Data TX(  34) / 'in microns.'/
     Data TX(  35) / ' ' /
     Data TX(  36) / '"Y-PIXEL SIZE": Sets the dimension of the'/
     Data TX(  37) / 'pixels in the vertical direction (as'/
     Data TX(  38) / 'displayed on the screen). The units are'/
     Data TX(  39) / 'in microns.'/
     Data TX(  40) / ' ' /
     Data TX(  41) / '"DISTANCE": Sets the sample to detector'/
     Data TX(  42) / 'distance. This is the distance from the'/
     Data TX(  43) / 'centre of the sample to the intersection'/
     Data TX(  44) / 'of the beam and the detector, or the'/
     Data TX(  45) / 'plane of the detector.'/
     Data TX(  46) / ' ' /
     Data TX(  47) / ' ' /
     Data TX(  48) / '"X-BEAM CENTRE": Sets the position "on"'/
     Data TX(  49) / 'the detector of the direct beam'/
     Data TX(  50) / 'intersection. This is the horizontal'/
     Data TX(  51) / 'coordinate as displayed. The units are'/
     Data TX(  52) / 'in pixels and fractions of pixels.'/
     Data TX(  53) / '(The left-hand edge of the first pixel'/
     Data TX(  54) / 'has the coordinate 0.0 and the right-'/
     Data TX(  55) / 'hand edge 1.0.)'/
     Data TX(  56) / ' ' /
     Data TX(  57) / '"Y-BEAM CENTRE": Sets the position "on"'/
     Data TX(  58) / 'the detector of the direct beam'/
     Data TX(  59) / 'intersection. This is the vertical'/
     Data TX(  60) / 'coordinate as displayed. The units are'/
     Data TX(  61) / 'in pixels and fractions of pixels.'/
     Data TX(  62) / '(The lower edge of the first pixel'/
     Data TX(  63) / 'has the coordinate 0.0 and the upper'/
     Data TX(  64) / 'edge 1.0.)'/
     Data TX(  65) / ' ' /
     Data TX(  66) / '"TILT ROTATION": Is the angle of'/
     Data TX(  67) / 'rotation of a plane in which the angle'/
     Data TX(  68) / 'of tilt is defined. The plane is defined'/
     Data TX(  69) / 'by a line at this angle from the inter-'/
     Data TX(  70) / 'section of the beam with the detector,'/
     Data TX(  71) / 'in the plane orthogonal to the beam. It'/
     Data TX(  72) / 'is defined anti-clockwise in degrees'/
     Data TX(  73) / 'from the X-axis. This angle may well be'/
     Data TX(  74) / 'poorly defined by the data, unless the'/
     Data TX(  75) / 'angle of tilt is large. Variation of a'/
     Data TX(  76) / 'few degrees and even 10''''s of degrees'/
     Data TX(  77) / 'should be of little concern.'/
     Data TX(  78) / ' ' /
     Data TX(  79) / '"ANGLE OF TILT": This is the angle of'/
     Data TX(  80) / 'tilt in degrees anti-clockwise from the'/
     Data TX(  81) / 'line from the detector-beam intersection'/
     Data TX(  82) / 'defined by "TILT ROTATION". This defines'/
     Data TX(  83) / 'the non-orthogonality of the detector.'/
     Data TX(  84) / 'Normally this value should be small;'/
     Data TX(  85) / 'typically 1 degree or less. An exception'/
     Data TX(  86) / 'is when the detector is deliberately'/
     Data TX(  87) / 'tilted. This angle should be well'/
     Data TX(  88) / 'defined by the data and is hopefully'/
     Data TX(  89) / 'repeatable to a small fraction of a'/
     Data TX(  90) / 'degree.'/
     Data TX(  91) / ' ' /
     Data TX(  92) / '---------------'/
     Data TX(  93) / 'END OF HELP TEXT'/
     Data TX(  94) / '---------------'/
     Data TX(  95) / ' ' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_EXPERIMENT ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8

        If (detector_offset) Then
           num_choices = 10
        Else
           num_choices = 9
        End If

        PROMPT(1) = 'EXPERIMENTAL GEOMETRY '
        PROMPT(2) = 'CONTROL FORM'
 
        BUTTONS(1) = 'X-PIXEL SIZE'
        BUTTONS(2) = 'Y-PIXEL SIZE'
        BUTTONS(3) = 'DISTANCE'
        BUTTONS(4) = 'WAVELENGTH'
        BUTTONS(5) = 'X-BEAM CENTRE'
        BUTTONS(6) = 'Y-BEAM CENTRE'
        BUTTONS(7) = 'TILT ROTATION'
        BUTTONS(8) = 'ANGLE OF TILT'
        BUTTONS(9) = 'DETECTOR ROTATION'
        BUTTONS(10) = 'DETECTOR OFFSET'
 
        TYPES(1) = Gs_real
        TYPES(2) = Gs_real
        TYPES(3) = Gs_real
        TYPES(4) = Gs_real
        TYPES(5) = Gs_real
        TYPES(6) = Gs_real
        TYPES(7) = Gs_real
        TYPES(8) = Gs_real
        TYPES(9) = Gs_real
        TYPES(10) = Gs_real
 
        TEXT(1) = 'SIZE OF HORIZONTAL PIXELS (MICRONS)'
        TEXT(2) = 'SIZE OF VERTICAL PIXELS (MICRONS)'
        TEXT(3) = 'SAMPLE TO DETECTOR DISTANCE (MM)'
        TEXT(4) = 'WAVELENGTH (ANGSTROMS)'
        TEXT(5) = 'X-PIXEL COORDINATE OF DIRECT BEAM'
        TEXT(6) = 'Y-PIXEL COORDINATE OF DIRECT BEAM'
        TEXT(7) = 'ROTATION ANGLE OF TILTING PLANE (DEGREES)'
        TEXT(8) = 'ANGLE OF DETECTOR TILT IN PLANE (DEGREES)'
        TEXT(9) = 'ROTATION ANGLE OF DETECTOR X-AXIS FROM HORIZONTAL (DEGREES)'
        TEXT(10) = 'DISTANCE OF START OF DETECTOR FROM ROTATION CENTRE (PIXELS)'
 
        FULL_PROMPTS(1) = 'Enter dimension of pixels ' // &
          'horizontally, as displayed (microns)'
        FULL_PROMPTS(2) = 'Enter dimension of pixels ' // &
          'vertically, as displayed (microns)'
        FULL_PROMPTS(3) = 'Enter sample to detector distance ' // '(mm)'
        FULL_PROMPTS(4) = 'Enter the wavelength of the radiation ' // &
          ' in Angstrom units'
        FULL_PROMPTS(5) = 'Enter X-pixel coordinate of ' // &
          'centre of the direct beam'
        FULL_PROMPTS(6) = 'Enter Y-pixel coordinate of ' // &
          'centre of the direct beam'
        FULL_PROMPTS(7) = 'Enter rotation angle of the plane ' // &
          'in which the tilt occurs'
        FULL_PROMPTS(8) = 'Enter angle of detector tilt ' // &
          'within the tilt plane'
        FULL_PROMPTS(9) = 'Enter angle of rotation of detector x-axis ' // &
          'from horizontal'
        FULL_PROMPTS(10) = 'Enter the distance from the rotation centre ' // &
          'to the start of the 1-D detector in pixels'
 
        BOUND(1) = .True.
        BOUND(2) = .True.
        BOUND(3) = .True.
        BOUND(4) = .True.
        BOUND(5) = .False.
        BOUND(6) = .False.
        BOUND(7) = .True.
        BOUND(8) = .True.
        BOUND(9) = .True.
        BOUND(10) = .True.
 
        REALS_LOWER(1) = 0.001
        REALS_UPPER(1) = 10000.0
        REALS_LOWER(2) = 0.001
        REALS_UPPER(2) = 10000.0
        REALS_LOWER(3) = 0.01
        REALS_UPPER(3) = 100000.0
        REALS_LOWER(4) = 0.0001
        REALS_UPPER(4) = 10000.0
        REALS_LOWER(7) = -180.0
        REALS_UPPER(7) = 180.0
        REALS_LOWER(8) = -90.0
        REALS_UPPER(8) = 90.0
        REALS_LOWER(9) = -180.0
        REALS_UPPER(9) = 180.0
        REALS_LOWER(10) = 0.0
        REALS_UPPER(10) = 10000.0
 
        REALS(1) = experiment%x_pixel_size * 1.0e6
        REALS(2) = experiment%y_pixel_size * 1.0e6
        REALS(3) = experiment%detector_distance * 1000.0
        REALS(4) = experiment%wavelength * 1.0e10
        REALS(5) = experiment%x_beam
        REALS(6) = experiment%y_beam
        REALS(7) = experiment%tilt_plane_rotation * 180.0 / Pi
        REALS(8) = experiment%tilt_angle * 180.0 / Pi
        REALS(9) = experiment%detector_rotation * 180.0 / Pi
        REALS(10) = experiment%detector_offset / experiment%x_pixel_size
 
!     Output interactive graphical form
        Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, Max_choices, &
          num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, dummy, &
          dummy, REALS_LOWER, REALS_UPPER, dummy, dummy, REALS, STRINGS, &
          retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           status = St_escapevalue
           Return
        End If
 
!     Set resulting values
        experiment%x_pixel_size = REALS(1) / 1.0e6
        experiment%y_pixel_size = REALS(2) / 1.0e6
        experiment%detector_distance = REALS(3) / 1000.0
        experiment%wavelength =  REALS(4) / 1.0e10
        experiment%x_beam = REALS(5)
        experiment%y_beam = REALS(6)
        experiment%tilt_plane_rotation = REALS(7) * Pi / 180.0
        experiment%tilt_angle = REALS(8) * Pi / 180.0
        experiment%detector_rotation = REALS(9) * Pi / 180.0
        experiment%detector_offset = REALS(10) * experiment%x_pixel_size

!     Set set values
        experiment%pixel_sizes_set = .True.
        experiment%detector_distance_set = .True. 
        experiment%wavelength_set = .True.
        experiment%beam_centre_set = .True.
        experiment%tilt_set = .True.

     End If
 
     End Subroutine F2D_GUI_EXPERIMENT
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
