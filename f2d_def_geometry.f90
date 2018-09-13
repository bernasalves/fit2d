!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_def_geometry.f90 *
!  *                      *
!  ************************
 
!+ F2D_DEF_GEOMETRY: define DEFault GEOMETRY
     Subroutine F2D_DEF_GEOMETRY (experiment, status)
!  Description:
!    Set experimental geometry
!  Keywords:
!    Geometry.Experimental.Default, Default.Geometry.Experimental,
!    Default.Geometry.Experimental
!  Method:
!    Stores geometry parameters in internal database
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    17-Mar-2006: V0.2 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    03-Mar-2004: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
     Integer, Parameter :: Max_chars = 42 ! Maximum number of characters
!      in a line
     Integer, Parameter :: Max_lines = 20 ! Number of lines in message
     Integer, Parameter :: Max_choices = 12 ! Maximum number of choices:
!      Note: Must
!    of at least 5 more than 'num_choices' to allow for general
!    form buttons
!  Local Variables:
     Real :: default_distance ! Default distance (metres)
     Real :: default_x_pixel_size ! Default X-pixel size (metres)
     Real :: default_y_pixel_size ! Default Y pixel size (metres)
     Real :: default_wavelength ! Default wavelength (metres)
     Integer :: dummy ! Dummy variable, not used
     Integer :: num_choices ! Number of choices
     Logical, Save :: override_distance = .False. ! .True., if distance is
!      to be over-ridden with default value on input
     Logical, Save :: override_pixel_sizes = .False. ! .True., if
!      pixel_sizes are to be over-ridden with default value on input
     Logical, Save :: override_wavelength = .False. ! .True., if wavelength
!      is to be over-ridden with default value on input
     Integer :: retstat ! Return status variable:
!      0 = Good status
!      1 = No more room (for storage), or no more values
!      2 = Key not found for retrieval, or element doesn't exist
!      3 = Problem converting character string to a real value
!  Local Arrays:
     Character(Len = 21) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 60) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 70) :: PROMPT(2) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to 'GS_FORM'
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
     Logical :: LOGICALS(Max_choices) ! Contains logical variables to change
     Real :: REALS(Max_choices) ! Real variables to be input
     Real :: REALS_LOWER(Max_choices) ! Lower bound of real variables
     Real :: REALS_UPPER(Max_choices) ! Upper bound of real variables
!  External Functions:
!  Local Data:
     Data TX(   1) / ' ' /
     Data TX(   2) / '      -------------------------------'/
     Data TX(   3) / '      Welcome to the FIT2D Over-ride'/
     Data TX(   4) / '        auxilliary header information form'/
     Data TX(   5) / '      -------------------------------'/
     Data TX(   6) / ' ' /
     Data TX(   7) / 'This over-ride form allows you'/
     Data TX(   8) / 'to control the over-riding of auxiliary file'/
     Data TX(   9) / 'header information and use default values'/
     Data TX(  10) / 'instead'/
     Data TX(  11) / ' ' /
     Data TX(  12) / 'This can also allow inappropriate pixel sizes'/
     Data TX(  13) / 'and other experimental geometry data to be'/
     Data TX(  14) / 'set to appropriate values when data is input'/
     Data TX(  15) / 'from a file which contains no auxiliary'/
     Data TX(  16) / 'information.'/
     Data TX(  17) / ' ' /
     Data TX(  18) / 'END OF HELP TEXT'/
     Data TX(  19) / '---------------'/
     Data TX(  20) / ' ' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DEF_GEOMETRY ' // Version)
     Else
 
!     Initialise with current geometry values
        default_distance = experiment%detector_distance
        default_wavelength = experiment%wavelength
        default_x_pixel_size = experiment%x_pixel_size
        default_y_pixel_size = experiment%y_pixel_size
 
!     Get values from internal data-base if defined
        Call IO_INQ_LKEYVALUE ('OVERRIDE_WAVELENGTH', override_wavelength, &
          retstat, status)
        Call IO_INQ_RKEYVALUE ('DEFAULT_WAVELENGTH', default_wavelength, &
          retstat, status)
        Call IO_INQ_LKEYVALUE ('OVERRIDE_DISTANCE', override_distance, &
          retstat, status)
        Call IO_INQ_RKEYVALUE ('DEFAULT_DETECTOR_DISTANCE', default_distance, &
          retstat, status)
        Call IO_INQ_LKEYVALUE ('OVERRIDE_PIXEL_SIZES', override_pixel_sizes, &
          retstat, status)
        Call IO_INQ_RKEYVALUE ('DEFAULT_X_PIXEL_SIZE', default_x_pixel_size, &
          retstat, status)
        Call IO_INQ_RKEYVALUE ('DEFAULT_Y_PIXEL_SIZE', default_y_pixel_size, &
          retstat, status)
 
        num_choices = 7
        PROMPT(1) = 'OVER-RIDE FILE INPUT AUXILLIARY'
        PROMPT(2) = 'VALUES CONTROL FORM'
 
        BUTTONS(1) = 'OVER-RIDE DISTANCE'
        BUTTONS(2) = 'DEFAULT DISTANCE'
        BUTTONS(3) = 'OVER-RIDE WAVELENGTH'
        BUTTONS(4) = 'DEFAULT WAVELENGTH'
        BUTTONS(5) = 'OVER-RIDE PIXEL SIZES'
        BUTTONS(6) = 'DEFAULT X-PIXEL SIZE'
        BUTTONS(7) = 'DEFAULT Y-PIXEL SIZE'
 
        TYPES(1) = Gs_logical
        TYPES(2) = Gs_real
        TYPES(3) = Gs_logical
        TYPES(4) = Gs_real
        TYPES(5) = Gs_logical
        TYPES(6) = Gs_real
        TYPES(7) = Gs_real
 
        TEXT(1) = 'OVER-RIDE SAMPLE TO DETECTOR DISTANCE ON INPUT'
        TEXT(2) = 'DEFAULT SAMPLE TO DETECTOR DISTANCE (MM)'
        TEXT(3) = 'OVER-RIDE WAVELENGTH ON INPUT'
        TEXT(4) = 'DEFAULT WAVELENGTH (ANGSTROMS)'
        TEXT(5) = 'OVER-RIDE PIXEL SIZES ON INPUT'
        TEXT(6) = 'DEFAULT SIZE OF HORIZONTAL PIXELS (MICRONS)'
        TEXT(7) = 'DEFAULT SIZE OF VERTICAL PIXELS (MICRONS)'
 
        FULL_PROMPTS(1) = 'Enter "YES" to over-ride sample to ' // &
          'detector distance when inputting'
        FULL_PROMPTS(2) = 'Enter default sample to detector ' // &
          'distance in millimetres'
        FULL_PROMPTS(3) = 'Enter "YES" to over-ride radiation ' // &
          'wavelength when inputting'
        FULL_PROMPTS(4) = 'Enter the default wavelength of the ' // &
          'radiation in Angstrom units'
        FULL_PROMPTS(5) = 'Enter "YES" to over-ride pixel ' // &
          'sizes when inputting'
        FULL_PROMPTS(6) = 'Enter default dimension of pixels ' // &
          'hortizontally, as displayed (microns)'
        FULL_PROMPTS(7) = 'Enter default dimension of pixels ' // &
          'vertically, as displayed (microns)'
 
        BOUND(2) = .True.
        BOUND(4) = .True.
        BOUND(6) = .False.
        BOUND(7) = .True.
 
        REALS_LOWER(2) = 0.001
        REALS_UPPER(2) = 10000.0
        REALS_LOWER(4) = 0.01
        REALS_UPPER(4) = 10000.0
        REALS_LOWER(6) = 0.01
        REALS_UPPER(6) = 10000.0
        REALS_LOWER(7) = 0.01
        REALS_UPPER(7) = 10000.0
 
        LOGICALS(1) = override_distance
        REALS(2) = default_distance * 1000.0
        LOGICALS(3) = override_wavelength
        REALS(4) = default_wavelength * 1.0e10
        LOGICALS(5) = override_pixel_sizes
        REALS(6) = default_x_pixel_size * 1.0e6
        REALS(7) = default_y_pixel_size * 1.0e6
 
!     Output interactive graphical form
        Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, Max_choices, &
          num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, dummy, &
          dummy, REALS_LOWER, REALS_UPPER, dummy, LOGICALS, REALS, STRINGS, &
          retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           status = St_escapevalue
           Return
        End If
 
!     Set resulting values
        override_distance = LOGICALS(1)
        default_distance = REALS(2) / 1000.0
        override_wavelength = LOGICALS(3)
        default_wavelength = REALS(4) / 1.0e10
        override_pixel_sizes = LOGICALS(5)
        default_x_pixel_size = REALS(6) / 1.0e6
        default_y_pixel_size = REALS(7) / 1.0e6
 
!     Set values in internal data-base
        Call IO_SET_LKEYVALUE ('OVERRIDE_WAVELENGTH', override_wavelength, &
          retstat, status)
        Call IO_SET_RKEYVALUE ('DEFAULT_WAVELENGTH', default_wavelength, &
          retstat, status)
        Call IO_SET_LKEYVALUE ('OVERRIDE_DISTANCE', override_distance, &
          retstat, status)
        Call IO_SET_RKEYVALUE ('DEFAULT_DETECTOR_DISTANCE', default_distance, &
          retstat, status)
        Call IO_SET_LKEYVALUE ('OVERRIDE_PIXEL_SIZES', override_pixel_sizes, &
          retstat, status)
        Call IO_SET_RKEYVALUE ('DEFAULT_X_PIXEL_SIZE', default_x_pixel_size, &
          retstat, status)
        Call IO_SET_RKEYVALUE ('DEFAULT_Y_PIXEL_SIZE', default_y_pixel_size, &
          retstat, status)
 
     End If
 
     End Subroutine F2D_DEF_GEOMETRY
!********1*********2*********3*********4*********5*********6*********7*********8
