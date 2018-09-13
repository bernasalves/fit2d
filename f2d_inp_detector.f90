!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_inp_detector.f90 *
!  *                      *
!  ************************
 
!+ F2D_INP_DETECTOR -  INPut DETECTOR parameters
     Subroutine F2D_INP_DETECTOR (experiment, status)
!  Description:
!    User input of detector detection depth and percentage attenuation. 
!  Keywords:
!    Detector.Depth, Detector.Attenuation, Depth.Detection~Layer, 
!    Attenuation.Detector
!  Method:
!    Uses "GS_FORM"
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    17-Apr-2013: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
     Integer, Parameter :: Max_chars =  48 ! Maximum number of characters
!      in a line
     Integer, Parameter :: Max_lines =   2 ! Number of lines in message
     Integer, Parameter :: Max_choices = 7 ! Maximum number of choices
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
     Data TX(   1) / 'HELP TEXT TO BE ADDED WHEN OPTION COMPLETE ' /
     Data TX(   2) / '(PRESENTLY UNDER DEVELOPMENT)'/
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_DETECTOR ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Get user preferences in data base
        Call IO_INQ_RKEYVALUE ('DETECTION_DEPTH', experiment%detection_depth, &
          retstat, status)
        Call IO_INQ_RKEYVALUE ('DETECTION_ATTENUATION', &
          experiment%detection_attenuation, retstat, status)

        PROMPT(1) = 'CONTROL OF DETECTOR PARALLAX'
        PROMPT(2) = 'PARAMETERS'
        BUTTONS(1) = 'DETECTION DEPTH'
        BUTTONS(2) = 'DET. ATTENUATION'

        TYPES(1) = Gs_real
        TYPES(2) = Gs_real

        TEXT(1) = 'DETECTION LAYER DEPTH (Microns)'
        TEXT(2) = 'DETECTION ATTENUATION (%)'
        FULL_PROMPTS(1) = 'Enter the depth (thickness of the detection layer)'
        FULL_PROMPTS(2) = 'Enter the percentage attenuation of the ' // &
          'detection layer'
        BOUND(1) = .True.
        BOUND(2) = .True.
 
        REALS_LOWER(1) = 0.00001
        REALS_UPPER(1) = 1.0e6
        REALS(1) = experiment%detection_depth * 1.0e6
        REALS_LOWER(2) = 0.00001
        REALS_UPPER(2) = 100.0
        REALS(2) = experiment%detection_attenuation * 100.0

!     Output interactive graphical form
        Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, Max_choices, &
          2, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, INTS_LOWER, &
          INTS_UPPER, REALS_LOWER, REALS_UPPER, INTEGERS, LOGICALS, REALS, &
          STRINGS, retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           status = St_escapevalue
           Return
        End If
 
!     Set resulting values
        experiment%detection_depth = REALS(1) / 1.0e6
        experiment%detection_attenuation = REALS(2) / 100.0

!     set user preferences in data base
        Call IO_SET_RKEYVALUE ('DETECTION_DEPTH', experiment%detection_depth, &
          retstat, status)
        Call IO_SET_RKEYVALUE ('DETECTION_ATTENUATION', &
          experiment%detection_attenuation, retstat, status)

     End If

     End Subroutine F2D_INP_DETECTOR
!********1*********2*********3*********4*********5*********6*********7*********8
