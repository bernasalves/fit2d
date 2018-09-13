!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************************
!  *                           *
!  * f2d_inp_id06_azimuths.f90 *
!  *                           *
!  *****************************
 
!+ F2D_INP_ID06_AZIMUTHS -  INPut data AZIMUTH limits
     Subroutine F2D_INP_ID06_AZIMUTHS (azimuth_start, azimuth_end, status)
!  Description:
!    User input of data azimuthal limits 
!  Keywords:
!    Azimuth.Limits, Start.Azimuth, End.Azimuth
!  Method:
!    Uses "GS_FORM"
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    29-Apr-2014: V0.2 Increase size of text arrays (Hammersley)
!    01-Apr-2014: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
!  Import/Export:
     Real, Intent(INOUT) :: azimuth_start ! Azimuth for start of data
     Real, Intent(INOUT) :: azimuth_end ! Azimuth for end of data
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
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
     Character(Len = 25) :: BUTTONS(Max_choices) ! Choice buttons
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
     Data TX(   1) / 'Enter the start and end azimuths corresponding to' /
     Data TX(   2) / 'the full data region input from file or files'/
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_ID06_AZIMUTHS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        PROMPT(1) = 'INPUT OF FULL DATA REGION'
        PROMPT(2) = 'STARTING AND END AZIMUTH VALUES'
        BUTTONS(1) = 'START (DEGREES)'
        BUTTONS(2) = 'END (DEGREES)'

        TYPES(1) = Gs_real
        TYPES(2) = Gs_real

        TEXT(1) = 'AZIMUTH AT START OF FULL DATA REGION (DEGREES)'
        TEXT(2) = 'AZIMUTH AT END OF FULL DATA REGION (DEGREES)'
        FULL_PROMPTS(1) = 'Enter azimuth at start of full data region (degrees)'
        FULL_PROMPTS(2) = 'Enter azimuth at end of full data region (degrees)'
        BOUND(1) = .True.
        BOUND(2) = .True.
 
        REALS_LOWER(1) = 0.0
        REALS_UPPER(1) = 360.0
        REALS(1) = azimuth_start * 180.0 / Pi
        REALS_LOWER(2) = 0.0
        REALS_UPPER(2) = 360.0
        REALS(2) = azimuth_end * 180.0 / Pi

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
        azimuth_start = REALS(1) * Pi / 180.0
        azimuth_end = REALS(2) * Pi /180.0

     End If

     End Subroutine F2D_INP_ID06_AZIMUTHS
!********1*********2*********3*********4*********5*********6*********7*********8
