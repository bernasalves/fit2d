!********1*********2*********3*********4*********5*********6*********7**
 
!  **************************
!  *                        *
!  * f2d_gui_reflection.f90 *
!  *                        *
!  **************************
 
!+ F2D_GUI_REFLECTION: Reciprocal MAP input UB MATRIX
     Subroutine F2D_GUI_REFLECTION (image, num_reflection, prompt, h, k, l, &
       EXPERIMENT, status)
!  Description:
!    Input indices and diffractometer angles of a reflection.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    20-Jun-2006: V0.6 Add option of questions for image input (Hammersley)
!    24-Apr-2006: V0.5 Set start and end diffractometer angles (Hammersley)
!    19-Apr-2006: V0.4 Use "EXPERIMENT" structure (Hammersley)
!    14-Oct-2005: V0.3 Save values in data-base (Hammersley)
!    10-Oct-2005: V0.2 Allow negative indices (Hammersley)
!    10-Jun-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: image ! .True., if input ofr image data
     Integer, Intent(INOUT) :: num_reflection ! Number of reflection to input
     Character(Len = *), Intent(IN) :: prompt ! User prompt for input form
!  Import/Export:
     Integer, Intent(INOUT) :: h ! H index of reflection
     Integer, Intent(INOUT) :: k ! K index of reflection
     Integer, Intent(INOUT) :: l ! L index of reflection
     TYPE(EXPERIMENTAL_DETAILS), Intent(OUT) :: EXPERIMENT ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
     Integer, Parameter :: Max_chars = 43 ! Maximum number of characters in
!      a line
     Integer, Parameter :: Max_lines = 13 ! Number of lines in message
     Integer, Parameter :: Max_choices = 14 ! Maximum number of choices:
!      Note: Must of at least 5 more than "num_choices" to allow for general
!      form buttons
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Double Precision :: value ! Dummy variable for functions
     Integer :: dummy ! Dummy variable, not used
     Integer :: i ! Loop variable for dimensions
     Integer :: num_choices ! Number of choices
     Integer :: retstat ! Return status variable
     Logical :: finished ! .True., when the input is finished O.K.
!  Local Arrays:
     Character(Len = 1) :: reflection ! Reflection 1 or 2
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 70) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = Max_chars) :: TX(Max_lines) ! Text array
     Integer :: INTEGERS(Max_choices) ! Integer variables to input
     Integer :: INTS_LOWER(Max_choices) ! Lower bounds of integer variables
     Integer :: INTS_UPPER(Max_choices) ! Upper bounds of integer variables
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
     Logical :: BOUND(Max_choices) ! .True., if variable range is bound
     Logical :: LOGICALS(Max_choices) ! Logical input choices
     Double Precision :: DOUBLES(Max_choices) ! Double Precision variables 
     Double Precision :: DOUBLES_LOWER(Max_choices) ! Lower bound of variables
     Double Precision :: DOUBLES_UPPER(Max_choices) ! Upper bound of variables
!  Internal Functions:
     Double Precision :: Degrees
     Degrees(value) = value * 180.0 / Pi_d
     Double Precision :: Radians
     Radians(value) = value * Pi_d / 180.0
!  External Functions:
!  Local Data:
     Data TX(   1) / ' ' /
     Data TX(   2) / '      -------------------------------'/
     Data TX(   3) / '      Welcome to the FIT2D Reflection'/
     Data TX(   4) / '      Parameter Input Form'/
     Data TX(   5) / '      -------------------------------'/
     Data TX(   6) / ' ' /
     Data TX(   7) / 'You can set the indices and diffractometer'/
     Data TX(   8) / 'angles of a reflection.'/
     Data TX(   9) / ' ' /
     Data TX(  10) / '---------------'/
     Data TX(  11) / 'END OF HELP TEXT'/
     Data TX(  12) / '---------------'/
     Data TX(  13) / ' ' /
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_REFLECTION ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
 
        If (num_reflection .Eq. 1) Then
           reflection = '1'
        Else
           reflection = '2'
        End If
 
!     Try to get default values from internal data-base
        Call IO_INQ_IKEYVALUE ('H_' // reflection, h, retstat, status)
        Call IO_INQ_IKEYVALUE ('K_' // reflection, k, retstat, status)
        Call IO_INQ_IKEYVALUE ('L_' // reflection, l, retstat, status)
        Call IO_INQ_DKEYVALUE ('TWO_THETA_' // reflection, &
          EXPERIMENT%two_theta, retstat, status)
        If (retstat .Eq. 0) Then
           EXPERIMENT%two_theta = Radians(EXPERIMENT%two_theta)
        End If
        Call IO_INQ_DKEYVALUE ('PHI_' // reflection, &
          EXPERIMENT%phi_start, retstat, status)
        If (retstat .Eq. 0) Then
           EXPERIMENT%phi_start = Radians(EXPERIMENT%phi_start)
        End If
        Call IO_INQ_DKEYVALUE ('CHI_' // reflection, &
          EXPERIMENT%chi_start, retstat, status)
        If (retstat .Eq. 0) Then
           EXPERIMENT%chi_start = Radians(EXPERIMENT%chi_start)
        End If
        Call IO_INQ_DKEYVALUE ('OMEGA_' // reflection, &
          EXPERIMENT%omega_start, retstat, status)
        If (retstat .Eq. 0) Then
           EXPERIMENT%omega_start = Radians(EXPERIMENT%omega_start)
        End If

        If (image) Then
           num_choices = 9
        Else
           num_choices = 7
        End If

        BUTTONS(1) = 'h'
        BUTTONS(2) = 'k'
        BUTTONS(3) = 'l'
        BUTTONS(4) = '2-Theta'
        BUTTONS(5) = 'PHI'
        BUTTONS(6) = 'CHI'
        BUTTONS(7) = 'OMEGA'
        BUTTONS(8) = 'VERT. 2-TH'
        BUTTONS(9) = 'VIEW FROM SAMPLE'
 
        TYPES(1) = Gs_integer
        TYPES(2) = Gs_integer
        TYPES(3) = Gs_integer
        TYPES(4) = Gs_double
        TYPES(5) = Gs_double
        TYPES(6) = Gs_double
        TYPES(7) = Gs_double
        TYPES(8) = Gs_logical
        TYPES(9) = Gs_logical
 
        TEXT(1) = 'h index of reflection'
        TEXT(2) = 'k index of reflection'
        TEXT(3) = 'l index of reflection'
        If (image) Then
           TEXT(4) = '2-theta angle (Degrees) of detector'
        Else
           TEXT(4) = '2-theta angle (Degrees) of reflection'
        End If
        TEXT(5) = 'phi angle (Degrees) of diffractometer'
        TEXT(6) = 'chi angle (Degrees) of diffractometer'
        TEXT(7) = 'omega angle (Degrees) of diffractometer'
        TEXT(8) = 'Vertical detector rotation'
        TEXT(9) = 'Image viewed from sample'
 
        FULL_PROMPTS(1) = 'Enter h index of reflection'
        FULL_PROMPTS(2) = 'Enter k index of reflection'
        FULL_PROMPTS(3) = 'Enter l index of reflection'
        If (image) Then
           FULL_PROMPTS(4) = 'Enter 2-theta angle (Degrees) of detector'
        Else
           FULL_PROMPTS(4) = 'Enter 2-theta angle (Degrees) of reflection'
        End If
        FULL_PROMPTS(5) = 'Enter phi angle (Degrees) of diffractometer'
        FULL_PROMPTS(6) = 'Enter chi angle (Degrees) of diffractometer'
        FULL_PROMPTS(7) = 'Enter omega angle (Degrees) of diffractometer'
        FULL_PROMPTS(8) = '"YES" if image is viewed from the sample; ' // &
          '"NO" behind detector'
        FULL_PROMPTS(8) = '"YES" if the detector rotates vertically'
        FULL_PROMPTS(9) = '"YES": if looking from sample to detector ' // &
          'NO: if view is from behind the detector'
 
        BOUND(1) = .True.
        BOUND(2) = .True.
        BOUND(3) = .True.
        BOUND(4) = .True.
        BOUND(5) = .True.
        BOUND(6) = .True.
        BOUND(7) = .True.
        BOUND(8) = .True.
        BOUND(9) = .True.
 
        INTEGERS(1) = h
        INTEGERS(2) = k
        INTEGERS(3) = l
        DOUBLES(4) = Degrees(EXPERIMENT%two_theta)
        DOUBLES(5) = Degrees(EXPERIMENT%phi_start)
        DOUBLES(6) = Degrees(EXPERIMENT%chi_start)
        DOUBLES(7) = Degrees(EXPERIMENT%omega_start)
        LOGICALS(8) = EXPERIMENT%vertical_2theta
        LOGICALS(9) = EXPERIMENT%view_from_sample 
 
        INTS_LOWER(1) = -100
        INTS_UPPER(1) = 100
        INTS_LOWER(2) = -100
        INTS_UPPER(2) = 100
        INTS_LOWER(3) = -100
        INTS_UPPER(3) = 100
        DOUBLES_LOWER(4) = -360.0
        DOUBLES_UPPER(4) = 360.0
        DOUBLES_LOWER(5) = -360.0
        DOUBLES_UPPER(5) = 360.0
        DOUBLES_LOWER(6) = -360.0
        DOUBLES_UPPER(6) = 360.0
        DOUBLES_LOWER(7) = -360.0
        DOUBLES_UPPER(7) = 360.0

!     Output interactive graphical form
        Call GS_FORM_D (1, 1, prompt, Max_lines, Max_lines, TX, &
         max_choices, num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, &
         INTS_LOWER, INTS_UPPER, DOUBLES_LOWER, DOUBLES_UPPER, INTEGERS, &
         LOGICALS, DOUBLES, STRINGS, retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           status = St_escapevalue
           Return
        End If
 
!     Set resulting values
        h = INTEGERS(1)
        k = INTEGERS(2)
        l = INTEGERS(3)
        EXPERIMENT%goniometer_set = .True.
        EXPERIMENT%two_theta = Radians(DOUBLES(4))
        EXPERIMENT%phi_start = Radians(DOUBLES(5))
        EXPERIMENT%chi_start = Radians(DOUBLES(6))
        EXPERIMENT%omega_start = Radians(DOUBLES(7))
        EXPERIMENT%vertical_2theta = LOGICALS(8)
        EXPERIMENT%two_theta_set = .True.
        EXPERIMENT%view_from_sample = LOGICALS(9)
        EXPERIMENT%image_orientation_set = .True.
        EXPERIMENT%phi_end = EXPERIMENT%phi_start
        EXPERIMENT%chi_end = EXPERIMENT%chi_start
        EXPERIMENT%omega_end = EXPERIMENT%omega_start
 
!     Set values in internal data-base
        Call IO_SET_IKEYVALUE ('H_' // reflection, h, retstat, status)
        Call IO_SET_IKEYVALUE ('K_' // reflection, k, retstat, status)
        Call IO_SET_IKEYVALUE ('L_' // reflection, l, retstat, status)
        Call IO_SET_DKEYVALUE ('TWO_THETA_' // reflection, &
          Degrees(EXPERIMENT%two_theta), retstat, status)
        Call IO_SET_DKEYVALUE ('PHI_' // reflection, &
          Degrees(EXPERIMENT%phi_start), retstat, status)
        Call IO_SET_DKEYVALUE ('CHI_' // reflection, &
          Degrees(EXPERIMENT%chi_start), retstat, status)
        Call IO_SET_DKEYVALUE ('OMEGA_' // reflection, &
          Degrees(EXPERIMENT%omega_start), retstat, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''EXPERIMENT%view_from_sample = '', l1)') &
!          EXPERIMENT%view_from_sample
!        Write (*, '(''EXPERIMENT%vertical_2theta = '', l1)') &
!          EXPERIMENT%vertical_2theta
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End If
 
     End Subroutine F2D_GUI_REFLECTION
!********1*********2*********3*********4*********5*********6*********7**
 
 
