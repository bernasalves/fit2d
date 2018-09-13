!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     * 
!  * f2d_rmap_angles.f90 *
!  *                     *
!  ***********************
 
!+ F2D_RMAP_ANGLES: diffractometer ANGLES form
     Subroutine F2D_RMAP_ANGLES (EXPERIMENT, status)
!  Description:
!    User input of crystal setting angles for the sequence
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Mar-2006: V0.4 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    03-Nov-2005: V0.3 Option to mirror diffraction image (Hammersley)
!    27-Oct-2005: V0.2 Change the way detector horizontal or vertical position 
!      is handled for coordinate input (Hammersley)
!    17-Oct-2005: V0.1 Original (Hammersley)
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
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
     Integer, Parameter :: Max_chars = 43 ! Maximum number of characters in
!      a line
     Integer, Parameter :: Max_lines = 13 ! Number of lines in message
     Integer, Parameter :: Max_choices = 18 ! Maximum number of choices:
!      Note: Must be of at least 5 more than "num_choices" to allow for general
!      form buttons
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: dummy ! Dummy variable, not used
     Integer :: i ! Loop variable for dimensions
     Integer :: num_choices ! Number of choices
     Integer :: retstat ! Return status variable
     Logical :: finished ! .True., when the input is finished O.K.
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 60) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
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
     Logical :: LOGICALS(Max_choices) ! Logical variables to be input
     Double Precision :: DOUBLES(Max_choices) ! Double precision variables
     Double Precision :: DOUBLES_LOWER(Max_choices) ! Lower bound of double 
!      precision variables
     Double Precision :: DOUBLES_UPPER(Max_choices) ! Upper bound of double 
!      precision variables
!  External Functions:
!  Local Data:
     Data TX(   1) / ' ' /
     Data TX(   2) / '      -------------------------------'/
     Data TX(   3) / '      Welcome to the FIT2D Reciprocal'/
     Data TX(   4) / '      Map Crystal Setting Angles form'/
     Data TX(   5) / '      -------------------------------'/
     Data TX(   6) / ' ' /
     Data TX(   7) / 'You can set the diffractometer angles '/
     Data TX(   8) / 'corresponding to an image.'/
     Data TX(   9) / ' ' /
     Data TX(  10) / '---------------'/
     Data TX(  11) / 'END OF HELP TEXT'/
     Data TX(  12) / '---------------'/
     Data TX(  13) / ' ' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_ANGLES ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
 
        num_choices = 6
        PROMPT(1) = 'CRYSTAL/DETECTOR SETTING'
        PROMPT(2) = 'ANGLES CONTROL FORM'
 
        BUTTONS(1) = 'CHI'
        BUTTONS(2) = 'PHI'
        BUTTONS(3) = 'OMEGA'
        BUTTONS(4) = 'VERT. 2-TH'
        BUTTONS(5) = '2-THETA'
        BUTTONS(6) = 'VIEW FROM SAMPLE'
 
        TYPES(1) = Gs_double
        TYPES(2) = Gs_double
        TYPES(3) = Gs_double
        TYPES(4) = Gs_logical
        TYPES(5) = Gs_double
        TYPES(6) = Gs_logical
 
        TEXT(1) = 'Chi angle (degrees) of crystal for image'
        TEXT(2) = 'Phi angle (degrees) of crystal for image'
        TEXT(3) = 'Omega angle (degrees) of crystal for image'
        TEXT(4) = 'Vertical detector rotation'
        TEXT(5) = 'Two-theta angle (degrees) of detector for image'
        TEXT(6) = 'Is image view from sample to detector'
 
        FULL_PROMPTS(1) = 'Enter Chi angle (degrees) of crystal for image'
        FULL_PROMPTS(2) = 'Enter Phi angle (degrees) of crystal for image'
        FULL_PROMPTS(3) = 'Enter Omega angle (degrees) of crystal for image'
        FULL_PROMPTS(4) = '"YES" if the detector rotates vertically'
        FULL_PROMPTS(5) = 'Enter detector 2-theta angle (degrees) for image'
        FULL_PROMPTS(6) = 'YES: if looking from sample to detector ' // &
          'NO: if view is from behind the detector'
 
        BOUND(1) = .True.
        BOUND(2) = .True.
        BOUND(3) = .True.
        BOUND(4) = .True.
        BOUND(5) = .True.
        BOUND(6) = .True.
 
        DOUBLES(1) = experiment%chi_start * 180.0d0 / Pi_d
        DOUBLES(2) = experiment%phi_start * 180.0d0 / Pi_d
        DOUBLES(3) = experiment%omega_start * 180.0d0 / Pi_d
        LOGICALS(4) = experiment%vertical_2theta
         DOUBLES(5) = experiment%two_theta * 180.0d0 / Pi_d
        LOGICALS(6) = experiment%view_from_sample 
 
        DOUBLES_LOWER(1) = -360.0
        DOUBLES_UPPER(1) = 360.0
        DOUBLES_LOWER(2) = -360.0
        DOUBLES_UPPER(2) = 360.0
        DOUBLES_LOWER(3) = -360.0
        DOUBLES_UPPER(3) = 360.0
        DOUBLES_LOWER(5) = -360.0
        DOUBLES_UPPER(5) = 360.0
 
!     Output interactive graphical form
        Call GS_FORM_D (2, 2, PROMPT, Max_lines, Max_lines, TX, Max_choices, &
          num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, dummy, &
          dummy, DOUBLES_LOWER, DOUBLES_UPPER, dummy, LOGICALS, DOUBLES, &
          STRINGS, retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           status = St_escapevalue
           Return
        End If
 
!     Set resulting values
        experiment%chi_start = DOUBLES(1) * Pi_d / 180.0d0
        experiment%phi_start = DOUBLES(2) * Pi_d / 180.0d0
        experiment%omega_start = DOUBLES(3) * Pi_d / 180.0d0
        experiment%vertical_2theta = LOGICALS(4)
        experiment%two_theta = DOUBLES(5) * Pi_d / 180.0d0
        experiment%view_from_sample  = LOGICALS(6)

!     Set values set
        experiment%goniometer_set = .True.
        experiment%two_theta_set = .True.
        experiment%image_orientation_set = .True.

     End If
 
     End Subroutine F2D_RMAP_ANGLES
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
