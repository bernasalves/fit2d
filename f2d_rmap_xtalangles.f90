!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************************
!  *                         *
!  * f2d_rmap_xtalangles.f90 *
!  *                         *
!  ***************************
 
!+ F2D_RMAP_XTALANGLES: Reciprocal MAP crystal ANGLES form
     Subroutine F2D_RMAP_XTALANGLES (experiment, &
       start_chi, start_phi, start_omega, start_2theta, delta_chi, delta_phi, &
       delta_omega, delta_2theta, status)
!  Description:
!    User input of crystal setting angles for the sequence
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    22-Jun-2006: V0.8 Use "experiment" structure (Hammersley)
!    10-Mar-2006: V0.7 Angles now in Double Precision (Hammersley)
!    28-Oct-2005: V0.6 Add option to mirror image (Hammersley)
!    25-Oct-2005: V0.5 Change the way detector horizontal or vertical
!      position is handled (Hammersley)
!    08-Jun-2005: V0.4 Add detector 2-theta angles horizontal as
!      well as vertical (Hammersley)
!    13-May-2005: V0.3 Convert all angles internally to radians (Hammersley)
!    11-May-2005: V0.2 Add control of detector 2-theta angles (Hammersley)
!    26-Apr-2005: V0.1 Original (Hammersley)
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
     Double Precision, Intent(INOUT) :: start_chi ! Chi angle for first 
!      diffraction image
     Double Precision, Intent(INOUT) :: start_phi ! Phi angle for first 
!      diffraction image
     Double Precision, Intent(INOUT) :: start_omega ! Omega angle for first
!      diffraction image
     Double Precision, Intent(INOUT) :: start_2theta ! Detector 2-theta angle 
!      for first diffraction image
     Double Precision, Intent(INOUT) :: delta_chi ! Change in Chi angle for each
!      diffraction image
     Double Precision, Intent(INOUT) :: delta_phi ! Change in Phi angle for each
!      diffraction image
     Double Precision, Intent(INOUT) :: delta_omega ! Change in Omega angle for 
!      each diffraction image
     Double Precision, Intent(INOUT) :: delta_2theta ! Change in detector 
!      2-theta angle for each diffraction image
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
     Integer, Parameter :: Max_chars = 43 ! Maximum number of characters in
!      a line
     Integer, Parameter :: Max_lines = 13 ! Number of lines in message
     Integer, Parameter :: Max_choices = 18 ! Maximum number of choices:
!      Note: Must be at least 5 greater than 'num_choices' to allow for general
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
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to 'GS_FORM'
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = Max_chars) :: TX(Max_lines) ! Text array
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      Gs_real = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
!      Gs_double = Double precision real
     Logical :: BOUND(Max_choices) ! .True., if variable range is bound
     Logical :: LOGICALS(Max_choices) ! Logical variables to be input
     Double Precision :: DOUBLES(Max_choices) ! Double precision variables to 
!      be input
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
     Data TX(   7) / 'You can set the initial angles and'/
     Data TX(   8) / 'the difference between images.'/
     Data TX(   9) / ' ' /
     Data TX(  10) / '---------------'/
     Data TX(  11) / 'END OF HELP TEXT'/
     Data TX(  12) / '---------------'/
     Data TX(  13) / ' ' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_XTALANGLES ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
 
!     Try to get default values from internal data-base
        Call IO_INQ_DKEYVALUE ('START_CHI', start_chi, retstat, status)
        Call IO_INQ_DKEYVALUE ('START_PHI', start_phi, retstat, status)
        Call IO_INQ_DKEYVALUE ('START_OMEGA', start_omega, retstat, status)
        Call IO_INQ_DKEYVALUE ('START_2THETA', start_2theta, retstat, status)
        Call IO_INQ_DKEYVALUE ('DELTA_CHI', delta_chi, retstat, status)
        Call IO_INQ_DKEYVALUE ('DELTA_PHI', delta_phi, retstat, status)
        Call IO_INQ_DKEYVALUE ('DELTA_OMEGA', delta_omega, retstat, status)
        Call IO_INQ_DKEYVALUE ('DELTA_2THETA', delta_2theta, retstat, status)
 
        num_choices = 10
        PROMPT(1) = 'CRYSTAL/DETECTOR SETTING'
        PROMPT(2) = 'ANGLES CONTROL FORM'

        BUTTONS(1) = 'VIEW FROM SAMPLE'
        BUTTONS(2) = 'VERT. 2-TH'
        BUTTONS(3) = 'START CHI'
        BUTTONS(4) = 'START PHI'
        BUTTONS(5) = 'START OMEGA'
        BUTTONS(6) = 'START 2-THETA'
        BUTTONS(7) = 'DELTA CHI'
        BUTTONS(8) = 'DELTA PHI'
        BUTTONS(9) = 'DELTA OMEGA'
        BUTTONS(10) = 'DELTA 2-THETA'
 
        TYPES(1) = Gs_logical
        TYPES(2) = Gs_logical
        TYPES(3) = Gs_double
        TYPES(4) = Gs_double
        TYPES(5) = Gs_double
        TYPES(6) = Gs_double
        TYPES(7) = Gs_double
        TYPES(8) = Gs_double
        TYPES(9) = Gs_double
        TYPES(10) = Gs_double
 
        TEXT(1) = 'Is image view from sample to detector'
        TEXT(2) = 'Vertical detector rotation'
        TEXT(3) = 'Chi angle (degrees) of crystal for first image'
        TEXT(4) = 'Phi angle (degrees) of crystal for first image'
        TEXT(5) = 'Omega angle (degrees) of crystal for first image'
        TEXT(6) = '2-theta angle (degrees) of detector for first image'
        TEXT(7) = 'Difference in Chi angle (degrees) between each image'
        TEXT(8) = 'Difference in Phi angle (degrees) between each image'
        TEXT(9) = 'Difference in Omega angle (degrees) between each image'
        TEXT(10) = 'Difference in detector 2-theta ' // &
          'angle (degrees) between each image'
 
        FULL_PROMPTS(1) = 'YES: if looking from sample to detector ' // &
          'NO: if view is from behind the detector'
        FULL_PROMPTS(2) = '"YES" if the detector rotates vertically'
        FULL_PROMPTS(3) = &
          'Enter Chi angle (degrees) of crystal for first image'
        FULL_PROMPTS(4) = &
          'Enter Phi angle (degrees) of crystal for first image'
        FULL_PROMPTS(5) = &
          'Enter Omega angle (degrees) of crystal for first image'
        FULL_PROMPTS(6) = 'Enter detector 2-theta ' // &
          'angle (degrees) for first image'
        FULL_PROMPTS(7) = 'Enter difference in Chi angle ' // &
          '(degrees) between each image'
        FULL_PROMPTS(8) = 'Enter difference in Phi angle ' // &
          '(degrees) between each image'
        FULL_PROMPTS(9) = 'Enter difference in Omega angle ' // &
          '(degrees) between each image'
        FULL_PROMPTS(10) = 'Enter difference in ' // &
          'detector 2-theta angle (degrees) between each image'
 
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
 
        LOGICALS(1) = experiment%view_from_sample 
        LOGICALS(2) = experiment%vertical_2theta
        DOUBLES(3) = start_chi * 180.0 / Pi_d
        DOUBLES(4) = start_phi * 180.0 / Pi_d
        DOUBLES(5) = start_omega * 180.0 / Pi_d
        DOUBLES(6) = start_2theta * 180.0 / Pi_d
        DOUBLES(7) = delta_chi * 180.0 / Pi_d
        DOUBLES(8) = delta_phi * 180.0 / Pi_d
        DOUBLES(9) = delta_omega * 180.0 / Pi_d
        DOUBLES(10) = delta_2theta * 180.0 / Pi_d
 
        DOUBLES_LOWER(3) = -180.0
        DOUBLES_UPPER(3) = 180.0
        DOUBLES_LOWER(4) = -180.0
        DOUBLES_UPPER(4) = 180.0
        DOUBLES_LOWER(5) = -180.0
        DOUBLES_UPPER(5) = 180.0
        DOUBLES_LOWER(6) = -180.0
        DOUBLES_UPPER(6) = 180.0
        DOUBLES_LOWER(7) = -180.0
        DOUBLES_UPPER(7) = 180.0
        DOUBLES_LOWER(8) = -180.0
        DOUBLES_UPPER(8) = 180.0
        DOUBLES_LOWER(9) = -180.0
        DOUBLES_UPPER(9) = 180.0
        DOUBLES_LOWER(10) = -180.0
        DOUBLES_UPPER(10) = 180.0
 
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
        experiment%view_from_sample  = LOGICALS(1)
        experiment%vertical_2theta = LOGICALS(2)
        start_chi = DOUBLES(3) * Pi_d / 180.0
        start_phi = DOUBLES(4) * Pi_d / 180.0
        start_omega = DOUBLES(5) * Pi_d / 180.0
        start_2theta = DOUBLES(6) * Pi_d / 180.0
        delta_chi = DOUBLES(7) * Pi_d / 180.0
        delta_phi = DOUBLES(8) * Pi_d / 180.0
        delta_omega = DOUBLES(9) * Pi_d / 180.0
        delta_2theta = DOUBLES(10) * Pi_d / 180.0
 
!     Save values in internal data-base
        Call IO_SET_DKEYVALUE ('START_CHI', start_chi, retstat, status)
        Call IO_SET_DKEYVALUE ('START_PHI', start_phi, retstat, status)
        Call IO_SET_DKEYVALUE ('START_OMEGA', start_omega, retstat, status)
        Call IO_SET_DKEYVALUE ('START_2THETA', start_2theta, retstat, status)
        Call IO_SET_DKEYVALUE ('DELTA_CHI', delta_chi, retstat, status)
        Call IO_SET_DKEYVALUE ('DELTA_PHI', delta_phi, retstat, status)
        Call IO_SET_DKEYVALUE ('DELTA_OMEGA', delta_omega, retstat, status)
        Call IO_SET_DKEYVALUE ('DELTA_2THETA', delta_2theta, retstat, status)
 
     End If
 
     End Subroutine F2D_RMAP_XTALANGLES
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
