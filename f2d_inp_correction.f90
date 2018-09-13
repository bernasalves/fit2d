!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_inp_correction.f90 *
!  *                        *
!  **************************
 
!+ F2D_INP_CORRECTION - FIT 2-D INPut detector distortion CORRECTION parameters
     Subroutine F2D_INP_CORRECTION (dc_correction, dc_file, ff_correction, &
       ff_file, ff_scale, ff_scaler, sd_correction, sd_file, status)
!  Description:
!    Interactive form to set-up detector distortion correction parameters
!  Keywords:
!    Detector~Distortion.Control~Parameters
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    07-Apr-2004: V0.2 Add option of correcting dark current (Hammersley)
!    23-Feb-1998: V0.1 Original, based on "f2d_gui_correction" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import/Export:
     Logical, Intent(INOUT) :: dc_correction ! .True., if a dark current image 
!      is to be subtracted
     Character(Len = *), Intent(INOUT) :: dc_file ! Name of dark current file
     Logical, Intent(INOUT) :: ff_correction ! .True., if a flat-field
!      correction is to be applied
     Character(Len = *), Intent(INOUT) :: ff_file ! Name of flat-field file
     Logical, Intent(INOUT) :: ff_scale ! .True., if the flat-fielded image is 
!      to scaled
     Real, Intent(INOUT) :: ff_scaler ! Scaling to multiple flat-field image
!      with
     Logical, Intent(INOUT) :: sd_correction ! .True., if a spatial distortion
!      correction is to be applied
     Character(Len = *), Intent(INOUT) :: sd_file ! Name of spatial distortion 
!      file
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
     Integer, Parameter :: Max_choices = 13 ! Maximum number of choices
!      (need 5 extra spaces)
     Integer, Parameter :: Max_prompt = 6 ! Dimension size of prompts
!  Local Variables:
     Integer :: dummy ! Dummy subroutine argument
     Integer :: num_choices ! Number of choices in form
     Integer :: retstat ! Return status variable:
!      0 = good status
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 70) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
     Character(Len = 132) :: STRINGS(Max_choices) ! String variables to be input
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = 60) :: TX(10) ! Text array
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
     Logical :: BOUND(Max_choices) ! .True., if variable range is bound
     Logical :: LOGICALS(Max_choices) ! Logical variables to be input
     Real :: REALS(Max_choices) ! Real variables to be input
     Real :: REALS_LOWER(Max_choices) ! Real variables lower limits
     Real :: REALS_UPPER(Max_choices) ! Real variables upper limits
!  External Functions:
!  Local Data:
!    Save:
!--------1---------2---------3---------4---------5---------6---------7---------8
 !  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_CORRECTION ' // Version)
        Return
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_INP_CORRECTION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Arguments would appear to be reasonable, go ahead.
 
!     Allow user of choose number of output bins and control various other 
!     options
        num_choices = 8
        PROMPT(1) = 'CONTROL OF DETECTOR'
        PROMPT(2) = 'DISTORTION CORRECTIONS'
        BUTTONS(1) = 'DARK CURRENT'
        BUTTONS(2) = 'DC FILE'
        BUTTONS(3) = 'FLAT-FIELD'
        BUTTONS(4) = 'FF FILE'
        BUTTONS(5) = 'FF SCALE'
        BUTTONS(6) = 'FF MULTIPLIER'
        BUTTONS(7) = 'SPATIAL DIS.'
        BUTTONS(8) = 'SD FILE'
 
        TYPES(1) = Gs_logical
        TYPES(2) = Gs_input_file
        TYPES(3) = Gs_logical
        TYPES(4) = Gs_input_file
        TYPES(5) = Gs_logical
        TYPES(6) = Gs_real
        TYPES(7) = Gs_logical
        TYPES(8) = Gs_input_file
 
        BOUND(6) = .True.
        REALS_LOWER(6) = 0.0001
        REALS_UPPER(6) = 1.0e16
 
        TEXT(1) = 'SUBTRACT DARK CURRENT IMAGE'
        TEXT(2) = 'NAME OF DARK CURRENT FILE'
        TEXT(3) = 'APPLY FLAT FIELD CORRECTION'
        TEXT(4) = 'NAME OF FLAT-FIELD FILE'
        TEXT(5) = 'APPLY SCALING AFTER FLAT FIELD CORRECTION'
        TEXT(6) = 'FLAT FIELD MULTIPLIER TO APPLY'
        TEXT(7) = 'APPLY SPATIAL DISTORTION CORRECTION'
        TEXT(8) = 'NAME OF SPATIAL DISTORTION FILE'
        FULL_PROMPTS(1) = 'Enter "YES" to input a dark current' // &
          'image and subtract from the data image'
        FULL_PROMPTS(2) = 'Select file containing the dark ' // &
          'current image for subtraction'
        FULL_PROMPTS(3) = 'Enter "YES" to input a flat field' // &
          'image and apply a correction'
        FULL_PROMPTS(4) = 'Select file containing the flat-field' // &
          'image for correction'
        FULL_PROMPTS(5) = 'Enter "YES" to multiply corrected' // &
          'image by the user controlled scale factor'
        FULL_PROMPTS(6) = 'Scale factor to multiple image' // &
          'after flat-field correction'
        FULL_PROMPTS(7) = 'Enter "YES" to input a spatial ' // &
          'distortion defining function and apply correction'
        FULL_PROMPTS(8) = 'Select file containing the spatial ' // &
          'distortion defining function'
 
        LOGICALS(1) = dc_correction
        STRINGS(2) = dc_file
        LOGICALS(3) = ff_correction
        STRINGS(4) = ff_file
        LOGICALS(5) = ff_scale
        REALS(6) = ff_scaler
        LOGICALS(7) = sd_correction
        STRINGS(8) = sd_file
 
!     Output interactive graphical form
        TX(1) = 'Control of dark current, flat-field, and '
        TX(2) = 'spatial distortion correction'
        Call GS_FORM (2, 2, PROMPT, 2, 2, TX, Max_choices, num_choices, &
          BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, dummy, dummy, &
          REALS_LOWER, REALS_UPPER, dummy, LOGICALS, REALS, STRINGS, retstat, &
          status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           status = St_escapevalue
           Return
        End If
 
!     Set resulting values
        dc_correction = LOGICALS(1)
        dc_file = STRINGS(2)
        ff_correction = LOGICALS(3)
        ff_file = STRINGS(4)
        ff_scale = LOGICALS(5)
        ff_scaler =  REALS(6)
        sd_correction = LOGICALS(7)
        sd_file =  STRINGS(8)
 
     End If
 
     End Subroutine F2D_INP_CORRECTION
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
