!********1*********2*********3*********4*********5*********6*********7**
 
!  *************************
!  *                       *
!  * f2d_input_options.f90 *
!  *                       *
!  *************************
 
!+ F2D_INPUT_OPTIONS -  INPUT OPTIONS for control of input of auxiliary 
!  experimental information
     Subroutine F2D_INPUT_OPTIONS (INPUT_OPTIONS, status)
!  Description:
!    Input form for control of input of auxiliary experimental information
!  Keywords:
!    Input~Options.Control
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    25-Jan-2007: V0.3 Add option for automatic auxilliary data input from 
!      CIF's (Hammersley)
!    15-Sep-2006: V0.2 Add control of "monitor" and "attenuation" (Hammersley)
!    20-Mar-2006: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use MA_LIB
     Use GS_LIB
!     Use FIO_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     TYPE(INPUT_OPTIONS_STRUCTURE), Intent(INOUT) :: INPUT_OPTIONS ! Control of 
!      input of auxiliary experimental information (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number 
     Integer, Parameter :: Max_chars =  50 ! Maximum characters in a line
     Integer, Parameter :: Max_lines =   9 ! Number of lines in message
     Integer, Parameter :: Max_choices = 19 ! Maximum number of choices:
!      Note: Must of at least 5 more than "num_choices" to allow for general
!      form buttons
!  Local Variables:
     Integer :: num_choices ! Number of choices
     Integer :: retstat ! Return status variable for "GS_FORM"
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 60) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 70) :: PROMPT(2) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = Max_chars) :: TX(Max_lines) ! Text array
     Integer :: INTEGERS(1) ! Dummy array
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
     Logical :: BOUND(1) ! Dummy array
     Logical :: LOGICALS(Max_choices) ! Logical parameters to change
     Real :: REALS(1) ! Dummy array
!  Local Data Structures:
!  External Functions:
!  Local Data:
     Data TX(   1) / ' ' /
     Data TX(   2) / '      -------------------------------'/
     Data TX(   3) / '      Welcome to the FIT2D Auxiliary'/
     Data TX(   4) / '      Information input control form'/
     Data TX(   5) / '      -------------------------------'/
     Data TX(   6) / ' ' /
     Data TX(   7) / 'This input control form allows you'/
     Data TX(   8) / 'to select which information will be'/
     Data TX(   9) / 'input from CBF/CIF files.'/

!--------1---------2---------3---------4---------5---------6---------7--
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INPUT_OPTIONS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
        num_choices = 14
        PROMPT(1) = 'AUXILIARY EXPERIMENTAL INFORMATION'
        PROMPT(2) = 'INPUT CONTROL FORM'
 
        BUTTONS(1) = 'AUTO CIF INPUT'
        BUTTONS(2) = 'INPUT PIXEL SIZES'
        BUTTONS(3) = 'INPUT WAVELENGTH'
        BUTTONS(4) = 'INPUT POLARISATION'
        BUTTONS(5) = 'INPUT DISTANCE'
        BUTTONS(6) = 'INPUT BEAM CENTRE'
        BUTTONS(7) = 'INPUT DETECTOR TILT'
        BUTTONS(8) = 'INPUT TWO THETA'
        BUTTONS(9) = 'INPUT GONIOMETER'
        BUTTONS(10) = 'INPUT IMAGE VIEW'
        BUTTONS(11) = 'INPUT UNIT CELL'
        BUTTONS(12) = 'INPUT UB MATRIX'
        BUTTONS(13) = 'INPUT MONITOR'
        BUTTONS(14) = 'INPUT ATTENUATION'
 
        TYPES(1) = Gs_logical
        TYPES(2) = Gs_logical
        TYPES(3) = Gs_logical
        TYPES(4) = Gs_logical
        TYPES(5) = Gs_logical
        TYPES(6) = Gs_logical
        TYPES(7) = Gs_logical
        TYPES(8) = Gs_logical
        TYPES(9) = Gs_logical
        TYPES(10) = Gs_logical
        TYPES(11) = Gs_logical
        TYPES(12) = Gs_logical
        TYPES(13) = Gs_logical
        TYPES(14) = Gs_logical
 
        TEXT(1) = 'AUTOMATIC INPUT OF AUXILIARY DATA FROM CIFS'
        TEXT(2) = 'INPUT PIXEL SIZES FROM CIF/CBF FILE'
        TEXT(3) = 'INPUT WAVELENGTH FROM CIF/CBF FILE'
        TEXT(4) = 'INPUT POLARISATION FROM CIF/CBF FILE'
        TEXT(5) = 'INPUT DETECTOR DISTANCE FROM CIF/CBF FILE'
        TEXT(6) = 'INPUT BEAM CENTRE FROM CIF/CBF FILE'
        TEXT(7) = 'INPUT DETECTOR TILT FROM CIF/CBF FILE'
        TEXT(8) = 'INPUT TWO THETA ANGLE FROM CIF/CBF FILE'
        TEXT(9) = 'INPUT GONIOMETER ANGLES FROM CIF/CBF FILE'
        TEXT(10) = 'INPUT IMAGE ORIENTATION FROM CIF/CBF FILE'
        TEXT(11) = 'INPUT UNIT CELL PARAMETERS FROM CIF/CBF FILE'
        TEXT(12) = 'INPUT UB ORIENTATION MATRIX FROM CIF/CBF FILE'
        TEXT(13) = 'INPUT MONITOR COUNT VALUE FROM CIF/CBF FILE'
        TEXT(14) = 'INPUT ATTENUATION FACTOR FROM CIF/CBF FILE'

        FULL_PROMPTS(1) = 'Enter "True" to allow automatic input of ' // &
          'from .cif files'
        FULL_PROMPTS(2) = 'Enter "True" to input pixel sizes from cif/cbf file'
        FULL_PROMPTS(3) = 'Enter "True" to input wavelength from cif/cbf file'
        FULL_PROMPTS(4) = 'Enter "True" to input polarisation from cif/cbf file'
        FULL_PROMPTS(5) = &
          'Enter "True" to input detector distance from cif/cbf file'
        FULL_PROMPTS(6) = 'Enter "True" to input beam centre from cif/cbf file'
        FULL_PROMPTS(7) = &
          'Enter "True" to input detector tilt from cif/cbf file'
        FULL_PROMPTS(8) = &
          'Enter "True" to input two theta angle from cif/cbf file'
        FULL_PROMPTS(9) = &
          'Enter "True" to input goniometer angles from cif/cbf file'
        FULL_PROMPTS(10) = &
           'Enter "True" to input image orientation from cif/cbf file'
        FULL_PROMPTS(11) = 'Enter "True" to input unit cell from cif/cbf file'
        FULL_PROMPTS(12) = 'Enter "True" to input ub matrix from cif/cbf file'
        FULL_PROMPTS(13) = 'Enter "True" to input monitor from cif/cbf file'
        FULL_PROMPTS(14) = 'Enter "True" to input attenuation from cif/cbf file'
 
        LOGICALS(1) = INPUT_OPTIONS%auto_input_from_cif
        LOGICALS(2) = INPUT_OPTIONS%input_pixel_sizes
        LOGICALS(3) = INPUT_OPTIONS%input_wavelength
        LOGICALS(4) = INPUT_OPTIONS%input_polarisation 
        LOGICALS(5) = INPUT_OPTIONS%input_detector_distance
        LOGICALS(6) = INPUT_OPTIONS%input_beam_centre
        LOGICALS(7) = INPUT_OPTIONS%input_tilt
        LOGICALS(8) = INPUT_OPTIONS%input_two_theta 
        LOGICALS(9) = INPUT_OPTIONS%input_goniometer_angles
        LOGICALS(10) = INPUT_OPTIONS%input_image_orientation
        LOGICALS(11) = INPUT_OPTIONS%input_cell 
        LOGICALS(12) = INPUT_OPTIONS%input_ub_matrix
        LOGICALS(13) = INPUT_OPTIONS%input_monitor
        LOGICALS(14) = INPUT_OPTIONS%input_attenuation
 
!     Output interactive graphical form
        Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, Max_choices, &
          num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, INTEGERS, &
          INTEGERS, REALS, REALS, INTEGERS, LOGICALS, REALS, STRINGS, &
          retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           status = St_escapevalue
           Return
        End If
 
!     Set resulting values
        INPUT_OPTIONS%auto_input_from_cif = LOGICALS(1) 
        INPUT_OPTIONS%input_pixel_sizes = LOGICALS(2) 
        INPUT_OPTIONS%input_wavelength = LOGICALS(3) 
        INPUT_OPTIONS%input_polarisation = LOGICALS(4) 
        INPUT_OPTIONS%input_detector_distance = LOGICALS(5) 
        INPUT_OPTIONS%input_beam_centre = LOGICALS(6) 
        INPUT_OPTIONS%input_tilt = LOGICALS(7) 
        INPUT_OPTIONS%input_two_theta = LOGICALS(8) 
        INPUT_OPTIONS%input_goniometer_angles = LOGICALS(9) 
        INPUT_OPTIONS%input_image_orientation = LOGICALS(10) 
        INPUT_OPTIONS%input_cell = LOGICALS(11)
        INPUT_OPTIONS%input_ub_matrix = LOGICALS(12)
        INPUT_OPTIONS%input_monitor = LOGICALS(13)
        INPUT_OPTIONS%input_attenuation = LOGICALS(14)
 
!     Save input values in internal data-base
        Call IO_SET_LKEYVALUE ('AUTOMATIC_INPUT_FROM_CIF', &
          INPUT_OPTIONS%auto_input_from_cif, retstat, status)
        Call IO_SET_LKEYVALUE ('AUXILIARY_INPUT_PIXEL_SIZES', &
          INPUT_OPTIONS%input_pixel_sizes, retstat, status)
        Call IO_SET_LKEYVALUE ('AUXILIARY_INPUT_WAVELENGTH', &
          INPUT_OPTIONS%input_wavelength, retstat, status)
        Call IO_SET_LKEYVALUE ('AUXILIARY_INPUT_POLARISATION', &
          INPUT_OPTIONS%input_polarisation, retstat, status)
        Call IO_SET_LKEYVALUE ('AUXILIARY_INPUT_DETECTOR_DISTANCE', &
          INPUT_OPTIONS%input_detector_distance, retstat, status)
        Call IO_SET_LKEYVALUE ('AUXILIARY_INPUT_BEAM_CENTRE', &
          INPUT_OPTIONS%input_beam_centre, retstat, status)
        Call IO_SET_LKEYVALUE ('AUXILIARY_INPUT_TILT', &
          INPUT_OPTIONS%input_tilt, retstat, status)
        Call IO_SET_LKEYVALUE ('AUXILIARY_INPUT_TWO_THETA', &
          INPUT_OPTIONS%input_two_theta, retstat, status)
        Call IO_SET_LKEYVALUE ('AUXILIARY_INPUT_GONIOMETER_ANGLES', &
          INPUT_OPTIONS%input_goniometer_angles, retstat, status)
        Call IO_SET_LKEYVALUE ('AUXILIARY_INPUT_IMAGE_ORIENTATION', &
          INPUT_OPTIONS%input_image_orientation, retstat, status)
        Call IO_SET_LKEYVALUE ('AUXILIARY_INPUT_CELL', &
          INPUT_OPTIONS%input_cell, retstat, status)
        Call IO_SET_LKEYVALUE ('AUXILIARY_INPUT_UB_MATRIX', &
          INPUT_OPTIONS%input_ub_matrix, retstat, status)
        Call IO_SET_LKEYVALUE ('AUXILIARY_INPUT_MONITOR', &
          INPUT_OPTIONS%input_monitor, retstat, status)
        Call IO_SET_LKEYVALUE ('AUXILIARY_INPUT_ATTENUATION', &
          INPUT_OPTIONS%input_attenuation, retstat, status)

     End If

     End Subroutine F2D_INPUT_OPTIONS
!********1*********2*********3*********4*********5*********6*********7**
