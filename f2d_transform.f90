!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_transform.f90 *
!  *                   *
!  *********************
 
!+ F2D_TRANSFORM - Fit 2-D  TRANSFORMation: rotate data about
!    a fixed point then translate
     Subroutine F2D_TRANSFORM (gui, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, &
       VARIANCES, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, title, &
       xlabel, ylabel, zlabel, variances_exist, x_pixel_size, y_pixel_size, &
       retstat, memory_exist, MXAXIS, MYAXIS, MDATA, MVARIANCES, mxnumdat, &
       mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, &
       mylabel, mzlabel, status)
!  Description:
!    Rotates and translates elements of "DATA(xmaxdat, ymaxdat)" in
!    the region from "(xstrelm, ystrelm)" to "(xendelm, yendelm)",
!    output in "MDATA".
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    19-Mar-2004: V0.5 Leave output array size unshrank by output region, if it 
!      was bigger beforehand (Hammersley)
!    19-Jul-2001: V0.4 Investigating error following use on WNT system: out of 
!      bounds problem ? (Hammersley)
!    13-Oct-2000: V0.3 Correct error in copying axis values to memory
!      (Hammersley)
!    27-Nov-1998: V0.2 Add timing code (Hammersley)
!    25-Nov-1998: V0.1 Original, based on "F2D_TRANSPOSE" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array to be rotated
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! Variance array to be
!      rotated
     Integer, Intent(IN) :: xnumdat ! Total number of defined data elements
!      in the X-direction
     Integer, Intent(IN) :: ynumdat ! Total number of defined data elements
!      in the Y-direction
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Character(Len = *), Intent(IN) :: title ! Title of data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
     Logical, Intent(IN) :: variances_exist ! .True., if variances arrays
!      exist
!  Import/Export:
     Real, Intent(INOUT) :: x_pixel_size ! Size of one unit (raw pixel) in
!      X-direction (metres)
     Real, Intent(INOUT) :: y_pixel_size ! Size of one unit (raw pixel) in
!      Y-direction (metres)
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status:
!      0 = Good status, operation performed correctly
!      1 = Bad status, operation not performed due to wrong ROI and array shape
     Logical, Intent(OUT) :: memory_exist ! .True., if the memory has been
!      defined
     Real, Intent(OUT) :: MXAXIS(xmaxdat) ! Averaged X-axis values
     Real, Intent(OUT) :: MYAXIS(ymaxdat) ! Averaged Y-axis values
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Result of transposing
     Real, Intent(INOUT) :: MVARIANCES(xmaxdat, ymaxdat) ! Transposed variance
!      array
     Integer, Intent(OUT) :: mxnumdat ! Total number of defined data
!      elements in the X-direction for memory array
     Integer, Intent(OUT) :: mynumdat ! Total number of defined data
!      elements in the Y-direction for memory array
     Integer, Intent(OUT) :: mxstrelm ! X-Start of transposed output region
     Integer, Intent(OUT) :: mystrelm ! Y-Start of transposed output region
     Integer, Intent(OUT) :: mxendelm ! X-End of transposed output region
     Integer, Intent(OUT) :: myendelm ! Y-End of transposed output region
     Character(Len = *), Intent(OUT) :: mtitle ! Title of data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for memory data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for memory data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for memory data
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
     Integer, Parameter :: Max_choices = 16 ! Maximum number of choices:
!      Note: Must of at least 5 more than "num_choices" to allow for general
!      form buttons
!  Local Variables:
     Character(Len = 80) :: message ! User text
     Integer :: dummy ! Dummy variable, not used
     Integer :: num_choices ! Number of choices
     Integer :: form_status ! Return status variable for "GS_FORM"
     Logical, Save :: first = .True. ! .True., if first call to the subroutine
     Real :: end_cpu ! CPU time at end of transform
     Real :: end_elapse ! Elapse time at end of transform
     Real, Save :: rotation_angle = 0.0 ! Angle for rotation in radians
     Real :: start_cpu ! CPU time at start of transform
     Real :: start_elapse ! Elapse time at start of transform
     Real, Save :: x_centre ! X-pixel coordinate of centre of rotation
     Real, Save :: x_translate = 0.0 ! X-component of translation vector
!      (pixels)
     Real, Save :: y_centre ! Y-pixel coordinate of centre of rotation
     Real, Save :: y_translate = 0.0 ! Y-component of translation vector
!      (pixels)
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 60) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 70) :: PROMPT(2) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = 80) :: TX(2) ! Text array
     Integer :: INTEGERS(Max_choices) ! Integer values
     Integer :: INTS_LOWER(Max_choices) ! Lower bound limits for integer values
     Integer :: INTS_UPPER(Max_choices) ! Upper bound limits for integer values
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
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_TRANSFORM ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm .Gt. xmaxdat .Or. xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Gt. ymaxdat .Or. yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_TRANSFORM ' // Version)
 
     Else
 
!     Set central rotation by default
        If (first) Then
           first = .False.
           x_centre = Real(xstrelm - 1 + xendelm) / 2.0
           y_centre = Real(ystrelm - 1 + yendelm) / 2.0
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Control form
        num_choices = 11
        PROMPT(1) = 'ROTATION PARAMETER'
        PROMPT(2) = 'CONTROL FORM'
 
        BUTTONS(1) = 'X-PIXEL SIZE'
        BUTTONS(2) = 'Y-PIXEL SIZE'
        BUTTONS(3) = 'X-CENTRE'
        BUTTONS(4) = 'Y-CENTRE'
        BUTTONS(5) = 'ANGLE'
        BUTTONS(6) = 'X-TRANSLATION'
        BUTTONS(7) = 'Y-TRANSLATION'
        BUTTONS(8) = 'X-START OUT'
        BUTTONS(9) = 'Y-START OUT'
        BUTTONS(10) = 'X-END OUT'
        BUTTONS(11) = 'Y-END OUT'
 
        TYPES(1) = Gs_real
        TYPES(2) = Gs_real
        TYPES(3) = Gs_real
        TYPES(4) = Gs_real
        TYPES(5) = Gs_real
        TYPES(6) = Gs_real
        TYPES(7) = Gs_real
        TYPES(8) = Gs_integer
        TYPES(9) = Gs_integer
        TYPES(10) = Gs_integer
        TYPES(11) = Gs_integer
 
        TEXT(1) = 'SIZE OF HORIZONTAL PIXELS (MICRONS)'
        TEXT(2) = 'SIZE OF VERTICAL PIXELS (MICRONS)'
        TEXT(3) = 'X-CENTRE OF ROTATION (PIXELS)'
        TEXT(4) = 'Y-CENTRE OF ROTATION (PIXELS)'
        TEXT(5) = 'ROTATION ANGLE (DEGREES)'
        TEXT(6) = 'X-COMPONENT OF TRANSLATION VECTOR'
        TEXT(7) = 'Y-COMPONENT OF TRANSLATION VECTOR'
        TEXT(8) = 'STARTING PIXEL OF OUTPUT REGION (X)'
        TEXT(9) = 'STARTING PIXEL OF OUTPUT REGION (Y)'
        TEXT(10) = 'END PIXEL OF OUTPUT REGION (X)'
        TEXT(11) = 'END PIXEL OF OUTPUT REGION (Y)'
 
        FULL_PROMPTS(1) = 'Enter dimension of pixels ' // &
          'horizontally, as displayed (microns)'
        FULL_PROMPTS(2) = 'Enter dimension of pixels ' // &
          'vertically, as displayed (microns)'
        FULL_PROMPTS(3) = 'X-coordinate of centre of rotation (pixels)'
        FULL_PROMPTS(4) = 'Y-coordinate of centre of rotation (pixels)'
        FULL_PROMPTS(5) = 'Angle of rotation anticlockwise (degrees)'
        FULL_PROMPTS(6) = 'Number of pixels to translate horizontally'
        FULL_PROMPTS(7) = 'Number of pixels to translate vertically'
        FULL_PROMPTS(8) = 'Minimum pixel of output region (horizontally)'
        FULL_PROMPTS(9) = 'Minimum pixel of output region (vertically)'
        FULL_PROMPTS(10) = 'Maximum pixel of output region (horizontally)'
        FULL_PROMPTS(11) = 'Maximum pixel of output region (vertically)'
 
        BOUND(1) = .True.
        BOUND(2) = .True.
        BOUND(3) = .False.
        BOUND(4) = .False.
        BOUND(5) = .True.
        BOUND(6) = .False.
        BOUND(7) = .False.
        BOUND(8) = .True.
        BOUND(9) = .True.
        BOUND(10) = .True.
        BOUND(11) = .True.
 
        REALS_LOWER(1) = 0.001
        REALS_UPPER(1) = 10000.0
        REALS_LOWER(2) = 0.001
        REALS_UPPER(2) = 10000.0
        REALS_LOWER(5) = -360.0
        REALS_UPPER(5) = 360.0
        INTS_LOWER(8) = 1
        INTS_UPPER(8) = xmaxdat
        INTS_LOWER(9) = 1
        INTS_UPPER(9) = ymaxdat
        INTS_LOWER(10) = 1
        INTS_UPPER(10) = xmaxdat
        INTS_LOWER(11) = 1
        INTS_UPPER(11) = ymaxdat
 
        REALS(1) = x_pixel_size * 1.0e6
        REALS(2) = y_pixel_size * 1.0e6
        REALS(3) = x_centre
        REALS(4) = y_centre
        REALS(5) = rotation_angle * 180.0 / Pi
        REALS(6) = x_translate
        REALS(7) = y_translate
        INTEGERS(8) = 1
        INTEGERS(9) = 1
        INTEGERS(10) = xmaxdat
        INTEGERS(11) = ymaxdat
 
!     Info text
        TX(1) = 'Enter rotation centre, angle of rotation,'
        TX(2) = 'and output region limits'
 
!     Output interactive graphical form
        Call GS_FORM (2, 2, PROMPT, 2, 2, TX, Max_choices, num_choices, &
          BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, INTS_LOWER, INTS_UPPER, &
          REALS_LOWER, REALS_UPPER, INTEGERS, dummy, REALS, STRINGS, &
          form_status, status)
 
!     Check for "CANCEL"
        If (form_status .Eq. -1) Then
           status = St_escapevalue
           Return
        End If
 
!     Set resulting values
        x_pixel_size = REALS(1) / 1.0e6
        y_pixel_size = REALS(2) / 1.0e6
        x_centre = REALS(3)
        y_centre =  REALS(4)
        rotation_angle = REALS(5) * Pi / 180.0
        x_translate = REALS(6)
        y_translate = REALS(7)
        mxstrelm = INTEGERS(8)
        mystrelm = INTEGERS(9)
        mxendelm = INTEGERS(10)
        myendelm = INTEGERS(11)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Output patience message
        Call GS_FPROMPT (1, 1, 'TRANSFORMING: Please wait', status)
 
!     Get start times
        Call IO_TIMES (start_elapse, start_cpu, status)
 
!     Rotate / translate data
        Call MA_2DTRANSFORM (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, &
          xendelm, yendelm, x_centre, y_centre, rotation_angle, x_translate, &
          y_translate, x_pixel_size, y_pixel_size, xmaxdat, ymaxdat, mxstrelm, &
          mystrelm, mxendelm, myendelm, MDATA, status)
 
!     Get end times
        Call IO_TIMES (end_elapse, end_cpu, status)
 
!     Output info to user
        Write (message, '(''INFO: CPU time (seconds) ' // &
          'for transformation = '', f7.2)') end_cpu - start_cpu
        Call IO_WRITE (message, status)
        Write (message, '(''      Elapse time (seconds) ' // &
          'for transformation = '', f7.2)') end_elapse - start_elapse
        Call IO_WRITE (message, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Set character strings
        mtitle = title
        mxlabel = ylabel
        mylabel = xlabel
        mzlabel = zlabel
 
!     Set memory defined data
        mxnumdat = mxendelm
        mynumdat = myendelm
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_TRANSFORM: Before F2D_AXES '')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Create new axis scales
        Call F2D_AXES (xmaxdat, mxnumdat, XAXIS(1), XAXIS(2) - XAXIS(1), &
          MXAXIS, status)
        Call F2D_AXES (ymaxdat, mynumdat, YAXIS(1), YAXIS(2) - YAXIS(1), &
          MYAXIS, status)
 
!     Set memory to be defined if everything is O.K.
        If (status .Eq. St_goodvalue) Then
           memory_exist = .True.
           retstat = 0
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_TRANSFORM: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG


     End If
 
     End Subroutine F2D_TRANSFORM
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
