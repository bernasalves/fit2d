!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_1dtransforms.f90 *
!  *                      *
!  ************************
 
!+ F2D_1DTRANSFORMS: FIT2D choice of 1-D TRANSFORMS to Q space data
     Subroutine F2D_1DTRANSFORMS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, DATA, status)
!  Description:
!    Transforms 1-D data to a choice of Q space scalings
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    03-Feb-2001: V0.5 Macro input doesn't accept "a", "b", or "c" menu items 
!      so change to "Coeff. a" etc. (Hammersley)
!    26-Aug-1998: V0.4 Changes user text (Hammersley)
!    30-Jan-1998: V0.3 Declare unused character string for call to "GS_FORM" 
!      (Hammersley)
!    28-May-1998: V0.2 Convert negative values to positive prior to taking logs
!      (Hammersley)
!    25-May-1998: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use GS_LIB
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array
     Integer, Intent(IN) :: xstrelm ! First element in X-direction to output
     Integer, Intent(IN) :: ystrelm ! First element in Y-direction to output
     Integer, Intent(IN) :: xendelm ! Last element in X-direction to output
     Integer, Intent(IN) :: yendelm ! Last element in Y-direction to output
!  Import/Export:
     Real, Intent(INOUT) :: XAXIS(xmaxdat) ! Array to contain X-coordinate data
     Real, Intent(INOUT) :: YAXIS(ymaxdat) ! Array to contain Y-coordinate data
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data values
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
     Integer, Parameter :: Max_choices = 10 ! Maximum number of choices
!      (need 5 extra spaces)
!  Local Variables:
     Integer, Save :: a = 1 ! Power of I(q) for the Y values transformation
     Integer, Save :: b = 0 ! Power of q for the Y values transformation
     Integer, Save :: c = 1 ! Power of q for the X values transformation
     Integer :: element ! Loop variable for array elements
     Integer :: num_choices ! Number of choices
     Integer :: retstat ! Return status variable
     Logical, Save :: log_i = .False. ! .True., if the log of the intensity
!      is to be calculated
     Logical, Save :: log_x = .False. ! .True., if the log of the Y-values
!      is to be calculated
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 70) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 80) :: MESSAGE(1) ! User message text
     Character(Len = 70) :: PROMPT(3) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
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
     Real :: REALS_UNUSED(1) ! Dummy array for "GS_FORM"
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_1DTRANSFORMS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_1DTRANSFORMS ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_1DTRANSFORMS ' // Version)
     Else
 
!     Check that the data set is 1-D
        If (yendelm .Ne. ystrelm) Then
 
           MESSAGE(1) = 'THE DATA IS NOT 1-D !'
           Call GS_FWARNING (1, 1, MESSAGE, status)
           Return
 
        End If
 
!     Allow user of choose number of output bins and control
!     various other options
        num_choices = 5
        PROMPT(1) = 'CONTROL OF TRANSFORMATION TO INTENSITY '
        PROMPT(2) = 'VERSUS Q SPACE SCAN, OF THE FORM:'
        PROMPT(3) = '[Log] I(q)**a * q**b versus [Log]q**c'
        BUTTONS(1) = 'Coeff. a'
        BUTTONS(2) = 'Coeff. b'
        BUTTONS(3) = 'LOG I'
        BUTTONS(4) = 'Coeff. c'
        BUTTONS(5) = 'LOG X'
        TYPES(1) = Gs_integer
        TYPES(2) = Gs_integer
        TYPES(3) = Gs_logical
        TYPES(4) = Gs_integer
        TYPES(5) = Gs_logical
        TEXT(1) = 'a: POWER OF INTENSITIES'
        TEXT(2) = 'b: POWER OF Q MULTIPLIED BY I(q)'
        TEXT(3) = 'TAKE LOG OF I(q)**a * q**b'
        TEXT(4) = 'c: POWER OF Q'
        TEXT(5) = 'TAKE LOG OF q**c'
 
        FULL_PROMPTS(1) = 'Integer power a of I(q)**a for transformation'
        FULL_PROMPTS(2) = &
          'Integer power b of I(q)**a * q**b for q tranformation'
        FULL_PROMPTS(3) = '"YES" to take Log of transformed intensities'
        FULL_PROMPTS(4) = 'Integer power of q**c for X-axis values '
        FULL_PROMPTS(5) = '"YES" to take Log of transformed ' // &
          'q values for the X-axis'
        BOUND(1) = .True.
        BOUND(2) = .True.
        BOUND(3) = .True.
        BOUND(4) = .True.
        BOUND(5) = .True.
 
        INTS_LOWER(1) = 0
        INTS_UPPER(1) = 4
        INTS_LOWER(2) = 0
        INTS_UPPER(2) = 4
        INTS_LOWER(4) = 0
        INTS_UPPER(4) = 4
        INTEGERS(1) = a
        INTEGERS(2) = b
        LOGICALS(3) = log_i
        INTEGERS(4) = c
        LOGICALS(5) = log_x
 
!     Output interactive graphical form
        Call GS_FORM (3, 3, PROMPT, 3, 3, PROMPT, Max_choices, num_choices, &
          BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, INTS_LOWER, INTS_UPPER, &
          REALS_UNUSED, REALS_UNUSED, INTEGERS, LOGICALS, REALS_UNUSED, &
          STRINGS, retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           status = St_escapevalue
           Return
        End If
 
!     Set resulting values
        a = INTEGERS(1)
        b = INTEGERS(2)
        log_i = LOGICALS(3)
        c = INTEGERS(4)
        log_x = LOGICALS(5)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Apply Y-axis scaling
        If (log_i) Then
 
           If (a .Ne. 1) Then
 
              If (b .Eq. 0) Then
 
                 Do element = xstrelm, xendelm
 
                    If (DATA(element, ystrelm) .Gt. 1.0e-30) Then
 
                       DATA(element, ystrelm) = Log10(DATA(element, ystrelm)**a)
                    Else
                       DATA(element, ystrelm) = -30.0
                    End If
 
                 End Do
 
              Else
 
                 Do element = xstrelm, xendelm
 
                    If (DATA(element, ystrelm) .Gt. 1.0e-30) Then
 
                       DATA(element, ystrelm) = &
                         Log10(DATA(element, ystrelm)**a * YAXIS(element)**b)
                    Else
                       DATA(element, ystrelm) = -30.0
                    End If
 
                 End Do
 
              End If
 
           Else
 
              If (b .Eq. 0) Then
 
                 Do element = xstrelm, xendelm
 
                    If (DATA(element, ystrelm) .Gt. 1.0e-30) Then
                       DATA(element, ystrelm) = Log10(DATA(element, ystrelm))
                    Else
                       DATA(element, ystrelm) = -30.0
                    End If
 
                 End Do
 
              Else
 
                 Do element = xstrelm, xendelm
 
                    If (DATA(element, ystrelm) .Gt. 1.0e-30) Then
                       DATA(element, ystrelm) = Log10(DATA(element, ystrelm) * &
                         YAXIS(element)**b)
                    Else
                       DATA(element, ystrelm) = -30.0
                    End If
 
                 End Do
 
              End If
 
           End If
 
        Else
 
           If (a .Ne. 1) Then
 
              If (b .Eq. 0) Then
 
                 Do element = xstrelm, xendelm
                    DATA(element, ystrelm) = DATA(element, ystrelm)**a
                 End Do
 
              Else
 
                 Do element = xstrelm, xendelm
                    DATA(element, ystrelm) = DATA(element, ystrelm)**a * &
                      YAXIS(element)**b
                 End Do
 
              End If
 
           Else
 
              If (b .Eq. 0) Then
 
!              No Change
                 Continue
 
              Else
 
                 Do element = xstrelm, xendelm
                    DATA(element, ystrelm) = DATA(element, ystrelm) * &
                      YAXIS(element)**b
                 End Do
 
              End If
 
           End If
 
        End If
 
!     X-axis scaling
        If (log_x) Then
 
           If (c .Ne. 1) Then
 
              Do element = xstrelm, xendelm
 
                 If (Abs(XAXIS(element)) .Gt. 1.0e-10) Then
                    XAXIS(element) = Log10(Abs(XAXIS(element))**c)
                 Else
                    XAXIS(element) = -10.0
                 End If
 
              End Do
 
           Else
 
              Do element = xstrelm, xendelm
 
                 If (Abs(XAXIS(element)) .Gt. 1.0e-10) Then
                    XAXIS(element) = Log10(Abs(XAXIS(element)))
                 Else
                    XAXIS(element) = -10.0
                 End If
 
              End Do
 
           End If
 
        Else
 
           If (c .Ne. 1) Then
 
              Do element = xstrelm, xendelm
                 XAXIS(element) = XAXIS(element)**c
              End Do
 
           Else
 
!           No change
              Continue
 
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_1DTRANSFORMS
!********1*********2*********3*********4*********5*********6*********7*********8
 
