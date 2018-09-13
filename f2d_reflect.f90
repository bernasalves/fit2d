!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_reflect.f90 *
!  *                 *
!  *******************
 
!+ F2D_REFLECT - Fit 2-D  REFLECT
     Subroutine F2D_REFLECT (gui, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, &
       VARIANCES, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, title, &
       xlabel, ylabel, zlabel, variances_exist, x_pixel_size, y_pixel_size, &
       retstat, memory_exist, MXAXIS, MYAXIS, MDATA, MVARIANCES, mxnumdat, &
       mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, &
       mylabel, mzlabel, status)
!  Description:
!    Reflects "DATA(xmaxdat, ymaxdat)" in the region "(xstrelm, ystrelm)" to 
!    "(xendelm, yendelm)" in a line of reflection user defined. The result is 
!    output in "MDATA(xmaxdat, ymaxdat)".
!  Method:
!    Uses "MA_RREFLECT" to perform the reflection.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    29-Nov-1998: V0.5 Change argument list to match "F2D_TRANSFORM" 
!      (Hammersley)
!    12-Nov-1993: V0.4 Change "MA_DC2PIXEL" to "MA_DC2PC" (Hammersley)
!    28-Apr-1993: V0.3 Treat variances (Hammersley)
!    05-Feb-1993: V0.2 Convert data coordinates to pixel coordinates 
!      (Hammersley)
!    18-Jan-1993: V0.1 Original (Hammersley)
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
     Logical, Intent(IN) :: variances_exist ! .True., if variances arrays exist
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
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat) ! Transposed variance
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
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
     Integer, Parameter :: Max_choices = 9 ! Maximum number of choices:
!      Note: Must of at least 5 more than "num_choices" to allow for general
!      form buttons
!  Local Variables:
     Character(Len = 80) :: message ! User text
     Integer :: dummy ! Dummy variable, not used
     Integer :: form_status ! Return status variable for "GS_FORM"
     Integer :: num_coordinates ! Number of returned coordinates
     Integer :: num_choices ! Number of choices
     Integer, Save :: subpixels = 0 ! Number of sub-pixels in each direction for
!      more accurate relecting of pixel intensity
     Real :: end_cpu ! CPU time at end of transform
     Real :: end_elapse ! Elapse time at end of transform
     Real :: start_cpu ! CPU time at start of transform
     Real :: start_elapse ! Elapse time at start of transform
     Real :: x1 ! X-component of first coordinate defining line of reflection
     Real :: xpc1 ! X-pixel coordinate of first point defining line of 
!      reflection
     Real :: x2 ! X-component of first coordinate defining line of reflection
     Real :: xpc2 ! X-pixel coordinate of second point defining line of 
!      reflection
     Real :: y1 ! Y-component of second coordinate defining line of reflection
     Real :: ypc1 ! Y-pixel coordinate of first point defining line of 
!      reflection
     Real :: y2 ! Y-component of second coordinate defining line of reflection
     Real :: ypc2 ! Y-pixel coordinate of second point defining line of 
!      reflection
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
     Real :: X_COORDINATES(2) ! X-coordinates of points defining the reflection
!      line
     Real :: Y_COORDINATES(2) ! Y-coordinates of points defining the reflection
!      line
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_REFLECT ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_REFLECT ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
        If (gui) Then
 
!        Enter points to define reflection line
           num_coordinates = 2
           Call GS_INPS_FCOORDINATES (.False., .True., xmaxdat, ymaxdat, &
             xstrelm, ystrelm, xendelm, yendelm, DATA, dummy, XAXIS, YAXIS, &
             title, xlabel, ylabel, zlabel, 'REFLECTION LINE', 1, &
             'Click on two points of line of reflection', .True., 2, &
             num_coordinates, X_COORDINATES, Y_COORDINATES, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''After GS_INPS_FCOORDINATES'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Set points on line
           xpc1 = X_COORDINATES(1)
           ypc1 = Y_COORDINATES(1)
           xpc2 = X_COORDINATES(2)
           ypc2 = Y_COORDINATES(2)
 
!        Control form
           num_choices = 4
           PROMPT(1) = 'OUTPUT REGION'
           PROMPT(2) = 'CONTROL FORM'
 
           BUTTONS(1) = 'X-START OUT'
           BUTTONS(2) = 'Y-START OUT'
           BUTTONS(3) = 'X-END OUT'
           BUTTONS(4) = 'Y-END OUT'
 
           TYPES(1) = Gs_integer
           TYPES(2) = Gs_integer
           TYPES(3) = Gs_integer
           TYPES(4) = Gs_integer
 
           TEXT(1) = 'STARTING PIXEL OF OUTPUT REGION (X)'
           TEXT(2) = 'STARTING PIXEL OF OUTPUT REGION (Y)'
           TEXT(3) = 'END PIXEL OF OUTPUT REGION (X)'
           TEXT(4) = 'END PIXEL OF OUTPUT REGION (Y)'
 
           FULL_PROMPTS(1) = 'Minimum pixel of output region ' // &
             '(horizontally)'
           FULL_PROMPTS(2) = 'Minimum pixel of output region ' // &
             '(vertically)'
           FULL_PROMPTS(3) = 'Maximum pixel of output region ' // &
             '(horizontally)'
           FULL_PROMPTS(4) = 'Maximum pixel of output region ' // &
             '(vertically)'
 
           BOUND(1) = .True.
           BOUND(2) = .True.
           BOUND(3) = .True.
           BOUND(4) = .True.
 
           INTS_LOWER(1) = 1
           INTS_UPPER(1) = xmaxdat
           INTS_LOWER(2) = 1
           INTS_UPPER(2) = ymaxdat
           INTS_LOWER(3) = 1
           INTS_UPPER(3) = xmaxdat
           INTS_LOWER(4) = 1
           INTS_UPPER(4) = ymaxdat
 
           INTEGERS(1) = 1
           INTEGERS(2) = 1
           INTEGERS(3) = xmaxdat
           INTEGERS(4) = ymaxdat
 
!        Info text
           TX(1) = 'Enter pixel limits of the'
           TX(2) = 'output region'
 
!        Output interactive graphical form
           Call GS_FORM (2, 2, PROMPT, 2, 2, TX, Max_choices, num_choices, &
             BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, INTS_LOWER, &
             INTS_UPPER, dummy, dummy, INTEGERS, dummy, dummy, STRINGS, &
             form_status, status)
 
!        Check for "CANCEL"
           If (form_status .Eq. -1) Then
              status = St_escapevalue
              Return
           End If
 
!        Set resulting values
           mxstrelm = INTEGERS(1)
           mystrelm = INTEGERS(2)
           mxendelm = INTEGERS(3)
           myendelm = INTEGERS(4)
 
!        Full triangular re-binning
           subpixels = 0
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Before GS_FPROMPT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Output patience message
           Call GS_FPROMPT (1, 1, 'REFLECTING: Please wait', status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''After GS_FPROMPT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        Else
 
!        Input first coordinate on reflection line
           Call IO_INPR (.False., 0.0, 0.0, .False., 'X COMPONENT POINT 1', 1, &
             'X-component of first coordinate on line of reflection', 1, &
             'Enter real number', x1, status)
           Call IO_INPR (.False., 0.0, 0.0, .False., 'Y COMPONENT POINT 1', 1, &
             'Y-component of first coordinate on line of reflection', 1, &
             'Enter real number', y1, status)
 
!        Input second coordinate on reflection line
           Call IO_INPR (.False., 0.0, 0.0, .False., 'X COMPONENT POINT 2', 1, &
             'X-component of second coordinate on line of reflection', &
             1, 'Enter real number', x2, status)
           Call IO_INPR (.False., 0.0, 0.0, .False., 'Y COMPONENT POINT 2', 1, &
             'Y-component of second coordinate on line of reflection', &
             1, 'Enter real number', y2, status)
 
!        Number of sub-pixels
           subpixels = 2
           Call IO_INPI (.True., 0, 100, .True., 'SUB-PIXELS', 1, &
             'Number of sub-pixels in each direction ' // &
             '(0 = area re-binning)', 1, 'Enter number in range', subpixels, &
             status)
 
!        More than 4 sub-pixels is slower than area re-binning
           If (subpixels .Gt. 4) Then
              subpixels = 0
           End If
 
!        Convert data coordinates to pixel coordinate values
           Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, x1, xpc1, status)
           Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, x2, xpc2, status)
           Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, y1, ypc1, status)
           Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, y2, ypc2, status)
 
           mxstrelm = xstrelm
           mystrelm = ystrelm
           mxendelm = xendelm
           myendelm = yendelm
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Check if user escape has been entered (or error condition)
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Get start times
        Call IO_TIMES (start_elapse, start_cpu, status)
 
!     Reflect data in line of reflection
        Call MA_RREFLECT (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
          yendelm, xpc1, ypc1, xpc2, ypc2, subpixels, subpixels, xmaxdat, &
          ymaxdat, mxstrelm, mystrelm, mxendelm, myendelm, MDATA, status)
 
        If (variances_exist) Then
 
!        Reflect data variances in line of reflection
           Call MA_RREFLECT (xmaxdat, ymaxdat, VARIANCES, xstrelm, ystrelm, &
             xendelm, yendelm, xpc1, ypc1, xpc2, ypc2, subpixels, subpixels, &
             xmaxdat, ymaxdat, mxstrelm, mystrelm, mxendelm, myendelm, &
             MVARIANCES, status)
 
        End If
 
!     Get end times
        Call IO_TIMES (end_elapse, end_cpu, status)
 
!     Output info to user
        Write (message, '(''INFO: CPU time (seconds) ' // &
          'for transformation = '', f7.2)') end_cpu - start_cpu
        Call IO_WRITE (message, status)
        Write (message, '(''      Elapse time (seconds) ' // &
          'for transformation = '', f7.2)') end_elapse - start_elapse
        Call IO_WRITE (message, status)
 
!     Set character strings
        mtitle = title
        mxlabel = ylabel
        mylabel = xlabel
        mzlabel = zlabel
 
!     Copy axis values
        Call MA_RCOPY (xmaxdat, 1, XAXIS, 1, 1, xendelm, 1, ymaxdat, 1, &
          MYAXIS, status)
        Call MA_RCOPY (ymaxdat, 1, YAXIS, 1, 1, yendelm, 1, xmaxdat, 1, &
          MXAXIS, status)
 
!     Set memory defined data
        mxnumdat = mxendelm
        mynumdat = myendelm
 
!     Set memory to be defined if everything is O.K.
        If (status .Eq. St_goodvalue) Then
           memory_exist = .True.
           retstat = 0
        End If
 
     End If
 
     End Subroutine F2D_REFLECT
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
