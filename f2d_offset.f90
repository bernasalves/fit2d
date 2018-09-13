!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  *  f2d_offset.f90 *
!  *                 *
!  *******************
 
!+ F2D_OFFSET - Fit 2-D OFFSET and scaling between two data-sets
     Subroutine F2D_OFFSET (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, VARIANCES, MDATA, MVARIANCES, status)
!  Description:
!    Calculates offset and scaling between "DATA(xmaxdat, ymaxdat)" and
!    "MDATA(xmaxdat, ymaxdat)" in the region "(xstrelm, ystrelm)" to 
!    "(xendelm, yendelm)".
!  Method:
!    Calculates means of both images at for two separated ranges and can hence
!    deduce scaling and offset.
!  Deficiencies:
!    Variances are not used at present
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    03-Jan-1996: V0.5 Changes necessary for IBM AIX "xlf" compiler (Hammersley)
!    16-Sep-1993: V0.4 Allow lower limit for both images (Hammersley)
!    26-Aug-1993: V0.3 Allow different saturation values for both images 
!      (Hammersley)
!    23-Aug-1993: V0.2 Add possibility of known scaling (Hammersley)
!    06-May-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Logical, Intent(IN) :: variances_exist ! .True., if the variances array
!      exists
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! Variance array
     Real, Intent(IN) :: MDATA(xmaxdat, ymaxdat) ! Memory data
     Real, Intent(IN) :: MVARIANCES(xmaxdat, ymaxdat) ! Memory variance array
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User text
     Integer :: num_lower ! Number of values in lower mean
     Integer :: num_upper ! Number of values in upper mean
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Logical :: fail ! .True., if the algorithm cannot deduce the scaling and 
!      offset, e.g. if all the numbers are one side of the divide line
     Logical, Save :: scaling_known = .True. ! .True., if user already
!      knows the scaling between the two images
     Real, Save :: divide = 100.0 ! Level at which either side the means of
!      both images are calculated to allow both the scale factor and the
!      offset to be calculated
     Real, Save :: low_lim1 = 100.0 ! Lower limit for first data
     Real, Save :: low_lim2 = 100.0 ! Lower limit for second data
     Real :: mean1l, mean1u, mean2l, mean2u
     Real :: offset ! Offset factor between two data-sets i.e. "DATA * scaling -
!      MDATA"
     Real, Save :: saturated1 = 840.0 ! Level of saturation of range for
!      first image
     Real, Save :: saturated2 = 290.0 ! Level of saturation of range for
!      second image
     Real, Save :: scaling = 2.9 ! Scaling factor between two data-sets
!      "(DATA / MDATA)"
     Real :: sum_lower1, sum_lower2, sum_upper1, sum_upper2
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 !  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_OFFSET ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_OFFSET ' // Version)
 
     Else
 
!     Input whether scaling is known or not
        Call IO_INPL (.True., 0, 1, .True., 'SCALING KNOWN', 1, &
          '"YES" if the scaling between the two images is known', 1, &
          'Enter "YES" or "NO"', scaling_known, status)
 
        If (scaling_known) Then
 
!        Input scale factor
           Call IO_INPR (.False., 0.0, 1.0, .True., 'SCALE FACTOR', 1, &
             'Enter scale factor (current data divided by memory)', 1, &
             'Enter real number', scaling, status)
 
        Else
 
!        Input separation value
           Call IO_INPR (.False., 0.0, 0.0, .True., 'SEPARATION LIMIT VALUE', &
             1, 'Value below which lower mean is defined, '// &
             'above upper mean', 1, 'Enter real number', divide, status)
 
        End If
 
!     Input lower limit value
        Call IO_INPR (.True., 0.0, 1.7e38, .True., 'FIRST LOWER LIMIT', 1, &
          'Value below which values are not reliable for first image', &
          1, 'Enter real number', low_lim1, status)
 
!     Input saturation value
        Call IO_INPR (.True., low_lim1, 1.7e38, .True., &
          'FIRST SATURATION VALUE', 1, &
          'Value above which values are not reliable for first image', &
          1, 'Enter real number', saturated1, status)
 
!     Input lower limit value
        Call IO_INPR (.True., 0.0, 1.7e38, .True., 'SECOND LOWER LIMIT', 1, &
          'Value below which values are not reliable for second image', &
          1, 'Enter real number', low_lim2, status)
 
!     Input saturation value
        Call IO_INPR (.True., low_lim2, 1.7e38, .True., &
          'SECOND SATURATION VALUE', 1, &
          'Value above which values are not reliable for second image', &
          1, 'Enter real number', saturated2, status)
 
!     If the scaling is known then the job is much simpler
        If (scaling_known) Then
           fail = .False.
           sum_lower1 = 0.0
           num_lower = 0
 
           Do y = ystrelm, yendelm
 
              Do x = xstrelm, xendelm
 
                 If (DATA(x, y) .Ge. low_lim1 .And. DATA(x, y) .Le. saturated1 &
                   .And. MDATA(x, y) .Ge. low_lim2 .And. MDATA(x, y) .Le. &
                   saturated2) Then
                    num_lower = num_lower + 1
                    sum_lower1 = sum_lower1 + DATA(x, y) * scaling - &
                      MDATA(x, y)
                 End If
 
              End Do
 
           End Do
 
           If (num_lower .Gt. 0) Then
              offset = sum_lower1 / Real(num_lower)
           Else
              Call IO_WRITE ( 'WARNING: Range contains no values', status)
              Return
           End If
 
        Else
 
!        Initialise variables
           fail = .False.
           sum_lower1 = 0.0
           sum_lower2 = 0.0
           sum_upper1 = 0.0
           sum_upper2 = 0.0
           num_lower = 0
           num_upper = 0
 
           Do y = ystrelm, yendelm
 
              Do x = xstrelm, xendelm
 
                 If (DATA(x, y) .Lt. divide .And. MDATA(x, y) .Lt. saturated2) &
                   Then
 
                    num_lower = num_lower + 1
                    sum_lower1 = sum_lower1 + DATA(x, y)
                    sum_lower2 = sum_lower2 + MDATA(x, y)
 
                 Else If (MDATA(x, y) .Lt. saturated2) Then
 
                    num_upper = num_upper + 1
                    sum_upper1 = sum_upper1 + DATA(x, y)
                    sum_upper2 = sum_upper2 + MDATA(x, y)
 
                 End If
 
              End Do
 
           End Do
 
           If (num_lower .Gt. 0) Then
              mean1l = sum_lower1 / Real (num_lower)
              mean2l = sum_lower2 / Real (num_lower)
           Else
              fail = .True.
           End If
 
           If (num_upper .Gt. 0) Then
              mean1u = sum_upper1 / Real (num_upper)
              mean2u = sum_upper2 / Real (num_upper)
           Else
              fail = .True.
           End If
 
           If (.Not. fail .And. mean2l .Ne. mean2u) Then
 
              scaling = (mean1l - mean1u) / (mean2l - mean2u)
!           y_value = (mean1u + mean2u) / (scaling + 1)
!           offset = mean2u - scaling * y_value
              offset = (mean1u - scaling * mean2u) / (1.0 - scaling)
 
           Else
 
              fail = .True.
 
           End If
 
        End If
 
        If (.Not. fail) Then
           Write (message, '(''INFO: Scaling factor = '', g14.5)') scaling
           Call IO_WRITE (message, status)
           Write (message, '(''INFO: Offset = '', g14.5)') offset
           Call IO_WRITE (message, status)
        Else
           Call IO_WRITE ('WARNING: Not possible, try ' // &
             'different separation value', status)
        End If
 
     End If
 
     End Subroutine F2D_OFFSET
!********1*********2*********3*********4*********5*********6*********7*********8
