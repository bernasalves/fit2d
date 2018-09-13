 
!  *********************
!  *                   *
!  * f2d_variances.f90 *
!  *                   *
!  *********************
 
!+ F2D_VARIANCES - Fit 2-D: VARIANCES estimation
     Subroutine F2D_VARIANCES (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, VARIANCES, status)
!  Description:
!    Estimates data value variances based only on the input data and the 
!    assumption that the data comes from a Poisson process and local smoothness.
!    Estimated variance values are output in "VARIANCES". The data in the region
!    "(xstrelm, ystrelm)" to "(xendelm, yendelm)" is used.
!  Method:
!    The data is assumed to be Poissonian, which has been scaled by an unknown 
!    scaling constant (c). Therefore we measure instead of x xc and the variance
!    is xc**2.
!
!    If we measure a point and its variance (s) (exactly) we can determine c:
!
!    c = s/x
!
!    In reality we know neither x nor s, but have a sample x' which is a Poisson
!    deviate. For every point we will assume x' is x and take s to be the square
!    of the difference between x' and the average of four nearest neighbours.
!
!    For every point we will calculate a value for c and calculate the average
!    and standard deviation of c for the whole data.
!  Deficiencies:
!    Needs more careful consideration, theory. (Results not fully understood, 
!    but tested to be reasonable.)
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    03-May-1993: V0.2 Add lower limit of "Poisson" range (Hammersley)
!    29-Apr-1993: V0.1 Original (Hammersley)
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
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array
!  Export:
     Real, Intent(OUT) :: VARIANCES(xmaxdat, ymaxdat) ! Estimated variances
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: num_elements ! Number of elements used for calculation of 
!      scaling constant
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Real :: c ! Local scaling factor
     Real :: c_sum ! Sum of local scaling factors
     Real :: cc_sum ! Sum of square of local scaling factors
     Real :: local_average ! Average of nearest neighbour data values
     Real, Save :: lower_limit = 80.0 ! Limit below which data is assumed
!      to no-longer obey Poisson statististics
     Real :: minimum_variance ! The lowest estimated variance
     Real :: s ! Estimated local variance
     Real :: scaling_constant ! Factor by which true Poissonian statistics data
!      was multiplied to obtain input data
     Real, Save :: upper_limit = 254.0 ! Upper limit of "Poisson" range
!      (inclusive of non-saturated data values)
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_VARIANCES ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_VARIANCES ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Input maximum non-saturated data value
        Call IO_INPR (.False., 0.0, 0.0, .True., &
          'MAXIMUM NON-SATURATED VALUE', 1, &
          'Enter maximum value of reliable range', 1, 'Enter real number', &
          upper_limit, status)
 
!     Input lower limit for Poisson assumption
        Call IO_INPR (.False., 0.0, 0.0, .True., &
          'LOWER "POISSON" LIMIT (FOG)', 1, &
          'Enter lower limit of reliable "Poisson" range', 1, &
          'Enter real number', lower_limit, status)
 
!     Initialise variables
        c_sum = 0.0
        cc_sum = 0.0
        num_elements = 0
 
        Do y = ystrelm + 1, yendelm - 1
 
           Do x = xstrelm + 1, xendelm - 1
 
              If (DATA(x, y) .Gt. lower_limit .And. DATA(x, y) .Lt. &
                upper_limit) Then
                 num_elements = num_elements + 1
                 local_average = ( DATA(x - 1, y) + DATA(x + 1, y) + DATA(x, y &
                   - 1) + DATA(x, y + 1) + DATA(x - 1, y - 1) + DATA(x - 1, y &
                   + 1) + DATA(x + 1, y - 1) + DATA(x + 1, y + 1) ) /8.0
 
                 s = ( DATA(x, y) - local_average )**2
                 c = s / DATA(x, y)
                 c_sum = c_sum + c
                 cc_sum = cc_sum + c**2
              End If
 
           End Do
 
        End Do
 
!     Calculate scaling constant
        If (num_elements .Gt. 0) Then
           scaling_constant = (c_sum / Real(num_elements)) * 0.8888888 ! 8/9
 
           Write (*, '(''Scaling constant = '', g12.5)') scaling_constant
 
!        Calculate minimum variance
           minimum_variance = lower_limit * scaling_constant
 
!        Calculate estimated variances, average by surrounding data values
           Do y = ystrelm + 1, yendelm - 1
 
              Do x = xstrelm + 1, xendelm - 1
 
                 If (DATA(x, y) .Lt. lower_limit) Then
                    VARIANCES(x, y) = minimum_variance
 
                 Else If (DATA(x, y) .Le. upper_limit) Then
                    VARIANCES(x, y) = scaling_constant * ( DATA(x - 1, y) + &
                      DATA(x + 1, y) + DATA(x, y - 1) + DATA(x, y + 1) + &
                      DATA(x, y) + DATA(x - 1, y - 1) + DATA(x - 1, y + 1) + &
                      DATA(x + 1, y - 1) + DATA(x + 1, y + 1) ) / 9.0
                 Else
 
!                 Set very high variance
                    VARIANCES(x, y) = 1.0e20
                 End If
 
              End Do
 
           End Do
 
!        Handle edges specially
           Do x = xstrelm, xendelm
 
              If (DATA(x, ystrelm) .Lt. lower_limit) Then
                 VARIANCES(x, ystrelm) = minimum_variance
              Else If (DATA(x, ystrelm) .Le. upper_limit) Then
                 VARIANCES(x, ystrelm) = scaling_constant * DATA(x, ystrelm)
              Else
                 VARIANCES(x, ystrelm) = 1.0e20
              End If
 
           End Do
 
           Do x = xstrelm, xendelm
 
              If (DATA(x, yendelm) .Lt. lower_limit) Then
                 VARIANCES(x, yendelm) = minimum_variance
              Else If (DATA(x, yendelm) .Le. upper_limit) Then
                 VARIANCES(x, yendelm) = scaling_constant * DATA(x, yendelm)
              Else
                 VARIANCES(x, yendelm) = 1.0e20
              End If
 
           End Do
 
           Do y = ystrelm + 1, yendelm - 1
 
              If (DATA(xstrelm, y) .Lt. lower_limit) Then
                 VARIANCES(xstrelm, y) = minimum_variance
              Else If (DATA(xstrelm, y) .Le. upper_limit) Then
                 VARIANCES(xstrelm, y) = scaling_constant * DATA(xstrelm, y)
              Else
                 VARIANCES(xstrelm, y) = 1.0e20
              End If
 
           End Do
 
           Do y = ystrelm + 1, yendelm - 1
 
              If (DATA(xendelm, y) .Lt. lower_limit) Then
                 VARIANCES(xendelm, y) = minimum_variance
              Else If (DATA(xendelm, y) .Le. upper_limit) Then
                 VARIANCES(xendelm, y) = scaling_constant * DATA(xendelm, y)
              Else
                 VARIANCES(xendelm, y) = 1.0e20
              End If
 
           End Do
 
        Else
           Write (*, '(''Scaling constant can not be calculated '')')
        End If
 
     End If
 
     End Subroutine F2D_VARIANCES
!********1*********2*********3*********4*********5*********6*********7*********8
