!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************
!  *              *
!  * f2d_fuji.f90 *
!  *              *
!  ****************
 
!+ F2D_FUJI - Fit 2-D FUJI intensity conversion
     Subroutine F2D_FUJI (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, VARIANCES, status)
!  Description:
!    Converts intensities as read in from a Fuji image plate reader to a linear 
!    scale. The original values are stored as an offset log_10 value. The 
!    conversion is:
!
!    logarithmic form. To linearise the output intensities the following formula
!    is applied:
!
!    PSL/mm**2 = 100 * (4000/S) * 10**L(D_xy/2**b - 0.5)
!
!    where L is the latitude
!    S is the sensitivity
!    b number of bits
!    D_xy is the stored integer pixel value
!    These values are given in the .inf file.
!
!    (lin_value = 10**(log_value * log_10(4000) / 1023.0) was used previously.)
!
!  Method:
!    Basic arithmetic
!  Deficiencies:
!    ERROR PROPAGATION IS NOT PERFORMED
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    06-Jul-1998: V0.3 Update formula so that user can enter latitude etc., 
!      and get full formula (so far as I know) (Hammersley)
!    05-Oct-1993: V0.2 Correct scaling by using Log10 instead of Log(e) 
!      (Hammersley)
!    15-Sep-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of active data region
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of active data region
     Integer, Intent(IN) :: xendelm ! Defines X-end of active data region
     Integer, Intent(IN) :: yendelm ! Defines Y-end of active data region
     Logical, Intent(IN) :: variances_exist ! .True., if the variances array
!      exists
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data array
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! Variance array
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer, Save :: num_bits = 16 ! Number of bits used to digitise pixel
!      intensities
     Integer, Save :: sensitivity = 10000 ! Fuji "sensitivity" parameter
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for X-direction
     Real, Save :: latitude = 5.0 ! Fuji "latitude" parameter
     Real :: normalise ! Normalisation constant for linearisation of input 
!      pixel values
!  Local Arrays:
     Real :: LINEAR(0: 65535) ! Linearisation look-up table from input pixel 
!      value to linearised intensity
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FUJI ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_FUJI ' // Version)
 
     Else
 
!     Output warning message
        Call IO_WRITE (' ', status)
        Call IO_WRITE ('WARNING: User input has changed ', status)
        Call IO_WRITE (' ', status)
 
!     Output information on linearisation
        Call IO_WRITE ('The linearisation is performed ' // &
          'according to the formula:', status)
        Call IO_WRITE (' ', status)
        Call IO_WRITE ('OUT(x, y) = 100 * (4000 / S) * ' // &
          '10**L( IN(x, y) / 2**b - 0.5)', status)
        Call IO_WRITE (' ', status)
        Call IO_WRITE ('where L is the latitude', status)
        Call IO_WRITE ('      S is the sensitivity', status)
        Call IO_WRITE ('      b number of bits', status)
        Call IO_WRITE (' ', status)
        Call IO_WRITE ('These values are given in the .inf (or .INF) file.', &
          status)
 
!     Input linearisation parameters
        Call IO_INPI (.True., 1, 1000000, .True., 'SENSITIVITY', 1, &
          'Enter sensitivity (S in linearisation formula)', 1, &
          'Enter integer value within given range', sensitivity, status)
        Call IO_INPR (.True., 1.0, 20.0, .True., 'LATITUDE', 1, &
          'Enter latitude (L in linearisation formula)', 1, &
          'Enter real value within given range', latitude, status)
        Call IO_INPI (.True., 8, 32, .True., 'NUMBER OF BITS', 1, &
          'Enter number of bits (b in linearisation formula)', 1, &
          'Enter integer value within given range', num_bits, status)
 
!     Check status
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Fill linearisation table
        normalise = 2.0**num_bits
        Do x = 0, 2**num_bits - 1
           LINEAR(x) = 100.0 * (4000.0 / sensitivity) * &
             10.0**(latitude*(Real(x) / normalise - 0.5))
        End Do
 
!     Linearise data values
        Do y = ystrelm, yendelm
 
           Do x = xstrelm, xendelm
              DATA(x, y) = LINEAR(Nint(DATA(x, y)))
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_FUJI
!********1*********2*********3*********4*********5*********6*********7*********8
