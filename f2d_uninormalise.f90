!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_uninormalise.f90 *
!  *                      *
!  ************************
 
!+ F2D_UNINORMALISE - Fit 2-D UNI-directional NORMALISE data
     Subroutine F2D_UNINORMALISE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, MASK, VARIANCES, maxwork, TOTAL, SUM, &
       MDATA, MVARIANCES, status)
!  Description:
!    Uni-normalises "DATA(xmaxdat,ymaxdat)" in the region "(xstrelm, ystrelm)"
!    to "(xendelm, yendelm)". (Error propagation if "variances_exist").
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    04-Jan-1996: V0.4 Changes for IBM AIX "xlf" compiler: Doesn't like logical
!      comparison using .Ne., must use .Neqv. instead (Hammersley)
!    28-Feb-1995: V0.3 Make mask elements single bytes (Hammersley)
!    07-Jun-1993: V0.2 Choice of X or Y normalisation (Hammersley)
!    04-Jun-1993: V0.1 Original (Hammersley)
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
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask
     Integer, Intent(IN) :: maxwork ! Dimension size for work arrays
!  Export:
     Integer, Intent(OUT) :: TOTAL(maxwork) ! Work array
     Real, Intent(OUT) :: SUM(maxwork) ! Work array, normalised profile
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Memory data
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat) ! Memory variance array
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Integer :: element ! Loop variable for either direction
     Integer :: endelm ! Last element for 1-D operations
     Integer :: strelm ! First element for 1-D operations
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Logical, Save :: x_direction = .True. ! .True., if normalisation is
!      for the X-direction
     Real :: maximum_value
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_UNINORMALISE ' // Version)
        Return
     End If
 
!  Check that the region is reasonably defined
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
        Call ST_SAVE ('Subroutine F2D_UNINORMALISE ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Inquire direction of normalisation
        Call IO_INPL (.True., 0, 1, .True., 'HORIZONTAL NORMALISATION', 1, &
          '"YES" if normalisation for horizontal ' // &
          'direction, "NO" for vertical', 1, 'Enter "YES" or "NO"', &
          x_direction, status)
 
!     Check size of work array
        If (x_direction .And. xendelm .Gt. maxwork .Or. (.Not. x_direction) &
          .And. yendelm .Gt. maxwork) Then
           Call IO_WRITE ('WARNING: Work arrays too small, '// &
             'operation not performed', status)
           Return
        End If
 
!     Define range
        If (x_direction) Then
           strelm = xstrelm
           endelm = xendelm
        Else
           strelm = ystrelm
           endelm = yendelm
        End If
 
!     Initialise total counts and summmation arrays
        Do element = strelm, endelm
           TOTAL(element) = 0
           SUM(element) = 0.0
        End Do
 
!     Calculate 1-D average profile, for non-masked elements
        If (x_direction) Then
 
           Do y = ystrelm, yendelm
 
              Do x = xstrelm, xendelm
 
                 If (MASK(x, y) .Eqv. .False.) Then
 
                    TOTAL(x) = TOTAL(x) + 1
                    SUM(x) = SUM(x) + DATA(x, y)
 
                 End If
 
              End Do
 
           End Do
 
        Else
 
           Do y = ystrelm, yendelm
 
              Do x = xstrelm, xendelm
 
                 If (MASK(x, y) .Eqv. .False.) Then
 
                    TOTAL(y) = TOTAL(y) + 1
                    SUM(y) = SUM(y) + DATA(x, y)
 
                 End If
 
              End Do
 
           End Do
 
        End If
 
!     Divide by number of summed elements to give 1-D profile
        Do element = strelm, endelm
 
           If (TOTAL(element) .Gt. 0) Then
              SUM(element) = SUM(element) / Real(TOTAL(element))
           Else
              SUM(element) = 0.0
           End If
 
        End Do
 
!     Find largest element in 1-D profile
        Call MA_RMAX (maxwork, 1, SUM, strelm, 1, endelm, 1, maximum_value, &
          status)
 
!     Divide by maximum value to give normalised 1-D profile
        Do element = strelm, endelm
 
           If (TOTAL(element) .Gt. 0) Then
              SUM(element) = SUM(element) / maximum_value
           Else
              SUM(element) = 1.0
           End If
 
        End Do
 
!     Normalise values of input array
        If (x_direction) Then
 
           Do y = ystrelm, yendelm
 
              Do x = xstrelm, xendelm
                 MDATA(x, y) = DATA(x, y) / SUM(x)
              End Do
 
           End Do
 
           If (variances_exist) Then
 
!           Normalise values of input array
              Do y = ystrelm, yendelm
 
                 Do x = xstrelm, xendelm
                    MVARIANCES(x, y) = VARIANCES(x, y) / SUM(x)**2
                 End Do
 
              End Do
 
           End If
 
        Else
 
           Do y = ystrelm, yendelm
 
              Do x = xstrelm, xendelm
                 MDATA(x, y) = DATA(x, y) / SUM(y)
              End Do
 
           End Do
 
           If (variances_exist) Then
 
!           Normalise values of input array
              Do y = ystrelm, yendelm
 
                 Do x = xstrelm, xendelm
                    MVARIANCES(x, y) = VARIANCES(x, y) / SUM(y)**2
                 End Do
 
              End Do
 
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_UNINORMALISE
!********1*********2*********3*********4*********5*********6*********7*********8
