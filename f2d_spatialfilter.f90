!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_spatialfilter.f90 *
!  *                       *
!  *************************
 
!+ F2D_SPATIALFILTER - Fit 2-D SPATIAL FILTERing
     Subroutine F2D_SPATIALFILTER (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MDATA, status)
!  Description:
!    Spatial filter of "DATA(xmaxdat,ymaxdat)" in the region "(xstrelm, 
!    ystrelm)" to "(xendelm, yendelm)" with a choice of filters
!  Method:
!    Uses "MA_SCONVOLUTION".
!  Deficiencies:
!    No error propagation.
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    13-Dec-1996: V0.2 Option of user defined coefficients (Hammersley)
!    27-May-1988: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of ROI
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of ROI
     Integer, Intent(IN) :: xendelm ! Defines X-end of ROI
     Integer, Intent(IN) :: yendelm ! Defines Y-end of ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array to be filtered
!  Export:
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Filtered data array
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
     Integer, Parameter :: xmaxpsf = 3 ! First Dimension size of "PSF"
     Integer, Parameter :: ymaxpsf = 3 ! Second Dimension size of "PSF"
     Real, Parameter :: Ninth = 1.0 / 9.0 ! 1.0 / 9.0
!  Local Variables:
     Character(Len = 80) :: prompt ! Prompt for user input
     Character(Len = 80) :: message ! User message
     Integer, Save :: filter_type = 3 ! Type of filter 1 is the least
!      filtering, 3 the most, 4 is user defined
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Logical, Save :: low_pass = .True. ! .True., if the filtering is to be
!      low pass
!  Local Arrays:
     Real, Save :: PSF(xmaxpsf, ymaxpsf) ! Point spread function array for
!      spatial filtering
!  Local Data:
     Data PSF / Ninth, Ninth, Ninth, Ninth, Ninth, Ninth, Ninth, Ninth, Ninth /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SPATIALFILTER ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_SPATIALFILTER ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Type of filtering
        Call IO_INPL (.True., 0, 1, .True., 'LOW-PASS FILTER', 1, &
          '"YES" for low-pass filtering, "NO" for high-pass filtering', &
          1, 'Enter "YES" or "NO"', low_pass, status)
 
        Call IO_INPI (.True., 1, 4, .True., 'FILTER TYPE', 1, &
          'Type 1 filters the least, type 3 ' // &
          'the most: (4 =user defined)', 1, &
          'Enter integer within given range', filter_type, status)
 
!     Set spatial filter elements
        If (filter_type .Eq. 4) Then
 
           Do y = 1, 3
 
              Do x = 1, 3
 
                 Write (prompt, '(''ENTER COEFFICIENT '', 2i2)') x, y
                 Write (message, '(''Enter value of ' // &
                   'convolution coefficient '', 2i2)') x, y
                 Call IO_INPR (.False., 0.0, 0.0, .True., prompt, 1, message, &
                   1, 'Enter real value', PSF(x, y), status)
 
              End Do
 
           End Do
 
        Else If (low_pass) Then
 
           If (filter_type .Eq. 1) Then
 
              PSF(2, 2) = 4.0 / 16.0
              PSF(1, 2) = 2.0 / 16.0
              PSF(2, 1) = 2.0 / 16.0
              PSF(2, 3) = 2.0 / 16.0
              PSF(3, 2) = 2.0 / 16.0
              PSF(1, 3) = 1.0 / 16.0
              PSF(3, 1) = 1.0 / 16.0
              PSF(3, 3) = 1.0 / 16.0
              PSF(1, 1) = 1.0 / 16.0
 
           Else If (filter_type .Eq. 2) Then
 
              Do y = 1, 3
 
                 Do x = 1, 3
                    PSF(x, y) = 1.0 / 10.0
                 End Do
 
              End Do
 
              PSF(2, 2) = 2.0 / 10.0
 
           Else If (filter_type .Eq. 3) Then
 
              Do y = 1, 3
 
                 Do x = 1, 3
                    PSF(x, y) = 1.0 / 9.0
                 End Do
 
              End Do
 
           End If
 
        Else
 
!        High pass filter
           If (filter_type .Eq. 1) Then
              PSF(2, 2) = 5.0
              PSF(1, 2) = -2.0
              PSF(2, 1) = -2.0
              PSF(2, 3) = -2.0
              PSF(3, 2) = -2.0
              PSF(1, 3) = 1.0
              PSF(3, 1) = 1.0
              PSF(3, 3) = 1.0
              PSF(1, 1) = 1.0
 
           Else If (filter_type .Eq. 2) Then
 
              PSF(2, 2) = 19.0 / 7.0
              PSF(1, 2) = -2.0 / 7.0
              PSF(2, 1) = -2.0 / 7.0
              PSF(2, 3) = -2.0 / 7.0
              PSF(3, 2) = -2.0 / 7.0
              PSF(1, 3) = -1.0 / 7.0
              PSF(3, 1) = -1.0 / 7.0
              PSF(3, 3) = -1.0 / 7.0
              PSF(1, 1) = -1.0 / 7.0
 
           Else If (filter_type .Eq. 3) Then
 
              PSF(2, 2) = 5.0
              PSF(1, 2) = -1.0
              PSF(2, 1) = -1.0
              PSF(2, 3) = -1.0
              PSF(3, 2) = -1.0
              PSF(1, 3) = 0.0
              PSF(3, 1) = 0.0
              PSF(3, 3) = 0.0
              PSF(1, 1) = 0.0
 
           End If
 
        End If
 
!     Perform spatial filtering on ROI as a convolution
        Call MA_SCONVOLUTION (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, &
          xendelm, yendelm, xmaxpsf, ymaxpsf, 3, 3, PSF, xmaxdat, ymaxdat, &
          MDATA, status)
 
     End If
 
     End Subroutine F2D_SPATIALFILTER
!********1*********2*********3*********4*********5*********6*********7*********8
 
