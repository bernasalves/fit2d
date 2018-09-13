!*********1*********2*********3*********4*********5*********6*********7**
 
!  *************************
!  *                       *
!  * f2d_1dinterpolate.f90 *
!  *                       *
!  *************************
 
!+ F2D_1DINTERPOLATE - Fit 2-D 1-D linear INTERPOLATion
     Subroutine F2D_1DINTERPOLATE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, XAXIS, YAXIS, DATA, status)
!  Description:
!    1-D linear interpolation correction on data values, from user input 
!    reference point to second correction point, which is given a correction 
!    constant
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    01-Mar-1999: V0.2 Add "ERROR" text array for F90 compliance (Hammersley)
!    09-Nov-1994: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use MA_LIB
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
!  Import/Export:
     Real, Intent(INOUT) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(INOUT) :: YAXIS(xmaxdat) ! Y-axis values
     Real, Intent(INOUT) :: DATA(xmaxdat,ymaxdat) ! Data array corrected
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Real, Save :: coord1 = 1.0 ! Reference coordinate
     Real, Save :: coord2 = 100.0 ! Scaling coordinate
     Real, Save :: value2 = 1.02 ! Scale value at second coordinate
!  Local Arrays:
     Character(Len = 80) :: ERROR(1) ! Error message text
     Character(Len = 80) :: MESSAGES(6) ! User text
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_1DINTERPOLATE ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (yendelm .Gt. 1) Then ! Temporary limit
        status = St_bad_int1
     Else If (xendelm .Gt. xmaxdat .Or. xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Gt. ymaxdat .Or. yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_1DINTERPOLATE ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Input reference coordinate
        MESSAGES(1) = 'Enter the relative scaling to apply at ' // &
          'the entered scaling coordinate.'
        MESSAGES(2) = 'This is relative to 1.0 at the reference ' // &
          'coordinate.e.g. If you want'
        MESSAGES(3) = 'to increase values linearly between ' // &
          'coordinates x=1 and x = 100, such'
        MESSAGES(4) = 'that at x=100 the value is multiplied ' // &
          'by 1.1, (and 1.0 at x=1, by a'
        MESSAGES(5) = 'linearly proportional value inbetween) ' // &
          'You should enter: 1, 100, 1.1'
        MESSAGES(6) = 'to the three prompts.'
        ERROR(1) = 'Enter real number'
        Call IO_INPR (.False., 0.0, 0.0, .True., 'REFERENCE X-COORDINATE', 6, &
          MESSAGES, 1, ERROR, coord1, status)
 
!     Input scaling coordinate
        Call IO_INPR (.False., 0.0, 0.0, .True., 'SCALING POINT X-COORDINATE', &
          6, MESSAGES, 1, ERROR, coord2, status)
 
!     Input scaling value
        Call IO_INPR (.False., 0.0, 0.0, .True., &
          'RELATIVE SCALING VALUE AT SCALING COORDINATE', 6, MESSAGES, 1, &
          ERROR, value2, status)
 
!     Check status value
        If (status .Eq. St_goodvalue) Then
 
!        Apply 1-D linear interpolated correction
           Call MA_1DINTERCORR (xmaxdat, xstrelm, xendelm, coord1, coord2, &
             value2, XAXIS, DATA, status)
 
        End If
 
     End If
 
     End Subroutine F2D_1DINTERPOLATE
!********1*********2*********3*********4*********5*********6*********7**
 
