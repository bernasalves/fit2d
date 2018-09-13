!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_selectpixel.f90 *
!  *                     *
!  ***********************
 
!+ F2D_SELECTPIXEL - Fit 2-D SELECTed PIXEL value range operations
     Subroutine F2D_SELECTPIXEL (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, status)
!  Description:
!    Performs operation on a selected pixel subset which have input values in a
!    given range.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    23-Feb-1999: V0.4 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.3 Change to use IO internal database routines (Hammersley)
!    22-Mar-1996: V0.2 Use overload value as default decision level (Hammersley)
!    18-Nov-1994: V0.1 Original (Hammersley)
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
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data array
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Integer :: db_stat ! Data Store status return variable
     Integer :: x ! Loop variable
     Integer :: y ! Loop variable
     Logical, Save :: greater_than = .True. ! .True., if the operation is to be
!      carried out on pixels with input values greater than an input operand, 
!      otherwise the comparision is a less than
     Logical, Save :: multiplication = .True. ! .True., if the operation is
!      multiplication
     Real, Save :: level = 16382.5 ! Decision level
     Real :: overload ! Overloaded or saturated intensity value for the input 
!      data
     Real, Save :: operand = 0.0 ! Operation operand
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(2)
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SELECTPIXEL ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_SELECTPIXEL ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Obtain overloaded intensity value from internal data-base
        Call IO_INQ_RKEYVALUE ('#OVERLOAD_VALUE', overload, db_stat, status)
 
!     Input comparison type
        MESSAGE(1) = '"YES" if pixels are to be selected by ' // &
          'a greater than comparison (exclusive),'
        MESSAGE(2) = '"NO" if pixels are to be selected by a ' // &
          'lesser than comparison (exclusive).'
        Call IO_INPL (.True., 0, 1, .True., 'GREATER THAN COMPARISON', 2, &
          MESSAGE, 1, 'Enter "YES" or "NO"', greater_than, status)
 
        If (greater_than) Then
           level = overload
        End If
 
!     Input cut-off level
        Call IO_INPR (.False., 0.0, 0.0, .True., 'DECISION PIXEL VALUE', 1, &
          'Input pixel value for comparison', 1, 'Enter real number', level, &
          status)
 
!     Operation type
        MESSAGE(1) = '"YES" if the pixel values are to be ' // &
          'multiplied by a constant, "NO" if a'
        MESSAGE(2) = 'constant is to be added to the selected pixels.'
        Call IO_INPL (.True., 0, 1, .True., &
          'MULTIPLICATION ("NO" = ADDITION)', 2, MESSAGE, 1, &
          'Enter "YES" or "NO"', multiplication, status)
 
!     Operand
        Call IO_INPR (.False., 0.0, 0.0, .True., 'OPERAND VALUE', 1, &
          'Input value to multiply or add to pixel values', 1, &
          'Enter real number', operand, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Check for user escape
        If (status .Eq. St_goodvalue) Then
 
           If (greater_than) Then
 
              Do y = ystrelm, yendelm
 
                 Do x = xstrelm, xendelm
 
                    If (DATA(x, y) .Gt. level) Then
 
                       If (multiplication) Then
                          DATA(x, y) = DATA(x, y) * operand
 
                       Else
                          DATA(x, y) = DATA(x, y) + operand
                       End If
 
                    End If
 
                 End Do
 
              End Do
 
           Else
 
              Do y = ystrelm, yendelm
 
                 Do x = xstrelm, xendelm
 
                    If (DATA(x, y) .Lt. level) Then
 
                       If (multiplication) Then
                          DATA(x, y) = DATA(x, y) * operand
 
                       Else
                          DATA(x, y) = DATA(x, y) + operand
                       End If
 
                    End If
 
                 End Do
 
              End Do
 
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_SELECTPIXEL
!********1*********2*********3*********4*********5*********6*********7*********8
