!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_triangle.f90 *
!  *                  *
!  ********************
 
!+ F2D_TRIANGLE - Fit 2-D Add TRIANGLE
     Subroutine F2D_TRIANGLE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, status)
!  Description:
!    Adds a user defined triangle to "DATA(xmaxdat,ymaxdat)" in the region
!    "(xstrelm, ystrelm)" to "(xendelm, yendelm)".
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    13-Nov-2000: V0.1 Original (Hammersley)
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
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data array to be added to
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
     Integer, Parameter :: Max_lines = 4096 ! Maximum number of lines which
!      the triangle can extend
!  Local Variables:
     Integer :: x ! Loop variable for X-direction
     Integer :: x_max ! Maximum coordinate X
     Integer :: x_mid ! Middle coordinate X
     Integer :: x_min ! Minimum coordinate X
     Integer, Save :: x1 = 15 ! First X-coordinate
     Integer, Save :: x2 = 15 ! Second X-coordinate
     Integer, Save :: x3 = 5 ! Third X-coordinate
     Integer :: y ! Loop variable for Y-direction
     Integer :: y_max ! Maximum coordinate Y
     Integer :: y_mid ! Middle coordinate Y
     Integer :: y_min ! Minimum coordinate Y
     Integer :: y_out ! Y-pixel for output in the raster
     Integer, Save :: y1 = 2 ! First Y-coordinate
     Integer, Save :: y2 = 19 ! Second Y-coordinate
     Integer, Save :: y3 = 10 ! Third Y-coordinate
     Real, Save :: value = 1.0 ! Value to add
!  Local Arrays:
     Integer :: X_STARTS(Max_lines) ! Starting X-pixel for each line
     Integer :: X_ENDS(Max_lines) ! Ending X-pixel for each line
!    Commons:
     Common / WORK / X_STARTS, X_ENDS
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_TRIANGLE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_TRIANGLE ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     X-pixel number of first triangle vertex
        Call GS_INPI (.True., xstrelm, xendelm, .True., &
          'X-PIXEL FOR 1ST VERTEX', 1, &
          'Enter X-pixel number of first triangle vertex', 1, &
          'Enter integer number in range', x1, status)
 
!     Y-pixel number of first triangle vertex
        Call GS_INPI (.True., ystrelm, yendelm, .True., &
          'Y-PIXEL FOR 1ST VERTEX', 1, &
          'Enter Y-pixel number of first triangle vertex', 1, &
          'Enter integer number in range', y1, status)
 
 
!     X-pixel number of second triangle vertex
        Call GS_INPI (.True., xstrelm, xendelm, .True., &
          'X-PIXEL FOR 2ND VERTEX', 1, &
          'Enter X-pixel number of second triangle vertex', 1, &
          'Enter integer number in range', x2, status)
 
!     Y-pixel number of second triangle vertex
        Call GS_INPI (.True., ystrelm, yendelm, .True., &
          'Y-PIXEL FOR 2ND VERTEX', 1, &
          'Enter Y-pixel number of second triangle vertex', 1, &
          'Enter integer number in range', y2, status)
 
!     X-pixel number of third triangle vertex
        Call GS_INPI (.True., xstrelm, xendelm, .True., &
          'X-PIXEL FOR 3RD VERTEX', 1, &
          'Enter X-pixel number of third triangle vertex', 1, &
          'Enter integer number in range', x3, status)
 
!     Y-pixel number of third triangle vertex
        Call GS_INPI (.True., ystrelm, yendelm, .True., &
          'Y-PIXEL FOR 3RD VERTEX', 1, &
          'Enter Y-pixel number of third triangle vertex', 1, &
          'Enter integer number in range', y3, status)
 
!     Input value to add
        Call GS_INPR (.False., 0.0, 1.0, .True., 'ADDITION CONSTANT', 1, &
          'Enter real value to add to region of interest', 1, &
          'Enter real number', value, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        DATA(x1, y1) = DATA(x1, y1) + value
        DATA(x2, y2) = DATA(x2, y2) + value
        DATA(x3, y3) = DATA(x3, y3) + value
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
        If (value .Ne. 0.0) Then
 
!        Arrange coordinates by Y-order
           Call LG_RAS_ORDERY (x1, y1, x2, y2, x3, y3, x_min, y_min, x_mid, &
             y_mid, x_max, y_max, status)
 
!        Calculate X-limits of triangle
           Call LG_RAS_TRIXLIMITS (x_min, y_min, x_mid, y_mid, x_max, y_max, &
             Max_lines, X_STARTS, X_ENDS, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''y_min, y_max = '', 2i6)') y_min, y_max
!           Do y = 1, y_max - y_min + 1
!              Write (*, '(3i6)') y, X_STARTS(y), X_ENDS(y)
!           End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Check good status
           If (status .Eq. St_goodvalue) Then
 
!           Draw triangle in raster
              Do y = 1, y_max - y_min + 1
 
!              Calculate output Y
                 y_out = y + y_min - 1
 
                 If (y_out .Ge. ystrelm .And. y_out .Le. yendelm) Then
 
                    Do x = Max(X_STARTS(y), xstrelm), Min(X_ENDS(y), xendelm)
                       DATA(x, y_out) = DATA(x, y_out) + value
                    End Do
 
                 End If
 
              End Do
 
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_TRIANGLE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
