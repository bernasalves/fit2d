!*********1*********2*********3*********4*********5*********6*********7********8
 
!  ************************
!  *                      *
!  * f2d_centreofmass.f90 *
!  *                      *
!  ************************
 
!+ F2D_CENTREOFMASS - Fit2D: calculate CENTRE OF MASS
     Subroutine F2D_CENTREOFMASS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, retstat, x_com, y_com, status)
!  Description:
!    Calculate of two 1-D centre of masses, taking into account very simple 
!    1-D estimated sloping backgrounds
!  Keywords:
!    Centre~of~Mass.Calculate, Calculate.Centre~of~Mass
!  Method:
!    The average values on each of the edges is found, and these are used to 
!    interpolate 1-D linear background values for the X and Y-directions
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    22-Mar-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First Dimension size for "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second Dimension size for "DATA"
     Integer, Intent(IN) :: xstrelm ! First X-element of data region
     Integer, Intent(IN) :: ystrelm ! First Y-element of data region
     Integer, Intent(IN) :: xendelm ! Last X-element of data region
     Integer, Intent(IN) :: yendelm ! Last Y-element of data region
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Array containing data
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status:
!      0 = Good status
!      1 = Bad status, not possible owing to zero normalisation, probably no
!          peak within the given region
     Real, Intent(OUT) :: x_com ! X-coordinate of the centre of mass
     Real, Intent(OUT) :: y_com ! Y-coordinate of the centre of mass
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Real :: background ! Interpolate linear background estimate
     Real :: data_value ! Background subtracted data value
     Real :: height ! Height of search region
     Real :: left_edge ! Average value on left edge of region
     Real :: lower_edge ! Average value on lower edge of region
     Real :: normalise ! Normalisation factor
     Real :: position ! Position of mass
     Real :: right_edge ! Average value on right edge of region
     Real :: total ! Total sum of data values
     Real :: upper_edge ! Average value on upper edge of region
     Real :: weight ! Sum of mass weights
     Real :: width ! Width of search region
     Real :: x_slope ! Estimated background slope in X-direction
     Real :: y_slope ! Estimated background slope in Y-direction
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_CENTREOFMASS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status.
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CENTREOFMASS ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xendelm .Gt. xmaxdat .Or. xstrelm .Gt. &
       xendelm) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. yendelm .Gt. ymaxdat .Or. ystrelm .Gt. &
       yendelm) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_CENTREOFMASS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input parameters appear to be correct
        width = xendelm - xstrelm + 1
        height = yendelm - ystrelm + 1
 
!     Find average edge values
        total = 0.0
        Do y = ystrelm, yendelm
           total = total + DATA(xstrelm, y)
        End Do
        left_edge = total / height
 
        total = 0.0
        Do y = ystrelm, yendelm
           total = total + DATA(xendelm, y)
        End Do
        right_edge = total / height
 
        total = 0.0
        Do x = xstrelm, xendelm
           total = total + DATA(x, ystrelm)
        End Do
        lower_edge = total / width
 
        total = 0.0
        Do x = xstrelm, xendelm
           total = total + DATA(x, yendelm)
        End Do
        upper_edge = total / width
 
!     Calculate background slopes
        x_slope = (right_edge - left_edge) / (width - 1.0)
        y_slope = (upper_edge - lower_edge) / (height - 1.0)
 
!     Calculate 1-D centre of masses, subtracting estimated linear background
        weight = 0.0
        normalise = 0.0
        Do x = xstrelm, xendelm
 
           background = left_edge + x_slope * Real(x - xstrelm)
           position = Real(x) - 0.5
           Do y = ystrelm, yendelm
 
              data_value = DATA(x, y) - background
              weight = weight + data_value * position
              normalise = normalise + data_value
 
           End Do
 
        End Do
 
        If (normalise .Gt. 0.0) Then
           x_com = weight / normalise
        Else
           retstat = -1
        End If
 
        weight = 0.0
        normalise = 0.0
        Do y = ystrelm, yendelm
 
           background = lower_edge + y_slope * Real(y - ystrelm)
           position = Real(y) - 0.5
           Do x = xstrelm, xendelm
 
              data_value = DATA(x, y) - background
              weight = weight + data_value * position
              normalise = normalise + data_value
 
           End Do
 
        End Do
 
        If (normalise .Gt. 0.0) Then
           y_com = weight / normalise
        Else
           retstat = -1
        End If
 
     End If
 
     End Subroutine F2D_CENTREOFMASS
!*********1*********2*********3*********4*********5*********6*********7********8
 
