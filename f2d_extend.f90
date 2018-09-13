!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_extend.f90 *
!  *                *
!  ******************
 
!+ F2D_EXTEND - Fit 2-D EXTEND data region
     Subroutine F2D_EXTEND (gui, xmaxdat, ymaxdat, variance_exist, xnumdat, &
       ynumdat, xstrelm, ystrelm, xendelm, yendelm, XAXIS, YAXIS, DATA, MASK, &
       VARIANCES, status)
!  Description:
!    Allows user to define larger data region.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    06-Jan-2015: V0.2 Add "MASK" (Hammersley)
!    12-Mar-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used, (image will not be draw)
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Logical, Intent(IN) :: variance_exist ! .True., if a data variance
!      array is created
!  Import/Export:
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements defined in
!      Y-direction
     Integer, Intent(INOUT) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(INOUT) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(INOUT) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(INOUT) :: yendelm ! Defines Y-end of region of interest
     Real, Intent(INOUT) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(INOUT) :: YAXIS(ymaxdat) ! Y-axis values
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! The data mask
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! The estimated
!      variance values
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: elem ! Loop variable for elements
     Integer :: x_extend ! Size of extended region horizontally in pixels
     Integer :: y_extend ! Size of extended region vertically in pixels
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_EXTEND ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_EXTEND ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
        x_extend = xmaxdat
        y_extend = ymaxdat
        If (gui) Then
 
!        Size in X-direction
           Call GS_INPI (.True., xnumdat, xmaxdat, .True., &
             'X-NUMBER OF PIXELS', 1, &
             'Enter size of extended data region horizontally', 1, &
             'Enter a valid integer within given range', x_extend, status)
 
!        Size in Y-direction
           Call GS_INPI (.True., ynumdat, ymaxdat, .True., &
             'Y-NUMBER OF PIXELS', 1, &
             'Enter size of extended data region vertically', 1, &
             'Enter a valid integer within given range', y_extend, status)
 
        Else
 
!        Size in X-direction
           Call IO_INPI (.True., xnumdat, xmaxdat, .True., &
             'X-NUMBER OF PIXELS', 1, &
             'Enter size of extended data region horizontally', 1, &
             'Enter a valid integer within given range', x_extend, status)
 
!        Size in Y-direction
           Call IO_INPI (.True., ynumdat, ymaxdat, .True., &
             'Y-NUMBER OF PIXELS', 1, &
             'Enter size of extended data region vertically', 1, &
             'Enter a valid integer within given range', y_extend, status)
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Set extended pixels to zero
        If (x_extend .Gt. xnumdat) Then
 
!        Set all pixels to zero in band from "xnumdat + 1" to
!        "x_extend" and from 1 to "ynumdat"
           Call MA_RVALUE (xmaxdat, ymaxdat, xnumdat + 1, 1, x_extend, &
             ynumdat, 0.0, DATA, status)
           Call MA_L1VALUE (xmaxdat, ymaxdat, xnumdat + 1, 1, x_extend, &
             ynumdat, .False., MASK, status)
 
        End If
 
        If (y_extend .Gt. ynumdat) Then
 
!        Set all pixels to zero in band from 1 to "x_extend"
!        and from "ynumdat + 1" to "y_extend"
           Call MA_RVALUE (xmaxdat, ymaxdat, 1, ynumdat + 1, x_extend, &
             y_extend, 0.0, DATA, status)
           Call MA_L1VALUE (xmaxdat, ymaxdat, 1, ynumdat + 1, x_extend, &
             y_extend, .False., MASK, status)
 
        End If
 
        If (variance_exist) Then
 
           If (x_extend .Gt. xnumdat) Then
 
!           Set all pixels to zero in band from "xnumdat + 1" to
!           "x_extend" and from 1 to "ynumdat"
              Call MA_RVALUE (xmaxdat, ymaxdat, xnumdat + 1, 1, x_extend, &
                ynumdat, 0.0, VARIANCES, status)
 
           End If
 
           If (y_extend .Gt. ynumdat) Then
 
!           Set all pixels to zero in band from 1 to "x_extend"
!           and from "ynumdat + 1" to "y_extend"
              Call MA_RVALUE (xmaxdat, ymaxdat, 1, ynumdat + 1, x_extend, &
                y_extend, 0.0, VARIANCES, status)
 
           End If
 
        End If
 
!     Extend axis values
        Do elem = xnumdat + 1, x_extend
           XAXIS(elem) = XAXIS(xnumdat) + Real(elem - xnumdat) * &
             (XAXIS(xnumdat) - XAXIS(xnumdat - 1))
        End Do
 
        Do elem = ynumdat + 1, y_extend
           YAXIS(elem) = YAXIS(ynumdat) + Real(elem - ynumdat) * &
             (YAXIS(ynumdat) - YAXIS(ynumdat - 1))
        End Do
 
!     Set output region values
        xnumdat = x_extend
        ynumdat = y_extend
        xendelm = x_extend
        yendelm = y_extend
 
     End If
 
     End Subroutine F2D_EXTEND
!********1*********2*********3*********4*********5*********6*********7*********8
 
