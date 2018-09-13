!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_cal_distortion.f90 *
!  *                        *
!  **************************
 
!+ F2D_CAL_DISTORTION - FIT 2-D CALculate DISTORTION function
     Subroutine F2D_CAL_DISTORTION (x_minimum, y_minimum, x_maximum, &
       y_maximum, x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
       y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, xmaxdat, ymaxdat, &
       XAXIS, YAXIS, xstrelm, ystrelm, xendelm, yendelm, MXAXIS, MYAXIS, &
       MDATA, mxstrelm, mystrelm, mxendelm, myendelm, mtitle, status)
!  Description:
!    Calculates 2-D fitted distortion spline function for every pixel in the
!    ROI, for either the X or Y-distortion. The result is output in the memory.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    31-Mar-2006: V0.4 Remove "fitpack" argument (Hammersley)
!    16-Dec-1996: V0.3 Avoid open strings crossing lines (Hammersley)
!    28-Aug-1996: V0.2 Option to use FITPACK routines (Hammersley)
!    28-Apr-1994: V0.1 Original, (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: x_minimum ! Minimum X-value applicable to spline
!      interpolation
     Real, Intent(IN) :: y_minimum ! Minimum Y-value applicable to spline
!      interpolation
     Real, Intent(IN) :: x_maximum ! Maximum X-value applicable to spline
!      interpolation
     Real, Intent(IN) :: y_maximum ! Maximum Y-value applicable to spline
!      interpolation
     Integer, Intent(IN) :: x_xnumknots ! Number of spline "knots" used for
!      X-direction for X-distortion function
     Integer, Intent(IN) :: x_ynumknots ! Number of spline "knots" used for
!      Y-direction of X-distortion function
     Real, Intent(IN) :: X_LAMBDA(x_xnumknots) ! X-Positions of spline
!      "knots" for X-distortion function
     Real, Intent(IN) :: X_MU(x_ynumknots) ! Y-Positions of spline
!      "knots" for X-distortion function
     Real, Intent(IN) :: X_COEFFS((x_xnumknots - 4) * (x_ynumknots - 4))
!      Coefficients of spline function for X-distortion function
     Integer, Intent(IN) :: y_xnumknots ! Number of spline "knots" used for
!      X-direction for Y-distortion function
     Integer, Intent(IN) :: y_ynumknots ! Number of spline "knots" used for
!      Y-direction of Y-distortion function
     Real, Intent(IN) :: Y_LAMBDA(y_xnumknots) ! X-Positions of spline
!      "knots" for Y-distortion function
     Real, Intent(IN) :: Y_MU(y_ynumknots) ! Y-Positions of spline
!      "knots" for Y-distortion function
     Real, Intent(IN) :: Y_COEFFS((y_xnumknots - 4) * (y_ynumknots - 4))
!      Coefficients of spline function for Y-distortion function
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Array containing data
!      X-coordinates
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Array containing data
!      Y-coordinates
     Integer, Intent(IN) :: xendelm ! End X-element of region to be fitted
     Integer, Intent(IN) :: xstrelm ! Starting X-element of region to be fitted
     Integer, Intent(IN) :: yendelm ! End Y-element of region to be fitted
     Integer, Intent(IN) :: ystrelm ! Starting Y-element of region to be fitted
!  Import/Export:
     Real, Intent(INOUT) :: MXAXIS(xmaxdat) ! Array containing data
!      X-coordinates
     Real, Intent(INOUT) :: MYAXIS(ymaxdat) ! Array containing data
!      Y-coordinates
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat) ! Array containing data
!      to be fitted
     Integer, Intent(INOUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(INOUT) :: mxstrelm ! Starting X-element of memory data
!      region
     Integer, Intent(INOUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(INOUT) :: mystrelm ! Starting Y-element of memory data
!      region
     Character(Len = *), Intent(INOUT) :: mtitle ! Title for output array
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: x ! Loop variable
     Integer :: x_low ! Lower X-pixel limit of spline region
     Integer :: x_up ! Upper X-pixel limit of spline  region
     Integer :: y ! Loop variable
     Integer :: y_low ! Lower Y-pixel limit of spline region
     Integer :: y_up ! Upper Y-pixel limit of spline region
     Logical :: x_display ! .True., if the distortion for the X-direction is to
!      be calculated, otherwise the distortion for the Y-direction is calculated
     Real :: temp ! Temporary storage variable
!  Local Arrays:
!  External Functions:
!  Local Data:
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_CAL_DISTORTION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_CAL_DISTORTION ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably defined
     If (xmaxdat .Lt. 1) Then
        status = St_bad_dim1
     Else If (ymaxdat .Lt. 1) Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xstrelm .Gt. xendelm .Or. xendelm .Gt. &
       xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. ystrelm .Gt. yendelm .Or. yendelm .Gt. &
       ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
 
!     Add module number to status value
        status = St_mod_fit2d + status
 
!     Save details of subroutine call
        Call ST_SAVE ('Subroutine F2D_CAL_DISTORTION ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Inquire if the distortion function is to be calculated for the X or 
!     Y-distortions
        Call IO_INPL (.True., 0, 1, .True., 'CALCULATE X-DISTORTION', 1, &
          'Enter "YES" for X-distortion, "NO" for Y-distortion ', 1, &
          'Value must be within given range', x_display, status)
 
!     Find limits of valid pixel region (may be smaller than required ROI region
        Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, x_minimum, temp, status)
        x_low = Int(temp) + 1
        Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, x_maximum, temp, status)
        x_up = Int(temp)
        Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, y_minimum, temp, status)
        y_low = Int(temp) + 1
        Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, y_maximum, temp, status)
        y_up = Int(temp)
 
!     Set rebinning region to be the lesser of the ROI and the valid region
        If (xstrelm .Lt. x_low .Or. ystrelm .Lt. y_low .Or. xendelm .Gt. x_up &
          .Or. yendelm .Gt. y_up) Then
           Call IO_WRITE ('WARNING: Region of Interest ' // &
             '(ROI) extends outside valid region of', status)
           Call IO_WRITE ('spline. Pixels outside the ' // &
             'valid region will be ignored.', status)
           Call IO_WRITE ('INFO: The valid region of ' // &
             'the spline correction function is:', status)
           Write (message, '(''      ('', f8.2, '', '', f8.2, ' // &
             ''') to ('', f8.2, '', '', f8.2, '')'')') x_minimum, y_minimum, &
             x_maximum, y_maximum
           Call IO_WRITE (message, status)
 
        End If
 
!     Set Intersection of ROI and valid spline region
        x_low = Max(x_low, xstrelm)
        y_low = Max(y_low, ystrelm)
        x_up = Min(x_up, xendelm)
        y_up = Min(y_up, yendelm)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Set output ROI
        mxstrelm = xstrelm
        mystrelm = ystrelm
        mxendelm = xendelm
        myendelm = yendelm
 
!     Calculate fitted distortion function
        If (x_display) Then
 
           Call F2D_CAL2_DISTORTION (mxstrelm, mystrelm, mxendelm, myendelm, &
             x_low, y_low, x_up, y_up, x_xnumknots, x_ynumknots, &
             X_LAMBDA, X_MU, X_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, MDATA, &
             status)
           mtitle = 'X-Distortion Spline Function'
        Else
 
           Call F2D_CAL2_DISTORTION (mxstrelm, mystrelm, mxendelm, myendelm, &
             x_low, y_low, x_up, y_up, y_xnumknots, y_ynumknots, &
             Y_LAMBDA, Y_MU, Y_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, MDATA, &
             status)
           mtitle = 'Y-Distortion Spline Function'
 
        End If
 
!     Create axis values
        Do x = 1, mxendelm
           MXAXIS(x) = XAXIS(x)
        End Do
        Do y = 1, myendelm
           MYAXIS(y) = YAXIS(y)
        End Do
 
     End If
 
     End Subroutine F2D_CAL_DISTORTION
!********1*********2*********3*********4*********5*********6*********7*********8
