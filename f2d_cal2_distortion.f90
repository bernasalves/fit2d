!********1*********2*********3*********4*********5*********6*********7**
 
!  ***************************
!  *                         *
!  * f2d_cal2_distortion.f90 *
!  *                         *
!  ***************************
 
!+ F2D_CAL2_DISTORTION - FIT 2-D CALculate (2) DISTORTION
     Subroutine F2D_CAL2_DISTORTION (mxstrelm, mystrelm, mxendelm, &
       myendelm, x_low, y_low, x_up, y_up, xnumknots, ynumknots, LAMBDA, MU, &
       COEFFS, mxmaxdat, mymaxdat, XAXIS, YAXIS, MDATA, status)
!  Description:
!    Calculates distortion spline at regular intervals.
!  Method:
!    "MA_CAL_2DNCUBSPLINE" is used to calculate the distortion values.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    31-Mar-2006: V0.6 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    16-Dec-1996: V0.5 Avoid open strings crossing lines (Hammersley)
!    02-Sep-1996: V0.4 Remove call to NAG routine (Hammersley)
!    28-Aug-1996: V0.3 Option to use FITPACK routines (Hammersley)
!    04-Jan-1996: V0.2 Remove call to "Secnds" (Hammersley)
!    28-Apr-1994: V0.1 Original, (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: mxstrelm ! Starting X-element of region
     Integer, Intent(IN) :: mystrelm ! Starting Y-element of region
     Integer, Intent(IN) :: mxendelm ! End X-element of region
     Integer, Intent(IN) :: myendelm ! End Y-element of region
     Integer, Intent(IN) :: x_low ! Lower X-pixel limit of spline region
     Integer, Intent(IN) :: y_low ! Lower Y-pixel limit of spline region
     Integer, Intent(IN) :: x_up ! Upper X-pixel limit of spline  region
     Integer, Intent(IN) :: y_up ! Upper Y-pixel limit of spline region
     Integer, Intent(IN) :: xnumknots ! Number of spline "knots" used for
!      X-direction
     Integer, Intent(IN) :: ynumknots ! Number of spline "knots" used for
!      Y-direction
     Real, Intent(IN) :: LAMBDA(xnumknots) ! X-Positions of spline "knots"
     Real, Intent(IN) :: MU(ynumknots) ! Y-Positions of spline "knots"
     Real, Intent(IN) :: COEFFS((xnumknots - 4) * (ynumknots - 4))
!      Coefficients of spline function
     Integer, Intent(IN) :: mxmaxdat ! First dimension of output array
     Integer, Intent(IN) :: mymaxdat ! Second dimension of output array
     Real, Intent(IN) :: XAXIS(mxmaxdat) ! Array containing data X-coordinates
     Real, Intent(IN) :: YAXIS(mymaxdat) ! Array containing data Y-coordinates
!  Export:
     Real, Intent(OUT) :: MDATA(mxmaxdat, mymaxdat) ! Array containing
!      calculated distortion values
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer stat ! Status return variable for "Allocate"
     Integer :: xmax_work ! Dimension of "X_WORK"
     Integer :: ymax_work ! Dimension of "Y_WORK"
     Real :: time ! Time to perform operation in seconds
     Real :: time_cpu ! CPU Time of process
     Real :: timend ! Time at end of operation
     Real :: timstr ! Time at start of operation
!  Local Arrays:
     Integer, Allocatable :: X_IWORK(:) ! Dynamic work array
     Real, Allocatable :: X_WORK(:) ! Dynamic work array
     Integer, Allocatable :: Y_IWORK(:) ! Dynamic work array
     Real, Allocatable :: Y_WORK(:) ! Dynamic work array
!--------1---------2---------3---------4---------5---------6---------7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(/1x,''Subroutine F2D_CAL2_DISTORTION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CAL2_DISTORTION ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably defined
     If (mxmaxdat .Lt. 1) Then
        status = St_bad_dim1
     Else If (mymaxdat .Lt. 1) Then
        status = St_bad_dim2
     Else If (mxstrelm .Le. 0 .Or. mxstrelm .Gt. mxendelm .Or. mxendelm .Gt. &
       mxmaxdat) Then
        status = St_bad_adr1
     Else If (mystrelm .Le. 0 .Or. mystrelm .Gt. myendelm .Or. myendelm .Gt. &
       mymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
 
!     Add module number to status value
        status = St_mod_fit2d + status
 
!     Save details of subroutine call
        Call ST_SAVE ('Subroutine F2D_CAL2_DISTORTION ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Calculate size of work arrays for "MA_CAL_2DNCUBSPLINE"
        xmax_work = x_up * 4
        ymax_work = y_up * 4
        Allocate (X_WORK(xmax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CAL2_DISTORTION ' // Version)
           Return
        End If
        Allocate (Y_WORK(ymax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CAL2_DISTORTION ' // Version)
           Return
        End If
        Allocate (X_IWORK(xmax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CAL2_DISTORTION ' // Version)
           Return
        End If
        Allocate (Y_IWORK(ymax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CAL2_DISTORTION ' // Version)
           Return
        End If

!     Store start time
        Call IO_TIMES (timstr, time_cpu, status)
 
        Call MA_CAL_2DNCUBSPLINE (1, xnumknots, xnumknots, LAMBDA, ynumknots, &
          ynumknots, MU, (xnumknots - 4) * (ynumknots - 4), COEFFS, mxmaxdat, &
          XAXIS, mymaxdat, YAXIS, x_low, y_low, x_up, y_up, xmax_work, &
          ymax_work, MDATA, X_WORK, Y_WORK, X_IWORK, Y_IWORK, status)
 
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Time for operation
        Call IO_TIMES (timend, time_cpu, status)
        time = timend - timstr
        Write (message, &
          '(''INFO: Time for calculation = '', f12.2, '' seconds'')') time
        Call IO_WRITE (message, status)
 
!     Free dynamic array space
        Deallocate (X_WORK)
        Deallocate (X_IWORK)
        Deallocate (Y_WORK)
        Deallocate (Y_IWORK)
 
     End If
 
     End Subroutine F2D_CAL2_DISTORTION
!********1*********2*********3*********4*********5*********6*********7**
 
 
