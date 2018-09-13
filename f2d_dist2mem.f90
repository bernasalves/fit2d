!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_dist2mem.f90 *
!  *                  *
!  ********************
 
!+ F2D_DIST2MEM - FIT 2-D DISTortion 2 MEMory
     Subroutine F2D_DIST2MEM (xmax_peaks, ymax_peaks, xnum_peaks, ynum_peaks, &
       X_DISTORTION, Y_DISTORTION, xmaxdat, ymaxdat, mxnumdat, mynumdat, &
       MXAXIS, MYAXIS, MDATA, mxstrelm, mystrelm, mxendelm, myendelm, mtitle, &
       mxlabel, mylabel, mzlabel, status)
!  Description:
!    Saves either X or Y-direction distortion in memory.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    03-Nov-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks ! Dimension size of "X_PEAKS" and
!      "Y_PEAKS"
     Integer, Intent(IN) :: ymax_peaks ! Dimension size of "X_PEAKS" and
!      "Y_PEAKS"
     Integer, Intent(IN) :: xnum_peaks ! Number of peaks in grid horizontally
     Integer, Intent(IN) :: ynum_peaks ! Number of peaks in grid vertically
     Real, Intent(IN) :: X_DISTORTION(xmax_peaks, ymax_peaks) ! X-distortion at
!      each peak (The distortion is measured as true position minus measured 
!      position)
     Real, Intent(IN) :: Y_DISTORTION(xmax_peaks, ymax_peaks) ! Y-distortion at
!      each peak (The distortion is measured as true position minus measured 
!      position)
     Integer, Intent(IN) :: xmaxdat ! First dimension of memory arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of memory arrays
!  Export:
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of memory region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of memory region
     Real, Intent(OUT) :: MXAXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(OUT) :: MYAXIS(ymaxdat) ! Array containing data Y-coordinates
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Array containing data to
!      be fitted
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Logical, Save :: x_display = .True. ! .True., if X-distortion function
!      is to be displayed
!  Local Arrays:
!  Local Data:
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ( 'Subroutine  F2D_DIST2MEM ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably defined
     If (xmax_peaks .Lt. 1) Then
        status = St_bad_dim1
     Else If (ymax_peaks .Lt. 1) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
 
!     Add module number to status value
        status = St_mod_fit2d + status
 
!     Save details of subroutine call
        Call ST_SAVE ( 'Subroutine F2D_DIST2MEM ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Inquire if X or Y-direction distortion function is to be displayed
        Call IO_INPL (.True., 0, 1, .True., 'STORE X-DISTORTION IN MEMORY', 1, &
          'Enter "YES" for X-distortion, "NO" for Y-distortion ', 1, &
          'Enter "YES" or "NO"', x_display, status)
 
!     Store distortion in memory
        If (x_display) Then
           Call MA_RCOPY (xmax_peaks, ymax_peaks, X_DISTORTION, 1, 1, &
             xnum_peaks, ynum_peaks, xmaxdat, ymaxdat, MDATA, status)
           mtitle = '2-D X-Distortion'
           mzlabel = 'X-Distortion in pixels (Ideal - Measured)'
        Else
           Call MA_RCOPY (xmax_peaks, ymax_peaks, Y_DISTORTION, 1, 1, &
             xnum_peaks, ynum_peaks, xmaxdat, ymaxdat, MDATA, status)
           mtitle = '2-D Y-Distortion'
           mzlabel = 'Y-Distortion in pixels (Ideal - Measured)'
        End If
 
!     Set scalars
        mxnumdat = xnum_peaks
        mynumdat = ynum_peaks
        mxstrelm = 1
        mystrelm = 1
        mxendelm = xnum_peaks
        myendelm = ynum_peaks
        mxlabel = 'Horizontal Grid Holes'
        mylabel = 'Vertical Grid Holes'
 
!     Set axis values
        Do x = 1, xnum_peaks
           MXAXIS(x) = Real(x) - 0.5
        End Do
        Do y = 1, ynum_peaks
           MYAXIS(y) = Real(y) - 0.5
        End Do
 
     End If
 
     End Subroutine F2D_DIST2MEM
!********1*********2*********3*********4*********5*********6*********7*********8
