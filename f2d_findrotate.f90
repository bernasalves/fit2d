!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_findrotate.f90 *
!  *                    *
!  **********************
 
!+ F2D_FINDROTATE: FIND peak ROTATE arrays
     Subroutine F2D_FINDROTATE (xmaxdat, ymaxdat, xendelm, yendelm, X_AXIS, &
       Y_AXIS, xmax_peaks, ymax_peaks, xnum_grid, ynum_grid, num_left, &
       num_down, X_2DPEAKS, Y_2DPEAKS, X_DISTORTION, Y_DISTORTION, status)
!  Description:
!    Rotate position array to correct position and convert to data coordinates
!  Keywords:
!  Method:
!    Distortion arrays are used for temporary storage.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    04-Jan-1994: V0.2 Make more robust (Hammersley)
!    01-Nov-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Integer, Intent(IN) :: xmax_peaks ! First Dimension size of "X_PEAKS"
!      and "Y_PEAKS"
     Integer, Intent(IN) :: ymax_peaks ! Second Dimension size of "X_PEAKS"
!      and "Y_PEAKS"
     Integer, Intent(IN) :: xnum_grid ! Number of peaks in X-direction of grid
     Integer, Intent(IN) :: ynum_grid ! Number of peaks in Y-direction of grid
     Integer, Intent(IN) :: num_left ! Number of peaks to the left of the origin
     Integer, Intent(IN) :: num_down ! Number of peaks below the origin
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: X_2DPEAKS(xmax_peaks, ymax_peaks)
!      X-coordinates of peak centres
     Real, Intent(OUT) :: Y_2DPEAKS(xmax_peaks, ymax_peaks)
!      Y-coordinates of peak centres
     Real, Intent(OUT) :: X_DISTORTION(xmax_peaks, ymax_peaks)
!      X-distortion at each peak (The distortion is measured as true position 
!      minus measured position)
     Real, Intent(OUT) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
!      Y-distortion at each peak (The distortion is measured as true position 
!      minus measured position)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: x ! Loop variable for X-direction
     Integer :: x_in ! Loop variable for input arrays
     Integer :: y ! Loop variable for Y-direction
     Integer :: y_in ! Loop variable for input arrays
!  Local Arrays:
!    Internal Functions:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FINDROTATE ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0 .Or. xmax_peaks .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0 .Or. ymax_peaks .Le. 1) Then
        status = St_bad_dim2
     Else If (xnum_grid .Gt. xmax_peaks ) Then
        status = St_bad_adr1
     Else If (ynum_grid .Gt. ymax_peaks) Then
        status = St_bad_adr2
     Else If (num_left .Ge. xnum_grid) Then
        status = St_bad_rel1
     Else If (num_down .Ge. ynum_grid) Then
        status = St_bad_rel2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_ma + status
        Call ST_SAVE ('Subroutine F2D_FINDROTATE ' // Version)
     Else
 
!     Rotate data from X_2DPEAKS and Y_2DPEAKS into distortion arrays
        Do y = 1, ynum_grid
 
           y_in = y - num_down
           If (y_in .Lt. 1) Then
              y_in = y_in + ymax_peaks
           End If
 
           Do x = 1, xnum_grid
 
              x_in = x - num_left
              If (x_in .Lt. 1) Then
                 x_in = x_in + xmax_peaks
              End If
 
              X_DISTORTION(x, y) = X_2DPEAKS(x_in, y_in)
              Y_DISTORTION(x, y) = Y_2DPEAKS(x_in, y_in)
           End Do
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Convert peak coordinates from pixel coordinates to data coordinates
        Do y = 1, ynum_grid
 
           Do x = 1, xnum_grid
 
              If (X_DISTORTION(x, y) .Gt. -1.7e38) Then
                 Call MA_PC2DC (xmaxdat, xendelm, X_AXIS, X_DISTORTION(x, y), &
                   X_2DPEAKS(x, y),  status)
                 Call MA_PC2DC (ymaxdat, yendelm, Y_AXIS, Y_DISTORTION(x, y), &
                   Y_2DPEAKS(x, y),  status)
              Else
                 X_2DPEAKS(x, y) = -1.7e38
              End If
 
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_FINDROTATE
!********1*********2*********3*********4*********5*********6*********7*********8
