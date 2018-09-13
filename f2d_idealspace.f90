!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_idealspace.f90 *
!  *                    *
!  **********************
 
!+ F2D_IDEALSPACE: IDEAL SPACE to distorted space distortion definition
     Subroutine F2D_IDEALSPACE (xmax_peaks, ymax_peaks, xnumpeaks, ynumpeaks, &
       X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, status)
!  Description:
!    Re-calculates distortion values, for the tranformation to go from the ideal
!    grid to the distorted grid, based on the ideal grid coordinates, and vice 
!    versa.
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    06-Sep-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks ! First dimension size of "X_PEAKS" and
!      "Y_PEAKS"
     Integer, Intent(IN) :: ymax_peaks ! Second dimension size of "X_PEAKS" and
!      "Y_PEAKS"
     Integer, Intent(IN) :: xnumpeaks ! Number of peaks in X-direction of grid
     Integer, Intent(IN) :: ynumpeaks ! Number of peaks in Y-direction of grid
!  Import/Export:
     Real, Intent(INOUT) :: X_PEAKS(xmax_peaks, ymax_peaks)
!      X-coordinates of peak centres
     Real, Intent(INOUT) :: Y_PEAKS(xmax_peaks, ymax_peaks)
!      Y-coordinates of peak centres
     Real, Intent(INOUT) :: X_DISTORTION(xmax_peaks, ymax_peaks) ! X-distortion 
!      at each peak (The output distortion is measured as measured position 
!       minus true position. Thus True + Distortion = Measured)
     Real, Intent(INOUT) :: Y_DISTORTION(xmax_peaks, ymax_peaks) ! Y-distortion 
!      at each peak (The output distortion is measured as measured position
!      minus true position. Thus True + Distortion = Measured)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: x_peak ! Loop variable for peaks in the X-direction
     Integer :: y_peak ! Loop variable for peaks in the Y-direction
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_IDEALSPACE ' // Version)
     Else
 
!     Re-calculate distortion for each peak
        Do y_peak = 1, ynumpeaks
 
           Do x_peak  = 1, xnumpeaks
 
              If (X_PEAKS(x_peak, y_peak) .Gt. -1.7e38) Then
 
                 X_PEAKS(x_peak, y_peak) = X_PEAKS(x_peak, y_peak) + &
                   X_DISTORTION(x_peak, y_peak)
                 Y_PEAKS(x_peak, y_peak) = Y_PEAKS(x_peak, y_peak) + &
                   Y_DISTORTION(x_peak, y_peak)
                 X_DISTORTION(x_peak, y_peak) = - X_DISTORTION(x_peak, y_peak)
                 Y_DISTORTION(x_peak, y_peak) = - Y_DISTORTION(x_peak, y_peak)
 
              End If
 
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_IDEALSPACE
!********1*********2*********3*********4*********5*********6*********7*********8
