!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_destroypeaks.f90 *
!  *                      *
!  ************************
 
!+ F2D_DESTROYPEAKS: DESTROY PEAKS
     Subroutine F2D_DESTROYPEAKS ( xmax_peaks, ymax_peaks, xnum_peaks, &
       ynum_peaks, X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, status)
!  Description:
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    21-Jun-1993: V0.1 Original, based on "F2D_VIEW2DPEAKS" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks ! First dimension size of "X_PEAKS"
!      and "Y_PEAKS"
     Integer, Intent(IN) :: ymax_peaks ! Second dimension size of "X_PEAKS"
!      and "Y_PEAKS"
     Integer, Intent(IN) :: xnum_peaks ! Number of peaks in X-direction of
!      grid
     Integer, Intent(IN) :: ynum_peaks ! Number of peaks in Y-direction of
!      grid
!  Import/Export:
     Real, Intent(INOUT) :: X_PEAKS(xmax_peaks, ymax_peaks)
!      X-coordinates of peak centres
     Real, Intent(INOUT) :: Y_PEAKS(xmax_peaks, ymax_peaks)
!      Y-coordinates of peak centres
     Real, Intent(INOUT) :: X_DISTORTION(xmax_peaks, ymax_peaks) ! X-distortion 
!      at each peak (The distortion is measured as true position minus measured
!      position)
     Real, Intent(INOUT) :: Y_DISTORTION(xmax_peaks, ymax_peaks) ! Y-distortion 
!      at each peak (The distortion is measured as true position minus measured
!      position)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: xpeak ! Loop variable for peaks in X-direction of grid
     Integer :: ypeak ! Loop variable for peaks in Y-direction of grid
     Logical :: destroy ! .True., if a peak is to be destroyed
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DESTROYPEAKS ' // Version)
        Return
     End If
 
     If (xmax_peaks .Lt. 1) Then
        status = St_bad_dim1
     Else If (ymax_peaks .Lt. 1) Then
        status = St_bad_dim1
     Else If (xnum_peaks .Lt. 1 .Or. xnum_peaks .Gt. xmax_peaks) Then
        status = St_bad_adr1
     Else If (ynum_peaks .Lt. 1 .Or. ynum_peaks .Gt. ymax_peaks) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DESTROYPEAKS ' // Version)
        status = St_mod_fit2d + status
     Else
 
!     Allow user to get details of peak positions
        xpeak = 1
        ypeak = 1
        Do While (xpeak .Gt. 0)
 
!        Enter number of X-peak
           Call IO_INPI (.True., 0, xnum_peaks, .True., &
             'X-PEAK NUMBER TO DESTROY (0 to exit)', 1, &
             'Enter X-peak number to see it''s position', 1, &
             'Value must be within given range', xpeak, status)
 
           If (xpeak .Gt. 0) Then
 
!           Enter number of Y-peak
              Call IO_INPI (.True., 1, ynum_peaks, .True., &
                'Y-PEAK NUMBER TO DESTROY', 1, &
                'Enter Y-peak number to see it''s position', 1, &
                'Value must be within given range', ypeak, status)
 
              If (X_PEAKS(xpeak, ypeak) .Gt. -1.7e38) Then
                 Write (message, '(''INFO: Position/Distortion ' // &
                   '= '', 4f12.4)') X_PEAKS(xpeak, ypeak), Y_PEAKS(xpeak, &
                   ypeak), X_DISTORTION(xpeak, ypeak), Y_DISTORTION(xpeak, &
                   ypeak)
                 Call IO_WRITE (message, status)
 
!              Offer the user the opportunity to destroy the peak
                 destroy = .False.
                 Call IO_INPL (.True., 0, 1, .True., 'DESTROY PEAK', 1, &
                   'Enter "YES" to destroy the peak, "NO" to leave it', &
                   1, 'Value must be within given range', destroy, status)
 
                 If (destroy) Then
                    X_PEAKS(xpeak, ypeak) = -1.7e38
                    X_DISTORTION(xpeak, ypeak) = 0.0
                    Y_DISTORTION(xpeak, ypeak) = 0.0
                 End If
 
              Else
                 Call IO_WRITE ('INFO: Peak is missing', status)
              End If
 
 
!           Set default value for next peak
              xpeak = xpeak + 1
              If (xpeak .Gt. xnum_peaks) Then
                 xpeak = 1
                 ypeak = ypeak + 1
 
                 If (ypeak .Gt. ynum_peaks) Then
                    ypeak = 1
                 End If
 
              End If
 
           End If
 
        End Do
 
     End If
 
     End Subroutine F2D_DESTROYPEAKS
!********1*********2*********3*********4*********5*********6*********7*********8
