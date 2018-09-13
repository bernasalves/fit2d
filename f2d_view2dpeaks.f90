!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_view2dpeaks.f90 *
!  *                     *
!  ***********************
 
!+ F2D_VIEW2DPEAKS: VIEW 2-D PEAK centreS and distortions
     Subroutine F2D_VIEW2DPEAKS ( xmax_peaks, ymax_peaks, xnum_peaks, &
       ynum_peaks, X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, status)
!  Description:
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Dec-1996: V0.2 Avoid open strings crossing lines (Hammersley)
!    03-Nov-1993: V0.1 Original, based on "F2D_VIEWPEAKS" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks ! First dimension size of "X_PEAKS" and 
!      "Y_PEAKS"
     Integer, Intent(IN) :: ymax_peaks ! Second dimension size of "X_PEAKS" and
!      "Y_PEAKS"
     Integer, Intent(IN) :: xnum_peaks ! Number of peaks in X-direction of grid
     Integer, Intent(IN) :: ynum_peaks ! Number of peaks in Y-direction of grid
     Real, Intent(IN) :: X_PEAKS(xmax_peaks, ymax_peaks) ! X-coordinates of
!      peak centres
     Real, Intent(IN) :: Y_PEAKS(xmax_peaks, ymax_peaks) ! Y-coordinates of
!      peak centres
     Real, Intent(IN) :: X_DISTORTION(xmax_peaks, ymax_peaks) ! X-distortion at
!      each peak (The distortion is measured as true position minus measured 
!      position)
     Real, Intent(IN) :: Y_DISTORTION(xmax_peaks, ymax_peaks) ! Y-distortion at
!      each peak (The distortion is measured as true position minus measured 
!      position)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: xpeak ! Loop variable for peaks in X-direction of grid
     Integer :: ypeak ! Loop variable for peaks in Y-direction of grid
     Logical :: output ! .True., if output of results is required
     Logical :: row ! .True., if a row is to be output rather than a column
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_VIEW2DPEAKS ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_VIEW2DPEAKS ' // Version)
        status = St_mod_fit2d + status
     Else
 
!     Allow user to get details of peak positions
        xpeak = 1
        ypeak = 1
        Do While (xpeak .Gt. 0)
 
!        Enter number of X-peak
           Call IO_INPI (.True., 0, xnum_peaks, .True., &
             'X-PEAK NUMBER TO VIEW (0 to exit)', 1, &
             'Enter X-peak number to see it''s position', 1, &
             'Value must be within given range', xpeak, status)
 
           If (xpeak .Gt. 0) Then
 
!           Enter number of Y-peak
              Call IO_INPI (.True., 1, ynum_peaks, .True., &
                'Y-PEAK NUMBER TO VIEW', 1, &
                'Enter Y-peak number to see it''s position', 1, &
                'Value must be within given range', ypeak, status)
 
              If (X_PEAKS(xpeak, ypeak) .Gt. -1.7e38) Then
                 Write (message, '(''INFO: Position/Distortion = '', 4f12.4)') &
                   X_PEAKS(xpeak, ypeak), Y_PEAKS(xpeak, ypeak), &
                   X_DISTORTION(xpeak, ypeak), Y_DISTORTION(xpeak, ypeak)
 
              Else
                 message = 'INFO: Peak was not found'
              End If
              Call IO_WRITE (message, status)
 
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
 
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Allow user to output details of complete rows or columns
        output = .True.
        xpeak = 1
        ypeak = 1
        Do While (output)
 
           Call IO_INPL (.True., 0, 1, .True., &
             'OUTPUT VALUES OF ROW OR COLUMN', 1, &
             'YES for choice of row or column output', 1, &
             'Enter "YES" or "NO"', output, status)
 
           If (output) Then
 
!           Row or column output
              Call IO_INPL (.True., 0, 1, .True., &
                'OUTPUT VALUES OF ROW OF PEAKS', 1, &
                'YES for grid row, "NO" for grid column', 1, &
                'Enter "YES" or "NO"', row, status)
 
              If (row) Then
 
                 If (ypeak .Gt. ynum_peaks) Then
                    ypeak = 1
                 End If
 
                 Call IO_INPI (.True., 1, ynum_peaks, .True., 'NUMBER OF ROW', &
                   1, 'Enter number of row to output', 1, &
                   'Enter integer in given range', ypeak, status)
 
                 Do xpeak = 1, xnum_peaks
 
                    If (X_PEAKS(xpeak, ypeak) .Gt. -1.7e38) Then
                       Write (message, '(''INFO: ' // &
                         'Position/Distortion = '', 4f12.4)') X_PEAKS(xpeak, &
                         ypeak), Y_PEAKS(xpeak, ypeak), X_DISTORTION(xpeak, &
                         ypeak), Y_DISTORTION(xpeak, ypeak)
 
                    Else
                       message = 'INFO: Peak was not found'
                    End If
                    Call IO_WRITE (message, status)
 
                 End Do
                 ypeak = ypeak + 1
 
              Else
 
                 If (xpeak .Gt. xnum_peaks) Then
                    xpeak = 1
                 End If
 
!              Column output
                 Call IO_INPI (.True., 1, xnum_peaks, .True., &
                   'NUMBER OF COLUMN', 1, 'Enter number of column to output', &
                   1, 'Enter integer in given range', xpeak, status)
 
                 Do ypeak = 1, ynum_peaks
 
                    If (X_PEAKS(xpeak, ypeak) .Gt. -1.7e38) Then
                       Write (message, &
                         '(''INFO: Position/Distortion = '', 4f12.4)') &
                         X_PEAKS(xpeak, ypeak), Y_PEAKS(xpeak, ypeak), &
                         X_DISTORTION(xpeak, ypeak), Y_DISTORTION(xpeak, ypeak)
 
                    Else
                       message = 'INFO: Peak was not found'
                    End If
                    Call IO_WRITE (message, status)
 
                 End Do
 
                 xpeak = xpeak + 1
 
              End If
 
           End If
 
        End Do
 
     End If
 
     End Subroutine F2D_VIEW2DPEAKS
!********1*********2*********3*********4*********5*********6*********7*********8
