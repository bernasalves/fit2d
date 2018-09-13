!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_save2dpeaks.f90 *
!  *                     *
!  ***********************
 
!+ F2D_SAVE2DPEAKS: SAVE PEAKS to file
     Subroutine F2D_SAVE2DPEAKS ( xmax_peaks, ymax_peaks, xnum_peaks, &
       ynum_peaks, X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, status)
!  Description:
!    Output peak positions and distortions to ASCII file
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Oct-1997: V0.3 Add extra space to output to separate numbers
!      (Hammersley)
!    16-Dec-1996: V0.2 Avoid open strings crossing lines (Hammersley)
!    03-Nov-1993: V0.1 Original, based on "F2D_SAVEPEAKS" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks ! First dimension size of "X_PEAKS" and
!      "Y_PEAKS"
     Integer, Intent(IN) :: ymax_peaks ! Second dimension size of "X_PEAKS" and
!      "Y_PEAKS"
     Integer, Intent(IN) :: xnum_peaks ! Number of peaks in grid horizontally
     Integer, Intent(IN) :: ynum_peaks ! Number of peaks in grid vertically
     Real, Intent(IN) :: X_PEAKS(xmax_peaks, ymax_peaks) ! X-coordinates of
!      grid peak centres
     Real, Intent(IN) :: Y_PEAKS(xmax_peaks, ymax_peaks) ! Y-coordinates of
!      grid peak centres
     Real, Intent(IN) :: X_DISTORTION(xmax_peaks, ymax_peaks) ! X-distortion at
!      each peak (The distortion is measured as true position minus measured 
!      position)
     Real, Intent(IN) :: Y_DISTORTION(xmax_peaks, ymax_peaks) ! Y-distortion at
!      each peak (The distortion is measured as true position minus measured 
!      position)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
     Integer, Parameter :: Io_out = 17 ! Unit number for output
!  Local Variables:
     Character(Len = 80), Save :: file_name = 'peaks.dat' ! Name of output file
     Integer :: retstat ! "IO_OPEN ASCIIFILE" return status variable
     Integer :: xpeak ! Loop variable for X-peaks
     Integer :: ypeak ! Loop variable for Y-peaks
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered: F2D_SAVE2DPEAKS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SAVE2DPEAKS ' // Version)
        Return
     End If
 
     If (xmax_peaks .Lt. 1) Then
        status = St_bad_dim1
     Else If (ymax_peaks .Lt. 1) Then
        status = St_bad_dim2
     Else If (xnum_peaks .Lt. 1 .Or. xnum_peaks .Gt. xmax_peaks) Then
        status = St_bad_adr1
     Else If (ynum_peaks .Lt. 1 .Or. ynum_peaks .Gt. ymax_peaks) Then
        status = St_bad_adr2
     End If
 
!  Re-check input status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_SAVE2DPEAKS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input arguments appear to be correct
!     Open ASCII file for output
        Call IO_WRITE ('Name of file for peak positions', status)
        Call IO_OPEN_ASCIIFILE (.True., 'WRITE', Io_out, file_name, retstat, &
          status)
 
        If (retstat .Eq. 0) Then
 
           Write (Io_out, '(''Peaks positions and estimated ' // &
             'distortions for a '', i6, '' times '', i6, '' grid'')') &
             xnum_peaks, ynum_peaks
           Write (Io_out, '(''  X-PEAK   Y_PEAK  X-POSITIONS    ' // &
             'Y-POSITIONS   X-DISTORTIONS  Y-DISTORTIONS '')')
           Do ypeak = 1, ynum_peaks
 
              Do xpeak = 1, xnum_peaks
                 Write (Io_out, '(2i8, 1x, 4(1pe14.7, 1x))') xpeak, ypeak, &
                   X_PEAKS(xpeak, ypeak), Y_PEAKS(xpeak, ypeak), &
                   X_DISTORTION(xpeak, ypeak), Y_DISTORTION(xpeak, ypeak)
              End Do
 
           End Do
 
!        Close file
           Close (Io_out)
 
        End If
 
     End If
 
     End Subroutine F2D_SAVE2DPEAKS
!********1*********2*********3*********4*********5*********6*********7*********8
