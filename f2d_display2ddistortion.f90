!********1*********2*********3*********4*********5*********6*********7**
 
!  *******************************
!  *                             *
!  * f2d_display2ddistortion.f90 *
!  *                             *
!  *******************************
 
!+ F2D_DISPLAY2DDISTORTION - FIT 2-D DISPLAY 2-D DISTORTION
     Subroutine F2D_DISPLAY2DDISTORTION (xmax_peaks, ymax_peaks, xnum_peaks, &
       ynum_peaks, X_DISTORTION, Y_DISTORTION, status)
!  Description:
!    Displays either X or Y-direction distortion.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    25-Apr-2006: V0.3 Use Fortran-90 dynamically allocated arrays (Hammersley)
!    20-Jun-1995: V0.2 Convert to GS graphics libray (Hammersley)
!    03-Nov-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks ! Dimension size of "X_PEAKS" and
!       "Y_PEAKS"
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
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer stat ! Status return variable for "Allocate"
     Integer x ! Loop varaible for X-direction
     Integer y ! Loop varaible for Y-direction
     Logical, Save :: x_display = .True. ! .True., if X-distortion function
!      is to be displayed
!  Local Arrays:
     Real, Allocatable :: XAXIS(:) ! "XAXIS" array
     Real, Allocatable :: YAXIS(:) ! "YAXIS" array
!  Local Data:
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ( 'Subroutine  F2D_DISPLAY2DDISTORTION ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably
!  defined
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
        Call ST_SAVE ( 'Subroutine F2D_DISPLAY2DDISTORTION ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Inquire if X or Y-direction distortion function is to be
!     displayed
        Call IO_INPL (.True., 0, 1, .True., 'DISPLAY X-DISTORTION', 1, &
          'Enter "YES" for X-distortion, "NO" for Y-distortion ', 1, &
          'Value must be within given range', x_display, status)
 
!     Allocate arrays to contain axis values
        Allocate (XAXIS(xnum_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_DISPLAY2DDISTORTION ' // Version)
           Return
        End If
        Allocate (YAXIS(ynum_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_DISPLAY2DDISTORTION ' // Version)
           Return
        End If
 
!     Define axis values
        Do x = 1, xnum_peaks
           XAXIS(x) = Real(x) - 0.5
        End Do
 
        Do y = 1, ynum_peaks
           YAXIS(y) = Real(y) - 0.5
        End Do
 
!     Display image
        If (x_display) Then
 
           Call GS_2DIMAGE ( xmax_peaks, ymax_peaks, X_DISTORTION, &
             XAXIS, YAXIS, 1, 1, xnum_peaks, ynum_peaks, &
             '2-D X-Distortion', 'Horizontal Peaks', 'Vertical Peaks', &
             'X-Distortion in pixels (Ideal - Measured)', status)

        Else

           Call GS_2DIMAGE ( xmax_peaks, ymax_peaks, Y_DISTORTION, &
             XAXIS, YAXIS, 1, 1, xnum_peaks, ynum_peaks, &
             '2-D Y-Distortion', 'Horizontal Peaks', 'Vertical Peaks', &
             'Y-Distortion in pixels (Ideal - Measured)', status)

        End If
 
!     Free axis value arrays
        Deallocate (XAXIS)
        Deallocate (YAXIS)
 
     End If
 
     End Subroutine F2D_DISPLAY2DDISTORTION
!********1*********2*********3*********4*********5*********6*********7**
