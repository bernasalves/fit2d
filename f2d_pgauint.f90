!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_pgauint.f90 *
!  *                 *
!  *******************
 
!+ F2D_PGAUINT - Fit2d: Polar Gaussian numerical INTegration
     Subroutine F2D_PGAUINT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, peak_intensity, xcenpeak, ycenpeak, x_centre, &
       y_centre, sigradius, angsig, WORKR, intensity, status)
!  Description:
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!    None
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    07-Apr-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Array of X-coordinate values
!      giving the positions of the centre of each pixel in world coordinates
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Array of Y-coordinate values
!      giving the positions of the centre of each pixel in world coordinates
     Real, Intent(IN) :: peak_intensity ! Maximum intensity of peak
     Real, Intent(IN) :: xcenpeak ! The X-coordinate of the centre of the peak
     Real, Intent(IN) :: ycenpeak ! The Y-coordinate of the centre of the peak
!      point and the centre of symmetry, and the row line
     Real, Intent(IN) :: x_centre ! The centre of the symmetry system in the 
!      X-direction
     Real, Intent(IN) :: y_centre ! The centre of the symmetry system in the 
!      Y-direction
     Real, Intent(IN) :: sigradius ! Radial sigma width of peak
     Real, Intent(IN) :: angsig ! Angular sigma width of peak in radians
!  Import/Export:
     Real, Intent(INOUT) :: WORKR(xendelm, yendelm) ! Work array for calculating
!      peaks
     Real, Intent(INOUT) :: intensity ! Integrated intensity of peak
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: x, y
!  Local Arrays:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PGAUINT ' // Version)
        Return
     End If
 
!  Check that the work array is large enough to produce the data
     If (xstrelm .Le. 0 .Or. xendelm .Gt. xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. yendelm .Gt. ymaxdat) Then
        status = St_bad_adr2
     Else If (xstrelm .Gt. xendelm) Then
        status = St_bad_adr1
     Else If (ystrelm .Gt. yendelm) Then
        status = St_bad_adr2
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Re-check input status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_PGAUINT ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Initialise array
     Call MA_RVALUE (xendelm, yendelm, xstrelm, ystrelm, xendelm, yendelm, &
       0.0, WORKR, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''peak_intensity = '', g12.5)') peak_intensity
!  Write (*, '(''xcenpeak, ycenpeak = '', 2g12.5)')
!  :     xcenpeak, ycenpeak
!  Write (*, '(''x_centre, y_centre = '', 2g12.5)')
!  :     x_centre, y_centre
!  Write (*, '(''sigradius, angsig = '', 2g12.5)')
!  :     sigradius, angsig
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Add polar Gaussian to array
     Call MA_2DPOLARGAU (xendelm, yendelm, xstrelm, ystrelm, xendelm, yendelm, &
       XAXIS, YAXIS, peak_intensity, xcenpeak, ycenpeak, x_centre, y_centre, &
       sigradius, angsig, 4.0, 1, WORKR, status)
 
!  Calculate sum of intensities
     intensity = 0.0
     Do y = ystrelm, yendelm
 
        Do x = xstrelm, xendelm
           intensity = intensity + WORKR(x, y)
        End Do
 
     End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''intensity = '', 1pg12.5)') intensity
!  Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_PGAUINT
!********1*********2*********3*********4*********5*********6*********7*********8
