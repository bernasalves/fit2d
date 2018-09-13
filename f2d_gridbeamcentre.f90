!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_gridbeamcentre.f90 *
!  *                        *
!  **************************
 
!+ F2D_GRIDBEAMCENTRE: GRID coordinate BEAM CENTRE calculation
     Subroutine F2D_GRIDBEAMCENTRE (x_beam, y_beam, xmax_peaks, ymax_peaks, &
       xnumpeaks, ynumpeaks, X_PEAKS, Y_PEAKS, x_grid_co, y_grid_co, status)
!  Description:
!    Calculates the fractional grid hole coordinate corresponding to the beam 
!    centre, as input in pixel coordinates, using the grid hole positions.
!  Keywords:
!    Grid~Coordinates.Beam~Centre.Calculation
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    12-Aug-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: x_beam ! X-pixel coordinate of beam on detector
     Real, Intent(IN) :: y_beam ! Y-pixel coordinate of beam on detector
     Integer, Intent(IN) :: xmax_peaks ! First dimension size of "X_PEAKS" and 
!      "Y_PEAKS"
     Integer, Intent(IN) :: ymax_peaks ! Second dimension size of "X_PEAKS" and
!      "Y_PEAKS"
     Integer, Intent(IN) :: xnumpeaks ! Number of peaks in X-direction of grid
     Integer, Intent(IN) :: ynumpeaks ! Number of peaks in Y-direction of grid
     Real, Intent(IN) :: X_PEAKS(xmax_peaks, ymax_peaks) ! X-coordinates of
!      peak centres
     Real, Intent(IN) :: Y_PEAKS(xmax_peaks, ymax_peaks) ! Y-coordinates of
!      peak centres
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: x_grid_co ! X-grid coordinate of beam
     Real, Intent(OUT) :: y_grid_co ! Y-grid coordinate of beam
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: xpeak ! Loop variable for peaks in X-direction of grid
     Integer :: ypeak ! Loop variable for peaks in Y-direction of grid
     Real :: distance_sqr ! Square of distance (in pixels) from a grid peak to 
!      the beam centre
     Real :: min_distance_sqr ! Minimum square of distance (in pixels) from the
!      grid peaks to the beam centre
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GRIDBEAMCENTRE ' // Version)
     Else
 
!     Find nearest grid position
        min_distance_sqr = 1.7e38
        Do ypeak = 1, ynumpeaks
 
           Do xpeak = 1, xnumpeaks
 
!           Check that peak has been found
              If (X_PEAKS(xpeak, ypeak) .Gt. -1.7e38) Then
 
!              Calculate distance in pixels from peak to beam centre
                 distance_sqr = (X_PEAKS(xpeak, ypeak) - x_beam)**2 + &
                   (Y_PEAKS(xpeak, ypeak) - y_beam)**2
 
                 If (distance_sqr .Lt. min_distance_sqr) Then
 
                    min_distance_sqr = distance_sqr
                    x_grid_co = Real(xpeak)
                    y_grid_co = Real(ypeak)
 
                 End If
 
              End If
 
           End Do
 
        End Do
 
!     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     Interpolation between near-by grid peaks should be added here
!     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
     End If
 
     End Subroutine F2D_GRIDBEAMCENTRE
!********1*********2*********3*********4*********5*********6*********7*********8
