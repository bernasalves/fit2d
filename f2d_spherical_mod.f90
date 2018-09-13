!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_spherical_mod.f90 *
!  *                       *
!  *************************
 
!+ F2D_SPHERICAL_MOD - FIT2D: SPHERICAL MODification
     Subroutine F2D_SPHERICAL_MOD (xmaxdat, ymaxdat, title, xlabel, ylabel, &
       zlabel, xnumdat, ynumdat, XAXIS, YAXIS, xstrelm, ystrelm, xendelm, &
       yendelm, x_beam, y_beam, DATA, status)
!  Description:
!    Modify intensities I' = I * ABS(SIN(angle)) measured from
!    the meridian.
!  Keywords:
!    Spherical~Averaging, Spherical~Modification
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    31-Mar-2006: V0.2 Remove "f2d_fit2d.inc" (Hammersley)
!    29-Apr-1999: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
!     Use GS_LIB
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Integer, Intent(IN) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements in Y-direction
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis values
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: x_beam ! X-coordinate of beam centre in pixels
     Real, Intent(IN) :: y_beam ! Y-coordinate of beam centre in pixels
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The data values
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: num_coordinates ! Number of input coordinates
     Integer :: x ! Loop variable for first dimension
     Integer :: y ! Loop variable for second dimension
     Real :: angle ! Angle of pixel
     Real :: angle_meridian ! Angle of the meridian
     Real :: x_diff ! Difference in X-direction
     Real :: x_meridian ! X-coordinate of meridian point
     Real :: x_pixel ! X-pixel coordinate
     Real :: y_diff ! Difference in Y-direction
     Real :: y_meridian ! Y-coordinate of meridian point
     Real :: y_pixel ! Y-pixel coordinate
!  Local Arrays:
     Character(Len = 60) :: MESSAGE(2) ! User messages
     Logical*1 :: DUMMY(1, 1) ! Dummy array instead of mask
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SPHERICAL_MOD ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_SPHERICAL_MOD ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Draw beam centre
        Call GS_MARK (x_beam, y_beam, 2.0, status)
 
!     Turn on simple "rubber-band"
        Call LG_SET_RUBBERBAND (Gs_wkid_terminal, 1, x_beam, y_beam, status)
 
!     Click on meridian
        num_coordinates = 0
        MESSAGE(1) = 'Click on meridian '
        Call GS_INPS_FCOORDINATES ( .False., .True., xmaxdat, ymaxdat, &
          xstrelm, ystrelm, xendelm, yendelm, DATA, DUMMY, &
          XAXIS, YAXIS, title, xlabel, ylabel, zlabel, &
          'CLICK ON MERIDIAN', 1, MESSAGE, .False., 1, num_coordinates, &
          x_meridian, y_meridian, status)
 
!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           status = St_goodvalue
 
!        Turn off "rubber-band"
           Call LG_SET_RUBBERBAND (Gs_wkid_terminal, 0, x_beam, y_beam, status)
 
        Else If (status .Ne. St_goodvalue) Then
           Return
        Else
 
!        Turn off "rubber-band"
           Call LG_SET_RUBBERBAND (Gs_wkid_terminal, 0, x_beam, y_beam, status)
 
!        Draw line
           Call GS_LINESTYLE (Lg_solid, 1.0, Gs_red, status)
           Call GS_LINE (x_beam, y_beam, x_meridian, y_meridian, status)
           Call GS_UPDATE (status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!        Calculate angle of meridian
           x_diff = x_meridian - x_beam
           y_diff = y_meridian - y_beam
 
           If (Abs(x_diff) .Lt. 1.0e-12 .And. Abs(y_diff) .Lt. 1.0e-12) Then
              MESSAGE(1) = 'Meridian point must not be on beam centre'
              Call GS_FWARNING (1, 1, MESSAGE, status)
              Return
           Else
              angle_meridian = Atan2(y_diff, x_diff)
 
              Write (*, '(''Meridian angle (degrees) = '', f12.5)') &
                angle_meridian * 180.0 / Pi
           End If
 
!        Perform intensity modification
           Do y = ystrelm, yendelm
 
              y_pixel = Real(y) - 0.5
              y_diff = y_pixel - y_beam
 
              Do x = xstrelm, xendelm
                 x_pixel = Real(x) - 0.5
                 x_diff = x_pixel - x_beam
 
                 If (Abs(x_diff) .Lt. 1.0e-12 .And. Abs(y_diff) .Lt. 1.0e-12) &
                   Then
                    angle = 0.0
                 Else
                    angle = Atan2(y_diff, x_diff)
                 End If
 
!              Angle from meridian
                 angle = Abs(angle - angle_meridian)
 
!              Modify intensity
                 DATA(x, y) = DATA(x, y) * Abs(Sin(angle))
 
              End Do
 
           End Do
 
        End If
 
     End If
 
     End Subroutine F2D_SPHERICAL_MOD
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
