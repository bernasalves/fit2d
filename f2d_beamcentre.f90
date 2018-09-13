!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_beamcentre.f90 *
!  *                    *
!  **********************
 
!+ F2D_BEAMCENTRE -  TILT/ beam CENTRE determination
     Subroutine F2D_BEAMCENTRE (gui, mask_data, xmaxdat, ymaxdat, DATA, MASK, &
       X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
       ylabel, zlabel, full_info, method_used, &
       experiment, radius1, radius2, angle, radial_error, status)
!  Description:
!    Input of beam centre by the user with a choice of methods,
!    including graphical.
!  Keywords:
!    Beam~Centre
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Nov-2014: V0.27 Changes to "F2D_CLICK" (Hammersley)
!    17-Mar-2006: V0.26 Support for arbitrary aspect ratio windows (Hammersley)
!    15-Mar-2006: V0.25 Calculate beam centre in pixel coordinates (Hammersley)
!    13-Mar-2006: V0.24 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.23 Alter menu lay-out for landscape windows (Hammersley)
!    02-Jun-2003: V0.22 Tidy-up code (Hammersley)
!    23-Feb-1999: V0.21 All data-base saving and recovering routines
!      renamed (Hammersley)
!    15-Dec-1998: V0.20 Change to use IO internal database routines (Hammersley)
!    02-Nov-1998: V0.19 Save beam centre in pixel units (Hammersley)
!    23-Oct-1998: V0.18 Save values internally in data-base (Hammersley)
!    25-May-1998: V0.17 Option for GISAXS beam centre determination (Hammersley)
!    10-Jan-1997: V0.16 Convert data coordinates to pixel coordinates
!      for fitting beam centre (Hammersley)
!    16-Dec-1996: V0.15 Avoid open strings crossing lines (Hammersley)
!    22-Oct-1996: V0.14 Cope with failure of Gaussian fitting (Hammersley)
!    26-Aug-1996: V0.13 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    23-Aug-1996: V0.12 Changes to "GS_INPS_COORDINATES" (Hammersley)
!    13-Feb-1996: V0.11 Set "method_used" for 2-D fitted Gaussian (Hammersley)
!    06-Feb-1996: V0.10 Add "2-D GAUSSIAN" option (Hammersley)
!    05-Feb-1996: V0.9 Use "GS_MPLOT" to allow either 2-D or 1-D data to be 
!      output (Hammersley)
!    03-Feb-1996: V0.8 Add "NO CHANGE" option (Hammersley)
!    02-Feb-1996: V0.7 Change to subroutine and "F2D_INP_BEAMCENTRE"
!      arguments, option of GUI interface (Hammersley)
!    03-Jan-1996: V0.6 Changes for IBM AIX "xlf" compiler: Doesn't
!      like "g" format descriptor withiout a width qualifier (Hammersley)
!    26-Oct-1995: V0.5 Output "spy-glass" during coordinate input (Hammersley)
!    20-Jun-1995: V0.4 Convert to using GS graphics library (Hammersley)
!    03-Mar-1995: V0.3 Option to display masked-off data (Hammersley)
!    25-Jan-1995: V0.2 Display beam centre graphically, if graphical input is 
!      used (Hammersley)
!    23-Jan-1995: V0.1 Original, based on "F2D_GEOMETRY" and "F2D_TILTCENTRE"
!      (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used
     Logical, Intent(IN) :: mask_data ! .True., if the mask is to be used to
!      mask off pixels
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Axis data for the Y-axis
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! .True., if a pixel is
!      masked-off
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Logical, Intent(IN) :: full_info ! .True., if full information on
!      results is required by the user
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: method_used ! The input method that was used to
!      specify the beam centre:
!        -1 = Failure in method used, centre not defined
!         0 = None, (existing centre was O.K.)
!         1 = Keyboard,            only "x_beam, y_beam" are defined
!         2 = Single cursor point,   "      "       "     "     "
!         3 = Symmetric points,      "      "       "     "     "
!         4 = Points on circle,  "radius1" and "radial_error" are defined
!         5 = Points on ellipse, "radius1", radius2", "angle" and
!             "radial_error" are defined
!         6 = Fitted 2-D Gaussian
!         7 = 1-D Fitted Projection
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Real, Intent(OUT) :: radius1 ! Radius of circle or first radius of
!      ellipse if defined (metres)
     Real, Intent(OUT) :: radius2 ! Second radius of ellipse if defined (metres)
     Real, Intent(OUT) :: angle ! Angle of "radius1" of ellipse to X-axis
!      (anti-clockwise radians)
     Real, Intent(OUT) :: radial_error ! Estimated average error (radially)
!      in coordinate positions if defined (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.27' ! Version number
     Integer, Parameter :: Max_coordinates = 100 ! Dimension size of
!      coordinate arrays
     Integer, Parameter :: Max_menu = 10 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80), Save :: command = 'CIRCLE COORDINATES'
!      User entered command
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable for DATA statements
     Integer :: num_coordinates ! Number of entered coordinates
     Integer :: num_menu ! Number of items in menu
     Integer :: retstat ! Return status variable
     Integer :: x_pixel ! X-pixel of input coordinate
     Integer :: y_pixel ! Y-pixel of input coordinate
     Logical :: continue ! .True., until user wants to exit
     Logical :: update ! .True., if the graphics needs to be redrawn
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!    position
!  Local Arrays:
     Character(Len = 30) :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80) :: MENUTXT(Max_menu) ! Menu command explanation
     Character(Len = 80) :: MESSAGE(5) ! User text
     Real :: X_COORDINATES(Max_coordinates) ! X-coordinates
     Real :: Y_COORDINATES(Max_coordinates) ! Y-coordinates
!  Local Data:
     Data (MENU(item), item = 1, Max_menu) / '?', 'HELP', '2-D GAUSSIAN FIT', &
       'AVERAGED GRAPHICAL', 'CIRCLE COORDINATES', 'ELLIPSE COORDINATES', &
       'GRAPHICAL COORDINATE', 'FIT 1-D PROJECTION', 'KEYBOARD', 'NO CHANGE' /
     Data (MENUTXT(item), item = 1, Max_menu) / &
       '?: This discription list of commands', &
       'HELP: On defining the beam centre', &
       '2-D GAUSSIAN FIT: Fit direct beam mark with 2-D Gaussian', &
       'AVERAGED GRAPHICAL: Average of average symmetry centres', &
       'CIRCLE COORDINATES: Least squares fit on >=3 coordinates', &
       'ELLIPSE COORDINATES: Least squares fit on >=5 coordinates', &
       'GRAPHICAL COORDINATE: Single graphical input coordinate', &
       'FIT 1-D PROJECTION: Fit symmetry of projected region', &
       'KEYBOARD: Single keyboard entered X/Y coordinate', &
       'NO CHANGE: Use existing beam centre' /
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_BEAMCENTRE ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Lt. 1 .Or. xstrelm .Gt. xendelm .Or. &
       xendelm .Gt. xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Lt. 1 .Or. ystrelm .Gt. yendelm .Or. &
       yendelm .Gt. ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_BEAMCENTRE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Inquire window format
        Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
        num_menu = Max_menu
 
        If (gui) Then
 
!        Set menu layout style
           If (width / height .Gt. 1.2) Then
              Call GS_SET_MENULAYOUT (gs_vertical, 1, 14, 12, status)
           Else
              Call GS_SET_MENULAYOUT (gs_horizontal, 2, 14, 12, status)
           End If
 
!        Draw menu
           Call GS_FMENU (1, 1, 'BEAM CENTRE:', Max_menu, num_menu, MENU, &
             status)
 
        End If
 
!     Method used to input beam centre
        num_menu = Max_menu
 
        continue = .True.
        Do While (continue)
 
!        By default no updating of graphics
           update = .False.
 
           If (gui) Then
 
!           Get user to select between the available menu options
              command = 'null'
              Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 2, input_type, &
                command, x_coordinate, y_coordinate, status)
 
           Else
 
              Call IO_MENU (.True., 'INPUT METHOD FOR BEAM CENTRE', Max_menu, &
                MENUTXT, 1, 'Enter one of available choices', Max_menu, &
                num_menu, MENU, command, status)
              input_type = Gs_choice
 
           End If
 
           If (input_type .Eq. Gs_resize) Then
 
              update = .True.
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Output the coordinate position and intensity
              Call F2D_CLICK (1, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, zlabel, &
                x_coordinate, y_coordinate, experiment, update, update, status)
 
           Else
 
!           Menu choice input
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!           Carry out menu choices
              If (command .Eq. 'null') Then
 
                 update = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 If (gui) Then
                    Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
                    update = .True.
                 Else
                    Call IO_TEXT (num_menu, MENUTXT, status)
                 End If
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the "BEAM CENTRE" menu
                 Call F2D_BEAMCENTREHELP (.True., status)
                 update = .True.
 
              Else If (command .Eq. 'FIT 1-D PROJECTION' .Or. command .Eq. &
                '2-D GAUSSIAN FIT' .Or. command .Eq. 'AVERAGED GRAPHICAL' .Or. &
                command .Eq. 'ELLIPSE COORDINATES' .Or. command .Eq. &
                'GRAPHICAL COORDINATE' .Or. command .Eq. 'CIRCLE COORDINATES') &
                Then
 
                 continue = .False.
 
                 If (.Not. gui) Then
 
!                 Create image
                    Call GS_MPLOT (mask_data, xmaxdat, ymaxdat, DATA, MASK, &
                      X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, &
                      title, xlabel, ylabel, zlabel, status)
 
                 End If
 
                 If (command .Eq. 'FIT 1-D PROJECTION') Then
 
                    Call F2D_PROJECTFIT (mask_data, xmaxdat, ymaxdat, DATA, &
                      MASK, X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, &
                      yendelm, title, xlabel, ylabel, zlabel, &
                      experiment, status)
                    method_used = 7
 
                 Else If (command .Eq. '2-D GAUSSIAN FIT') Then
 
!                 Input approximate peak centre
                    num_coordinates = 1
                    MESSAGE(1) = 'Click on or near to direct beam mark.'
                    MESSAGE(2) = 'So long as you are not too far from the'
                    MESSAGE(3) = 'edge of the mark the algorithm should be'
                    MESSAGE(4) = 'able to find the centre.'
                    Call GS_INPS_FCOORDINATES ( mask_data, .True., xmaxdat, &
                      ymaxdat, xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, &
                      X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
                      'CLICK ON DIRECT BEAM MARK', 4, MESSAGE, .False., &
                      max_coordinates, num_coordinates, X_COORDINATES, &
                      Y_COORDINATES, status)
 
!                 Check for user escape
                    If (status .Eq. St_escapevalue) Then
                       status = St_goodvalue
                       method_used = 0
                       Return
                    Else If (status .Ne. St_goodvalue) Then
                       Return
                    End If
 
!                 Output user message
                    Call GS_FPROMPT (1, 1, &
                      'REFINING BEAM CENTRE: PLEASE WAIT', status)
 
!                 Make sure that the display is updated
                    Call GS_UPDATE (status)
 
!                 Coordinate data coordinates to pixel coordinates
                    Call GS_CAL_WCTOPIX (xmaxdat, xendelm, X_AXIS, &
                      X_COORDINATES(1), x_pixel, status)
                    Call GS_CAL_WCTOPIX (ymaxdat, yendelm, Y_AXIS, &
                      Y_COORDINATES(1), y_pixel, status)
 
!                 Find beam mark size and fit with a 2-D Gaussian
                    Call F2D_BEAMGAUSSIAN (xmaxdat, ymaxdat, DATA, xstrelm, &
                      ystrelm, xendelm, yendelm, &
                      x_pixel, y_pixel, 0, retstat, &
                      experiment%x_beam, experiment%y_beam, status)
 
!                 Check return status
                    If (retstat .Ne. 0) Then
                       method_used = -1
                       Return
                    End If
 
                    method_used = 6
 
                 Else If (command .Eq. 'AVERAGED GRAPHICAL') Then
 
!                 Input coordinates of pairs of peaks
                    num_coordinates = 0
                    MESSAGE(1) = 'Click on pairs of point which ' // &
                      'are symmetric about a'
                    MESSAGE(2) = 'centre of symmetry e.g. ' // &
                      'centres of peaks. Make sure'
                    MESSAGE(3) = 'that the number of ' // &
                      'coordinates entered is even. The'
                    MESSAGE(4) = 'average of the input points ' // &
                      'will be used for the'
                    MESSAGE(5) = 'beam centre.'
                    Call GS_INPS_FCOORDINATES ( mask_data, .True., xmaxdat, &
                      ymaxdat, xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, &
                      X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
                      'DEFINE PAIRS OF SYMMETRIC COORDINATES', 5, MESSAGE, &
                      .False., max_coordinates, num_coordinates, &
                      X_COORDINATES, Y_COORDINATES, status)
 
!                 Check for user escape
                    If (status .Eq. St_escapevalue) Then
                       status = St_goodvalue
                       method_used = 0
                       Return
                    Else If (status .Ne. St_goodvalue) Then
                       Return
                    End If
 
                    experiment%x_beam = 0.0
                    experiment%y_beam = 0.0
                    Do coordinate = 1, (num_coordinates / 2) * 2
                       experiment%x_beam = &
                         experiment%x_beam + X_COORDINATES(coordinate)
                       experiment%y_beam = &
                         experiment%y_beam + Y_COORDINATES(coordinate)
                    End Do
                    experiment%x_beam = experiment%x_beam / &
                      Real((num_coordinates / 2) * 2)
                    experiment%y_beam = experiment%y_beam / &
                      Real((num_coordinates / 2) * 2)
                    method_used = 3
 
                 Else If (command .Eq. 'ELLIPSE COORDINATES') Then
 
!                 Input five or more coordinates
                    num_coordinates = 0
                    MESSAGE(1) = 'Click on five or more ' // &
                      'coordinates on an ellipse, or'
                    MESSAGE(2) = 'circle, which is centred about ' // &
                      'the beam centre. If'
                    MESSAGE(3) = 'more than five coordinates are ' // &
                      'entered, the least'
                    MESSAGE(4) = 'squares positional error ' // &
                      'solution will be used, and'
                    MESSAGE(5) = 'an error estimate will be output.'
                    Call GS_INPS_FCOORDINATES ( mask_data, .True., xmaxdat, &
                      ymaxdat, xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, &
                      X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
                      'INPUT COORDINATES ON ELLIPSE (>=5)', 5, MESSAGE, &
                      .False., max_coordinates, num_coordinates, &
                      X_COORDINATES, Y_COORDINATES, status)
 
!                 Check for user escape
                    If (status .Eq. St_escapevalue) Then
                       status = St_goodvalue
                       method_used = 0
                       Return
                    Else If (status .Ne. St_goodvalue) Then
                       Return
                    End If
 
!                 From input coordinates find the least squares solution
!                 for the centre and ellipse axes and orientation
                    If (num_coordinates .Lt. 5) Then
 
                       Call GS_FWARNING (1, 1, 'WARNING: Less ' // &
                         'than five coordinates input', status)
                       continue = .True.
 
                    Else
 
!                    Convert to metre units
                       Do coordinate = 1, num_coordinates
                          X_COORDINATES(coordinate) = &
                            X_COORDINATES(coordinate) * experiment%x_pixel_size
                          Y_COORDINATES(coordinate) = &
                            Y_COORDINATES(coordinate) * experiment%y_pixel_size
                       End Do
 
                       Call F2D_LSQELLIPSE (max_coordinates, num_coordinates, &
                         X_COORDINATES, Y_COORDINATES, &
                         experiment%x_beam, experiment%y_beam, &
                         radius1, radius2, angle, radial_error, status)

!                    Convert beam centre to pixel units
                       experiment%x_beam = &
                         experiment%x_beam / experiment%x_pixel_size
                       experiment%y_beam = &
                         experiment%y_beam / experiment%y_pixel_size

!                    Output information to user
                       If (full_info) Then
                          Write (MESSAGE(1), '(''INFO: Best fit ' // &
                            'radius 1, radius 2 (mm) = '', 2g14.7)') radius1 * &
                            1000.0, radius2 * 1000.0
                          Call IO_WRITE (MESSAGE(1), status)
                          Write (MESSAGE(1), '(''INFO: Best fit ' // &
                            'radius 1, radius 2 (X-pixels) = '', ' // &
                            '2g14.7)') radius1 / experiment%x_pixel_size, &
                            radius2 / experiment%x_pixel_size
                          Call IO_WRITE (MESSAGE(1), status)
                          Write (MESSAGE(1), '(''INFO: Best fit angle of ' // &
                            'axis 1 (degrees) = '', g14.7)') angle * 180.0 / &
                            Pi
                          Call IO_WRITE (MESSAGE(1), status)
 
                          If (num_coordinates .Gt. 5) Then
                             Write (MESSAGE(1), &
                               '(''INFO: Estimated coordinate ' // &
                               'radial position error (mm) = '', g14.7)') &
                               radial_error * 1000.0
                             Call IO_WRITE (MESSAGE(1), status)
                             Write (MESSAGE(1), &
                               '(''INFO: Estimated coordinate ' // &
                               'radial position error (X pixels) = '',' &
                               // 'g14.7)') &
                               radial_error / experiment%x_pixel_size
                             Call IO_WRITE (MESSAGE(1), status)
                          Else
                             Call IO_WRITE ( 'INFO: The coordinate radial ' // &
                               'position error is totally undetermined', &
                               status)
                          End If
 
                       End If
 
                    End If
 
                    method_used = 5
 
                 Else If (command .Eq. 'CIRCLE COORDINATES') Then
 
!                 Input three or more coordinates
                    num_coordinates = 0
                    MESSAGE(1) = 'Click on three or more ' // &
                      'coordinates on an ellipse, or'
                    MESSAGE(2) = 'circle, which is centred about ' // &
                      'the beam centre. If'
                    MESSAGE(3) = 'more than three coordinates ' // &
                      'are entered, the least'
                    MESSAGE(4) = 'squares positional error ' // &
                      'solution will be used, and'
                    MESSAGE(5) = 'an error estimate will be output.'
                    Call GS_INPS_FCOORDINATES ( mask_data, .True., xmaxdat, &
                      ymaxdat, xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, &
                      X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
                      'INPUT CONCENTRIC COORDINATES (>=3)', 5, MESSAGE, &
                      .False., max_coordinates, num_coordinates, &
                      X_COORDINATES, Y_COORDINATES, status)
 
!                 Check for user escape
                    If (status .Eq. St_escapevalue) Then
                       status = St_goodvalue
                       method_used = 0
                       Return
                    Else If (status .Ne. St_goodvalue) Then
                       Return
                    End If
 
!                 From input coordinates find the least squares solution
!                 for the beam centre and radius
                    If (num_coordinates .Lt. 3) Then
                       Call GS_FWARNING (1, 1, &
                         'WARNING: Less than three coordinates input', status)
                    Else
 
!                    Convert to metre units
                       Do coordinate = 1, num_coordinates
                          X_COORDINATES(coordinate) = &
                            X_COORDINATES(coordinate) * experiment%x_pixel_size
                          Y_COORDINATES(coordinate) = &
                            Y_COORDINATES(coordinate) * experiment%y_pixel_size
                       End Do
 
                       Call F2D_LSQCIRCLE (max_coordinates, num_coordinates, &
                         X_COORDINATES, Y_COORDINATES, &
                         experiment%x_beam, experiment%y_beam, &
                         radius1, radial_error, status)
 
!                    Convert beam centre to pixel units
                       experiment%x_beam = &
                         experiment%x_beam / experiment%x_pixel_size
                       experiment%y_beam = &
                         experiment%y_beam / experiment%y_pixel_size

!                    Output information to user
                       If (full_info) Then
                          Write (MESSAGE(1), '(''INFO: Best fit ' // &
                            'radius (mm) = '', g14.7)') radius1 * 1000.0
                          Call IO_WRITE (MESSAGE(1), status)
                          Write (MESSAGE(1), '(''INFO: Best fit ' // &
                            'radius (X-pixels) = '', g14.7)') radius1 / &
                            experiment%x_pixel_size
                          Call IO_WRITE (MESSAGE(1), status)
 
                          If (num_coordinates .Gt. 3) Then
                             Write (MESSAGE(1), &
                               '(''INFO: Estimated coordinate ' // &
                               'radial position error (mm) = '',' // 'g14.7)') &
                               radial_error * 1000.0
                             Call IO_WRITE (MESSAGE(1), status)
                             Write (MESSAGE(1), &
                               '(''INFO: Estimated coordinate ' // &
                               'radial position error (X pixels) = '',' &
                               // 'g14.7)') &
                               radial_error / experiment%x_pixel_size
                             Call IO_WRITE (MESSAGE(1), status)
                          Else
                             Call IO_WRITE ( 'INFO: The coordinate radial ' // &
                               'position error is totally undetermined', &
                               status)
                          End If
 
                       End If
 
                    End If
 
                    method_used = 4
 
                 Else
 
!                 GRAPHICAL COORDINATE input
                    num_coordinates = 1
                    MESSAGE(1) = 'Click directly on the ' // &
                      'estimated position of the'
                    MESSAGE(2) = 'beam centre.'
                    Call GS_INPS_FCOORDINATES ( mask_data, .True., xmaxdat, &
                      ymaxdat, xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, &
                      X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
                      'DEFINE BEAM/SYMMETRY CENTRE', 2, MESSAGE, .False., 1, &
                      num_coordinates, experiment%x_beam, experiment%y_beam, &
                      status)
 
!                 Check for user escape
                    If (status .Eq. St_escapevalue) Then
                       status = St_goodvalue
                       method_used = 0
                       Return
                    Else If (status .Ne. St_goodvalue) Then
                       Return
                    End If
 
                    method_used = 2
                 End If
 
!              Output results to user
                 Write (MESSAGE(1), '(''INFO: Beam/symmetry ' // &
                   'centre (mm) = ('', g14.7, '','', g14.7, '')'')') &
                   experiment%x_beam * experiment%x_pixel_size * 1000.0, &
                   experiment%y_beam * experiment%y_pixel_size * 1000.0
                 Call IO_WRITE (MESSAGE(1), status)
                 Write (MESSAGE(1), '(''INFO: Beam/symmetry ' // &
                   'centre (pixels) = ('', g14.7, '','', ' // 'g14.7, '')'')') &
                   experiment%x_beam, experiment%y_beam
                 Call IO_WRITE (MESSAGE(1), status)
 
!              Display beam centre on image
                 Call GS_MARK (experiment%x_beam, experiment%y_beam, 2.0, &
                   status)
                 Call GS_UPDATE (status)
 
              Else If (command .Eq. 'KEYBOARD') Then
 
!              Input peak centre by keyboard input
                 Call F2D_INP_BEAMCENTRE (gui, experiment%x_beam, &
                   experiment%y_beam, status)
 
                 method_used = 1
                 continue = .False.
 
              Else If (command .Eq. 'NO CHANGE') Then
 
!              Use existing centre
                 continue = .False.
                 method_used = 0
 
              End If
 
           End If

           experiment%beam_centre_set = .True.
 
!        Change graphics output if required
           If (update) Then
 
!           Redraw image
              Call GS_MPLOT (mask_data, xmaxdat, ymaxdat, DATA, MASK, X_AXIS, &
                Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                ylabel, zlabel, status)
 
!           Re-draw menu
              Call GS_FMENU (1, 1, 'BEAM CENTRE MENU', Max_menu, num_menu, &
                MENU, status)
 
           End If
 
!        Check status
           Call ST_OUT (status)
           If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
        If (.Not. gui) Then
 
!        User message
           Call GS_FPROMPT (1, 1, 'CONTROL RETURNED TO TERMINAL WINDOW', status)
           Call GS_UPDATE (status)
 
        End If
 
     End If
 
     End Subroutine F2D_BEAMCENTRE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 
 
