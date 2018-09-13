!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_inputmodel.f90 *
!  *                    *
!  **********************
 
!+ F2D_INPUTMODEL - FIT2D: INPUT parameters of PEAKS
     Subroutine F2D_INPUTMODEL (EXPERIMENT, xmaxdat, ymaxdat, &
       xnumdat, ynumdat, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, zlabel, &
       xmaxcoeff, ymaxcoeff, maxpeak, maxorders, xstrelm, ystrelm, xendelm, &
       yendelm, MASK, RESIDUALS, polynomial_defined, x_order, y_order, &
       xmin_poly, ymin_poly, xmax_poly, ymax_poly, COEFFICIENTS, xpolcen, &
       ypolcen, numpeak, PEAKTYPE, XPEAKCEN, YPEAKCEN, PEAKMAX, ORIENT, &
       MAJHWHM, MINHWHM, TWINMAX, rowline_defined, loworder, highorder, &
       xrowcen, yrowcen, rowline_angle, ppangle, ppashift, ratio, sigradius, &
       sigtheta, INTENSITIES, status)
!  Description:
!    Allow user to initialise model for fitting. The parameters of a
!    2-D polynomial and various types of peaks can be defined. the
!    positions of the half-width half-maximum of the major and minor
!    axes (the major axis is taken for the orientation, whereas the
!    minor axis is used only for the extent).
!    The number of peak positions is returned in "numpeak" and the
!    arrays "PEAKTYPE", "XPEAKCEN", "YPEAKCEN", etc. are used to
!    return the type of peak, the central position, and the HWHM
!    of the major and minor axes, and the orientation of the major
!    axis. "PEAKMAX" returns the estimated peak intensity, which
!    is the intensity at the centre minus the calculated value of
!    the polynomial background function and previously input peaks
!    at that point.
!  Method:
!    The "RESIDUALS" should start with the data values. Each feature
!    is subtracted from the current "RESIDUALS". The the remaining
!    intensity is used to initialise feature intensities. At the
!    end "RESIDUALS" contains the residuals.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Mar-2006: V0.10 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    13-Mar-2006: V0.9 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.8 Alter menu lay-out for landscape windows (Hammersley)
!    26-Nov-2003: V0.7 Set "num_coordinates" to 1 when only one coordinate
!      is to be input, to stop possible failures (Hammersley)
!    11-Oct-2001: V0.6 Change calls to "GS_INPS_FCOORDINATES" so that
!      the number of coordinates input is returned to "num_coordinates".
!      This is not needed, but previously this was the constant 1, and
!      the Windows compiler correctly doesn't like overwriting constants.
!      (Hammersley)
!    04-Dec-1996: V0.5 Improve peak initialise by calculating the
!      initial height from the twice maximum minus the half height (Hammersley)
!    01-Dec-1996: V0.4 Option to input 2-D fitting polynomial in
!      Chebyshev form, resulting in argument list changes (Hammersley)
!    20-Jun-1995: V0.3 Convert to GS graphics library (Hammersley)
!    12-Nov-1993: V0.2 Change "MA_DC2PIXEL" to "MA_DC2PIXC" (Hammersley)
!    26-Jan-1993: V0.1 Original, based on FIT2PEAK (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: EXPERIMENT ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xnumdat ! Number of defined pixels in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of defined pixels in Y-direction
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Axis data for the Y-axis
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Integer, Intent(IN) :: xmaxcoeff ! SECOND dimension of 2-D polynomial
!      fit coefficient array
     Integer, Intent(IN) :: ymaxcoeff ! FIRST dimension of 2-D polynomial
!      fit coefficient array
     Integer, Intent(IN) :: maxpeak ! The dimension size for the peak defining 
!      arrays, and hence the maximum number of peaks which may be defined
     Integer, Intent(IN) :: maxorders ! Dimension size of "INTENSITIES"
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: xendelm ! The X-pixel number for the end of
!      the ROI
     Integer, Intent(INOUT) :: yendelm ! The Y-pixel number for the end of
!      the ROI
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Real, Intent(INOUT) :: RESIDUALS(xmaxdat, ymaxdat) ! The value of the
!      user initialised fit model
!  Export:
     Logical, Intent(OUT) :: polynomial_defined ! .True., if the fit model
!      contains a polynomial
     Integer, Intent(OUT) :: x_order ! Order of background polynomial in
!      X-direction
     Integer, Intent(OUT) :: y_order ! Order of background polynomial in
!      Y-direction
     Real, Intent(OUT) :: xmin_poly ! X-coordinate of minimum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(OUT) :: ymin_poly ! Y-coordinate of minimum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(OUT) :: xmax_poly ! X-coordinate of maximum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(OUT) :: ymax_poly ! Y-coordinate of maximum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(OUT) :: COEFFICIENTS(ymaxcoeff, xmaxcoeff)
!      2-D polynomial coefficients in Chebyshev form. NOTE: Unusual order of
!      storage of coefficients for efficiency reasons
     Real, Intent(OUT) :: xpolcen ! X-centre of polar coordinate system
     Real, Intent(OUT) :: ypolcen ! Y-centre of polar coordinate system
     Integer, Intent(OUT) :: numpeak ! The number of defined peaks
     Integer, Intent(OUT) :: PEAKTYPE(maxpeak) ! the type of each of the
!      defined peaks:
!        1 = 2-D Gaussian
!        2 = Lorentzian
!        3 = 2-D Gaussian in polar coordinate system
!        4 = 2-D twin Gaussian in polar coordinate system
!        Other values may be defined in the future
     Real, Intent(OUT) :: XPEAKCEN(maxpeak) ! X-axis world coordinate of the
!      centre of each peak
     Real, Intent(OUT) :: YPEAKCEN(maxpeak) ! Y-axis world coordinate of the
!      centre of each peak
     Real, Intent(OUT) :: PEAKMAX(maxpeak) ! Estimated peak height above the
!      background
     Real, Intent(OUT) :: ORIENT(maxpeak) ! Orientation of each peak, this
!      is the angle anti-clockwise from the X-axis in radians to the major axis
     Real, Intent(OUT) :: MAJHWHM(maxpeak) ! The (estimated) half-width at
!      half-maximum of the major axis of the peaks
     Real, Intent(OUT) :: MINHWHM(maxpeak) ! The (estimated) half-width at
!      half-maximum of the minor axis of the peaks
     Real, Intent(OUT) :: TWINMAX(maxpeak) ! The maximum intensity of the
!      second peak of a twin pair
     Logical, Intent(OUT) :: rowline_defined ! .True., if the fit model
!      contains a row-line
     Integer, Intent(OUT) :: loworder ! Lowest order to input
     Integer, Intent(OUT) :: highorder ! Highest order to input
     Real, Intent(OUT) :: xrowcen ! The X-coordinate of the centre of the line
     Real, Intent(OUT) :: yrowcen ! The Y-coordinate of the centre of the line
     Real, Intent(OUT) :: rowline_angle ! The angle in RADIANS between the
!      row-line axis and the row-line "equator"
     Real, Intent(OUT) :: ppangle ! The peak to peak angle in radians
     Real, Intent(OUT) :: ppashift ! The angle phase shift of the zero order
!      peak along the row line. This is the distance from the reflection
!      centre to the zero order peak. Positive values correspond
!      to a peak to the right of the reflection point, on the bottom line
     Real, Intent(OUT) :: ratio ! The ratio of the reflected peak
!      intensities to those of the primary line of peaks
     Real, Intent(OUT) :: sigradius ! The radial sigma width of the peaks
     Real, Intent(OUT) :: sigtheta ! The angular sigma width of the peaks
     Real, Intent(OUT) :: INTENSITIES(-maxorders: maxorders) ! The intensities 
!      of the primary row-line peaks (other intensities are taken relative to
!      these ones)
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.10' ! Version number
     Integer, Parameter :: Max_menu = 9 ! Dimension size of commands array
!  Local Variables:
     Character(Len = 20) :: command ! Choice entered by user
     Integer :: input_type ! Type of graphical input
     Integer :: num_coordinates ! Number of input coordinates
     Integer :: num_menu ! Number of commands in menu
     Integer :: retstat ! Return status variable:
!      0 = Good status
!      1 = Bad status, not enough independent data points in the
!          X-direction for the required X-order polynomial fit for
!          one or more of the rows
!      2 = Bad status, not enough independent data points in the
!          X-direction for the required Y-order polynomial fit for
!          one or more of the columns
     Integer stat ! Status return variable for "Allocate"
     Integer :: x ! Loop variable for the X-direction
     Integer :: x_element ! Input coordinate converted to X-direction pixel
     Integer :: xend ! X-end pixel number
     Integer :: xorderinp ! Order of initialisation background polynomial in 
!      X-direction
     Integer :: xstr ! X-start pixel number
     Integer :: y ! Loop variable for the Y-direction
     Integer :: y_element ! Input coordinate converted to Y-direction pixel
     Integer :: yend ! Y-end pixel number
     Integer :: yorderinp ! Order of initialisation background polynomial in 
!      Y-direction
     Integer :: ystr ! Y-start pixel number
     Logical :: centre_undefined ! .True., until the centre of the polar
!      coordinate system has been defined
     Logical :: continue ! .True., until end of input session
     Logical :: update_image ! .True., if image needs to be re-drawn
     Logical :: update_menu ! .True., if menu needs to be re-drawn
     Real :: disgaucen ! Distance from the polar centre to the centre of
!      a polar peak
     Real :: half1 ! Intensity at first half height coordinate
     Real :: half2 ! Intensity at second half height coordinate
     Real :: peak_max ! Intensity at peak maximum coordinate
     Real :: sum ! Summation variable
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: x_pixel ! X-pixel coordinate
     Real :: xpos ! Returned X-coordinate position
     Real :: xtwincen ! X-coordinate of centre of twin polar Gaussian
     Real :: y_coordinate ! Graphical input Y-coordinate
     Real :: y_pixel ! Y-pixel coordinate
     Real :: ypos ! Returned Y-coordinate position
     Real :: ytwincen ! Y-coordinate of centre of twin polar Gaussian
!  Local Arrays:
     Character(Len = 20), Save :: MENU(Max_menu) ! Commands choices for
!      graphical menu
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu text
     Real, Allocatable :: COEFFS_1D(:) ! Dynamic work array to hold 1-D 
!      X-coefficients for each row
     Real, Allocatable :: X_COEFFS(:) ! Dynamic work array
!  External Functions:
!  Local Data:
     Data MENU / 'EXIT', '?', '2-D POLYNOMIAL', 'GAUSSIAN', 'MASK', &
       'POLAR GAUSSIAN', 'ROW-LINE', 'REMOVE LAST PEAK', &
       'TWIN POLAR GAUSSIAN'/
     Data MENUTXT / 'EXIT: No more peaks to define, exit model defining menu', &
       '?: This help text', &
       '2-D POLYNOMIAL: Define general "background" polynomial', &
       'GAUSSIAN: Add 2-D Gaussian peak parameters to fit model', &
       'MASK: Mask-off or un-mask regions of the data', &
       'POLAR GAUSSIAN: Add 2-D polar Gaussian parameters to model', &
       'ROW-LINE: Add a "row-line" of peaks to the fit model', &
       'REMOVE LAST PEAK: Remove previous peak from model', &
       'TWIN POLAR GAUSSIAN: Add symmetric polar Gaussians to model'/
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INPUTMODEL ' // Version)
        Return
     End If
 
!  Check subroutine input argument variables
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xendelm .Gt. xmaxdat .Or. xstrelm .Gt. &
       xendelm) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. yendelm .Gt. ymaxdat .Or. ystrelm .Gt. &
       yendelm) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_INPUTMODEL ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Loop inputing menu commands or peak parameters until requested to stop
     num_menu = Max_menu
     numpeak = 0
     polynomial_defined = .False.
     rowline_defined = .False.
     centre_undefined = .True.
     update_image = .False.
     update_menu = .True.
 
     continue = .True.
     Do While (continue)
 
        If (update_image) Then
 
!        Re-draw image
           Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, X_AXIS, &
             Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
             ylabel, zlabel, status)
 
        End If
 
        If (update_menu) Then
 
!        Inquire window format
           Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!        Re-draw menu
           If (width / height .Gt. 1.2) Then
              Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 20, status)
           Else
              Call GS_SET_MENULAYOUT (gs_horizontal, 3, 12, 20, status)
           End If
 
           Call GS_FMENU (1, 1, 'PEAK TYPE:', Max_menu, num_menu, MENU, &
             status)
 
        End If
 
!     By default no update
        update_image = .False.
        update_menu = .False.
 
!     Get user to select between the available menu options
        Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 1, input_type, &
          command, x_coordinate, y_coordinate, status)
 
        If (input_type .Eq. Gs_resize) Then
 
           update_image = .True.
           update_menu = .True.
 
        Else If (input_type .Eq. Gs_choice) Then
 
!        Carry out menu choices
           If (command .Eq. 'EXIT') Then
              continue = .False.
              update_image = .False.
              update_menu = .False.
 
           Else If (command .Eq. '?') Then
 
!           Display list of available commands
              Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
              update_image = .True.
              update_menu = .True.
 
           Else If (command .Eq. '2-D POLYNOMIAL') Then
 
!           Check that the polynomial is not already defined
              If (polynomial_defined) Then
 
                 Call GS_FWARNING (1, 1, &
                   'The 2-D polynomial is already defined', status)
 
              Else
 
!              Enter order of background polynomial
                 x_order = 0
                 y_order = 0
                 Call GS_INPI (.True., 0, xmaxcoeff - 1, .True., &
                   '2-D POLYNOMIAL X ORDER', 1, &
                   'Order of polynomial function in X-direction', 1, &
                   'Must be within specified bounds', x_order, status)
                 Call GS_INPI (.True., 0, ymaxcoeff - 1, .True., &
                   'POLYNOMIAL Y ORDER', 1, &
                   'Order of polynomial function in Y-direction', 1, &
                   'Must be within specified bounds', y_order, status)
 
!              Enter order of initialisation polynomial (all other
!              coefficients will be set to zero)
                 xorderinp = x_order
                 yorderinp = y_order
                 Call GS_INPI (.True., 0, x_order, .True., &
                   'INITIALISATION POLYNOMIAL X ORDER', 1, &
                   'Order of initialisation polynomial function ' // &
                   'in X-direction', 1, 'Must be within specified bounds', &
                   xorderinp, status)
                 Call GS_INPI (.True., 0, y_order, .True., &
                   'INITIALISATION POLYNOMIAL Y ORDER', 1, &
                   'Order of initialisation polynomial function ' // &
                   'in Y-direction', 1, 'Must be within specified bounds', &
                   yorderinp, status)
 
!              Set all polynomial coefficients to zero
                 Call MA_RVALUE (ymaxcoeff, xmaxcoeff, 1, 1, y_order + 1, &
                   x_order + 1, 0.0, COEFFICIENTS, status)
 
!              Fit 2-D polynomial to the residuals
                 Call F2D_FITCHEBYSHEV (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, RESIDUALS, X_AXIS, Y_AXIS, MASK, &
                   xmaxcoeff, ymaxcoeff, xorderinp, yorderinp, retstat, &
                   xmin_poly, ymin_poly, xmax_poly, ymax_poly, COEFFICIENTS, &
                   status)
 
                 If (retstat .Ne. 0) Then
 
                    polynomial_defined = .False.
 
!                 Output warning
                    Call GS_FWARNING (1, 1, &
                      'Failed to fit polynomial coefficients', status)
 
                 Else
 
                    polynomial_defined = .True.
 
!                 Temporarily negate all the coefficients so that
!                 the polynomial values are subtracted from the residuals
!                 instead of being added
                    Call MA_RCMULT (ymaxcoeff, xmaxcoeff, 1, 1, y_order + 1, &
                      x_order + 1, -1.0, COEFFICIENTS, status)
 
!                 Allocate work space
                    Allocate (X_COEFFS(x_order + 1), Stat = stat)
                    If (stat .Ne. 0) Then
                       status = St_mod_fit2d + St_bad_malloc
                       Call ST_SAVE ('Subroutine F2D_INPUTMODEL ' // Version)
                       Return
                    End If
                    Allocate (COEFFS_1D((x_order + 1) * &
                      (yendelm - ystrelm + 1)), Stat = stat)
                    If (stat .Ne. 0) Then
                       status = St_mod_fit2d + St_bad_malloc
                       Call ST_SAVE ('Subroutine F2D_INPUTMODEL ' // Version)
                       Return
                    End If

!                 Calculate polynomial values
                    Call MA_CAL_2DCHEBYSHEV (.True., xmin_poly, ymin_poly, &
                      xmax_poly, ymax_poly, xmaxcoeff, ymaxcoeff, &
                      COEFFICIENTS, x_order, y_order, xendelm, yendelm, &
                      xstrelm, ystrelm, xendelm, yendelm, x_order + 1, yendelm &
                      - ystrelm + 1, X_AXIS, Y_AXIS, RESIDUALS, &
                      COEFFS_1D, X_COEFFS, status)
 
!                 Negate coefficient values again to undo previous
!                 negation
                    Call MA_RCMULT (ymaxcoeff, xmaxcoeff, 1, 1, y_order + 1, &
                      x_order + 1, -1.0, COEFFICIENTS, status)
 
!                 Free memory
                    Deallocate (X_COEFFS)
                    Deallocate (COEFFS_1D)
 
                 End If
 
              End If
              update_menu = .True.
 
           Else If (command .Eq. 'MASK') Then
 
!           Change mask definition
              Call F2D_MASK (.True., xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, &
                X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, EXPERIMENT, &
                xstrelm, ystrelm, xendelm, yendelm, MASK, status)
              update_menu = .True.
 
           Else If (command .Eq. 'ROW-LINE') Then
 
              If (rowline_defined) Then
 
!              Output warning
                 Call GS_FWARNING (1, 1, &
                   'A "row-line" is already defined (1 maximum)', status)
 
              Else
 
!              Input parameters to define a row-line
                 Call F2D_INPUTROWLINE (EXPERIMENT, xmaxdat, ymaxdat, &
                   xstrelm, ystrelm, xendelm, yendelm, X_AXIS, &
                   Y_AXIS, DATA, maxorders, RESIDUALS, loworder, highorder, &
                   xpolcen, ypolcen, xrowcen, yrowcen, rowline_angle, ppangle, &
                   ppashift, ratio, sigradius, sigtheta, INTENSITIES, status)
 
                 rowline_defined = .True.
              End If
 
              update_menu = .True.
 
           Else If (command .Eq. 'GAUSSIAN' .Or. command .Eq. 'POLAR GAUSSIAN' &
             .Or. command .Eq. 'TWIN POLAR GAUSSIAN') Then
 
              update_menu = .True.
 
              If (numpeak .Lt. Maxpeak) Then
                 numpeak = numpeak + 1
 
!              Set peak type
                 If (command .Eq. 'GAUSSIAN') Then
                    PEAKTYPE(numpeak) = 1
                 Else If (command .Eq. 'POLAR GAUSSIAN') Then
                    PEAKTYPE(numpeak) = 3
                 Else If (command .Eq. 'TWIN POLAR GAUSSIAN') Then
                    PEAKTYPE(numpeak) = 4
                 End If
 
                 If (command .Eq. 'POLAR GAUSSIAN' .Or. command .Eq. &
                   'TWIN POLAR GAUSSIAN' .And. centre_undefined) Then
 
!                 Input polar symmetry centre
                    num_coordinates = 1
                    Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, &
                      ymaxdat, xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, &
                      X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
                      'CLICK ON SYMMETRY CENTRE', 1, &
                      'Place cursor on centre of polar symmetry', .False., 1, &
                      num_coordinates, xpolcen, ypolcen, status)
                    centre_undefined = .False.
 
                 End If
 
!              Input peak centre
                 num_coordinates = 1
                 Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, &
                   xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, X_AXIS, &
                   Y_AXIS, title, xlabel, ylabel, zlabel, &
                   'CLICK ON PEAK CENTRE', 1, &
                   'Place cursor on centre of peak to be defined', .False., 1, &
                   num_coordinates, xpos, ypos, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(
!              :                ''F2D_INPUTMODEL: After
!              GS_INPS_FCOORDINATES'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 XPEAKCEN(numpeak) = xpos
                 YPEAKCEN(numpeak) = ypos
 
!              Convert data coordinates to pixel coordinates
                 Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, xpos, x_pixel, &
                   status)
                 x_element = Int(x_pixel) + 1
                 Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, ypos, y_pixel, &
                   status)
                 y_element = Int(y_pixel) + 1
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(
!              :                ''F2D_INPUTMODEL: x_element, y_element = '',
!              :                2i6)') x_element, y_element
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Calculate average intensity based on 3*3 square
!              around the pixel
                 xstr = Max(1, x_element - 1)
                 xend = Min(xendelm, x_element + 1)
                 ystr = Max(1, y_element - 1)
                 yend = Min(yendelm, y_element + 1)
                 sum = 0.0
 
                 Do y = ystr, yend
 
                    Do x = xstr, xend
                       sum = sum + RESIDUALS(x, y)
                    End Do
 
                 End Do
 
!              Peak intensity is taken as the value at the centre
                 peak_max = sum / Real((xend - xstr + 1) * (yend - ystr + 1))
 
                 If (command .Eq. 'POLAR GAUSSIAN' .Or. command .Eq. &
                   'TWIN POLAR GAUSSIAN') Then
 
!                 Calculate distance from peak to symmetry centre
                    disgaucen = Sqrt((xpos - xpolcen)**2 + (ypos - &
                      ypolcen)**2)
 
                 End If
 
!              Input half height
                 num_coordinates = 1
                 Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, &
                   xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, X_AXIS, &
                   Y_AXIS, title, xlabel, ylabel, zlabel, &
                   'CLICK ON FIRST (OR RADIAL) HALF HEIGHT COORDINATE', &
                   1, 'Place cursor on first half height position', .False., &
                   1, num_coordinates, xpos, ypos, status)
 
!              Convert data coordinates to pixel coordinates
                 Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, xpos, x_pixel, &
                   status)
                 x_element = Int(x_pixel) + 1
                 Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, ypos, y_pixel, &
                   status)
                 y_element = Int(y_pixel) + 1
 
                 xstr = Max(1, x_element - 1)
                 xend = Min(xendelm, x_element + 1)
                 ystr = Max(1, y_element - 1)
                 yend = Min(yendelm, y_element + 1)
                 sum = 0.0
 
                 Do y = ystr, yend
 
                    Do x = xstr, xend
                       sum = sum + RESIDUALS(x, y)
                    End Do
 
                 End Do
 
!              Peak intensity is taken as the value at the centre
                 half1 = sum / Real((xend - xstr + 1) * (yend - ystr + 1))
 
!              Position of major axis, which gives the orientation of
!              The peak and the HWHM of the major axis
                 MAJHWHM(numpeak) = Sqrt((xpos - XPEAKCEN(numpeak))**2 + &
                   (ypos - YPEAKCEN(numpeak))**2)
                 ORIENT(numpeak) = Atan2(ypos - YPEAKCEN(numpeak), &
                   xpos - XPEAKCEN(numpeak))
 
!              Input second half height
                 num_coordinates = 1
                 Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, &
                   xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, X_AXIS, &
                   Y_AXIS, title, xlabel, ylabel, zlabel, &
                   'CLICK ON SECOND (OR ANGULAR) HALF HEIGHT COORDINATE', &
                   1, 'Place cursor on second half height position', .False., &
                   1, num_coordinates, xpos, ypos, status)
 
                 Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, xpos, x_pixel, &
                   status)
                 x_element = Int(x_pixel) + 1
                 Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, ypos, y_pixel, &
                   status)
                 y_element = Int(y_pixel) + 1
 
                 xstr = Max(1, x_element - 1)
                 xend = Min(xendelm, x_element + 1)
                 ystr = Max(1, y_element - 1)
                 yend = Min(yendelm, y_element + 1)
                 sum = 0.0
 
                 Do y = ystr, yend
 
                    Do x = xstr, xend
                       sum = sum + RESIDUALS(x, y)
                    End Do
 
                 End Do
 
!              Peak intensity is taken as the value at the centre
                 half2 = sum / Real((xend - xstr + 1) * (yend - ystr + 1))
 
!              Set initial peak height to be 4 times the maximum
!              minus the two half height values
                 PEAKMAX(numpeak) = 2.0 * peak_max - half1 - half2
 
!              The HWHM of the minor axis
                 MINHWHM(numpeak) = Sqrt((xpos - XPEAKCEN(numpeak))**2 + &
                   (ypos - YPEAKCEN(numpeak))**2)
 
                 If (command .Eq. 'TWIN POLAR GAUSSIAN') Then
 
!                 Input peak centre
                    num_coordinates = 1
                    Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, &
                      ymaxdat, xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, &
                      X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
                      'CLICK ON TWIN PEAK CENTRE', 1, &
                      'Place cursor on centre of twin peak', .False., 1, &
                      num_coordinates, xpos, ypos, status)
 
!                 Convert data coordinates to pixel coordinates
                    Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, xpos, x_pixel, &
                      status)
                    x_element = Int(x_pixel) + 1
                    Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, ypos, y_pixel, &
                      status)
                    y_element = Int(y_pixel) + 1
 
!                 Calculate average intensity based on 3*3 square
!                 around the pixel
                    xstr = Max(1, x_element - 1)
                    xend = Min(xendelm, x_element + 1)
                    ystr = Max(1, y_element - 1)
                    yend = Min(yendelm, y_element + 1)
                    sum = 0.0
 
                    Do y = ystr, yend
 
                       Do x = xstr, xend
                          sum = sum + RESIDUALS(x, y)
                       End Do
 
                    End Do
 
!                 Peak intensity is taken as the value at the centre
                    TWINMAX(numpeak) = sum / Real((xend - xstr + 1) * &
                      (yend - ystr + 1))
 
                 End If
 
!              Subtract input peak from the residuals
                 If (command .Eq. 'GAUSSIAN') Then
 
!                 Subtract 2-D Gaussian from the residuals
                    Call MA_2DGAUSSIAN (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, X_AXIS, Y_AXIS, -PEAKMAX(numpeak), &
                      XPEAKCEN(numpeak), YPEAKCEN(numpeak), MAJHWHM(numpeak) * &
                      0.8496, MINHWHM(numpeak) * 0.8496, ORIENT(numpeak), 4.0, &
                      1, RESIDUALS, status)
 
                 Else If (command .Eq. 'POLAR GAUSSIAN') Then
 
!                 Subtract 2-D polar Gaussian from the residuals
                    Call MA_2DPOLARGAU (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, X_AXIS, Y_AXIS, -PEAKMAX(numpeak), &
                      XPEAKCEN(numpeak), YPEAKCEN(numpeak), xpolcen, ypolcen, &
                      MAJHWHM(numpeak) * 0.8496, MINHWHM(numpeak) * 0.8496 / &
                      disgaucen, 4.0, 1, RESIDUALS, status)
 
                 Else If (command .Eq. 'TWIN POLAR GAUSSIAN') Then
 
!                 Subtract first 2-D polar Gaussian from the residuals
                    Call MA_2DPOLARGAU (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, X_AXIS, Y_AXIS, -PEAKMAX(numpeak), &
                      XPEAKCEN(numpeak), YPEAKCEN(numpeak), xpolcen, ypolcen, &
                      MAJHWHM(numpeak) * 0.8496, &
                      MINHWHM(numpeak) * 0.8496 / disgaucen, 4.0, 1, &
                      RESIDUALS, status)
 
!                 Subtract second peak
                    xtwincen = 2.0 * xpolcen - XPEAKCEN(numpeak)
                    ytwincen = 2.0 * ypolcen - YPEAKCEN(numpeak)
                    Call MA_2DPOLARGAU (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, X_AXIS, Y_AXIS, -TWINMAX(numpeak), &
                      xtwincen, ytwincen, xpolcen, ypolcen, MAJHWHM(numpeak) * &
                      0.8496, MINHWHM(numpeak) * 0.8496 / disgaucen, 4.0, 1, &
                      RESIDUALS, status)
 
                 End If
 
              End If
 
           Else If (command .Eq. 'REMOVE LAST PEAK') Then
 
              If (numpeak .Ge. 1) Then
 
!              Add peak intensity to the residuals
                 If (PEAKTYPE(numpeak) .Eq. 1) Then
 
!                 Add 2-D Gaussian to the residuals
                    Call MA_2DGAUSSIAN (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, X_AXIS, Y_AXIS, PEAKMAX(numpeak), &
                      XPEAKCEN(numpeak), YPEAKCEN(numpeak), MAJHWHM(numpeak) * &
                      0.8496, MINHWHM(numpeak) * 0.8496, ORIENT(numpeak), 4.0, &
                      1, RESIDUALS, status)
 
 
                 Else If (PEAKTYPE(numpeak) .Eq. 3) Then
 
!                 Add 2-D polar Gaussian to the residuals
                    disgaucen = Sqrt((XPEAKCEN(numpeak) - xpolcen)**2 + &
                      (YPEAKCEN(numpeak) - ypolcen)**2)
                    Call MA_2DPOLARGAU (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, X_AXIS, Y_AXIS, PEAKMAX(numpeak), &
                      XPEAKCEN(numpeak), YPEAKCEN(numpeak), xpolcen, ypolcen, &
                      MAJHWHM(numpeak) * 0.8496, MINHWHM(numpeak) * 0.8496 / &
                      disgaucen, 4.0, 1, RESIDUALS, status)
 
                 Else If (command .Eq. 'TWIN POLAR GAUSSIAN') Then
 
!                 Add first 2-D polar Gaussian to the residuals
                    disgaucen = Sqrt((XPEAKCEN(numpeak) - xpolcen)**2 + &
                      (YPEAKCEN(numpeak) - ypolcen)**2)
                    Call MA_2DPOLARGAU (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, X_AXIS, Y_AXIS, PEAKMAX(numpeak), &
                      XPEAKCEN(numpeak), YPEAKCEN(numpeak), xpolcen, ypolcen, &
                      MAJHWHM(numpeak) * 0.8496, MINHWHM(numpeak) * 0.8496 / &
                      disgaucen, 4.0, 1, RESIDUALS, status)
 
!                 Add second peak
                    xtwincen = 2.0 * xpolcen - XPEAKCEN(numpeak)
                    ytwincen = 2.0 * ypolcen - YPEAKCEN(numpeak)
                    Call MA_2DPOLARGAU (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, X_AXIS, Y_AXIS, TWINMAX(numpeak), &
                      xtwincen, ytwincen, xpolcen, ypolcen, MAJHWHM(numpeak) * &
                      0.8496, MINHWHM(numpeak) * 0.8496 / disgaucen, 4.0, 1, &
                      RESIDUALS, status)
 
                 End If
 
                 numpeak = numpeak - 1
 
              End If
 
           End If
 
        End If
 
!     Check status
        If (status .Ne. St_goodvalue) Then
           continue = .False.
        End If
 
     End Do
 
     End Subroutine F2D_INPUTMODEL
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
