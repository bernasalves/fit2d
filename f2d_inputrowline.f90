!********1*********2*********3*********4*********5*********6*********7**
 
!  ************************
!  *                      *
!  * f2d_inputrowline.f90 *
!  *                      *
!  ************************
 
!+ F2D_INPUTROWLINE - Fit2D: INPUT ROW LINE parameters
     Subroutine F2D_INPUTROWLINE (EXPERIMENT, xmaxdat, &
       ymaxdat, xstrelm, ystrelm, xendelm, yendelm, XAXIS, YAXIS, DATA, &
       maxorders, MODEL, loworder, highorder, x_centre, y_centre, xrowcen, &
       yrowcen, rowline_angle, ppangle, ppashift, ratio, sigradius, sigtheta, &
       INTENSITIES, status)
!  Description:
!    Inputs parameters necessary to create a row line
!
!    Model
!    -----
!
!    This routine models the row lines of off-meridian reflections
!    formed in the diffraction pattern of Collagen. The model is
!    symmetric about a centre of symmetry "(x_centre, y_centre)".
!    Each double line is also reflected about a symmetry point
!    "(xrowcen, yrowcen)".
!    The vector ["(xrowcen, yrowcen)" -> "(x_centre, y_centre)"] forms a
!    reference line for rotations. The angle from this vector to the
!    line of peaks is "rowline_angle" (in radians). The angle from the
!    this centre symmetry centre to the zero order peak is "ppashift".
!    The centre of the peaks are then repeated at an angular spacing
!    "ppangle". The intensities of the right bottom line are
!    defined by the array "INTENSITIES(-maxorders: maxorders)" which
!    contains the intensites of the peaks from "loworder" to
!    "highorder". The intensities of the reflected line are
!    similar to those of the primary line but are multiplied by a factor
!    "ratio". This takes into account the orientation of the
!    sample.
!    These parameters therefore define a series of
!    peak centres and peak central intensities. Each individual peak
!    is assumed to have a Gaussian profile in the radial direction
!    defined by the standard deviation "sigradius" and a Gaussian
!    profile in the angle (along a circle) defined by the angle "sigtheta".
!  Keywords:
!    Input.Row-line, Row-line.Input
!  Method:
!    Inputs parameters from user.
!  Deficiencies:
!    Conversion of axes parameters in world coordinates to
!    pixel coordinates is not properly done at present; the value
!    is assumed to apply to the X-direction only
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    13-Mar-2006: V0.8 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    01-Dec-1996: V0.7 Calculate intensities from the model and subtract the 
!      row-line from the model (Hammersley)
!    11-Feb-1996: V0.6 Changes to "F2D_GEOMETRY" (Hammersley)
!    03-Jan-1996: V0.5 Changes necessary for IBM AIX "xlf" compiler (Hammersley)
!    24-Jan-1995: V0.4 Input pixel sizes from arguments (Hammersley)
!    12-Nov-1993: V0.3 Change "MA_DC2PIXEL" to "MA_DC2PIXC" (Hammersley)
!    12-Mar-1993: V0.2 Row-line peak to peak spacing specified as
!      an angle (Hammersley)
!    27-Jan-1993: V0.1 Original, based on "FIT2IROW" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     TYPE(EXPERIMENTAL_DETAILS), Intent(INOUT) :: EXPERIMENT ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: xmaxdat ! First dimension of array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Array of X-coordinate values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Array of Y-coordinate values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data to be fitted
     Integer, Intent(IN) :: maxorders ! Dimension size of "INTENSITIES"
!  Import/Export:
     Real, Intent(INOUT) :: MODEL(xmaxdat, ymaxdat) ! Initial fit model
!  Export:
     Integer, Intent(OUT) :: loworder ! Lowest order to input
     Integer, Intent(OUT) :: highorder ! Highest order to input
     Real, Intent(OUT) :: x_centre ! The centre of the symmetry system in
!      the X-direction
     Real, Intent(OUT) :: y_centre ! The centre of the symmetry system in
!      the Y-direction
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
     Real, Intent(OUT) :: INTENSITIES(-maxorders: maxorders)
!      The intensities of the primary row-line peaks (other intensities are 
!      taken relative to these ones)
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
!  Local Variables:
     Character(Len = 132) :: format ! Format string for I/O
     Character(Len = 80) :: prompt ! User prompt
     Integer :: order ! Loop variable for peaks orders
     Integer :: x ! Loop variable for X-direction
     Integer :: xend ! X-end of region around central data point
     Integer :: xstr ! X-start of region around central data point
     Integer :: y ! Loop variable for Y-direction
     Integer :: yend ! Y-end of region around central data point
     Integer :: ystr ! Y-start of region around central data point
     Real :: delta_latitude ! Change in latitude
     Real :: peak_angle ! Angle along row-line of a peak
     Real :: peak_latitude ! Latitude angle for a peak centre
     Real :: peak_longitude ! Longitude angle for a peak centre
     Real :: ppdistance ! The peak to peak distance i.e. the spacing
!      along the row lines of the peaks
     Real :: ppshift ! The distance phase shift of the zero order peak
!      along the row line. This is the distance from the reflection
!      centre to the zero order peak. Positive values correspond to a
!      peak to the right of the reflection point, on the bottom line
     Real :: radial_distance ! Distance from beam centre to row-line centre
     Real :: rowline_distance ! Distance from sample to row-line
     Real :: rowline_latitude ! Latitude angle for the centre of the row-line
     Real :: x_up ! X-component of "Up" vector as viewed on detector
     Real :: xcenpeak ! The X-centre of a Bragg peak
     Real :: xcoord ! X-centre of a row-line peak in pixel coordinates
     Real :: xrel ! The X-component of the vector from the symmetry
!      centre to the row-line centre
     Real :: y_up ! Y-component of "Up" vector as viewed on detector
     Real :: ycenpeak ! The Y-centre of a Bragg peak
     Real :: yrel ! The Y-component of the vector from the symmetry
!      centre to the row-line centre
     Real :: sum ! Sum of elements within a region
     Real :: ycoord ! Y-centre of a row-line peak in pixel coordinates
!  Local Arrays:
     Character(Len = 80) :: ERROR(10) ! Contains error text for the user
     Character(Len = 80) :: MESSAGE(10) ! Contains messages to user
!    Local data:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INPUTROWLINE ' // Version)
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
     Else If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_INPUTROWLINE ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
 
!  Orders to be calculated and added
     loworder = -10
     Call IO_INPI (.True., -maxorders, maxorders, .True., 'LOWEST ORDER', 1, &
       'Lowest order to be simulated', 1, &
       'Must be within set limit, which are program defined', loworder, &
       status)
 
     highorder = 10
     Call IO_INPI (.True., loworder, maxorders, .True., 'HIGHEST ORDER', 1, &
       'Highest order to be simulated', 1, &
       'Must be within set limit, which are program defined', highorder, &
       status)
 
!  Input Centre of row line from the user
     xrowcen = (XAXIS(xendelm) + XAXIS(xstrelm)) / 2.0
     yrowcen = (YAXIS(yendelm) + 3.0 * YAXIS(ystrelm)) / 4.0
     Call IO_INPR (.False., 0.0, 0.0, .True., 'ROW-LINE CENTRE X-COORDINATE', &
       1, 'X-coordinate of centre of lower (or right) row-line', 1, &
       'Enter valid real value', xrowcen, status)
     Call IO_INPR (.False., 0.0, 0.0, .True., 'ROW-LINE CENTRE Y-COORDINATE', &
       1, 'Y-coordinate of centre of lower (or right) row-line', 1, &
       'Enter valid real value', yrowcen, status)
 
!  Calculate relative distances and radial distance from beam centre
     xrel = (xrowcen - x_centre) * EXPERIMENT%x_pixel_size
     yrel = (yrowcen - y_centre) * EXPERIMENT%y_pixel_size
     radial_distance = Sqrt(xrel**2 + yrel**2)
 
!  Angle (in degrees anticlockwise) from centre symmetry to row line
!  centre vector and direction of row line
     rowline_angle = 3.0
     MESSAGE(1) = 'Rotation angle of the row line from row-line "equator" '
     Message(2) = 'The angle is to be defined in degrees (positive means '// &
       'anticlockwise)'
     Call IO_INPR (.True., -90.0, 90.0, .True., 'ROW LINE ROTATION', 2, &
       MESSAGE, 1, 'Must be within set limits', rowline_angle, status)
     rowline_angle = rowline_angle * pi / 180.0
 
!  Distance between peaks
     ppdistance = 18.0
     Call IO_INPR (.True., 0.0, 1.7e38, .True., 'PEAK TO PEAK DISTANCE', 1, &
       'Spacing between Bragg peaks', 1, 'Must be within set limits', &
       ppdistance, status)
 
!  Convert distance into an angle in radians
     rowline_distance = Sqrt(EXPERIMENT%detector_distance**2 + &
       radial_distance**2)
     ppangle = ppdistance * EXPERIMENT%x_pixel_size / rowline_distance
 
!  Distance from row line centre to zero order peak centre
     ppshift = 9.0
     Call IO_INPR (.True., 0.0, 1.7e38, .True., 'ZERO ORDER PEAK DISTANCE', &
       1,'Distance from row line centre to zero order peak centre', &
       1,'Must be within set limits', ppshift, status)
     ppashift = ppshift * EXPERIMENT%x_pixel_size / rowline_distance
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''radial_distance = '', g)') radial_distance
!  Write (*, '(''rowline_distance = '', g)') rowline_distance
!  Write (*, '(''ppangle = '', g)') ppangle
!  Write (*, '(''ppashift = '', g)') ppashift
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Ratio of reflected row-line peak intensities to those of the
!  primary row-line
     ratio = 1.0
     MESSAGE(1) = &
       'Ratio of reflected row-line peak intensities to those of the'
     MESSAGE(2) = 'primary row-line peaks'
     Call IO_INPR (.True., 0.0, 1.7e38, .True., 'LEFT RIGHT INTENSITY RATIO', &
       2, MESSAGE, 1,'Must be within set limits', ratio, status)
 
!  Radial standard deviation width of peaks
     sigradius = 2.5
     Call IO_INPR (.True., 0.0, 1.7e38, .True., 'RADIAL STANDARD DEVIATION', &
       1, 'Standard deviation of peaks in the radial direction', 1, &
       'Must be within set limits', sigradius, status)
 
!  Angular standard deviation in degrees
     sigtheta = 0.1
     MESSAGE(1) = 'Standard deviation of peaks in the angular direction'
     MESSAGE(2) = '(in degrees)'
     Call IO_INPR (.True., 0.0, 1.7e38, .True., 'ANGULAR STANDARD DEVIATION', &
       2, MESSAGE, 1, 'Must be within set limits', sigtheta, status)
     sigtheta = sigtheta * pi / 180.0
 
!  Loop for each maximum peak intenisty
!  Do order=loworder,highorder
 
!  INTENSITIES(order)=500.0
!  Write (prompt,'(1x,''INTENSITY ORDER '',i3)') order
!  Write (MESSAGE,'(1x,''Enter maximum peak intensity for ''//
!  :       ''order '',i3)') order
!  Call IO_INPR (.True., 0.0, 1.7e38, .True.,
!  :       prompt, 1, MESSAGE, 1 ,ERROR,
!  :       INTENSITIES(order), status)
!  End Do
 
!  Estimate peak intenisties by the average data value around the
!  centre of the peak minus the model value
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!  Calculate "Up" vector on detector
     x_up = -xrel / radial_distance
     y_up = -yrel / radial_distance
 
!  Calculate row-line centre radial angle
     rowline_latitude = &
       Sign(Atan(radial_distance / EXPERIMENT%detector_distance), yrel)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Row-line angle = '', g, '' degrees'')')
!  :    rowline_angle * 180.0 / Pi
!  Write (*, '(''Row-line radial distance = '', g)')
!  :    radial_distance
!  Write (*, '(''X_UP = '', g, '' degrees'')') x_up
!  Write (*, '(''Y_UP = '', g, '' degrees'')') y_up
!  Write (*, '(''Row-line latitude = '', g, '' degrees'')')
!  :    rowline_latitude * 180.0 / Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Create primary row-line
     Do order = loworder, highorder
 
!     Calculate angle along the row-line
        peak_angle = -(Real(order) * ppangle + ppashift)
        delta_latitude = peak_angle * Sin(rowline_angle)
 
!     Calculate peak angles
        peak_longitude = peak_angle * Cos(rowline_angle)
        peak_latitude = rowline_latitude + delta_latitude
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''order = '', i)') order
!     Write (*, '(''peak angle = '', g, '' degrees'')')
!     :       peak_angle * 180.0 / Pi
!     Write (*, '(''peak longitude = '', g, '' degrees'')')
!     :       peak_longitude * 180.0 / Pi
!     Write (*, '(''peak latitude = '', g, '' degrees'')')
!     :       peak_latitude * 180.0 / Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Calculate projection coordinates on detector
        Call F2D_CAL_PROJECTION (EXPERIMENT%detector_distance, &
          EXPERIMENT%x_pixel_size, EXPERIMENT%y_pixel_size, &
          x_centre, y_centre, x_up, y_up, peak_longitude, peak_latitude, &
          xcenpeak, ycenpeak, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''xcenpeak = '', g)') xcenpeak
!     Write (*, '(''ycenpeak = '', g)') ycenpeak
!     Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Convert peak centre in world coordinates to pixel coordinates
        Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, xcenpeak, xcoord, status)
        Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, ycenpeak, ycoord, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''xcoord = '', g)') xcoord
!     Write (*, '(''ycoord = '', g)') ycoord
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Calculate average data intensity based on 3*3 square around the
!     central pixel
        xstr = Max(1, Int(xcoord))
        xend = Min(xendelm, Int(xcoord) + 2)
        ystr = Max(1, Int(ycoord))
        yend = Min(yendelm, Int(ycoord) + 2)
        sum = 0.0
 
        If ((xend - xstr + 1) .Ge. 1 .And. (yend - ystr + 1) .Ge. 1) Then
 
           Do y = ystr, yend
 
              Do x = xstr, xend
                 sum = sum + MODEL(x, y)
              End Do
 
           End Do
 
           INTENSITIES(order) = sum / Real((xend - xstr + 1) * (yend - ystr + &
             1)) - MODEL(Int(xcoord) + 1, Int(ycoord) + 1)
 
        Else
 
!        Peak centre is outside data region, input height manually
           Write (prompt,'(1x,''INTENSITY ORDER '',i3)') order
           format = '(''Centre of Peak order '', i3,' // &
             ''' is outside data region'')'
           Write (MESSAGE(1), format) order
           MESSAGE(2) = ' please input intensity'
           Call IO_INPR (.True., 0.0, 1.7e38, .True., prompt, 2, MESSAGE, 1 &
             ,ERROR, INTENSITIES(order), status)
 
        End If
 
     End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''F2D_INPUTROWLINE: Intensities calculated'')')
!  Do order = loworder, highorder
!  Write (*, '(''Order = '', i3, '' intensity = '', g14.5)')
!  :       order, INTENSITIES(order)
!  End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Subtract row-line from the model
     Do order = loworder, highorder
        INTENSITIES(order) = -INTENSITIES(order)
     End Do
     Call F2D_ROWLINE (EXPERIMENT, xmaxdat, ymaxdat, xstrelm, &
       ystrelm, xendelm, yendelm, 1, maxorders, loworder, highorder, x_centre, &
       y_centre, xrowcen, yrowcen, rowline_angle, ppangle, ppashift, ratio, &
       sigradius, sigtheta, INTENSITIES, XAXIS, YAXIS, MODEL, status)
 
!  Re-set correct intensity values
     Do order = loworder, highorder
        INTENSITIES(order) = -INTENSITIES(order)
     End Do
 
     End Subroutine F2D_INPUTROWLINE
!********1*********2*********3*********4*********5*********6*********7**
