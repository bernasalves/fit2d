!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_rowline.f90 *
!  *                 *
!  *******************
 
!+ F2D_ROWLINE - Fit2d: ROW LINE calculation
     Subroutine F2D_ROWLINE (experiment, xmaxdat, ymaxdat, &
       xstrelm, ystrelm, xendelm, yendelm, method, maxorders, loworder, &
       highorder, x_centre, y_centre, xrowcen, yrowcen, rowline_angle, &
       ppangle, ppashift, ratio, sigradius, sigtheta, INTENSITIES, XAXIS, &
       YAXIS, DATA, status)
!  Description:
!    Adds a "row-line" of 2-D polar Gaussian peaks described by the input
!    parameters to the data array "DATA" in the region of interest
!    "(xstrelm, ystrelm)" to "(xendelm, yendelm)".
!
!    Model
!    -----
!
!    See "f2d_inputrowline.f90"
!  Keywords:
!    Row-line.Calculate, Calculate.Row-line
!  Method:
!    Sets up necessary calls to "MA_2DGAUSSIAN"
!  Deficiencies:
!    Conversion of radial sigma parameter in world coordinates to
!    pixel coordinates is not properly done at present; the value
!    is assumed to apply to the X-direction only
!  Bugs:
!    None
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    14-Mar-2006: V0.5 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    24-Jan-1995: V0.4 Input pixel sizes from arguments (Hammersley)
!    15-Mar-1994: V0.3 Change sign of "up" vector, to make right hand side 
!      row-line appear correctly (Hammersley)
!    11-Mar-1993: V0.2 Convert to angle spacing between peaks (Hammersley)
!    27-Jan-1993: V0.1 Original, based on "FIT2ROW" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: xmaxdat ! First dimension of array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Integer, Intent(IN) :: method ! Method to be used to calculate Gaussian
!      peak values:
!        0 = Look-up table, nearest value
!        1 = Look-up table, linear interpolation
!        9 = Elementary function calculation
     Integer, Intent(IN) :: maxorders ! Dimension size of "INTENSITIES"
     Integer, Intent(IN) :: loworder ! Lowest order peak to be calculated
     Integer, Intent(IN) :: highorder ! Highest order peak to be calculated
!      data array
     Real, Intent(IN) :: x_centre ! The centre of the symmetry system in
!      the X-direction
     Real, Intent(IN) :: y_centre ! The centre of the symmetry system in
!      the Y-direction
     Real, Intent(IN) :: xrowcen ! The X-coordinate of the centre of the line
     Real, Intent(IN) :: yrowcen ! The Y-coordinate of the centre of the line
     Real, Intent(IN) :: rowline_angle ! The angle in RADIANS between the
!      "line reflection"  point and the centre of symmetry, and the row line
     Real, Intent(IN) :: ppangle ! The peak to peak angle in radians
     Real, Intent(IN) :: ppashift ! The angle phase shift of the zero order
!      peak along the row line. This is the angle from the reflection centre to
!      the zero order peak. Positive values correspond to a peak to
!      the right of the reflection point, on the bottom line
     Real, Intent(IN) :: ratio ! The ratio of the peak intensities of
!      the reflected row relative to those of the primary
     Real, Intent(IN) :: sigradius ! The radial sigma width of the peaks
     Real, Intent(IN) :: sigtheta ! The angular sigma width of the peaks
     Real, Intent(IN) :: INTENSITIES(-maxorders: maxorders)
!      The intensities of the primary row peaks (other intensities are taken 
!      relative tothese ones)
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Array of X-coordinate values
!      giving the positions of the centre of each pixel in world coordinates
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Array of Y-coordinate values
!      giving the positions of the centre of each pixel in world coordinates
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data array for addition
!      of row lines
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Integer :: order ! Loop variable for the reflection orders
     Real :: delta_latitude ! Change in latitude
     Real :: rowline_latitude ! Latitude angle for the centre of the row-line
     Real :: peak_angle ! Angle along row-line of a peak
     Real :: peak_latitude ! Latitude angle for a peak centre
     Real :: peak_longitude ! Longitude angle for a peak centre
     Real :: radial_distance ! Distance from beam centre to row-line centre
     Real :: x_up ! X-component of "Up" vector as viewed on detector
     Real :: xcenpeak ! The X-centre of a Bragg peak
     Real :: xrel ! The X-component of the vector from the symmetry
!      centre to the row-line centre
     Real :: y_up ! Y-component of "Up" vector as viewed on detector
     Real :: ycenpeak ! The Y-centre of a Bragg peak
     Real :: yrel ! The Y-component of the vector from the symmetry
!      centre to the row-line centre
!  Local Arrays:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''low order/ high order = '', 2i)')
!  :    loworder, highorder
!  Write (*, '(''x_centre, y_centre = '', 2g)') x_centre, y_centre
!  Write (*, '(''xrowcen, yrowcen = '', 2g)') xrowcen, yrowcen
!  Write (*, '(''rowline_angle = '', g)') rowline_angle
!  Write (*, '(''ppangle = '', g)') ppangle
!  Write (*, '(''ppashift = '', g)') ppashift
!  Write (*, '(''ratio = '', g)') ratio
!  Write (*, '(''sigradius = '', g)') sigradius
!  Write (*, '(''sigtheta = '', g)') sigtheta
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ROWLINE ' // Version)
        Return
     End If
 
!  Check that the work array is large enough to produce the data
     If (xstrelm .Le. 0 .Or. xendelm .Gt. xmaxdat .Or. Abs(loworder) .Gt. &
       maxorders .Or. Abs(highorder) .Gt. maxorders) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. yendelm .Gt. ymaxdat) Then
        status = St_bad_adr2
     Else If (xstrelm .Gt. xendelm) Then
        status = St_bad_adr1
     Else If (ystrelm .Gt. yendelm) Then
        status = St_bad_adr2
     Else If (xmaxdat .Le. 0 .Or. maxorders .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (loworder .Gt. highorder) Then
        status = St_bad_rel1
     End If
 
!  Re-check input status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_ROWLINE ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Calculate relative distances and radial distance from beam centre
     xrel = (xrowcen - x_centre) * experiment%x_pixel_size
     yrel = (yrowcen - y_centre) * experiment%y_pixel_size
     radial_distance = Sqrt(((xrowcen - x_centre) * &
       experiment%x_pixel_size)**2 + &
       ((yrowcen - y_centre) * experiment%y_pixel_size)**2)
 
!  Calculate "Up" vector on detector
     x_up = xrel / radial_distance
     y_up = yrel / radial_distance
 
!  Calculate row-line centre radial angle
!**OLD-CODE
!  rowline_latitude = Sign(Atan(radial_distance/distance), yrel)
     rowline_latitude = Atan(radial_distance / experiment%detector_distance)
 
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
        Call F2D_CAL_PROJECTION (experiment%detector_distance, &
          experiment%x_pixel_size, experiment%y_pixel_size, &
          x_centre, y_centre, x_up, y_up, peak_longitude, peak_latitude, &
          xcenpeak, ycenpeak, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''xcenpeak = '', g)') xcenpeak
!     Write (*, '(''ycenpeak = '', g)') ycenpeak
!     Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Add peak
        Call MA_2DPOLARGAU (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, XAXIS, YAXIS, INTENSITIES(order), xcenpeak, ycenpeak, &
          x_centre, y_centre, sigradius, sigtheta, 4.0, method, DATA, status)
 
     End Do
 
!  Create secondary row-line
     Do order = loworder, highorder
 
!     Calculate angle along the row-line
        peak_angle = -(Real(order) * ppangle + ppashift)
        delta_latitude = peak_angle * Sin(rowline_angle)
 
!     Calculate peak angles
        peak_longitude = -peak_angle * Cos(rowline_angle)
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
        Call F2D_CAL_PROJECTION (experiment%detector_distance, &
          experiment%x_pixel_size, experiment%y_pixel_size, &
          x_centre, y_centre, x_up, y_up, peak_longitude, peak_latitude, &
          xcenpeak, ycenpeak, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''xcenpeak = '', g)') xcenpeak
!     Write (*, '(''ycenpeak = '', g)') ycenpeak
!     Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Add peak
        Call MA_2DPOLARGAU (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, XAXIS, YAXIS, INTENSITIES(order)*ratio, xcenpeak, ycenpeak, &
          x_centre, y_centre, sigradius, sigtheta, 4.0, method, DATA, status)
 
     End Do
 
     End Subroutine F2D_ROWLINE
!********1*********2*********3*********4*********5*********6*********7*********8
