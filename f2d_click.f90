!*********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************
!  *               *
!  * f2d_click.f90 *
!  *               *
!  *****************
 
!+ F2D_CLICK - FIT 2-D user CLICK on a pixel position in an
!    image, or within a displayed look-up table
     Subroutine F2D_CLICK (data_type, xmaxdat, ymaxdat, &
       xstrelm, ystrelm, xendelm, yendelm, X_AXIS, Y_AXIS, DATA, &
       title, xlabel, ylabel, zlabel, x_coordinate, y_coordinate, experiment, &
       update_image, update_menu, status)
!  Description:
!    Simple display of X,Y and intensity, when a user clicks on an
!    image pixel. Also possibility to set Z-scaling
!  Keywords:
!    Pixel.Information
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    27-Nov-2014: V0.13 Investigating error in D-spacing for integrated powder
!      diffraction data (Hammersley)
!    26-Nov-2014: V0.12 Investigating error in D-spacing for ID06LVP (Hammersley)
!    13-Mar-2006: V0.11 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Sep-1998: V0.10 Output equivalent D-spacing for 1-D 2-theta
!      scans if the geometry has been set (Hammersley)
!    22-Apr-1998: V0.9 Increase size of whited out rectangle (Hammersley)
!    10-Jan-1998: V0.8 Allow clicking within look-up table to set
!      scaling range. Output proper title and axis labels (Hammersley)
!    09-Jan-1998: V0.7 Possibility to set look-up table scaling (Hammersley)
!    10-Sep-1996: V0.6 Adapt for 1-D X/Y graph display (Hammersley)
!    30-Jan-1996: V0.5 Use DDDR instead of DDR (Hammersley)
!    22-Jan-1996: V0.4 Use "GS_INQ_DGPP" to get displayed page
!      coordinates (Hammersley)
!    21-Jan-1996: V0.3 Take into account landscape orientation (Hammersley)
!    10-Nov-1995: V0.2 Output d-spacing if the geometry has been
!      defined (Hammersley)
!    06-Sep-1995: V0.1 Original, (Hammersley)
!  Modules:
     Use IO_LIB
     Use LG_LIB
     Use GS_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics System constants
!  Import:
     Integer, Intent(IN) :: data_type ! Type of data:
!      0 = Unknown: No D-spacings!
!      1 = Raw X/Y image, with beam centre
!      2 = Horizontal 2-theta scale
!      3 = 2-D polar image
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: xendelm ! The X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! The Y-pixel number for the end of the ROI
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Real, Intent(IN) :: x_coordinate ! Graphical input X-coordinate
     Real, Intent(IN) :: y_coordinate ! Graphical input Y-coordinate
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
!  Export:
     Logical, Intent(OUT) :: update_image ! .True., if the image needs to be
!      redrawn
     Logical, Intent(OUT) :: update_menu ! .True., if the menu needs to be
!      redrawn
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.13' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User message
     Integer :: dummy ! Dummy variable, unused
     Integer :: x ! Array index
     Integer :: y ! Array index
     Logical :: lut_drawn ! .True., if a look-up table has been drawn
     Logical :: x_linear ! .True., if the X-axis scale is to be linear
     Logical :: y_linear ! .True., if the Y-axis scale is to be linear
     Real :: angle ! Diffraction angle
     Real :: d_spacing ! Bragg d spacing assuming n=1
     Real :: radial_distance ! Distance radially from beam centre
     Real :: x_pc ! X-page coordinate
     Real :: x_pixel ! X-pixel coordinate
     Real :: x_value ! Value of X coordinate
     Real :: xmax_dddr ! Maximum X-limit of the data display region
     Real :: xmax_gpp ! The maximum X-coordinate for the graph page position
     Real :: xmax_lut ! The maximum X-coordinate for displayed look-up table 
!      position
     Real :: xmax_message, xmin_message, ymax_message, ymin_message
     Real :: xmin_dddr ! Minimum X-limit of the data display region
     Real :: xmin_gpp ! The minimum X-coordinate for the graph page position
     Real :: xmin_lut ! The minimum X-coordinate for displayed look-up table 
!      position
     Real :: y_pc ! Y-page coordinate
     Real :: y_pixel ! Y-pixel coordinate
     Real :: ymax_dddr ! Maximum Y-limit of the data display region
     Real :: ymax_gpp ! The maximum Y-coordinate for the graph page position
     Real :: ymax_lut ! The maximum Y-coordinate for displayed look-up table 
!      position
     Real :: ymin_dddr ! Minimum Y-limit of the data display region
     Real :: ymin_gpp ! The minimum Y-coordinate for the graph page position
     Real :: ymin_lut ! The minimum Y-coordinate for displayed look-up table 
!      position
     Real :: y_value ! Value of Y coordinate
!  Local Arrays:
!  External Functions:
!     Character(Len = 20), External :: Io_itoc ! Convert integer to character
!      string
!     Character(Len = 20), External :: Io_rtoc ! Convert real to character
!      string
!     Real, External :: Lg_wc2pc ! Convert world coordinates to page coordinates
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CLICK ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm .Gt. xmaxdat .Or. xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Gt. ymaxdat .Or. yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_CLICK ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
        update_menu = .False.
        update_image = .False.
 
!     Inquire current message region
        Call GS_INQ_MESSAGE (xmin_message, ymin_message, xmax_message, &
          ymax_message, status)
 
!     Get current data display region
        Call GS_INQ_DDDR (xmin_dddr, ymin_dddr, xmax_dddr, ymax_dddr, status)
        Call GS_INQ_DGPP (xmin_gpp, ymin_gpp, xmax_gpp, ymax_gpp, status)
 
!     Get current look-up table displayed position
        Call GS_INQ_DLUTPC (lut_drawn, xmin_lut, ymin_lut, xmax_lut, ymax_lut, &
          status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x_coordinate, y_coordinate = '', 2g12.5)')
!     :       x_coordinate, y_coordinate
!     Write (*, '(''xmin_lut, xmax_lut, ymin_lut, ymax_lut = '',
!     :       4g12.5)') xmin_lut, xmax_lut, ymin_lut, ymax_lut
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Convert coordinates to page coordinates
        x_pc = Lg_wc2pc (.True., x_coordinate, status)
        y_pc = Lg_wc2pc (.False., y_coordinate, status)
 
        If (lut_drawn .And. x_pc .Ge. xmin_lut .And. x_pc .Le. xmax_lut .And. &
          y_pc .Ge. ymin_lut .And. y_pc .Le. ymax_lut) Then
 
!        Allow redefinition of intensity scaling
           Call F2D_GUI_ZSCALE (.False., xmaxdat, ymaxdat, xendelm, yendelm, &
             X_AXIS, Y_AXIS, DATA, dummy, title, xlabel, ylabel, zlabel, &
             xstrelm, ystrelm, xendelm, yendelm, experiment, &
             .True., x_pc, y_pc, status)
 
           update_menu = .True.
 
        Else If (x_coordinate .Ge. xmin_dddr .And. x_coordinate .Le. xmax_dddr &
          .And. y_coordinate .Ge. ymin_dddr .And. y_coordinate .Le. ymax_dddr) &
          Then
 
!        Set unit transform
           Call LG_DATAWINDOW (0.0, 0.0, 1.0, 1.0, status)
           Call LG_VIEWPORT (0.0, 0.0, 1.0, 1.0, status)
 
           If (xendelm .Gt. xstrelm .And. yendelm .Gt. ystrelm) Then
 
!           Convert data coordinates to pixel coordinates
              Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, x_coordinate, &
                x_pixel, status)
              Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, y_coordinate, &
                y_pixel, status)
 
              x = Int(x_pixel) + 1
              y = Int(y_pixel) + 1
 
!           Output information in top left corner
              Call GS_FILLSTYLE (Lg_solid, Gs_white, status)
 
              If (experiment%beam_centre_set .And. &
                experiment%detector_distance_set) Then
                 Call GS_RECTANGLE (0.0, ymax_message - 0.09, 0.16, &
                   ymax_message, .False., .True., status)
              Else
                 Call GS_RECTANGLE (0.0, ymax_message - 0.07, 0.16, &
                   ymax_message, .False., .True., status)
              End If
 
              Call GS_TEXTSTYLE ( Gs_font_publication, Gs_blue, 0.018, 1.0, &
                0.0, 0, 0, 0, 0.0, 1.0, status)
              message = ' X = ' // Io_itoc(x)
              Call LG_TEXT (0.0, ymax_message - 0.02, message, status)
              message = ' Y = ' // Io_itoc(y)
              Call LG_TEXT (0.0, ymax_message - 0.04, message, status)
              message = '  I = ' // Io_rtoc(DATA(x, y))
              Call LG_TEXT (0.0, ymax_message - 0.06, message, status)
 
              If (experiment%beam_centre_set .And. &
                experiment%detector_distance_set) Then
 
!              Calculate d-spacing
                 If (data_type .Eq. 1) Then
                    radial_distance = Sqrt(((x_coordinate - experiment%x_beam) *&
                      experiment%x_pixel_size)**2 + &
                      ((y_coordinate - experiment%y_beam) * &
                      experiment%y_pixel_size)**2)
                    angle = 0.5 * Atan(radial_distance / &
                      experiment%detector_distance)
                 Else If (data_type .Eq. 2) Then
                    angle = X_AXIS(x) / 2.0 * Pi / 180.0
                 End If

!           d_spacing = wavelength / (2.0 * Sin(two_theta / 2.0))

                 If (data_type .Eq. 1 .Or. data_type .Eq. 2) Then
                    d_spacing = experiment%wavelength / (2.0 * Sin(angle))
                    message = '  d = ' // Io_rtoc(d_spacing * 1.0e10)
                    Call LG_TEXT (0.0, ymax_message - 0.08, message, status)
                 End If

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''data_type = '', i6)') data_type 
!                 Write (*, '(''x = '', i6)') x 
!                 Write (*, '(''2-theta = '', f10.2)') X_AXIS(x) 
!                 Write (*, '(''wavelength = '', 1pe12.5)') experiment%wavelength
!                 Write (*, '(''d-spacing (A) = '', 1pe12.5)') d_spacing * 1.0e10
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

              End If
 
           Else
 
!           Inquire X/Y graph log/linear requirements
              Call GS_INQ_DATALOGLIN (x_linear, y_linear, status)
              If (.Not. x_linear) Then
                 x_value = 10**x_coordinate
              Else
                 x_value = x_coordinate
              End If
              If (.Not. y_linear) Then
                 y_value = 10**y_coordinate
              Else
                 y_value = y_coordinate
              End If
 
!           Output information in top left corner
              Call GS_FILLSTYLE (Lg_solid, Gs_white, status)
 
              Call GS_RECTANGLE (0.0, ymax_message - 0.07, 0.14, ymax_message, &
                .False., .True., status)
 
              Call GS_TEXTSTYLE (Gs_font_publication, Gs_blue, 0.018, 1.0, &
                0.0, 0, 0, 0, 0.0, 1.0, status)
 
              message = ' X = ' // Io_rtoc(x_value)
              Call LG_TEXT (0.0, ymax_message - 0.02, message, status)
              message = ' Y = ' // Io_rtoc(y_value)
              Call LG_TEXT (0.0, ymax_message - 0.04, message, status)
 
              If (experiment%wavelength_set .And. xlabel .Eq. &
                '2-Theta Angle (Degrees)') Then
 
                 d_spacing = experiment%wavelength / &
                   (2.0 * Sin(0.5 * x_value * Pi / 180.0))
                 message = '  d = ' // Io_rtoc(d_spacing * 1.0e10)
                 Call LG_TEXT (0.0, ymax_message - 0.06, message, status)
 
              End If
 
           End If
 
!        Update workstation
           Call GS_UPDATE (status)
 
!        Reset current data display region
           Call LG_DATAWINDOW (xmin_dddr, ymin_dddr, xmax_dddr, ymax_dddr, &
             status)
           Call LG_VIEWPORT (xmin_gpp, ymin_gpp, xmax_gpp, ymax_gpp, status)
 
        End If
 
     End If
 
     End Subroutine F2D_CLICK
!*********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 
 
