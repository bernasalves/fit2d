!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_plot_pres.f90 *
!  *                   *
!  *********************
 
!+ F2D_PLOT_PRES - Fit2D: PLOT 1-D X/Y graph and PRESsure calibrant lines
     Subroutine F2D_PLOT_PRES (mask_data, xmaxdat, ymaxdat, DATA, MASK, &
       X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, &
       x_axis_label, y_axis_label, z_axis_label, experiment, &
       max_calibrants, num_calibrants, CALIBRANT_NAMES, CALIBRANTS, &
       pressure, baseline, status)
!  Description:
!  Keywords:
!  Method:
!  Usage:
!    General
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    03-Nov-2014: V0.5 Add extra figure to pressure text (Hammersley)
!    15-Apr-2014: V0.4 Make "pressure" double precision (Hammersley)
!    11-Apr-2014: V0.3 Add "baseline" control (Hammersley)
!    18-Sep-2013: V0.2 Add "pressure" variable (Hammersley)
!    11-Sep-2013: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use LG_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: mask_data ! .True., if the mask is to be used to
!      mask off pixels
     Integer, Intent(IN) :: xmaxdat ! Maximum number of values to plot in X;
!      first array dimension for "DATA"
     Integer, Intent(IN) :: ymaxdat ! Maximum number of values to plot in Y;
!      second array dimension for "DATA"
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! 2-D grid of pixel values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! .True., if a pixel is
!      masked-off
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis data
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis data
     Integer, Intent(IN) :: xstrelm ! First element to output in X-direction
     Integer, Intent(IN) :: ystrelm ! First element to output in Y-direction
     Integer, Intent(IN) :: xendelm ! Last element to output in X-direction
     Integer, Intent(IN) :: yendelm ! Last element to output in Y-direction
     Character(Len = *), Intent(IN) :: title ! Title for the graph
     Character(Len = *), Intent(IN) :: x_axis_label ! X-axis label for the graph
     Character(Len = *), Intent(IN) :: y_axis_label ! Y-axis label for the graph
     Character(Len = *), Intent(IN) :: z_axis_label ! Z-axis label for the graph
     Character(Len = *), Intent(IN) :: CALIBRANT_NAMES(Max_calibrants) 
! Name of calibrants
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of 
!      experiment (see "io.inc")
     Integer, Intent(IN) :: max_calibrants ! Dimension of "CALIBRANTS"
     Integer, Intent(IN) :: num_calibrants ! Number of defined calibrants
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: CALIBRANTS(max_calibrants) 
       ! Pressure calibrants (see "io.inc")
     Double Precision, Intent(IN) :: pressure ! Pressure on samples in GPa
     Real, Intent(IN) :: baseline ! Baseline for drawing peak positions
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Integer :: marker_colour ! Colour for markers
     Integer :: marker_type ! Type for markers
     Integer :: mask_colour ! Colour for masked elements
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Logical :: logarithmic ! .True., if intensity scaling is logarithmic
     Logical :: x_linear ! .True., if X-axis scaling is to be linear
     Logical :: y_linear ! .True., if Y-axis scaling is to be linear
     Real :: marker_size ! Size scaling factor for markers
     Real :: x_value ! Coordinate to draw masked
     Real :: y_value ! Coordinate to draw masked
!  Local Arrays:
     Real :: WORK(yendelm) ! Work array for 1-D graphs
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_PLOT_PRES'')')
!  Call ST_OUT (status)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status value
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PLOT_PRES ' // Version)
        Return
     End If
 
!  Check import variables
     If (xmaxdat .Lt. 1) Then
        status = St_bad_dim1
     Else If (ymaxdat  .Lt. 1) Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xstrelm .Gt. xendelm .Or. xendelm .Gt. &
       xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. ystrelm .Gt. yendelm .Or. yendelm .Gt. &
       ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
 
!     Add module number to status value
        status = St_mod_gs + status
 
!     Save details of subroutine call
        Call ST_SAVE ('Subroutine F2D_PLOT_PRES ' // Version)
 
     Else If (Lg_inq_open(status)) Then

 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Find out if logarithmic or linear intensity scaling
        Call LG_INQ_LOGZSCALING (logarithmic, status)
 
!     Find out currect axis log/lin scaling
        Call GS_INQ_AXESLOGLIN (x_linear, y_linear, status)
 
!     Set appropriate axes scaling
        Call GS_SET_AXESLOGLIN (x_linear, (.Not. logarithmic), status)
 
!     Simple output of a line as an X/Y graph
        Call GS_XYSGRAPH (xmaxdat, xstrelm, xendelm, X_AXIS, DATA(1, &
          ystrelm), title, x_axis_label, z_axis_label, status)
 
        If (mask_data) Then
 
!        Draw masked data-points
           Call GS_INQ_MASKCOLOUR (mask_colour, status)
           Call GS_INQ_CURVEMARKERS (1, marker_type, marker_colour, &
             marker_size, status)
           Call GS_SET_CURVEMARKERS (1, marker_type, mask_colour, &
             marker_size, status)
 
           Do x = xstrelm, xendelm
 
              If (MASK(x, ystrelm)) Then
 
                 x_value = X_AXIS(x)
 
                 If (logarithmic) Then
                    y_value = Log10(Max(1.0e-30, DATA(x, ystrelm)))
                 Else
                    y_value = DATA(x, ystrelm)
                 End If
 
                 Call GS_MARKERS (1, 1, 1, x_value, y_value, 1, status)
 
              End If
 
           End Do
 
!        Re-set marker colours
           Call GS_SET_CURVEMARKERS (1, marker_type, marker_colour, &
             marker_size, status)
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Draw lines for calibrant peak positions
        Call F2D_DRAW_PRESCAL (experiment, Max_calibrants, num_calibrants, &
          CALIBRANT_NAMES, CALIBRANTS, pressure, baseline, status)

!     Re-set axis log/lin scaling
        Call GS_SET_AXESLOGLIN (x_linear, y_linear, status)
 
     End If
 
     End Subroutine F2D_PLOT_PRES
!********1*********2*********3*********4*********5*********6*********7*********8
