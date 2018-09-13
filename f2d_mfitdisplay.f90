!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_mfitdisplay.f90 *
!  *                     *
!  ***********************
 
!+ F2D_MFITDISPLAY - MFIT DISPLAY of fitting results
     Subroutine F2D_MFITDISPLAY (pixel_size, variances_exist, xmaxdat, &
       ymaxdat, AXIS, DATA, VARIANCES, MASK, MODEL, strelm, endelm, y_row, &
       str_optimised, end_optimised, title, x_label, y_label, max_parameters, &
       num_parameters, num_1dfeatures, PARAM_INFO, PARAMS, poly_order, &
       min_poly, max_poly, max_text, num_text, TEXT, status)
!  Description:
!    Graphical display of results of model fitting
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Mar-2006: V0.6 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    05-Feb-1997: V0.5 Display masked-off coordinates in the masking colour 
!      (Hammersley)
!    15-Jan-1997: V0.4 Only display the part of the model and residuals which 
!      have been optimsed (Hammersley)
!    16-Dec-1996: V0.3 Correct a call to "GS_INQ_CURVESTYLE" to 
!      "GS_SET_CURVESTYLE" which caused a crash on Solaris (Hammersley)
!    13-Dec-1996: V0.2 Add output of error bars (Hammersley)
!    10-Dec-1996: V0.1 Original, based on "GR_UGRAF" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Real, Intent(IN) :: pixel_size ! Size of one bin/pixel (metres)
     Logical, Intent(IN) :: variances_exist ! .True., if a data variance
!      array is created
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Real, Intent(IN) :: AXIS(xmaxdat) ! Axis data values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Array containing data to
!      be fitted
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! Array containing
!      variances in the data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Real, Intent(IN) :: MODEL(xmaxdat) ! Array containing calculated model
     Integer, Intent(IN) :: strelm ! Starting element of region to be fitted
     Integer, Intent(IN) :: endelm ! End element of region to be fitted
     Integer, Intent(IN) :: str_optimised ! First bin in ROI when "OPTIMISE"
!      was used
     Integer, Intent(IN) :: end_optimised ! Last bin in ROI when "OPTIMISE"
!      was used
     Integer, Intent(IN) :: y_row ! Y-row to display
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: x_label ! X-axis label for plot
     Character(Len = *), Intent(IN) :: y_label ! Y-axis label for plot
     Integer, Intent(IN) :: max_parameters ! Dimension size of 'PARAMS', etc.
     Integer, Intent(IN) :: num_parameters ! Total number of variable and
!      constrained parameters in the fit
     Integer, Intent(IN) :: num_1dfeatures ! Number of features in 1-D model
     Integer, Intent(IN) :: PARAM_INFO(max_parameters) ! Information on each
!      of the parameters of the fit. Describing to which feature it belongs,
!      the type of feature and the number of the parameter within the feature, 
!      and whether the parameter is constrained or free to be varied.
!      (See 'F2D_1DFEATURE' for further details.)
     Real, Intent(IN) :: PARAMS(max_parameters) ! The fit model parameters
     Integer, Intent(IN) :: poly_order ! Order of polynomial
     Real, Intent(IN) :: min_poly ! Coordinate of minimum extent of Chebyshev 
!      interval (scaled to -1.0: +1.0)
     Real, Intent(IN) :: max_poly ! Coordinate of maximum extent of Chebyshev 
!      interval (scaled to -1.0: +1.0)
     Integer, Intent(IN) :: max_text ! Dimension size of "TEXT"
     Integer, Intent(IN) :: num_text ! Number of lines of defined results text
     Character(Len = *), Intent(IN) :: TEXT(max_text) ! Output text
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
!  Local Variables:
     Integer stat ! Status return variable for "Allocate"
!  Local Arrays:
     Real, Allocatable :: WORK(:) ! Dynamic work space
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MFITDISPLAY ' // Version)
        Return
     End If
 
!  Check that the input arguments are reasonable
     If (Min(xmaxdat, max_parameters) .Le. 0) Then
        status = St_bad_dim1
     Else If (strelm .Le. 0 .Or. strelm .Gt. endelm .Or. endelm .Gt. xmaxdat) &
       Then
        status = St_bad_adr1
     Else If (num_parameters .Gt. max_parameters) Then
        status = St_bad_adr1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_MFITDISPLAY ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Allocate work space
     Allocate (WORK(endelm), Stat = stat)
     If (stat .Ne. 0) Then
        status = St_mod_fit2d + St_bad_malloc
        Call ST_SAVE ('Subroutine F2D_MFITDISPLAY ' // Version)
        Return
     End If

     Call F2D_SUBMFITDISPLAY (pixel_size, variances_exist, xmaxdat, AXIS, &
       DATA(1, y_row), VARIANCES(1, y_row), MASK(1, y_row), MODEL, strelm, &
       endelm, str_optimised, end_optimised, title, x_label, y_label, &
       max_parameters, num_parameters, num_1dfeatures, PARAM_INFO, PARAMS, &
       poly_order, min_poly, max_poly, max_text, num_text, TEXT, WORK, &
       status)
 
!  Free work space
     Deallocate (WORK)
 
     End Subroutine F2D_MFITDISPLAY
!********1*********2*********3*********4*********5*********6*********7*********8
!********1*********2*********3*********4*********5*********6*********7*********8
 
!+ F2D_SUBMFITDISPLAY - MFIT DISPLAY of fitting results
     Subroutine F2D_SUBMFITDISPLAY (pixel_size, variances_exist, maxdat, AXIS, &
       DATA, VARIANCES, MASK, MODEL, strelm, endelm, str_optimised, &
       end_optimised, title, x_label, y_label, max_parameters, num_parameters, &
       num_1dfeatures, PARAM_INFO, PARAMS, poly_order, min_poly, max_poly, &
       max_text, num_text, TEXT, WORK, status)
!  Description:
!    Graphical display of results of model fitting
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Mar-2006: V0.6 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    17-Mar-2006: V0.5 Support for arbitrary aspect ratio windows (Hammersley)
!    05-Feb-1997: V0.4 Display masked-off coordinates in the masking colour 
!      (Hammersley)
!    15-Jan-1997: V0.3 Only display the part of the model and residuals which 
!      have been optimsed (Hammersley)
!    13-Dec-1996: V0.2 Add output of error bars (Hammersley)
!    10-Dec-1996: V0.1 Original, based on "GR_UGRAF" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Real, Intent(IN) :: pixel_size ! Size of one bin/pixel (metres)
     Logical, Intent(IN) :: variances_exist ! .True., if a data variance
!      array is created
     Integer, Intent(IN) :: maxdat ! First dimension of data arrays
     Real, Intent(IN) :: AXIS(maxdat) ! Axis data values
     Real, Intent(IN) :: DATA(maxdat) ! Array containing data to be fitted
     Real, Intent(IN) :: VARIANCES(maxdat) ! Array containing variances in
!      the data values
     Logical*1, Intent(IN) :: MASK(maxdat) ! Data mask, .True., indicates
!      masked data point (i.e. point not considered in fitting)
     Real, Intent(IN) :: MODEL(maxdat) ! Array containing calculated model
     Integer, Intent(IN) :: strelm ! Starting element of region to be fitted
     Integer, Intent(IN) :: endelm ! End element of region to be fitted
     Integer, Intent(IN) :: str_optimised ! First bin in ROI when "OPTIMISE"
!      was used
     Integer, Intent(IN) :: end_optimised ! Last bin in ROI when "OPTIMISE"
!      was used
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: x_label ! X-axis label for plot
     Character(Len = *), Intent(IN) :: y_label ! Y-axis label for plot
     Integer, Intent(IN) :: max_parameters ! Dimension size of 'PARAMS', etc.
     Integer, Intent(IN) :: num_parameters ! Total number of variable and
!      constrained parameters in the fit
     Integer, Intent(IN) :: num_1dfeatures ! Number of features in 1-D model
     Integer, Intent(IN) :: PARAM_INFO(max_parameters) ! Information on each
!      of the parameters of the fit. Describing to which feature it belongs,
!      the type of feature and the number of the parameter within the
!      feature, and whether the parameter is constrained or free to
!      be varied.
!      (See 'F2D_1DFEATURE' for further details.)
     Real, Intent(IN) :: PARAMS(max_parameters) ! The fit model parameters
     Integer, Intent(IN) :: poly_order ! Order of polynomial
     Real, Intent(IN) :: min_poly ! Coordinate of minimum extent of Chebyshev 
!      interval (scaled to -1.0: +1.0)
     Real, Intent(IN) :: max_poly ! Coordinate of maximum extent of Chebyshev 
!      interval (scaled to -1.0: +1.0)
     Integer, Intent(IN) :: max_text ! Dimension size of "TEXT"
     Integer, Intent(IN) :: num_text ! Number of lines of defined results text
     Character(Len = *), Intent(IN) :: TEXT(max_text) ! Output text
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: WORK(endelm) ! Data minus the model
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
!  Local Variables:
     Integer :: back_colour ! Background drawing colour
     Integer :: dummy ! Dummy variable
     Integer :: elem ! Loop variable for elements
     Integer :: feature ! Loop variable for features
     Integer :: line_colour ! Colour index for vertical axes
     Integer :: line_type ! Line type for vertical axes
     Integer :: marker_colour ! Colour for markers
     Integer :: marker_type ! Type for markers
     Integer :: mask_colour ! Colour for masked elements
     Integer :: number_smallticks ! Number of small tick marks between two 
!      large tick marks
     Integer :: retstat ! Subroutine return status:
!      0 = Good status
!      1 = Attempt to take square root of negative number for one
!          or more array elements
!      2 = Attempt to divide by zero for one or more numbers
     Integer stat ! Status return variable for "Allocate"
     Integer :: window_format ! Format of workstation window
     Integer :: x ! Loop variable for X-direction
     Logical :: draw_errorboxes ! .True., if error-boxes are to be
!      drawn at each data-point (only if errors are defined)
     Logical :: draw_line ! .True., if a line is to be drawn through
!      the data-points
     Logical :: draw_markers ! .True., if markers are to be drawn at
!      each data-point
     Logical :: fill_background ! .True., if background is to be filled
     Logical :: x_errors ! .True., means draw X-direction error bars
     Logical :: x_linear ! .True., if X-axis scaling is to be linear
     Logical :: xmaxautoddr ! .True., if the maximum X-coordinate for
!      the data display region is to be calculated automatically
     Logical :: xminautoddr ! .True., if the minimum X-coordinate for
!      the data display region is to be calculated automatically
     Logical :: y_errors ! .True., means draw Y-direction error bars
     Logical :: y_linear ! .True., if Y-axis scaling is to be linear
     Logical :: ymaxautoddr ! .True., if the maximum Y-coordinate for
!      the data display region is to be calculated automatically
     Logical :: yminautoddr ! .True., if the minimum Y-coordinate for
!      the data display region is to be calculated automatically
     Real :: extra ! Extra X-range used in automatic scaling to
!      account for marker size etc.
     Real :: height ! Height of window in page coordinates 
     Real :: largeticks_size ! Size of large tick marks for the vertical axis
     Real :: line_width ! Scale factor for line width for vertical axes
     Real :: marker_size ! Size scaling factor for markers
     Real :: smallticks_size ! Size of small tick marks for the vertical axis
     Real :: width ! Width of window in page coordinates 
     Real :: x_value ! Coordinate to draw masked
     Real :: xmax_dddr ! The maximum X-coordinate of the displayed data
!      display region
     Real :: xmax_gpp ! The maximum X-coordinate for the graph page position
     Real :: xmin_dddr ! The minimum X-coordinate of the displayed data
!      display region
     Real :: xmin_gpp ! The minimum X-coordinate for the graph page position
     Real :: y_value ! Coordinate to draw masked
     Real :: ymax_dddr ! The maximum Y-coordinate of the displayed data
!      display region
     Real :: ymax_gpp ! The maximum Y-coordinate for the graph page position
     Real :: ymin_dddr ! The minimum Y-coordinate of the displayed data
!      display region
     Real :: ymin_gpp ! The minimum Y-coordinate for the graph page position
!  Local Arrays:
     Real, Allocatable :: ERRORS(:) ! Dynamic work space for calculating
!      error bars
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_SUBMFITDISPLAY'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SUBMFITDISPLAY ' // Version)
        Return
     End If
 
!  Check that the input arguments are reasonable
     If (Min(maxdat, max_parameters) .Le. 0) Then
        status = St_bad_dim1
     Else If (strelm .Le. 0 .Or. strelm .Gt. endelm .Or. endelm .Gt. maxdat) &
       Then
        status = St_bad_adr1
     Else If (num_parameters .Gt. max_parameters) Then
        status = St_bad_adr1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_SUBMFITDISPLAY ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Find out current graph page position
     Call GS_INQ_GPP (xmin_gpp, ymin_gpp, xmax_gpp, ymax_gpp, status)
 
!  Find out current style for line drawing for first curve
     Call GS_INQ_CURVESTYLE (1, draw_line, draw_markers, draw_errorboxes, &
       status)
 
!  Set markers and error boxes for output
     Call GS_SET_CURVESTYLE (1, .False., .True., .True., status)
 
!  Find out currect axis log/lin scaling
     Call GS_INQ_AXESLOGLIN (x_linear, y_linear, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''After GS_INQ_CURVESTYLE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Calculate the residual values
     Do elem = Max(strelm, str_optimised), Min(endelm, end_optimised)
        WORK(elem) = DATA(elem) - MODEL(elem)
     End Do
 
!  Set graph page position for residuals
     Call GS_SET_GPP (xmin_gpp, ymin_gpp, xmax_gpp, ymin_gpp + 0.2 * (ymax_gpp &
       - ymin_gpp), status)
 
!  Save Vertical axis style
     Call GS_INQ_VAXESSTYLE (line_type, line_width, line_colour, &
       largeticks_size, smallticks_size, number_smallticks, status)
 
!  Set no small tick marks on vertical axis
     Call GS_SET_VAXESSTYLE (line_type, line_width, line_colour, &
       largeticks_size, smallticks_size, 0, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''After GS_SET_VAXESSTYLE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Save automatic display display region reqirements
     Call GS_INQ_AUTODDR (xminautoddr, yminautoddr, xmaxautoddr, ymaxautoddr, &
       status)
     Call GS_INQ_DDR (xmin_dddr, ymin_dddr, xmax_dddr, ymax_dddr, status)
 
!  Set fixed X-scale, and fully automatic Y-scaling
     Call GS_SET_AUTODDR (.False., .True., .False., .True., status)
 
!  Calculate extra X-range of values to allow for marker sizes etc.
     extra = (AXIS(endelm) - AXIS(strelm)) * 0.05
 
     If (xminautoddr .And. xmaxautoddr) Then
 
        Call GS_SET_DDR (AXIS(strelm) - extra, ymin_dddr, AXIS(endelm) + &
          extra, ymax_dddr, status)
 
     Else If (xminautoddr) Then
 
        Call GS_SET_DDR (AXIS(strelm) - extra, ymin_dddr, xmax_dddr, &
          ymax_dddr, status)
 
     Else If (xmaxautoddr) Then
 
        Call GS_SET_DDR (xmin_dddr, ymin_dddr, AXIS(endelm) + extra, &
          ymax_dddr, status)
 
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''After GS_SET_AUTODDR'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Output residuals
     If (variances_exist) Then
 
!     Allocate memory for calculating error estimates
        Allocate (ERRORS(endelm), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SUBMFITDISPLAY ' // Version)
           Return
        End If

!     Copy variances to work array
        Call MA_RCOPY (maxdat, 1, VARIANCES, strelm, 1, endelm, 1, endelm, 1, &
          ERRORS, status)
 
!     Take square root of values
        Call MA_POWER (maxdat, 1, strelm, 1, endelm, 1, 0.5, ERRORS, &
          retstat, status)
 
        If (retstat .Ne. 0) Then
           Call IO_WRITE ('WARNING: Problem with variance values', status)
           Return
        End If
 
!     Save existing error bar requirements
        Call GS_INQ_ERRORBARS (x_errors, y_errors, status)
 
!     Turn off X-direction error bars
        Call GS_SET_ERRORBARS (.False., y_errors, status)
 
!     Draw residuals with error bars
        Call GS_XYEGRAPH (maxdat, 1, 1, Max(strelm, str_optimised), &
          Min(endelm, end_optimised), AXIS, WORK, dummy, dummy, ERRORS, &
          ERRORS, ' ', x_label, 'Residuals', status)
 
     Else
 
!     Simple graphical output of residuals
        Call GS_XYSGRAPH (end_optimised, Max(strelm, str_optimised), &
          Min(endelm, end_optimised), AXIS, WORK, ' ', x_label, 'Residuals ', &
          status)
 
     End If
 
!  Draw masked residual values
     Call GS_INQ_MASKCOLOUR (mask_colour, status)
     Call GS_INQ_CURVEMARKERS (1, marker_type, marker_colour, marker_size, &
       status)
     Call GS_SET_CURVEMARKERS (1, marker_type, mask_colour, marker_size, &
       status)
     Do x = Max(strelm, str_optimised), Min(endelm, end_optimised)
 
        If (MASK(x)) Then
 
           x_value = AXIS(x)
 
           If (y_linear) Then
              y_value = WORK(x)
           Else
              y_value = Log10(Max(1.0e-30, DATA(x)))
           End If
 
           Call GS_MARKERS (1, 1, 1, x_value, y_value, 1, status)
 
        End If
 
     End Do
 
!  Re-set marker colours
     Call GS_SET_CURVEMARKERS (1, marker_type, marker_colour, marker_size, &
       status)
 
!  Re-set Vertical axis style
     Call GS_SET_VAXESSTYLE (line_type, line_width, line_colour, &
       largeticks_size, smallticks_size, number_smallticks, status)
 
!  Re-set previous automatic scaling
     Call GS_SET_AUTODDR (xminautoddr, yminautoddr, xmaxautoddr, ymaxautoddr, &
       status)
     Call GS_SET_DDR (xmin_dddr, ymin_dddr, xmax_dddr, ymax_dddr, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''After drawing residuals'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Set graph page position for data and model
     Call GS_SET_GPP (xmin_gpp, ymin_gpp + 0.3 * (ymax_gpp - ymin_gpp), &
       xmax_gpp, ymax_gpp, status)
 
!  Save current background style
     Call GS_INQ_BACKGROUND (fill_background, back_colour, status)
 
!  Set no background drawing
     Call GS_SET_BACKGROUND (.False., back_colour, status)
 
!  Output data
     If (variances_exist) Then
 
!     Draw data with error bars X/Y graph
        Call GS_XYEGRAPH (maxdat, 1, 1, strelm, endelm, AXIS, DATA, dummy, &
          dummy, ERRORS, ERRORS, title, ' ', y_label, status)
 
!     Re-set error bars requirements
        Call GS_SET_ERRORBARS (x_errors, y_errors, status)
 
!     Free array space
        Deallocate (ERRORS)
 
     Else
 
!     Draw simple X/Y graph
        Call GS_XYSGRAPH (maxdat, strelm, endelm, AXIS, DATA, title, ' ', &
          y_label, status)
 
     End If
 
!  Draw masked data-points
     Call GS_INQ_CURVEMARKERS (1, marker_type, marker_colour, marker_size, &
       status)
     Call GS_SET_CURVEMARKERS (1, marker_type, mask_colour, marker_size, &
       status)
     Do x = strelm, endelm
 
        If (MASK(x)) Then
 
           x_value = AXIS(x)
 
           If (y_linear) Then
              y_value = DATA(x)
           Else
              y_value = Log10(Max(1.0e-30, DATA(x)))
           End If
 
           Call GS_MARKERS (1, 1, 1, x_value, y_value, 1, status)
 
        End If
 
     End Do
 
!  Re-set marker colours
     Call GS_SET_CURVEMARKERS (1, marker_type, marker_colour, marker_size, &
       status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''After drawing data'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Find out displayed data region
     Call GS_INQ_DDDR (xmin_dddr, ymin_dddr, xmax_dddr, ymax_dddr, status)
 
!  Turn on clipping
     Call LG_CLIPWINDOW (xmin_dddr, ymin_dddr, xmax_dddr, ymax_dddr, status)
     Call LG_CLIP (.True., status)
 
!  Set line style dotted, blue, and standard width
     Call GS_LINESTYLE (Lg_dots, 1.0, Gs_blue, status)
 
!  Create features and draw on top of graph
     Do feature = 1, num_1dfeatures
 
!     Initialise work array
        Call MA_RVALUE (maxdat, 1, strelm, 1, endelm, 1, 0.0, WORK, status)
 
!     Calculate feature
        Call F2D_1DFEATURE (pixel_size, feature, max_parameters, PARAMS, &
          PARAM_INFO, num_parameters, maxdat, Max(strelm, str_optimised), &
          Min(endelm, end_optimised), poly_order, min_poly, max_poly, AXIS, &
          WORK, status)
 
!     Draw model on top of the data
        Call LG_POLYLINE (Min(endelm, end_optimised) - Max(strelm, &
          str_optimised) + 1, AXIS(Max(strelm, str_optimised)), &
          WORK(Max(strelm, str_optimised)), status)
 
     End Do
 
!  Draw model on top of the data
     Call GS_LINESTYLE (Lg_solid, 1.0, Gs_blue, status)
     Call LG_POLYLINE (Min(endelm, end_optimised) - Max(strelm, str_optimised) &
       + 1, AXIS(Max(strelm, str_optimised)), MODEL(Max(strelm, &
       str_optimised)), status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''After drawing features'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Turn off clipping
     Call LG_CLIP (.False., status)
 
!  Re-set previous background drawing style
     Call GS_SET_BACKGROUND (fill_background, back_colour, status)
 
!  Re-set previous graph page position
     Call GS_SET_GPP (xmin_gpp, ymin_gpp, xmax_gpp, ymax_gpp, status)
 
!  Set displayed graph page position
     Call GS_SET_DGPP (xmin_gpp, ymin_gpp + 0.3 * (ymax_gpp - ymin_gpp), &
       xmax_gpp, ymax_gpp, status)
 
!  Re-set previous style for line drawing for first curve
     Call GS_SET_CURVESTYLE (1, draw_line, draw_markers, draw_errorboxes, &
       status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Find out window format
     Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!  Set unity transform
     Call LG_UNITYTRANSFORM (.True., status)
 
!  Add text describing the fitted parameters
     If (num_text .Le. 12 .And. ymin_gpp .Ge. 0.2) Then
 
!     Draw text below graph
        Call GS_TEXT (max_text, num_text, TEXT, xmin_gpp, 0.0, xmax_gpp, &
          ymin_gpp * 0.8, status)
 
     Else If (num_text .Le. 24 .And. ymin_gpp .Ge. 0.2) Then
 
!     Draw text below graph in two columns
        Call GS_TEXT (max_text, (num_text + 1) / 2, TEXT, 0.05, 0.0, 0.3335, &
          ymin_gpp * 0.8, status)
        Call GS_TEXT (max_text, num_text - (num_text + 1) / 2, TEXT((num_text &
          + 1) / 2), 0.3735, 0.0, 0.6571, ymin_gpp * 0.8, status)
 
     Else If (width / height .Gt. 1.2 .And. xmax_gpp .Le. 0.8 .And. &
       num_text .Le. 40) Then
 
!     Draw text to the right of the graph
        Call GS_TEXT (max_text, num_text, TEXT, xmax_gpp * 1.05, 0.0, 1.0, &
          0.7071, status)
 
     End If
 
!  Re-set previous transform
     Call LG_UNITYTRANSFORM (.False., status)

     End Subroutine F2D_SUBMFITDISPLAY
!********1*********2*********3*********4*********5*********6*********7*********8
