!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_inp_datastore.f90 *
!  *                       *
!  *************************
 
!+ F2D_INP_DATASTORE - FIT2D: INput defaults for internal DATA STORE
     Subroutine F2D_INP_DATASTORE (output_graphics, experiment, input_options, &
       status)
!  Description:
!    Fills internal data store from values input from "~/.fit2d.def"
!  Keywords:
!    Data~Base
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    14-Sep-2006: V0.13 Add "monitor" and "attenuation" items (Hammersley)
!    22-Mar-2006: V0.12 Use "LUT_STYLE" structure (Hammersley)
!    20-Mar-2006: V0.11 Add "INPUT OPTIONS" support (Hammersley)
!    13-Mar-2006: V0.10 Use "EXPERIMENT" structure (Hammersley)
!    10-Mar-2006: V0.9 Remove pixel size arguments (Hammersley)
!    23-Feb-1999: V0.8 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.7 Change to use IO internal database routines (Hammersley)
!    25-Nov-1998: V0.6 Make sure that all the colour tables can be set on 
!      start-up (Hammersley)
!    27-Oct-1998: V0.5 Set values of pixel sizes if available (Hammersley)
!    13-Jan-1998: V0.4 Set Z-scaling mode, if keywords are found (Hammersley)
!    26-Feb-1997: V0.3 Take account of graphics system not being enabled 
!     (Hammersley)
!    11-Apr-1996: V0.2 Set default colour map and curve style (Hammersley)
!    29-Feb-1996: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use LG_LIB
     Use GS_LIB
!  Type Definitions:
     Implicit None
     Include 'gs.inc'
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: output_graphics ! .True., if graphics are to be
!      output
!  Import/Export:
!  Export:
     Type(EXPERIMENTAL_DETAILS), Intent(OUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Type(INPUT_OPTIONS_STRUCTURE), Intent(OUT) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.13' ! Version number
!  Local Variables:
     Character(Len = 256) :: value ! Data name values
     Integer :: iostat ! I/O return status
     Integer :: len_string ! Defined length of a string
     Integer :: num_levels ! Number of levels to set in the colour table
     Integer :: retstat ! Return status variable:
!      0 = Good status
!      1 = No more room (for storage), or no more values
!      2 = Key not found for retrieval, or element doesn't exist
     Integer :: scale_mode ! Mode of scaling (thresholding) used to
!      display image:
!        0 = Automatic full scaling
!        1 = Linear scaling within set limits (thresholds)
!        2 = Linear scaling with set minimum (threshold), but automatic maximum
!        3 = Linear scaling with set maximum (threshold), but automatic minimum
!        4 = "Diffraction peak" automatic scaling, designed to display weak 
!            diffraction peaks
     Integer :: table ! Loop variable for colour table choices
     Logical :: draw_line ! .True., if a line is to be drawn through
!      the data-points
     Logical :: draw_markers ! .True., if markers are to be drawn at
!      each data-point
     Logical :: draw_errorboxes ! .True., if error-boxes are to be
!      drawn at each data-point (only if errors are defined)
     Logical :: logarithmic ! .True., if the Z-scaling is logarithmic
     Logical :: set_zscaling ! .True., if the Z-scaling mode is to be set
     Logical :: x_linear ! .True., if the X-axis scale is to be logarithmic
     Logical :: xmaxautoddr ! .True., if the X-maximum of the data
!      display region is to be automatically controlled
     Logical :: xminautoddr ! .True., if the X-minimum of the data
!      display region is to be automatically controlled
     Logical :: y_linear ! .True., if the Y-axis scale is to be logarithmic
     Logical :: ymaxautoddr ! .True., if the Y-maximum of the data
!      display region is to be automatically controlled
     Logical :: yminautoddr ! .True., if the Y-minimum of the data
!      display region is to be automatically controlled
     Real :: max_image ! Upper range of Z-scaling
     Real :: min_image ! Lower range of Z-scaling
     Real :: xmaxddr ! The maximum X-coordinate of the data display region
     Real :: xminddr ! The minimum X-coordinate of the data display region
     Real :: ymaxddr ! The maximum Y-coordinate of the data display region
     Real :: yminddr ! The minimum Y-coordinate of the data display region
!  Local Arrays:
     Character(Len = 20) :: COLOUR_TABLE(11) ! Choice of colour tables
!  Local Data Structures:
     Type(LUT_STYLE) :: STYLE_LUT ! LUT Style (see "gs.inc")
!  External Functions:
!  Local Data:
     Data COLOUR_TABLE / 'COLOUR WHEEL', 'GEOGRAPHICAL', 'GREY-SCALE', &
       'INVERSE GREY-SCALE', 'ORIGINAL', 'PSYCHOLOGIAL', 'REPEATING', &
       'TEMPERATURE', 'TRIAL', 'UPSIDE DOWN', 'BLACK-PURPLE-WHITE' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_DATASTORE ' // Version)
 
     Else
 
!     Default Z-scaling values
        set_zscaling = .False.
        min_image = 0.0
        max_image = 1.0
        logarithmic = .False.
 
!     Is a default colour table defined ?
        Call IO_INQ_KEYVALUE ('COLOUR_TABLE', len_string, value, retstat, &
          status)
 
        If (retstat .Eq. 0 .And. output_graphics) Then
 
!        Find current values for the false colour table
           Call GS_INQ_LUTSTYLE (STYLE_LUT, status)
 
!        Set appropriate colour table choice
           Do table = 1, 11
 
              If (value .Eq. COLOUR_TABLE(table)) Then
 
!              Set the false colour table
                 STYLE_LUT%colour_table = table
                 Call GS_SET_LUTSTYLE (STYLE_LUT, status)
 
!              Change workstation colour table
                 Call GS_COLOURS (status)
 
              End If
 
           End Do
 
        End If
 
!     Default intensity scaling ?
        Call IO_INQ_IKEYVALUE ('Z_SCALING_MODE', scale_mode, retstat, status)
 
        If (retstat .Eq. 0) Then
           set_zscaling = .True.
        End If
 
!     Default intensity scaling minimum ?
        Call IO_INQ_RKEYVALUE ('Z_SCALING_MINIMUM', min_image, retstat, &
          status)
 
!     Default intensity scaling maximum ?
        Call IO_INQ_RKEYVALUE ('Z_SCALING_MAXIMUM', max_image, retstat, &
          status)
 
!     Default log scaling ?
        Call IO_INQ_LKEYVALUE ('Z_SCALING_LOG', logarithmic, retstat, status)
 
!     Default 1st curve style ?
        Call IO_INQ_KEYVALUE ('CURVE_STYLE_1', len_string, value, retstat, &
          status)
 
        If (retstat .Eq. 0) Then
 
           Read (value, Fmt = '(3l2)', Iostat = iostat) draw_line, &
             draw_markers, draw_errorboxes
 
           If (output_graphics) Then
 
!           Set style of first curve
              Call GS_SET_CURVESTYLE (1, draw_line, draw_markers, &
                draw_errorboxes, status)
           End If
 
        End If
 
!     Set Z-scaling if required
        If (output_graphics .And. set_zscaling) Then
 
           Call GS_SET_IMAGESCALE (scale_mode, min_image, max_image, status)
 
!        Inquire current graphics display settings
           Call GS_INQ_AUTODDR (xminautoddr, yminautoddr, xmaxautoddr, &
             ymaxautoddr, status)
           Call GS_INQ_DDR (xminddr, yminddr, xmaxddr, ymaxddr, status)
 
!        Set X/Y graph Y-axis (Z) range
           If (scale_mode .Eq. 0) Then
              Call GS_SET_AUTODDR (.True., .True., .True., .True., status)
           Else If (scale_mode .Eq. 1) Then
              Call GS_SET_AUTODDR (.True., .False., .True., .False., status)
              Call GS_SET_DDR (xminddr, min_image, xmaxddr, max_image, status)
           Else If (scale_mode .Eq. 2) Then
              Call GS_SET_AUTODDR (.True., .False., .True., .True., status)
              Call GS_SET_DDR (xminddr, min_image, xmaxddr, min_image + 1.0, &
                status)
           Else If (scale_mode .Eq. 3) Then
              Call GS_SET_AUTODDR (.True., .True., .True., .False., status)
              Call GS_SET_DDR (xminddr, max_image - 1.0, xmaxddr, max_image, &
                status)
           End If
 
!        Set log scaling requirements
           Call LG_SET_LOGZSCALING (logarithmic, status)
 
!        Inquire X/Y graph log/linear requirements
           Call GS_INQ_DATALOGLIN (x_linear, y_linear, status)
 
!        Set X/Y graph log/linear requirements
           Call GS_SET_DATALOGLIN (x_linear, .Not. logarithmic, status)
 
        End If
 
!     Set default experiment details 
        Call F2D_DEF_EXPERIMENT (experiment, status)

!     Try to input experimental geometry information
        Call F2D_INQ_EXPERIMENT (experiment, status)

!     Set default values for input options
        INPUT_OPTIONS%auto_input_from_cif = .False.
        INPUT_OPTIONS%input_pixel_sizes = .True.
        INPUT_OPTIONS%input_wavelength = .True.
        INPUT_OPTIONS%input_polarisation = .True.
        INPUT_OPTIONS%input_detector_distance = .True.
        INPUT_OPTIONS%input_beam_centre = .True.
        INPUT_OPTIONS%input_tilt = .True.
        INPUT_OPTIONS%input_two_theta = .True.
        INPUT_OPTIONS%input_goniometer_angles = .True.
        INPUT_OPTIONS%input_image_orientation = .True.
        INPUT_OPTIONS%input_cell = .True.
        INPUT_OPTIONS%input_ub_matrix = .True.
        INPUT_OPTIONS%input_monitor = .True.
        INPUT_OPTIONS%input_attenuation = .True.

!     Try to input values from internal data-base
        Call IO_INQ_LKEYVALUE ('AUTOMATIC_INPUT_FROM_CIF', &
          INPUT_OPTIONS%auto_input_from_cif, retstat, status)
        Call IO_INQ_LKEYVALUE ('AUXILIARY_INPUT_PIXEL_SIZES', &
          INPUT_OPTIONS%input_pixel_sizes, retstat, status)
        Call IO_INQ_LKEYVALUE ('AUXILIARY_INPUT_WAVELENGTH', &
          INPUT_OPTIONS%input_wavelength, retstat, status)
        Call IO_INQ_LKEYVALUE ('AUXILIARY_INPUT_POLARISATION', &
          INPUT_OPTIONS%input_polarisation, retstat, status)
        Call IO_INQ_LKEYVALUE ('AUXILIARY_INPUT_DETECTOR_DISTANCE', &
          INPUT_OPTIONS%input_detector_distance, retstat, status)
        Call IO_INQ_LKEYVALUE ('AUXILIARY_INPUT_BEAM_CENTRE', &
          INPUT_OPTIONS%input_beam_centre, retstat, status)
        Call IO_INQ_LKEYVALUE ('AUXILIARY_INPUT_TILT', &
          INPUT_OPTIONS%input_tilt, retstat, status)
        Call IO_INQ_LKEYVALUE ('AUXILIARY_INPUT_TWO_THETA', &
          INPUT_OPTIONS%input_two_theta, retstat, status)
        Call IO_INQ_LKEYVALUE ('AUXILIARY_INPUT_GONIOMETER_ANGLES', &
          INPUT_OPTIONS%input_goniometer_angles, retstat, status)
        Call IO_INQ_LKEYVALUE ('AUXILIARY_INPUT_IMAGE_ORIENTATION', &
          INPUT_OPTIONS%input_image_orientation, retstat, status)
        Call IO_INQ_LKEYVALUE ('AUXILIARY_INPUT_CELL', &
          INPUT_OPTIONS%input_cell, retstat, status)
        Call IO_INQ_LKEYVALUE ('AUXILIARY_INPUT_UB_MATRIX', &
          INPUT_OPTIONS%input_ub_matrix, retstat, status)
        Call IO_INQ_LKEYVALUE ('AUXILIARY_INPUT_MONITOR', &
          INPUT_OPTIONS%input_monitor, retstat, status)
        Call IO_INQ_LKEYVALUE ('AUXILIARY_INPUT_ATTENUATION', &
          INPUT_OPTIONS%input_attenuation, retstat, status)

     End If
 
     End Subroutine F2D_INP_DATASTORE
!********1*********2*********3*********4*********5*********6*********7*********8
