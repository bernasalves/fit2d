!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_fsintegrate.f90 *
!  *                     *
!  ***********************
 
!+ F2D_FSINTEGRATE - File Series: INTEGRATE
     Subroutine F2D_FSINTEGRATE (input_options, &
       data_defined, memory_exist, memory_defined, &
       variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, &
       YAXIS, DATA, VARIANCES, MASK, title, xlabel, ylabel, zlabel, &
       experiment, xstrelm, ystrelm, xendelm, yendelm, &
       mxnumdat, mynumdat, MXAXIS, MYAXIS, MDATA, MVARIANCES, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    2-D to 1-D scan integration from a series of files, to build up a 2-D
!    vector of scans.
!  Keywords:
!    Integrate.File~Series, File~Series.Integrate, Series.Files.Integrate
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    21-Sep-2012: V0.18 Implement parallax effect correction (Hammersley)
!    19-Sep-2012: V0.17 Option to correct parallax effect (Hammersley)
!    23-Aug-2012: V0.16 Option of look-up table calculation (Hammersley)
!    24-Apr-2006: V0.15 Add "input_options" structure (Hammersley)
!    14-Mar-2006: V0.14 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    07-Apr-2004: V0.13 Add option of subtracting a dark current image 
!      (Hammersley)
!    05-Mar-2004: V0.12 Output warning message when file is missing (Hammersley)
!    02-Mar-1999: V0.11 Remove calls to save wavelength and sample distance 
!      internally since they are save already (Hammersley)
!    23-Feb-1999: V0.10 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    17-Feb-1999: V0.9 "F2D_GUI_INPUT" replaced by "FIO_GUI_INPUT" (Hammersley)
!    15-Dec-1998: V0.8 Change to use IO internal database routines (Hammersley)
!    09-Jul-1998: V0.7 Replace call to "Io_nopath" with "IO_NODIRPATH" 
!      (Hammersley)
!    03-Jul-1998: V0.6 Correct calls to "IO_FREE", previously called "IO_MALLOC"
!      with wrong argument list (Hammersley)
!    02-Mar-1998: V0.5 Correct detector distortions before display for masking 
!      (Hammersley)
!    25-Feb-1998: V0.4 Option to save 1-D scans in output files (Hammersley)
!    23-Feb-1998: V0.3 Add options to correct input data for detector 
!      distortions (Hammersley)
!    22-Feb-1998: V0.2 Use "IO_FILENAME" to generate file names (Hammersley)
!    20-Feb-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined ! .True., if data exists within
!      the program
     Logical, Intent(INOUT) :: memory_exist ! .True. if memory array exists
     Logical, Intent(INOUT) :: memory_defined ! .True. if the memory
!      contains data
     Logical, Intent(INOUT) :: variances_exist ! .True., if a data variance
!      array is created
     Logical, Intent(INOUT) :: mask_exist ! .True., if the mask array exists
     Integer, Intent(INOUT) :: xmaxdat ! Dimension size in X-direction for
!      data arrays
     Integer, Intent(INOUT) :: ymaxdat ! Dimension size in Y-direction for
!      data arrays
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements defined in
!      Y-direction
     Real, Intent(INOUT) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(INOUT) :: YAXIS(ymaxdat) ! Y-axis values
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! The estimated
!      variance values
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! Data mask
     Character(Len = *), Intent(INOUT) :: title ! Title for plot
     Character(Len = *), Intent(INOUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: zlabel ! Z-axis label for plot
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
!  Export:
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
     Real, Intent(OUT) :: MXAXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(OUT) :: MYAXIS(ymaxdat) ! Array containing data Y-coordinates
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Array containing data to
!      be fitted
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat) ! Array containing
!      variances in the data values
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Real, Intent(OUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(OUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.18' ! Version number
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection
!      prompt text
!  Local Variables:
     Real :: az_pixel_size ! Size of pixel to be used for the azimuth bin 
!      sizes. The units are radians
     Logical, Save :: conserve_intensity = .False. ! .True., if the total
!      intensity in to be conserved in the re-binning operation
     Real :: cor_grid_spacing ! Grid spacing for correction function
     Logical :: correct_geometry ! .True., if the scan intensities are
!      to be corrected for the 1/Cos**3(angle) drop owing to the
!      geometrical difference between a 2-theta scan and a flat detector
     Logical :: correct_parallax = .False. ! .True., if the effect of parallax
!      on angular position is to be corrected
     Integer :: db_stat ! Return status from "IO_INQ_*KEYVALUE"
     Logical, Save :: dc_correction = .False. ! .True., if a dark current
!      image is to be subtracted
     Logical :: dc_defined ! .True., if the dark current image is input O.K.
     Character(Len = 256), Save :: dc_file = 'dark_current.bin'
!      Name of the dark current file
     Integer :: end_value ! Value at end of sequence
     Character(Len = 256) :: extension ! File name extension
     Character(Len = 256), Save :: ff_file = 'flat_field.bin'
!      Name of flat-field file
     Integer :: file ! Loop variable for files
     Character(Len = 256) :: file_name ! Name of file in the series
     Character(Len = 80) :: file1 ! First file name without directory path
     Character(Len = 80) :: file2 ! Second file name without directory path
     Character(Len = 256) :: postfix ! Fixed end of file names
     Character(Len = 256) :: prefix ! Fixed start of file names
     Integer :: first_image ! First image in series (not used)
     Character(Len = 256) :: first_file ! Full name of first file
     Logical, Save :: ff_correction = .False. ! .True., if a flat-field
!      correction is to be applied
     Logical :: ff_defined ! .True., if the flat-field image is input O.K.
     Logical, Save :: ff_scale = .False. ! .True., if the flat-fielded
!      image is to scaled
     Logical :: geometry_defined ! .True.,  if geometry is defined
     Integer, Save :: increment ! Step value in sequence
     Character(Len = 256) :: last_file ! Full name of last file
     Integer :: last_image ! Last image in series (not used)
     Integer :: len_string ! Defined length of a string
     Integer, Save :: lorentz_geometry = 1 ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the equivalent of a 
!            2-theta scan i.e. detector at equal distance
     Type(EXPERIMENTAL_DETAILS) :: m_experiment ! "memory" experimental details 
     Integer :: num_2theta ! Number of 2-theta angle or radial bins in
!      rebinned array
     Integer :: num_characters ! Number of characters in numerical part
     Integer :: num_scans ! Number of output scans
     Character(Len = 256) :: output_file ! Name of output file
     Character(Len = 80) :: output_format ! File format for 1-D scan output 
!       files
     Character(Len = Len_name) :: parameter_file ! Output file for saving 
!      parameters
     Integer :: pixel ! Loop variable
     Integer :: retstat ! Return status variable
     Character(Len = 80), Save :: save_1dext = 'int' ! Extension for 1-D
!      scan output files
     Integer stat ! Status return variable for "Allocate"
     Logical, Save :: save_1dscans = .True. ! .True., if the 1-D scans are
!      to be saved to output files
     Logical, Save :: save_parameters = .False. ! ".True." if integration 
!      parameters are to be saved to a file
     Integer, Save :: scan_type = 1 ! Type of 1-D scan to be produced:
!      0 = Radial: equal distance scan
!      1 = "2-theta": equal angle element scan
!      2 = Q-space: Equal Q element scan
!          (Q = (4 * Pi / wavelength) Sin(theta / 2) )
!      3 = D-spacing scan
     Logical, Save :: sd_correction = .False. ! .True., if a spatial
!      distortion correction is to be applied
     Character(Len = 256), Save :: sd_file = 'spatial.spline'
!      Name of spatial distiortion file
     Integer :: start_value ! Value at start of sequence
     Logical :: threshold ! .True., if a threshold is to be applied to
!      the inverse flat-field
     Logical :: variable_characters ! .True., if the number of characters
!      in the numerical part of the names changes
     Integer :: x_xnumknots ! Number of X-axis "knot" positions for
!      X-distortion spline function
     Integer :: x_ynumknots ! Number of Y-axis "knot" positions for
!      X-distortion spline function
     Integer :: xmaxknots ! Maximum number of knot points in X-direction
     Integer :: xnum_dc ! Number of X-elements input from the dark current
     Integer :: xnum_ff ! Number of X-elements input from the flat-field
     Integer :: y ! Variable for the input scans
     Integer :: y_xnumknots ! Number of X-axis "knot" positions for
!      Y-distortion spline function
     Integer :: y_ynumknots ! Number of Y-axis "knot" positions for
!      Y-distortion spline function
     Integer :: ymaxknots ! Maximum number of knot points in Y-direction
     Integer :: ynum_dc ! Number of Y-elements input from the dark current
     Integer :: ynum_ff ! Number of Y-elements input from the flat-field
     Real :: detector_rotation ! Angle of rotation from ideal detector
!      X-axis (laboratory Y-axis) TO X-axis of actual detector i.e.
!      scanned film, or image plate (radians)
     Real, Save :: ff_scaler = 1000.0 ! Scaling to multiple flat-field
!      image with
     Real, Save :: ff_threshold = 10.0 ! Threshold value to apply to
!      maximum multiplication by flat-field correction
     Real :: inner_2theta ! 2-theta angle of inside edge of region
     Real, Save :: maximum_d = 20.0 ! Maximum of range for D-spacings scans
     Real :: outer_angle ! Maximum angle of 2-theta scan
     Real :: outer_limit ! Outer radius in metres
     Real :: outer_q ! Outer q value
     Real :: overload_value ! Value above which pixels are considered to
!      be over-loaded
     Real :: rad_pixel_size ! Size of pixel to be used for the
!      2-theta, radial, or q-space bin sizes. If an equal radial
!      distance scan is to be calculated ("scan_type = 0") the units
!      are metres, and for an equal angle pixel scan ("scan_type = 1")
!      the units are radians, and for a Q-space scan the units are
!      inverse Angstroms (?)
     Logical, Save :: use_lut = .True. ! .True., if a look-up table is to be
!      used for the re-binning operation
     Real :: x_cor_size ! Size of corrected pixel in metres in X-direction
     Real :: x_maximum ! Maximum X-value applicable to spline interpolation
     Real :: x_minimum ! Minimum X-value applicable to spline interpolation
     Real :: x_user_size ! Size of one unit (raw pixel) in X-direction
!      (metres) set by user
     Real :: y_cor_size ! Size of corrected pixel in metres in X-direction
     Real :: y_maximum ! Maximum Y-value applicable to spline interpolation
     Real :: y_minimum ! Minimum Y-value applicable to spline interpolation
     Real :: y_user_size ! Size of one unit (raw pixel) in Y-direction
!      (metres) set by user
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(7) ! User messages
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
     Real, Allocatable :: DC_IMAGE(:, :) ! Dynamic array "DC_IMAGE", to
!      store a dark current image
     Real, Allocatable :: FF_IMAGE(:, :) ! Dynamic array "FF_IMAGE", to
!      store a flat-field image
     Real, Allocatable :: SCANS(:, :) ! Dynamic array "SCANS", to
!      store the output scans
     Real, Allocatable :: WORK(:) ! Dynamic array "WORK": Work array for
!      "F2D_RTHETA2" used to store fractions of pixels contributing to each 
!      angular/radial pixel
     Real, Allocatable :: X_COEFFS(:) ! Dynamic array "X_COEFFS"
     Real, Allocatable :: X_LAMBDA(:) ! Dynamic array "X_LAMBDA"
     Real, Allocatable :: X_MU(:) ! Dynamic array "X_MU"
     Real, Allocatable :: Y_COEFFS(:) ! Dynamic array "Y_COEFFS"
     Real, Allocatable :: Y_LAMBDA(:) ! Dynamic array "Y_LAMBDA"
     Real, Allocatable :: Y_MU(:) ! Dynamic array "Y_MU"
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FSINTEGRATE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_FSINTEGRATE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Define file series
        Call F2D_DEF_FS (input_options, &
          .True., variances_exist, xmaxdat, ymaxdat, retstat, &
          xnumdat, ynumdat, XAXIS, YAXIS, DATA, VARIANCES, title, xlabel, &
          ylabel, zlabel, xstrelm, ystrelm, xendelm, yendelm, &
          first_file, first_image, last_file, last_image, &
          start_value, end_value, prefix, variable_characters, num_characters, &
          postfix, extension, increment, experiment, status)
 
!     Check return status
        If (retstat .Ne. 0) Then
           Return
        Else
           data_defined = .True.
        End If
 
!     Add the "postfix" to the extension if it exists
        If (Len_trim(postfix) .Gt. 0) Then
 
           If (Len_trim(extension) .Gt. 0) Then
              extension = Trim(postfix) // Trim(extension)
           Else
              extension = Trim(postfix)
           End If
 
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''extension = '', a)') Trim(extension)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Remove directory path
        Call IO_NODIRPATH (first_file, file1, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Obtain overloaded intensity value from internal data-base
        Call IO_INQ_RKEYVALUE ('#OVERLOAD_VALUE', overload_value, db_stat, &
          status)
 
        If (db_stat .Ne. 0) Then
           overload_value = 100000.0
        End If
 
!     Try to obtain name of spline file from the data-store
        Call IO_INQ_LKEYVALUE ('DARK_CURRENT_CORRECTION', dc_correction, &
          db_stat, status)
        Call IO_INQ_KEYVALUE ('DARK_CURRENT_FILE', len_string, dc_file, &
          db_stat, status)
        Call IO_INQ_LKEYVALUE ('FF_CORRECTION', ff_correction, db_stat, &
          status)
        Call IO_INQ_KEYVALUE ('FF_FILE', len_string, ff_file, db_stat, status)
        Call IO_INQ_LKEYVALUE ('FF_SCALE', ff_scale, db_stat, status)
        Call IO_INQ_RKEYVALUE ('FF_SCALER', ff_scaler, db_stat, status)
        Call IO_INQ_LKEYVALUE ('SD_CORRECTION', sd_correction, db_stat, &
          status)
        Call IO_INQ_KEYVALUE ('SD_FILE', len_string, sd_file, db_stat, status)
        Call IO_INQ_LKEYVALUE ('FS_INT_SAVE_1D', save_1dscans, db_stat, &
          status)
        Call IO_INQ_KEYVALUE ('FS_INT_SAVE_EXT', len_string, save_1dext, &
          db_stat, status)
 
!     Allow user of set detector distortion control parameters
        Call F2D_INP_CORRECTION (dc_correction, dc_file, ff_correction, &
          ff_file, ff_scale, ff_scaler, sd_correction, sd_file, status)
 
        If (status .Ne. St_goodvalue) Then
 
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
           End If
           Return
 
        End If
 
!     Save set-up in internal data-store
        Call IO_SET_LKEYVALUE ('DARK_CURRENT_CORRECTION', dc_correction, &
          db_stat, status)
        len_string = Len_trim(dc_file)
        Call IO_SET_KEYVALUE ('DARK_CURRENT_FILE', len_string, dc_file, &
          db_stat, status)
        Call IO_SET_LKEYVALUE ('FF_CORRECTION', ff_correction, db_stat, &
          status)
        len_string = Len_trim(ff_file)
        Call IO_SET_KEYVALUE ('FF_FILE', len_string, ff_file, db_stat, status)
        Call IO_SET_LKEYVALUE ('FF_SCALE', ff_scale, db_stat, status)
        Call IO_SET_RKEYVALUE ('FF_SCALER', ff_scaler, db_stat, status)
        Call IO_SET_LKEYVALUE ('SD_CORRECTION', sd_correction, db_stat, &
          status)
        len_string = Len_trim(sd_file)
        Call IO_SET_KEYVALUE ('SD_FILE', len_string, sd_file, db_stat, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
        If (dc_correction) Then
 
!        Allocate memory to store dark current image
           Allocate (DC_IMAGE(xnumdat, ynumdat), Stat = stat)
           If (stat .Ne. 0) Then
              Call GS_FWARNING (1, 1, &
                'PROBLEM ALLOCATING MEMORY FOR DARK CURRENT IMAGE', status)
              Return
           End If

           dc_defined = .False.
           Call GS_PPROMPT (1, 1, 'INPUT OF DARK CURRENT IMAGE', status)
           Call GS_UPDATE (status)
           Call IO_SLEEP (1.0, status)
 
           Call IO_WRITE ('INFO: Input of dark current image', status)
           PROMPT(1) = 'SELECT DARK CURRENT FILE'
           PROMPT(2) = '(click on "HELP" for list of formats)'
           Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, -2147483647, &
             input_options, xnumdat, ynumdat, .False., dc_defined, dc_file, &
             xnum_dc, ynum_dc, MXAXIS, MYAXIS, DC_IMAGE, MVARIANCES, &
             mtitle, mxlabel, mylabel, mzlabel, m_experiment, status)

           If (.Not. dc_defined) Then
 
              Call GS_FWARNING (1, 1, &
                'PROBLEM INPUTTING THE DARK CURRENT IMAGE', status)
 
!           Free dark current image array space
              Deallocate (DC_IMAGE)
 
           Else If (xnum_dc .Ne. xnumdat .Or. ynum_dc .Ne. ynumdat) Then
 
              Call GS_FWARNING (1, 1, &
                'THE DARK CURRENT IMAGE IS THE WRONG SIZE', status)
 
!           Free dark current image array space
              Deallocate (DC_IMAGE)
              Return
 
           Else
 
              Call GS_BACKGROUND (status)
 
!           Apply correction to data
              Call IO_WRITE ( 'INFO: Subtraction dark current', status)
 
!           Subtract dark current
              Call MA_RSUBTRACT (xnumdat, ynumdat, 1, 1, xnumdat, ynumdat, &
                DC_IMAGE, xmaxdat, ymaxdat, DATA, status)
 
           End If
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        If (ff_correction) Then
 
!        Allocate memory to store flat-field image
           Allocate (FF_IMAGE(xnumdat, ynumdat), Stat = stat)
           If (stat .Ne. 0) Then
              Call GS_FWARNING (1, 1, &
                'PROBLEM ALLOCATING MEMORY FOR FLAT-FIELD IMAGE', status)
              Return
           End If
 
           ff_defined = .False.
           Call GS_PPROMPT (1, 1, 'INPUT OF FLAT-FIELD IMAGE', status)
           Call GS_UPDATE (status)
           Call IO_SLEEP (1.0, status)
 
           Call IO_WRITE ('INFO: Input of flat-field image', status)
           PROMPT(1) = 'SELECT FLAT FIELD FILE'
           PROMPT(2) = '(click on "HELP" for list of formats)'
           Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, -2147483647, &
             input_options, xnumdat, ynumdat, &
             .False., ff_defined, ff_file, xnum_ff, ynum_ff, MXAXIS, &
             MYAXIS, FF_IMAGE, MVARIANCES, mtitle, mxlabel, mylabel, &
             mzlabel, m_experiment, status)

           If (.Not. ff_defined) Then
 
              Call GS_FWARNING (1, 1, &
                'PROBLEM INPUTTING THE FLAT-FIELD IMAGE', status)
 
!           Free flat-field image array space
              Deallocate (FF_IMAGE)
 
           Else If (xnum_ff .Ne. xnumdat .Or. ynum_ff .Ne. ynumdat) Then
 
              Call GS_FWARNING (1, 1, &
                'THE FLAT-FIELD IMAGE IS THE WRONG SIZE', status)
 
!           Free flat-field image array space
              Deallocate (FF_IMAGE)
              Return
 
           Else
 
              Call GS_BACKGROUND (status)
              Call GS_FPROMPT (1, 1, &
                'CALCULATING SCALED INVERSE OF FLAT-FIELD', status)
              Call GS_UPDATE (status)
 
!           Calculate inverse of flat-field image
              Call MA_POWER (xnumdat, ynumdat, 1, 1, xnumdat, ynumdat, -1.0, &
                FF_IMAGE, retstat, status)
 
              If (retstat .Eq. 2) Then
 
!              Division by 0.0
                 Call GS_FWARNING (1, 1, &
                   'THE FLAT-FIELD CONTAINED ONE OR MORE ZEROS', status)
 
                 Call GS_INPR (.True., 0.001, 1.7e10, .True., &
                   'MAXIMUM MULTIPLER FOR FLAT-FIELD CORRECTION', 1, &
                   'Enter maximum value to increase intensity', 1, &
                   'Enter real number within given range', ff_threshold, &
                   status)
 
                 threshold = .True.
 
              Else
                 threshold = .False.
              End If
 
              If (ff_scale) Then
 
!              Scale inverse flat-field correction
                 Call MA_RCMULT (xnumdat, ynumdat, 1, 1, xnumdat, ynumdat, &
                   ff_scaler, FF_IMAGE, status)
 
              End If
 
              If (threshold) Then
 
!              Threshold inverse flat-field image
                 Call MA_THRESHOLD (xnumdat, ynumdat, 1, 1, xnumdat, ynumdat, &
                   .False., .True., 0.0, ff_threshold, FF_IMAGE, status)
 
              End If
 
!           Apply correction to data
              Call IO_WRITE ( 'INFO: Applying flat-field correction', status)
 
!           Multiply data by scaled inverse flat-field image
              Call MA_RMULTIPLY (xnumdat, ynumdat, 1, 1, xnumdat, ynumdat, &
                FF_IMAGE, xmaxdat, ymaxdat, DATA, status)
 
           End If
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
        If (sd_correction) Then
 
!        Get dynamic memory for spline coefficient arrays
           xmaxknots = 100
           ymaxknots = 100
           Allocate (X_LAMBDA(xmaxknots), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_FSINTEGRATE ' // Version)
              Return
           End If
           Allocate (Y_LAMBDA(xmaxknots), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_FSINTEGRATE ' // Version)
              Return
           End If
           Allocate (X_MU(ymaxknots), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_FSINTEGRATE ' // Version)
              Return
           End If
           Allocate (Y_MU(ymaxknots), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_FSINTEGRATE ' // Version)
              Return
           End If
           Allocate (X_COEFFS((xmaxknots - 4) * (ymaxknots - 4)), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_FSINTEGRATE ' // Version)
              Return
           End If
           Allocate (Y_COEFFS((xmaxknots - 4) * (ymaxknots - 4)), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_FSINTEGRATE ' // Version)
              Return
           End If
 
!        Input values of spline coefficients from file
           Call F2D_IN_SPATIAL (.True., xmaxknots, ymaxknots, sd_file, &
             retstat, x_minimum, y_minimum, x_maximum, y_maximum, &
             cor_grid_spacing, x_cor_size, y_cor_size, x_xnumknots, &
             x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
             y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
 
!        Check status
           If (retstat .Ne. 0) Then
 
              Call GS_FWARNING (1, 1, 'PROBLEM WITH SPATIAL DISTORTION FILE', &
                status)
 
!           Free array space
              Deallocate (X_LAMBDA)
              Deallocate (Y_LAMBDA)
              Deallocate (X_MU)
              Deallocate (Y_MU)
              Deallocate (X_COEFFS)
              Deallocate (Y_COEFFS)
 
              If (ff_correction) Then
 
!              Free flat-field image array space
                 Deallocate (FF_IMAGE)
              End If
 
              Return
 
           End If
 
!        Output user message
           Call GS_FPROMPT (1, 1, '1ST IMAGE: CORRECTING SPATIAL DISTORTION', &
             status)
           Call GS_UPDATE (status)
           Call IO_WRITE ( 'INFO: 1st image: Spatial distortion correction', &
             status)
 
!        Apply spatial distortion correction functions to data
           Call F2D_CORR_SPATIAL (.False., .True., x_minimum, y_minimum, &
             x_maximum, y_maximum, x_cor_size, y_cor_size, x_xnumknots, &
             x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
             y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, &
             xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, 1, 1, &
             xnumdat, ynumdat, 40, overload_value, MXAXIS, MYAXIS, MDATA, &
             mxstrelm, mystrelm, mxendelm, myendelm, status)
           mxnumdat = mxendelm
           mynumdat = myendelm
           memory_defined = .True.
 
!        Set pixel sizes to be spatially corrected ones
           experiment%x_pixel_size = x_cor_size
           experiment%y_pixel_size = y_cor_size
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Information message
        Call IO_WRITE ('INFO: The "MASK" menu allows you to ' // &
          'mask out bad pixels and regions,', status)
        Call IO_WRITE ('      and to zoom in on a ' // &
          'sub-region, if required.', status)
        MESSAGE(1) = 'THE MASKING MENU WILL NOW'
        MESSAGE(2) = 'APPEAR. YOU CAN ZOOM IN ON A'
        MESSAGE(3) = 'SUB-REGION AND MASK BAD DATA.'
        MESSAGE(4) = ' '
        MESSAGE(5) = 'CLICK EXIT WHEN FINISHED'
        MESSAGE(6) = ' '
        MESSAGE(7) = ' '
        Call GS_PPROMPT (7, 7, MESSAGE, status)
        Call GS_UPDATE (status)
        Call IO_SLEEP (2.0, status)
 
!     Allow mask to be changed
        If (sd_correction) Then
 
           Call F2D_MASK (.False., xmaxdat, ymaxdat, xnumdat, ynumdat, MDATA, &
             MXAXIS, MYAXIS, title, xlabel, ylabel, zlabel, experiment, &
             xstrelm, ystrelm, xendelm, yendelm, MASK, status)
 
        Else
 
           Call F2D_MASK (.False., xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, &
             XAXIS, YAXIS, title, xlabel, ylabel, zlabel, experiment, &
             xstrelm, ystrelm, xendelm, yendelm, MASK, status)
 
        End If
 
!     Find any previously set user preferences
        Call IO_INQ_IKEYVALUE ('SCAN_TYPE', scan_type, retstat, status)
        Call IO_INQ_RKEYVALUE ('MAXIMUM_D', maximum_d, retstat, status)
 
!     Integrate control parameters
        Call F2D_INQ_INTEGRATE (file_name, xmaxdat, ymaxdat, &
          xnumdat, ynumdat, xstrelm, &
          ystrelm, xendelm, yendelm, experiment, &
          outer_limit, scan_type, maximum_d, conserve_intensity, &
          correct_geometry, outer_angle, &
          num_2theta, lorentz_geometry, rad_pixel_size, outer_q, &
          save_parameters, parameter_file, use_lut, correct_parallax, status)
 
!     Save set user preferences
        Call IO_SET_IKEYVALUE ('SCAN_TYPE', scan_type, retstat, status)
        Call IO_SET_RKEYVALUE ('MAXIMUM_D', maximum_d, retstat, status)
 
!     Check status
        If (status .Ne. St_goodvalue) Then
 
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
           End If
 
           Return
 
        End If
 
!     Option to save each scan in an output file
        Call GS_BACKGROUND (status)
        Call GS_INPL (.True., 0, 1, .True., &
          'SAVE EACH INTEGRATED SCAN TO FILE', 1, &
          '"YES", to save scans in output files', 1, 'Click "YES" or "NO"', &
          save_1dscans, status)
 
        Call IO_SET_LKEYVALUE ('FS_INT_SAVE_1D', save_1dscans, db_stat, status)
 
        If (save_1dscans) Then
 
!        Input choice of output format
           Call FIO_INP_OUTPUT (output_format, status)
 
!        Input output file extension
           Call GS_INPC (.True., '1-D SCAN OUTPUT FILE EXTENSION', 1, &
             'Choose file extension for output files for 1-D scans', 1, &
             'Enter short character string', 1, save_1dext, status)
 
           len_string = Len_trim(save_1dext)
           Call IO_SET_KEYVALUE ('FS_INT_SAVE_EXT', len_string, save_1dext, &
             db_stat, status)
 
        End If
 
!     Check status
        If (status .Ne. St_goodvalue) Then
 
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
           Else
 
              Call ST_DEF_SYSTEM (status)
              Call GS_FWARNING (1, 1, 'PROBLEM IN FORM INPUT', status)
 
           End If
 
           If (ff_correction) Then
              Deallocate (FF_IMAGE)
           End If
           If (sd_correction) Then
              Deallocate (X_LAMBDA)
              Deallocate (Y_LAMBDA)
              Deallocate (X_MU)
              Deallocate (Y_MU)
              Deallocate (X_COEFFS)
              Deallocate (Y_COEFFS)
           End If
           Return
 
        End If
 
!     Remember set pixel sizes, as they may be different to ones input from the 
!     files
        x_user_size = experiment%x_pixel_size
        y_user_size = experiment%y_pixel_size
 
!     Get dynamic work array space
        Allocate (WORK(num_2theta), Stat = stat)
        If (stat .Ne. 0) Then
 
!        Dynamic memory allocation failed
           Call GS_FWARNING (1, 1, &
             'INTEGRATION WORK ARRAY: MEMORY ALLOCATION FAILED', status)
 
           If (ff_correction) Then
              Deallocate (FF_IMAGE)
           End If
           If (sd_correction) Then
              Deallocate (X_LAMBDA)
              Deallocate (Y_LAMBDA)
              Deallocate (X_MU)
              Deallocate (Y_MU)
              Deallocate (X_COEFFS)
              Deallocate (Y_COEFFS)
           End If
           Return
 
        End If
 
!     Number of output scans
        num_scans = 0
        Do file = start_value, end_value, increment
           num_scans = num_scans + 1
        End Do
 
!     Get dynamic work array space to hold output scans
        Allocate (SCANS(num_2theta, num_scans), Stat = stat)
 
!     Check status
        If (stat .Ne. 0) Then
 
!        Dynamic memory allocation failed
           Call GS_FWARNING (1, 1, &
             'OUTPUT SCANS WORK ARRAY: MEMORY ALLOCATION FAILED', status)
 
           Deallocate (WORK)
           If (ff_correction) Then
              Deallocate (FF_IMAGE)
           End If
           If (sd_correction) Then
              Deallocate (X_LAMBDA)
              Deallocate (Y_LAMBDA)
              Deallocate (X_MU)
              Deallocate (Y_MU)
              Deallocate (X_COEFFS)
              Deallocate (Y_COEFFS)
           End If
           Return
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     User progress macro
        Call GS_BACKGROUND (status)
        Call GS_FPROMPT (1, 1, 'AUTOMATIC SEQUENCE INPUT STARTED', status)
        Call GS_UPDATE (status)
 
!     Automatic part of sequence processing
        y = 0
        Do file = start_value, end_value, increment
 
           y = y + 1
 
!        Generate input file name
           Call IO_FILENAME (prefix, file, .Not. variable_characters, &
             num_characters, extension, retstat, file_name, status)
 
           If (y .Ne. 1) Then
 
!           Progress report
              MESSAGE(1) = 'INPUTTING FILE:'
              MESSAGE(2) = file_name
              Call GS_FPROMPT (2, 2, MESSAGE, status)
              Call GS_UPDATE (status)

!           Input file
              data_defined = .False.
              Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, -2147483647, &
                input_options, xmaxdat, &
                ymaxdat, variances_exist, data_defined, file_name, xnumdat, &
                ynumdat, XAXIS, YAXIS, DATA, VARIANCES, title, xlabel, ylabel, &
                zlabel, experiment, status)
 
              If (data_defined) Then
 
                 Call IO_WRITE ('INFO: Input ' // file_name, status)
 
                 If (dc_correction) Then
 
                    Call IO_WRITE ('INFO: Subtracting dark current image', &
                      status)
 
!                 Subtract dark current image
                    Call MA_RSUBTRACT (xnumdat, ynumdat, 1, 1, xnumdat, &
                      ynumdat, DC_IMAGE, xmaxdat, ymaxdat, DATA, status)
 
                 End If
 
                 If (ff_correction) Then
 
                    Call IO_WRITE ('INFO: Applying flat-field correction', &
                      status)
 
!                 Multiply data by scaled inverse flat-field image
                    Call MA_RMULTIPLY (xnumdat, ynumdat, 1, 1, xnumdat, &
                      ynumdat, FF_IMAGE, xmaxdat, ymaxdat, DATA, status)
 
                 End If
 
                 If (sd_correction) Then
 
!                 Output user message
                    Call GS_FPROMPT (1, 1, 'CORRECTING SPATIAL DISTORTION', &
                      status)
                    Call GS_UPDATE (status)
                    Call IO_WRITE ( &
                      'INFO: Applying spatial distortion correction', status)
 
!                 Apply spatial distortion correction functions to data
                    Call F2D_CORR_SPATIAL (.False., .True., x_minimum, &
                      y_minimum, x_maximum, y_maximum, x_cor_size, y_cor_size, &
                      x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
                      y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, &
                      xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, 1, 1, &
                      xnumdat, ynumdat, 40, overload_value, MXAXIS, MYAXIS, &
                      MDATA, mxstrelm, mystrelm, mxendelm, myendelm, status)
                    mxnumdat = mxendelm
                    mynumdat = myendelm
                    memory_defined = .True.
 
                 End If
 
              End If
 
           End If
 
           If (data_defined) Then
 
!           Output user message
              Call GS_FPROMPT (1, 1, 'INTEGRATING DATA', status)
              Call GS_UPDATE (status)
              Call IO_WRITE ('INFO: Integrating data', status)
  
!           Set pixel sizes to those set by the user
              experiment%x_pixel_size = x_user_size
              experiment%y_pixel_size = y_user_size

!           Calculate 2-theta scan
              If (sd_correction) Then
 
                 If (use_lut) Then
                    Call F2D_CAL_LUT_CAKE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, MDATA, MASK, conserve_intensity, &
                      scan_type, maximum_d, experiment, 0.0, 360.0, 0.0, &
                      outer_limit, num_2theta, 1, lorentz_geometry, &
                      correct_parallax, num_2theta, 1, num_2theta, 1, &
                      az_pixel_size, rad_pixel_size, inner_2theta, WORK, &
                      SCANS(1, y), status)
                 Else
                    Call F2D_CAL2_CAKE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, MDATA, MASK, conserve_intensity, &
                      scan_type, maximum_d, experiment, 0.0, 360.0, 0.0, &
                      outer_limit, num_2theta, 1, lorentz_geometry, &
                      correct_parallax, num_2theta, 1, num_2theta, 1, &
                      az_pixel_size, rad_pixel_size, inner_2theta, WORK, &
                      SCANS(1, y), status)
                 End If

              Else
 
                 If (use_lut) Then
                    Call F2D_CAL_LUT_CAKE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, DATA, MASK, conserve_intensity, &
                      scan_type, maximum_d, experiment, 0.0, 360.0, 0.0, &
                      outer_limit, num_2theta, 1, lorentz_geometry, &
                      correct_parallax, num_2theta, 1, num_2theta, 1, &
                      az_pixel_size, rad_pixel_size, inner_2theta, WORK, &
                      SCANS(1, y), status)

                 Else
                    Call F2D_CAL2_CAKE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, DATA, MASK, conserve_intensity, &
                      scan_type, maximum_d, experiment, 0.0, 360.0, 0.0, &
                      outer_limit, num_2theta, 1, lorentz_geometry, &
                      correct_parallax, num_2theta, 1, num_2theta, 1, &
                      az_pixel_size, rad_pixel_size, inner_2theta, WORK, &
                      SCANS(1, y), status)
                 End If

              End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''Integration finished'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Define axis data
              If (scan_type .Eq. 0) Then
 
                 Do pixel = 1, num_2theta
                    MXAXIS(pixel) = ((Real(pixel) - 0.5) * rad_pixel_size) * &
                      1000.0
                 End Do
 
                 xlabel = 'Radial Distance (mm)'
 
              Else If (scan_type .Eq. 1) Then
 
                 Do pixel = 1, num_2theta
                    MXAXIS(pixel) = (inner_2theta + (Real(pixel) - 0.5) * &
                      rad_pixel_size) * 180.0 / Pi
                 End Do
 
                 xlabel = '2-Theta Angle (Degrees)'
 
              Else If (scan_type .Eq. 2) Then
 
                 Do pixel = 1, num_2theta
                    MXAXIS(pixel) = (inner_2theta + (Real(pixel) - 0.5) * &
                      rad_pixel_size)
                 End Do
 
                 xlabel = 'Q (Inverse Nanometres)'
 
              Else If (scan_type .Eq. 3) Then
 
 
                 Do pixel = 1, num_2theta
                    MXAXIS(pixel) = (inner_2theta + (Real(pixel) - 0.5) * &
                      rad_pixel_size)
                 End Do
 
                 xlabel = 'D-spacing (Angstroms)'
 
              End If
              zlabel = 'Intensity'
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''Axis data defined'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Output 1-D data to the screen
              Call GS_XYSGRAPH (num_2theta, 1, num_2theta, MXAXIS, &
                SCANS(1, y), file_name, xlabel, 'Intensity', status)
 
              If (save_1dscans) Then
 
!              Generate output file name
                 Call IO_FILEEXTENSION (file_name, save_1dext, retstat, &
                   output_file, status)
 
                 Call IO_WRITE ('INFO: Saving 1-D scan to file:', status)
                 Call IO_WRITE (output_file, status)
 
!              Output 1-D scan to a file
                 Call FIO_GUI_OUT (output_file, output_format, .True., &
                   num_2theta, 1, 1, 1, num_2theta, 1, MXAXIS, YAXIS, &
                   SCANS(1, y), VARIANCES, title, &
                   xlabel, ylabel, zlabel, variances_exist, &
                   experiment%wavelength, experiment%detector_distance, &
                   rad_pixel_size, az_pixel_size, output_file, retstat, status)
 
              End If
 
           Else
 
!           File not input
              Call IO_WRITE ('WARNING: File not input: ' // file_name, status)
 
!           Set array values to zero
              Call MA_RVALUE (xmaxdat, 1, 1, 1, num_2theta, 1, 0.0, &
                SCANS(1, y), status)
 
           End If
 
        End Do
 
!     Transfer data from temporary array to current data
        Call MA_RCOPY (num_2theta, num_scans, SCANS, 1, 1, num_2theta, &
          num_scans, xmaxdat, ymaxdat, DATA, status)
 
!     Free dynamic array space used for integration
        Deallocate (SCANS)
        Deallocate (WORK)
 
        If (dc_correction) Then
 
!        Free dark current image array space
           Deallocate (DC_IMAGE)
 
        End If
 
        If (ff_correction) Then
 
!        Free flat-field image array space
           Deallocate (FF_IMAGE)
 
        End If
 
        If (sd_correction) Then
 
!        Free array space
           Deallocate (X_LAMBDA)
           Deallocate (Y_LAMBDA)
           Deallocate (X_MU)
           Deallocate (Y_MU)
           Deallocate (X_COEFFS)
           Deallocate (Y_COEFFS)
 
        End If
 
!     Transfer axis data from memory array to current data
        Call MA_RCOPY (xmaxdat, 1, MXAXIS, 1, 1, num_2theta, 1, xmaxdat, 1, &
          XAXIS, status)
 
!     Set values in memory X-axis
        Do pixel = 1, mxnumdat
           MXAXIS(pixel) = Real(pixel) - 0.5
        End Do
 
!     Set scan number axis
        Do pixel = 1, y
           YAXIS(pixel) = Real(pixel) - 0.5
        End Do
 
        ylabel = 'Scan Number'
 
        xstrelm = 1
        ystrelm = 1
        xendelm = num_2theta
        yendelm = y
        xnumdat = num_2theta
        ynumdat = y
        data_defined = .True.
 
!     Set combined title
        Call IO_NODIRPATH (last_file, file2, status)
        title = Trim(file1) // ' to ' // Trim(file2)

        mx_pixel_size = m_experiment%x_pixel_size
        my_pixel_size = m_experiment%y_pixel_size
 
     End If
 
     End Subroutine F2D_FSINTEGRATE
!********1*********2*********3*********4*********5*********6*********7*********8
 
