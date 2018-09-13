!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_calibration.f90 *
!  *                     *
!  ***********************
 
!+ F2D_CALIBRATION - FIT 2-D CALIBRATION sub-menu
     Subroutine F2D_CALIBRATION (memory_exist, variances_exist, input_file, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, YAXIS, DATA, VARIANCES, &
       xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
       x_pixel_size, y_pixel_size, mxnumdat, mynumdat, MXAXIS, MYAXIS, MDATA, &
       MVARIANCES, mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, &
       mylabel, mzlabel, memory_defined, mx_pixel_size, my_pixel_size, status)
!  Description:
!    Menu for calculating and applying calibration functions
!  Method:
!    Menu driven loop
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    25-Apr-2006: V2.31 Use Fortran-90 dynamically allocated arrays (Hammersley)
!    31-Mar-2006: V2.30 Changes to "F2D_CAL_DISTORTION" (Hammersley)
!    09-Apr-1999: V2.29 Add "FALSE PEAK" option (Hammersley)
!    23-Feb-1999: V2.28 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V2.27 Change to use IO internal database routines (Hammersley)
!    17-Sep-1997: V2.26 Change to "F2D_INP_DCLUT" (Hammersley)
!    03-Sep-1997: V2.25 Remove option add in V2.24, as at present only 1-byte 
!      per pixel LUT are supported, and the flat-field correction is handled 
!      separately for these. (The LUT flat-field correction would make sense for
!      2-byte per pixel version.) (Hammersley)
!    05-Mar-1997: V2.24 Option to apply "flat-field" correction to distortion 
!      correction look-up table (Hammersley)
!    03-Mar-1997: V2.23 Option to load distortion correction LUT from file 
!      (Hammersley)
!    01-Mar-1997: V2.22 Name of input file passed to routine (Hammersley)
!    27-Feb-1997: V2.21 Add option to output spatial distortion
!      look-up table to a file (Hammersley)
!    03-Oct-1996: V2.20 Add commands to allow creation of a spatial
!      distortion look-up table, and use it to correct data (Hammersley)
!    03-Sep-1996: V2.19 Use "FITPACK" routine "SURFIT" to fit spatial distortion
!      (Hammersley)
!    02-Sep-1996: V2.18 Remove "Z BISPEV" and "Z CALCULATE" options since now
!      "MA_CAL_2DNCUBSPLINE" is used for standard spatial distortion 
!      correction and spline surface calculation. (Hammersley)
!    28-Aug-1996: V2.17 Add temporary command "Z CAL" to allow testing and 
!      comparison of "MA_CAL_2DNCUBSPLINE" evaluation of bi-cubic 
!      spline functions (Hammersley)
!    20-Aug-1996: V2.16 Add temporary command "Z BISPEV" to allow
!      testing and comparison of FITPACK evaluation of bi-cubic
!      spline function and temporary command "Z FIT GRID" to
!      test and compare SURFIT (Hammersley)
!    03-Jun-1996: V2.15 Double size of peak arrays so that up to
!      400 peaks may be stored in either direction (Hammersley)
!    17-Apr-1996: V2.14 Use format type to define default values (Hammersley)
!    29-Mar-1996: V2.13 Increase default size of "FIND PEAKS" display
!      region (Hammersley)
!    26-Feb-1996: V2.12 Changes to "F2D_IN_SPATIAL" (Hammersley)
!    07-Sep-1995: V2.11 Check for re-sizing of graphics window (Hammersley)
!    12-Aug-1995: V2.10 Add mask vignetting correction option (Hammersley)
!    18-Jan-1995: V2.9 Pixel sizes for the current data and the memory are 
!      input and output (Hammersley)
!    06-Jan-1995: V2.8 Change menu prompt to upper case "ENTER COMMAND" 
!      (Hammersley)
!    19-Dec-1994: V2.7 Change definition of pixel sizes to metre units 
!      (Hammersley)
!    06-Sep-1994: V2.5 Add command to change distortion measurements
!      to those relative to the ideal grid (by default the distortion
!      measurements are based on the distorted grid (Hammersley)
!    29-Oct-1994: V2.6 Hypenate "flat-field" (Hammersley)
!    24-Jun-1994: V2.4 Theoretical flat-field correction for X-ray
!      image intensifier system (Hammersley)
!    23-Jun-1994: V2.3 Add image plate decay correction option
!      "DECAY CORRECTION" (Hammersley)
!    22-Jun-1994: V2.2 Add option to create residuals between measured peak 
!      positions and fitted spline (Hammersley)
!    21-Jun-1994: V2.1 Add option to destory "found" grid peaks (Hammersley)
!    04-May-1994: V2.0 Change to argument list, now the memory does
!      not necessarily exist so flags is set and certain options must
!      check for its existence (Hammersley)
!    29-Apr-1994: V1.9 Add option to correct image for non-linearity 
!      (Hammersley)
!    28-Apr-1994: V1.8 Add option to calculate full fitted distortion function 
!      in memory (Hammersley)
!    27-Apr-1994: V1.7 Change to "F2D_FIT2DGRID" arguments, "spatial_exist" is 
!      now set within call (Hammersley)
!    31-Mar-1994: V1.6 Add option to output "PLATYPUS" calibration file 
!      (Hammersley)
!    24-Mar-1994: V1.5 Add option to change size of displayed region in "FIND 
!      PEAKS" (Hammersley)
!    08-Feb-1994: V1.4 Pass limits of valid spatially calibrated
!      region to correction subroutine (Hammersley)
!    02-Feb-1993: V1.3 Add flat-field calibration calibration option 
!      (Hammersley)
!    04-Jan-1994: V1.2 Return status from "F2D_CALIBRATE2DGRID" (Hammersley)
!    10-Nov-1993: V1.1 Add command to "learn" hole profile (Hammersley)
!    03-Nov-1993: V1.0 Convert to 2-D peak position and distortion
!      arrays (Hammersley)
!    21-Oct-1993: V0.9 New 2-D grid peak search command (Hammersley)
!    13-Oct-1993: V0.8 Save re-binned ideal pixel dimensions in spline 
!      definition files (Hammersley)
!    01-Oct-1993: V0.7 Convert spline defining arrays to single precision 
!      (Hammersley)
!    21-Sep-1993: V0.6 Option of destroying dynamic arrays (Hammersley)
!    15-Sep-1993: V0.5 Add "re-calculate distortion" option (Hammersley)
!    17-Aug-1993: V0.4 Create 2-D "image" of grid peak distortions (Hammersley)
!    16-Aug-1993: V0.3 Command to output peaks and distortion (Hammersley)
!    13-Aug-1993: V0.2 Add commands to allow inspection of results
!      and input and output of interpolating spline functions (Hammersley)
!    11-Aug-1993: V0.1 Original, based on "F2D_FIT" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Logical, Intent(IN) :: memory_exist ! .True., if memory arrays exist
     Logical, Intent(IN) :: variances_exist ! .True., if error arrays exist
     Character(Len = *), Intent(IN) :: input_file ! Name of current data input 
!      file
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Integer, Intent(IN) :: xnumdat ! Defines X-extent of data region
     Integer, Intent(IN) :: ynumdat ! Defines Y-extent of data region
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Array containing data Y-coordinates
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Array containing data to
!      be fitted
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! Array containing
!      variances in the data values
     Integer, Intent(IN) :: xendelm ! End X-element of region to be fitted
     Integer, Intent(IN) :: xstrelm ! Starting X-element of region to be fitted
     Integer, Intent(IN) :: yendelm ! End Y-element of region to be fitted
     Integer, Intent(IN) :: ystrelm ! Starting Y-element of region to be fitted
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
     Real, Intent(IN) :: x_pixel_size ! Size of a pixel in the current data
!      in the X-direction (metres)
     Real, Intent(IN) :: y_pixel_size ! Size of a pixel in the current data
!      in the Y-direction (metres)
!  Import/Export:
     Integer, Intent(INOUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(INOUT) :: mynumdat ! Defines Y-extent of data region
     Real, Intent(INOUT) :: MXAXIS(xmaxdat) ! Memory X-coordinates
     Real, Intent(INOUT) :: MYAXIS(ymaxdat) ! Memory Y-coordinates
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat) ! Array containing data
!      to be fitted
     Real, Intent(INOUT) :: MVARIANCES(xmaxdat, ymaxdat) ! Array containing
!      variances in the data values
     Integer, Intent(INOUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(INOUT) :: mxstrelm ! Starting X-element of memory data
!      region
     Integer, Intent(INOUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(INOUT) :: mystrelm ! Starting Y-element of memory data
!      region
     Character(Len = *), Intent(INOUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(INOUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(INOUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(INOUT) :: mzlabel ! Z-axis label for data
     Logical, Intent(INOUT) :: memory_defined ! .True. if the memory
!      contains data
     Real, Intent(INOUT) :: mx_pixel_size ! Size of a corrected pixel in the
!      memory data in the X-direction (metres)
     Real, Intent(INOUT) :: my_pixel_size ! Size of a corrected pixel in the
!      memory data in the Y-direction (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V2.31' ! Version number
     Integer, Parameter :: Max_menu = 30 ! Number of instructions in the menu
!  Local Variables:
     Character(Len = 80) :: command ! Full command corresponding to user
!      request
     Character(Len = 80) :: file_format ! Choice of file format to input
     Character(Len = 256), Save :: spline_file = 'grid.spline'
!      Name of file containing distortion function
     Integer :: db_stat ! Data Store status return variable
     Integer :: item ! Loop variable for "DATA" statements items
     Integer :: len_string ! Defined length of a string
     Integer, Save :: num_display = 400 ! Number of pixels (in each
!      dimension) to display in the "FIND PEAKS" starting peaks option
     Integer :: num_menu ! Number of choices available in the menu
     Integer, Save :: num_peaks ! Number of peaks found in calibration grid
     Integer :: retstat ! Return status from "F2D_IN_SPATIAL":
!      or from "F2D_CALIBRATE2DGRID"
!        0 = Good status
!        1 = Bad status, grid too large for internal array
!        2 = Bad status, peaks missing from central cross
     Integer stat ! Status return variable for "Allocate"
     Integer :: temp_status ! Temporary "status" variable
     Integer, Save :: x_xnumknots ! Number of X-axis "knot" positions for
!      X-distortion spline function
     Integer, Save :: x_ynumknots ! Number of Y-axis "knot" positions for
!      X-distortion spline function
     Integer, Save :: xmax_lut ! First dimension for spatial distortion
!      look up table arrays
     Integer, Save :: xmax_peaks ! First Dimension size for 2-D peak arrays
     Integer, Save :: xmaxknots ! Maximum number of knot points in
!      X-direction
     Integer, Save :: xnum_lut ! Number of pixels to define in LUT in
!      the X-direction
     Integer, Save :: xnum_peaks = 21 ! Number of peaks found in
!      X-direction of grid
     Integer, Save :: y_rows = 40 ! The absolute value is the second
!      dimension of temporary arrays, number of rows of the array for which the
!      distortions are calculated in one go. The sign determines
!      the type of re-binning algorithm which is applied: positive
!      means the centre of the edges of each distorted pixel are
!      calculated and over-lapped areas are approximated by
!      orthogonally aligned edges.
!      Negative means the corner positions will be calculated and
!      straight lines are "drawn" between the corners to provide
!      continuous quadlaterals. The intensity is re-binned
!      according to the over-lapped areas of these polygons.
     Integer, Save :: y_xnumknots ! Number of X-axis "knot" positions for
!      Y-distortion spline function
     Integer, Save :: y_ynumknots ! Number of Y-axis "knot" positions for
!      Y-distortion spline function
     Integer, Save :: ymax_lut ! Second dimension for spatial distortion
!      look up table arrays
     Integer, Save :: ymax_peaks ! Second Dimension size for 2-D peak
!      arrays
     Integer, Save :: ymaxknots ! Maximum number of knot points in
!      Y-direction
     Integer, Save :: ynum_lut ! Number of pixels to define in LUT in
!      the Y-direction
     Integer, Save :: ynum_peaks = 21 ! Number of peaks found in
!      Y-direction of grid
     Logical, Save :: arrays_exist = .False. ! .True., if dynamic arrays exist
     Logical :: continue ! .True. until user wants to exit menu
     Logical, Save :: distorted_space = .True. ! .True., if the distortion
!      measurements are calculated in the distorted space (default), i.e. the
!      distortion values are those to go from the distorted pixel
!      values to ideal pixel grid , measured in the distorted pixel
!      grid. Otherwise the values are those to go from an ideal pixel
!      grid to the distorted position, measured in the ideal pixel grid
     Logical :: distorted_spline ! .True., if the spline function was
!      calculated with "distorted_space" .True.
     Logical :: destroy_arrays ! .True., if dynamic arrays are to be
!      destroyed to recuperate the memory
     Logical, Save :: first = .True. ! .True., means the subroutine has not
!      yet been called and some dynamic arrays have not been allocated
     Logical :: flat_field_defined ! .True., if the flat-field response,
!      has been stored in the distortion file
     Logical, Save :: peaks_exist = .False. ! .True., if peak centres have
!      been calculated
     Logical :: reset ! .True., if "status" variable is to be reset
     Logical :: resize ! .True., if the graphics output window has been
!      re-sized
     Logical, Save :: dc_lut_defined ! .True., if the distortion correction
!      look-up table values are properly defined
     Logical, Save :: dc_lut_exists = .False. ! .True., if the distortion
!      correction look-up table arrays exist
     Logical, Save :: spatial_exist = .False. ! .True., if a spatial
!      calibration function has been input or calculated
     Real, Save :: closeness = 0.1 ! Required maximum average discrepancy
!      between calculated peak positions and fitted function
     Real, Save :: cor_grid_spacing ! Grid spacing for correction function
     Real, Save :: grid_spacing = 2000.0 ! Distance in microns from centre
!      to centre of grid holes
     Real, Save :: overload_value = 65534.5 ! Value above which pixels are
!      considered to be over-loaded
     Real, Save :: x_cor_size ! Size of corrected pixel in metres in
!      X-direction
     Real, Save :: x_maximum ! Maximum X-value applicable to spline
!      interpolation
     Real, Save :: x_minimum ! Minimum X-value applicable to spline
!      interpolation
     Real, Save :: y_cor_size ! Size of corrected pixel in metres in
!      X-direction
     Real, Save :: y_maximum ! Maximum Y-value applicable to spline
!      interpolation
     Real, Save :: y_minimum ! Minimum Y-value applicable to spline
!      interpolation
!  Local Arrays:
     Character(Len = 35), Save :: MENU(Max_menu) ! Array containing menu
!      choices
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Text to explain menu
!      choices
     Character(Len = 80) :: MESSAGE(3) ! User messages
     Character(Len = 80) :: TEXT(6) ! Text to explain user input
     Byte, Allocatable, Save :: INT_REBINNED(:) ! Dynamic array "INT_REBINNED"
!      to store fraction of an input pixel rebinned into the 9 pixels which are 
!      the target pixel and the 8 nearest pixels with the exception of the upper
!      right pixel. Each fraction is stored as a byte, so change to unsigned, 
!      and divide by 256, to obtain the required fraction. Since the total must
!      equal 1.0, the fraction for the upper right pixel can be deduced. The 
!      order of the fractions is lower left, lower middle, lower right, left of 
!      target, target, right of target, upper left, and upper middle.
     Real, Allocatable, Save :: X_2DDISTORTION(:, :) ! Dynamic array for storing
!      the X-distortion values (true minus measured)
     Real, Allocatable, Save :: X_2DPEAKS(:, :) ! Dynamic array for storing the 
!      X-coordinates of grid mask peak centres
     Real, Allocatable, Save :: X_COEFFS(:) ! Dynamic array 
     Real, Allocatable, Save :: X_LAMBDA(:) ! Dynamic array 
     Real, Allocatable, Save :: X_MU(:) ! Dynamic array 
     Byte, Allocatable, Save :: X_SD(:) ! Dynamic array for storing the 
!      truncated spatial distortion in pixels in the X-direction. This is a 
!      byte array for each pixel.
     Real, Allocatable, Save :: Y_2DDISTORTION(:, :) ! Dynamic array for storing
!      the Y-distortion values (true minus measured)
     Real, Allocatable, Save :: Y_2DPEAKS(:, :) ! Dynamic array for storing the
!      Y-coordinates of grid mask peak centres
     Real, Allocatable, Save :: Y_COEFFS(:) ! Dynamic array 
     Real, Allocatable, Save :: Y_LAMBDA(:) ! Dynamic array 
     Real, Allocatable, Save :: Y_MU(:) ! Dynamic array 
     Byte, Allocatable, Save :: Y_SD(:) ! Dynamic array for storing the 
!      truncated spatial distortion in pixels in the Y-direction. This is a byte
!      array for each pixel
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 10) / '?', 'CALCULATE FITTED DISTORTION', &
       'DECAY CORRECTION', 'DESTROY GRID PEAKS', 'DISPLAY DISTORTION', 'EXIT', &
       'FALSE PEAK', 'FAST CORRECTION', 'FIND PEAKS', 'FIT GRID PEAKS' /
     Data (MENU(item), item = 11, 20) / 'FLAT-FIELD CORRECTION', 'HELP', &
       'INPUT SPATIAL FUNCTION', 'INVERSE DISTORTED/IDEAL', &
       'LEARN HOLE PROFILE', 'LINEARISE INTENSITIES', 'LOAD LOOK-UP TABLE', &
       'LOOK-UP TABLE (SPATIAL DISTORTION)', 'OUTPUT SPATIAL FUNCTION', &
       'PLATYPUS CORRECTION FILE' /
     Data (MENU(item), item = 21, 30) / 'QUIT', 'RE-CALCULATE DISTORTION', &
       'RESIDUALS OF FIT', 'SAVE PEAKS', 'SIZE (IMAGE DISPLAY)', &
       'SPATIAL CORRECTION', 'STORE LOOK-UP TABLE', 'TRANSFER DISTORTION', &
       'VIEW PEAKS', 'XRII FLAT-FIELD' /
!    :    'APPLY FLAT-FIELD COR. TO LUT: Correct Look-up table for FF',
     Data (MENUTXT(item), item = 1, 10) / '?: List of available commands', &
       'CALCULATE FITTED DISTORTION: output in memory (for X or Y)', &
       'DECAY CORRECTION: Correct intenisty decay (Molecular Dynamics)', &
       'DESTROY GRID PEAKS: Set "found" peaks to missing peaks', &
       'DISPLAY DISTORTION: Create 2-D image in memory and display', &
       'EXIT: Exit fit sub-menu', &
       'FALSE PEAK: Create false peak from existing peak positions', &
       'FAST CORRECTION: Using spatial distortion look-up table', &
       'FIND PEAKS: Measure centres of grid mask peaks', &
       'FIT GRID PEAKS: Fit interpolation function to distortion' /
     Data (MENUTXT(item), item = 11, 20) / &
       'FLAT-FIELD CORRECTION: Correct for source distribution, etc', &
       'HELP: User help text', &
       'INPUT SPATIAL FUNCTION: Recover from file distortion', &
       'INVERSE DISTORTED/IDEAL: definition of distortion mapping', &
       'LEARN HOLE PROFILE: Average sub-pixel hole profile', &
       'LINEARISE INTENSITIES: Correct intensities for non-linearity', &
       'LOAD LOOK-UP TABLE: Input distortions LUT from a file', &
       'LOOK-UP TABLE (SPATIAL DISTORTION): Create look-up table', &
       'OUTPUT SPATIAL FUNCTION: Save current distortion function', &
       'PLATYPUS CORRECTION FILE: Create PLATYPUS calibration file' /
     Data (MENUTXT(item), item = 21, 30) / 'QUIT: Exit fit sub-menu', &
       'RE-CALCULATE DISTORTION: More user choice on distortion', &
       'RESIDUALS OF FIT: Fit minus measured distortion (in memory)', &
       'SAVE PEAKS: Output peak position to ASCII file', &
       'SIZE (IMAGE DISPLAY): Change region size for "FIND PEAKS"', &
       'SPATIAL CORRECTION: Apply spatial correction function', &
       'STORE LOOK-UP TABLE: Save distortions LUT in a file', &
       'TRANSFER DISTORTION: Save distortion values in memory', &
       'VIEW PEAKS: Examine peak positions and distortions', &
       'XRII FLAT-FIELD: Theoretical flat-field calculation' /
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_CALIBRATION ' // Version)
        Return
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_CALIBRATION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check that the subroutine argument variables are reasonably
!  defined
     If (xmaxdat .Lt. 1) Then
        status = St_bad_dim1
     Else If (ymaxdat .Lt. 1) Then
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
        status = St_mod_fit2d + status
 
!     Save details of subroutine call
        Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
 
        Return
 
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Set appropriate default values for input format
     If (first) Then
 
!     Get input format from data-base
        Call IO_INQ_KEYVALUE ('#INPUT_FORMAT', len_string, file_format, &
          db_stat, status)
 
        If (db_stat .Ne. 0) Then
           file_format = 'None'
        End If
 
        If (file_format .Eq. 'IMAGEQUANT' .Or. file_format .Eq. &
          'HAMAMATSU PHOTONICS') Then
 
!        Normally 4mm grid is used
           grid_spacing = 4000.0
 
!        Display centre 400 square
           num_display = 400
 
        Else
 
!        Assume Be XRII 5mm grid
           grid_spacing = 5000.0
 
!        Display more owing to bigger spacing / bigger holes and beam-stop
           num_display = 600
 
        End If
 
        first = .False.
 
     End If
 
!  If arrays are not presently created, create dynamic arrays to hold spatial 
!  distortion correction spline coefficients
     If (.Not. arrays_exist) Then
 
!     Get dynamic memory for peak coordinate arrays
        xmax_peaks = 400
        ymax_peaks = 400
        Allocate (X_2DPEAKS(xmax_peaks, ymax_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
           Return
        End If
        Allocate (Y_2DPEAKS(xmax_peaks, ymax_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
           Return
        End If
        Allocate (X_2DDISTORTION(xmax_peaks, ymax_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
           Return
        End If
        Allocate (Y_2DDISTORTION(xmax_peaks, ymax_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
           Return
        End If
 
        xmaxknots = 200
        ymaxknots = 200
        Allocate (X_LAMBDA(xmaxknots), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
           Return
        End If
        Allocate (Y_LAMBDA(xmaxknots), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
           Return
        End If
        Allocate (X_MU(ymaxknots), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
           Return
        End If
        Allocate (Y_MU(ymaxknots), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
           Return
        End If
        Allocate (X_COEFFS((xmaxknots - 4) * (ymaxknots - 4)), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
           Return
        End If
        Allocate (Y_COEFFS((xmaxknots - 4) * (ymaxknots - 4)), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
           Return
        End If

        arrays_exist = .True.
 
     End If
 
     num_menu = Max_menu ! Number of instructions available in the menu
     If (dc_lut_defined) Then
        command = 'FAST CORRECTION'
     Else If (spatial_exist) Then
        command = 'SPATIAL CORRECTION'
     Else
        command = 'FIND PEAKS'
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Start command input/action loop until EXIT requested
     continue = .True.
     Do While (continue)
 
!     Get user to select between the available menu options
        Call IO_MENU (.True., 'Calibration sub-menu: ENTER COMMAND', Max_menu, &
          MENUTXT, 1, 'Enter one of available commands ("HELP" for help)', &
          Max_menu, num_menu, MENU, command, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Command = '', a)') Trim(command)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Check for re-sizing of graphics window
        Call LG_INP_RESIZE (Gs_wkid_terminal, resize, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     EXECUTE MENU CHOICES
 
!     Call subroutine to perform requested operation.
        If (command .Eq. '?') Then
 
!        Output list of available commands
           Call IO_TEXT (num_menu, MENUTXT, status)
           command = 'HELP'
 
        Else If (command .Eq. 'APPLY FLAT-FIELD COR. TO LUT') Then
 
           If (dc_lut_defined) Then
 
              Call IO_WRITE ('NOTE: The current data array ' // &
                'must contain a normalised', status)
              Call IO_WRITE ('      "flat-field" response ' // 'image.', &
                status)
 
!           Multiply output fractions by flat-field response
              Call F2D_FFDCLUTC (xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, &
                xmax_lut, ymax_lut, xnum_lut, ynum_lut, X_SD, &
                Y_SD, INT_REBINNED, retstat)
 
              command = 'STORE LOOK-UP TABLE'
 
!           Check return status
              If (retstat .Eq. 1) Then
 
                 Call IO_WRITE ('WARNING: The flat-field ' // &
                   'array and the distotion correction', status)
                 Call IO_WRITE ('         look-up table are ' // &
                   'of different sizes. Operation has', status)
                 Call IO_WRITE ('         NOT BEEN PERFORMED.', status)
                 command = 'EXIT'
 
              Else If (retstat .Eq. -1) Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
                 Call IO_WRITE ('WARNING: The "flat-' // &
                   'fielding" operation lead to values which', status)
                 Call IO_WRITE ('         were too large ' // &
                   'to be stored in the elements of the', status)
                 Call IO_WRITE ('         distortion ' // &
                   'correction look-up table.', status)
              End If
 
           Else
 
!           No spatial correction function has been defined
              Call IO_WRITE ('WARNING: No spatial ' // &
                'distortion look-up table is defined, you must ' // 'use', &
                status)
              Call IO_WRITE ('         "LOOK-UP TABLE' // &
                '(SPATIAL DISTORTION)"', status)
              command = 'LOOK-UP TABLE (SPATIAL DISTORTION)'
 
           End If
 
        Else If (command .Eq. 'CALCULATE FITTED DISTORTION') Then
 
           If (spatial_exist .And. memory_exist) Then
 
!           Calculate fitted spatial distortion function in memory
              Call F2D_CAL_DISTORTION (x_minimum, y_minimum, &
                x_maximum, y_maximum, x_xnumknots, x_ynumknots, &
                X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, &
                y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, &
                xmaxdat, ymaxdat, XAXIS, YAXIS, xstrelm, ystrelm, xendelm, &
                yendelm, MXAXIS, MYAXIS, MDATA, mxstrelm, mystrelm, mxendelm, &
                myendelm, mtitle, status)
              mxnumdat = Max(mxnumdat, mxendelm)
              mynumdat = Max(mynumdat, myendelm)
              memory_defined = .True.
              mxlabel = xlabel
              mylabel = ylabel
              mzlabel = zlabel
              command = 'EXIT'
 
           Else If (spatial_exist) Then
 
!           No memory, issue message
              Call IO_WRITE ( 'WARNING: Memory arrays do not exist '// &
                'you must create them with "DIMENSIONS"', status)
              command = 'EXIT'
 
           Else
 
!           No spatial correction function has been defined
              Call IO_WRITE ('WARNING: No spatial ' // &
                'calibration function exists present, you must ' // &
                '"FIT GRID PEAKS"', status)
              Call IO_WRITE ('WARNING: to create a function.', status)
              command = 'FIT GRID PEAKS'
           End If
 
        Else If (command .Eq. 'DECAY CORRECTION') Then
 
           Call F2D_DECAY (xmaxdat, ymaxdat, ynumdat, xstrelm, ystrelm, &
             xendelm, yendelm, variances_exist, DATA, VARIANCES, status)
           command = 'EXIT'
 
        Else If (command .Eq. 'DESTROY GRID PEAKS') Then
 
           If (peaks_exist) Then
 
!           Allow user to interactively define peaks as missing
              Call F2D_DESTROYPEAKS (xmax_peaks, ymax_peaks, xnum_peaks, &
                ynum_peaks, X_2DPEAKS, Y_2DPEAKS, &
                X_2DDISTORTION, Y_2DDISTORTION, status)
              command = 'DISPLAY DISTORTION'
 
           Else
 
!           No peaks have been found
              Call IO_WRITE ('WARNING: No peak centres ' // &
                'have been found you must "FIND PEAKS"', status)
              command = 'FIND PEAKS'
 
           End If
 
        Else If (command .Eq. 'DISPLAY DISTORTION') Then
 
!        Create 2-d distortion image in memory and display
           If (peaks_exist) Then
 
!           Output peak positions to an ASCII file
              Call F2D_DISPLAY2DDISTORTION (xmax_peaks, ymax_peaks, &
                xnum_peaks, ynum_peaks, X_2DDISTORTION, Y_2DDISTORTION, status)
 
              command = 'EXIT'
 
           Else
 
!           No peaks have been found
              Call IO_WRITE ('WARNING: No peak centres ' // &
                'have been found you must "FIND PEAKS"', status)
              command = 'FIND PEAKS'
 
           End If
 
        Else If (command .Eq. 'EXIT' .Or. command .Eq. 'QUIT') Then
 
!        EXIT: Return to main menu
           continue = .False.
 
        Else If (command .Eq. 'FALSE PEAK') Then
 
           Call F2D_FALSEPEAK (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, DATA, status)
 
        Else If (command .Eq. 'FAST CORRECTION') Then
 
           If (dc_lut_defined .And. memory_exist) Then
 
!           Apply fast spatial distortion correction to data
              Call F2D_CORR_FAST (.False., x_cor_size, y_cor_size, xmax_lut, &
                ymax_lut, xnum_lut, ynum_lut, X_SD, Y_SD, &
                INT_REBINNED, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, &
                xstrelm, ystrelm, xendelm, yendelm, overload_value, MXAXIS, &
                MYAXIS, MDATA, mxstrelm, mystrelm, mxendelm, myendelm, status)
              mxnumdat = Max(mxnumdat, mxendelm)
              mynumdat = Max(mynumdat, myendelm)
              memory_defined = .True.
              mtitle = title
              mxlabel = xlabel
              mylabel = ylabel
              mzlabel = zlabel
              mx_pixel_size = x_cor_size
              my_pixel_size = y_cor_size
              command = 'EXIT'
 
           Else If (dc_lut_defined) Then
 
!           No memory, issue message
              Call IO_WRITE ( 'WARNING: Memory arrays do not exist ' // &
                'you must create them with "DIMENSIONS"', status)
              command = 'EXIT'
 
           Else
 
!           No spatial correction function has been defined
              Call IO_WRITE ('WARNING: No spatial ' // &
                'distortion look-up table is defined, you must ' // 'use', &
                status)
              Call IO_WRITE ('         "LOOK-UP TABLE ' // &
                '(SPATIAL DISTORTION)" or ', status)
              Call IO_WRITE ('         "LOAD LOOK-UP TABLE"', status)
              command = 'LOOK-UP TABLE (SPATIAL DISTORTION)'
           End If
 
        Else If (command .Eq. 'FIND PEAKS') Then
 
!        Calculate centres of grid peaks for spatial calibration
           Call F2D_CALIBRATE2DGRID (xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, DATA, XAXIS, YAXIS, title, xlabel, ylabel, &
             zlabel, num_display, xmax_peaks, ymax_peaks, grid_spacing, &
             x_pixel_size, y_pixel_size, retstat, num_peaks, xnum_peaks, &
             ynum_peaks, X_2DPEAKS, Y_2DPEAKS, &
             X_2DDISTORTION, Y_2DDISTORTION, status)
 
!        Set flags and next command
           If (retstat .Eq. 0) Then
              peaks_exist = .True.
              command = 'DISPLAY DISTORTION'
           End If
 
        Else If (command .Eq. 'FIT GRID PEAKS') Then
 
           If (peaks_exist) Then
 
!           Input required average accuracy of interpolating function
              TEXT(1) = 'Enter required maximum AVERAGE discrepancy ' // &
                'between the calculated peak'
              TEXT(2) = 'distortions and the values of the spline ' // &
                'function at the corresponding'
              TEXT(3) = 'positions. Note this is the AVERAGE value ' // &
                'and does not mean the the'
              TEXT(4) = 'function is better fitted for some peaks ' // &
                'and worse for others. A value'
              TEXT(5) = 'around the estimated error in calculated ' // &
                'peak distortion should be'
              TEXT(6) = 'about right.'
              Call IO_INPR (.True., 0.00001, 1.0e3, .True., &
                'AVERAGE FIT DISCREPANCY (PIXELS)', 6, TEXT, 1, &
                'Value must be within given range', closeness, status)
 
!           Define extent of spline function
              x_minimum = XAXIS(xstrelm) - 0.5
              y_minimum = YAXIS(ystrelm) - 0.5
              x_maximum = XAXIS(xendelm) + 0.5
              y_maximum = YAXIS(yendelm) + 0.5
 
!           Allow user to fit grid with spline functions
              Call F2D_FIT2DGRID (xmax_peaks, ymax_peaks, xnum_peaks, &
                ynum_peaks, closeness, x_minimum, y_minimum, x_maximum, &
                y_maximum, xmaxknots, ymaxknots, X_2DPEAKS, Y_2DPEAKS, &
                X_2DDISTORTION, Y_2DDISTORTION, spatial_exist, x_xnumknots, &
                x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
                y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
 
!           Set size of corrected pixels
              cor_grid_spacing = grid_spacing
              x_cor_size = x_pixel_size
              y_cor_size = y_pixel_size
              distorted_spline = distorted_space
 
              If (spatial_exist) Then
 
!              Set flags and next command
                 command = 'OUTPUT SPATIAL FUNCTION'
              End If
 
           Else
 
!           No peaks have been found
              Call IO_WRITE ('WARNING: No peak centres ' // &
                'have been found you must "FIND PEAKS"', status)
              command = 'FIND PEAKS'
 
           End If
 
        Else If (command .Eq. 'FLAT-FIELD CORRECTION') Then
 
!        Correct flat field measurement for anisotropic angular intensity 
!        distribution
           Call F2D_FLATFIELD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, XAXIS, YAXIS, x_pixel_size, y_pixel_size, DATA, status)
           command = 'EXIT'
 
        Else If (command .Eq. 'HELP') Then
 
!        Output user help text
           Call F2D_HELP_CALIBRATE (status)
           command = '?'
 
        Else If (command .Eq. 'INVERSE DISTORTED/IDEAL') Then
 
           If (peaks_exist) Then
 
!           Inverse definition of distortion, from measured to ideal and vice 
!           versa (note this subroutine is self inversing)
              Call F2D_IDEALSPACE (xmax_peaks, ymax_peaks, xnum_peaks, &
                ynum_peaks, X_2DPEAKS, Y_2DPEAKS, &
                X_2DDISTORTION, Y_2DDISTORTION, status)
              distorted_space = .Not. distorted_space
 
!           Remind user which way round the distortion mapping is defined
              If (distorted_space) Then
                 Call IO_WRITE ('INFO: Distortion now ' // &
                   'defined from distorted grid to ideal grid', status)
              Else
                 Call IO_WRITE ('INFO: Distortion now ' // &
                   'defined from ideal grid to distorted grid', status)
              End If
              command = 'FIT GRID PEAKS'
 
           Else
 
!           No peaks have been found
              Call IO_WRITE ('WARNING: No peak centres ' // &
                'have been found you must "FIND PEAKS"', status)
              command = 'FIND PEAKS'
 
           End If
 
        Else If (command .Eq. 'INPUT SPATIAL FUNCTION') Then
 
           dc_lut_defined = .False.
 
!        Input values of spline coefficients from file
           Call F2D_IN_SPATIAL (.False., xmaxknots, ymaxknots, spline_file, &
             retstat, x_minimum, y_minimum, x_maximum, y_maximum, &
             cor_grid_spacing, x_cor_size, y_cor_size, x_xnumknots, &
             x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
             y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
 
!        Set flags and next command
           If (retstat .Eq. 0) Then
              spatial_exist = .True.
              command = 'SPATIAL CORRECTION'
           Else
              Call IO_WRITE ('WARNING: Interpolation function ' // &
                'coefficients have not been input', status)
           End If
 
        Else If (command .Eq. 'LINEARISE INTENSITIES') Then
 
!        Correct intensities to a linear scale
           Call F2D_CLINEARISE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, DATA, status)
           command = 'EXIT'
 
        Else If (command .Eq. 'LEARN HOLE PROFILE') Then
 
           If (peaks_exist .And. memory_exist) Then
 
              Call F2D_LEARNPROFILE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                xendelm, yendelm, XAXIS, YAXIS, DATA, xmax_peaks, ymax_peaks, &
                xnum_peaks, ynum_peaks, X_2DPEAKS, Y_2DPEAKS, &
                mxnumdat, mynumdat, MXAXIS, MYAXIS, MDATA, mxstrelm, mystrelm, &
                mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                memory_defined, status)
 
!           Set flags and next command
              command = 'EXIT'
 
           Else If (peaks_exist) Then
 
!           No memory, issue message
              Call IO_WRITE ( 'WARNING: Memory arrays do not exist '// &
                'you must create them with "DIMENSIONS"', status)
              command = 'EXIT'
 
           Else
 
!           No peaks have been found
              Call IO_WRITE ('WARNING: No peak centres ' // &
                'have been found you must "FIND PEAKS"', status)
              command = 'FIND PEAKS'
 
           End If
 
        Else If (command .Eq. 'LOAD LOOK-UP TABLE') Then
 
!        Create arrays if necessary
           If (.Not. dc_lut_exists) Then
 
              xmax_lut = xmaxdat
              ymax_lut = ymaxdat
              Allocate (X_SD(xmax_lut * ymax_lut), Stat = stat)
              If (stat .Ne. 0) Then
                 status = St_mod_fit2d + St_bad_malloc
                 Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
                 Return
              End If
              Allocate (Y_SD(xmax_lut * ymax_lut), Stat = stat)
              If (stat .Ne. 0) Then
                 status = St_mod_fit2d + St_bad_malloc
                 Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
                 Return
              End If
              Allocate (INT_REBINNED(xmax_lut * ymax_lut * 9), Stat = stat)
              If (stat .Ne. 0) Then
                 status = St_mod_fit2d + St_bad_malloc
                 Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
                 Return
              End If

              dc_lut_exists = .True.
 
           End If
 
           If (dc_lut_exists) Then
 
!           Input distortion correction look-up table from file
              Call F2D_INP_DCLUT (.False., input_file, retstat, x_cor_size, &
                y_cor_size, xmax_lut, ymax_lut, xnum_lut, ynum_lut, &
                X_SD, Y_SD, INT_REBINNED, xmaxdat, &
                ymaxdat, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
                DATA, title, dc_lut_defined, flat_field_defined, status)
 
!           Set next command
              command = 'FAST CORRECTION'
           Else
              command = 'EXIT'
           End If
 
        Else If (command .Eq. 'LOOK-UP TABLE (SPATIAL DISTORTION)') Then
 
           If (spatial_exist) Then
 
!           Create arrays if they necessary
              If (.Not. dc_lut_exists) Then
 
                 xmax_lut = Min(xmaxdat, Int(x_maximum - 0.00001) + 1)
                 ymax_lut = Min(ymaxdat, Int(y_maximum - 0.00001) + 1)
                 xnum_lut = xmax_lut
                 ynum_lut = ymax_lut
                 Allocate (X_SD(xmax_lut * ymax_lut), Stat = stat)
                 If (stat .Ne. 0) Then
                    status = St_mod_fit2d + St_bad_malloc
                    Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
                    Return
                 End If
                 Allocate (Y_SD(xmax_lut * ymax_lut), Stat = stat)
                 If (stat .Ne. 0) Then
                    status = St_mod_fit2d + St_bad_malloc
                    Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
                    Return
                 End If
                 Allocate (INT_REBINNED(xmax_lut * ymax_lut * 9), Stat = stat)
                 If (stat .Ne. 0) Then
                    status = St_mod_fit2d + St_bad_malloc
                    Call ST_SAVE ('Subroutine F2D_CALIBRATION ' // Version)
                    Return
                 End If

                 dc_lut_exists = .True.
 
              End If
 
!           Create Spatial distortion look-up table
              dc_lut_defined = .False.
              Call F2D_SDLUT ( x_minimum, y_minimum, x_maximum, y_maximum, &
                x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
                y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, &
                xmaxdat, ymaxdat, XAXIS, YAXIS, xmax_lut, ymax_lut, &
                xnum_lut, ynum_lut, X_SD, Y_SD, INT_REBINNED, dc_lut_defined, &
                status)
 
           Else
 
!           No spatial correction function has been defined
              Call IO_WRITE ('WARNING: No spatial calibration function ' // &
                'exists present, you must "FIT GRID"', status)
              Call IO_WRITE ('         to create a function.', status)
              command = 'FIT GRID PEAKS'
 
           End If
 
!        Set flags and next command
           command = 'EXIT'
 
        Else If (command .Eq. 'OUTPUT SPATIAL FUNCTION') Then
 
           If (spatial_exist) Then
 
!           Write values of spline coefficients to file
              Call F2D_OUT_SPATIAL (distorted_spline, x_minimum, y_minimum, &
                x_maximum, y_maximum, cor_grid_spacing, x_cor_size, &
                y_cor_size, xmaxknots, ymaxknots, x_xnumknots, x_ynumknots, &
                X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, &
                y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
 
           Else
 
!           No spatial correction function has been defined
              Call IO_WRITE ('WARNING: No spatial calibration function ' // &
                'exists present, you must "FIT GRID"', status)
              Call IO_WRITE ('         to create a function.', status)
              command = 'FIT GRID PEAKS'
 
           End If
 
!        Set flags and next command
           command = 'EXIT'
 
        Else If (command .Eq. 'PLATYPUS CORRECTION FILE') Then
 
           Call IO_WRITE ('WARNING: This option has been ' // &
             'temporarily removed. Please request', status)
           Call IO_WRITE ('         that it be re-instated ' // &
             'if you need it (e-mail to', status)
           Call IO_WRITE ('         hammersley@esrf.fr)', status)
 
!        If (spatial_exist) Then
 
!        Create spatial distortion correction file for PLATYPUS
!        Call F2D_PLATYPUS (xmaxdat, ymaxdat,
!        :             xendelm, yendelm, XAXIS, YAXIS,
!        :             x_minimum, y_minimum, x_maximum, y_maximum,
!        :             x_cor_size, y_cor_size,
!        :             x_xnumknots, x_ynumknots,
!        :             X_LAMBDA, X_MU, X_COEFFS,
!        :             y_xnumknots, y_ynumknots,
!        :             Y_LAMBDA, Y_MU, Y_COEFFS,
!        :             status)
!        command = 'EXIT'
 
!        Else
 
!        No spatial correction function has been defined
!        Call IO_WRITE ('WARNING: No spatial ' //
!        :             'calibration function exists present, you must ' //
!        :             '"FIT GRID"', status)
!        Call IO_WRITE ('         to create a function.',
!        :             status)
!        command = 'FIT GRID PEAKS'
!        End If
 
        Else If (command .Eq. 'RE-CALCULATE DISTORTION') Then
 
           If (peaks_exist) Then
 
!           Enter spacing between each grid hole
              Call IO_INPR (.True., 1.0, 1.0e6, .True., &
                'GRID SPACING (CENTRE TO CENTRE IN microns)', 1, &
                'Enter distance between adjacent peaks in microns', 1, &
                'Value must be within given range', grid_spacing, status)
 
              Call F2D_2DDISTORTION (xmax_peaks, ymax_peaks, xnum_peaks, &
                ynum_peaks, X_2DPEAKS, Y_2DPEAKS, grid_spacing, &
                x_pixel_size, y_pixel_size, X_2DDISTORTION, &
                Y_2DDISTORTION, status)
              command = 'EXIT'
 
           Else
 
!           No peaks have been found
              Call IO_WRITE ('WARNING: No peak centres ' // &
                'have been found you must "FIND PEAKS"', status)
              command = 'FIND PEAKS'
 
           End If
 
        Else If (command .Eq. 'RESIDUALS OF FIT') Then
 
           If (spatial_exist .And. memory_exist) Then
 
!           Calculate residuals between fitted spatial distortion function and
!           measured values in memory
              Call F2D_RESIDUALS ( xmax_peaks, ymax_peaks, xnum_peaks, &
                ynum_peaks, X_2DPEAKS, Y_2DPEAKS, &
                X_2DDISTORTION, Y_2DDISTORTION, x_xnumknots, &
                x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
                y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, &
                Y_COEFFS, xmaxdat, ymaxdat, MXAXIS, MYAXIS, MDATA, &
                mxstrelm, mystrelm, mxendelm, myendelm, mtitle, status)
              mxnumdat = mxendelm
              mynumdat = myendelm
              memory_defined = .True.
              mxlabel = xlabel
              mylabel = ylabel
              mzlabel = zlabel
              command = 'EXIT'
 
           Else If (spatial_exist) Then
 
!           No memory, issue message
              Call IO_WRITE ( 'WARNING: Memory arrays do not exist ' // &
                'you must create them with "DIMENSIONS"', status)
              command = 'EXIT'
 
           Else
 
!           No spatial correction function has been defined
              Call IO_WRITE ('WARNING: No spatial calibration function ' // &
                'exists present, you must "FIT GRID"', status)
              Call IO_WRITE ('         to create a function.', status)
              command = 'FIT GRID PEAKS'
           End If
 
        Else If (command .Eq. 'SAVE PEAKS') Then
 
           If (peaks_exist) Then
 
!           Output peak positions to an ASCII file
              Call F2D_SAVE2DPEAKS ( xmax_peaks, ymax_peaks, xnum_peaks, &
                ynum_peaks, X_2DPEAKS, Y_2DPEAKS, &
                X_2DDISTORTION, Y_2DDISTORTION, status)
 
!           Set next command
              command = 'EXIT'
 
           Else
 
!           No peaks have been found
              Call IO_WRITE ('WARNING: No peak centres ' // &
                'have been found you must "FIND PEAKS"', status)
              command = 'FIND PEAKS'
 
           End If
 
        Else If (command .Eq. 'SIZE (IMAGE DISPLAY)') Then
 
!        Number of pixels to be displayed
           Call IO_INPI (.True., 50, 5000, .True., 'NUMBER OF PIXELS', 1, &
             'Enter number of pixels to be displayed in '// &
             '"FIND PEAKS" (each dimension)', 1, &
             'Enter integer with given range', num_display, status)
           command = 'FIND PEAKS'
 
        Else If (command .Eq. 'SPATIAL CORRECTION') Then
 
           If (spatial_exist .And. memory_exist) Then
 
!           Apply spatial distortion correction functions to data
              Call F2D_CORR_SPATIAL (.False., .False., x_minimum, y_minimum, &
                x_maximum, y_maximum, x_cor_size, y_cor_size, x_xnumknots, &
                x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
                y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, &
                Y_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, &
                xstrelm, ystrelm, xendelm, yendelm, y_rows, overload_value, &
                MXAXIS, MYAXIS, MDATA, mxstrelm, mystrelm, mxendelm, myendelm, &
                status)
              mxnumdat = Max(mxnumdat, mxendelm)
              mynumdat = Max(mynumdat, myendelm)
              memory_defined = .True.
              mtitle = title
              mxlabel = xlabel
              mylabel = ylabel
              mzlabel = zlabel
              mx_pixel_size = x_cor_size
              my_pixel_size = y_cor_size
              command = 'EXIT'
 
           Else If (spatial_exist) Then
 
!           No memory, issue message
              Call IO_WRITE ( 'WARNING: Memory arrays do not exist ' // &
                'you must create them with "DIMENSIONS"', status)
              command = 'EXIT'
 
           Else
 
!           No spatial correction function has been defined
              Call IO_WRITE ('WARNING: No spatial calibration function ' // &
                'exists present, you must "FIT GRID"', status)
              Call IO_WRITE ('         to create a function.', status)
              command = 'FIT GRID PEAKS'
           End If
 
        Else If (command .Eq. 'STORE LOOK-UP TABLE') Then
 
           If (dc_lut_defined) Then
 
!           Store distortion correction look-up table and flat-field response 
!           in a file
              Call F2D_OUT_DCLUT (.False., input_file, x_cor_size, y_cor_size, &
                xmax_lut, ymax_lut, xnum_lut, ynum_lut, X_SD, &
                Y_SD, INT_REBINNED, xmaxdat, ymaxdat, xnumdat, &
                ynumdat, DATA, retstat, status)
 
              command = 'EXIT'
 
           Else
 
!           No spatial correction function has been defined
              Call IO_WRITE ('WARNING: No spatial distortion look-up ' // &
                'table is defined, you must use', status)
              Call IO_WRITE ('         "LOOK-UP TABLE (SPATIAL DISTORTION)"', &
                status)
              command = 'LOOK-UP TABLE (SPATIAL DISTORTION)'
 
           End If
 
        Else If (command .Eq. 'TRANSFER DISTORTION') Then
 
!        Allow user to store either X or Y-direction distortions in memory
           If (peaks_exist .And. memory_exist) Then
 
              Call F2D_DIST2MEM (xmax_peaks, ymax_peaks, xnum_peaks, &
                ynum_peaks, X_2DDISTORTION, Y_2DDISTORTION, &
                xmaxdat, ymaxdat, mxnumdat, mynumdat, MXAXIS, MYAXIS, MDATA, &
                mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, &
                mylabel, mzlabel, status)
              memory_defined = .True.
              command = 'EXIT'
 
           Else If (peaks_exist) Then
 
!           No memory, issue message
              Call IO_WRITE ( 'WARNING: Memory arrays do not exist ' // &
                'you must create them with "DIMENSIONS"', status)
              command = 'EXIT'
 
           Else
 
!           No peaks have been found
              Call IO_WRITE ('WARNING: No peak centres ' // &
                'have been found you must "FIND PEAKS"', status)
              command = 'FIND PEAKS'
 
           End If
 
        Else If (command .Eq. 'VIEW PEAKS') Then
 
!        Allow user to view results of peak searching and estimated distortions
           If (peaks_exist) Then
              Call F2D_VIEW2DPEAKS (xmax_peaks, ymax_peaks, xnum_peaks, &
                ynum_peaks, X_2DPEAKS, Y_2DPEAKS, &
                X_2DDISTORTION, Y_2DDISTORTION, status)
              command = 'EXIT'
 
           Else
 
!           No peaks have been found
              Call IO_WRITE ('WARNING: No peak centres ' // &
                'have been found you must "FIND PEAKS"', status)
              command = 'FIND PEAKS'
 
           End If
 
        Else If (command .Eq. 'XRII FLAT-FIELD') Then
 
!        Apply thoeretical flat-field correction based on geometric path lengths
!        through the different vacuum and detection layers
           Call F2D_XRIIFLATFIELD (xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, x_pixel_size, y_pixel_size, DATA, status)
           command = 'EXIT'
 
!        Else If (command .Eq. 'Z CALCULATE') Then
 
!        If (spatial_exist .And. memory_exist) Then
 
!        Calculate fitted spatial distortion function in memory
!        Call F2D_CAL_DISTORTION (.True.,
!        :             x_minimum, y_minimum, x_maximum, y_maximum,
!        :             x_xnumknots, x_ynumknots,
!        :             X_LAMBDA, X_MU, X_COEFFS,
!        :             y_xnumknots, y_ynumknots,
!        :             Y_LAMBDA, Y_MU, Y_COEFFS,
!        :             xmaxdat, ymaxdat,
!        :             XAXIS, YAXIS,
!        :             xstrelm, ystrelm, xendelm, yendelm,
!        :             MXAXIS, MYAXIS, MDATA,
!        :             mxstrelm, mystrelm, mxendelm, myendelm, mtitle,
!        :             status)
!        mxnumdat = Max(mxnumdat, mxendelm)
!        mynumdat = Max(mynumdat, myendelm)
!        memory_defined = .True.
!        mxlabel = xlabel
!        mylabel = ylabel
!        mzlabel = zlabel
!        command = 'EXIT'
 
!        Else If (spatial_exist) Then
 
!        No memory, issue message
!        Call IO_WRITE (
!        :             'WARNING: Memory arrays do not exist '//
!        :            'you must create them with "DIMENSIONS"', status)
!        command = 'EXIT'
 
!        Else
 
!        No spatial correction function has been defined
!        Call IO_WRITE ('WARNING: No spatial ' //
!        :             'calibration function exists present, you must ' //
!        :             '"FIT GRID PEAKS"', status)
!        Call IO_WRITE ('WARNING: to create a function.',
!        :             status)
!        command = 'FIT GRID PEAKS'
!        End If
 
!        Else If (command .Eq. 'Z FIT GRID') Then
 
!        Temporary fitting using SURFIT
!        If (peaks_exist) Then
 
!        Input required average accuracy of interpolating function
!        TEXT(1) = 'Enter required maximum AVERAGE discrepancy '
!        :             // 'between the calculated peak'
!        TEXT(2) = 'distortions and the values of the spline '
!        :             // 'function at the corresponding'
!        TEXT(3) = 'positions. Note this is the AVERAGE value '
!        :             // 'and does not mean the the'
!        TEXT(4) = 'function is better fitted for some peaks '
!        :             // 'and worse for others. A value'
!        TEXT(5) = 'around the estimated error in calculated '
!        :             // 'peak distortion should be'
!        TEXT(6) = 'about right.'
!        Call IO_INPR (.True., 0.00001, 1.0e3, .True.,
!        :             'AVERAGE FIT DISCREPANCY (PIXELS)',
!        :             6, TEXT,
!        :             1, 'Value must be within given range',
!        :             closeness, status)
 
!        Define extent of spline function
!        x_minimum = XAXIS(xstrelm) - 0.5
!        y_minimum = YAXIS(ystrelm) - 0.5
!        x_maximum = XAXIS(xendelm) + 0.5
!        y_maximum = YAXIS(yendelm) + 0.5
 
!        Allow user to fit grid with spline functions
!        Call F2D_FIT2DGRID (xmax_peaks, ymax_peaks,
!        :             xnum_peaks, ynum_peaks,
!        :             closeness,
!        :             x_minimum, y_minimum, x_maximum, y_maximum,
!        :             xmaxknots, ymaxknots,
!        :             X_2DPEAKS), Y_2DPEAKS),
!        :             X_2DDISTORTION), Y_2DDISTORTION),
!        :             spatial_exist,
!        :             x_xnumknots, x_ynumknots, X_LAMBDA),
!        :             X_MU), X_COEFFS),
!        :             y_xnumknots, y_ynumknots, Y_LAMBDA),
!        :             Y_MU), Y_COEFFS),
!        :             status)
 
!        Set size of corrected pixels
!        cor_grid_spacing = grid_spacing
!        x_cor_size = x_pixel_size
!        y_cor_size = y_pixel_size
!        distorted_spline = distorted_space
 
!        Set flags and next command
!        command = 'OUTPUT SPATIAL FUNCTION'
 
!        Else
 
!        No peaks have been found
!        Call IO_WRITE ('WARNING: No peak centres ' //
!        :             'have been found you must ' //
!        :             '"FIND PEAKS"', status)
!        command = 'FIND PEAKS'
 
!        End If
 
        Else If (command .Eq. 'null') Then
 
!        Null command returned do nothing
           Continue
 
        Else
 
!        Unknown command
           Call IO_WRITE ( &
             'WARNING: Unknown command, please enter new command', status)
 
        End If
 
!     Output user message if error
        Call ST_OUT (status)
 
        If (status .Ne. St_goodvalue) Then
 
!        Use choice to EXIT or reset status
           reset = .True.
           temp_status = St_goodvalue
           Call IO_INPL (.True., 0, 1, .True., 'RESET "status"', 1, &
             'YES: to reset "status" value, other exit program', 1, &
             'Enter YES or NO', reset, temp_status)
 
           If (reset) Then
              Call ST_DEF_SYSTEM (status)
           Else
              continue = .False.
           End If
 
        End If
 
     End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Inquire if dynamic arrays are to be saved or destroyed
     destroy_arrays = .False.
     MESSAGE(1) = '"YES" to destroy peak position and fit '// &
       'function arrays, and recuperate'
     MESSAGE(2) = 'program memory. "NO" to save peak position '// &
       'and fit function arrays for'
     MESSAGE(3) = 'later re-use.'
     Call IO_INPL (.True., 0, 1, .True., 'DESTROY DYNAMIC ARRAYS', 3, MESSAGE, &
       1, 'Enter YES or NO', destroy_arrays, status)
 
     If (destroy_arrays) Then
 
!     Set flags
        arrays_exist = .False.
        peaks_exist = .False.
        spatial_exist = .False.
 
!     Free dynamic memory for peak coordinate and fit function arrays
        Deallocate (X_2DPEAKS)
        Deallocate (Y_2DPEAKS)
        Deallocate (X_2DDISTORTION)
        Deallocate (Y_2DDISTORTION)
        Deallocate (X_LAMBDA)
        Deallocate (Y_LAMBDA)
        Deallocate (X_MU)
        Deallocate (Y_MU)
        Deallocate (X_COEFFS)
        Deallocate (Y_COEFFS)
 
     End If
 
     End Subroutine F2D_CALIBRATION
!********1*********2*********3*********4*********5*********6*********7*********8
