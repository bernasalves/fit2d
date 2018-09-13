!********1*********2*********3*********4*********5*********6*********7**
 
!  ***************************
!  *                         *
!  * f2d_calibrate2dgrid.f90 *
!  *                         *
!  ***************************
 
!+ F2D_CALIBRATE2DGRID: CALIBRATES 2-D GRID peak centres
     Subroutine F2D_CALIBRATE2DGRID (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, &
       num_display, xmax_peaks, ymax_peaks, grid_spacing, x_pixel_size, &
       y_pixel_size, retstat, num_peaks, xnumpeaks, ynumpeaks, X_2DPEAKS, &
       Y_2DPEAKS, X_DISTORTION, Y_DISTORTION, status)
!  Description:
!    Displays part of image, inputs three starting peaks to define
!    grid vectors, and finds peaks in a 2-D grid
!  Keywords:
!    Calibrate.Grid, Mask~Grid.Calibration
!  Method:
!    The following tasks are undertaken:
!
!    1  Initialise grid: All X-positions are set to -1.7e38 to
!    indicate no peak has been found
!    2  Display section of image, maximum of 200*200 pixels centred
!    of centred of current ROI
!    3  Input graphically three peaks to define a start peak and
!    two vectors of the grid search
!    4  Use pixel search over smallish square centred on user
!    initial coordinate to find best pixel centre, followed by
!    sub-pixel search to find best centre
!    5  Define starting grid vectors
!    6  Store peak positions; Array element (1, 1) is used for the
!    starting peak, (2, 1) for the next horizontal peak, and (1, 2)
!    for the next vertical peak
!    7  Search to right edge of ROI, jumping over missing peaks
!    if necessary (See below)
!    8  Search to left edge of ROI, jumping over missing peaks if
!    necessary (See below). Peaks are wrapped to the opposite
!    side of the peak position array
!    9  Search to upper edge of ROI, jumping over missing peaks
!    if necessary (See below)
!    10 Search to lower edge of ROI, jumping over missing peaks if
!    necessary (See below). Peaks are wrapped to the opposite
!    side of the peak position array
!    11 For each quadrant find all remaining peaks, using predicted
!    position from the two peak vectors already available to the
!    sides of the peak. Storing maximum left/right up/down
!    numbers of peaks
!    12 When the grid search is complete, rotate (in place) the
!    peaks position arrays, so that the element (1, 1) contains
!    the lower left limit of the grid (even if the peak is
!    missing).
!
!    Missing Peaks:
!
!    In general a limited range sub-pixel search is used for each
!    peak, if the centre is not within the limits, or if the
!    cross-correlation value ratio from the new peak to the nearby
!    peaks is less than some limit the peak is declared to be
!    missing.
!
!    After a missing peak the search for the next peak begins
!    with a best pixel search in a limited region (as for the
!    initial three peaks). The starting position is based on the
!    closest found peaks. A sub-pixel search is then used to find the
!    best sub-pixel.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    31-Mar-2006: V0.16 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    23-Feb-1999: V0.15 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.14 Change to use IO internal database routines (Hammersley)
!    16-Dec-1996: V0.13 Avoid open strings crossing lines (Hammersley)
!    25-Apr-1996: V0.12 Increase default value of "detect_ratio" to
!      0.3 for XRII/CCD data (Hammersley)
!    17-Apr-1996: V0.11 Use format type to define default values (Hammersley)
!    04-Jan-1996: V0.10 Remove call to "Secnds" (Hammersley)
!    16-Aug-1995: V0.9 Increase user information (Hammersley)
!    12-Aug-1995: V0.8 Option to correct for mask vignetting (Hammersley)
!    25-Jan-1995: V0.7 Check for input of "user escape" and return immediately
!      (Hammersley)
!    19-Dec-1994: V0.6 Change definition of pixel sizes to metre units 
!      (Hammersley)
!    31-Oct-1994: V0.5 Spell "position" correctly in user messages (Hammersley)
!    24-Mar-1994: V0.4 Displayed image region size now an input argument 
!      (Hammersley)
!    04-Jan-1994: V0.3 Add return status variable to "F2D_FIND2DGRID" 
!       (Hammersley)
!    03-Nov-1993: V0.2 Add user control of detection ratio (Hammersley)
!    26-Oct-1993: V0.1 Original, based on "F2D_CALIBRATEGRID" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Integer, Intent(IN) :: num_display ! Number of pixels to display
     Integer, Intent(IN) :: xmax_peaks ! First Dimension size of "X_PEAKS"
!      and "Y_PEAKS"
     Integer, Intent(IN) :: ymax_peaks ! Second Dimension size of "X_PEAKS"
!      and "Y_PEAKS"
!  Import/Export:
     Real, Intent(INOUT) :: grid_spacing ! Distance in microns from centre
!      to centre of grid holes
     Real, Intent(INOUT) :: x_pixel_size ! Pixel size in metres in X-direction
     Real, Intent(INOUT) :: y_pixel_size ! Pixel size in metres in Y-direction
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status:
!      0 = Good status
!      1 = Bad status, grid too large for internal array
!      2 = Bad status, peaks missing from central cross
     Integer, Intent(OUT) :: num_peaks ! Number of found peaks
     Integer, Intent(OUT) :: xnumpeaks ! Number of peaks in X-direction of grid
     Integer, Intent(OUT) :: ynumpeaks ! Number of peaks in Y-direction of grid
     Real, Intent(OUT) :: X_2DPEAKS(xmax_peaks, ymax_peaks)
!      X-coordinates of peak centres
     Real, Intent(OUT) :: Y_2DPEAKS(xmax_peaks, ymax_peaks)
!      Y-coordinates of peak centres
     Real, Intent(OUT) :: X_DISTORTION(xmax_peaks, ymax_peaks)
!      X-distortion at each peak (The distortion is measured as true position 
!      minusmeasured position)
     Real, Intent(OUT) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
!      Y-distortion at each peak (The distortion is measured as true position 
!      minusmeasured position)
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.16' ! Version number
!  Local Variables:
     Character(Len = 80) :: file_format ! Choice of file format to input
     Integer :: db_stat ! Data Store status return variable
     Integer :: len_string ! Number of defined characters in a string
     Integer :: maxsubtemplate ! Dimension size for "SUBTEMPLATE" (both
!      dimensions)
     Integer :: maxtemplate ! Dimension size for "TEMPLATE" (both dimensions)
     Integer :: num_down ! Number of rows of peaks found below the starting 
!      peak
     Integer :: num_left ! Number of columns of peaks found to the left of the
!      starting peak
     Integer, Save :: output_frequency = 200 ! Frequency of progress report
!      messages (in peaks)
     Integer, Save :: search_limit = 3 ! Limit for search
     Integer stat ! Status return variable for "Allocate"
     Integer, Save :: sub_pixelling = 9 ! Level of sub-pixelling for
!      cross-correlation
     Logical :: continue ! .True., until the three starting peaks have
!      been successfully entered
     Logical, Save :: first = .True. ! .True., means the subroutine has not
!      yet been called and some dynamic arrays have not been allocated
     Logical :: success1 ! .True., if starting peak is found
     Logical :: success2 ! .True., if axis 1 peak is found
     Logical :: success3 ! .True., if axis 2 peak is found
     Logical, Save :: vignetting_correction ! .True., if vignetting is to
!      be corrected
     Real, Save :: detect_ratio = 0.1 ! Ratio of a cross-correlation value
!      found for a peak divided by previous found peak value, above which
!      a new "peak" is accepted (avoids noise being classed as peaks)
     Real, Save :: sigma = 1.3 ! Standard deviation of peak function
     Real :: time ! Time to perform operation in seconds
     Real :: time_cpu ! CPU Time of process
     Real :: timend ! Time at end of operation
     Real :: timstr ! Time at start of operation
     Real :: correlation ! Maximum cross-correlation value
     Real :: x_axis1 ! X-centre of peak along axis 1 from starting peak
     Real :: x_axis2 ! X-centre of peak along axis 2 from starting peak
     Real :: x_start ! Approximate X-coordinate of start peak
     Real :: y_axis1 ! Y-centre of peak along axis 1 from starting peak
     Real :: y_axis2 ! Y-centre of peak along axis 2 from starting peak
     Real :: y_start ! Approximate Y-coordinate of start peak
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(9) ! User messages
     Real, Allocatable:: SUBTEMPLATE(:, :) ! Dynamic work array
     Real, Allocatable:: TEMPLATE(:, :) ! Dynamic work array
!  Internal Functions:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CALIBRATE2DGRID '// Version)
     Else
 
!     Set appropriate default values for input format
        If (first) Then
 
!        Get input format from data-base
           Call IO_INQ_KEYVALUE ('#INPUT_FORMAT', len_string, file_format, &
             db_stat, status)
 
           If (db_stat .Ne. 0) Then
              file_format = 'None'
           End If
 
           If (file_format .Eq. 'IMAGEQUANT') Then
 
!           Set defaults for ID-9 and ID-27 use of Molecular dynamics
!           scanners
              search_limit = 3
              detect_ratio = 0.2
              sigma = 1.3
              vignetting_correction = .False.
 
           Else
 
!           Set defaults for Be XRII/CCD
              search_limit = 6
              detect_ratio = 0.3
              sigma = 2.63
              vignetting_correction = .True.
 
           End If
 
           first = .False.
 
        End If
 
!     Initialise values of X-positions to all bad values
        Call MA_RVALUE (xmax_peaks, ymax_peaks, 1, 1, xmax_peaks, ymax_peaks, &
          -1.7e38, X_2DPEAKS, status)
 
        continue = .True.
        Do While (continue)
 
!        Display data, and input approximate starting position and
!        grid vectors (all in pixel coordinates)
           Call F2D_INITGRIDSEARCH (xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, DATA, XAXIS, YAXIS, title, xlabel, ylabel, &
             zlabel, num_display, x_start, y_start, x_axis1, y_axis1, x_axis2, &
             y_axis2, status)
 
!        Check status
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
              Return
           Else If (status .Ne. St_goodvalue) Then
              Return
           End If
 
!        Check that the first vector points in positive X and that
!        the second vector points in positive Y
           If (x_axis1 .Lt. x_start .Or. y_axis2 .Lt. y_start) Then
 
              Call IO_WRITE ('WARNING: The horizontal peak ' // &
                'search vector must point to the right and', status)
              Call IO_WRITE ('         the vertical peak ' // &
                'search vector must point upwards', status)
 
           Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!           Size of search region around predicted centre
              MESSAGE(1) = 'Enter the maximum number of pixels a ' // &
                'search for a new peak centre may'
              MESSAGE(2) = 'continue from the original predicted ' // &
                'centre (based on previously found'
              MESSAGE(3) = 'peaks). (This does not affect the ' // &
                'searching for the starting three'
              MESSAGE(4) = 'peaks, but affects all other peaks).'
              Call IO_INPI (.True., 1, 100, .True., &
                'MAXIMUM PEAK SEARCH DISTANCE (PIXELS)', 4, MESSAGE, 1, &
                'Value must be within given range', search_limit, status)
 
!           Enter cross-correlation ratio above which a new "peak" is
!           considered to be found
              MESSAGE(1) = 'Enter ratio above which a new "peak" ' // &
                'is considered to be found. This'
              MESSAGE(2) = 'the minimum ratio of maximum ' // &
                'cross-correlation value for a new "peak"'
              MESSAGE(3) = 'divided by the maximum ' // &
                'cross-correlation value of the last found peak.'
              MESSAGE(4) = 'This value is used to discriminate ' // &
                'against noise. If this value is too'
              MESSAGE(5) = 'low, noise may be counted as peaks, ' // &
                'if it is too high, then true peaks'
              MESSAGE(6) = 'may be discarded.'
              Call IO_INPR (.True., 0.0, 0.99, .True., 'PEAK DETECTION RATIO', &
                6, MESSAGE, 1, 'Value must be within given range', &
                detect_ratio, status)
 
!           Enter standard deviation of peaks
              MESSAGE(1) = 'Enter standard deviation width of ' // &
                'grid peaks in pixel units.  An approximate'
              MESSAGE(2) = 'value will work,  but will not ' // &
                'produce optimum estimates of the peak centres'
              MESSAGE(3) = 'i.e. the average centre will be ' // &
                'correct,  but the variance will  be  higher.'
              MESSAGE(4) = 'For the ESRF Berylium tube XRII with ' // &
                'the copper grid a value of 2.6 is about'
              MESSAGE(5) = 'optimum.  If you do not  know a ' // &
                'suitable value,  then after an initial "FIND'
              MESSAGE(6) = 'PEAKS"  you may calculate an ' // &
                'averaged peak profile  at  sub-pixel resolution'
              MESSAGE(7) = 'using  the  "LEARN HOLE PROFILE"  ' // &
                'command.  This will produce  the  averaged'
              MESSAGE(8) = 'profile in the memory,  which may be ' // &
                'fitted with  a  2-D Gaussian within the'
              MESSAGE(9) = '"FIT" sub-menu.'
              Call IO_INPR (.True., 0.0, 1.0e5, .True., &
                'PEAK STANDARD DEVIATION WIDTH (PIXELS)', 9, MESSAGE, 1, &
                'Value must be within given range', sigma, status)
 
!           Enter number of sub-pixels
              Call IO_INPI (.True., 1, 100, .True., 'NUMBER OF SUB-PIXELS', 1, &
                'More takes longer, but can give more accurate '// 'results', &
                1, 'Value must be within given range', sub_pixelling, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!           Calculate size of pixel resolution template
              maxtemplate = Max(3, (Int(6.0 * sigma) / 2) * 2 + 1)
              Allocate (TEMPLATE(maxtemplate, maxtemplate), Stat = stat)
              If (stat .Ne. 0) Then
                 status = St_mod_fit2d + St_bad_malloc
                 Call ST_SAVE ('Subroutine F2D_CALIBRATE2DGRID ' // Version)
                 Return
              End If
 
!           Calculate size of sub-pixel resolution template and
!           allocate array space (over-sampling of data)
              maxsubtemplate = Max(3, (Int(Real(sub_pixelling) * 6.0 * sigma) &
              / 2)*2 + 1)
              maxtemplate = Max(3, (Int(6.0 * sigma) / 2) * 2 + 1)
              Allocate (SUBTEMPLATE(maxsubtemplate, maxsubtemplate), &
                Stat = stat)
              If (stat .Ne. 0) Then
                 status = St_mod_fit2d + St_bad_malloc
                 Call ST_SAVE ('Subroutine F2D_CALIBRATE2DGRID ' // Version)
                 Return
              End If
 
!           Calculate template functions
              Call F2D_CALTEMPLATE (maxtemplate, maxtemplate, maxtemplate, &
                maxtemplate, sigma, 1, TEMPLATE, status)
              Call F2D_CALTEMPLATE (maxsubtemplate, maxsubtemplate, &
                maxsubtemplate, maxsubtemplate, sigma, sub_pixelling, &
                SUBTEMPLATE, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!           Find best correlation centres of starting peaks
              Call F2D_FINDCENTRE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                xendelm, yendelm, DATA, maxtemplate, maxtemplate, maxtemplate, &
                maxtemplate, TEMPLATE, maxsubtemplate, maxsubtemplate, &
                maxsubtemplate, maxsubtemplate, sub_pixelling, &
                SUBTEMPLATE, 5, x_start, y_start, success1, &
                correlation, status)
              Call F2D_FINDCENTRE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                xendelm, yendelm, DATA, maxtemplate, maxtemplate, maxtemplate, &
                maxtemplate, TEMPLATE, maxsubtemplate, maxsubtemplate, &
                maxsubtemplate, maxsubtemplate, sub_pixelling, &
                SUBTEMPLATE, 5, x_axis1, y_axis1, success2, &
                correlation, status)
              Call F2D_FINDCENTRE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                xendelm, yendelm, DATA, maxtemplate, maxtemplate, maxtemplate, &
                maxtemplate, TEMPLATE, maxsubtemplate, maxsubtemplate, &
                maxsubtemplate, maxsubtemplate, sub_pixelling, &
                SUBTEMPLATE, 5, x_axis2, y_axis2, success3, &
                correlation, status)
 
!           Check that the starting peaks have been found O.K.
              If (success1 .And. success2 .And. success3) Then
                 Call IO_WRITE ('INFO: Starting peaks found O.K.', status)
                 Write (MESSAGE(1), '(''INFO: Starting peak is ' // &
                   'at position '', 2f12.5)') x_start, y_start
                 Call IO_WRITE (MESSAGE(1), status)
                 continue = .False.
                 Write (MESSAGE(1), '(''INFO: Next first grid ' // &
                   'axis peak is at position '', 2f12.5)') x_axis1, y_axis1
                 Call IO_WRITE (MESSAGE(1), status)
                 Write (MESSAGE(1), '(''INFO: Next second grid ' // &
                   'axis peak is at position '', 2f12.5)') x_axis2, y_axis2
                 Call IO_WRITE (MESSAGE(1), status)
 
                 continue = .False.
 
              Else
                 Call IO_WRITE ('WARNING: Problem finding ' // &
                   'the starting peaks, please re-enter', status)
              End If
 
           End If
 
!        Check for user escape or other signalled problem
           If (status .Ne. St_goodvalue) Then
              Return
           End If
 
        End Do
 
!     Enter frequency of progress for grid search
        Call IO_INPI (.True., 1, 100000, .True., &
          'PROGRESS REPORT FREQUENCY (PEAKS)', 1, &
          'Number of peaks between progress report messages', 1, &
          'Value must be within given range', output_frequency, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Information for the user
        Call IO_WRITE (' ', status)
        Call IO_WRITE ('INFO: Starting peak search, this ' // &
          'takes some time for big grids', status)
 
!     Store start time
        Call IO_TIMES (timstr, time_cpu, status)
 
!     Search for peaks
        Call F2D_FIND2DGRID (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, maxtemplate, maxtemplate, maxtemplate, maxtemplate, &
          TEMPLATE, maxsubtemplate, maxsubtemplate, maxsubtemplate, &
          maxsubtemplate, sub_pixelling, SUBTEMPLATE, search_limit, &
          detect_ratio, x_start, y_start, x_axis1, y_axis1, x_axis2, y_axis2, &
          output_frequency, xmax_peaks, ymax_peaks, correlation, retstat, &
          xnumpeaks, ynumpeaks, num_peaks, num_left, num_down, X_2DPEAKS, &
          Y_2DPEAKS, status)
 
!     Store start time
        Call IO_TIMES (timend, time_cpu, status)
        time = timend - timstr
 
!     Information for the user
        Write (MESSAGE(1), '(''INFO: Number of peaks found = '', i6)') &
          num_peaks
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''INFO: Number of grid peaks (X/Y) ' // &
          '= '', 2i6, '' ('', i8, '')'')') xnumpeaks, ynumpeaks, xnumpeaks * &
          ynumpeaks
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''INFO: Time taken = '', g10.2, '' seconds'')') &
          time
        Call IO_WRITE (MESSAGE(1), status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Rotate position array to correct position and convert to
!     data coordinates
        If (retstat .Eq. 0) Then
           Call F2D_FINDROTATE (xmaxdat, ymaxdat, xendelm, yendelm, XAXIS, &
             YAXIS, xmax_peaks, ymax_peaks, xnumpeaks, ynumpeaks, num_left, &
             num_down, X_2DPEAKS, Y_2DPEAKS, X_DISTORTION, Y_DISTORTION, &
             status)
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Free dynamic memory
        Deallocate (TEMPLATE)
        Deallocate (SUBTEMPLATE)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Enter spacing between each grid hole
        MESSAGE(1) = 'In order to calibrate the absolute pixel' // &
          ' sizes and spatial'
        MESSAGE(2) = 'distortion value the distances between ' // &
          'grid holes in the'
        MESSAGE(3) = 'calibration grid need to be input. At the ' // &
          'ESRF the beryllium'
        MESSAGE(4) = 'entrance window X-ray image intensifer / ' // &
          'CCD detector systems'
        MESSAGE(5) = 'are normally calibrated with a grid with ' // &
          'a 5000 micron spacing.'
        MESSAGE(6) = 'The image plates are normally calibrated ' // &
          'using a grid which'
        MESSAGE(7) = 'has a grid spacing of 4000 microns, but ' // &
          'another finer grid is'
        MESSAGE(8) = 'available with a 2000 micron spacing.'
        Call IO_INPR (.True., 1.0, 1.0e6, .True., &
          'GRID SPACING (CENTRE TO CENTRE IN microns)', 8, MESSAGE, 1, &
          'Value must be within given range', grid_spacing, status)
 
!     Correct off-axis peak positions for mask vignetting
        MESSAGE(1) = 'When a mask hole is off-axis  ' // &
          '(non-orthogonal to  the beam)  the width of'
        MESSAGE(2) = 'the mask casts a shadow which changes the ' // &
          'apparent size and centre of the'
        MESSAGE(3) = 'hole as recorded on  the detector.  This ' // &
          'option allows this effect to  be'
        MESSAGE(4) = 'very largely corrected, assuming that the ' // &
          'mask is orthogonal to the beam,'
        MESSAGE(5) = 'and that the mask hole sizes are ' // &
          'perpendicular to the surface of the mask.'
        Call IO_INPL (.True., 0, 1, .True., &
          'CORRECT OFF-AXIS MASK VIGNETTING', 5, MESSAGE, 1, &
          'Value must be within given range', vignetting_correction, status)
 
        If (vignetting_correction) Then
 
!        Correction for mask shadowing
           Call F2D_VIGNETTING (grid_spacing, xstrelm, ystrelm, xendelm, &
             yendelm, xmax_peaks, ymax_peaks, xnumpeaks, ynumpeaks, X_2DPEAKS, &
             Y_2DPEAKS, status)
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Calculate distortion
        If (retstat .Eq. 0) Then
           Call F2D_2DDISTORTION (xmax_peaks, ymax_peaks, xnumpeaks, &
             ynumpeaks, X_2DPEAKS, Y_2DPEAKS, grid_spacing, x_pixel_size, &
             y_pixel_size, X_DISTORTION, Y_DISTORTION, status)
        End If
 
     End If
 
     End Subroutine F2D_CALIBRATE2DGRID
!********1*********2*********3*********4*********5*********6*********7**
