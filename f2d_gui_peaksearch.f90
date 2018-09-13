!********1*********2*********3*********4*********5*********6*********7*********8

!  **************************
!  *                        *
!  * f2d_gui_peaksearch.f90 *
!  *                        *
!  **************************
 
!+ F2D_GUI_PEAKSEARCH - FIT 2-D GUI PEAK SEARCH (diffraction)
     Subroutine F2D_GUI_PEAKSEARCH (allow_edit, current_file, ask_change, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, MASK, XAXIS, YAXIS, xstrelm, &
       ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, max_peaks, &
       experiment, num_peaks, MDATA, PEAKS, draw_bad_weak, status)
!  Description:
!    Searches the region "(xstrelm, ystrelm)" to "(xendelm, yendelm)"
!    for peaks, and displays them overlayed on the image.
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    23-Oct-2007: V0.22 Changes to control parameters (Hammersley)
!    12-Feb-2007: V0.21 Changes to "F2D_PEAKSEARCH2" (Hammersley)
!    16-Nov-2006: V0.20 Use "PEAK_SEARCH_CONTROL" structure (Hammersley)
!    15-Nov-2006: V0.19 Use "PEAK_STRUCTURE" to hold results (Hammersley)
!    14-Mar-2006: V0.18 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    23-Feb-1999: V0.17 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.16 Change to use IO internal database routines (Hammersley)
!    26-Jun-1998: V0.15 Add peaks D-spacings array (Hammersley)
!    11-Jul-1997: V0.14 Increase allowed range of pixel sigma to be counted 
!      as a potential peak pixel (Hammersley)
!    10-Mar-1997: V0.13 Account for mask in peak search (Hammersley)
!    09-Mar-1997: V0.12 Menu to allow user addition or removal of peaks 
!      (Hammersley)
!    21-Feb-1997: V0.11 Set maximum number of "MIN PIXELS" to 25 which is 
!      the most that the code can find (Hammersley)
!    20-Feb-1997: V0.10 Save settings of peak search to data-base and use as 
!      defaults if possible (Hammersley)
!    19-Feb-1997: V0.9 Option to output peak positions and intensities 
!      (Hammersley)
!    16-Dec-1996: V0.8 Avoid open strings crossing lines (Hammersley)
!    08-Mar-1996: V0.7 Check for "CANCEL" from form (Hammersley)
!    12-Feb-1996: V0.6 Add gain parameter (Hammersley)
!    10-Feb-1996: V0.5 Add histogram output of I/sigmas (Hammersley)
!    09-Feb-1996: V0.4 Add "data_limit" minimum value for pixel values to 
!      accepted as being valid (Hammersley)
!    07-Feb-1996: V0.3 Control search parameters through a form (Hammersley)
!    29-Jan-1996: V0.2 Changes to "F2D_PEAKSEARCH" (Hammersley)
!    28-Jan-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc' ! Data structure definitions
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Logical, Intent(IN) :: allow_edit ! .True., if peak list can be edited
     Character(Len = *), Intent(IN) :: current_file ! Full name of input file
     Logical, Intent(IN) :: ask_change ! .True., if the user is asked
!      whether or not to change search parameters
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements defined in
!      Y-direction
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! .True. if element is
!      masked-off
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis values
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Integer, Intent(IN) :: max_peaks ! Dimension of peak arrays
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(INOUT) :: num_peaks ! On input should be set to zero, or
!      to the number of previously found peak, if peaks are to be added to
!      an existing peak search. On exit: total peaks found, including
!      problem peaks
!  Export:
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Blurred data
     Type(PEAK_STRUCTURE), Intent(OUT) :: PEAKS(max_peaks) ! Peak results
     Logical, Intent(OUT) :: draw_bad_weak ! .True., if bad but non-saturated 
!      "peaks" are to be drawn
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.22' ! Version number
     Integer, Parameter :: Io_unit = 17 ! I/O unit for ASCII output of results
     Integer, Parameter :: Max_chars =  53 ! Maximum number of characters
!      in a line
     Integer, Parameter :: Max_lines =  116 ! Number of lines in message
     Integer, Parameter :: Max_choices = 20 ! Maximum number of choices
!      (plus 5 for control menu)
!  Local Variables:
     Character(Len = 256) :: output_file ! File to contain results of
!      peak search
     Integer :: bin ! Loop variable for histogram bins
     Type(PEAK_SEARCH_CONTROL), Save :: mode ! Mode and control of peak 
!      search
     Integer :: num_bad ! Number of bad "peaks"
     Integer :: num_choices ! Number of choices
     Integer :: num_good ! Counter for good peaks
     Integer :: num_saturated ! Number of saturated peaks e.g.
!      intensity in one or more pixels equal or greater than saturation
     Integer :: peak ! Loop variable for peaks
     Integer :: retstat ! Return status from "F2D_PEAKSEARCH":
!      0 = Good status
!      1 = Too many peaks for peak arrays
     Logical, Save :: first = .True. ! .True., if first call to routine
     Real :: average_iosigma ! Average I / sigma(I)
     Real :: d_spacing ! D-spacing calculated for a spot
     Real :: iosigma ! intensity divided by standard deviation of
!      intensity for a peak
     Real :: lower_range ! Lower limit of I/sigma(I) bin
     Real :: time_cpu ! CPU time
     Real :: time_end ! Time at end of search
     Real :: time_start ! Time at start of search
     Real :: upper_range ! Upper limit of I/sigma(I) bin
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 60) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 70) :: HELP(23) ! User help text on the form
     Character(Len = 60) :: MESSAGE(2) ! User messages
     Character(Len = 70) :: PROMPT(3) ! User prompt text
     Character(Len = 256) :: STRINGS(256) ! String varaibles to input
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = Max_chars) :: TX(Max_lines) ! Text array
     Integer :: INTEGERS(Max_choices) ! Integer variables to be input
     Integer :: INTS_LOWER(Max_choices) ! Lower bound of integer variables
     Integer :: INTS_UPPER(Max_choices) ! Upper bound of integer variables
     Integer :: ISIGMAS(16) ! Histogram of number of spots in I/sigma ranges
     Integer :: RES_NUM(14) ! Number of spots for different resolution bins
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
     Logical :: BOUND(Max_choices) ! .True., if variable range is bound
     Logical :: LOGICALS(Max_choices) ! Logical variables to be changed
     Real :: REALS_LOWER(Max_choices) ! Lower bounds on reals
     Real :: REALS_UPPER(Max_choices) ! Upper bounds on reals
     Real :: REALS(Max_choices) ! Reals to be changed
     Real :: RES_SIGMAS(14) ! Average I/sigma(I) for different resolution bins
!  External Functions:
!  Local Data:
     Data TX(   1) / ' ' /
     Data TX(   2) / '      ------------------------'/
     Data TX(   3) / '      PEAK SEARCH CONTROL FORM'/
     Data TX(   4) / '      ------------------------'/
     Data TX(   5) / ' ' /
     Data TX(   6) / 'The peak search control form allows a'/
     Data TX(   7) / 'number of control parameters for the'/
     Data TX(   8) / 'peak search to be varied to optimise'/
     Data TX(   9) / 'the success in finding genuine peaks'/
     Data TX(  10) / 'and rejecting background noise and'/
     Data TX(  11) / 'image imprefections.'/
     Data TX(  12) / ' ' /
     Data TX(  13) / 'The peak search first smoothes the data'/
     Data TX(  14) / 'by a top-hat function of user defined'/
     Data TX(  15) / 'size ("BLUR SIZE"). This is the number'/
     Data TX(  16) / 'of pixels in each direction by which the'/
     Data TX(  17) / 'data pixels are averaged. (Entering a'/
     Data TX(  18) / 'value of 1 means that no smoothing is'/
     Data TX(  19) / 'applied.)'/
     Data TX(  20) / ' ' /
     Data TX(  21) / 'A minumum threshold value for "good" data'/
     Data TX(  22) / 'values may be set to easily exclude non X-ray'/
     Data TX(  23) / 'data. This is set with "DATA MINIMUM". Any'/
     Data TX(  24) / 'pixel with a value below this value is excluded'/
     Data TX(  25) / 'from the search.'/
     Data TX(  26) / ' ' /
     Data TX(  27) / 'After smoothing, the peak search looks at the'/
     Data TX(  28) / 'image and sets "good" pixels to be either '/
     Data TX(  29) / 'candidate "peak" pixels or "background"'/
     Data TX(  30) / 'pixels. For an initial square region pixels'/
     Data TX(  31) / 'which are significantly above the mean are'/
     Data TX(  32) / 'determined as peak pixels, and the mean and the'/
     Data TX(  33) / 'standard deviation of the remaining "background"'/
     Data TX(  34) / 'pixels is re-calculated. Pixels whose value is'/
     Data TX(  35) / 'above the mean plus the number of standard'/
     Data TX(  36) / 'deviations defined by "PEAK SIGMA", are considered'/
     Data TX(  37) / 'as candidate peak pixels, whilst pixels whose'/
     Data TX(  38) / 'values are less than the mean minus the number of'/
     Data TX(  39) / 'standard devaitions defined by "BAD SIGMA" are'/
     Data TX(  40) / 'rejected as "bad" data and are neither considered'/
     Data TX(  41) / 'as peak pixels, nor used to calculate the background'/
     Data TX(  42) / 'values. This procedure is repeated iteratively until'/
     Data TX(  43) / 'the reduction in the mean "background" level is less'/
     Data TX(  44) / 'than the estimated noise.'/
     Data TX(  45) / ' ' /
     Data TX(  46) / 'The minimum number of "background" pixels which'/
     Data TX(  47) / 'are averaged before a background mean value is'/
     Data TX(  48) / 'considered reliable enough is set with'/
     Data TX(  49) / '"MIN BACKGROUND".'/
     Data TX(  50) / ' ' /
     Data TX(  51) / 'The size of moving window for calculating means' / 
     Data TX(  52) / 'and sigmas is set with "X BOX SIZE" and "Y BOX SIZE".'/
     Data TX(  53) / ' ' /
     Data TX(  54) / ' ' /
     Data TX(  55) / ' ' /
     Data TX(  56) / ' ' /
     Data TX(  57) / ' ' /
     Data TX(  58) / ' ' /
     Data TX(  59) / 'Once candidate "peak" pixels have been found the'/
     Data TX(  60) / 'the "peaks" are found according to a number of'/
     Data TX(  61) / 'user controlled "rules". A peak must contain a'/
     Data TX(  62) / 'minimum number of candidate pixels. This number is'/
     Data TX(  63) / 'set with "NUM PIXELS", and the centre of one peak'/
     Data TX(  64) / 'must be a minimum number of pixels away from the'/
     Data TX(  65) / 'centre of the closest peak. This is set with'/
     Data TX(  66) / '"MIN. SPACING".'/
     Data TX(  67) / ' ' /
     Data TX(  68) / 'The pixel with the highest value within the candidate'/
     Data TX(  69) / 'pixels is taken to be the peak centre.'/
     Data TX(  70) / ' ' /
     Data TX(  71) / 'If this value is above or equal to the value set'/
     Data TX(  72) / 'with the "SATURATION" button, then the peak will'/
     Data TX(  73) / 'be considered saturated and indicated with a'/
     Data TX(  74) / 'yellow cross. Non-saturated, "good" peaks are'/
     Data TX(  75) / 'indicated with a red cross.'/
     Data TX(  76) / ' ' /
     Data TX(  77) / 'If required, "Bad peaks" may also be indicated'/
     Data TX(  78) / 'by using the "BAD WEAK" button. Such "peaks"'/
     Data TX(  79) / 'are drawn with a red asterisk. This may be'/
     Data TX(  80) / 'useful in identifying optimum parameters for'/
     Data TX(  81) / 'differentiating between peaks and noise.'/
     Data TX(  82) / ' ' /
     Data TX(  83) / 'Peak intensities are estimated from the total'/
     Data TX(  84) / '"box" intensity of the candidate pixels minus'/
     Data TX(  85) / 'the esitmated mean background level per pixel.'/
     Data TX(  86) / ' ' /
     Data TX(  87) / 'To estimate the standard deviation of the'/
     Data TX(  88) / 'integrated intensities and hence I/Sigma(I)''s'/
     Data TX(  89) / 'the detector offset value may be set using the'/
     Data TX(  90) / 'the "DATA OFFSET" button. Many CCD read-out'/
     Data TX(  91) / 'systems and other detector read-out systems'/
     Data TX(  92) / 'have a considerable offset value (e.g. 200'/
     Data TX(  93) / 'counts). So this should be taken into account'/
     Data TX(  94) / 'when estimates based on counting statistics'/
     Data TX(  95) / 'are being calculated. Similarly, the gain of'/
     Data TX(  96) / 'the detector system can be set using the'/
     Data TX(  97) / '"DET. GAIN" button. Many systems have gains'/
     Data TX(  98) / 'significantly different to 1.'/
     Data TX(  99) / ' ' /
     Data TX( 100) / 'The result of the peak search may be stored'/
     Data TX( 101) / 'in an ASCII file by using the "SAVE PEAKS"'/
     Data TX( 102) / 'button. By default this file while have the'/
     Data TX( 103) / 'same base name as the input image, but with'/
     Data TX( 104) / 'the file extension "peaks". However, this'/
     Data TX( 105) / 'file name may be changed using the'/
     Data TX( 106) / '"OUTPUT FILE" button.'/
     Data TX( 107) / ' ' /
     Data TX( 108) / ' ' /
     Data TX( 109) / '---------------'/
     Data TX( 110) / 'END OF HELP TEXT'/
     Data TX( 111) / '---------------'/
     Data TX( 112) / ' ' /
     Data TX( 113) / ' ' /
     Data TX( 114) / ' ' /
     Data TX( 115) / ' ' /
     Data TX( 116) / ' ' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_PEAKSEARCH ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0 ) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_GUI_PEAKSEARCH ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_GUI_PEAKSEARCH'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Set default name of results file
        Call IO_OUTFILE (0, current_file, '.peaks', 'data.peaks', output_file, &
          status)
 
        If (first) Then

!        Set default values for peak search control
           mode%mode = 1
           mode%blur_size = 3
           mode%x_box_size = 31
           mode%y_box_size = 31
           mode%num_adjacent = 16
           mode%min_spacing = 12
           mode%data_limit = 1.0
           mode%num_peak_sigma = 3.5
           mode%num_bad_sigma = 12.0
           mode%min_back_pixels = 16
           mode%offset = 0.0
           mode%saturation_level = 65534.5
           mode%output_results = .False.

!        Try to input default parameters for peak search from the data-base
           Call IO_INQ_IKEYVALUE ('PEAK_SEARCH_BLUR_SIZE', mode%blur_size, &
             retstat, status)
           Call IO_INQ_IKEYVALUE ('PEAK_SEARCH_X_BOX_SIZE', mode%x_box_size, &
             retstat, status)
           Call IO_INQ_IKEYVALUE ('PEAK_SEARCH_Y_BOX_SIZE', mode%y_box_size, &
             retstat, status)
           Call IO_INQ_IKEYVALUE ('PEAK_SEARCH_NUM_MINIMUM', &
             mode%num_adjacent, retstat, status)
           Call IO_INQ_IKEYVALUE ('PEAK_SEARCH_MIN_SPACING', mode%min_spacing, &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('PEAK_SEARCH_DATA_LIMIT', mode%data_limit, &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('PEAK_SEARCH_PEAK_SIGMA', &
             mode%num_peak_sigma, retstat, status)
           Call IO_INQ_RKEYVALUE ('PEAK_SEARCH_BAD_SIGMA', mode%num_bad_sigma, &
             retstat, status)
           Call IO_INQ_IKEYVALUE ('PEAK_SEARCH_MIN_BACK', &
             mode%min_back_pixels, retstat, status)
           Call IO_INQ_RKEYVALUE ('PEAK_SEARCH_DET_OFFSET', mode%offset, &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('#OVERLOAD_VALUE', mode%saturation_level, &
             retstat, status)
           Call IO_INQ_LKEYVALUE ('PEAK_SEARCH_DRAW_BAD', draw_bad_weak, &
             retstat, status)
           Call IO_INQ_LKEYVALUE ('PEAK_SEARCH_OUTPUT', mode%output_results, &
             retstat, status)
 
           first = .False.
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        If (ask_change) Then
 
!        Allow user of choose number of output bins and control
!        various other options
           num_choices = 15
           PROMPT(1) = 'CONTROL OF PEAK SEARCH VARIABLES '
           PROMPT(2) = '(SAVED BETWEEN FIT2D SESSIONS)'
           HELP(1) = 'This form allows you to control various ' // &
             'options controlling'
           HELP(2) = 'the peak search'
           BUTTONS(1) = 'BLUR SIZE'
           BUTTONS(2) = 'DATA MINIMUM'
           BUTTONS(3) = 'MIN BACKGROUND'
           BUTTONS(4) = 'X BOX SIZE'
           BUTTONS(5) = 'Y BOX SIZE'
           BUTTONS(6) = 'MIN. SPACING'
           BUTTONS(7) = 'SATURATION'
           BUTTONS(8) = 'PEAK SIGMA'
           BUTTONS(9) = 'BAD SIGMA'
           BUTTONS(10) = 'NUM PIXELS'
           BUTTONS(11) = 'BAD WEAK'
           BUTTONS(12) = 'DATA OFFSET'
           BUTTONS(13) = 'DET. GAIN'
           BUTTONS(14) = 'SAVE PEAKS'
           BUTTONS(15) = 'OUTPUT FILE'
           TYPES(1) = 1
           TYPES(2) = 3
           TYPES(3) = 1
           TYPES(4) = 1
           TYPES(5) = 1
           TYPES(6) = 1
           TYPES(7) = 3
           TYPES(8) = 3
           TYPES(9) = 3
           TYPES(10) = 1
           TYPES(11) = 2
           TYPES(12) = 3
           TYPES(13) = 3
           TYPES(14) = 2
           TYPES(15) = 8
           TEXT(1) = 'NUMBER OF PIXELS IN SMOOTHING'
           TEXT(2) = 'MINIMUM ACCEPTED "GOOD" DATA VALUE'
           TEXT(3) = 'MINIMUM NUMBER OF BACKGROUND PIXELS'
           TEXT(4) = 'X SIZE OF BACKGROUND BOX'
           TEXT(5) = 'Y SIZE OF BACKGROUND BOX'
           TEXT(6) = 'MINIMUM SPACING BETWEEN PEAKS'
           TEXT(7) = 'DETECTOR SATURATION VALUE'
           TEXT(8) = 'NUMBER OF SIGMA ABOVE MEAN'
           TEXT(9) = 'NUMBER OF SIGMA BELOW MEAN'
           TEXT(10) = 'MINIMUM NUMBER OF PIXELS IN A PEAK'
           TEXT(11) = 'DRAW BAD BUT NON-SATURATED "PEAKS"'
           TEXT(12) = 'DETECTOR OFFSET VALUE (DATA ZERO)'
           TEXT(13) = 'DETECTOR GAIN FACTOR'
           TEXT(14) = 'SAVE PEAK X/Y/I IN ASCII FILE'
           TEXT(15) = 'NAME OF PEAK SEARCH OUTPUT FILE'
           FULL_PROMPTS(1) = 'Enter size of top-hat "smoothing function'
           FULL_PROMPTS(2) = 'Enter lowest accepted value for ' // &
             'a pixel value (inclusive)'
           FULL_PROMPTS(3) = 'Minimum number of background ' // &
             'pixels for statistics'
           FULL_PROMPTS(4) = 'X size of region for calculating sigma'
           FULL_PROMPTS(5) = 'Y size of region for calculating sigma'
           FULL_PROMPTS(6) = 'Enter minimum spacing between peaks'
           FULL_PROMPTS(7) = 'Enter value above which pixels ' // &
             'will be considered as saturated'
           FULL_PROMPTS(8) = 'Enter number of standard ' // &
             'deviations above background'
           FULL_PROMPTS(9) = 'Enter number of standard ' // &
             'deviations below background'
           FULL_PROMPTS(10) = 'Enter minimum number of candidate ' // &
             'pixels in a peak'
           FULL_PROMPTS(11) = 'Enter "YES" to output bad ' // &
             'non-saturated "peaks"'
           FULL_PROMPTS(12) = 'Enter detector zero level ' // &
             'value: detector offset value'
           FULL_PROMPTS(13) = 'Enter detector gain (multipler ' // &
             'on Poisson statistics)'
           FULL_PROMPTS(14) = 'Enter "YES" to save peak ' // &
             'positions and intensities in an ASCII file'
           FULL_PROMPTS(15) = 'Enter name of ASCII file to ' // &
             'store results of peak search'
           BOUND(1) = .True.
           BOUND(2) = .True.
           BOUND(3) = .True.
           BOUND(4) = .True.
           BOUND(5) = .True.
           BOUND(6) = .True.
           BOUND(7) = .True.
           BOUND(8) = .True.
           BOUND(9) = .True.
           BOUND(10) = .True.
           BOUND(11) = .True.
           BOUND(12) = .True.
           BOUND(13) = .True.
           BOUND(14) = .True.
           BOUND(15) = .True.
           INTS_LOWER(1) = 1
           INTS_UPPER(1) = 9
           REALS_LOWER(2) = 0.0
           REALS_UPPER(2) = 100000.0
           INTS_LOWER(3) = 1
           INTS_UPPER(3) = 10000
           INTS_LOWER(4) = 7
           INTS_UPPER(4) = xendelm
           INTS_LOWER(5) = 7
           INTS_UPPER(5) = yendelm
           INTS_LOWER(6) = 5
           INTS_UPPER(6) = 30
           REALS_LOWER(7) = 100.0
           REALS_UPPER(7) = 1.0e20
           REALS_LOWER(8) = 2.0
           REALS_UPPER(8) = 30.0
           REALS_LOWER(9) = 5.0
           REALS_UPPER(9) = 30.0
           INTS_LOWER(10) = 1
           INTS_UPPER(10) = 25
           REALS_LOWER(12) = 0.0
           REALS_UPPER(12) = 100000.0
           REALS_LOWER(13) = 0.01
           REALS_UPPER(13) = 10.0
           INTEGERS(1) = mode%blur_size
           REALS(2) = mode%data_limit
           INTEGERS(3) = mode%min_back_pixels
           INTEGERS(4) = mode%x_box_size
           INTEGERS(5) = mode%y_box_size
           INTEGERS(6) = mode%min_spacing
           REALS(7) = mode%saturation_level
           REALS(8) = mode%num_peak_sigma
           REALS(9) = mode%num_bad_sigma
           INTEGERS(10) = mode%num_adjacent
           LOGICALS(11) = draw_bad_weak
           REALS(12) = mode%offset
           REALS(13) = experiment%detector_gain
           LOGICALS(14) = mode%output_results
           STRINGS(15) = output_file
 
!        Output interactive graphical form
           Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, Max_choices, &
             num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, &
             INTS_LOWER, INTS_UPPER, REALS_LOWER, REALS_UPPER, INTEGERS, &
             LOGICALS, REALS, STRINGS, retstat, status)
 
!        Redraw image
           Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, XAXIS, &
             YAXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
             zlabel, status)
 
!        Check for "CANCEL"
           If (retstat .Eq. -1) Then
              Return
           End If
 
!        Set resulting values
           mode%blur_size = INTEGERS(1)
           mode%data_limit = REALS(2)
           mode%min_back_pixels = INTEGERS(3)
           mode%x_box_size = INTEGERS(4)
           mode%y_box_size = INTEGERS(5)
           mode%min_spacing = INTEGERS(6)
           mode%saturation_level = REALS(7)
           mode%num_peak_sigma = REALS(8)
           mode%num_bad_sigma = REALS(9)
           mode%num_adjacent = INTEGERS(10)
           draw_bad_weak = LOGICALS(11)
           mode%offset = REALS(12)
           experiment%detector_gain = REALS(13)
           mode%output_results = LOGICALS(14)
           output_file = STRINGS(15)
 
!        Save values in data-base and hence ".fit2d.def" file
           Call IO_SET_IKEYVALUE ('PEAK_SEARCH_BLUR_SIZE', mode%blur_size, &
             retstat, status)
           Call IO_SET_IKEYVALUE ('PEAK_SEARCH_X_BOX_SIZE', mode%x_box_size, &
             retstat, status)
           Call IO_SET_IKEYVALUE ('PEAK_SEARCH_Y_BOX_SIZE', mode%y_box_size, &
             retstat, status)
           Call IO_SET_IKEYVALUE ('PEAK_SEARCH_NUM_MINIMUM', &
             mode%num_adjacent, retstat, status)
           Call IO_SET_IKEYVALUE ('PEAK_SEARCH_MIN_SPACING', mode%min_spacing, &
             retstat, status)
           Call IO_SET_RKEYVALUE ('PEAK_SEARCH_DATA_LIMIT', mode%data_limit, &
             retstat, status)
           Call IO_SET_RKEYVALUE ('PEAK_SEARCH_PEAK_SIGMA', &
             mode%num_peak_sigma, retstat, status)
           Call IO_SET_RKEYVALUE ('PEAK_SEARCH_BAD_SIGMA', mode%num_bad_sigma, &
             retstat, status)
           Call IO_SET_IKEYVALUE ('PEAK_SEARCH_MIN_BACK', &
             mode%min_back_pixels, retstat, status)
           Call IO_SET_RKEYVALUE ('PEAK_SEARCH_DET_OFFSET', mode%offset, &
             retstat, status)
           Call IO_SET_LKEYVALUE ('PEAK_SEARCH_DRAW_BAD', draw_bad_weak, &
             retstat, status)
           Call IO_SET_LKEYVALUE ('PEAK_SEARCH_OUTPUT', mode%output_results, &
             retstat, status)
 
        End If
 
!     Inform user of calculation and swapping of data to the memory
        MESSAGE(1) = 'WORKING: PEAK SEARCHING'
        Call GS_FPROMPT (1, 1, MESSAGE, status)
 
!     Force output
        Call GS_UPDATE (status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Check time
        Call IO_TIMES (time_start, time_cpu, status)
 
!     Conduct peak search
        num_saturated = 0
        num_bad = 0
        Call F2D_PEAKSEARCH2 (experiment, .True., mode, xmaxdat, ymaxdat, &
          DATA, MASK, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
          max_peaks, num_peaks, MDATA, retstat, num_saturated, num_bad, PEAKS, &
          status)

!     Check time
        Call IO_TIMES (time_end, time_cpu, status)
        Write (MESSAGE(1), '(''INFO: Time taken for search = '', f7.2, ' // &
          ''' seconds'')') time_end - time_start
        Call IO_WRITE (MESSAGE(1), status)
 
        If (retstat .Eq. 0) Then
 
!        Output user information
           Write (MESSAGE(1), '(''INFO: Number of peaks found = '', i6)') &
             num_peaks - num_bad
           Call IO_WRITE (MESSAGE(1), status)
           Write (message, &
             '(''INFO: Number of saturated peaks found = '', i6)') &
             num_saturated
           Call IO_WRITE (message, status)
           Write (message, '(''INFO: Number of bad "peaks" found = '', i6)') &
             num_bad
           Call IO_WRITE (message, status)
 
!        Draw peaks
           If (num_peaks .Gt. 0) Then
              Call F2D_DRAW_PEAKS (draw_bad_weak, max_peaks, num_peaks, &
                PEAKS, status)
           End If
 
           If (allow_edit) Then
 
!           Allow peaks to be added or removed from the list
              Call F2D_EDITPEAKS (draw_bad_weak, experiment, &
                xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, MASK, XAXIS, YAXIS, &
                xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
                zlabel, max_peaks, num_peaks, PEAKS, status)
 
           End If
 
           If (mode%output_results) Then
 
              Call IO_OPEN_ASCIIFILE (.False., 'WRITE', Io_unit, output_file, &
                retstat, status)
 
              If (retstat .Eq. 0) Then
 
                 Write (Io_unit, '(''      X         Y   ' // &
                   '    INTENSITY      SIGMA    D (Angstroms) '')')
                 Do peak = 1, num_peaks
 
                    If (PEAKS(peak)%status .Eq. 0 .And. &
                      PEAKS(peak)%intensity .Ge. 0.0) Then
                       Write (Io_unit, '(2f10.1, 2f12.1, f9.3)') &
                         PEAKS(peak)%x_centre, PEAKS(peak)%y_centre, &
                         PEAKS(peak)%intensity, PEAKS(peak)%sigma, &
                         PEAKS(peak)%d_spacing
                    End If
 
                 End Do
 
!              Close output file
                 Close (Io_unit)
 
              Else
                 Call IO_WRITE ('WARNING: Problem ' // &
                   'outputing peak search results', status)
 
              End If
 
           End If
 
        Else
           Call GS_FWARNING (1, 1, 'Peak search failed: too ' // &
             'many peaks for peak arrays', status)
           Call IO_WRITE ('WARNING: Peak search failed: too ' // &
             'many peaks for peak arrays', status)
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Create I/sigma histogram
        Do bin = 1, 16
           ISIGMAS(bin) = 0
        End Do
 
!     Create resolution number of reflections and average
!     I/sigma(I) histogram
        Do bin = 1, 14
           RES_NUM(bin) = 0
           RES_SIGMAS(bin) = 0.0
        End Do
 
        average_iosigma = 0.0
        num_good = 0
        Do peak = 1, num_peaks
 
           If (PEAKS(peak)%status .Eq. 0) Then
 
              If (PEAKS(peak)%sigma .Gt. 0.0) Then
                 iosigma = PEAKS(peak)%intensity / PEAKS(peak)%sigma
                 num_good = num_good + 1
                 average_iosigma = average_iosigma + iosigma
                 If (iosigma .Lt. 10.0) Then
                    bin = Int(iosigma / 2.0) + 1
                 Else If (iosigma .Lt. 25) Then
                    bin = Int((iosigma - 10.0) / 3.0) + 6
                 Else If (iosigma .Lt. 50) Then
                    bin = Int((iosigma - 25.0) / 5.0) + 11
                 Else
                    bin = 16
                 End If
                 ISIGMAS(bin) = ISIGMAS(bin) + 1
 
                 If (experiment%wavelength_set) Then
 
                    d_spacing = PEAKS(peak)%d_spacing
 
                    If (d_spacing .Gt. 3.0) Then
 
                       If (d_spacing .Gt. 7.0) Then
 
                          If (d_spacing .Gt. 15.0) Then
 
                             If (d_spacing .Gt. 25.0) Then
                                bin = 1
                             Else
                                bin = 2
                             End If
 
                          Else
 
                             If (d_spacing .Gt. 10.0) Then
                                bin = 3
                             Else
                                bin = 4
                             End If
 
                          End If
 
                       Else
 
                          If (d_spacing .Gt. 5.0) Then
                             bin = 5
                          Else If (d_spacing .Gt. 4.0) Then
                             bin = 6
                          Else
                             bin = 7
                          End If
 
                       End If
 
                    Else
 
                       If (d_spacing .Gt. 1.7) Then
 
                          If (d_spacing .Gt. 2.0) Then
 
                             If (d_spacing .Gt. 2.5) Then
                                bin = 8
                             Else
                                bin = 9
                             End If
 
                          Else
 
                             If (d_spacing .Gt. 1.8) Then
                                bin = 10
                             Else
                                bin = 11
                             End If
 
                          End If
 
                       Else
 
                          If (d_spacing .Gt. 1.6) Then
                             bin = 12
                          Else If (d_spacing .Gt. 1.5) Then
                             bin = 13
                          Else
                             bin = 14
                          End If
 
                       End If
 
                    End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''d_spacing , bin = '', f6.2, i3)')
!                 :                   d_spacing, bin
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                    RES_NUM(bin) = RES_NUM(bin) + 1
                    RES_SIGMAS(bin) = RES_SIGMAS(bin) + iosigma
 
                 End If
 
              End If
 
           End If
 
        End Do
        average_iosigma = average_iosigma / Real(num_good)
 
!     Normalised the resolution bin sigmas
        Do bin = 1, 14
 
           If (RES_NUM(bin) .Gt. 0) Then
              RES_SIGMAS(bin) = RES_SIGMAS(bin) / Real(RES_NUM(bin))
           End If
 
        End Do
 
!     Output histogram results
        Write (message, '(''INFO: Average I / Sigma(I) of found ' // &
          'peaks = '', f10.2)') average_iosigma
        Call IO_WRITE (message, status)
 
        Call IO_WRITE ('INFO: Number of peaks with given ' // &
          'ranges of estimated I / sigma(I)', status)
        Call IO_WRITE ('      Lower    Upper   Number', status)
        Do bin = 1, 16
 
           If (bin .Lt. 6) Then
              lower_range = Real(bin - 1) * 2.0
              upper_range = lower_range + 2.0
           Else If (bin .Lt. 11) Then
              lower_range = Real(bin - 6) * 3.0 + 10.0
              upper_range = lower_range + 3.0
           Else If (bin .Lt. 16) Then
              lower_range = Real(bin - 11) * 5.0 + 25.0
              upper_range = lower_range + 5.0
           Else
              lower_range = 50.0
              upper_range = 100000.0
           End If
 
           Write (message, '(''     '', f5.1, '' to '', f5.1, ' // &
             ''': '', i5)') lower_range, upper_range, ISIGMAS(bin)
           Call IO_WRITE (message, status)
 
        End Do
 
        Call IO_WRITE (' ', status)
 
        Call IO_WRITE ('INFO: Number of peaks within given ' // &
          'resolution ranges and average I / sigma(I)', status)
        Call IO_WRITE ('      Lower    Upper  Number <I/sigma(I)>', status)
        Do bin = 1, 14
 
           If (bin .Lt. 8) Then
 
              If (bin .Lt. 5) Then
 
                 If (bin .Lt. 3) Then
 
                    If (bin .Lt. 2) Then
                       lower_range = 1000.0
                       upper_range = 25.0
                    Else
                       lower_range = 25.0
                       upper_range = 15.0
                    End If
                 Else
 
                    If (bin .Lt. 4) Then
                       lower_range = 15.0
                       upper_range = 10.0
                    Else
                       lower_range = 10.0
                       upper_range = 7.0
 
                    End If
 
                 End If
 
              Else
 
                 If (bin .Lt. 6) Then
                    lower_range = 7.0
                    upper_range = 5.0
                 Else If (bin .Lt. 7) Then
                    lower_range = 5.0
                    upper_range = 4.0
                 Else
                    lower_range = 4.0
                    upper_range = 3.0
                 End If
 
              End If
 
           Else
 
              If (bin .Lt. 12) Then
 
                 If (bin .Lt. 10) Then
 
                    If (bin .Lt. 9) Then
                       lower_range = 3.0
                       upper_range = 2.5
                    Else
                       lower_range = 2.5
                       upper_range = 2.0
                    End If
 
                 Else
 
                    If (bin .Lt. 11) Then
                       lower_range = 2.0
                       upper_range = 1.8
                    Else
                       lower_range = 1.8
                       upper_range = 1.7
 
                    End If
 
                 End If
 
              Else
 
                 If (bin .Lt. 13) Then
                    lower_range = 1.7
                    upper_range = 1.6
                 Else If (bin .Lt. 14) Then
                    lower_range = 1.6
                    upper_range = 1.5
                 Else
                    lower_range = 1.5
                    upper_range = 0.0
                 End If
 
              End If
 
           End If
 
           Write (message, '(''     '', f5.1, '' to '', f5.1, ' // &
             ''': '', i6, f8.2)') lower_range, upper_range, RES_NUM(bin), &
             RES_SIGMAS(bin)
           Call IO_WRITE (message, status)
 
        End Do
        Call IO_WRITE (' ', status)
 
     End If
 
     End Subroutine F2D_GUI_PEAKSEARCH
!********1*********2*********3*********4*********5*********6*********7*********8
 
