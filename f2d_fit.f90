!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************
!  *             *
!  * f2d_fit.f90 *
!  *             *
!  ***************
 
!+ F2D_FIT - FIT 2-D FITting sub-menu
     Subroutine F2D_FIT (variances_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, &
       title, xlabel, ylabel, zlabel, experiment, xstrelm, &
       ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, &
       mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, memory_exist, &
       mx_pixel_size, my_pixel_size, status) ! : XAXIS, YAXIS, DATA,
!      VARIANCES,:    MXAXIS, MYAXIS, MDATA, MVARIANCES, :    MASK,
!  Description:
!    Main menu for fitting the 2-D data by a least squares fit
!    of a given model
!  Method:
!    Menu driven loop
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    27-Nov-2014: V0.53 Changes to "F2D_CAKE" (Hammersley)
!    13-Mar-2006: V0.52 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    29-Jan-2005: V0.51 Add "LOAD MODEL" command (Hammersley)
!    26-Jan-2005: V0.50 Add "SAVE MODEL" command (Hammersley)
!    25-Nov-1998: V0.49 Remove "first" time initialisation values (Hammersley)
!      "F2D_CALIBRANT" (Hammersley)
!    28-Jan-1998: V0.48 Option of Q-space re-binning in "CAKE" command 
!      (Hammersley)
!    08-Jul-1997: V0.47 Add "polarisation" argument to
!    02-Jul-1997: V0.46 Add command to simulate powder rings from
!      a 2-theta scan data-set (Hammersley)
!    24-Feb-1997: V0.45 Rename "F2D_SILICON" to "F2D_CALIBRANT" (Hammersley)
!    15-Jan-1997: V0.44 Add "OPTIMISE" command to menu, and replace "MINIMISE" 
!      with "OPTIMISE" as the default command after model initialisation, etc. 
!      (Hammersley)
!    01-Dec-1996: V0.43 Change 2-D fitting polynomial to a Chebyshev polynomial 
!      (Hammersley)
!    16-Nov-1996: V0.42 Remove NAG minimisation code and therefore change 
!      fitting parameter arrays from "COMMON" to argument lists (Hammersley)
!    12-Nov-1996: V0.41 Changes to argument list of "F2D_MASKTHRESHOLD" 
!      (Hammersley)
!    24-Aug-1996: V0.40 Add wavelength to "F2D_SILICON" interface (Hammersley)
!    10-Jun-1996: V0.39 Add "SILICON CALIBRATION" option (Hammersley)
!    08-Mar-1996: V0.38 Changes to "F2D_CAKE" (Hammersley)
!    27-Feb-1996: V0.37 Changes to "F2D_CAKE" (Hammersley)
!    15-Feb-1996: V0.36 Changes to "F2D_MASK" (Hammersley)
!    04-Feb-1996: V0.35 Pass program array pointers through common (Hammersley)
!    03-Feb-1996: V0.34 User text when control passes to graphics window 
!      (Hammersley)
!    31-Jan-1996: V0.33 Add "CAKE" command (Hammersley)
!    04-Jan-1996: V0.32 Remove call to "Secnds" (Hammersley)
!    03-Jan-1996: V0.31 Changes for IBM AIX "xlf" compiler (Hammersley)
!    10-Nov-1995: V0.30 Changes to "F2D_MASK" (Hammersley)
!    23-Oct-1995: V0.29 Option of partial Lorentz correction to 2-theta scan 
!      (Hammersley)
!    07-Sep-1995: V0.28 Check for re-sizing of graphics window (Hammersley)
!    01-Sep-1995: V0.27 Separate 2-theta and azimuthly polarisation
!      corrections, and add Lorentz correction (Hammersley)
!    28-Aug-1995: V0.26 Take account of polarisation (Hammersley)
!    14-May-1995: V0.25 Change to "F2D_RTHETA" call (Hammersley)
!    20-Apr-1995: V0.24 Radial pixel size output by "F2D_POWDERDIFFRACTION" 
!      (Hammersley)
!    10-Apr-1995: V0.23 Allow mask to be defined by threshold logic on the input
!      data (Hammersley)
!    05-Apr-1995: V0.22 Pixel sizes can now be changed by 
!      "F2D_POWDERDIFFRACTION" (Hammersley)
!    05-Mar-1995: V0.21 ROI can now be changed by "F2D_MASK" (Hammersley)
!    03-Mar-1995: V0.20 Add option to transfer mask to memory (Hammersley)
!    22-Feb-1995: V0.19 Save detector gain within subroutine (Hammersley)
!    15-Feb-1995: V0.18 Remove some fitting  from this level and transfer 
!      equivelent code to "F2D_NAG" (Hammersley)
!    08-Feb-1995: V0.17 Add "R/THETA BINNING" command (Hammersley)
!    30-Jan-1995: V0.16 Produce default values for beam centre etc. (Hammersley)
!    24-Jan-1995: V0.15 Changes to "F2D_NAG" (Hammersley)
!    19-Jan-1995: V0.14 Store sample to detector distance (Hammersley)
!    18-Jan-1995: V0.13 Add "TILT/BEAM CENTRE" command (Hammersley)
!    06-Jan-1995: V0.12 Change menu prompt to upper case "ENTER COMMAND" 
!      (Hammersley)
!    23-Aug-1994: V0.11 Include option for powder diffraction ring
!      fitting (beam centre and tilt angles) (Hammersley)
!    11-Aug-1993: V0.10 Transfer calibration operations to calibration 
!      sub-menu (Hammersley)
!    06-Aug-1993: V0.9 "F2D_CALIBRATEGRID" uses distortion arrays (Hammersley)
!    04-Aug-1993: V0.8 Add calibration grid peak centre finding command 
!      (Hammersley)
!    06-Jun-1993: V0.7 X or Y-direction normalisation (Hammersley)
!    04-Jun-1993: V0.6 Uni-directional normalisation (Hammersley)
!    19-Mar-1993: V0.5 NAG routine "E04FCF" used (Hammersley)
!    01-Mar-1993: V0.4 Add "SURFACE" command (Hammersley)
!    23-Feb-1993: V0.3 Add "OUTPUT PARAMETERS" command (Hammersley)
!    09-Feb-1993: V0.2 Add mask display command (Hammersley)
!    25-Jan-1993: V0.1 Original, based on "FIT2MENU" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
     Include 'f2d_lsqfit2d.inc' ! FIT2D model control variables
!  Import:
     Logical, Intent(IN) :: variances_exist ! .True., if error arrays exist
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Integer, Intent(IN) :: xnumdat ! Defines X-extent of data region
     Integer, Intent(IN) :: ynumdat ! Defines Y-extent of data region
!    Real XAXIS(xmaxdat) ! Array containing data X-coordinates
!    Real YAXIS(ymaxdat) ! Array containing data Y-coordinates
!    Real DATA(xmaxdat, ymaxdat) ! Array containing data to be fitted
!    Real VARIANCES(xmaxdat, ymaxdat) ! Array containing variances in the
!    data values
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
!  Import/Export:
     TYPE(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(INOUT) :: xendelm ! End X-element of region to be fitted
     Integer, Intent(INOUT) :: xstrelm ! Starting X-element of region to be
!      fitted
     Integer, Intent(INOUT) :: yendelm ! End Y-element of region to be
!      fitted
     Integer, Intent(INOUT) :: ystrelm ! Starting Y-element of region to be
!      fitted
     Integer, Intent(INOUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(INOUT) :: mynumdat ! Defines Y-extent of data region
!    Real MXAXIS(xmaxdat) ! Array containing data X-coordinates
!    Real MYAXIS(ymaxdat) ! Array containing data Y-coordinates
!    Real MDATA(xmaxdat, ymaxdat) ! Array containing data to be fitted
!    Real MVARIANCES(xmaxdat, ymaxdat) ! Array containing variances in
!    the data values
     Integer, Intent(INOUT) :: mxendelm ! End X-element of memory data
!      region
     Integer, Intent(INOUT) :: mxstrelm ! Starting X-element of memory data
!      region
     Integer, Intent(INOUT) :: myendelm ! End Y-element of memory data
!      region
     Integer, Intent(INOUT) :: mystrelm ! Starting Y-element of memory data
!      region
     Character(Len = *), Intent(INOUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(INOUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(INOUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(INOUT) :: mzlabel ! Z-axis label for data
     Logical, Intent(INOUT) :: memory_exist ! .True. if the memory contains
!      data
     Real, Intent(INOUT) :: mx_pixel_size ! Size of a pixel in the memory
!      data in the X-direction (metres)
     Real, Intent(INOUT) :: my_pixel_size ! Size of a pixel in the memory
!      data in the Y-direction (metres)
!    Logical*1 MASK(xmaxdat, ymaxdat) ! Data mask, .True., indicates
!    masked data point (i.e. point not considered in fitting)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.53' ! Version number
     Integer, Parameter :: Maxind = 5 ! Dimension for maximum number of
!      performance indicators which can be stored
     Integer, Parameter :: Max_menu = 30 ! Number of instructions in the
!      menu
     Integer, Parameter :: Max_parameters = 200 ! Maximum number of fitting
!      parameters
     Integer, Parameter :: Max_results = 100 ! Dimension for maximum number
!      of results deduced from the fitted parameters which can be stored
     Integer, Parameter :: Maxtext = 300 ! Dimension size of text array
!  Local Variables:
     Character(Len = 80) :: command ! Full command corresponding to user
!      request
     Character(Len = 132) :: format ! Format string for I/O
     Integer :: contint = 1 ! Dummy variable for "FITSET"
     Integer, Save :: data_type ! Type of data
!      0 = Unknown (No D-spacings)
!      1 = 2-D X/Y raw image with beam centre
!      2 = Horizontal 2-theta data
!      3 = 2-D polar image
     Integer, Save :: disfreq = 10 ! Frequency of graphical output of
!      result of fit
     Integer :: feature ! Loop variable for features
     Integer, Save :: haltcrit = 0 ! The criterion used to permaturely halt
!      the fitting process:
!        0 = No criterion, continue with fit
!        1 = Maximum upper limit on goodness of fit
     Integer :: item ! Loop variable for "DATA" statement items
     Integer, Save :: lorentz_geometry = 1 ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the equivalent of a 
!            2-theta scan i.e. detector at equal distance
     Integer :: maxwork ! Dimension of work array
     Integer :: num_menu ! Number of choices available in the menu
     Integer, Save :: num_parameters ! Number of parameters (varaible and
!      fixed) in fitting model function
     Integer, Save :: numiter ! Number of iterations
     Integer, Save :: num_results ! The number of results stored by the
!      subroutine
     Integer, Save :: numtext ! Number of lines of text to output
     Integer :: par ! Loop variable for parameters
     Integer :: pSUM ! Pointer to dynamic array "SUM"
     Integer :: pTOTAL ! Pointer to dynamic array "TOTAL"
     Integer :: temp_status ! Temporary status variable
     Logical :: continue ! .True. until user wants to exit menu
     Logical, Save :: correct_polarisation = .True. ! .True., if the
!      polarisation correction is to be applied
     Logical :: data_defined ! .True., if the current data arrays are
!    defined to contain data
     Logical, Save :: evolve = .True. ! .True. if the starting fit model is
!      to evolve,
!    otherwise the same starting model is used for every frame
     Logical, Save :: fastdis = .False. ! .True., if the display of the fit
!      is to be output with minimal text information to speed up the display
     Logical :: mask_data ! .True., if the current data is to be masked
     Logical :: mask_memory ! .True., if the memory data is to be masked
     Integer :: memory_data_type ! Type of data
!      0 = Unknown (No D-spacings)
!      1 = 2-D X/Y raw image with beam centre
!      2 = Horizontal 2-theta data
!      3 = 2-D polar image
     Logical :: mlm ! Dummy variable for "FITSET"
     Logical, Save :: params_exist ! .True. if fitting parameters exist
     Logical :: poisson ! Dummy variable for "FITSET"
     Logical :: reset ! .True. if reset status value
     Logical, Save :: results_exist ! .True. if fit results exist
     Logical :: resize ! .True., if the graphics output window has been re-sized
     Logical :: reverse ! Dummy variable for "F2D_SETUP"
     Logical, Save :: weighted_fit = .False. ! .True., if weighted fitting
!      is required
     Real, Save :: alpha = 1.1 ! Scaling factor for required accuracy
     Real, Save :: chisqr ! Goodness of fit value
     Real, Save :: haltval ! The value of the parameter used to halt the
!    fitting process (see "haltcrit" for meaning)
     Real, Save :: itsperpar = 11.5 ! Number of iterations per parameter
!      required to optimise function
     Real :: time_cpu ! CPU time of process in seconds
     Real :: timend ! End time for calculations
     Real :: timstr ! Start time for calculations
!  Local Arrays:
     Character(Len = 23), Save :: MENU(Max_menu) ! Array containing menu
!      choices
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Text to explain menu
!      choices
     Character(Len = 80) :: ERROR(10) ! Contains error text for the user
!    Character*(10) INDNAMES(Maxind) ! A description of each
!    performance indicator that is output
     Character(Len = 80) :: MESSAGE(10) ! Contains messages to user
     Character(Len = 10), Save :: PARNAMES(Max_parameters)
!      A description of each fit parameter
     Character(Len = 10), Save :: RESNAMES(Max_results) ! A description of
!      each result from the fit parameters
     Character(Len = 80), Save :: TEXT(Maxtext) ! Contains user text
     Integer, Save :: PARAM_INFO(Max_parameters) ! Information on each of
!      the parameters of the fit. Describing to which feature it belongs,
!      the type of feature and the number of the parameter within the
!      feature, and whether the parameter is constrained or free to
!      be varied.
!
!    The digits of each element are defined as follows :
!
!    ......XX - Number of feature (1:99)
!
!    ....XX.. - Type of feature     1 = Standard polynomial curve
!    2 = Gaussian
!    3 = Lorentzian
!    4 = Voigtian
!    5 = Chebyshev form polynomial
!    6 = 2-D Gaussian
!    7 = 2-D polar Gaussian
!    8 = 2-D polar Gaussian,but with
!    no centre of coordinate system
!    9 = 2-D polynomial (normal)
!    10 = 2-D twin polar Gaussian
!    11 = 2-D twin polar Gaussian (no
!    centre)
!
!    ..XX.... - Number of parameter within the feature
!    For a polynomial: 1 = zero order term
!    2 = first order term
!    etc.
!
!    For a Gaussian:   1 = Central position
!    2 = Maximum height (intensity)
!    3 = Standard deviation (width)
!
!    For a Lorentzian: 1 = Central position
!    2 = Maximum height (intensity)
!    3 = Full-width at half maximum
!
!    For a Voigtian :  1 = Centre position
!    2 = Maximum height (intensity)
!    3 = Gaussian sigma
!    4 = Lorentzian FWHM
!
!    For a 2-D Gaussian    1 = X-centre position
!    2 = Y-centre position
!    3 = Maximum height (intensity)
!    4 = Sigma 1st axis
!    5 = Sigma 2nd axis
!    6 = rotation (radians
!    anticlockwise from X-axis to
!    1st axis)
!
!    For a 2-D polar Gaussian 1 = X-centre (polar)
!    2 = Y-centre (polar)
!    3 = X-centre position
!    4 = Y-centre position
!    5 = Maximum height (intensity)
!    6 = Radial sigma
!    7 = Angular sigma (radians)
!
!    For a 2-D polar Gaussian 1 = X-centre position
!    without centre          2 = Y-centre position
!    3 = Maximum height (intensity)
!    4 = Radial sigma
!    5 = Angular sigma (radians)
!
!    For a 2-D twin polar Gaussian 1 = X-centre (polar)
!    2 = Y-centre (polar)
!    3 = X-centre position
!    4 = Y-centre position
!    5 = Maximum height (intensity)
!    6 = Radial sigma
!    7 = Angular sigma (radians)
!    8 = Twin peak maximum
!
!    For a 2-D twin polar Gaussian 1 = X-centre position
!    without centre                2 = Y-centre position
!    3 = Maximum height (intensity)
!    4 = Radial sigma
!    5 = Angular sigma (radians)
!    6 = Twin peak maximum
!
!    .X...... - Nature of parameter (Unused at present)
!
!    X....... - Transform type, for changing from world units to
!    pixels and vice versa
!    0 = No transform required
!    1 = X-axis Simple scaling
!    2 = X-axis Scaling and shift
!    3 = Y-axis simple scaling
!    4 = Y-axis scaling and shift
!    5 = Angle (internally radians)
!
     Logical, Save :: CONSTRAINTS(Max_parameters) ! Constraints array,
!      .True. if a model parameter is fixed and not to be fitted
     Real, Save :: COVARIANCE(Max_parameters, Max_parameters)
!      The estimated fractional variance-covariance matrix (1-D because SERROR
!      produces it that way)
     Real, Save :: PARAMS(Max_parameters) ! The fit model parameters
     Real, Save :: RESULT_ERR(Max_results) ! Array to contain errors in
!      calculated results
     Real, Save :: RESULTS(Max_results) ! Array to contain results
!      calculated from the fit parameters for each frame
     Real, Save :: PARAM_ERR(Max_parameters) ! Array to contain errors in
!      the fit
     Real, Save :: SCALE_FACTORS(Max_parameters) ! Scale factors array for
!      the parameter
!    Internal Functions:
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 10) / '2-THETA TO RINGS', 'CAKE', &
       'CHANGE SCALE', 'CLEAR MASK', 'CONSTRAIN', 'COVARIANCE', 'DEFINE MASK', &
       'DISPLAY MASK', 'EXIT', 'INPUT PARAMETERS' /
     Data (MENU(item), item = 11, 20) / 'LOAD MODEL', 'MASK STATISTICS', &
       'MINIMISE', 'MODEL', 'NORMALISATION', 'OPTIMISE', 'OUTPUT PARAMETERS', &
       'POWDER DIFFRACTION', 'QUIT', 'R/THETA RE-BINNING' /
     Data (MENU(item), item = 21, 30) / 'RADIAL PROFILE', 'RESULTS', &
       'SAVE MODEL', 'SILICON CALIBRATION', 'SET MASK COLOUR', 'SET UP', &
       'SURFACE POLYNOMIAL', 'THRESHOLD MASKING', 'TILT/BEAM CENTRE', &
       'TRANSFER MASK TO MEMORY' /
     Data (MENUTXT(item), item = 1, 10) / &
       '2-THETA TO RINGS: Calculate powder pattern from 2-theta scan', &
       'CAKE: Interactive azimuthal/radial integration of "CAKE" region', &
       'CHANGE SCALE: Change the typical parameter change sizes', &
       'CLEAR MASK: Eliminate all previously defined dead zones', &
       'CONSTRAIN: Constrain the fit parameters', &
       'COVARIANCE: Output Covariance Matrix', &
       'DEFINE MASK: Redefine masking of data', &
       'DISPLAY MASK: Display presently defined masking dead zones', &
       'EXIT: Exit fit sub-menu', 'INPUT PARAMETERS: Input fit parameters' /
     Data (MENUTXT(item), item = 11, 20) / &
       'LOAD MODEL: Input a fit model previous saved in a file', &
       'MASK STATISTICS: Statistics of mask', &
       'MINIMISE: (same as "OPTIMISE")', &
       'MODEL: Create model from parameter values, in memory', &
       'NORMALISATION: Uni-directional flat-field normalisation', &
       'OPTIMISE: Optimise fit model to data by iterative method', &
       'OUTPUT PARAMETERS: Output fit parameters to a file', &
       'POWDER DIFFRACTION: Integrate powder rings to 1-D scans', &
       'QUIT: Exit fit sub-menu', &
       'R/THETA RE-BINNING: Radial/Angular (polar) re-binning of ROI' /
     Data (MENUTXT(item), item = 21, 30) / &
       'RADIAL PROFILE: Calculate 1-D radial profile in memory', &
       'RESULTS: Results of fitting', &
       'SAVE MODEL: Save fit model in an output file', &
       'SILICON CALIBRATION: Refine distance/wavelength/centre etc.', &
       'SET MASK COLOUR: User choice of colour to draw masked pixels', &
       'SET UP: Alter set up parameters for the Least Squares Fit', &
       'SURFACE POLYNOMIAL: Fit 2-D Chebyshev polynomial', &
       'THRESHOLD MASKING: Mask elements depending on ROI data values', &
       'TILT/BEAM CENTRE: Determine from a powder ring', &
       'TRANSFER MASK TO MEMORY: Set masked elements to 1.0 in memory' /
!  Saved Variables:
!    Save INDNAMES
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_FIT ' // Version)
        Return
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_FIT'')')
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
        Call ST_SAVE ('Subroutine F2D_FIT ' // Version)
 
        Return
 
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Initialise values
     num_menu = Max_menu ! Number of instructions available in the menu
     command = 'INPUT PARAMETERS' ! Default command for menu input
     data_defined = .True.
 
!  Set no weighted fitting, if no variances
     If (.Not. variances_exist) Then
        weighted_fit = .False.
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Start command input/action loop until EXIT requested
     continue = .True.
     Do While (continue)
 
!     Get user to select between the available menu options
        ERROR(1) = 'Enter one of available commands'
        Call IO_MENU (.True., 'Fit sub-menu: ENTER COMMAND', Max_menu, &
          MENUTXT, 1, ERROR, Max_menu, num_menu, MENU, command, status)
 
!     Check for re-sizing of graphics window
        Call LG_INP_RESIZE (Gs_wkid_terminal, resize, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     EXECUTE MENU CHOICES
 
!     Call subroutine to perform requested operation.
        If (command .Eq. '2-THETA TO RINGS') Then
 
           Call IO_WRITE ('INFO: Control passed to graphics window', status)
 
!        Calculate powder pattern from 2-theta scan
           Call F2D_2THETATORINGS ( xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, title, xlabel, ylabel, zlabel, %val(pXAXIS), &
             %val(pYAXIS), %val(pDATA), experiment, correct_polarisation, &
             memory_exist, mtitle, mxlabel, mylabel, mzlabel, mxnumdat, &
             mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, %val(pMXAXIS), &
             %val(pMYAXIS), %val(pMDATA), mx_pixel_size, my_pixel_size, &
             status)
           command = 'EXIT'
 
        Else If (command .Eq. 'CAKE') Then
 
           Call IO_WRITE ('INFO: Control passed to graphics window', status)
 
!        Fit beam centre and tilt angles and calculate radial profile
           mask_data = .True.
           mask_memory = .False.
           Call F2D_CAKE (.False., xmaxdat, ymaxdat, data_defined, title, &
             xlabel, ylabel, zlabel, variances_exist, xnumdat, ynumdat, &
             xstrelm, ystrelm, xendelm, yendelm, experiment, &
             lorentz_geometry, data_type, &
             memory_exist, mask_data, mask_memory, mtitle, &
             mxlabel, mylabel, mzlabel, mxnumdat, mynumdat, mxstrelm, &
             mystrelm, mxendelm, myendelm, mx_pixel_size, my_pixel_size, &
             memory_data_type, status) 
! :          XAXIS, YAXIS, DATA, MASK, MXAXIS, MYAXIS, MDATA,
           command = 'EXIT'
 
        Else If (command .Eq. 'CHANGE SCALE') Then
 
           If (params_exist) Then
 
!           Change values of parameter scale sizes
              Call F2D_SCALE (Max_parameters, num_parameters, PARNAMES, &
                PARAMS, SCALE_FACTORS, status)
              command = 'OPTIMISE'
           Else
 
!           No parameters have been defined
              Call IO_WRITE ('WARNING: No fit parameters ' // &
                'have been defined, define using INPUT PARAMETERS', status)
              command = 'INPUT PARAMETERS'
           End If
 
        Else If (command .Eq. 'CLEAR MASK') Then
 
!        Reset all data region data mask values to .False.
           Call MA_L1VALUE (xmaxdat, ymaxdat, 1, 1, xnumdat, ynumdat, .False., &
             %val(pMASK), status)
 
           command = 'INPUT PARAMETERS'
 
        Else If (command .Eq. 'CONSTRAIN') Then
 
           If (params_exist) Then
 
!           Allow constraint of fit parameters
              Call F2D_CONSTRAIN (Max_parameters, num_parameters, PARNAMES, &
                CONSTRAINTS, PARAMS, status)
              command = 'OPTIMISE'
 
           Else
 
!           No parameters have been defined
              Call IO_WRITE ('WARNING: No parameters; ' // &
                'parameters must be input using INPUT PARAMETERS', status)
              command = 'INPUT PARAMETERS'
           End If
 
        Else If (command .Eq. 'COVARIANCE') Then
 
           Call IO_WRITE ('WARNING: Not yet (re-)implemented', status)
 
!        If (results_exist) Then
 
!        Output covariance matrix
!        Write (*,'(/1x, ''COVARIANCE MATRIX'', /)')
 
!        Do par = 1, numvaripar
 
!        Write (*,'(1x,16(f5.2))')
!        :               (COVARIANCE(par+(par1-1)*numvaripar),
!        :               par1=1,numvaripar)
!        End Do
 
!        command = 'EXIT'
 
!        Else
 
!        No results have been defined
!        Call IO_WRITE ('WARNING: Results must be ' //
!        :             'defined using "OPTIMISE"', status)
!        command = 'OPTIMISE'
!        End If
 
        Else If (command .Eq. 'DEFINE MASK') Then
 
           Call IO_WRITE ('INFO: Control passed to graphics window', status)
 
!        Graphical menu defining/ undefining mask
           Call F2D_MASK (.False., xmaxdat, ymaxdat, xnumdat, ynumdat, &
             %val(pDATA), %val(pXAXIS), %val(pYAXIS), title, xlabel, ylabel, &
             zlabel, experiment, &
             xstrelm, ystrelm, xendelm, yendelm, %val(pMASK), status)
 
           command = 'OPTIMISE'
 
        Else If (command .Eq. 'DISPLAY MASK') Then
 
!        Display data, but not masked pixels
           Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, %val(pDATA), &
             %val(pMASK), %val(pXAXIS), %val(pYAXIS), xstrelm, ystrelm, &
             xendelm, yendelm, title, xlabel, ylabel, zlabel, status)
           command = 'OPTIMISE'
 
        Else If (command .Eq. 'EXIT' .Or. command .Eq. 'QUIT') Then
 
!        EXIT: Return to main menu
           continue = .False.
 
        Else If (command .Eq. 'INPUT PARAMETERS') Then
 
!        Input parameters for fitting
           Call F2D_PARAMETERS (experiment, &
             xmaxdat, ymaxdat, xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), &
             %val(pDATA), %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
             Max_parameters, Max_results, xstrelm, ystrelm, xendelm, yendelm, &
             %val(pMASK), PARAMS, PARAM_INFO, CONSTRAINTS, PARNAMES, &
             SCALE_FACTORS, RESNAMES, f2d_num_features, num_parameters, &
             num_results, f2d_x_order, f2d_y_order, f2d_xmin_poly, &
             f2d_ymin_poly, f2d_xmax_poly, f2d_ymax_poly, params_exist, &
             weighted_fit, alpha, itsperpar, evolve, disfreq, fastdis, &
             haltcrit, haltval, %val(pMDATA), status)
 
!        Define memory
           memory_exist = .True.
           mxnumdat = xnumdat
           mynumdat = ynumdat
           mxstrelm = xstrelm
           mystrelm = ystrelm
           mxendelm = xendelm
           myendelm = yendelm
           mtitle = 'Residuals'
           mxlabel = xlabel
           mylabel = ylabel
           mzlabel = zlabel
           Call MA_RCOPY (xmaxdat, 1, %val(pXAXIS), 1, 1, xnumdat, 1, xmaxdat, &
             1, %val(pMXAXIS), status)
           Call MA_RCOPY (ymaxdat, 1, %val(pYAXIS), 1, 1, ynumdat, 1, ymaxdat, &
             1, %val(pMYAXIS), status)
 
           command = 'OPTIMISE'
 
        Else If (command .Eq. 'LOAD MODEL') Then
 
!        Load the model
           Call F2D_LOADMODEL (Max_parameters, params_exist, PARAMS, &
             PARAM_INFO, CONSTRAINTS, PARNAMES, SCALE_FACTORS, &
             f2d_num_features, num_parameters, f2d_x_order, f2d_y_order, &
             f2d_xmin_poly, f2d_ymin_poly, f2d_xmax_poly, f2d_ymax_poly, &
             weighted_fit, alpha, itsperpar, status)
 
!        Define memory
           memory_exist = .True.
           mxnumdat = xnumdat
           mynumdat = ynumdat
           mxstrelm = xstrelm
           mystrelm = ystrelm
           mxendelm = xendelm
           myendelm = yendelm
           mtitle = 'Residuals'
           mxlabel = xlabel
           mylabel = ylabel
           mzlabel = zlabel
           Call MA_RCOPY (xmaxdat, 1, %val(pXAXIS), 1, 1, xnumdat, 1, xmaxdat, &
             1, %val(pMXAXIS), status)
           Call MA_RCOPY (ymaxdat, 1, %val(pYAXIS), 1, 1, ynumdat, 1, ymaxdat, &
             1, %val(pMYAXIS), status)
 
           command = 'OPTIMISE'
 
        Else If (command .Eq. 'MASK STATISTICS') Then
 
!        Calculate and display mask statistics
           Call F2D_MASKSTATS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, %val(pMASK), status)
 
           command = 'DISPLAY MASK'
 
        Else If (command .Eq. 'MINIMISE' .Or. command .Eq. 'OPTIMISE') Then
 
           If (params_exist) Then
 
!           Store start time
              Call IO_TIMES (timstr, time_cpu, status)
              numiter = 0
 
!           Optimise model
              Call F2D_LSQFIT2D (experiment, xmaxdat, ymaxdat, &
                %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pVARIANCES), &
                %val(pMASK), xstrelm, ystrelm, xendelm, yendelm, alpha, &
                weighted_fit, itsperpar, max_parameters, num_parameters, &
                max_results, num_results, PARAM_INFO, CONSTRAINTS, &
                SCALE_FACTORS, PARAMS, PARAM_ERR, RESULTS, RESULT_ERR, &
                RESNAMES, numiter, chisqr, %val(pMDATA), COVARIANCE, status)
 
!           Check status
              Call ST_OUT (status)
 
!           Store end time
              Call IO_TIMES (timend, time_cpu, status)
              format = '(''INFO: Elapse time = '', g14.1, ' // &
                ''' seconds ('', g12.3, '' minutes)'')'
              Write (MESSAGE(1), format) (timend - timstr), (timend - timstr) &
                / 60.0
              Call IO_WRITE (MESSAGE(1), status)
 
!           Create text array of results of fit
              Write (TEXT(1), '(''PARAMETERS OF FIT'')')
              Write (TEXT(2), '('' '')')
              numtext = 2
 
              Write (TEXT(numtext + 1), '('' '')')
              Write (TEXT(numtext + 2), '(''No Quantity  Value      Error'')')
              Write (TEXT(numtext + 3),'('' '')')
              numtext = numtext + 3
 
!           Add parameter values to text
              Do par = 1, num_parameters
                 numtext = numtext + 1
                 Write (TEXT(numtext), '(a10, '' '', 1pe12.5, 1x, 1pe9.2)') &
                   PARNAMES(par), PARAMS(par), PARAM_ERR(par)
              End Do
 
!           Add results to text
              Do par = 1, num_results
                 numtext = numtext + 1
                 Write (TEXT(numtext),'(a10,'' '',1pe10.3)') RESNAMES(par), &
                   RESULTS(par)
              End Do
 
              mxnumdat = xendelm
              mynumdat = yendelm
              mxstrelm = xstrelm
              mystrelm = ystrelm
              mxendelm = xendelm
              myendelm = yendelm
              mtitle = 'FIT MODEL'
              mxlabel = xlabel
              mylabel = ylabel
              mzlabel = zlabel
              Call MA_RCOPY (xmaxdat, 1, %val(pXAXIS), 1, 1, xnumdat, 1, &
                xmaxdat, 1, %val(pMXAXIS), status)
              Call MA_RCOPY (ymaxdat, 1, %val(pYAXIS), 1, 1, ynumdat, 1, &
                ymaxdat, 1, %val(pMYAXIS), status)
 
              memory_exist = .True.
              results_exist = .True.
              command = 'EXIT'
 
           Else
 
!           No parameters have been defined
              Call IO_WRITE ('PARAMETERS MUST BE DEFINED ' // &
                'USING "INPUT PARAMETERS"', status)
              command = 'INPUT PARAMETERS'
           End If
 
        Else If (command .Eq. 'MODEL') Then
 
           If (params_exist) Then
 
!           Set model to zero
              Call MA_RVALUE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, 0.0, %val(pMDATA), status)
 
!           Add each feature to the fit
              Do feature = 1, f2d_num_features
 
                 Call F2D_FEATURE (experiment, feature, &
                   max_parameters, PARAMS, PARAM_INFO, num_parameters, &
                   xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, yendelm, &
                   f2d_x_order, f2d_y_order, f2d_xmin_poly, f2d_ymin_poly, &
                   f2d_xmax_poly, f2d_ymax_poly, %val(pXAXIS), %val(pYAXIS), &
                   %val(pMDATA), status)
 
              End Do
 
              mxnumdat = xendelm
              mynumdat = yendelm
              mxstrelm = xstrelm
              mystrelm = ystrelm
              mxendelm = xendelm
              myendelm = yendelm
              mtitle = 'FIT MODEL'
              mxlabel = xlabel
              mylabel = ylabel
              mzlabel = zlabel
              Call MA_RCOPY (xmaxdat, 1, %val(pXAXIS), 1, 1, xnumdat, 1, &
                xmaxdat, 1, %val(pMXAXIS), status)
              Call MA_RCOPY (ymaxdat, 1, %val(pYAXIS), 1, 1, ynumdat, 1, &
                ymaxdat, 1, %val(pMYAXIS), status)
 
              memory_exist = .True.
              command = 'EXIT'
 
           Else
 
!           No parameters have been defined
              Call IO_WRITE ('WARNING: No model parameters, ' // &
                'they must be defined using "INPUT PARAMETERS"', status)
              command = 'INPUT PARAMETERS'
 
           End If
 
        Else If (command .Eq. 'NORMALISATION') Then
 
!        Allocate dynamic memory
           maxwork = Max(xendelm, yendelm)
           Call IO_MALLOC (maxwork * 4, pTOTAL, status)
           Call IO_MALLOC (maxwork * 4, pSUM, status)
 
!        Uni-directional detector quantum efficiency normalisation
           Call F2D_UNINORMALISE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, variances_exist, %val(pDATA), %val(pMASK), &
             %val(pVARIANCES), maxwork, %val(pTOTAL), %val(pSUM), &
             %val(pMDATA), %val(pMVARIANCES), status)
 
!        Free dynamic memory
           Call IO_FREE (pTOTAL, status)
           Call IO_FREE (pSUM, status)
 
!        Set memory ROI etc.
           mxnumdat = xendelm
           mynumdat = yendelm
           mxstrelm = xstrelm
           mystrelm = ystrelm
           mxendelm = xendelm
           myendelm = yendelm
           mtitle = title
           mxlabel = xlabel
           mylabel = ylabel
           mzlabel = zlabel
           Call MA_RCOPY (xmaxdat, 1, %val(pXAXIS), 1, 1, xnumdat, 1, xmaxdat, &
             1, %val(pMXAXIS), status)
           Call MA_RCOPY (ymaxdat, 1, %val(pYAXIS), 1, 1, ynumdat, 1, ymaxdat, &
             1, %val(pMYAXIS), status)
           memory_exist = .True.
           command = 'EXIT'
 
        Else If (command .Eq. 'OUTPUT PARAMETERS') Then
 
           If (params_exist) Then
 
!           Store fit model parameters and minimisation control
!           parameters
              Call F2D_OUT_PARAMETERS (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                %val(pMASK), max_parameters, Max_results, PARAMS, PARAM_INFO, &
                CONSTRAINTS, PARNAMES, SCALE_FACTORS, RESNAMES, &
                f2d_num_features, num_parameters, num_results, f2d_x_order, &
                f2d_y_order, f2d_xmin_poly, f2d_ymin_poly, f2d_xmax_poly, &
                f2d_ymax_poly, weighted_fit, alpha, itsperpar, evolve, &
                disfreq, fastdis, haltcrit, haltval, status)
 
              command = 'OPTIMISE'
 
           Else
 
!           No parameters have been defined
              Call IO_WRITE ('WARNING: Parameters must be ' // &
                'defined using "INPUT PARAMETERS"', status)
              command = 'INPUT PARAMETERS'
 
           End If
 
        Else If (command .Eq. 'POWDER DIFFRACTION') Then
 
!        Integrate powder diffraction data
           Call F2D_POWDERDIFFRACTION (xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
             %val(pMASK), variances_exist, title, xmaxdat, experiment, &
             lorentz_geometry, mxendelm, &
             %val(pMXAXIS), %val(pMDATA), %val(pMVARIANCES), mx_pixel_size, &
             mtitle, mxlabel, mzlabel, status)
 
!        Set memory ROI etc.
           mxstrelm = 1
           mystrelm = 1
           myendelm = 1
           mxnumdat = mxendelm
           mynumdat = 1
           memory_exist = .True.
           Call IO_RSET (0.5, %val(pMYAXIS), status)
 
           command = 'EXIT'
 
        Else If (command .Eq. 'R/THETA RE-BINNING') Then
 
!        Fit beam centre and tilt angles and calculate radial profile
           Call F2D_RTHETA (title, xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pDATA), %val(pMASK), experiment, &
             xmaxdat, ymaxdat, lorentz_geometry, mxendelm, myendelm, &
             %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), mx_pixel_size, &
             my_pixel_size, mtitle, mxlabel, mylabel, status)
 
!        Set memory ROI etc.
           mxstrelm = 1
           mystrelm = 1
           mxnumdat = mxendelm
           mynumdat = myendelm
           memory_exist = .True.
 
           command = 'EXIT'
 
        Else If (command .Eq. 'RADIAL PROFILE') Then
 
!        Calculate radial profile of data, output in the memory
           Call F2D_RADIAL (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pMASK), &
             variances_exist, %val(pMXAXIS), %val(pMDATA), %val(pMVARIANCES), &
             status)
 
!        Set memory ROI etc.
           mxstrelm = 1
           mystrelm = 1
           mxendelm = xmaxdat
           myendelm = 1
           mtitle = 'Radial Profile'
           mxlabel = xlabel
           mzlabel = zlabel
           mxnumdat = xmaxdat
           mynumdat = 1
           memory_exist = .True.
           Call IO_RSET (0.5, %val(pMYAXIS), status)
 
           command = 'EXIT'
 
        Else If (command .Eq. 'RESULTS') Then
 
           If (results_exist) Then
 
!           Output to screen
              Call IO_TEXT (numtext, TEXT, status)
 
              command = 'EXIT'
           Else
 
!           No results have been defined
              Call IO_WRITE ('RESULTS MUST BE DEFINED USING "OPTIMISE"', status)
              command = 'OPTIMISE'
           End If
 
        Else If (command .Eq. 'SAVE MODEL') Then
 
!        Save the model
           Call F2D_SAVEMODEL (Max_parameters, PARAMS, PARAM_INFO, &
             CONSTRAINTS, PARNAMES, SCALE_FACTORS, f2d_num_features, &
             num_parameters, f2d_x_order, f2d_y_order, f2d_xmin_poly, &
             f2d_ymin_poly, f2d_xmax_poly, f2d_ymax_poly, weighted_fit, alpha, &
             itsperpar, status)
 
           command = 'EXIT'
 
        Else If (command .Eq. 'SET MASK COLOUR') Then
 
!        Set mask colour
           Call F2D_MASKCOLOUR (status)
           command = 'DISPLAY MASK'
 
        Else If (command .Eq. 'SET UP') Then
 
!        Define setup of minimisation control parameters
           Call F2D_SETUP (poisson, mlm, weighted_fit, alpha, itsperpar, &
             evolve, disfreq, contint, reverse, fastdis, haltcrit, haltval, &
             status)
 
           command = 'OPTIMISE'
 
        Else If (command .Eq. 'SILICON CALIBRATION') Then
 
!        Convert beam centre to metre units
           experiment%x_beam = experiment%x_beam * experiment%x_pixel_size
           experiment%y_beam = experiment%y_beam * experiment%y_pixel_size
 
!        Fit beam centre and tilt angles
           Call F2D_CALIBRANT (.False., xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
             %val(pMASK), title, xlabel, ylabel, zlabel, experiment, status)
 
!        Convert beam centre to pixel units
           experiment%x_beam = experiment%x_beam / experiment%x_pixel_size
           experiment%y_beam = experiment%y_beam / experiment%y_pixel_size
 
           command = 'POWDER DIFFRACTION'
 
        Else If (command .Eq. 'SURFACE POLYNOMIAL') Then
 
!        Calculate 2-D Chebyshev polynomial fit to data, output in
!        the memory
           Call F2D_CHEBYSHEV (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, %val(pDATA), %val(pXAXIS), %val(pYAXIS), %val(pMASK), &
             %val(pMDATA), status)
 
!        Set memory ROI etc.
           mxnumdat = xendelm
           mynumdat = yendelm
           mxstrelm = xstrelm
           mystrelm = ystrelm
           mxendelm = xendelm
           myendelm = yendelm
           mtitle = '2-D Chebyshev Polynomial Fit model'
           mxlabel = xlabel
           mylabel = ylabel
           mzlabel = zlabel
           Call MA_RCOPY (xmaxdat, 1, %val(pXAXIS), 1, 1, xnumdat, 1, xmaxdat, &
             1, %val(pMXAXIS), status)
           Call MA_RCOPY (ymaxdat, 1, %val(pYAXIS), 1, 1, ynumdat, 1, ymaxdat, &
             1, %val(pMYAXIS), status)
 
           memory_exist = .True.
           command = 'EXIT'
 
        Else If (command .Eq. 'THRESHOLD MASKING') Then
 
!        Define masked-off elements depending on a user set threshold
!        criterion based on the values of the ROI data elements
           Call F2D_MASKTHRESHOLD (.False., xmaxdat, ymaxdat, xstrelm, &
             ystrelm, xendelm, yendelm, %val(pDATA), %val(pMASK), status)
 
        Else If (command .Eq. 'TILT/BEAM CENTRE') Then
 
!        Fit beam centre and tilt angles
           Call F2D_TILTCENTRE (.False., xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
             %val(pMASK), title, xlabel, ylabel, zlabel, experiment, status)
 
           command = 'POWDER DIFFRACTION'
 
        Else If (command .Eq. 'TRANSFER MASK TO MEMORY') Then
 
           Call F2D_MASKMEMORY ( title, xlabel, ylabel, zlabel, xmaxdat, &
             ymaxdat, xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), &
             %val(pMASK), mtitle, mxlabel, mylabel, mzlabel, mxstrelm, &
             mystrelm, mxendelm, myendelm, mxnumdat, mynumdat, %val(pMXAXIS), &
             %val(pMYAXIS), %val(pMDATA), status)
 
!        Set memory flag.
           memory_exist = .True.
           command = 'EXIT'
 
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
 
     End  Subroutine F2D_FIT
!********1*********2*********3*********4*********5*********6*********7*********8
