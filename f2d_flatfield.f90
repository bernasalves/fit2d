!********1*********2*********3*********4*********5*********6*********7**
 
!  *********************
!  *                   *
!  * f2d_flatfield.f90 *
!  *                   *
!  *********************
 
!+ F2D_FLATFIELD: FLAT-FIELD calibration correction
     Subroutine F2D_FLATFIELD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, X_AXIS, Y_AXIS, x_pixel_size, y_pixel_size, DATA, status)
!  Description:
!    Corrects flat-field calibration measurement for anisotropic angular 
!    distribution. If the source angular distribution has been measured by scan
!    with a 0-D detector, the 1/r**2 fall off for a flat detector must be taken 
!    into account. An input "raw" flat-field image is thus corrected (output is 
!    in the current data array, i.e. the original values are over-written).
!  Keywords:
!    Calibrate.Flat-Field, Flat-Field.Calibration
!  Method:
!    1. Input Angle/Intensity coordinates of detector scan from ascii file
!    (1.b Correct for detector dead-time (optional) : removed)
!    (2. Find centre (of symmetry) by centroiding) : removed
!    (3. Re-centre coordinates)
!    (4. Fold coordinates about 0 (symmetry point), doubling number
!    of coordinates)
!    5. Find maximum of blurred function (3-coordinates)
!    6. Normalise function
!    7. Fit polynomial
!    8. Input sample to detector distance, pixel sizes, centre of
!    flat-field measurement, etc.
!    9. Correct flat-field, output in memory, by dividing by source distribution
!    at appropriate angle, and dividing by 1/r**2 fall-off, and dividing by 
!    change in tranmission owing to extra off-axis absorption
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    31-Mar-2006: V0.14 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    24-Feb-1999: V0.13 Change "IO_INFREEFORMAT" to "FIO_IN_FREEFORMAT" 
!      (Hammersley)
!    21-Jan-1998: V0.12 Correct user prompt text (Hammersley)
!    16-Nov-1996: V0.11 Convert to single precision version of "F2D_POLYNOMIAL"
!      (Hammersley)
!    29-Mar-1996: V0.10 Increase size of maximum number of 2-theta scan 
!      coordinates (Hammersley)
!    25-Jan-1996: V0.9 Remove confusing, generally unnecessary, dead time 
!      correction, flipping etc. (Hammersley)
!    19-Dec-1994: V0.8 Correct default size of pixels to microns (Hammersley)
!    10-Nov-1994: V0.7 Re-introduce option to flip scan, around user input 
!      X-coordinate (Hammersley)
!    09-Nov-1994: V0.6 Correct scan for beam decay and allow subtraction of
!      reference scan (Hammersley)
!    31-Oct-1994: V0.5 Add option of dead-time correction to 2-theta scan 
!      count values (Hammersley)
!    24-Jun-1994: V0.4 Remove "MDATA" from argument list, since it is not used
!      (Hammersley)
!    29-Apr-1994: V0.3 Use "IO_INFREEFORMAT" to input X/Y coordinate array, and
!      free dynamic array "POLYNOMIAL" (Hammersley)
!    10-Apr-1994: V0.2 Correct for extra off-axis absorption effects
!      (Hammersley)
!    02-Feb-1994: V0.1 Original (Hammersley)
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
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
!  Import/Export:
     Real, Intent(INOUT) :: x_pixel_size ! Pixel size in metres in X-direction
     Real, Intent(INOUT) :: y_pixel_size ! Pixel size in metres in Y-direction
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The data values, on
!      output the values are replaced by corrected values
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.14' ! Version number
     Integer, Parameter :: Max_coords = 3000 ! Dimension size for coordinate 
!      arrays
     Integer, Parameter :: Max_order = 200 ! Maximum order of fitted polynomial
!  Local Variables:
     Character(Len = 80) :: title ! File name entered
!    Character*(80) title_subtract ! File name entered for reference scan
!    Real correction ! Fractional correction factor at end of scan
     Real, Save :: end_i_beam = 160.0 ! Beam current at end of scan
     Real :: lower_range ! Lower limit of calculated polynomial
     Real, Save :: start_i_beam = 160.0 ! Beam current at start of scan
     Real :: upper_range ! Upper limit of calculated polynomial
     Integer :: lim_order ! Limit to polynomial order
     Integer :: max_pixels ! Dimension size for "PROFILE" array
     Integer :: num_coords ! Number of input coordinates of main scan
!    Integer num_subtract ! Number of input coordinates in reference scan
     Integer :: order ! The order
     Integer stat ! Status return variable for "Allocate"
     Logical, Save :: first = .True. ! .True., if first call to subroutine
!    Logical flip ! .True., if the data has been reflected about a symmetry 
!      point
!    Logical subtract ! .True., if a reference scan is to be input and 
!      subtracted from the main scan
     Real, Save :: absorption = 0.0 ! Fractional on-axis absorption
     Real :: count_time = 10.0 ! Time for counting per data value in the
!      2-theta scan
     Real, Save :: dead_time = 1.0e-6 ! Detector event dead-time in seconds
!      for the  counter used for the 2-theta scan
     Real, Save :: distance ! Distance from source to detector for
!      flat-field measurement
     Real, Save :: x_centre ! X-coordinate of centre of flat-field measurement
     Real, Save :: y_centre ! Y-coordinate of centre of flat-field measurement
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(7) ! User messages
     Real, Allocatable :: COEFFICIENTS(:) ! Dynamic array for storing the 
!      polynomial coefficients
     Real, Allocatable :: POLYNOMIAL(:) ! Dynamic array for storing polynomial 
!      coefficients for required order
     Real, Allocatable :: PROFILE(:) ! Dynamic array for storing reciprocal of 
!      source profile as seen by a flat detector at a given distance
     Real, Allocatable :: RESIDUALS(:) ! Dynamic array for storing the fit 
!      residuals for different polynomial orders
     Real, Allocatable :: X_COORDINATES(:) ! Dynamic array for storing the angle
!      coordinates
!    Real, Allocatable :: X_SUBTRACT(:) ! Dynamic array for storing the 
!      reference intensity coordinates to be subtracted from the main scan
     Real, Allocatable :: WEIGHTS(:) ! Dynamic array for storing the coordinate 
!      weights
     Real, Allocatable :: Y_COORDINATES(:) ! Dynamic array for storing the 
!      intensity coordinates
!    Real, Allocatable :: Y_SUBTRACT(:) ! Dynamic array for storing the 
!      reference intensity coordinates to be subtracted from the main scan
!    Internal Functions:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FLATFIELD ' // Version)
     Else
 
!     Default values for experimental geometry
        If (first) Then
           distance = 0.25
           x_centre = Real(xstrelm + xendelm) / 2.0
           y_centre = Real(ystrelm + yendelm) / 2.0
        End If
 
!     Create dynamic arrays for input of coordinate arrays
        Allocate (X_COORDINATES(max_coords), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FLATFIELD ' // Version)
           Return
        End If
        Allocate (Y_COORDINATES(max_coords), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FLATFIELD ' // Version)
           Return
        End If
        Allocate (WEIGHTS(max_coords), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FLATFIELD ' // Version)
           Return
        End If
        Allocate (COEFFICIENTS((max_order + 1) * (max_order + 1)), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FLATFIELD ' // Version)
           Return
        End If
        Allocate (RESIDUALS((max_order + 1)), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FLATFIELD ' // Version)
           Return
        End If
        Allocate (POLYNOMIAL((max_order + 1)), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FLATFIELD ' // Version)
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Output user information on required scan
        Call IO_WRITE ('INFO: You are required to input  ' // &
          'a  file which contains a list of angles', status)
        Call IO_WRITE ('      in degrees and intensity ' // &
          'values,  for  the  emission of the flood-', status)
        Call IO_WRITE ('      field source as a function of ' // &
          'angle.  This will be used to correct', status)
        Call IO_WRITE ('      the "flood-field" image to  ' // &
          'a  "flat-field" image.  Normally,  the', status)
        Call IO_WRITE ('      angles will vary from ' // &
          'slightly above 0.0 to 30 degrees or so. Zero', status)
        Call IO_WRITE ('      degrees is generally missing ' // &
          'because  of  the beam-stop.  You  are', status)
        Call IO_WRITE ('      advised not  to  input values ' // &
          'which were behind the beam-stop,  as', status)
        Call IO_WRITE ('      this is likely to cause a ' // &
          'dis-continuity in  the data.  This  dis-', status)
        Call IO_WRITE ('      continuity would cause ' // &
          'problems with the fitting and interpolation', status)
        Call IO_WRITE ('      of the data.', status)
        Call IO_WRITE (' ', status)
        Call IO_WRITE ('      (Previous,  various options ' // &
          'were available to treat such scan data', status)
        Call IO_WRITE ('      here.   These  have  been  ' // &
          'removed,  but  the  same facilities are', status)
        Call IO_WRITE ('      available in the main menu.)', status)
 
!     Input Main Angle/Intensity coordinates
        num_coords = 0
        Call FIO_IN_FREEFORMAT ( &
          'Enter name of file containing 1-D 2-theta scan of source', &
          max_coords, num_coords, X_COORDINATES, Y_COORDINATES, &
          title, status)
 
!     Input integration time for each data value
!     MESSAGE(1) = 'Enter the number of seconds used to count ' //
!     :       'for each data point in the'
!     MESSAGE(2) = '2-theta scan.'
!     Call IO_INPR (.True., 0.0, 1e6, .True.,
!     :       'COUNT TIME PER DATA POINT (SECONDS)', 2, MESSAGE,
!     :       1, 'Must be valid real number',
!     :       count_time, status)
 
!     Input user value for detector dead-time
!     MESSAGE(1) = 'You may choose between correcting the raw ' //
!     :       'input count values for'
!     MESSAGE(2) = 'dead-time or not. (Entering an event ' //
!     :       'dead-time of 0.0 will turn off the'
!     MESSAGE(3) = 'dead-time correction.)'
!     MESSAGE(4) = 'Enter the dead time of the detector ' //
!     :       'after it receives a single event.'
!     MESSAGE(5) = 'e.g. If detector is effectively dead ' //
!     :       'for 2 micro-seconds after an event'
!     MESSAGE(6) = 'enter 2e-6. (Note: check that a dead-time ' //
!     :       'correction has not already'
!     MESSAGE(7) = 'been applied to the count values.)'
!     Call IO_INPR (.True., 0.0, 0.1, .True.,
!     :       'EVENT DEAD TIME (SECONDS)', 7, MESSAGE,
!     :       1, 'Must be valid real number',
!     :       dead_time, status)
 
!     If the dead time is greater than 0.0, perform dead-time
!     correction
!     If (dead_time .Gt. 0.0) Then
!     Call F2D_COR_DEADTIME  (max_coords, num_coords,
!     :          count_time, dead_time, Y_COORDINATES, status)
!     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Correct for beam current decay
!     Call IO_INPD (.True., 0.0d0, 10000.0d0, .True.,
!     :       'STARTING BEAM CURRENT (mA)', 1,
!     :       'Enter beam current at start of scan (mA)',
!     :       1, 'Must be valid real number',
!     :       start_i_beam, status)
!     Call IO_INPD (.True., 0.0d0, 10000.0d0, .True.,
!     :       'ENDING BEAM CURRENT (mA)', 1,
!     :       'Enter beam current at end of scan (mA)',
!     :       1, 'Must be valid real number',
!     :       end_i_beam, status)
 
!     Correct for beam decay if values are different
!     If (end_i_beam .Lt. start_i_beam) Then
 
!     correction = start_i_beam / end_i_beam
 
!     Apply 1-D linear interpolated correction
!     Call F2D_FLTFIELD (max_coords, num_coords, correction, &
!       X_COORDINATES, Y_COORDINATES, status)
 
!     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Subtract reference (background) scan data
!     Call IO_INPL (.True., 0, 1, .True.,
!     :       'INPUT AND SUBTRACT REFERENCE SCAN', 1,
!     :       'Enter "YES" to input a reference (background) scan',
!     :       1, 'Enter "YES" or "NO"',
!     :       subtract, status)
 
!     If (subtract) Then
 
!     Input Reference (background) scan Angle/Intensity coordinates
!     num_subtract = 0
!     Call FIO_IN_FREEFORMAT (
!     :          'Enter name of file containing 1-D reference scan',
!     :          max_coords, num_subtract, X_SUBTRACT,
!     :          Y_SUBTRACT, title_subtract, status)
 
!     Check that the two scans are (at least) of the same size
!     If (num_subtract .Ne. num_coords) Then
!     Call IO_WRITE ('ERROR: The number of ' //
!     :             'coordinates in the reference scan is not the same',
!     :             status)
!     Call IO_WRITE ('       as the number in ' //
!     :             'the main scan.', status)
!     Return
!     End If
 
!     Input integration time for each data value
!     MESSAGE(1) = 'Enter the number of seconds used to count '
!     :          // 'for each data point in the'
!     MESSAGE(2) = 'reference scan.'
!     Call IO_INPR (.True., 0.0, 1e6, .True.,
!     :          'COUNT TIME PER DATA POINT (SECONDS)', 2, MESSAGE,
!     :          1, 'Must be valid real number',
!     :          count_time, status)
 
!     Input user value for detector dead-time
!     MESSAGE(1) = 'You may choose between correcting the raw '
!     :          // 'input count values for'
!     MESSAGE(2) = 'dead-time or not. (Entering an event ' //
!     :          'dead-time of 0.0 will turn off the'
!     MESSAGE(3) = 'dead-time correction.)'
!     MESSAGE(4) = 'Enter the dead time of the detector ' //
!     :          'after it receives a single event.'
!     MESSAGE(5) = 'e.g. If detector is effectively dead ' //
!     :          'for 2 micro-seconds after an event'
!     MESSAGE(6) = 'enter 2e-6. (Note: check that a dead-time '
!     :          // 'correction has not already'
!     MESSAGE(7) = 'been applied to the count values.)'
!     Call IO_INPR (.True., 0.0, 0.1, .True.,
!     :          'EVENT DEAD TIME (SECONDS)', 7, MESSAGE,
!     :          1, 'Must be valid real number',
!     :          dead_time, status)
 
!     If the dead time is greater than 0.0, perform dead-time
!     correction
!     If (dead_time .Gt. 0.0) Then
!     Call F2D_COR_DEADTIME  (max_coords, num_subtract,
!     :             count_time, dead_time, Y_SUBTRACT, status)
!     End If
 
!     Correct for beam current decay
!     Call IO_INPD (.True., 0.0d0, 10000.0d0, .True.,
!     :          'STARTING BEAM CURRENT (mA)', 1,
!     :          'Enter beam current at start of scan (mA)',
!     :          1, 'Must be valid real number',
!     :          start_i_beam, status)
!     Call IO_INPD (.True., 0.0d0, 10000.0d0, .True.,
!     :          'ENDING BEAM CURRENT (mA)', 1,
!     :          'Enter beam current at end of scan (mA)',
!     :          1, 'Must be valid real number',
!     :          end_i_beam, status)
 
!     Correct for beam decay if values are different
!     If (end_i_beam .Lt. start_i_beam) Then
 
!     correction = start_i_beam / end_i_beam
 
!     Apply 1-D linear interpolated correction
!     Call F2D_FLTFIELD (max_coords, num_subtract,
!     :             correction, X_SUBTRACT, Y_SUBTRACT, status)
 
!     End If
 
!     Subtract reference scan from main scan
!     Call MA_SUBTRACT (max_coords, 1,
!     :          1, 1, num_coords, 1, Y_SUBTRACT,
!     :          max_coords, 1, Y_COORDINATES, status)
 
!     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Sort coordinates into strictly increasing X-order
        Call MA_SORT (max_coords, num_coords, X_COORDINATES, Y_COORDINATES, &
          status)
 
!     Find centre (X) and adjust X-coordinates to this centre and
!     fold coordinates to account for assumed symmetry about zero
!     if enough space (this doubles the number of coordinates)
!     Call F2D_CENTRALISE (max_coords, num_coords,
!     :       X_COORDINATES, Y_COORDINATES, flip, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''num_coords = '', i)') num_coords
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Limit maximum polynomial order if the data was flipped
!     If (flip) Then
!     lim_order = num_coords / 2
!     Else
        lim_order = 0
!     End If
 
!     Fit coordinates with polynomial
        Call F2D_POLYNOMIAL (lim_order, max_coords, num_coords, &
          X_COORDINATES, Y_COORDINATES, max_order, &
          WEIGHTS, COEFFICIENTS, RESIDUALS, lower_range, &
          upper_range, order, POLYNOMIAL, status)
 
!     Free dynamic array space
        Deallocate (X_COORDINATES)
        Deallocate (Y_COORDINATES)
        Deallocate (WEIGHTS)
        Deallocate (COEFFICIENTS)
        Deallocate (RESIDUALS)
!     Deallocate (X_SUBTRACT)
!     Deallocate (Y_SUBTRACT)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input centre of flat-field on image
        Call IO_INPR (.False., 0.0, 0.0, .True., &
          'FLAT-FIELD CENTRE X-COORDINATE ', 1, &
          'Enter X-coordinate of centre flat-field measurement', 1, &
          'Must be valid real number', x_centre, status)
        Call IO_INPR (.False., 0.0, 0.0, .True., &
          'FLAT-FIELD CENTRE Y-COORDINATE ', 1, &
          'Enter Y-coordinate of centre of flat-field measurement', 1, &
          'Must be valid real number', y_centre, status)
 
!     Input sample to detector distance
        Call IO_WRITE ('NOTE: !!!!!!!!!!!!!!!!!!!!!!!!!!!!' // &
          '!!!!!!!!!!!!!!!!!!!!!!!!!! !!!!!!', status)
        Call IO_WRITE ('NOTE: UNITS OF SAMPLE DETECTOR ' // &
          'DISTANCE HAVE CHANGED TO MILLIMETRES', status)
        Call IO_WRITE ('NOTE: !!!!!!!!!!!!!!!!!!!!!!!!!!!!' // &
          '!!!!!!!!!!!!!!!!!!!!!!!!!! !!!!!!', status)
        distance = distance * 1000.0
        Call IO_INPR (.True., 0.001, 1000000.0, .True., &
          'SAMPLE: DETECTOR DISTANCE (MILLIMETRES)', 1, &
          'Enter distance from centre of sample to detector ' // &
          '(millimetres)', 1, 'Must be valid real number', distance, status)
        distance = distance / 1000.0
 
!     Input pixel sizes
        x_pixel_size = x_pixel_size * 1.0e6
        Call IO_INPR (.True., 0.001, 10000.0, .True., &
          'PIXEL X-SIZE (MICRONS)', 1, &
          'Enter X-size of one pixel in microns ', 1, &
          'Must be valid real number', x_pixel_size, status)
        x_pixel_size = x_pixel_size * 1.0e-6
        y_pixel_size = y_pixel_size * 1.0e6
        Call IO_INPR (.True., 0.001, 10000.0, .True., &
          'PIXEL Y-SIZE (MICRONS)', 1, &
          'Enter Y-size of one pixel in microns ', 1, &
          'Must be valid real number', y_pixel_size, status)
        y_pixel_size = y_pixel_size * 1.0e-6
 
!     Input On-axis absorption
        MESSAGE(1) = 'Enter fractional absorption; to be ' // &
          'used to calculate off-axis absorption'
        MESSAGE(2) = 'correction. If no off-axis correction ' // &
          'is required enter 0.0. If'
        MESSAGE(3) = 'off-axis correction is required enter ' // &
          'the on-axis axis absorption as a'
        MESSAGE(4) = 'fraction e.g. If the transmission is ' // &
          '80% enter 0.2 for the absorption.'
        Call IO_INPR (.True., 0.0, 1.0, .True., &
          'ON-AXIS ABSORPTION (FOR OFF-AXIS CORRECTION)', 4, MESSAGE, 1, &
          'Must be valid real number', absorption, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Calculate maximum distance from flat-field centre to edge of
!     flat-field image (in pixels)
        max_pixels = Int(Max( Sqrt((x_centre - Real(xstrelm))**2 + (y_centre - &
          Real(ystrelm)) **2), Sqrt((Real(xendelm) - x_centre)**2 + (y_centre &
          - Real(ystrelm))**2), Sqrt((x_centre - Real(xstrelm))**2 + &
          (Real(yendelm) - y_centre) **2), Sqrt((Real(xendelm) - x_centre)**2 &
          + (Real(yendelm) - y_centre) **2))) + 1
 
!     Create dynamic array to hold 1-D source distribution and
!     geometry correction profile
        Allocate (PROFILE(max_pixels + 1), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FLATFIELD ' // Version)
           Return
        End If
 
!     Calculate 1-D source distribution and geometry correction
!     profile
        Call F2D_CALPROFILE (lower_range, upper_range, max_order, order, &
          POLYNOMIAL, distance, x_pixel_size, absorption, max_pixels, &
          PROFILE, status)
 
!     Apply correcting profile to input flat-field measurement
        Call F2D_CALFLATFIELD (max_pixels, PROFILE, x_centre, y_centre, &
          x_pixel_size, y_pixel_size, xmaxdat, ymaxdat, xstrelm, ystrelm, &
          xendelm, yendelm, DATA, status)
 
!     Free dynamic polynomial and profile arrays
        Deallocate (POLYNOMIAL)
        Deallocate (PROFILE)
 
!     User reminder
        Call IO_WRITE ('NOTE: Corrected flat-field is in ' // &
          'the current data array (don''t EXCHANGE)', status)
 
     End If
 
     End Subroutine F2D_FLATFIELD
!********1*********2*********3*********4*********5*********6*********7**
!+ F2D_FLTFIELD: FLaT-FIELD sub-call
     Subroutine F2D_FLTFIELD (max_coords, num_subtract, correction, &
       X_SUBTRACT, Y_SUBTRACT, status)
!  Description:
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!  Type Definitions:
     Implicit None
!  Global Constants:
!    Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: max_coords ! Dimension size for data arrays
     Integer, Intent(IN) :: num_subtract ! Number of subtraction coordinates
     Real, Intent(IN) :: correction ! Fractional correction factor at end of 
!      scan
     Real, Intent(IN) :: X_SUBTRACT(max_coords) ! For storing the reference 
!      intensity coordinates to be subtracted from the main scan
!  Import/Export:
     Real, Intent(INOUT) :: Y_SUBTRACT(max_coords) ! Input/Corrected
!    intensities
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
!  Local Variables:
!--------1---------2---------3---------4---------5---------6---------7--
!  Call MA routine
     Call MA_1DINTERCORR (max_coords, 1, num_subtract, X_SUBTRACT(1), &
       X_SUBTRACT(num_subtract), correction, X_SUBTRACT, Y_SUBTRACT, status)
 
     End Subroutine F2D_FLTFIELD
!********1*********2*********3*********4*********5*********6*********7**
 
 
 
 
 
 
