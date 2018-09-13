!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_2thetatorings.f90 *
!  *                       *
!  *************************
 
!+ F2D_2THETATORINGS - FIT2D: 2-THETA angles TO powder RINGS
     Subroutine F2D_2THETATORINGS (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, title, xlabel, ylabel, zlabel, XAXIS, YAXIS, DATA, &
       experiment, apply_polarisation, memory_defined, mtitle, &
       mxlabel, mylabel, mzlabel, mxnumdat, mynumdat, mxstrelm, mystrelm, &
       mxendelm, myendelm, MXAXIS, MYAXIS, MDATA, mx_pixel_size, &
       my_pixel_size, status)
!  Description:
!    Simulate a powder pattern from a 2-theta scan
!  Keywords:
!    Simulate.Powder~Pattern.2-theta~Scan,
!    Powder~Pattern.Simulate.2-theta~Scan
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    13-Mar-2006: V0.5 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    30-Jan-1998: V0.4 Declare unused character string for call to "GS_FORM" 
!      (Hammersley)
!    11-Jul-1997: V0.3 Correction to output intensity, multiple by 
!      Cos(2theta)**3 instead of divide (Hammersley)
!    10-Jul-1997: V0.2 Correction to linear interpolation. Add
!      progress report messages (Hammersley)
!    02-Jul-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointer to program arrays
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis values, should be degrees
!      for a 2-theta scan in the X-direction
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis values, should be degrees
!      for a 2-theta scan in the Y-direction
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Logical, Intent(INOUT) :: apply_polarisation ! .True., if the
!      polarisation effect is to be applied
!  Export:
     Logical, Intent(OUT) :: memory_defined ! .True., if the memory arrays
!      are defined to contain data
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
     Real, Intent(OUT) :: MXAXIS(xmaxdat) ! Array containing data
!      X-coordinates
     Real, Intent(OUT) :: MYAXIS(ymaxdat) ! Array containing data
!      Y-coordinates
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Array containing data to
!      be fitted
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data
!      region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data
!      region
     Real, Intent(OUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(OUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
     Integer, Parameter :: Max_choices = 18 ! Maximum number of choices
!      (need 5 extra spaces)
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
!  Local Variables:
     Integer :: lower ! Lower bracket for search for two theta angle bracketing 
!      indices
     Integer :: num_choices ! Number of choices
     Integer :: pixel ! Loop variable for pixels
     Integer :: retstat ! Return status variable for "GS_FORM"
     Integer :: upper ! Lower bracket for search for two theta angle bracketing 
!      indices
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Logical :: continue ! .True., while searching for bracketing
!      indices for a 2-theta angle
     Real :: azimuth ! The azimuth angle of a pixel
     Real :: radial_distance ! Tilt corrected radial distance from the
!      radial symmetry centre to the data value position
     Real :: cos_rotation ! Cosine of the tilt plane rotation angle
     Real :: cos_tilt_sqr ! The square of the cosine of the tilt angle
     Real :: cos2_2theta ! The square of the cosine of the 2-theta angle
     Real :: intensity ! Intensity to put in a output pixel
     Real :: polarisation_effect ! Polarisation effect; change in
!      intensity owing to polarisation
     Real :: sin_rotation ! Sine of the tilt plane rotation angle
     Real :: sin_tilt ! Sine of the tilt angle
     Real :: two_theta ! Angle (two-theta) of pixel relative to direct
!      beam in radians
     Real :: two_theta_degrees ! Angle (two-theta) of pixel relative
!      to direct beam in degrees
     Real :: x_rotate ! The rotated X-coordinate
     Real :: xwc ! X-world coordinate of data point
     Real :: y_rotate ! The rotated Y-coordinate
     Real :: ywc ! Y-world coordinate of data point
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 70) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 60) :: MESSAGE(2) ! User messages
     Character(Len = 70) :: PROMPT(3) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Integer :: INTEGERS(Max_choices) ! Integer variables to be input
     Integer :: INTS_LOWER(Max_choices) ! Lower bound of integer variables
     Integer :: INTS_UPPER(Max_choices) ! Upper bound of integer variables
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
     Logical :: BOUND(Max_choices) ! .True., if variable range is bound
     Logical :: LOGICALS(Max_choices) ! Logical variables to be input
     Real :: REALS_LOWER(Max_choices) ! Lower bounds on reals
     Real :: REALS_UPPER(Max_choices) ! Upper bounds on reals
     Real :: REALS(Max_choices) ! Reals to be changed
!  External Functions:
!    trailing blanks
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_2THETATORINGS ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_2THETATORINGS ' // Version)
     Else
 
!     Check that the scan is a horizontal one
        If (xendelm - xstrelm .Le. 1 .Or. yendelm - ystrelm .Gt. 1) Then
 
           Call IO_WRITE ('WARNING: The 2-theta scan needs ' // &
             'to be defined in the X-direction', status)
           Call IO_WRITE ('         whereas the input data ' // &
             'appears to be defined in the', status)
           Call IO_WRITE ('         Y-direction. Maybe you ' // &
             'need to use the "TRANSPOSE" command', status)
           Call IO_WRITE ('         in the keyboard ' // &
             'interface main menu.', status)
           Return
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Allow user of choose number of output bins and control
!     various other options
        num_choices = 13
        PROMPT(1) = 'CONTROL OF POWDER PATTERN'
        PROMPT(2) = 'SIMULATION FROM A 2-THETA SCAN'
        BUTTONS(1) = 'X-START PIXEL'
        BUTTONS(2) = 'Y-START PIXEL'
        BUTTONS(3) = 'X-END PIXEL'
        BUTTONS(4) = 'Y-END PIXEL'
        BUTTONS(5) = 'X-PIXEL SIZE'
        BUTTONS(6) = 'Y-PIXEL SIZE'
        BUTTONS(7) = 'POLARISATION'
        BUTTONS(8) = 'FACTOR'
        BUTTONS(9) = 'DISTANCE'
        BUTTONS(10) = 'X-BEAM CENTRE'
        BUTTONS(11) = 'Y-BEAM CENTRE'
        BUTTONS(12) = 'TILT PLANE'
        BUTTONS(13) = 'TILT'
 
        TYPES(1) = 1
        TYPES(2) = 1
        TYPES(3) = 1
        TYPES(4) = 1
        TYPES(5) = 3
        TYPES(6) = 3
        TYPES(7) = 2
        TYPES(8) = 3
        TYPES(9) = 3
        TYPES(10) = 3
        TYPES(11) = 3
        TYPES(12) = 3
        TYPES(13) = 3
        TEXT(1) = 'X-PIXEL FOR START OF OUTPUT REGION'
        TEXT(2) = 'Y-PIXEL FOR START OF OUTPUT REGION'
        TEXT(3) = 'X-PIXEL FOR END OF OUTPUT REGION'
        TEXT(4) = 'Y-PIXEL FOR END OF OUTPUT REGION'
        TEXT(5) = 'X-PIXEL SIZE (HORIZONTAL) IN MICRONS'
        TEXT(6) = 'Y-PIXEL SIZE (VERTICAL) IN MICRONS'
        TEXT(7) = 'APPLY POLARISATION EFFECT'
        TEXT(8) = 'POLARISATION FACTOR'
        TEXT(9) = 'SAMPLE TO "DETECTOR" DISTANCE (MM)'
        TEXT(10) = 'X-PIXEL COORDINATE OF DIRECT BEAM'
        TEXT(11) = 'Y-PIXEL COORDINATE OF DIRECT BEAM'
        TEXT(12) = 'ROTATION ANGLE OF PLANE OF TILT (DEGREES)'
        TEXT(13) = 'TILT ANGLE OF DETECTOR (DEGREES)'
        FULL_PROMPTS(1) = 'Enter X-pixel number for start of ' // &
          'output region'
        FULL_PROMPTS(2) = 'Enter Y-pixel number for start of ' // &
          'output region'
        FULL_PROMPTS(3) = 'Enter X-pixel number for end of ' // &
          'output region'
        FULL_PROMPTS(4) = 'Enter Y-pixel number for end of ' // &
          'output region'
        FULL_PROMPTS(5) = 'Enter size of pixel in the ' // &
          'X-direction (horizontal) in microns'
        FULL_PROMPTS(6) = 'Enter size of pixel in the ' // &
          'Y-direction (vertical) in microns'
        FULL_PROMPTS(7) = 'Enter "YES" to apply polarisation effect'
        FULL_PROMPTS(8) = 'Enter ratio of horizontal to ' // &
          'vertical polarisation ((I_h - I_v) / (I_h + I_v))'
        FULL_PROMPTS(9) = 'Enter sample to detector distance (mm)'
        FULL_PROMPTS(10) = 'Enter X-pixel coordinate of centre ' // &
          'of the direct beam'
        FULL_PROMPTS(11) = 'Enter Y-pixel coordinate of centre ' // &
          'of the direct beam'
        FULL_PROMPTS(12) = 'Enter rotation angle anti-clockwise ' // &
          'from the X-axis of the plane of tilt (degrees)'
        FULL_PROMPTS(13) = 'Enter angle of tilt of the detector ' // &
          ' in the tilt plane (degrees)'
        BOUND(1) = .True.
        BOUND(2) = .True.
        BOUND(3) = .True.
        BOUND(4) = .True.
        BOUND(5) = .True.
        BOUND(6) = .True.
        BOUND(7) = .True.
        BOUND(8) = .True.
        BOUND(9) = .True.
        BOUND(10) = .False.
        BOUND(11) = .False.
        BOUND(12) = .True.
        BOUND(13) = .True.
 
        INTS_LOWER(1) = 1
        INTS_UPPER(1) = xmaxdat
        INTEGERS(1) = 1
        INTS_LOWER(2) = 1
        INTS_UPPER(2) = ymaxdat
        INTEGERS(2) = 1
        INTS_LOWER(3) = 1
        INTS_UPPER(3) = xmaxdat
        INTEGERS(3) = xmaxdat
        INTS_LOWER(4) = 1
        INTS_UPPER(4) = ymaxdat
        INTEGERS(4) = ymaxdat
        REALS_LOWER(5) = 0.001
        REALS_UPPER(5) = 1.0e6
        REALS(5) = mx_pixel_size * 1.0e6
        REALS_LOWER(6) = 0.001
        REALS_UPPER(6) = 1.0e6
        REALS(6) = my_pixel_size * 1.0e6
        LOGICALS(7) = apply_polarisation
        REALS_LOWER(8) = -1.0
        REALS_UPPER(8) = 1.0
        REALS(8) = experiment%polarisation
        REALS_LOWER(9) = 0.1
        REALS_UPPER(9) = 10000.0
        REALS(9) = experiment%detector_distance*1000.0
        REALS(10) = experiment%x_beam
        REALS(11) = experiment%y_beam
        REALS_LOWER(12) = -360.0
        REALS_UPPER(12) = 360.0
        REALS(12) = experiment%tilt_plane_rotation * 180.0 / Pi
        REALS_LOWER(13) = -90.0
        REALS_UPPER(13) = 90.0
        REALS(13) = experiment%tilt_angle * 180.0 / Pi
 
!     Output interactive graphical form
        Call GS_FORM (2, 2, PROMPT, 1, 1, 'HELP TEXT; NOT YET AVAILABLE', &
          Max_choices, num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, &
          INTS_LOWER, INTS_UPPER, REALS_LOWER, REALS_UPPER, INTEGERS, &
          LOGICALS, REALS, STRINGS, retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           Return
        End If
 
!     Set resulting values
        mxstrelm = INTEGERS(1)
        mystrelm = INTEGERS(2)
        mxendelm = INTEGERS(3)
        myendelm = INTEGERS(4)
        mx_pixel_size = REALS(5) / 1.0e6
        my_pixel_size = REALS(6) / 1.0e6
        apply_polarisation = LOGICALS(7)
        experiment%polarisation = REALS(8)
        experiment%detector_distance = REALS(9) / 1000.0
        experiment%x_beam = REALS(10)
        experiment%y_beam = REALS(11)
        experiment%tilt_plane_rotation = REALS(12) * Pi / 180.0
        experiment%tilt_angle = REALS(13) * Pi / 180.0
 
!     Inform user of calculation and swapping of data to the memory
        MESSAGE(1) = 'WORKING: SIMULATING PATTERN'
        MESSAGE(2) = 'NOTE: ORIGINAL DATA STORED IN THE MEMORY'
        Call GS_FPROMPT (2, 2, MESSAGE, status)
 
!     Force output
        Call GS_UPDATE (status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate cosines/sines of rotation angles
        cos_rotation = Cos(-experiment%tilt_plane_rotation)
        sin_rotation = Sin(-experiment%tilt_plane_rotation)
        cos_tilt_sqr = (Cos(experiment%tilt_angle))**2
        sin_tilt = Sin(experiment%tilt_angle)
 
!     Set initial bracketing indices
        lower = xstrelm
        upper = xendelm
 
!     Calculate output powder pattern
        Do y = mystrelm, myendelm
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''y = '', i6)') y
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           ywc = (Real(y) - 0.5 - experiment%y_beam) * my_pixel_size
 
           Do x = mxstrelm, mxendelm
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           If (y .Gt. 229) Then
!           Write (*, '(''x, y = '', 2i6)') x, y
!           End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              xwc = (Real(x) - 0.5 - experiment%x_beam) * mx_pixel_size
 
!           Rotate coordinate to "tilt plane"
              x_rotate = cos_rotation * xwc - sin_rotation * ywc
              y_rotate = sin_rotation * xwc + cos_rotation * ywc
 
!           Calculate "x" vector in ideal detector plane
              radial_distance = experiment%detector_distance * &
                Sqrt( (cos_tilt_sqr * x_rotate**2 + y_rotate**2) / &
                (experiment%detector_distance + sin_tilt * x_rotate)**2)
 
!           Calculate angle at centre of pixel
              two_theta = Atan2(radial_distance, experiment%detector_distance)
              two_theta_degrees = two_theta * 180.0 / Pi
 
!           Find bracketing angles in 2-theta scan
              continue = .True.
              Do While (continue)
 
                 If (XAXIS(lower) .Le. two_theta_degrees .And. XAXIS(upper) &
                   .Gt. two_theta_degrees) Then
 
                    If (upper - lower .Eq. 1) Then
                       continue = .False.
                    Else If (XAXIS(upper - 1) .Gt. two_theta_degrees) Then
                       upper = upper - 1
                    Else If (XAXIS(lower + 1) .Le. two_theta_degrees) Then
                       lower = lower + 1
                    End If
 
                 Else If (XAXIS(lower) .Le. two_theta_degrees) Then
                    upper = upper + 1
                 Else
                    lower = lower - 1
                 End If
 
                 If (lower .Lt. xstrelm) Then
                    lower = xstrelm
                    upper = xstrelm
                    continue = .False.
                 Else If (upper .Gt. xendelm) Then
                    lower = xendelm
                    upper = xendelm
                    continue = .False.
                 End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              If (x .Eq. 252 .And. y .Eq. 229) Then
!              Write (*, '(''lower, upper = '', 2i6)')
!              :                   lower, upper
!              End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              End Do
 
!           Set 2-theta scan intensity
              If (lower .Eq. upper) Then
                 intensity = DATA(lower, ystrelm)
              Else
 
!              Linear interpolation
                 intensity = ((two_theta_degrees - XAXIS(lower)) * &
                   DATA(upper, ystrelm) + (XAXIS(upper) - two_theta_degrees) * &
                   DATA(lower, ystrelm)) / (XAXIS(upper) - XAXIS(lower))
 
              End If
 
!           Account for Cos**3(2-theta) drop off in intensity
              intensity = intensity * Cos(two_theta)**3
 
!           Account for polarisation effect
              If (apply_polarisation) Then
 
!              Calculate square of Cosine of 2 theta angle
                 cos2_2theta = experiment%detector_distance**2 / &
                   (experiment%detector_distance**2 + radial_distance**2)
 
!              Calculate azimuth angle of pixel
!              WARNING THIS DOES NOT TAKE INTO ACCOUNT TILT !!!!!!!!!
                 azimuth = Atan2(ywc, xwc)
 
                 polarisation_effect = 0.5 * ( 1.0 + cos2_2theta - &
                   experiment%polarisation * Cos(2.0 * azimuth) * &
                   (1.0 - cos2_2theta))
 
                 intensity = intensity * polarisation_effect
 
              End If
 
!           Set output pixel intensity
              MDATA(x, y) = intensity
 
           End Do
 
!        Output progress report
           If (Mod(y - mystrelm + 1, 500) .Eq. 0) Then
 
              Write (message, '(''INFO: Number of rows treated = '', ' // &
                'i6, '' ('', i3, ''%)'')') y - mystrelm + 1, Int(100.0 * &
                Real(y - mystrelm + 1) / Real(myendelm - mystrelm + 1))
              Call IO_WRITE (message, status)
 
           End If
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Set number defined memory elements
        mxnumdat = mxendelm
        mynumdat = myendelm
 
!     Define axis data
        Do pixel = 1, mxendelm
           MXAXIS(pixel) = Real(pixel) - 0.5
        End Do
 
        Do pixel = 1, myendelm
           MYAXIS(pixel) = Real(pixel) - 0.5
        End Do
 
!     Set title and labels
        mtitle = Trim(title)
        mxlabel = 'X-direction'
        mylabel = 'Y-direction'
        mzlabel = 'Intensity'
 
!     Set memory to exist and set memory ROI
        memory_defined = .True.
 
     End If
 
     End Subroutine F2D_2THETATORINGS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
