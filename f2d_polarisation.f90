!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_polarisation.f90 *
!  *                      *
!  ************************
 
!+ F2D_POLARISATION - FIT2D: POLARISATION effect
     Subroutine F2D_POLARISATION (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, experiment, variances_exist, DATA, VARIANCES, status)
!  Description:
!    Apply polarisation effect to data intensities. This is
!    primarly for simulation purposes.
!  Keywords:
!    Polarisation.Effect.Intensities, Intensities.Polarisation.Effect
!  Method:
!    Applies formula from "Macromolecular Crystallography with
!    Synchrotron Radiation: Photographic Data Collection and
!    Polarization Correction", R Kahn, R Fourme, A Gadet, J Janin,
!    C Dumas, and D Andre, J. Appl. Cryst., (1982), Vol 15, pp330-337
!  Deficiencies:
!    Assumes that the detector is orthogonal to the direct beam.
!    (This could be removed but would complicate the calculations.)
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    14-Mar-2006: V0.3 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    30-Jan-1998: V0.2 Declare unused character string for call to "GS_FORM"
!      (Hammersley)
!    13-Jun-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Logical, Intent(IN) :: variances_exist ! .True., if variance arrays exist
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! The variances of
!      the data values if stored
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
     Integer, Parameter :: Max_choices = 11 ! Maximum number of choices
!      (need 5 extra spaces)
     Integer, Parameter :: Max_chars =  56 ! Maximum number of characters
!      in a line
     Integer, Parameter :: Max_lines =   40 ! Number of lines in message
!  Local Variables:
     Integer :: dummy ! Dummy variable for subroutine interfaces
     Integer :: num_choices ! Number of choices
     Integer :: retstat ! Return status variable for "GS_FORM"
     Integer :: x ! Loop variable for the X-direction
     Integer :: y ! Loop variable for the Y-direction
     Logical, Save :: first = .True. ! .True., if the subroutine is called
!      for the first time
     Real :: azimuth ! Azimuth angle of pixel relative to the direct beam
!      in radians
     Real :: p ! Polarisation intensity multiplier
     Real :: p_dash ! Azimuth dependent component of
!      polarisation intensity multiplier
     Real :: p0 ! Azimuth independent component of
!      polarisation intensity multiplier
     Real :: radial_distance ! Distance from direct beam to pixel centre
!      in metres
     Real :: two_theta ! Two theta angle of pixel relative to direct
!      beam position in radians
     Real :: x_relative ! Relative X-displacement to direct beam in metres
     Real :: xmax_prompt ! The maximum X-coordinate for the GUI region
     Real :: xmin_prompt ! The minimum X-coordinate for the GUI region
     Real :: y_relative ! Relative Y-displacement to direct beam in metres
     Real :: ymax_prompt ! The maximum Y-coordinate for the GUI region
     Real :: ymin_prompt ! The minimum Y-coordinate for the GUI region
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 70) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 60) :: MESSAGE(2) ! User messages
     Character(Len = 70) :: PROMPT(3) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to 'GS_FORM'
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = Max_chars) :: TX(Max_lines) ! Text array
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
     Logical :: BOUND(Max_choices) ! .True., if variable range is bound
     Real :: REALS_LOWER(Max_choices) ! Lower bounds on reals
     Real :: REALS_UPPER(Max_choices) ! Upper bounds on reals
     Real :: REALS(Max_choices) ! Reals to be changed
!  External Functions:
!  Local Data:
     Data TX(   1) / ' ' /
     Data TX(   2) / '      ----------------------------------'/
     Data TX(   3) / '      POLARISATION EFFECT Command Form Help'/
     Data TX(   4) / '      ----------------------------------'/
     Data TX(   5) / ' ' /
     Data TX(   6) / 'The "POLARISATION EFFECT" command allows the effect'/
     Data TX(   7) / 'of polarisation to be applied to reduce intensities'/
     Data TX(   8) / 'of the region of interest of the current data. This'/
     Data TX(   9) / 'is intended for simulating data. This effect applies'/
     Data TX(  10) / 'to Bragg scattered intensity, but not for example to'/
     Data TX(  11) / 'fluorescence, so care may need to be taken in'/
     Data TX(  12) / 'comparing simulated data to real data.'/
     Data TX(  13) / ' ' /
     Data TX(  14) / 'The reduction in intensity at high scattering'/
     Data TX(  15) / 'angles owing to the polarisation is a purely'/
     Data TX(  16) / 'geometrical effect. e.g. the wavelength is not'/
     Data TX(  17) / 'important.'/
     Data TX(  18) / ' ' /
     Data TX(  19) / &
       '"POLARISATION": This is the polarisation factor which is'/
     Data TX(  20) / 'used if the polarisation correction is applied.'/
     Data TX(  21) / ' ' /
     Data TX(  22) / '"X PIXEL SIZE": This is the size of a pixel in the'/
     Data TX(  23) / 'horizontal direction, measured in microns.'/
     Data TX(  24) / ' ' /
     Data TX(  25) / '"Y PIXEL SIZE": This is the size of a pixel in the'/
     Data TX(  26) / 'vertical direction, measured in microns.'/
     Data TX(  27) / ' ' /
     Data TX(  28) / '"DISTANCE": Set the sample to detector distance'/
     Data TX(  29) / 'in millimetres.'/
     Data TX(  30) / ' ' /
     Data TX(  31) / '"X-BEAM CENTRE": Horizontal pixel coordinate of the'/
     Data TX(  32) / 'main beam on the detector plane.'/
     Data TX(  33) / ' ' /
     Data TX(  34) / '"Y-BEAM CENTRE": Horizontal pixel coordinate of the'/
     Data TX(  35) / 'main beam on the detector plane.'/
     Data TX(  36) / ' ' /
     Data TX(  37) / '---------------'/
     Data TX(  38) / 'END OF HELP TEXT'/
     Data TX(  39) / '---------------'/
     Data TX(  40) / ' ' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_POLARISATION ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_POLARISATION ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Find out GUI prompt area
        Call GS_INQ_GUIREGION (xmin_prompt, ymin_prompt, xmax_prompt, &
          ymax_prompt, status)
 
!     Allow user of choose number of output bins and control
!     various other options
        num_choices = 6
        PROMPT(1) = 'CONTROL OF POLARISATION EFFECT'
        PROMPT(2) = 'PARAMETERS'
        BUTTONS(1) = 'POLARISATION'
        BUTTONS(2) = 'X PIXEL SIZE'
        BUTTONS(3) = 'X PIXEL SIZE'
        BUTTONS(4) = 'DISTANCE'
        BUTTONS(5) = 'X-BEAM CENTRE'
        BUTTONS(6) = 'Y-BEAM CENTRE'
 
        TYPES(1) = 3
        TYPES(2) = 3
        TYPES(3) = 3
        TYPES(4) = 3
        TYPES(5) = 3
        TYPES(6) = 3
        TEXT(1) = 'POLARISATION FACTOR'
        TEXT(2) = 'SIZE OF PIXEL IN X-DIRECTION (MICRONS)'
        TEXT(3) = 'SIZE OF PIXEL IN Y-DIRECTION (MICRONS)'
        TEXT(4) = 'SAMPLE TO "DETECTOR" DISTANCE (MM)'
        TEXT(5) = 'X-PIXEL COORDINATE OF DIRECT BEAM'
        TEXT(6) = 'Y-PIXEL COORDINATE OF DIRECT BEAM'
        FULL_PROMPTS(1) = 'Enter ratio of horizontal to ' // &
          'vertical polarisation'
        FULL_PROMPTS(2) = 'Enter horizontal size of pixels in microns'
        FULL_PROMPTS(3) = 'Enter vertical size of pixels in microns'
        FULL_PROMPTS(4) = 'Enter sample to detector distance (mm)'
        FULL_PROMPTS(5) = 'Enter X-pixel coordinate of centre ' // &
          'of the direct beam'
        FULL_PROMPTS(6) = 'Enter Y-pixel coordinate of centre ' // &
          'of the direct beam'
        BOUND(1) = .True.
        BOUND(2) = .True.
        BOUND(3) = .True.
        BOUND(4) = .True.
        BOUND(5) = .False.
        BOUND(6) = .False.
 
        REALS_LOWER(1) = -1.0
        REALS_UPPER(1) = 1.0
        REALS_LOWER(2) = 0.0001
        REALS_UPPER(2) = 1000000.0
        REALS_LOWER(3) = 0.0001
        REALS_UPPER(3) = 1000000.0
        REALS_LOWER(4) = 0.1
        REALS_UPPER(4) = 10000.0
        REALS(1) = experiment%polarisation
        REALS(2) = experiment%x_pixel_size * 1.0e6
        REALS(3) = experiment%y_pixel_size * 1.0e6
        REALS(4) = experiment%detector_distance * 1000.0
        REALS(5) = experiment%x_beam
        REALS(6) = experiment%y_beam
 
!     Output interactive graphical form
        Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, Max_choices, &
          num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, dummy, &
          dummy, REALS_LOWER, REALS_UPPER, dummy, dummy, REALS, STRINGS, &
          retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           Return
        End If
 
!     Set resulting values
        experiment%polarisation = REALS(1)
        experiment%x_pixel_size = REALS(2) / 1.0e6
        experiment%y_pixel_size = REALS(3) / 1.0e6
        experiment%detector_distance = REALS(4) / 1000.0
        experiment%x_beam = REALS(5)
        experiment%y_beam = REALS(6)
 
        experiment%polarisation_set = .True.
        experiment%pixel_sizes_set = .True.
        experiment%beam_centre_set = .True.
        experiment%detector_distance_set = .True.

!     Inform user of calculation and swapping of data to the memory
        MESSAGE(1) = 'WORKING: CALCULATING POLARISATION'
        MESSAGE(2) = 'EFFECT ON INTENSITY VALUES'
        Call GS_PROMPT (.False., 2, 2, MESSAGE, xmin_prompt, ymin_prompt, &
          xmax_prompt, ymax_prompt, status)
 
!     Force output
        Call GS_UPDATE (status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate polarisation factor
        Do y = ystrelm, yendelm
 
           y_relative = (Real(y) - 0.5 - experiment%y_beam) * &
             experiment%y_pixel_size
 
           Do x = xstrelm, xendelm
 
              x_relative = (Real(x) - 0.5 - experiment%x_beam) * &
                experiment%x_pixel_size
 
!           Calculate azimuth and two theta angle of pixel
              radial_distance = Sqrt(x_relative**2 + y_relative**2)
 
              If (radial_distance .Gt. 0.0) Then
                 azimuth = Atan2 (y_relative, x_relative)
                 two_theta = &
                   Atan2 (radial_distance, experiment%detector_distance)
              Else
                 azimuth = 0.0
                 two_theta = 0.0
              End If
 
!           Calculate polarisation effect on pixel intensity
              p0 = 0.5 * (1.0 + (Cos(two_theta))**2)
              p_dash = 0.5 * experiment%polarisation * Cos(2.0 * azimuth) * &
                (Sin(two_theta))**2
 
              p = p0 - p_dash
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*,
!           :             '(''x, y, azimuth, two_theta = '', 4(1pe12.5))')
!           :             x, y, azimuth, two_theta
!           Write (*, '(''p, p0, p_dash = '', 3(1pe12.5))')
!           :             p, p0, p_dash
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              DATA(x, y) = DATA(x, y) * p
 
              If (variances_exist) Then
                 VARIANCES(x, y) = VARIANCES(x, y) * p**2
              End If
 
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_POLARISATION
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
