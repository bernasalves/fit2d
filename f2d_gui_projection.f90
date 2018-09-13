!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_gui_projection.f90 *
!  *                        *
!  **************************
 
!+ F2D_GUI_PROJECTION - FIT2D: 2-D to 1-D PROJECTION
     Subroutine F2D_GUI_PROJECTION (mask_data, xmaxdat, ymaxdat, title, &
       xlabel, ylabel, zlabel, variances_exist, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, DATA, MASK, xstrelm, ystrelm, xendelm, yendelm, experiment, &
       memory_defined, mtitle, mxlabel, mylabel, mzlabel, &
       mxnumdat, mynumdat, MX_AXIS, MDATA, MVARIANCES, mxstrelm, mystrelm, &
       mxendelm, myendelm, mx_pixel_size, my_pixel_size, status)
!  Description:
!  Keywords:
!    2-theta~Scan.Re-binning, Re-binning.2-theta~Scan
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    14-Mar-2006: V0.8 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    23-Feb-1999: V0.7 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.6 Change to use IO internal database routines (Hammersley)
!    30-Jan-1998: V0.5 Declare unused character string for call to "GS_FORM"
!      (Hammersley)
!    18-Apr-1998: V0.4 Add tilt correction to projection (Hammersley)
!    16-Apr-1998: V0.3 User control of maximum D-spacing value (Hammersley)
!    15-Apr-1998: V0.2 Call "F2D_CAL_GISAXS" (Hammersley)
!    31-Mar-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointer to program arrays
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
!  Import/Export:
     Logical, Intent(INOUT) :: mask_data ! .True., if the data is to be masked
     Character(Len = *), Intent(INOUT) :: title ! Title for plot
     Character(Len = *), Intent(INOUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: zlabel ! Z-axis label for plot
     Logical, Intent(INOUT) :: variances_exist ! .True., if error arrays exist
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements defined in
!      Y-direction
     Real, Intent(INOUT) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(INOUT) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! The data mask
     Integer, Intent(INOUT) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: xendelm ! The X-pixel number for the end of
!      the ROI
     Integer, Intent(INOUT) :: yendelm ! The Y-pixel number for the end of
!      the ROI
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
     Logical, Intent(OUT) :: memory_defined ! .True., if the memory arrays
!      are defined to contain data
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
     Real, Intent(OUT) :: MX_AXIS(xmaxdat) ! Projected 1-D scan data
!      X-coordinates
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Projected data
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat) ! Projected data
!      variances
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
     Real, Intent(OUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(OUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
     Integer, Parameter :: Max_choices = 9 ! Maximum number of choices
!      (need 5 extra spaces)
!  Local Variables:
     Integer :: num_bins ! Number of output bins
     Integer :: num_choices ! Number of form choices
     Integer :: pNORMALISATION ! Pointer to dynamically allocated array
!      for storing fractional pixel contributions and for normalisation
     Integer :: retstat ! Return status variable
     Integer, Save :: scan_type = 2 ! Type of 1-D scan to be produced:
!      0 = Radial: equal distance scan
!      1 = "2-theta": equal angle element scan
!      2 = Q-space: Equal Q element scan
!          (Q = (4 * Pi / wavelength) Sin(theta/2) )
!      3 = D-spacing scan
     Logical, Save :: geometrical_correction = .True. ! .True., if a geometrical
!      correction is to be applied to correct for the 1/r**2
!      effect and obliqueness effects of the flat plate geometry
     Logical, Save :: scan_summation = .False. ! .True., if an approximate
!      sum of the projected region is to be calculated, otherwise the output
!      is normalised by the number of contributing pixels
     Real :: bin_size ! Size of an output bin
     Real :: distance ! Distance in metres between ends of projection
     Real, Save :: maximum_d = 50.0 ! Maximum D-spacing to calculate for
!      D-spacing scans
     Real :: scan_width ! Width of scan in pixels
!  Local Arrays:
     Character(Len = 20) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 70) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 70) :: PROMPT(3) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to 'GS_FORM'
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = 60) :: TX(10) ! Text array
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
     Real :: X_REGION(5) ! X-coordinates of projection region
     Real :: Y_REGION(5) ! X-coordinates of projection region
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entry to F2D_GUI_PROJECTION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_PROJECTION ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_PROJECTION ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Find any previously set user preferences
        Call IO_INQ_IKEYVALUE ('SCAN_TYPE', scan_type, retstat, status)
        Call IO_INQ_RKEYVALUE ('MAXIMUM_D', maximum_d, retstat, status)
        Call IO_INQ_LKEYVALUE ('SCAN_SUMMATION', scan_summation, retstat, &
          status)
        Call IO_INQ_LKEYVALUE ('GEOMETRICAL_CORRECTION', &
          geometrical_correction, retstat, status)
 
!     Define projection region
        Call F2D_INP_PROJECTION (.True., .True., &
          experiment%x_beam, experiment%y_beam, xmaxdat, &
          ymaxdat, xstrelm, ystrelm, xendelm, yendelm, X_AXIS, Y_AXIS, DATA, &
          MASK, title, xlabel, ylabel, zlabel, &
          experiment%x_pixel_size, experiment%y_pixel_size, &
          X_REGION, Y_REGION, status)
 
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Allow experimental geometry to be changed
        Call F2D_GUI_EXPERIMENT (.False., experiment, status)
 
!     Check status
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Integration / Projection Control form
        num_choices = 4
        PROMPT(1) = 'PROJECTION 1-D SCAN'
        PROMPT(2) = 'CONTROL FORM'
 
        BUTTONS(1) = 'SCAN TYPE'
        BUTTONS(2) = 'SUMMATION'
        BUTTONS(3) = 'GEOM. CORR.'
        BUTTONS(4) = 'MAX. D-SPACING'
        TYPES(1) = Gs_scan_type
        TYPES(2) = Gs_logical
        TYPES(3) = Gs_logical
        TYPES(4) = Gs_real
        TEXT(1) = 'SCAN TYPE (D, RADIAL, 2-THETA, Q-SPACE)'
        TEXT(2) = 'OUTPUT SUMMATION (NO NORMALISATION)'
        TEXT(3) = 'CORRECT FOR GEOMETRICAL 1/Cos**3(o) EFFECT'
        TEXT(4) = 'MAXIMUM FOR D-SPACINGS SCANS (ANGSTROMS)'
        FULL_PROMPTS(1) = 'Select type of output 1-D scan: ' // &
          '"RADIAL", "2-THETA", D, or "Q-SPACE"'
        FULL_PROMPTS(2) = '"YES" to output summation (approx), ' // &
          'of projected region, "NO" to normalise'
        FULL_PROMPTS(3) = '"YES" to applied intensity correction ' // &
          'to flat plate geometry'
        FULL_PROMPTS(4) = 'Enter maximum of range for ' // &
          'calculation of D-spacing scans (Angstroms)'
        BOUND(1) = .True.
        BOUND(2) = .True.
        BOUND(3) = .True.
        BOUND(4) = .True.
        INTEGERS(1) = scan_type
        LOGICALS(2) = scan_summation
        LOGICALS(3) = geometrical_correction
        REALS(4) = maximum_d
        REALS_LOWER(4) = 1.0
        REALS_UPPER(4) = 10000.0
 
!     Output interactive graphical form
        TX(1) = 'PROJECTION RE-BINNING CONTROL FORM'
        TX(2) = ' '
        TX(3) = '(under development)'
 
        Call GS_FORM (2, 2, PROMPT, 3, 3, TX, Max_choices, num_choices, &
          BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, INTS_LOWER, INTS_UPPER, &
          REALS_LOWER, REALS_UPPER, INTEGERS, LOGICALS, REALS, STRINGS, &
          retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           status = St_escapevalue
           Return
        End If
 
!     Set resulting values
        scan_type = INTEGERS(1)
        scan_summation = LOGICALS(2)
        geometrical_correction = LOGICALS(3)
        maximum_d = REALS(4)
 
!     Set user preferences
        Call IO_SET_IKEYVALUE ('SCAN_TYPE', scan_type, retstat, status)
        Call IO_SET_RKEYVALUE ('MAXIMUM_D', maximum_d, retstat, status)
        Call IO_SET_LKEYVALUE ('SCAN_SUMMATION', scan_summation, retstat, &
          status)
        Call IO_SET_LKEYVALUE ('GEOMETRICAL_CORRECTION', &
          geometrical_correction, retstat, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate number of bins and output bin size
        distance = Sqrt( ((X_REGION(2) - X_REGION(1)) * &
          experiment%x_pixel_size)**2 + &
          ((Y_REGION(2) - Y_REGION(1)) * experiment%y_pixel_size)**2)
        num_bins = Int(distance / &
          (0.5 * (experiment%x_pixel_size + experiment%y_pixel_size))) + 1
 
!     Make the number of bins even
        num_bins = ((num_bins + 1) / 2) * 2
        num_bins = Min(num_bins, xmaxdat)
 
        If (scan_type .Eq. 0) Then
           bin_size = 0.5 * (experiment%x_pixel_size + experiment%y_pixel_size)
        Else If (scan_type .Eq. 1) Then
           bin_size = Atan2(0.5 * (experiment%x_pixel_size + &
             experiment%y_pixel_size), &
             experiment%detector_distance) * 180.0 / Pi
        Else If (scan_type .Eq. 2) Then
           bin_size = (4.0 * Pi / (experiment%wavelength * 1.0e9)) * &
             Sin(Atan2(0.5 * &
             (experiment%x_pixel_size + experiment%y_pixel_size), &
             experiment%detector_distance) / 2.0)
        Else If (scan_type .Eq. 3) Then
           bin_size = maximum_d / Real(num_bins)
!        bin_size = wavelength * 1.0e10 / (2.0 *
!        :          Sin( Atan2(0.5 * (x_pixel_size + y_pixel_size),
!        :          sample_distance) / 2.0))
        End If
 
!     Bin size and number form
        num_choices = 2
        PROMPT(1) = 'OUTPUT BIN SIZE'
        PROMPT(2) = 'CONTROL FORM'
 
        BUTTONS(1) = 'BIN SIZE'
        BUTTONS(2) = 'NO. BINS'
 
        TYPES(1) = Gs_real
        TYPES(2) = Gs_integer
 
        If (scan_type .Eq. 0) Then
           TEXT(1) = 'SIZE OF OUTPUT BINS (microns)'
           FULL_PROMPTS(1) = 'Enter size of output pixels ' // &
             'in millimetres for a radial distance scan'
           REALS(1) = bin_size * 1.0e6
        Else If (scan_type .Eq. 1) Then
           TEXT(1) = 'SIZE OF OUTPUT BINS (degrees)'
           FULL_PROMPTS(1) = 'Enter size of output pixels ' // &
             'in degrees for a 2-theta scan'
           REALS(1) = bin_size
        Else If (scan_type .Eq. 2) Then
           TEXT(1) = 'SIZE OF OUTPUT BINS (Inverse nanometres)'
           FULL_PROMPTS(1) = 'Enter size of output pixels ' // &
             'in inverse nanometres for a Q-space scan'
           REALS(1) = bin_size
        Else If (scan_type .Eq. 3) Then
           TEXT(1) = 'SIZE OF OUTPUT BINS (Angstroms)'
           FULL_PROMPTS(1) = 'Enter size of output pixels ' // &
             'in Angstroms for a D-spacings scan'
           REALS(1) = bin_size
        End If
 
        TEXT(2) = 'NUMBER OF OUTPUT SCAN BINS'
        FULL_PROMPTS(2) = 'Enter number of bins' // 'in output scan'
        BOUND(1) = .True.
        BOUND(2) = .True.
 
        REALS_LOWER(1) = 0.00001
        REALS_UPPER(1) = 10000.0
        REALS_LOWER(2) = 2
        REALS_UPPER(2) = xmaxdat
        INTEGERS(2) = num_bins
 
!     Output interactive graphical form
        TX(1) = 'OUTPUT BIN SIZE CONTROL FORM'
        TX(2) = ' '
        TX(3) = '(under development)'
 
        Call GS_FORM (2, 2, PROMPT, 3, 3, TX, Max_choices, num_choices, &
          BUTTONS, TYPES, TEXT, FULL_PROMPTS, BOUND, INTS_LOWER, INTS_UPPER, &
          REALS_LOWER, REALS_UPPER, INTEGERS, LOGICALS, REALS, STRINGS, &
          retstat, status)
 
!     Check for "CANCEL"
        If (retstat .Eq. -1) Then
           status = St_escapevalue
           Return
        End If
 
!     Set resulting values
        If (scan_type .Eq. 0) Then
           bin_size = REALS(1) / 1.0e6
        Else If (scan_type .Eq. 1) Then
           bin_size = REALS(1)
        Else If (scan_type .Eq. 2) Then
           bin_size = REALS(1)
        Else If (scan_type .Eq. 3) Then
           bin_size = REALS(1)
        End If
 
        num_bins = INTEGERS(2)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Allocate space for normalisation array
        Call IO_MALLOC (num_bins * 4, pNORMALISATION, status)
 
!     Calculate 1-D scan of projected data
        Call F2D_CAL_GISAXS (mask_data, scan_type, experiment, &
          geometrical_correction, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, MASK, X_REGION, Y_REGION, &
          0, variances_exist, bin_size, xmaxdat, num_bins, retstat, MX_AXIS, &
          MDATA, MVARIANCES, %val(pNORMALISATION), status)
 
!     Free dynamic array space
        Call IO_FREE (pNORMALISATION, status)
 
!     Check status value
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Calculate approximate summation if required
        If (scan_summation) Then
 
           scan_width = Sqrt( (X_REGION(3) - X_REGION(2))**2 + (Y_REGION(3) - &
             Y_REGION(2))**2)
 
           Call MA_RCMULT (xmaxdat, ymaxdat, 1, 1, num_bins, 1, scan_width, &
             MDATA, status)
 
           If (variances_exist) Then
 
              Call MA_RCMULT (xmaxdat, ymaxdat, 1, 1, num_bins, 1, &
                scan_width**2, MVARIANCES, status)
 
           End If
 
        End If
 
!     Set output array sizes
        memory_defined = .True.
        mxnumdat = num_bins
        mynumdat = 1
        mxstrelm = 1
        mystrelm = 1
        mxendelm = num_bins
        myendelm = 1
 
!     Set title and labels
        If (scan_type .Eq. 0) Then
           mtitle = Trim(title) // ': Distance Projection'
           mxlabel = 'Distance (mm)'
        Else If (scan_type .Eq. 1) Then
           mtitle = Trim(title) // ': 2-theta Projection'
           mxlabel = '2-Theta Angle (Degrees)'
        Else If (scan_type .Eq. 2) Then
           mtitle = Trim(title) // ': Q-Space Projection'
           mxlabel = 'Q (Inverse Nanometres)'
        Else If (scan_type .Eq. 3) Then
           mtitle = Trim(title) // ': D-Spacings Projection'
           mxlabel = 'D-spacing (Angstroms)'
        End If

        mylabel = 'N.A.'
        mzlabel = 'N.A.'
        mx_pixel_size = bin_size
        my_pixel_size = 1.0

     End If
 
     End Subroutine F2D_GUI_PROJECTION
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 
