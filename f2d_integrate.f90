!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_integrate.f90 *
!  *                   *
!  *********************
 
!+ F2D_INTEGRATE - FIT2D: 2-D to 1-D INTEGRATION
     Subroutine F2D_INTEGRATE (input_file, xmaxdat, ymaxdat, data_defined, &
       title, xlabel, &
       ylabel, zlabel, variances_exist, xnumdat, ynumdat, X_AXIS, Y_AXIS, &
       DATA, MASK, xstrelm, ystrelm, xendelm, yendelm, experiment, &
       lorentz_geometry, &
       memory_defined, mtitle, mxlabel, mylabel, mzlabel, mxnumdat, mynumdat, &
       MX_AXIS, MY_AXIS, MDATA, mxstrelm, mystrelm, mxendelm, myendelm, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    360 degree integration from a powder ring to a 1-D 2-theta scan
!  Keywords:
!    2-theta~Scan.Re-binning, Re-binning.2-theta~Scan
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    19-Sep-2012: V0.17 Option to correct parallax effect (Hammersley)
!    23-Aug-2012: V0.16 Option of look-up table calculation (Hammersley)
!    14-Oct-2011: V0.15 Option to output integration parameters to a file
!      (Hammersley)
!    04-Oct-2011: V0.14 Use "F2D_CAL_INTEGRATE" instead of "F2D_CAL2_CAKE"
!      for faster integration (Hammersley)
!    30-Mar-2006: V0.13 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    10-Mar-2006: V0.12 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    02-Jun-2003: V0.11 Tidy up code (Hammersley)
!    23-Feb-1999: V0.10 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.9 Change to use IO internal database routines (Hammersley)
!    25-Feb-1998: V0.8 Option to set maximum D-spacing (Hammersley)
!    20-Feb-1998: V0.7 Swap X/Y outputting re-binning sense, so that a 2-theta 
!      scan is output in the X-direction (Hammersley)
!    03-Feb-1998: V0.6 Cope with "CANCEL" from geometry form (Hammersley)
!    28-Jan-1998: V0.5 Add option of Q-space re-binning; change definition of 
!      "angular_scan" argument (Hammersley)
!    18-Oct-1996: V0.4 Allow tilt plane rotation to be specified between +-180 
!      which is the range output by "TILT" (Hammersley)
!    25-Apr-1996: V0.3 Add detailed help text to form (Hammersley)
!    08-Mar-1996: V0.2 Check for "CANCEL" from form (Hammersley)
!    27-Feb-1996: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use GS_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointer to program arrays
!  Import:
     Character(Len = *), Intent(IN) :: input_file ! Name of input file
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined ! .True., if the current data
!      arrays are defined to contain data
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
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for end of the ROI
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(INOUT) :: lorentz_geometry ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the equivalent of a 
!            2-theta scan i.e. detector at equal distance
!  Export:
     Logical, Intent(OUT) :: memory_defined ! .True., if the memory arrays
!      are defined to contain data
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
     Real, Intent(OUT) :: MX_AXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(OUT) :: MY_AXIS(ymaxdat) ! Array containing data Y-coordinates
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Array containing data to
!      be fitted
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
     Character(Len = 5), Parameter :: Version = 'V0.17' ! Version number
     Integer, Parameter :: Max_chars = 48 ! Maximum number of characters in
!      a line
     Integer, Parameter :: Max_lines = 82 ! Number of lines in message
     Integer, Parameter :: Max_choices = 12 ! Maximum number of choices
!      (need 5 extra spaces)
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
!  Local Variables:
     Real :: az_pixel_size ! Size of pixel to be used for the azimuth bin sizes.
!      The units are radians
     Logical, Save :: conserve_intensity = .False. ! .True., if the total
!      intensity is to be conserved in the re-binning operation
     Logical :: correct_geometry ! .True., if the scan intensities are
!      to be corrected for the 1/Cos**3(angle) drop owing to the
!      geometrical difference between a 2-theta scan and a flat detector
     Logical :: correct_parallax = .False. ! .True., if the effect of parallax
!      on angular position is to be corrected
     Real :: end_cpu ! CPU time at end of re-binning
     Real :: end_elapse ! Elapse time at end of re-binning
     Real :: inner_2theta ! 2-theta angle of inside edge of region
     Real, Save :: maximum_d = 20.0 ! Maximum of range for D-spacings scans
     Integer :: num_2theta ! Number of 2-theta angle or radial bins in
!      rebinned array
     Real :: outer_angle ! Maximum angle of 2-theta scan
     Real :: outer_limit ! Outer radius in metres
     Real :: outer_q ! Outer q value
     Character(Len = Len_name) :: parameter_file ! Output file for
!      saving parameters
     Integer :: pixel ! Loop variable
     Real :: rad_pixel_size ! Size of pixel to be used for the 2-theta, radial,
!      or q-space bin sizes. If an equal radial distance scan is to be 
!      calculated ("scan_type = 0") the units are metres, and for an equal angle
!      pixel scan ("scan_type = 1") the units are radians, and for a Q-space 
!      scan the units are inverse nanometres
     Integer :: retstat ! Return status variable for "IO_INQ_*"
     Logical, Save :: save_parameters = .False. ! ".True." if integration 
!      parameters are to be saved to a file
     Integer, Save :: scan_type = 1 ! Type of 1-D scan to be produced:
!      0 = Radial: equal distance scan
!      1 = "2-theta": equal angle element scan
!      2 = Q-space: Equal Q element scan
!          (Q = (4 * Pi / wavelength) Sin(theta / 2) )
!      3 = D-spacing scan
     Real :: start_cpu ! CPU time at start of re-binning
     Real :: start_elapse ! Elapse time at start of re-binning
     Integer stat ! Status return variable for "Allocate"
     Logical, Save :: use_lut = .True. ! .True., if a look-up table is to be
!      used for the re-binning operation
!  Local Arrays:
     Character(Len = 60) :: MESSAGE(2) ! User messages
     Real, Allocatable :: WORK(:) ! Pointer to dynamic array "WORK": Work array 
!      for "F2D_RTHETA2" used to store fractions of pixels contributing to each 
!      angular/radial pixel
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INTEGRATE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_INTEGRATE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Find any previously set user preferences
        Call IO_INQ_IKEYVALUE ('SCAN_TYPE', scan_type, retstat, status)
        Call IO_INQ_RKEYVALUE ('MAXIMUM_D', maximum_d, retstat, status)
        Call IO_INQ_LKEYVALUE ('SAVE_INTEGRATION_PARAMETERS', save_parameters, &
           retstat, status)
        Call IO_INQ_LKEYVALUE ('CORRECT_PARALLAX', correct_parallax, &
          retstat, status)
 
!     Input integration control parameters
        Call F2D_INQ_INTEGRATE (input_file, xmaxdat, ymaxdat, &
          xnumdat, ynumdat, xstrelm, &
          ystrelm, xendelm, yendelm, experiment, &
          outer_limit, scan_type, maximum_d, conserve_intensity, &
          correct_geometry, outer_angle, &
          num_2theta, lorentz_geometry, rad_pixel_size, outer_q, &
          save_parameters, parameter_file, use_lut, correct_parallax, status)
 
!     Check status
        If (status .Ne. St_goodvalue) Then
 
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
 
!           Redraw image
              Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, X_AXIS, &
                Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                ylabel, zlabel, status)
 
           End If
 
           Return
 
        End If
 
!     Save set user preferences
        Call IO_SET_IKEYVALUE ('SCAN_TYPE', scan_type, retstat, status)
        Call IO_SET_RKEYVALUE ('MAXIMUM_D', maximum_d, retstat, status)
        Call IO_SET_LKEYVALUE ('SAVE_INTEGRATION_PARAMETERS', save_parameters, &
           retstat, status)
        Call IO_SET_LKEYVALUE ('CORRECT_PARALLAX', correct_parallax, &
          retstat, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Inform user of calculation and swapping of data to the memory
        MESSAGE(1) = 'WORKING: CALCULATING TRANSFORM'
        MESSAGE(2) = 'NOTE: ORIGINAL DATA STORED IN THE MEMORY'
        Call GS_FPROMPT (2, 2, MESSAGE, status)
 
!     Force output
        Call GS_UPDATE (status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Get dynamic work array space
        Allocate (WORK(num_2theta), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_INTEGRATE ' // Version)
           Return
        End If
 
!     Get start time
        Call IO_TIMES (start_elapse, start_cpu, status)
 
!     Calculate 2-theta scan
        If (use_lut) Then
           Call F2D_CAL_LUT_CAKE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, DATA, MASK, conserve_intensity, scan_type, maximum_d, &
             experiment, 0.0, 360.0, 0.0, outer_limit, &
             num_2theta, 1, lorentz_geometry, correct_parallax, &
             xmaxdat, ymaxdat, num_2theta, 1, az_pixel_size, rad_pixel_size, &
             inner_2theta, WORK, MDATA, status)
        Else
           Call F2D_CAL2_CAKE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, DATA, MASK, conserve_intensity, scan_type, maximum_d, &
             experiment, 0.0, 360.0, 0.0, outer_limit, &
             num_2theta, 1, lorentz_geometry, correct_parallax, &
             xmaxdat, ymaxdat, num_2theta, 1, az_pixel_size, rad_pixel_size, &
             inner_2theta, WORK, MDATA, status)

        End If
!        Call F2D_CAL_INTEGRATE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
!          yendelm, DATA, MASK, conserve_intensity, scan_type, maximum_d, &
!          experiment, 0.0, outer_limit, num_2theta, lorentz_geometry, &
!          xmaxdat, num_2theta, rad_pixel_size, &
!          inner_2theta, WORK, MDATA, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_INTEGRATE: After F2D_CAL2_CAKE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Get end time
        Call IO_TIMES (end_elapse, end_cpu, status)

!     Output time for re-binning
        Write (MESSAGE(1), '(''INFO: Elapse time for operation = '', f6.2 ' // &
          ''' seconds'')')  end_elapse - start_elapse
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''      CPU Time = '', f6.2, '' seconds'')') &
          end_cpu - start_cpu
        Call IO_WRITE (MESSAGE(1), status)

!     Free dynamic array space
        Deallocate (WORK)
 
!     Define axis data
        If (scan_type .Eq. 0) Then
 
           Do pixel = 1, num_2theta
              MX_AXIS(pixel) = ((Real(pixel) - 0.5) * rad_pixel_size) * 1000.0
           End Do
 
        Else If (scan_type .Eq. 1) Then
 
           Do pixel = 1, num_2theta
              MX_AXIS(pixel) = (inner_2theta + (Real(pixel) - 0.5) * &
                rad_pixel_size) * 180.0 / Pi
           End Do
 
        Else If (scan_type .Eq. 2) Then
 
           Do pixel = 1, num_2theta
              MX_AXIS(pixel) = (inner_2theta + (Real(pixel) - 0.5) * &
                rad_pixel_size)
           End Do
 
        Else If (scan_type .Eq. 3) Then
 
           Do pixel = 1, num_2theta
              MX_AXIS(pixel) = (inner_2theta + (Real(pixel) - 0.5) * &
                rad_pixel_size)
           End Do
 
        End If
 
        MY_AXIS(1) = 0.5
 
!     Set title and labels
        If (scan_type .Eq. 0) Then
           mtitle = Trim(title) // ': Radial Scan'
           mxlabel = 'Radial Distance (mm)'
        Else If (scan_type .Eq. 1) Then
           mtitle = Trim(title) // ': 2-theta Scan'
           mxlabel = '2-Theta Angle (Degrees)'
        Else If (scan_type .Eq. 2) Then
           mtitle = Trim(title) // ': Q-Space Scan'
           mxlabel = 'Q (Inverse Nanometres)'
        Else If (scan_type .Eq. 3) Then
           mtitle = Trim(title) // ': D-Spacing Scan'
           mxlabel = 'D-spacing (Angstroms)'
        End If
        mylabel = 'N.A.'
        mzlabel = 'Intensity'
 
!     Set memory to exist and set memory ROI
        memory_defined = .True.
        mxstrelm = 1
        mystrelm = 1
        mxendelm = num_2theta
        myendelm = 1
        mxnumdat = mxendelm
        mynumdat = myendelm
 
!     Swap current data with "memory"
        Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, xlabel, &
          ylabel, zlabel, variances_exist, data_defined, &
          experiment%x_pixel_size, experiment%y_pixel_size, &
          xstrelm, ystrelm, xendelm, yendelm, mxnumdat, &
          mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, &
          mylabel, mzlabel, memory_defined, mx_pixel_size, my_pixel_size, &
          status)

        If (save_parameters) Then

!        Output experimental geometry and integration parameters to a file
           Call F2D_SAVE_INTEGRATION_PARAMETERS (input_file, &
             experiment, xstrelm, ystrelm, &
             xendelm, yendelm, outer_limit, scan_type, maximum_d, &
             conserve_intensity, correct_geometry, outer_angle, &
             num_2theta, lorentz_geometry, rad_pixel_size, outer_q, &
             parameter_file, status)

        End If

     End If
 
     End Subroutine F2D_INTEGRATE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
