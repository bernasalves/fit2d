!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_ext_peaksearch.f90 *
!  *                        *
!  **************************
 
!+ F2D_EXT_PEAKSEARCH - Peak search of a series of images
     Subroutine F2D_EXT_PEAKSEARCH (input_options, xmaxdat, ymaxdat, max_peaks,&
       xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, experiment, &
       XAXIS, YAXIS, DATA, VARIANCES, MXAXIS, MYAXIS, MDATA, MVARIANCES, MASK,&
       title, xlabel, ylabel, zlabel, variances_exist, data_defined, &
       num_peaks, PEAKS, draw_bad_weak, status)
!  Description:
!    Definition and input of a series of diffraction images with peak search  
!    of each image and conversion of position in reciprocal space.
!  Keywords:
!    Peak~Search.File~Series, File~Series.Peak~Search
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    25-Oct-2007: V0.4 Debugging (Hammersley)
!    12-Feb-2007: V0.3 Add masking (Hammersley)
!    22-Nov-2006: V0.2 Remove background subtraction (Hammersley)
!    15-Nov-2006: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use GS_LIB
!     Use FIO_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc' ! IO data definitions
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: max_peaks ! Dimension of "PEAKS"
!  Import/Export:
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements in Y-direction
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Real, Intent(INOUT) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(INOUT) :: YAXIS(ymaxdat) ! Y-axis values
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! The estimated
!      variance values
     Real, Intent(INOUT) :: MXAXIS(xmaxdat) ! X-axis values
     Real, Intent(INOUT) :: MYAXIS(ymaxdat) ! Y-axis values
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(INOUT) :: MVARIANCES(xmaxdat, ymaxdat) ! The estimated
!      variance values
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! Data mask
     Character(Len = *), Intent(INOUT) :: title ! Title for plot
     Character(Len = *), Intent(INOUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: zlabel ! Z-axis label for plot
     Logical, Intent(INOUT) :: data_defined ! .True., if data exists within
!      the program
     Logical, Intent(INOUT) :: variances_exist ! .True., if a data variance
!      array is created
!  Export:
     Integer, Intent(OUT) :: num_peaks ! Number of defined peaks
     Type(PEAK_STRUCTURE), Intent(OUT) :: PEAKS(max_peaks) ! Returns peak 
!      information
     Logical, Intent(OUT) :: draw_bad_weak  ! .True., if bad but non-saturated
!      "peaks" are to be drawn
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection
!      prompt text
!  Local Variables:
     Logical, Save :: allow_edit = .False. ! .True., if the peaks list is to be 
!      edited
     Logical :: ask_values ! .True. if peak search control parameters values are
!      to be input
     Character(Len = 256) :: aux_file ! Name of auxiliary file
     Character(Len = 256), Save :: dark_file_name = 'dark_current.cbf'
!      Name of dark current file
     Character(Len = 256) :: extension ! File name extension
     Character(Len = 256) :: file1 ! First file name without directory path
     Character(Len = 256) :: file2 ! Second file name without directory path
     Character(Len = 256) :: file_name ! Name of file in the series
     Character(Len = 256) :: first_file ! Full name of first file
     Character(Len = 256) :: last_file ! Full name of last file
     Character(Len = 256) :: output_file ! Name of output file
     Character(Len = 256) :: postfix ! Fixed end of file names
     Character(Len = 256) :: prefix ! Fixed start of file names
     Integer :: db_stat ! Return status from "IO_INQ_*KEYVALUE"
     Integer :: dummy ! Dummy variable
     Integer :: end_value ! Value at end of sequence
     Integer :: increment ! Step value in sequence
     Integer :: file ! Loop variable for files
     Integer :: first_image ! First image in series (not used)
     Type(PEAK_SEARCH_CONTROL), Save :: mode ! Mode and control of peak 
!      search
     Integer :: last_image ! Last image in series (not used)
     Integer :: len_string ! Defined length of a string
     Integer :: num_characters ! Number of characters in numerical part
     Integer :: num_images ! Number of images input 
     Integer :: retstat ! Return status variable
     Integer :: start_peak ! Starting peak number for particular image
     Integer :: start_value ! Value at start of sequence
     Logical, Save :: angles_from_files = .False. ! .True., if angles should 
!      be input from files
     Logical :: file_ok ! .True., when the input file is O.K.
     Logical :: first ! .True., if first image in sequence
     Logical :: variable_characters ! .True., if the number of characters
!    in the numerical part of the names changes
     Double Precision :: chi ! Crystal chi rotation angle (radians)
     Double Precision, Save :: delta_2theta = 0.0 ! Change in detector 2-theta 
!      angle for each diffraction image (radians)
     Double Precision, Save :: delta_chi = 0.0 ! Change in Chi angle for each
!      diffraction image (radians)
     Double Precision, Save :: delta_phi = 0.0017453291 ! Change in Phi angle 
!      for each diffraction image (radians)
     Double Precision, Save :: delta_omega = 0.0 ! Change in Omega angle for 
!      each diffraction image (radians)
     Double Precision :: two_theta ! Detector 2-theta angle (radians)
     Double Precision :: omega ! Crystal omega rotation angle (radians)
     Double Precision :: phi ! Crystal phi rotation angle (radians)
     Real :: mb_per_minute ! Number of MBytes of input data processed per
!      minute
     Real :: mbytes ! Total number of MBytes of input data processed
     Double Precision, Save :: start_chi = 0.0 ! Chi angle for first diffraction
!      image (radians)
     Double Precision, Save :: start_2theta = 0.0 ! Detector 2-theta angle for 
!      first diffraction image (radians)
     Double Precision, Save :: start_phi = 0.0 ! Phi angle for first diffraction
!      image (radians)
     Double Precision, Save :: start_omega = 0.0 ! Omega angle for first 
!      diffraction image (radians)
     Real :: time_cpu1 ! CPU time at start of series
     Real :: time_cpu2 ! CPU time at end of series
     Real :: time_elapse1 ! Elapse time at start of series
     Real :: time_elapse2 ! Elapse time at end of series
!  Local Data Structures:
     Type(INPUT_OPTIONS_STRUCTURE) :: input_opts ! Control of 
!      input of auxiliary experimental information (see "io.inc")
!  Local Arrays:
     Character(Len = 80) :: ERROR(7) ! Error messages
     Character(Len = 80) :: MESSAGE(7) ! User messages
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_EXT_PEAKSEARCH ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_EXT_PEAKSEARCH ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        draw_bad_weak = .True.

!     Arguments would appear to be reasonable, go ahead

        If (input_options%input_goniometer_angles .And. &
          input_options%input_two_theta) Then
 
!        Try to get default values from internal data-base
           Call IO_INQ_LKEYVALUE ('DIFFRACTOMETER ANGLES FROM FILES', &
             angles_from_files, retstat, status)

        Else 

           angles_from_files = .False.

           MESSAGE(1) = 'NOTE: Goniometer angles and / or two theta angles'
           MESSAGE(2) = 'cannot be input from file. If this is incorrect,'
           MESSAGE(3) = 'change the setting with "INPUT OPTIONS" in the'
           MESSAGE(4) = '"SET-UP" interface.,'
           Call GS_MESSAGE (4, 4, MESSAGE, status)

        End If
 
!     Define file series
        Call F2D_DEF_FS (input_options, &
          .True., variances_exist, xmaxdat, ymaxdat, retstat, &
          xnumdat, ynumdat, XAXIS, YAXIS, DATA, VARIANCES, title, xlabel, &
          ylabel, zlabel, xstrelm, ystrelm, &
          xendelm, yendelm, first_file, first_image, last_file, last_image, &
          start_value, end_value, prefix, variable_characters, num_characters, &
          postfix, extension, increment, experiment, status)

!     Mask first image in file series (used for all)
        Call F2D_MASK (.False., xmaxdat, ymaxdat, xnumdat, ynumdat, &
          DATA, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, experiment, &
          xstrelm, ystrelm, xendelm, yendelm, MASK, status)

!     Input angles from file
        Call GS_BACKGROUND (status)
        MESSAGE(1) = 'Enter "YES" if the diffractometer angle are to be input'
        MESSAGE(2) = 'from CBF or CIF files; "NO" if they will be interactively'
        MESSAGE(3) = 'defined.'
        ERROR(1) = 'Only "YES" of "NO" or equivalent can be input'
        Call GS_INPL (.True., 0, 1, .True., &
          'DIFFRACTOMETER ANGLES FROM FILES', 3, MESSAGE, 1, ERROR, &
          angles_from_files, status)

!     Set default values from internal data-base
        Call IO_SET_LKEYVALUE ('DIFFRACTOMETER_ANGLES_FROM_FILES', &
          angles_from_files, retstat, status)

        If (.Not. angles_from_files) Then

!        Define crystal setting angles for sequence
           Call F2D_RMAP_XTALANGLES (experiment, &
             start_chi, start_phi, start_omega, start_2theta, &
             delta_chi, delta_phi, delta_omega, delta_2theta, status)

        Else

!        Only input goniometer and two theta values from file
           input_opts%input_pixel_sizes = .False.
           input_opts%input_wavelength = .False.
           input_opts%input_polarisation = .False.
           input_opts%input_detector_distance = .False.
           input_opts%input_beam_centre = .False.
           input_opts%input_tilt = .False.
           input_opts%input_two_theta = .True.
           input_opts%input_goniometer_angles = .True.
           input_opts%input_image_orientation = .False.
           input_opts%input_cell  = .False.
           input_opts%input_ub_matrix  = .False.

        End If

!     Check return status
        If (retstat .Ne. 0) Then
           Return
        Else
           data_defined = .True.
        End If

!     Add the "postfix" to the extension if it exists
        If (Len_Trim(postfix) .Gt. 0) Then
 
           If (Len_Trim(extension) .Gt. 0) Then
              extension = Trim(postfix) // Trim(extension)
           Else
              extension = Trim(postfix)
           End If
 
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''start_value, end_value = '', 2i6)')
!     :       start_value, end_value
!     Write (*, '(''extension = '', a)') Trim(extension)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Remove directory path
        Call IO_NODIRPATH (first_file, file1, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     User progress macro
        Call GS_BACKGROUND (status)
        MESSAGE(1) = 'AUTOMATIC SEQUENCE INPUT STARTED'
        Call GS_FPROMPT (1, 1, MESSAGE, status)
        Call GS_UPDATE (status)
 
        Call IO_TIMES (time_elapse1, time_cpu1, status)
 
!     Set initial crystal setting angles
        chi = start_chi
        phi = start_phi
        omega = start_omega
        two_theta = start_2theta
 
!     Automatic part of sequence processing
        first = .True.
        ask_values = .True.
        num_peaks = 0
        num_images = 0
        Do file = start_value, end_value, increment
 
!        Generate input file name
           Call IO_FILENAME (prefix, file, .Not. variable_characters, &
             num_characters, extension, retstat, file_name, status)
 
!        Progress report
           MESSAGE(1) = 'Inputting: ' // file_name
           Call GS_FPROMPT (1, 1, MESSAGE, status)
          Call GS_UPDATE (status)
 
!        Input file
           data_defined = .False.
           Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, -2147483647, &
             input_options, xmaxdat, &
             ymaxdat, variances_exist, data_defined, file_name, xnumdat, &
             ynumdat, XAXIS, YAXIS, DATA, VARIANCES, title, xlabel, ylabel, &
             zlabel, experiment, status)
 
           If (data_defined) Then
 
              num_images = num_images + 1

!           Plot data
              Call GS_MPLOT (.True., xmaxdat, ymaxdat, DATA, &
                MASK, XAXIS, YAXIS, xstrelm, ystrelm, &
                xendelm, yendelm, title, xlabel, ylabel, zlabel, status)

              Call IO_WRITE ('INFO: Processing ' // file_name, status)
              MESSAGE(1) = 'Processing: ' // file_name
              Call GS_FPROMPT (1, 1, MESSAGE, status)
              Call GS_UPDATE (status)

              If (angles_from_files) Then

!               Try to input diffractometer angles from auxiliary files .cif
                 Call IO_FILEEXTENSION (file_name, 'cif', retstat, aux_file, &
                   status)
                 Call FIO_IN_AUXILIARY (input_options, aux_file, retstat, &
                   experiment, status)

                 If (retstat .Ne. 0) Then

                    MESSAGE(1) = 'Problem inputting auxiliary data from'
                    MESSAGE(2) = aux_file
                    Call GS_FWARNING (2, 2, MESSAGE, status)

                 Else If (experiment%two_theta_set .And. &
                   experiment%goniometer_set) Then

                    two_theta = experiment%two_theta
                    chi = experiment%chi_start
                    phi = experiment%phi_start
                    omega = experiment%omega_start

                 Else

                    MESSAGE(1) = 'Diffractometer angles not defined in'
                    MESSAGE(2) = aux_file
                    Call GS_FWARNING (2, 2, MESSAGE, status)

                 End If

              End If

              start_peak = num_peaks + 1

!           Conduct peak search
              Call F2D_GUI_PEAKSEARCH (allow_edit, file_name, ask_values, &
                xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, MASK, XAXIS, YAXIS, &
                xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
                zlabel, max_peaks, experiment, num_peaks, MDATA, PEAKS, &
                draw_bad_weak, status)

              ask_values = .False.

!           Calculate positions in reciprocal space
              Call F2D_EXT_TRANSFORM (experiment, two_theta, chi, phi, omega, &
                max_peaks, start_peak, num_peaks, PEAKS, status)

           End If
 
!        Increment crystal setting angles
           chi = chi + (delta_chi * Dble(increment))
           phi = phi + (delta_phi * Dble(increment))
           omega = omega + (delta_omega * Dble(increment))
           two_theta = two_theta + (delta_2theta * Dble(increment))
 
        End Do

        Call IO_TIMES (time_elapse2, time_cpu2, status)
 
        mbytes = Real(xnumdat * ynumdat * 2 * num_images) / (1024.0 * 1024.0)
        mb_per_minute = 60 * mbytes / (time_elapse2 - time_elapse1)
 
        xstrelm = 1
        ystrelm = 1
        xendelm = xnumdat
        yendelm = ynumdat
        data_defined = .True.

 
!     Information on performance
        Write (MESSAGE(1), '(''INFO: Time for processing = '', ' // &
          'f12.3, '' seconds'')') time_elapse2 - time_elapse1
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''INFO: Assuming 16 bit, '', ' // &
          'f12.2, '' MBytes of input data processed per minute'')') &
          mb_per_minute
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''      equivalent to '', f12.2, ' // &
          ''' GBytes per hour'')') mb_per_minute * 60.0 / 1024.0
        Call IO_WRITE (MESSAGE(1), status)
 
     End If
 
     End Subroutine F2D_EXT_PEAKSEARCH
!********1*********2*********3*********4*********5*********6*********7*********8
 
