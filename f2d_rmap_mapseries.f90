!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_rmap_mapseries.f90 *
!  *                        *
!  **************************
 
!+ F2D_RMAP_MAPSERIES - MAP SERIES of diffraction images to Reciprocal space
     Subroutine F2D_RMAP_MAPSERIES (xmaxmap, ymaxmap, zmaxmap, xnummap, &
       ynummap, znummap, C1, C2C1, c2c1s, C3C1, c3c1s, C5, C5C1, c5c1s, CN, &
       step, thickness, xmaxdat, ymaxdat, xnumdat, ynumdat, xstrelm, ystrelm, &
       xendelm, yendelm, input_options, experiment, &
       XAXIS, YAXIS, DATA, VARIANCES, MXAXIS, MYAXIS, MEMORY, MVARIANCES, MASK,&
       title, xlabel, ylabel, zlabel, variances_exist, data_defined, &
       mask_exist, MAP, NORMALISE, status)
!  Description:
!    Definition and input of a series of diffraction images with transformation 
!    of each image to the reciprocal map array.
!  Keywords:
!    Reciprocal~Mapping.File~Series, File~Series.Reciprocal~Mapping
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    12-May-2009: V0.20 Optional subtraction of dark current (Hammersley)
!    26-Oct-2007: V0.19 Correct input image size (Hammersley)
!    25-Jan-2007: V0.18 Transfer auxiliary data input code to "FIO_GUI_INPUT"
!      (Hammersley)
!    25-Sep-2006: V0.17 Add select of background image (Hammersley)
!    21-Sep-2006: V0.16 Option for background subtraction (Hammersley)
!    11-Sep-2006: V0.15 Remove "monitor" (Hammersley)
!    22-Jun-2006: V0.14 Changes to "F2D_RMAP_TRANSFORM" (Hammersley)
!    27-Mar-2006: V0.13 Add masking (Hammersley)
!    20-Mar-2006: V0.12 Add "input options" support (Hammersley)
!    10-Mar-2006: V0.11 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    04-Mar-2006: V0.10 Changes to experimental details structure type
!      (Hammersley)
!    23-Feb-2006: V0.9 Check file input diffractometer angles have been set
!      (Hammersley)
!    22-Feb-2006: V0.8 Use data structure for experiemental geometry input
!      (Hammersley)
!    10-Feb-2006: V0.7 Option to input diffractometer angles from file
!      (Hammersley)
!    28-Oct-2005: V0.6 Add option to mirror image (Hammersley)
!    25-Oct-2005: V0.5 Change the way detector horizontal or vertical
!      position is handled (Hammersley)
!    08-Jun-2005: V0.4 Correct angle generation to account for jumps
!      in images being treated. Add detector 2-theta angles horizontal as
!      well as vertical (Hammersley)
!    13-May-2005: V0.3 Convert all angles internally to radians (Hammersley)
!    26-Apr-2005: V0.2 Input starting crystal angles and changes (Hammersley)
!    21-Apr-2005: V0.1 Original (Hammersley)
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
     Integer, Intent(IN) :: xmaxmap ! First dimension of reciprocal map
     Integer, Intent(IN) :: ymaxmap ! Second dimension of reciprocal map
     Integer, Intent(IN) :: zmaxmap ! Third dimension of reciprocal map
     Integer, Intent(IN) :: xnummap ! Number of first  dimension pixels of map
     Integer, Intent(IN) :: ynummap ! Number of second dimension pixels of map
     Integer, Intent(IN) :: znummap ! Number of third  dimension pixels of map
     Real, Intent(IN) :: C1(3) ! Starting coordinate of first section
     Real, Intent(IN) :: C2C1(3) ! Difference vector C2 - C1
     Real, Intent(IN) :: c2c1s ! Length of difference vector C2 - C1
     Real, Intent(IN) :: C3C1(3) ! Difference vector C3 - C1
     Real, Intent(IN) :: c3c1s ! Length of difference vector C3 - C1
     Real, Intent(IN) :: C5(3) ! Difference vector between sections
     Real, Intent(IN) :: C5C1(3) ! Difference vector C5 - C1
     Real, Intent(IN) :: c5c1s ! Square of length of Difference vector C5 - C1
     Real, Intent(IN) :: CN(3) ! Normalised difference vector
     Real, Intent(IN) :: step ! Step between sections
     Real, Intent(IN) :: thickness ! Thickness of a single section
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
!  Import/Export:
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements in Y-direction
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
     Type(INPUT_OPTIONS_STRUCTURE), Intent(INOUT) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Real, Intent(INOUT) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(INOUT) :: YAXIS(ymaxdat) ! Y-axis values
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! The estimated
!      variance values
     Real, Intent(INOUT) :: MXAXIS(xmaxdat) ! X-axis values
     Real, Intent(INOUT) :: MYAXIS(ymaxdat) ! Y-axis values
     Real, Intent(INOUT) :: MEMORY(xmaxdat, ymaxdat) ! The data values
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
     Logical, Intent(INOUT) :: mask_exist ! .True., if the mask array exists
!  Export:
     Real, Intent(OUT) :: MAP(xmaxmap, ymaxmap, zmaxmap) ! Reciprocal space map
     Real, Intent(OUT) :: NORMALISE(xmaxmap, ymaxmap, zmaxmap)
!      Normalisation array
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.20' ! Version number
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection
!      prompt text
!  Local Variables:
     Character(Len = 256), Save :: dark_file_name = 'dark_current.cbf'
!  Name of dark current file
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
     Integer :: last_image ! Last image in series (not used)
     Integer :: len_string ! Defined length of a string
     Integer :: num_characters ! Number of characters in numerical part
     Integer :: num_images ! Number of input images
     Integer :: retstat ! Return status variable
     Integer :: start_value ! Value at start of sequence
     Logical, Save :: angles_from_files = .False. ! .True., if angles should 
!      be input from files
     Logical :: file_ok ! .True., when the input file is O.K.
     Logical :: first ! .True., if first image in sequence
     Logical, Save :: subtract_darkcurrent = .True. ! .True., if a dark current
!      file is to be subtracted prior to transformation
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
     Real :: inv_wave_angstroms ! Reciprocal of wavelength in Angstroms
!      i.e. 1.0 / (wavelength * 1.0e10)
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
        Call ST_SAVE ('Subroutine F2D_RMAP_MAPSERIES ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_RMAP_MAPSERIES ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.

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

!     Subtract dark current or not
        Call IO_INQ_LKEYVALUE ('RECIPROCAL SPACE MAPPING SUBTRACT DARK', &
          subtract_darkcurrent, retstat, status)
        MESSAGE(1) = '"YES": to select a dark current image to subtract'
        ERROR(1) = 'Enter "YES" or "NO" only'
        Call GS_INPL (.True., 0, 1, .True., 'SUBTRACT DARK CURRENT', 1, &
          MESSAGE, 1, ERROR, subtract_darkcurrent, status)
        Call IO_SET_LKEYVALUE ('RECIPROCAL SPACE MAPPING SUBTRACT DARK', &
          subtract_darkcurrent, retstat, status)

        If (subtract_darkcurrent) Then

!        Input dark current image
           PROMPT(1) = 'SELECT DARK CURRENT IMAGE'
           Call FIO_GUI_INPUT (Max_prompt, 1, PROMPT, 0, INPUT_OPTS, xmaxdat, &
             ymaxdat, variances_exist, data_defined, dark_file_name, xnumdat, &
             ynumdat, MXAXIS, MYAXIS, MEMORY, MVARIANCES, title, xlabel, &
             ylabel, zlabel, experiment, status)

        End If
           
!     Only input goniometer and two theta values from file
        INPUT_OPTS = input_options
        INPUT_OPTS%input_pixel_sizes = .False.
        INPUT_OPTS%input_wavelength = .False.
        INPUT_OPTS%input_polarisation = .False.
        INPUT_OPTS%input_detector_distance = .False.
        INPUT_OPTS%input_beam_centre = .False.
        INPUT_OPTS%input_tilt = .False.
        INPUT_OPTS%input_two_theta = .True.
        INPUT_OPTS%input_goniometer_angles = .True.
        INPUT_OPTS%input_image_orientation = .False.
        INPUT_OPTS%input_cell  = .False.
        INPUT_OPTS%input_ub_matrix  = .False.

!     Input angles from file
        Call GS_BACKGROUND (status)
        MESSAGE(1) = 'Enter "YES" if the diffractometer angle are to be input'
        MESSAGE(2) = 'from CBF or CIF files; "NO" if they will be interactively'
        MESSAGE(3) = 'defined.'
        ERROR(1) = 'Only "YES" of "NO" or equivalent can be input'
        Call GS_INPL (.True., 0, 1, .True., &
          'DIFFRACTOMETER ANGLES FROM FILES', 3, MESSAGE, 1, ERROR, &
          input_options%auto_input_from_cif, status)

!     Set value in internal data-base
        Call IO_SET_LKEYVALUE ('DIFFRACTOMETER_ANGLES_FROM_FILES', &
          angles_from_files, retstat, status)

        If (.Not. input_options%auto_input_from_cif) Then

!        Define crystal setting angles for sequence
           Call F2D_RMAP_XTALANGLES (experiment, &
             start_chi, start_phi, start_omega, start_2theta, &
             delta_chi, delta_phi, delta_omega, delta_2theta, status)

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
 
!     Calculate the reciprocal of the wavength in Angstrom units
        inv_wave_angstroms = 1.0 / (experiment%wavelength * 1.0e10)
 
!     Automatic part of sequence processing
        num_images = 0
        first = .True.
        Do file = start_value, end_value, increment
 
           num_images = num_images + 1
 
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
 
              Call IO_WRITE ('INFO: Processing ' // file_name, status)
              MESSAGE(1) = 'Processing: ' // file_name
              Call GS_FPROMPT (1, 1, MESSAGE, status)
              Call GS_UPDATE (status)

              If (input_options%auto_input_from_cif .And. &
                experiment%two_theta_set .And. experiment%goniometer_set) Then

                 two_theta = experiment%two_theta
                 chi = experiment%chi_start
                 phi = experiment%phi_start
                 omega = experiment%omega_start

              Else If (input_options%auto_input_from_cif) Then

                 MESSAGE(1) = 'Diffractometer angles not defined in .cif file'
                 Call GS_FWARNING (1, 1, MESSAGE, status)

              End If

              If (subtract_darkcurrent) Then

!              Subtract dark count image
                 Call MA_RSUBTRACT (xmaxdat, ymaxdat, 1, 1, xnumdat, ynumdat, &
                   MEMORY, xmaxdat, ymaxdat, DATA, status)

              End If

!           Transform data to reciprocal space
              Call F2D_RMAP_TRANSFORM (xmaxmap, ymaxmap, zmaxmap, xnummap, &
                ynummap, znummap, C1, C2C1, c2c1s, C3C1, c3c1s, C5, CN, step, &
                thickness, two_theta, chi, &
                phi, omega, xmaxdat, ymaxdat, 1, 1, xnumdat, ynumdat, &
                experiment, inv_wave_angstroms, DATA, MASK, MAP, NORMALISE, &
                status)
 
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
 
     End Subroutine F2D_RMAP_MAPSERIES
!********1*********2*********3*********4*********5*********6*********7*********8
 
