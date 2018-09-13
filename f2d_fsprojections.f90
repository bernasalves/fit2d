!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_fsprojections.f90 *
!  *                       *
!  *************************
 
!+ F2D_FSPROJECTIONS - File Series: PROJECTIONS
     Subroutine F2D_FSPROJECTIONS (input_options, &
       data_defined, memory_exist, memory_defined, &
       variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, &
       YAXIS, DATA, VARIANCES, MASK, title, xlabel, ylabel, zlabel, &
       experiment, xstrelm, ystrelm, xendelm, yendelm, &
       mxnumdat, mynumdat, MXAXIS, MYAXIS, MDATA, MVARIANCES, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    2-D to 1-D projections from a series of files, to build up a 2-D vector of
!    scans.
!  Keywords:
!    Projection.File~Series, File~Series.Projection, Series.Files.Projection
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    24-Apr-2006: V0.7 Add "input_options" structure (Hammersley)
!    31-Mar-2006: V0.6 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    14-Mar-2006: V0.5 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    19-Nov-2004: V0.4 Debugging (Hammersley)
!    18-Nov-2004: V0.3 Continue implementation (Hammersley)
!    17-Nov-2004: V0.2 Continue implementation (Hammersley)
!    16-Nov-2004: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined ! .True., if data exists within
!      the program
     Logical, Intent(INOUT) :: memory_exist ! .True. if memory array exists
     Logical, Intent(INOUT) :: memory_defined ! .True. if the memory
!      contains data
     Logical, Intent(INOUT) :: variances_exist ! .True., if a data variance
!      array is created
     Logical, Intent(INOUT) :: mask_exist ! .True., if the mask array exists
     Integer, Intent(INOUT) :: xmaxdat ! Dimension size in X-direction for
!      data arrays
     Integer, Intent(INOUT) :: ymaxdat ! Dimension size in Y-direction for
!      data arrays
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements defined in
!      Y-direction
     Real, Intent(INOUT) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(INOUT) :: YAXIS(ymaxdat) ! Y-axis values
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! The estimated
!      variance values
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! Data mask
     Character(Len = *), Intent(INOUT) :: title ! Title for plot
     Character(Len = *), Intent(INOUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: zlabel ! Z-axis label for plot
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
!  Export:
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
     Real, Intent(OUT) :: MXAXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(OUT) :: MYAXIS(ymaxdat) ! Array containing data Y-coordinates
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Array containing data to
!      be fitted
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat) ! Array containing
!      variances in the data values
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Real, Intent(OUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(OUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.7' ! Version number
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection
!      prompt text
!  Local Variables:
     Character(Len = 256) :: back_file_name = 'background.dat'
!      Full name of background file
     Character(Len = 256) :: extension ! File name extension
     Character(Len = 256) :: file1 ! First file name without directory path
     Character(Len = 256) :: file2 ! Second file name without directory path
     Character(Len = 256) :: file_name ! Name of file in the series
     Character(Len = 256) :: first_file ! Full name of first file
     Character(Len = 256) :: last_file ! Full name of last file
     Character(Len = 256) :: output_file ! Name of output file
     Character(Len = 256) :: postfix ! Fixed end of file names
     Character(Len = 256) :: prefix ! Fixed start of file names
     Character(Len = 80), Save :: save_1dext = 'chi' ! Extension for 1-D
!      projections output files
     Integer :: db_stat ! Return status from 'IO_INQ_*KEYVALUE'
     Integer :: dummy ! Dummy variable
     Integer :: end_value ! Value at end of sequence
     Integer :: increment ! Step value in sequence
     Integer :: file ! Loop variable for files
     Integer :: first_image ! First image in series (not used)
     Integer :: last_image ! Last image in series (not used)
     Integer :: len_string ! Defined length of a string
     Integer :: num_characters ! Number of characters in numerical part
     Integer :: pixel ! Loop variable
     Integer :: retstat ! Return status variable
     Integer :: start_value ! Value at start of sequence
     Integer stat ! Status return variable for "Allocate"
     Integer :: y ! Variable for the input scans
     Logical :: file_ok ! .True., when the input file is O.K.
     Logical :: first ! .True., if first image in sequence
     Logical, Save :: subtract_background = .True. ! .True., if a
!      background image is to be subtracted
     Logical :: variable_characters ! .True., if the number of characters
!      in the numerical part of the names changes
     Real :: mb_per_minute ! Number of MBytes of input data processed per
!      minute
     Real :: mbytes ! Total number of MBytes of input data processed
     Real :: time_cpu1 ! CPU time at start of series
     Real :: time_cpu2 ! CPU time at end of series
     Real :: time_elapse1 ! Elapse time at start of series
     Real :: time_elapse2 ! Elapse time at end of series
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(7) ! User messages
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
     Real, Allocatable :: BACKGROUND(:, :) ! Dynamic array to store a background
!      image
     Real, Allocatable :: NORMALISE(:) ! Dynamic array to normalise a projection
     Real :: X_REGION(5) ! X-coordinates of projection region
     Real :: Y_REGION(5) ! X-coordinates of projection region
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FSPROJECTIONS ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_FSPROJECTIONS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Define file series
        Call F2D_DEF_FS (INPUT_OPTIONS, .True., variances_exist, &
          xmaxdat, ymaxdat, retstat, &
          xnumdat, ynumdat, XAXIS, YAXIS, DATA, VARIANCES, title, xlabel, &
          ylabel, zlabel, xstrelm, ystrelm, &
          xendelm, yendelm, first_file, first_image, last_file, last_image, &
          start_value, end_value, prefix, variable_characters, num_characters, &
          postfix, extension, increment, experiment, status)
 
!     Check return status
        If (retstat .Ne. 0) Then
           Return
        Else
           data_defined = .True.
        End If
 
!     Add the 'postfix' to the extension if it exists
        If (Len_trim(postfix) .Gt. 0) Then
 
           If (Len_trim(extension) .Gt. 0) Then
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
 
!     Inquire if a background file is too be subtracted.
        MESSAGE(1) = 'Enter "YES" if a background image is to input'
        MESSAGE(2) = 'and subtracted from each image in the input'
        MESSAGE(3) = 'file series.'
        Call GS_INPL (.True., 0, 1, .True., 'SUBTRACT BACKGROUND IMAGE', 3, &
          MESSAGE, 1, 'Enter "YES" or "NO"', subtract_background, status)
 
        If (subtract_background) Then
 
!        Create temporary array to contain background image
           Allocate (BACKGROUND(xmaxdat, ymaxdat), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_FSPROJECTIONS ' // Version)
              Return
           End If
 
!        Try to obtain default name for background file
           Call IO_INQ_KEYVALUE ('FS_PROJECTIONS_BACKGROUND_FILE', len_string, &
             back_file_name, db_stat, status)
 
!        Input background image
           file_ok = .False.
           Do While (.Not. file_ok)
 
              PROMPT(1) = 'SELECT FILE CONTAINING BACKGROUND IMAGE'
              PROMPT(2) = '(click on "HELP" for list of formats)'
              Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 0, &
                input_options, xnumdat, ynumdat, &
                variances_exist, data_defined, back_file_name, xnumdat, &
                ynumdat, XAXIS, YAXIS, BACKGROUND, &
                VARIANCES, title, xlabel, ylabel, zlabel, experiment, status)
 
!           Display image
              Call GS_PLOT (xnumdat, ynumdat, BACKGROUND, XAXIS, &
                YAXIS, xstrelm, ystrelm, xendelm, yendelm, &
                'Background', xlabel, ylabel, zlabel, status)
 
!           Check that the background file is O.K.
              Call GS_INPL (.True., 0, 1, .True., 'FILE O.K.', 1, &
                'Enter "YES" if the file is O.K.', 1, 'Enter "YES" or "NO"', &
                file_ok, status)
 
!           Check for user escape
              If (status .Ne. St_goodvalue) Then
                 Return
              End If
 
           End Do
 
!        Save name of background file in dat-base
           Call IO_SET_KEYVALUE ('FS_PROJECTIONS_BACK_FILE', &
             Len_trim(back_file_name), back_file_name, db_stat, status)
 
        End If
 
        Call IO_INQ_KEYVALUE ('FS_PROJECTIONS_SAVE_EXT', len_string, &
          save_1dext, db_stat, status)
 
!     Input output file extension
        Call GS_INPC (.True., 'FILE EXTENSION FOR OUTPUT OF 1-D PROJECTIONS ', &
          1, 'Choose file extension for output files for projections', 1, &
          'Enter short character string', 1, save_1dext, status)
 
        Call IO_SET_KEYVALUE ('FS_PROJECTIONS_SAVE_EXT', Len_trim(save_1dext), &
          save_1dext, db_stat, status)
 
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     User progress macro
        Call GS_BACKGROUND (status)
        Call GS_FPROMPT (1, 1, 'AUTOMATIC SEQUENCE INPUT STARTED', status)
        Call GS_UPDATE (status)
 
!     Automatic part of sequence processing
        y = 0
        first = .True.
        Do file = start_value, end_value, increment
 
           y = y + 1
 
!        Generate input file name
           Call IO_FILENAME (prefix, file, .Not. variable_characters, &
             num_characters, extension, retstat, file_name, status)
 
!        Progress report
           MESSAGE(1) = 'INPUTTING FILE:'
           MESSAGE(2) = file_name
           Call GS_FPROMPT (2, 2, MESSAGE, status)
           Call GS_UPDATE (status)
 
!        Input file
           data_defined = .False.
           Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, -2147483647, &
             input_options, xmaxdat, &
             ymaxdat, variances_exist, data_defined, file_name, mxnumdat, &
             mynumdat, MXAXIS, MYAXIS, MDATA, MVARIANCES, mtitle, mxlabel, &
             mylabel, mzlabel, experiment, status)
 
           If (data_defined) Then
 
              mxstrelm = 1
              mystrelm = 1
              mxendelm = mxnumdat
              myendelm = mynumdat
              mx_pixel_size = experiment%x_pixel_size
              my_pixel_size = experiment%y_pixel_size

              Call IO_WRITE ('INFO: Input ' // file_name, status)
 
              If (subtract_background) Then
 
                 Call IO_WRITE ( 'INFO: Subtracting background', status)
 
!              Subtract background image
                 Call MA_RSUBTRACT (mxnumdat, mynumdat, 1, 1, mxnumdat, &
                   mynumdat, BACKGROUND, xmaxdat, ymaxdat, MDATA, status)
 
              End If
 
              If (first) Then
 
!              Create image plot of data
                 Call GS_PLOT (xmaxdat, ymaxdat, MDATA, MXAXIS, MYAXIS, 1, 1, &
                   mxnumdat, mynumdat, title, xlabel, ylabel, zlabel, status)
 
!              Define projection region
                 Call F2D_INP_PROJECTION (.False., .False., dummy, dummy, &
                   xmaxdat, ymaxdat, 1, 1, mxnumdat, mynumdat, MXAXIS, MYAXIS, &
                   MDATA, dummy, mtitle, mxlabel, mylabel, mzlabel, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   X_REGION, Y_REGION, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''X/Y_REGION(1) = '', 2f12.5)')
!              :                X_REGION(1), Y_REGION(1)
!              Write (*, '(''X/Y_REGION(2) = '', 2f12.5)')
!              :                X_REGION(2), Y_REGION(2)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Allocate memory for normalisation array
                 Allocate (NORMALISE(xmaxdat), Stat = stat)
                 If (stat .Ne. 0) Then
                    status = St_mod_fit2d + St_bad_malloc
                    Call ST_SAVE ('Subroutine F2D_FSPROJECTIONS ' // Version)
                    Return
                 End If
 
!              Set values in X-axis
                 Do pixel = 1, xmaxdat
                    XAXIS(pixel) = Real(pixel) - 0.5
                 End Do
 
                 Call IO_TIMES (time_elapse1, time_cpu1, status)
 
              End If
 
!           Calculate slice through data
              Call MA_PROJECTION (xmaxdat, ymaxdat, 1, 1, mxnumdat, mynumdat, &
                MDATA, MVARIANCES, experiment%x_pixel_size, &
                experiment%y_pixel_size, X_REGION, &
                Y_REGION, 0, variances_exist, &
                (experiment%x_pixel_size + experiment%y_pixel_size) / 2.0, &
                xmaxdat, retstat, xnumdat, XAXIS, DATA(1, y), VARIANCES, &
                NORMALISE, status)
 
              If (first) Then
 
                 first = .False.
 
!              Set values in memory X-axis
                 Do pixel = 1, xnumdat
                    XAXIS(pixel) = Real(pixel) - 0.5
                 End Do
 
              End If
 
!           Output 1-D data to the screen
              Call GS_XYSGRAPH (xmaxdat, 1, xnumdat, XAXIS, DATA(1, y), &
                file_name, xlabel, 'Intensity', status)
 
!           Generate output file name
              Call IO_FILEEXTENSION (file_name, save_1dext, retstat, &
                output_file, status)
 
              Call IO_WRITE ( 'INFO: Saving projection to file:', status)
              Call IO_WRITE (output_file, status)
 
!           Output projection to a file
              Call FIO_GUI_OUT (output_file, 'CHIPLOT', .True., xmaxdat, 1, 1, &
                1, xnumdat, 1, XAXIS, YAXIS, DATA(1, y), VARIANCES, title, &
                xlabel, ylabel, zlabel, variances_exist, &
                experiment%wavelength, experiment%detector_distance, &
                (experiment%x_pixel_size + experiment%y_pixel_size) / 2.0, &
                experiment%y_pixel_size, output_file, retstat, status)
 
           End If
 
        End Do
 
        Call IO_TIMES (time_elapse2, time_cpu2, status)
 
        mbytes = Real(mxnumdat * mynumdat * 2 * y) / (1024.0 * 1024.0)
        mb_per_minute = 60 * mbytes / (time_elapse2 - time_elapse1)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''num_images = '', i6)') y
!     Write (*, '(''MBYTES = '', f12.1)') mbytes
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Free projection array space
        Deallocate (NORMALISE)
 
        If (subtract_background) Then
 
!        Free dark current image array space
           Deallocate (BACKGROUND)
 
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Set scan number axis
        Do pixel = 1, y
           YAXIS(pixel) = Real(pixel) - 0.5
        End Do
 
        ylabel = 'Image Number'
 
        xstrelm = 1
        ystrelm = 1
        xendelm = xnumdat
        yendelm = y
        xnumdat = xnumdat
        ynumdat = y
        data_defined = .True.
 
!     Set combined title
        Call IO_NODIRPATH (last_file, file2, status)
        title = Trim(file1) // ' to ' // Trim(file2)
 
!     Information on performance
        Write (MESSAGE(1), '(''INFO: Time for processing = '', ' // &
          ' f12.3,'' seconds'')') time_elapse2 - time_elapse1
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''INFO: Assuming 16 bit, '', f12.2, ' // &
          ''' MBytes of input data processed per minute'')') mb_per_minute
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''      equivalent to '', f12.2, ' // &
          ''' GBytes per hour'')') mb_per_minute * 60.0 / 1024.0
        Call IO_WRITE (MESSAGE(1), status)
 
     End If
 
     End Subroutine F2D_FSPROJECTIONS
!********1*********2*********3*********4*********5*********6*********7*********8
 
