!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_fsstatistics.f90 *
!  *                      *
!  ************************
 
!+ F2D_FSSTATISTICS - File Series: INTEGRATE
     Subroutine F2D_FSSTATISTICS (input_options, data_defined, &
       variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, &
       YAXIS, DATA, VARIANCES, MASK, title, xlabel, ylabel, zlabel, &
       experiment, xstrelm, ystrelm, xendelm, yendelm, results, status)
!  Description:
!    Calculate statistics from a series of files, to output to results
!    vectors.
!  Keywords:
!    Statistics.File~Series, File~Series.Statistics, Series.Files.Statistics
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    09-Dec-2014: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined ! .True., if data exists within
!      the program
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
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
     Type(RESULT_VECTORS), Intent(INOUT) :: results ! Result vectors
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection
!      prompt text
!  Local Variables:
     Integer :: end_value ! Value at end of sequence
     Character(Len = 256) :: extension ! File name extension
     Integer :: file ! Loop variable for files
     Character(Len = 256) :: file_name ! Name of file in the series
     Character(Len = 80) :: file1 ! First file name without directory path
     Character(Len = 80) :: file2 ! Second file name without directory path
     Integer :: i ! Loop variable for vectors
     Character(Len = 256) :: postfix ! Fixed end of file names
     Character(Len = 256) :: prefix ! Fixed start of file names
     Integer :: first_image ! First image in series (not used)
     Character(Len = 256) :: first_file ! Full name of first file
     Integer, Save :: increment ! Step value in sequence
     Character(Len = 256) :: last_file ! Full name of last file
     Integer :: last_image ! Last image in series (not used)
     Integer :: len_string ! Defined length of a string
     Real :: maximum ! The maximum value in the region
     Real :: mb_per_minute ! Number of MBytes of input data processed per
!      minute
     Real :: mbytes ! Total number of MBytes of input data processed
     Real :: minimum ! The minimum value found in the region
     Real :: mean ! The average of all the values found in the
!      region
     Real :: total ! The total sum of the elements in the
!      region
     Real :: rms ! The root mean square of the elements in the
!      region
     Real :: sigma ! The sample standard deviation (N-1, 
!      definition) of all the values found in the region
     Real :: skewness ! The skewness of the distribution
     Real :: par1 ! Unused at present, reserved for future needs
     Real :: par2 ! Unused at present, reserved for future needs
     Real :: par3 ! Unused at present, reserved for future needs
     Real :: par4 ! Unused at present, reserved for future needs
     Integer :: num_characters ! Number of characters in numerical part
     Integer :: retstat ! Return status variable
     Integer :: start_value ! Value at start of sequence
     Real :: time_cpu1 ! CPU time at start of series
     Real :: time_cpu2 ! CPU time at end of series
     Real :: time_elapse1 ! Elapse time at start of series
     Real :: time_elapse2 ! Elapse time at end of series
     Logical :: variable_characters ! .True., if the number of characters
!      in the numerical part of the names changes
     Integer :: y ! Variable for the input images
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(7) ! User messages
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FSSTATISTICS ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_FSSTATISTICS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead
 
!     Define file series
        Call F2D_DEF_FS (input_options, &
          .True., variances_exist, xmaxdat, ymaxdat, retstat, &
          xnumdat, ynumdat, XAXIS, YAXIS, DATA, VARIANCES, title, xlabel, &
          ylabel, zlabel, xstrelm, ystrelm, xendelm, yendelm, &
          first_file, first_image, last_file, last_image, &
          start_value, end_value, prefix, variable_characters, num_characters, &
          postfix, extension, increment, experiment, status)
 
!     Check return status
        If (retstat .Ne. 0) Then
           Return
        Else
           data_defined = .True.
        End If
 
!     Add the "postfix" to the extension if it exists
        If (Len_trim(postfix) .Gt. 0) Then
 
           If (Len_trim(extension) .Gt. 0) Then
              extension = Trim(postfix) // Trim(extension)
           Else
              extension = Trim(postfix)
           End If
 
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''extension = '', a)') Trim(extension)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Remove directory path
        Call IO_NODIRPATH (first_file, file1, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     User progress macro
        Call GS_BACKGROUND (status)
        Call GS_FPROMPT (1, 1, 'AUTOMATIC SEQUENCE INPUT STARTED', status)
        Call GS_UPDATE (status)
 
        Call IO_TIMES (time_elapse1, time_cpu1, status)

!     Automatic part of sequence processing
        y = 0
        Do file = start_value, end_value, increment
 
           y = y + 1
 
!        Generate input file name
           Call IO_FILENAME (prefix, file, .Not. variable_characters, &
             num_characters, extension, retstat, file_name, status)
 
           If (y .Ne. 1) Then
 
!           Progress report
              MESSAGE(1) = 'INPUTTING FILE:'
              MESSAGE(2) = file_name
              Call GS_FPROMPT (2, 2, MESSAGE, status)
              Call GS_UPDATE (status)

!           Input file
              data_defined = .False.
              Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, -2147483647, &
                input_options, xmaxdat, &
                ymaxdat, variances_exist, data_defined, file_name, xnumdat, &
                ynumdat, XAXIS, YAXIS, DATA, VARIANCES, title, xlabel, ylabel, &
                zlabel, experiment, status)
 
              If (data_defined) Then
 
                 Call IO_WRITE ('INFO: Input ' // file_name, status)
 
              End If
 
           End If
 
           If (data_defined) Then
 
!           Output user message
              Call GS_FPROMPT (1, 1, 'CALCULATING STATISTICS', status)
              Call GS_UPDATE (status)
              Call IO_WRITE ('INFO: Calculating statistics', status)
  
              Call MA_STATISTICS (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, &
                xendelm, yendelm, maximum, minimum, mean, total, rms, sigma, &
                skewness, par1, par2, par3, par4, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''Statistics finished'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!           Transfer results to results vector
              If (y .Lt. results%max_values) Then
                 results%VECTORS(y, 1) = maximum
                 results%VECTORS(y, 2) = minimum
                 results%VECTORS(y, 3) = mean
                 results%VECTORS(y, 4) = total
                 results%VECTORS(y, 5) = rms
                 results%VECTORS(y, 6) = sigma
                 results%VECTORS(y, 7) = skewness
              End If

           Else
 
!           File not input
              Call IO_WRITE ('WARNING: File not input: ' // file_name, status)
 
           End If
 
        End Do

!     Find end time
        Call IO_TIMES (time_elapse2, time_cpu2, status)
 
        mbytes = Real(xnumdat * ynumdat * 2 * y) / (1024.0 * 1024.0)
        mb_per_minute = 60.0 * mbytes / (time_elapse2 - time_elapse1)
 
!     Set number of defined vectors
        results%num_vectors = 7

!     Set vector defined regions
        Do i = 1, 7
           results%STARTS(i) = 1
           results%ENDS(i) = y
        End Do

!     Define result vector titles
        results%TITLES(1) = 'Maximums'
        results%TITLES(2) = 'Minimums'
        results%TITLES(3) = 'Means'
        results%TITLES(4) = 'Totals'
        results%TITLES(5) = 'Root Mean Squares'
        results%TITLES(6) = 'Standard Deviations'
        results%TITLES(7) = 'Skewnesses'

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
        Write (MESSAGE(1), '(''      equivalent to '', f12.2, ' // &
          ''' TBytes per day'')') &
          mb_per_minute * 60.0 * 24.0 / (1024.0 * 1024.0)
        Call IO_WRITE (MESSAGE(1), status)

     End If
 
     End Subroutine F2D_FSSTATISTICS
!********1*********2*********3*********4*********5*********6*********7*********8
 
