!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_average.f90 *
!  *                 *
!  *******************
 
!+ F2D_AVERAGE - FIT 2-D AVERAGE a series of files
     Subroutine F2D_AVERAGE (input_options, average, data_defined, &
       memory_exist, &
       memory_defined, variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, XAXIS, YAXIS, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
       x_pixel_size, y_pixel_size, xstrelm, ystrelm, xendelm, yendelm, &
       experiment, status)
!  Description:
!    Create an average from a series of files
!  Keywords:
!    Average.File~Series, File~Series.Average, Series.Files.Average
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    24-Apr-2006: V0.7 Add "input_options" and "experiment" structures 
!      (Hammersley)
!    09-Feb-2001: V0.6 Option to average or to only sum images (Hammersley)
!    17-Feb-1999: V0.5 "F2D_GUI_INPUT" replaced by "FIO_GUI_INPUT" (Hammersley)
!    09-Jul-1998: V0.4 Replace call to "Io_nopath" with "IO_NODIRPATH" 
!      (Hammersley)
!    22-Feb-1998: V0.3 Use "IO_FILENAME" to generate file names (Hammersley)
!    20-Feb-1998: V0.2 Call to "F2D_DEF_FS" (Hammersley)
!    26-Jan-1998: V0.1 Original, based  on "F2D_COMPOSITE" (Hammersley)
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
     Logical, Intent(IN) :: average ! .True., if the average is to be
!      formed, otherwise the sum is output
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
     Character(Len = *), Intent(INOUT) :: title ! Title for plot
     Character(Len = *), Intent(INOUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: zlabel ! Z-axis label for plot
     Real, Intent(INOUT) :: x_pixel_size ! Size of one unit (raw pixel) in
!      X-direction (metres)
     Real, Intent(INOUT) :: y_pixel_size ! Size of one unit (raw pixel) in
!      Y-direction (metres)
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.7' ! Version number
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection
!      prompt text
!  Local Variables:
     Character(Len = 256) :: extension ! File name extension
     Character(Len = 256) :: file_name ! Name of file in the series
     Character(Len = 80) :: file1 ! First file name without directory path
     Character(Len = 80) :: file2 ! Second file name without directory path
     Character(Len = 256) :: first_file ! Full name of first file
     Character(Len = 256) :: last_file ! Full name of last file
     Character(Len = 80) :: message ! User messages
     Character(Len = 256) :: postfix ! Fixed end of file names
     Character(Len = 256) :: prefix ! Fixed start of file names
     Integer :: end_value ! Value at end of sequence
     Integer, Save :: increment = 1 ! Step value in sequence
     Integer :: file ! Loop variable for files
     Integer :: first_image ! First image in series (not used)
     Integer :: last_image ! Last image in series (not used)
     Integer :: num_characters ! Number of characters in numerical part
     Integer :: num_input ! Number of data images sucessfully input
     Integer :: retstat ! Return status variable
     Integer :: start_value ! Value at start of sequence
     Integer stat ! Status return variable for "Allocate"
     Logical :: variable_characters ! .True., if the number of characters
!      in the numerical part of the names changes
!  Local Arrays:
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
     Real, Allocatable :: TMP(:, :) ! Dynamic array to contain input data
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_AVERAGE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_AVERAGE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Define file series
        Call F2D_DEF_FS (input_options, .True., variances_exist, &
          xmaxdat, ymaxdat, retstat, &
          xnumdat, ynumdat, XAXIS, YAXIS, DATA, VARIANCES, title, xlabel, &
          ylabel, zlabel, xstrelm, ystrelm, &
          xendelm, yendelm, first_file, first_image, last_file, last_image, &
          start_value, end_value, prefix, variable_characters, num_characters, &
          postfix, extension, increment, experiment, status)
 
!     Check return status
        If (retstat .Ne. 0) Then
           Return
        End If
 
!     Add the "postfix" to the extension if existing
        If (Len_trim(postfix) .Gt. 0) Then
 
           If (Len_trim(extension) .Gt. 0) Then
              extension = Trim(postfix) // Trim(extension)
           Else
              extension = Trim(postfix)
           End If
 
        End If
 
        Call IO_NODIRPATH (first_file, file1, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Create temporary array to contain input data
        Allocate (TMP(xnumdat, ynumdat), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_AVERAGE ' // Version)
           Return
        End If
 
!     User progress macro
        Call GS_BACKGROUND (status)
        Call GS_FPROMPT (1, 1, 'AUTOMATIC SEQUENCE INPUT STARTED', status)
        Call GS_UPDATE (status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Automatic part of sequence processing
        num_input = 1
 
        Do file = start_value + increment, end_value, increment
 
!        Generate input file name
           Call IO_FILENAME (prefix, file, .Not. variable_characters, &
             num_characters, extension, retstat, file_name, status)
 
!        Input file
           data_defined = .False.
           Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, -2147483647, &
            input_options, xendelm, &
             yendelm, variances_exist, data_defined, file_name, xnumdat, &
             ynumdat, XAXIS, YAXIS, TMP, VARIANCES, title, xlabel, &
             ylabel, zlabel, experiment, status)
 
           If (data_defined) Then
 
              Call IO_WRITE ('INFO: Input ' // file_name, status)
 
!           Add image data to sum of images
              Call MA_RADD (xnumdat, ynumdat, 1, 1, xnumdat, ynumdat, &
                TMP, xmaxdat, ymaxdat, DATA, status)
 
              num_input = num_input + 1
 
           Else
 
!           File not input
              Call IO_WRITE ('WARNING: File not input: ' // file_name, status)
 
           End If
 
        End Do
 
        If (average) Then
 
!        Divide by number of input images
           Call MA_RCMULT (xmaxdat, ymaxdat, 1, 1, xnumdat, ynumdat, 1.0 / &
             Real(num_input), DATA, status)
 
        End If
 
!     Free dynamic memory
        Deallocate (TMP)
 
!     User information
        Write (message, '(''INFO: Number of correctly input images = '', i6)') &
          num_input
        Call IO_WRITE (message, status)
 
        data_defined = .True.
 
!     Set combined title
        Call IO_NODIRPATH (last_file, file2, status)
        title = Trim(file1) // ' to ' // Trim(file2)
 
     End If
 
     End Subroutine F2D_AVERAGE
!********1*********2*********3*********4*********5*********6*********7*********8
