!********1*********2*********3*********4*********5*********6*********7********8
 
!  *******************
!  *                 *
!  * f2d_fsinput.f90 *
!  *                 *
!  *******************
 
!+ F2D_FSINPUT - File Series: INPUT (multiple 1-D)
     Subroutine F2D_FSINPUT (input_options, &
       data_defined, memory_exist, memory_defined, &
       variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, &
       YAXIS, DATA, VARIANCES, MASK, title, xlabel, ylabel, zlabel, &
       x_pixel_size, y_pixel_size, xstrelm, ystrelm, xendelm, yendelm, &
       mxnumdat, mynumdat, MXAXIS, MYAXIS, MDATA, MVARIANCES, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, experiment, status)
!  Description:
!    Input a series of 1-D data-sets from file and build up a 2-D "image" of 1-D
!    scans.
!  Keywords:
!    Input.File~Series, File~Series.Input, Series.Files.Input
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    24-Apr-2006: V0.4 Add "input_options" and "experiment" structures
!      (Hammersley)
!    17-Feb-1999: V0.3 "F2D_GUI_INPUT" replaced by "FIO_GUI_INPUT" (Hammersley)
!    02-Nov-1998: V0.2 Correct generation of file names, when there is a 
!      post-fix part (Hammersley)
!    27-Oct-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic 'status' constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
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
     Real, Intent(INOUT) :: x_pixel_size ! Size of one unit (raw pixel) in
!      X-direction (metres)
     Real, Intent(INOUT) :: y_pixel_size ! Size of one unit (raw pixel) in
!      Y-direction (metres)
     Integer, Intent(INOUT) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: xendelm ! The X-pixel number for the end of
!      the ROI
     Integer, Intent(INOUT) :: yendelm ! The Y-pixel number for the end of
!      the ROI
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
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection prompt 
!      text
!  Local Variables:
     Character(Len = 256) :: extension ! File name extension
     Character(Len = 256) :: file_name ! Name of file in the series
     Character(Len = 256) :: first_file ! Full name of first file
     Character(Len = 256) :: last_file ! Full name of last file
     Character(Len = 256) :: postfix ! Fixed end of file names
     Character(Len = 256) :: prefix ! Fixed start of file names
     Integer :: end_value ! Value at end of sequence
     Integer :: increment ! Step value in sequence
     Integer :: input_row ! Number of row to input
     Integer :: file ! Loop variable for files
     Integer :: first_image ! First image in series (not used)
     Integer :: last_image ! Last image in series (not used)
     Integer :: num_characters ! Number of characters in numerical part
     Integer :: pixel ! Loop variable
     Integer :: retstat ! Return status variable for "GS_FILESELECTION"
     Integer :: start_value ! Value at start of sequence
     Integer :: y ! Loop variable for output rows
     Logical :: variable_characters ! .True., if the number of characters in the
!      numerical part of the names changes
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(2) ! User messages
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FSINPUT ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_FSINPUT ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Define file series
        Call F2D_DEF_FS (input_options, .True., variances_exist, &
          xmaxdat, ymaxdat, retstat, &
          mxnumdat, mynumdat, MXAXIS, MYAXIS, MDATA, MVARIANCES, mtitle, &
          mxlabel, mylabel, mzlabel, mxstrelm, &
          mystrelm, mxendelm, myendelm, first_file, first_image, last_file, &
          last_image, start_value, end_value, prefix, variable_characters, &
          num_characters, postfix, extension, increment, experiment, status)
 
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
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Row to input
        input_row = mystrelm
        Call GS_INPI (.True., mystrelm, myendelm, .True., &
          'ENTER NUMBER OF ROW TO INPUT', 1, &
          'You can choose to input one row within the given range', 1, &
          'Number must be within given range', input_row, status)
 
!     Input data and transfer to 2-D array
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
                input_row, variances_exist, data_defined, file_name, mxnumdat, &
                mynumdat, MXAXIS, MYAXIS, MDATA, MVARIANCES, mtitle, mxlabel, &
                mylabel, mzlabel, experiment, status)
 
              If (data_defined) Then
 
                 Call IO_WRITE ('INFO: Input ' // file_name, status)
 
              End If
 
           End If
 
           If (data_defined) Then
 
!           Transfer row to appropriate row in 2-D image
              Call MA_RMOVE (xmaxdat, ymaxdat, MDATA, mxstrelm, input_row, &
                mxendelm, input_row, xmaxdat, ymaxdat, mxstrelm, y, DATA, &
                status)
 
           Else
 
!           File not input
              Call IO_WRITE ('WARNING: File not input: ' // file_name, status)
 
!           Set array values to zero
              Call MA_RVALUE (xmaxdat, ymaxdat, mxstrelm, y, mxendelm, y, 0.0, &
                DATA, status)
 
           End If
 
        End Do
 
!     Set data region
        data_defined = .True.
        xnumdat = mxendelm
        ynumdat = y
        xstrelm = mxstrelm
        xendelm = mxendelm
        ystrelm = 1
        yendelm = y
        xlabel = mxlabel
        ylabel = 'Scan Number'
 
!     Copy X-axis data
        Do pixel = 1, xendelm
           XAXIS(pixel) = MXAXIS(pixel)
        End Do
 
!     Set scan number axis
        Do pixel = 1, y
           YAXIS(pixel) = Real(pixel) - 0.5
        End Do
 
        mx_pixel_size = experiment%x_pixel_size
        my_pixel_size = experiment%y_pixel_size

     End If
 
     End Subroutine F2D_FSINPUT
!********1*********2*********3*********4*********5*********6*********7********8
 
 
 
 
 
 
