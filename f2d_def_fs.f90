!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_def_fs.f90 *
!  *                *
!  ******************
 
!+ F2D_DEF_FS - DEFine File Series
     Subroutine F2D_DEF_FS (input_options, ask_increment, variances_exist, &
       xmaxdat, ymaxdat, retstat, xnumdat, ynumdat, XAXIS, YAXIS, DATA, &
       VARIANCES, title, xlabel, ylabel, zlabel, xstrelm, ystrelm, &
       xendelm, yendelm, first_file, first_image, last_file, last_image, &
       start_value, end_value, prefix, variable_characters, num_characters, &
       postfix, extension, file_increment, experiment, status)
!  Description:
!    Define file series
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    24-Apr-2006: V0.4 ! Use "input_options" and "experiment" structures 
!      (Hammersley)
!    10-Jun-1999: V0.3 Set default file name to blank (Hammersley)
!    17-Feb-1999: V0.2 "F2D_GUI_INPUT" replaced by "FIO_GUI_INPUT" (Hammersley)
!    20-Feb-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc' ! IO data definitions
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
!  Import/Export:
     Logical, Intent(INOUT) :: ask_increment ! .True., if the file series
!      increment is to be asked
     Logical, Intent(INOUT) :: variances_exist ! .True., if a data variance
!      array is created
     Integer, Intent(INOUT) :: xmaxdat ! Dimension size in X-direction for
!      data arrays
     Integer, Intent(INOUT) :: ymaxdat ! Dimension size in Y-direction for
!      data arrays
!  Import/Export:
     Integer, Intent(INOUT) :: retstat ! Return status variable:
!       0: Good status, sequence components deduced
!      -1: Can't deduce sequence from file names
!      -2: No data input
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements in Y-direction
     Real, Intent(INOUT) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(INOUT) :: YAXIS(ymaxdat) ! Y-axis values
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! The estimated
!      variance values
     Character(Len = *), Intent(INOUT) :: title ! Title for plot
     Character(Len = *), Intent(INOUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: zlabel ! Z-axis label for plot
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
!  Export:
     Character(Len = *), Intent(OUT) :: first_file ! First file in file series
     Integer, Intent(INOUT) :: first_image ! First image number in file series
!      (not used at present)
     Character(Len = *), Intent(OUT) :: last_file ! Last file in file series
     Integer, Intent(INOUT) :: last_image ! Last image number in file series
!      (not used at present)
     Integer, Intent(OUT) :: start_value ! Number of first file in sequence
     Integer, Intent(OUT) :: end_value ! Number of last file in sequence
     Character(Len = *), Intent(OUT) :: prefix ! The part of the file name
!      before any varying numerical part
     Logical, Intent(OUT) :: variable_characters ! .True., if the number of
!      characters in the numerical part is changing
     Integer, Intent(OUT) :: num_characters ! If the number of characters is
!      fixed this is the number which are always found
     Character(Len = *), Intent(OUT) :: postfix ! The part of the file name
!      after any varying numerical part, but not including the extension
     Character(Len = *), Intent(OUT) :: extension ! Any file type extension,
!      after a decimal point
     Integer, Intent(OUT) :: file_increment ! Increment between input files
     Type(EXPERIMENTAL_DETAILS), Intent(OUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection
!      prompt text
!  Local Variables:
     Logical :: data_defined ! .True., if data has been input
     Logical :: file_ok ! .True., when the input file is O.K.
!  Local Arrays:
     Character(Len = 70) :: INFO(6) ! File input information text
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DEF_FS ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  First file in series
     first_file = ' '
     retstat = -2
     file_ok = .False.
     data_defined = .False.
     Do While (.Not. file_ok)
        PROMPT(1) = 'SELECT FIRST FILE IN SERIES'
        PROMPT(2) = '(click on "HELP" for list of formats)'
        Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 0, input_options, &
          xmaxdat, ymaxdat, variances_exist, data_defined, first_file, &
          xnumdat, ynumdat, XAXIS, YAXIS, DATA, VARIANCES, title, xlabel,& 
          ylabel, zlabel, experiment, status)
        xstrelm = 1
        ystrelm = 1
        xendelm = xnumdat
        yendelm = ynumdat
 
!     Check status
        If (status .Ne. St_goodvalue .Or. (.Not. data_defined)) Then
           Return
        End If
 
!     Display image
        Call GS_PLOT (xmaxdat, ymaxdat, DATA, XAXIS, YAXIS, xstrelm, ystrelm, &
          xendelm, yendelm, title, xlabel, ylabel, zlabel, status)
 
!     Check that the input file is O.K.
        Call GS_INPL (.True., 0, 1, .True., 'FILE O.K.', 1, &
          'Enter "YES" if the file is O.K.', 1, 'Enter an integer number', &
          file_ok, status)
 
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
     End Do
 
!  Last file in series
     last_file = ' '
     PROMPT(1) = 'Select LAST Input file in series'
     PROMPT(2) = '(click on "INFO" for details of file types)'
     INFO(1) = 'Select the last file in the series to be input.'
     INFO(2) = 'The file selection tool will help you to'
     INFO(3) = 'search directories and find the required file.'
     INFO(4) = ' '
     Call GS_FILESELECTION (2, 2, PROMPT, 4, 4, INFO, 1, .False., retstat, &
       last_file, status)
 
!  Deduce components of file sequence
     Call IO_FILESEQUENCE (first_file, last_file, retstat, start_value, &
       end_value, file_increment, prefix, variable_characters, num_characters, &
       postfix, extension, status)
 
     If (retstat .Ne. 0) Then
 
        Call GS_FWARNING (1, 1, 'COULD NOT DEDUCE REQUIRED FILE SEQUENCE', &
          status)
        Return
 
     End If
 
!  Clear graphics
     Call GS_BACKGROUND (status)
 
     If (ask_increment) Then
 
!     Input file increment
        INFO(1) = 'The "FILE INCREMENT" is the numerical step between'
        INFO(2) = 'input files. This allows, for example, every'
        INFO(3) = 'alternative file to be input by entering 2, or to'
        INFO(4) = 'define a series where the file number changes, 10,'
        INFO(5) = '20, 30, etc. by entering 10. Normally, to input'
        INFO(6) = 'every file, 1 should be entered.'
        Call GS_INPI (.True., 1, end_value - start_value + 1, .True., &
          'FILE INCREMENT', 6, INFO, 1, 'Enter integer in range', &
          file_increment, status)
 
     End If
 
     End Subroutine F2D_DEF_FS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 
 
 
 
