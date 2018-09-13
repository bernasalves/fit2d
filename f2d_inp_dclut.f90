!********1*********2*********3*********4*********5*********6*********7**
 
!  *********************
!  *                   *
!  * f2d_inp_dclut.f90 *
!  *                   *
!  *********************
 
!+ F2D_INP_DCLUT - FIT 2-D Input Distortion Correction Look-up Table
     Subroutine F2D_INP_DCLUT (gui, input_file, retstat, x_cor_size, &
       y_cor_size, xmax_lut, ymax_lut, xnum_lut, ynum_lut, X_SD, Y_SD, &
       INT_REBINNED, xmaxdat, ymaxdat, xnumdat, ynumdat, xstrelm, ystrelm, &
       xendelm, yendelm, INVERSE_FF, title, dc_lut_defined, &
       flat_field_defined, status)
!  Description:
!    Input distortion correction look-up table from a binary file.
!    The file is identified with the characters:
!    DISTORTION CORRECTION LOOK-UP TABLE V**.**
!    where the **.** refers to the version number.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    23-Feb-1999: V0.7 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.6 Change to use IO internal database routines (Hammersley)
!    17-Sep-1997: V0.5 Separate pure "input" code into "F2D_IN2DCLUT"
!      (Hammersley)
!    08-Sep-1997: V0.4 Input version 2 file with optional flat-field image 
!      (Hammersley)
!    18-Mar-1997: V0.3 Ultrix doesn't like "Mod(byte_value, 16)"
!      so use intermediate integer value (Hammersley)
!    07-Mar-1997: V0.2 Extend range of stored pixel shift differences 
!      (Hammersley)
!    03-Mar-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if the graphics window is to be
!      used for user prompts, messages and input
     Character(Len = *), Intent(IN) :: input_file ! Input file name
     Integer, Intent(IN) :: xmax_lut ! First dimension of look-up tables
     Integer, Intent(IN) :: ymax_lut ! Second dimension of look-up tables
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status:
!      0 = Good status
!      1 = Bad status
     Real, Intent(OUT) :: x_cor_size ! Size of corrected pixel in metres in
!      X-direction
     Real, Intent(OUT) :: y_cor_size ! Size of corrected pixel in metres in
!      Y-direction
     Integer, Intent(OUT) :: xnum_lut ! Number of pixels to define in LUT in
!      the X-direction
     Integer, Intent(OUT) :: ynum_lut ! Number of pixels to define in LUT in
!      the Y-direction
     Byte, Intent(OUT) :: X_SD(xmax_lut, ymax_lut) ! Rounded X-distortion in
!      pixels
     Byte, Intent(OUT) :: Y_SD(xmax_lut, ymax_lut) ! Rounded Y-distortion in
!      pixels
     Byte, Intent(OUT) :: INT_REBINNED(9, xmax_lut, ymax_lut)
!      "Fraction" of each input pixel rebinned into the 9 pixels which are the 
!      target pixel and the 8 nearest pixels. Each fraction is stored as a
!      byte, so change to unsigned, and divide by 256, to obtain the
!      required fraction. The order of the fractions is
!      lower left, lower middle, lower right, left middle, middle,
!      right of middle, upper left, upper middle, upper right.
     Integer, Intent(OUT) :: xnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: ynumdat ! Defines Y-extent of data region
     Integer, Intent(OUT) :: xstrelm ! Starting X-element of region to be fitted
     Integer, Intent(OUT) :: ystrelm ! Starting Y-element of region to be fitted
     Integer, Intent(OUT) :: xendelm ! End X-element of region to be fitted
     Integer, Intent(OUT) :: yendelm ! End Y-element of region to be fitted
     Real, Intent(OUT) :: INVERSE_FF(xmaxdat, ymaxdat) ! Flat-field image,
!      as stored in the file
     Character(Len = *), Intent(OUT) :: title ! Title label for data
     Logical, Intent(OUT) :: dc_lut_defined ! .True., if the LUT's have been
!      defined
     Logical, Intent(OUT) :: flat_field_defined ! .True., if the flat-field
!      response, has been stored in the distortion file
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.7' ! Version number
     Integer, Parameter :: Max_buffer = 1024 ! Size of byte buffer
!  Local Variables:
     Character(Len = 256) :: file_name ! Name of the input file
     Integer :: len_string ! Defined length of a string
     Logical :: file_ok ! .True., if the file is to be used
!  Local Arrays:
     Character(Len = 80) :: INFO(4) ! User info
     Character(Len = 80) :: MESSAGE(14) ! User messages
     Character(Len = 80) :: PROMPT(2) ! User prompts
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(''Entered Subroutine F2D_INP_DCLUT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_INP_DCLUT ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably
!  defined
     If (xmax_lut .Lt. 1 .Or. xmaxdat .Lt. 1) Then
        status = St_bad_dim1
     Else If (ymax_lut .Lt. 1 .Or. ymaxdat .Lt. 1) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
 
!     Add module number to status value
        status = St_mod_fit2d + status
 
!     Save details of subroutine call
        Call ST_SAVE ('Subroutine F2D_INP_DCLUT ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Try to inquire name from data-base
        Call IO_INQ_KEYVALUE ('DCLUT_FILE_NAME', len_string, file_name, &
          retstat, status)
 
        If (retstat .Ne. 0) Then
 
!        Create default output file name
           Call IO_OUTFILE (0, input_file, '.dclt', 'test.dclt', file_name, &
             status)
 
        End If
 
        If (gui) Then
 
!        Name of input file
           PROMPT(1) = 'USE ' // file_name
           MESSAGE(1) = file_name
           MESSAGE(2) = 'is the default file for input of ' // &
             'the look-up table.'
           MESSAGE(3) = 'If this file is appropriate enter ' // &
             '"YES". If you want '
           MESSAGE(4) = 'to use a different file, enter "NO".'
           Call GS_INPL (.True., 0, 1, .True., PROMPT(1), 4, MESSAGE, 1, &
             'Enter "YES" or "NO"', file_ok, status)
 
           If (.Not. file_ok) Then
 
!           Use file selection tool to obtain file name from user
              PROMPT(1) = 'SELECT OUTPUT FILE FOR DISTORTION'
              PROMPT(2) = 'CORRECTION LOOK-UP TABLE'
              INFO(1) = 'Use the file selection tool to select ' // &
                'a directory'
              INFO(2) = 'and an input file to contain the ' // 'distortion'
              INFO(3) = 'correction lut. Normally such ' // 'a file ends'
              INFO(4) = 'with the extension ".dclt".'
              Call GS_FILESELECTION (2, 2, PROMPT, 4, 4, INFO, 1, .False., &
                retstat, file_name, status)
 
!           Check return status
              If (retstat .Ne. 0) Then
                 Return
              End If
 
           End If
 
        Else
 
!        Input file name for output
           Call IO_INPC (.True., 'INPUT FILE NAME', 1, &
             'Name of file containing distortion correction LUT', 1, &
             'Unacceptable input', 1, file_name, status)
 
        End If
 
!     If user escape return immediately
        If (status .Eq. St_escapevalue) Then
           Return
        End If
 
!     Translate (if necessary) the entered file directory path
        Call IO_TRANS_PATH (file_name, retstat, file_name, status)
 
        If (retstat .Ne. 0) Then
           Call IO_WRITE ('WARNING: Problem with file ' // &
             'directory path (directory doesn''t exist ?)', status)
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input distortion correction data from the input file
        Call F2D_IN2DCLUT (file_name, retstat, x_cor_size, y_cor_size, &
          xmax_lut, ymax_lut, xnum_lut, ynum_lut, X_SD, Y_SD, INT_REBINNED, &
          xmaxdat, ymaxdat, INVERSE_FF, dc_lut_defined, flat_field_defined, &
          status)
 
        xnumdat = xnum_lut
        ynumdat = ynum_lut
        xstrelm = 1
        ystrelm = 1
        xendelm = xnumdat
        yendelm = ynumdat
        title = 'Inverse Flat-Field Response Image'
 
     End If
 
     End Subroutine F2D_INP_DCLUT
!********1*********2*********3*********4*********5*********6*********7**
 
 
 
 
 
