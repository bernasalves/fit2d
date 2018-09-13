!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_out_dclut.f90 *
!  *                   *
!  *********************
 
!+ F2D_OUT_DCLUT - FIT 2-D OUTput Distortion Correction Look-Up Table
     Subroutine F2D_OUT_DCLUT (gui, input_file, x_cor_size, y_cor_size, &
       xmax_lut, ymax_lut, xnum_lut, ynum_lut, X_SD, Y_SD, INT_REBINNED, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, retstat, status)
!  Description:
!    Save look-up table in a binary file. Avoiding storing unnecessary 
!    information. The file is identified with the characters:
!    DISTORTION CORRECTION LOOK-UP TABLE V02.00
!  Method:
!    A dedicated file format is written, but with a keyworded ASCII header, and
!    sufficient information to allow for storage of a separate flat-field 
!    correction file or not, and possible future choice of 1-byte per output 
!    pixel or two byte per output pixel in the spatial distortion correction
!    LUT.
!
!    At present 1 byte is used to store the fractional output from
!    each input pixel into one of up to 3*3 output pixels. This
!    stores the fractions rounded to units of 1/255. Slight adjustment
!    is applied to ensure that all the fractions add up to 1.0
!    exactly. The LUT is stored in a "packed" format. Initially the
!    vector which maps the first input pixel to the output pixel is
!    stored in 2 signed bytes, which allows shifts of -128 to 127
!    pixels for the X and Y components. All other vectors are stored
!    relative changes to the previous vector, and both the X and the
!    Y components are packed into a single byte. This allows a relative
!    change in distortion of +-7 pixels per pixel. At the start of each
!    new line the change is taken relative to the previous line.
!    For each input pixel one byte is used to store which of the 8 unknown
!    output pixels have contributions or not (the lower lefthand pixel
!    is always contributed to). This is immediately followed by each
!    of the output pixel contributions except the last one, which can
!    be deduced from the 255 minus the sum of the rest. In the case
!    that all the input pixel goes into only one output pixel, no
!    fractions need to be stored. The fractions immediately follow
!    the output pixel occupation byte, and the next occupation byte follows.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    12-Oct-1999: V0.9 Use FIO instead of IO routines (Hammersley)
!    23-Feb-1999: V0.8 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.7 Change to use IO internal database routines (Hammersley)
!    08-Jun-1998: V0.6 Correct GUI input of output file name to be for an output
!      file (previously, was for an input file) (Hammersley)
!    19-Dec-1997: V0.5 Ultrix doesn't like:
!      byte_value = Ibset(byte_value, 7)
!        so replace with:
!      byte_value = byte_value - 256 (Hammersley)
!    10-Sep-1997: V0.4 Check that the sum of fractions adds up to 255 
!      (Hammersley)
!    03-Sep-1997: V0.3 Option to save separate 2-byte per pixel flat-field 
!      response within output file (Hammersley)
!    07-Mar-1997: V0.2 Output relative shifts which are too large to be saved, 
!      and extend stored range (Hammersley)
!    27-Feb-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if the graphics window is to be
!      used for user prompts, messages and input
     Character(Len = *), Intent(IN) :: input_file ! Input file name
     Real, Intent(IN) :: x_cor_size ! Size of corrected pixel in metres in
!      X-direction
     Real, Intent(IN) :: y_cor_size ! Size of corrected pixel in metres in
!      Y-direction
     Integer, Intent(IN) :: xmax_lut ! First dimension of look-up tables
     Integer, Intent(IN) :: ymax_lut ! Second dimension of look-up tables
     Integer, Intent(IN) :: xnum_lut ! Number of pixels to define in LUT in
!      the X-direction
     Integer, Intent(IN) :: ynum_lut ! Number of pixels to define in LUT in
!      the Y-direction
     Byte, Intent(IN) :: X_SD(xmax_lut, ymax_lut) ! Rounded X-distortion in
!      pixels
     Byte, Intent(IN) :: Y_SD(xmax_lut, ymax_lut) ! Rounded Y-distortion in
!      pixels
     Byte, Intent(IN) :: INT_REBINNED(9, xmax_lut, ymax_lut)
!      "Fraction" of each input pixel rebinned into the 9 pixels which are the 
!      targetpixel and the 8 nearest pixels with the exception of the upper
!      right pixel. Each fraction is stored as a byte, so change to
!      unsigned, and divide by 256, to obtain the required fraction.
!      Since the total must equal 1.0, the fraction for the upper
!      right pixel can be deduced. The order of the fractions is
!      lower left, lower middle, lower right, left of target, target,
!      right of target, upper left, and upper middle.
     Integer, Intent(IN) :: xmaxdat ! First dimension of "DATA" array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of "DATA" array
     Integer, Intent(IN) :: xnumdat ! Defines X-extent of data region
     Integer, Intent(IN) :: ynumdat ! Defines Y-extent of data region
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Array containing
!      flat-field response data
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status:
!      0 = Good status
!      1 = Bad status
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.9' ! Version number
     Integer, Parameter :: Max_buffer = 1024 ! Size of byte buffer
!  Local Variables:
     Byte :: byte1 ! First byte to store of a two byte integer
     Byte :: byte2 ! Second byte to store of a two byte integer
     Byte :: byte_value ! Value used to store which of the possible output 
!      fractions are stored
     Character(Len = 256) :: file_name ! Name of the output file
     Character(Len = 50) :: text ! Stores character text prior to output
     Integer :: chr ! Loop variable for characters
     Integer :: combined_shifts ! The X and Y relative shifts in distortion 
!      combined in a single value
     Integer :: file_id ! "C" file identifier for output
     Integer :: filstat ! File I/O status return variable
     Integer :: fraction ! Loop variable for 9 output fractions
     Integer :: int1 ! Temporary storage value
     Integer :: int_value ! Scaled normalised flat-field response for 2-byte 
!      unsigned integer output
     Integer :: len_string ! Defined length of string
     Integer :: num_contributes ! Number of output pixels which an input pixel
!      provides a non-zero contribution
     Integer :: pos_buffer ! Position of filled buffer
     Integer :: sum ! Sum of output fractions
     Integer :: written_bytes ! Number of bytes successfully output to file
     Integer :: x ! Loop variable for X-direction
     Integer :: x_ref ! X-direction reference spatial distortion shift
     Integer :: x_rel_shift ! X-direction spatial distortion shift relative to
!      the previous pixel
!    Integer xtest ! X test pixel position
     Integer :: y ! Loop variable for Y-direction
     Integer :: y_ref ! Y-direction reference spatial distortion shift
     Integer :: y_rel_shift ! Y-direction spatial distortion shift relative to 
!      the previous pixel
!    Integer ytest ! Y test pixel position
     Logical :: file_ok ! .True., if the file is to be used
     Logical, Save :: store_flat_field = .True. ! .True., if the flat-field
!      response, which should be in the 'DATA' array, is to be stored in the
!      output file
     Real :: maximum_value ! Maximum value in flat-field response image
     Real :: minimum_value ! Minimum value in flat-field response image
     Real :: range ! Range of values in flat-field response image
     Real :: scale ! Scale factor from floating point flat-field response image
!      to stored 2-byte unsigned integers
!  Local Arrays:
     Byte :: BUFFER(Max_buffer) ! Output byte buffer
     Character(Len = 80) :: INFO(4) ! User info
     Character(Len = 80) :: MESSAGE(4) ! User messages
     Character(Len = 80) :: PROMPT(2) ! User prompts
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_OUT_DCLUT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_OUT_DCLUT ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably defined
     If (xmax_lut .Lt. 1 .Or. xmaxdat .Lt. 1) Then
        status = St_bad_dim1
     Else If (ymax_lut .Lt. 1 .Or. ymaxdat .Lt. 1) Then
        status = St_bad_dim2
     Else If (xnumdat .Lt. 1 .Or. xnumdat .Gt. xmaxdat) Then
        status = St_bad_adr1
     Else If (ynumdat .Lt. 1 .Or. ynumdat .Gt. ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
 
!     Add module number to status value
        status = St_mod_fit2d + status
 
!     Save details of subroutine call
        Call ST_SAVE ('Subroutine F2D_OUT_DCLUT ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Check that the defined size of the LUT is equal to the defined
!     size of the data array
        If (xnum_lut .Ne. xnumdat .Or. ynum_lut .Ne. ynumdat) Then
 
           Call IO_WRITE ('ERROR: The defined size of the ' // &
             'data is not the same as the size of the', status)
           Call IO_WRITE ('       look-up table.', status)
 
           Write (*, '(''xnum_lut, ynum_lut = '', 2i8, ' // &
             ''' xnumdat, ynumdat = '', 2i8)') xnum_lut, ynum_lut, xnumdat, &
             ynumdat
 
           Return
 
        End If
 
!     Create default output file name
        Call IO_OUTFILE (0, input_file, '.dclt', 'test.dclt', file_name, &
          status)
 
        If (gui) Then
 
!        Name of output file
           PROMPT(1) = 'USE ' // file_name
           MESSAGE(1) = file_name
           MESSAGE(2) = 'is the default file for output of ' // &
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
              INFO(2) = 'and an output file to contain the distortion'
              INFO(3) = 'correction lut. Normally such a file ends'
              INFO(4) = 'with the extension ".dclt".'
              Call GS_FILESELECTION (2, 2, PROMPT, 4, 4, INFO, 3, .False., &
                retstat, file_name, status)
 
!           Check return status
              If (retstat .Ne. 0) Then
                 Return
              End If
 
           End If
 
        Else
 
!        Input file name for output
           Call IO_INPC (.True., 'OUTPUT FILE NAME', 1, &
             'Name of file to contain output data',  1, 'Unacceptable input', &
             1, file_name, status)
 
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
 
        If (gui) Then
 
           Call GS_INPL (.True., 0, 1, .True., &
             'STORE INVERSE NORMALISED FLAT-FIELD RESPONSE ' // &
             '(MUST BE PRESENT)', 1, &
             'Enter "YES" to save inverse flat-field response data', 1, &
             'Click on "YES" or "NO"', store_flat_field, status)
 
        Else
 
           Call IO_INPL (.True., 0, 1, .True., &
             'STORE INVERSE NORMALISED FLAT-FIELD RESPONSE ' // &
             '(MUST BE PRESENT)', 1, &
             'Enter "YES" to save inverse flat-field response data', 1, &
             'Click on "YES" or "NO"', store_flat_field, status)
 
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Enter xtest, ytest pixel position'')')
!     Read (*, *) xtest, ytest
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Open file for output
        Call FIO_FILEOPEN (file_name, 'WRITE', filstat, file_id, status)
 
        If (filstat .Ne. 0) Then
           Call IO_WRITE ( 'WARNING: Error in opening file', status)
           retstat = 1
           Return
        End If
 
!     Output look-up table
 
!     Output file type identifier
        text = 'DISTORTION CORRECTION LOOK-UP TABLE V02.00'
        pos_buffer = Len_trim(text)
        Do chr = 1, pos_buffer
           BUFFER(chr) = Ichar(text(chr:chr))
        End Do
        BUFFER(pos_buffer + 1) = 13
        BUFFER(pos_buffer + 2) = 10
        pos_buffer = pos_buffer + 2
 
!     Output X-direction pixel size in metres
        Write (text, '(''X_PIXEL_SIZE = '', 1pe14.7)') x_cor_size
        Do chr = 1, Len_trim(text)
           BUFFER(pos_buffer + chr) = Ichar(text(chr:chr))
        End Do
        pos_buffer = pos_buffer + Len_trim(text)
        BUFFER(pos_buffer + 1) = 13
        BUFFER(pos_buffer + 2) = 10
        pos_buffer = pos_buffer + 2
 
!     Output Y-direction pixel size in metres
        Write (text, '(''Y_PIXEL_SIZE = '', 1pe14.7)') y_cor_size
        Do chr = 1, Len_trim(text)
           BUFFER(pos_buffer + chr) = Ichar(text(chr:chr))
        End Do
        pos_buffer = pos_buffer + Len_trim(text)
        BUFFER(pos_buffer + 1) = 13
        BUFFER(pos_buffer + 2) = 10
        pos_buffer = pos_buffer + 2
 
!     Output number of X-pixels
        Write (text, '(''X_NUMBER = '', i12)') xnum_lut
        Do chr = 1, Len_trim(text)
           BUFFER(pos_buffer + chr) = Ichar(text(chr:chr))
        End Do
        pos_buffer = pos_buffer + Len_trim(text)
        BUFFER(pos_buffer + 1) = 13
        BUFFER(pos_buffer + 2) = 10
        pos_buffer = pos_buffer + 2
 
!     Output number of Y-pixels
        Write (text, '(''Y_NUMBER = '', i12)') ynum_lut
        Do chr = 1, Len_trim(text)
           BUFFER(pos_buffer + chr) = Ichar(text(chr:chr))
        End Do
        pos_buffer = pos_buffer + Len_trim(text)
        BUFFER(pos_buffer + 1) = 13
        BUFFER(pos_buffer + 2) = 10
        pos_buffer = pos_buffer + 2
 
!     Output 1-byte per pixel or 2-byte per pixel variety of LUT
        Write (text, '(''BYTES_PER_PIXEL_LUT = '', i12)') 1
        Do chr = 1, Len_trim(text)
           BUFFER(pos_buffer + chr) = Ichar(text(chr:chr))
        End Do
        pos_buffer = pos_buffer + Len_trim(text)
        BUFFER(pos_buffer + 1) = 13
        BUFFER(pos_buffer + 2) = 10
        pos_buffer = pos_buffer + 2
 
!     Is the flat-field image stored ?
        Write (text, '(''FLAT_FIELD_STORED = '', l1)') store_flat_field
        Do chr = 1, Len_trim(text)
           BUFFER(pos_buffer + chr) = Ichar(text(chr:chr))
        End Do
        pos_buffer = pos_buffer + Len_trim(text)
        BUFFER(pos_buffer + 1) = 13
        BUFFER(pos_buffer + 2) = 10
        pos_buffer = pos_buffer + 2
 
        If (store_flat_field) Then
 
!        Find out minimum and maximum of flat-field data values
           Call MA_RMINMAX (xmaxdat, ymaxdat, DATA, 1, 1, xnumdat, ynumdat, &
             minimum_value, maximum_value, status)
 
           range = Max(1.0e-5, maximum_value - minimum_value)
           scale = 65535.0 / range
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''offset, scale = '', 2(1pe14.7))')
!        :          minimum_value, scale
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Output flat-field offset value
           Write (text, '(''FF_OFFSET = '', 1pe14.7)') minimum_value
           Do chr = 1, Len_trim(text)
              BUFFER(pos_buffer + chr) = Ichar(text(chr:chr))
           End Do
           pos_buffer = pos_buffer + Len_trim(text)
           BUFFER(pos_buffer + 1) = 13
           BUFFER(pos_buffer + 2) = 10
           pos_buffer = pos_buffer + 2
 
!        Output flat-field scaling value
           Write (text, '(''FF_SCALING = '', 1pe14.7)') scale
           Do chr = 1, Len_trim(text)
              BUFFER(pos_buffer + chr) = Ichar(text(chr:chr))
           End Do
           pos_buffer = pos_buffer + Len_trim(text)
           BUFFER(pos_buffer + 1) = 13
           BUFFER(pos_buffer + 2) = 10
           pos_buffer = pos_buffer + 2
 
        End If
 
!     End of ASCII header section, add Ctrl-D, Crtl-Z to stop
!     text output cleanly (if possible)
        text = '!!! END OF HEADER SECTION !!!'
        Do chr = 1, Len_trim(text)
           BUFFER(pos_buffer + chr) = Ichar(text(chr:chr))
        End Do
        pos_buffer = pos_buffer + Len_trim(text)
        BUFFER(pos_buffer + 1) = 13 ! \cr
        BUFFER(pos_buffer + 2) = 10 ! \lf
        BUFFER(pos_buffer + 3) = 4 ! Ctrl-D
        BUFFER(pos_buffer + 4) = 26 ! Ctrl-Z
        pos_buffer = pos_buffer + 4
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        If (store_flat_field) Then
 
!        Add zero valued byte if the number of defined bytes is odd
           If (Mod(pos_buffer, 2) .Ne. 0) Then
              BUFFER(pos_buffer + 1) = 0
              pos_buffer = pos_buffer + 1
           End If
 
           Do y = 1, ynumdat
 
              Do x = 1, xnumdat
 
!              Convert value to a 2-byte unsigned integer value
 
!              Store most significant byte value in buffer
                 int_value = Nint((DATA(x, y) - minimum_value) * scale)
                 int1 = int_value / 256
                 If (int1 .Gt. 127) Then
                    byte1 = int1 - 256
                 Else
                    byte1 = int1
                 End If
                 Call F2D_SUBOUTDCLUT (file_id, Max_buffer, byte1, pos_buffer, &
                   BUFFER, retstat, status)
 
!              Store least significant byte value in buffer
                 int1 = Mod(int_value, 256)
                 If (int1 .Gt. 127) Then
                    byte2 = int1 - 256
                 Else
                    byte2 = int1
                 End If
                 Call F2D_SUBOUTDCLUT (file_id, Max_buffer, byte2, pos_buffer, &
                   BUFFER, retstat, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              If (x .Eq. 150 .And. y .Eq. 1) Then
!
!              Write (*, '(''DATA, int, byte1, byte2 = '', ' //
!              :                   '1pe14.7, 3i6)')
!              :                   DATA(150, 1), int_value, byte1, byte2
!              End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              End Do
 
           End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''End of flat field: pos_buffer = '', i5)')
!        :          pos_buffer
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Output X and Y relative distortion values
        Do y = 1, ynum_lut
 
           Do x = 1, xnum_lut
 
              If (x .Eq. 1 .And. y .Eq. 1) Then
 
!              Output initial X and Y distortion values
                 x_ref = X_SD(1, 1)
                 y_ref = Y_SD(1, 1)
                 If (x_ref .Le. 127 .And. x_ref .Ge. -128) Then
                    byte1 = x_ref
                 Else If (x_ref .Gt. 127) Then
                    byte1 = 127
                 Else
                    byte1 = -128
                 End If
                 Call F2D_SUBOUTDCLUT (file_id, Max_buffer, byte1, pos_buffer, &
                   BUFFER, retstat, status)
                 If (y_ref .Le. 127 .And. y_ref .Ge. -128) Then
                    byte2 = y_ref
                 Else If (y_ref .Gt. 127) Then
                    byte2 = 127
                 Else
                    byte2 = -128
                 End If
                 Call F2D_SUBOUTDCLUT (file_id, Max_buffer, byte2, pos_buffer, &
                   BUFFER, retstat, status)
 
              Else
 
!              Empty buffer if necessary
                 If (pos_buffer .Eq. Max_buffer) Then
 
!                 Write buffer to file
                    Call FIO_FILEWRITEC (file_id, Max_buffer, pos_buffer, 0, &
                      BUFFER, retstat, written_bytes)
 
                    If (retstat .Ne. 0 .Or. written_bytes .Ne. pos_buffer) &
                      Then
 
!                    Problem writing data
                       Call IO_WRITE ('ERROR: Problem ' // &
                         'writing data to file', status)
 
!                    Close file
                       Call FIO_FILECLOSEC (file_id, retstat)
 
                       Return
 
                    End If
 
                    pos_buffer = 0
 
                 End If
 
!              Calculate distortion shifts relative to previous pixel
                 x_rel_shift = X_SD(x, y) - x_ref
                 y_rel_shift = Y_SD(x, y) - y_ref
                 x_ref = X_SD(x, y)
                 y_ref = Y_SD(x, y)
 
!              Check relative change is within allowed range
                 If (x_rel_shift .Ge. -7 .And. x_rel_shift .Le. 7 .And. &
                   y_rel_shift .Ge. -7 .And. y_rel_shift .Le. 7) Then
 
!                 Form combined value
                    pos_buffer = pos_buffer + 1
                    combined_shifts = (x_rel_shift + 7) * 16 + (y_rel_shift + &
                      7)
                    If (combined_shifts .Gt. 127) Then
                       combined_shifts = combined_shifts - 256
                    End If
                    BUFFER(pos_buffer) = combined_shifts
 
                 Else
 
!                 The relative change in distortion is too large
!                 for the packed storage scheme
                    Call IO_WRITE ('ERROR: The relative change in ' // &
                      'distortion is too large for the storage scheme', status)
                    Write (MESSAGE(1), '(''       X-direction ' // &
                      'shift relative to the previous pixel = '', i6)') &
                      x_rel_shift
                    Call IO_WRITE (MESSAGE(1), status)
                    Write (MESSAGE(1), '(''       Y-direction ' // &
                      'shift relative to the previous pixel = '', i6)') &
                      y_rel_shift
                    Call IO_WRITE (MESSAGE(1), status)
 
!                 Close file
                    Call FIO_FILECLOSEC (file_id, retstat)
 
                    Return
 
                 End If
 
              End If
 
           End Do
 
           x_ref = X_SD(1, y)
           y_ref = Y_SD(1, y)
 
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''End of distortion vectors = '', i5)')
!     :          pos_buffer
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Output rebinned output pixel fractions
        Do y = 1, ynum_lut
 
           Do x = 1, xnum_lut
 
!           Check that the sum of the fractions is 255
              sum = 0
              Do fraction = 1, 9
                 If (INT_REBINNED(fraction, x, y) .Ge. 0) Then
                    sum = sum + INT_REBINNED(fraction, x, y)
                 Else
                    sum = sum + INT_REBINNED(fraction, x, y) + 256
                 End If
 
              End Do
 
              If (sum .Ne. 255) Then
 
                 Write (message, '(''ERROR: The sum of the ' // &
                   'fractions is not 255 at pixel '', 2i6)') x, y
                 Call IO_WRITE (message, status)
                 Write (message, '(''       Sum of output ' // &
                   'fractions = '', i5)') sum
                 Call IO_WRITE (message, status)
 
!              Close output file
                 Call FIO_FILECLOSEC (file_id, retstat)
 
!************************* REMOVE WHILST TESTING   Return
 
              End If
 
!           The first fraction is always saved unless, only it
!           contains output intensity, in which case it is not
!           necessary. Note: it is possible for the first pixel
!           to have zero intensity, but this is rare, and in this
!           case 0 is stored. The 8 bits of 1 byte are set to show
!           which or the other eight values have intensity. The
!           last fraction with intensity is not stored, as its value
!           can be deduced from 255 - the sum of the other stored
!           values. The stored fractions are in the same order as
!           the set bits in the occupation byte.
              If (INT_REBINNED(2, x, y) .Ne. 0) Then
                 byte_value = 1
                 num_contributes = 2
              Else
                 byte_value = 0
                 num_contributes = 1
              End If
              If (INT_REBINNED(3, x, y) .Ne. 0) Then
                 byte_value = byte_value + 2
                 num_contributes = num_contributes + 1
              End If
              If (INT_REBINNED(4, x, y) .Ne. 0) Then
                 byte_value = byte_value + 4
                 num_contributes = num_contributes + 1
              End If
              If (INT_REBINNED(5, x, y) .Ne. 0) Then
                 byte_value = byte_value + 8
                 num_contributes = num_contributes + 1
              End If
              If (INT_REBINNED(6, x, y) .Ne. 0) Then
                 byte_value = byte_value + 16
                 num_contributes = num_contributes + 1
              End If
              If (INT_REBINNED(7, x, y) .Ne. 0) Then
                 byte_value = byte_value + 32
                 num_contributes = num_contributes + 1
              End If
              If (INT_REBINNED(8, x, y) .Ne. 0) Then
                 byte_value = byte_value + 64
                 num_contributes = num_contributes + 1
              End If
              If (INT_REBINNED(9, x, y) .Ne. 0) Then
!              byte_value = Ibset(byte_value, 7)
                 byte_value = byte_value - 256
                 num_contributes = num_contributes + 1
              End If
 
!           Store storage indicator byte value in buffer
              Call F2D_SUBOUTDCLUT (file_id, Max_buffer, byte_value, &
                pos_buffer, BUFFER, retstat, status)
 
!           Output first fraction (unless there's only one contributed output 
!           pixel)
              If (num_contributes .Gt. 1) Then
                 Call F2D_SUBOUTDCLUT (file_id, Max_buffer, &
                   INT_REBINNED(1, x, y), pos_buffer, BUFFER, retstat, status)
              End If
 
              Do fraction = 2, 9
 
                 If (INT_REBINNED(fraction, x, y) .Ne. 0) Then
 
                    num_contributes = num_contributes - 1
 
!                 Don't bother outputting the last fraction
                    If (num_contributes .Gt. 1) Then
 
!                    Output to file
                       Call F2D_SUBOUTDCLUT (file_id, Max_buffer, &
                         INT_REBINNED(fraction, x, y), pos_buffer, BUFFER, &
                         retstat, status)
 
                    End If
 
                 End If
 
              End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           If (x .Eq. xtest .And. y .Eq. ytest) Then
 
!           Write (*, '('' X_SD, Y_SD = '', 2i5)')
!           :                X_SD(x, y), Y_SD(x, y)
!           Write (*, '(''x, Storage byte = '', 2i4)')
!           :                x, byte_value
!           Write (*, '(''pos_buffer = '', i4)') pos_buffer
!           sum = 0
!           Do fraction = 1, 9
!           Write (*, '(''f, INT_REBINNED(f, x, y) = '',
!           :                   2i5)') fraction, INT_REBINNED(fraction, x, y)
!           If (INT_REBINNED(fraction, x, y) .Ge. 0) Then
!           sum = sum + INT_REBINNED(fraction, x, y)
!           Else
!           sum = sum + INT_REBINNED(fraction, x, y) + 256
!           End If
 
!           End Do
 
!           Write (*, '(''sum = '', i6)') sum
 
!           End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           End Do
 
        End Do
 
!     Flush output buffer if necessary
        If (pos_buffer .Gt. 0) Then
 
!        Write buffer to file
           Call FIO_FILEWRITEC (file_id, Max_buffer, pos_buffer, 0, BUFFER, &
             retstat, written_bytes)
 
           If (retstat .Ne. 0 .Or. written_bytes .Ne. pos_buffer) Then
 
!           Problem writing data
              Call IO_WRITE ('ERROR: Problem writing data to file', status)
 
!           Close file
              Call FIO_FILECLOSEC (file_id, retstat)
 
              Return
 
           End If
 
        End If
 
!     Close file
        Call FIO_FILECLOSEC (file_id, retstat)
 
        If (retstat .Ne. 0) Then
           Write (MESSAGE(1), '(''WARNING: Problem closing file: ' // &
             'Return status = '', i6)') retstat
           Call IO_WRITE (MESSAGE(1), status)
        Else
 
!        Save name of distortion correction file in data base
           len_string = Len_trim(file_name)
           Call IO_SET_KEYVALUE ('DCLUT_FILE_NAME', len_string, file_name, &
             retstat, status)
 
        End If
 
     End If
 
     End Subroutine F2D_OUT_DCLUT
!********1*********2*********3*********4*********5*********6*********7*********8
!********1*********2*********3*********4*********5*********6*********7*********8
!+ F2D_SUBOUTDCLUT - FIT 2-D (sub-) OUTput Distortion Correction
!  Look-up Table. Output 1 byte to buffer, emptying if necessary
     Subroutine F2D_SUBOUTDCLUT (file_id, max_buffer, byte_value, pos_buffer, &
       BUFFER, retstat, status)

!  Description:
!    Empties the output buffer to file if necessary. And adds 1 byte to it.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    01-Mar-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
!    Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: file_id ! "C" file identifier for output
     Integer, Intent(IN) :: max_buffer ! Size of byte buffer
     Byte, Intent(IN) :: byte_value ! Value to save to the buffer
!  Import/Export:
     Integer, Intent(INOUT) :: pos_buffer ! Position of filled buffer
     Byte, Intent(INOUT) :: BUFFER(max_buffer) ! Output byte buffer
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status:
!      0 = Good status
!      1 = Bad status
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: written_bytes ! Number of bytes successfully output to file
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Empty buffer if necessary
     If (pos_buffer .Eq. Max_buffer) Then
 
!     Write buffer to file
        Call FIO_FILEWRITEC (file_id, Max_buffer, pos_buffer, 0, BUFFER, &
          retstat, written_bytes)
 
        If (retstat .Ne. 0 .Or. written_bytes .Ne. pos_buffer) Then
 
!        Problem writing data
           Call IO_WRITE ('ERROR: Problem writing data to file', status)
 
!        Close file
           Call FIO_FILECLOSEC (file_id, retstat)
 
           Return
 
        End If
 
        pos_buffer = 0
 
     End If
 
!  Add byte to the buffer
     pos_buffer = pos_buffer + 1
     BUFFER(pos_buffer) = byte_value
 
     End Subroutine F2D_SUBOUTDCLUT
!********1*********2*********3*********4*********5*********6*********7*********8
