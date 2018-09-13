!********1*********2*********3*********4*********5*********6*********7**
 
!  ********************
!  *                  *
!  * f2d_in2dclut.f90 *
!  *                  *
!  ********************
 
!+ F2D_IN2DCLUT - FIT 2-D Input Distortion Correction Look-up Table
     Subroutine F2D_IN2DCLUT (input_file, retstat, x_cor_size, y_cor_size, &
       xmax_lut, ymax_lut, xnum_lut, ynum_lut, X_SD, Y_SD, INT_REBINNED, &
       xmaxdat, ymaxdat, INVERSE_FF, dc_lut_defined, flat_field_defined, &
       status)
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
!    17-Sep-1997: V0.1 Original, transferred from "F2D_INDCLUT" to completely 
!      separate input code from auxilliary code (Hammersley)
!    12-Oct-1999: V0.2 Use FIO file I/O input instead of I/O routines
!      (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Character(Len = *), Intent(IN) :: input_file ! Input file name
     Integer, Intent(IN) :: xmax_lut ! First dimension of look-up tables
     Integer, Intent(IN) :: ymax_lut ! Second dimension of look-up tables
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status:
!    0 = Good status
!    1 = Bad status
     Real, Intent(OUT) :: x_cor_size ! Size of corrected pixel in metres in
!    X-direction
     Real, Intent(OUT) :: y_cor_size ! Size of corrected pixel in metres in
!    Y-direction
     Integer, Intent(OUT) :: xnum_lut ! Number of pixels to define in LUT in
!    the X-direction
     Integer, Intent(OUT) :: ynum_lut ! Number of pixels to define in LUT in
!    the Y-direction
     Byte, Intent(OUT) :: X_SD(xmax_lut, ymax_lut) ! Rounded X-distortion in
!      pixels
     Byte, Intent(OUT) :: Y_SD(xmax_lut, ymax_lut) ! Rounded Y-distortion in
!      pixels
     Byte, Intent(OUT) :: INT_REBINNED(9, xmax_lut, ymax_lut)
!      "Fraction" of each input pixel rebinned into the 9 pixels which are the 
!      targetpixel and the 8 nearest pixels. Each fraction is stored as a
!      byte, so change to unsigned, and divide by 256, to obtain the
!      required fraction. The order of the fractions is
!      lower left, lower middle, lower right, left middle, middle,
!      right of middle, upper left, upper middle, upper right.
     Real, Intent(OUT) :: INVERSE_FF(xmaxdat, ymaxdat) ! Flat-field image,
!      as stored in the file
     Logical, Intent(OUT) :: dc_lut_defined ! .True., if the LUT's have been
!      defined
     Logical, Intent(OUT) :: flat_field_defined ! .True., if the flat-field
!      response, has been stored in the distortion file
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
     Integer, Parameter :: Max_buffer = 1024 ! Size of byte buffer
!  Local Variables:
     Byte :: byte1 ! First byte to store of a two byte integer
     Byte :: byte2 ! Second byte to store of a two byte integer
     Byte :: byte_value ! Value used to store output fractions
     Byte :: storage_byte ! Value used to store which of the possible output 
!      fractions are stored
     Character(Len = 50) :: text ! Stores character text prior to output
     Integer :: bytes_per_pixel ! Number of bytes used in the LUT to store the 
!      fraction of an input pixel which ends up in any one of 9 output pixels
     Integer :: chr ! Loop variable for characters
     Integer :: file_id ! "C" file identifier for input
     Integer :: filstat ! File I/O status return variable
!    Integer fraction ! Loop variable
!    Integer i ! Debugging loop variable
     Integer :: int1 ! Temporary integer storage value
     Integer :: int2 ! Temporary integer storage value
     Integer :: int_value ! byte value stored as an integer
     Integer :: num_contributes ! Number of output pixels which an
!      input pixel provides a non-zero contribution
     Integer :: pos_buffer ! Position of filled buffer
     Integer :: returned_bytes ! Number of bytes successfully input
!      from the file
     Integer :: sum ! Sum of saved contributions to output pixels
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
     Logical :: convert ! .True., if a token is converted to a value
     Logical :: eigth_set ! .True., if the eigth fraction is non-zero
     Logical :: fifth_set ! .True., if the fifth fraction is non-zero
     Logical :: fourth_set ! .True., if the fourth fraction is non-zero
     Logical :: ninth_set ! .True., if the ninth fraction is non-zero
     Logical :: second_set ! .True., if the second fraction is non-zero
     Logical :: seventh_set ! .True., if the seventh fraction is non-zero
     Logical :: third_set ! .True., if the third fraction is non-zero
     Logical :: sixth_set ! .True., if the sixth fraction is non-zero
     Real :: minimum_value ! Minimum value in flat-field response image
     Real :: scale ! Scale factor from floating point flat-field
!      response image to stored 2-byte unsigned integers
!  Local Arrays:
     Byte :: BUFFER(Max_buffer) ! Output byte buffer
     Character(Len = 80) :: MESSAGE(14) ! User messages
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(''Entered Subroutine F2D_IN2DCLUT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_IN2DCLUT ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably defined
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
        Call ST_SAVE ('Subroutine F2D_IN2DCLUT ' // Version)
 
     Else
 
!     Open file for input
        Call FIO_FILEOPEN (input_file, 'READ', filstat, file_id, status)
 
        If (filstat .Ne. 0) Then
           Call IO_WRITE ('WARNING: Error in opening file: ', status)
           Call IO_WRITE (input_file, status)
           retstat = 1
           Return
        End If
 
!     Input look-up table
 
!     Input first buffer
        Call FIO_FILEREADC (file_id, Max_buffer, Max_buffer, 0, retstat, &
          returned_bytes, BUFFER)
 
        If (returned_bytes .Lt. 100) Then
           Call IO_WRITE ('ERROR: Problem in inputting ' // &
             'header; wrong format ?', status)
 
!        Close file
           Call FIO_FILECLOSEC (file_id, retstat)
 
           retstat = 1
           Return
        End If
 
!     Input file type identifier
        Do chr = 1, 42
           text(chr: chr) = Char(BUFFER(chr))
        End Do
 
!     Check identifier
        If (text(1: 42) .Ne. 'DISTORTION CORRECTION LOOK-UP TABLE V02.00') &
          Then
 
           Call IO_WRITE ('ERROR: This file is not ' // &
             'a valid distortion correction LUT file', status)
 
!        Close file
           Call FIO_FILECLOSEC (file_id, retstat)
 
           retstat = 1
           Return
        End If
        pos_buffer = 44
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input X-direction pixel size in metres
        Do chr = 1, 29
           text(chr: chr) = Char(BUFFER(chr + pos_buffer))
        End Do
        pos_buffer = pos_buffer + 31
 
!     Check keyword
        If (text(1: 14) .Ne. 'X_PIXEL_SIZE =') Then
 
           Call IO_WRITE ('ERROR: "X_PIXEL_SIZE" Corrupted file', status)
 
!        Close file
           Call FIO_FILECLOSEC (file_id, retstat)
 
           retstat = 1
           Return
        End If
 
!     Convert value
        Call IO_TOKTR (text(16: 29), convert, x_cor_size, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input Y-direction pixel size in metres
        Do chr = 1, 29
           text(chr: chr) = Char(BUFFER(chr + pos_buffer))
        End Do
        pos_buffer = pos_buffer + 31
 
!     Check keyword
        If (text(1: 14) .Ne. 'Y_PIXEL_SIZE =') Then
 
           Call IO_WRITE ('ERROR: "Y_PIXEL_SIZE" Corrupted file', status)
 
!        Close file
           Call FIO_FILECLOSEC (file_id, retstat)
 
           retstat = 1
           Return
        End If
 
!     Convert value
        Call IO_TOKTR (text(16: 29), convert, y_cor_size, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input number of X-pixels
        Do chr = 1, 23
           text(chr: chr) = Char(BUFFER(chr + pos_buffer))
        End Do
        pos_buffer = pos_buffer + 25
 
!     Check keyword
        If (text(1: 10) .Ne. 'X_NUMBER =') Then
 
           Call IO_WRITE ('ERROR: "X_NUMBER" Corrupted file', status)
 
!        Close file
           Call FIO_FILECLOSEC (file_id, retstat)
 
           retstat = 1
           Return
        End If
 
!     Convert value
        Call IO_TOKTI (text(12: 23), convert, xnum_lut, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input number of Y-pixels
        Do chr = 1, 23
           text(chr: chr) = Char(BUFFER(chr + pos_buffer))
        End Do
        pos_buffer = pos_buffer + 25
 
!     Check keyword
        If (text(1: 10) .Ne. 'Y_NUMBER =') Then
 
           Call IO_WRITE ('ERROR: "Y_NUMBER" Corrupted file', status)
 
!        Close file
           Call FIO_FILECLOSEC (file_id, retstat)
 
           retstat = 1
           Return
        End If
 
!     Convert value
        Call IO_TOKTI (text(12: 23), convert, ynum_lut, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input number bytes used to store each output pixel fraction
        Do chr = 1, 34
           text(chr: chr) = Char(BUFFER(chr + pos_buffer))
        End Do
        pos_buffer = pos_buffer + 36
 
!     Check keyword
        If (text(1: 21) .Ne. 'BYTES_PER_PIXEL_LUT =') Then
 
           Call IO_WRITE ('ERROR: "BYTES_PER_PIXEL_LUT =" Corrupted file', &
             status)
 
!        Close file
           Call FIO_FILECLOSEC (file_id, retstat)
 
           retstat = 1
           Return
        End If
 
!     Convert value
        Call IO_TOKTI (text(23: 34), convert, bytes_per_pixel, status)
 
        If (bytes_per_pixel .Ne. 1) Then
 
           Call IO_WRITE ('WARNING: Only 1 byte per pixel ' // &
             'LUT is available', status)
 
!        Close file
           Call FIO_FILECLOSEC (file_id, retstat)
 
           retstat = 1
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input if a flat-field response image is stored
        Do chr = 1, 21
           text(chr: chr) = Char(BUFFER(chr + pos_buffer))
        End Do
        pos_buffer = pos_buffer + 23
 
!     Check keyword
        If (text(1:19) .Ne. 'FLAT_FIELD_STORED =') Then
 
           Call IO_WRITE ('ERROR: "FLAT_FIELD_STORED" Corrupted file', status)
 
!        Close file
           Call FIO_FILECLOSEC (file_id, retstat)
 
           retstat = 1
           Return
        End If
 
!     Convert value
        flat_field_defined = text(21: 21) .Eq. 't' .Or. text(21: 21) .Eq. 'T'
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
        If (flat_field_defined) Then
 
!        Flat-field image offset value
           Do chr = 1, 26
              text(chr: chr) = Char(BUFFER(chr + pos_buffer))
           End Do
           pos_buffer = pos_buffer + 28
 
!        Check keyword
           If (text(1: 11) .Ne. 'FF_OFFSET =') Then
 
              Call IO_WRITE ('ERROR: "FF_OFFSET" Corrupted file', status)
 
!           Close file
              Call FIO_FILECLOSEC (file_id, retstat)
 
              retstat = 1
              Return
           End If
 
!        Convert value
           Call IO_TOKTR (text(13: 26), convert, minimum_value, status)
 
!        Flat-field scaling value
           Do chr = 1, 27
              text(chr: chr) = Char(BUFFER(chr + pos_buffer))
           End Do
           pos_buffer = pos_buffer + 29
 
!        Check keyword
           If (text(1: 12) .Ne. 'FF_SCALING =') Then
 
              Call IO_WRITE ('ERROR: "FF_SCALING" Corrupted file', status)
 
!           Close file
              Call FIO_FILECLOSEC (file_id, retstat)
 
              retstat = 1
              Return
           End If
 
!        Convert value
           Call IO_TOKTR (text(14: 27), convert, scale, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''offset, scale = '', 2(1pe14.7))')
!        :          minimum_value, scale
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input header terminator
        Do chr = 1, 29
           text(chr: chr) = Char(BUFFER(chr + pos_buffer))
        End Do
        pos_buffer = pos_buffer + 33
 
!     Check format
        If (text(1: 29) .Ne. '!!! END OF HEADER SECTION !!!') Then
 
           Call IO_WRITE ('ERROR: Header terminator corrupted ', status)
 
           Write (*, '(a)') text(1: 29)
 
!        Close file
           Call FIO_FILECLOSEC (file_id, retstat)
 
           retstat = 1
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Check that the internal array is large enough to hold the
!     look-up tables
        If (xnum_lut .Gt. xmax_lut .Or. ynum_lut .Gt. ymax_lut) Then
 
           Call IO_WRITE ('ERROR: The look-up table is too ' // &
             'big for the internal arrays', status)
 
!        Close file
           Call FIO_FILECLOSEC (file_id, retstat)
 
           retstat = 1
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Set LUT's not to be defined
        dc_lut_defined = .False.
 
        If (flat_field_defined) Then
 
!        Skip byte if the current byte input position is odd
           If (Mod(pos_buffer, 2) .Ne. 0) Then
              pos_buffer = pos_buffer + 1
           End If
 
           Do y = 1, ynum_lut
 
              Do x = 1, xnum_lut
 
!              Input bytes from buffer, filling from file if necessary
                 Call F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, &
                   pos_buffer, retstat, byte1, status)
                 If (byte1 .Ge. 0) Then
                    int1 = byte1
                 Else
                    int1 = byte1 + 256
                 End If
                 Call F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, &
                   pos_buffer, retstat, byte2, status)
                 If (byte2 .Ge. 0) Then
                    int2 = byte2
                 Else
                    int2 = byte2 + 256
                 End If
 
                 int_value = int1 * 256 + int2
 
                 INVERSE_FF(x, y) = Real(int_value) / scale + minimum_value
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              If (x .Eq. 150 .And. y .Eq. 1) Then
 
!              Write (*, '(''INVERSE_FF, int, byte1, byte2 = '', ' //
!              :                   '1pe14.7, 3i6)')
!              :                   INVERSE_FF(150, 1), int_value, byte1, byte2
!              End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              End Do
 
           End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''End of flat field: pos_buffer = '', i5)')
!        :          pos_buffer
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input X and Y relative distortion values
        Do y = 1, ynum_lut
 
           Do x = 1, xnum_lut
 
              If (x .Eq. 1 .And. y .Eq. 1) Then
 
!              Input initial X and Y distortion values
                 Call F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, &
                   pos_buffer, retstat, byte1, status)
                 x_ref = byte1
                 Call F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, &
                   pos_buffer, retstat, byte2, status)
                 y_ref = byte2
                 X_SD(1, 1) = x_ref
                 Y_SD(1, 1) = y_ref
 
              Else
 
!              Input byte from buffer, filling from file if necessary
                 Call F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, &
                   pos_buffer, retstat, byte_value, status)
 
                 If (byte_value .Ge. 0) Then
                    int_value = byte_value
                    x_rel_shift = int_value / 16 - 7
                    y_rel_shift = Mod(int_value, 16) - 7
                 Else
                    int_value = byte_value + 256
                    x_rel_shift = int_value / 16 - 7
                    y_rel_shift = Mod(int_value, 16) - 7
                 End If
 
!              Calculate distortion shifts relative to previous pixel
                 x_ref = x_rel_shift + x_ref
                 y_ref = y_rel_shift + y_ref
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              If (x_ref .Ne. X_SD(x, y)) Then
!              Write (*, '('' x, y = '', 2i6)') x, y
!              Write (*, '(''X_SD(x, y) = '', i4, '' x_ref = '',
!              :                   i8)') X_SD(x, y), x_ref
!              Write (*, '('' x_rel_shift, y_rel_shift = '',
!              :                   2i14)') x_rel_shift, y_rel_shift
!              Write (*, '(''byte_value = '', i6)') byte_value
!              Write (*, '('' x, y = '', 2i6)') x, y
!              Return
!              End If
!              If (y_ref .Ne. Y_SD(x, y)) Then
!              Write (*, '('' x, y = '', 2i6)') x, y
!              Write (*, '(''Y_SD(1, 1) = '', i4, '' y_ref = '',
!              :                   i8)') Y_SD(x, y), y_ref
!              Return
!              End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 X_SD(x, y) = x_ref
                 Y_SD(x, y) = y_ref
 
              End If
 
           End Do
 
           x_ref = X_SD(1, y)
           y_ref = Y_SD(1, y)
 
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''End of distortion vectors = '', i5)')
!     :          pos_buffer
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input rebinned output pixel fractions
        Do y = 1, ynum_lut
 
           Do x = 1, xnum_lut
 
!           The first fraction is always stored unless only one
!           pixel contains all the outpt intensity, the other
!           eight are only stored if they are non-zero. The
!           bits of 1 byte are set to show which or the other
!           eight values are stored, these then follow apart
!           from the last fraction which is not stored. This
!           can be deduce by 255 - sum of the other fractions
 
!           Input storage byte
              Call F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, pos_buffer, &
                retstat, storage_byte, status)
 
!           Decode byte_value
              If (storage_byte .Lt. 0) Then
                 ninth_set = .True.
                 int_value = storage_byte
                 num_contributes = 2
              Else
                 ninth_set = .False.
                 int_value = storage_byte
                 num_contributes = 1
              End If
 
              eigth_set = Btest(int_value, 6)
              If (eigth_set) Then
                 num_contributes = num_contributes + 1
              End If
              seventh_set = Btest(int_value, 5)
              If (seventh_set) Then
                 num_contributes = num_contributes + 1
              End If
              sixth_set = Btest(int_value, 4)
              If (sixth_set) Then
                 num_contributes = num_contributes + 1
              End If
              fifth_set = Btest(int_value, 3)
              If (fifth_set) Then
                 num_contributes = num_contributes + 1
              End If
              fourth_set = Btest(int_value, 2)
              If (fourth_set) Then
                 num_contributes = num_contributes + 1
              End If
              third_set = Btest(int_value, 1)
              If (third_set) Then
                 num_contributes = num_contributes + 1
              End If
              second_set = Btest(int_value, 0)
              If (second_set) Then
                 num_contributes = num_contributes + 1
              End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''second_set to ninth set = '', 8l2)')
!           :             second_set, third_set, fourth_set, fifth_set,
!           :             sixth_set, seventh_set, eigth_set, ninth_set
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              sum = 0
              If (num_contributes .Gt. 1) Then
 
!              Input first fraction
                 Call F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, &
                   pos_buffer, retstat, byte_value, status)
                 INT_REBINNED(1, x, y) = byte_value
                 num_contributes = num_contributes - 1
                 If (byte_value .Ge. 0) Then
                    sum = byte_value
                 Else
                    sum = 256 + byte_value
                 End If
 
              Else
 
!              All output is in the first position
                 INT_REBINNED(1, x, y) = -1 ! (255)
              End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           If (byte_value .Ne. INT_REBINNED(1, x, y)) Then
!           Write (*, '(''x, y = '', 2i6)') x, y
!           Write (*,
!           :                '(''INT_REBINNED(1, x, y), byte_value = '', 2i9)')
!           :                INT_REBINNED(1, x, y), byte_value
!           Return
!           End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              If (second_set) Then
 
                 If (num_contributes .Gt. 1) Then
 
!                 Input value from file
                    Call F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, &
                      pos_buffer, retstat, byte_value, status)
 
                    num_contributes = num_contributes - 1
                    INT_REBINNED(2, x, y) = byte_value
                    If (byte_value .Ge. 0) Then
                       sum = sum + byte_value
                    Else
                       sum = sum + 256 + byte_value
                    End If
 
                 Else
 
                    int1 = 255 - sum
                    If (int1 .Gt. 127) Then
                       int1 = int1 - 256
                    End If
                    INT_REBINNED(2, x, y) = int1
 
                 End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              If (byte_value .Ne. INT_REBINNED(2, x, y)) Then
!              Write (*, '(''x, y = '', 2i6)') x, y
!              Write (*,
!              :                   '(''INT_REBINNED(2, x, y), byte_value = '',
!              :                   2i9)') INT_REBINNED(2, x, y), byte_value
!              Return
!              End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              Else
                 INT_REBINNED(2, x, y) = 0
              End If
 
              If (third_set) Then
 
                 If (num_contributes .Gt. 1) Then
 
!                 Input value from file
                    Call F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, &
                      pos_buffer, retstat, byte_value, status)
 
                    num_contributes = num_contributes - 1
                    INT_REBINNED(3, x, y) = byte_value
                    If (byte_value .Ge. 0) Then
                       sum = sum + byte_value
                    Else
                       sum = sum + 256 + byte_value
                    End If
 
                 Else
 
                    int1 = 255 - sum
                    If (int1 .Gt. 127) Then
                       int1 = int1 - 256
                    End If
                    INT_REBINNED(3, x, y) = int1
 
                 End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              If (byte_value .Ne. INT_REBINNED(3, x, y)) Then
!              Write (*, '(''x, y = '', 2i6)') x, y
!              Write (*,
!              :                   '(''INT_REBINNED(3, x, y), byte_value = '',
!              :                   2i9)') INT_REBINNED(3, x, y), byte_value
!              Return
!              End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              Else
                 INT_REBINNED(3, x, y) = 0
              End If
 
              If (fourth_set) Then
 
                 If (num_contributes .Gt. 1) Then
 
!                 Input value from file
                    Call F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, &
                      pos_buffer, retstat, byte_value, status)
 
                    num_contributes = num_contributes - 1
                    INT_REBINNED(4, x, y) = byte_value
                    If (byte_value .Ge. 0) Then
                       sum = sum + byte_value
                    Else
                       sum = sum + 256 + byte_value
                    End If
 
                 Else
 
                    int1 = 255 - sum
                    If (int1 .Gt. 127) Then
                       int1 = int1 - 256
                    End If
                    INT_REBINNED(4, x, y) = int1
 
                 End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              If (byte_value .Ne. INT_REBINNED(4, x, y)) Then
!              Write (*, '(''x, y = '', 2i6)') x, y
!              Write (*,
!              :                   '(''INT_REBINNED(4, x, y), byte_value = '',
!              :                   2i9)') INT_REBINNED(4, x, y), byte_value
!              Return
!              End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              Else
                 INT_REBINNED(4, x, y) = 0
              End If
 
              If (fifth_set) Then
 
                 If (num_contributes .Gt. 1) Then
 
!                 Input value from file
                    Call F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, &
                      pos_buffer, retstat, byte_value, status)
 
                    num_contributes = num_contributes - 1
                    INT_REBINNED(5, x, y) = byte_value
                    If (byte_value .Ge. 0) Then
                       sum = sum + byte_value
                    Else
                       sum = sum + 256 + byte_value
                    End If
 
                 Else
 
                    int1 = 255 - sum
                    If (int1 .Gt. 127) Then
                       int1 = int1 - 256
                    End If
                    INT_REBINNED(5, x, y) = int1
 
                 End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              If (byte_value .Ne. INT_REBINNED(5, x, y)) Then
!              Write (*, '(''x, y = '', 2i6)') x, y
!              Write (*,
!              :                   '(''INT_REBINNED(5, x, y), byte_value = '',
!              :                   2i9)') INT_REBINNED(5, x, y), byte_value
!              Return
!              End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              Else
                 INT_REBINNED(5, x, y) = 0
              End If
 
              If (sixth_set) Then
 
                 If (num_contributes .Gt. 1) Then
 
!                 Input value from file
                    Call F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, &
                      pos_buffer, retstat, byte_value, status)
 
                    num_contributes = num_contributes - 1
                    INT_REBINNED(6, x, y) = byte_value
                    If (byte_value .Ge. 0) Then
                       sum = sum + byte_value
                    Else
                       sum = sum + 256 + byte_value
                    End If
 
                 Else
 
                    int1 = 255 - sum
                    If (int1 .Gt. 127) Then
                       int1 = int1 - 256
                    End If
                    INT_REBINNED(6, x, y) = int1
 
                 End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              If (byte_value .Ne. INT_REBINNED(6, x, y)) Then
!              Write (*, '(''x, y = '', 2i6)') x, y
!              Write (*,
!              :                   '(''INT_REBINNED(6, x, y), byte_value = '',
!              :                   2i9)') INT_REBINNED(6, x, y), byte_value
!              Return
!              End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              Else
                 INT_REBINNED(6, x, y) = 0
              End If
 
              If (seventh_set) Then
 
                 If (num_contributes .Gt. 1) Then
 
!                 Input value from file
                    Call F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, &
                      pos_buffer, retstat, byte_value, status)
 
                    num_contributes = num_contributes - 1
                    INT_REBINNED(7, x, y) = byte_value
                    If (byte_value .Ge. 0) Then
                       sum = sum + byte_value
                    Else
                       sum = sum + 256 + byte_value
                    End If
 
                 Else
 
                    int1 = 255 - sum
                    If (int1 .Gt. 127) Then
                       int1 = int1 - 256
                    End If
                    INT_REBINNED(7, x, y) = int1
 
                 End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              If (byte_value .Ne. INT_REBINNED(7, x, y)) Then
!              Write (*, '(''x, y = '', 2i6)') x, y
!              Write (*,
!              :                   '(''INT_REBINNED(7, x, y), byte_value = '',
!              :                   2i9)') INT_REBINNED(7, x, y), byte_value
!              Return
!              End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              Else
                 INT_REBINNED(7, x, y) = 0
              End If
 
              If (eigth_set) Then
 
                 If (num_contributes .Gt. 1) Then
 
!                 Input value from file
                    Call F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, &
                      pos_buffer, retstat, byte_value, status)
 
                    num_contributes = num_contributes - 1
                    INT_REBINNED(8, x, y) = byte_value
                    If (byte_value .Ge. 0) Then
                       sum = sum + byte_value
                    Else
                       sum = sum + 256 + byte_value
                    End If
                 Else
 
                    int1 = 255 - sum
                    If (int1 .Gt. 127) Then
                       int1 = int1 - 256
                    End If
                    INT_REBINNED(8, x, y) = int1
 
                 End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              If (byte_value .Ne. INT_REBINNED(8, x, y)) Then
!              Write (*, '(''x, y = '', 2i6)') x, y
!              Write (*,
!              :                   '(''INT_REBINNED(8, x, y), byte_value = '',
!              :                   2i9)') INT_REBINNED(8, x, y), byte_value
!              Return
!              End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              Else
                 INT_REBINNED(8, x, y) = 0
              End If
 
              If (ninth_set) Then
 
!              Value is never stored, it is always deduced from
!              the sum of the other values
                 int1 = 255 - sum
                 If (int1 .Gt. 127) Then
                    int1 = int1 - 256
                 End If
                 INT_REBINNED(9, x, y) = int1
 
              Else
                 INT_REBINNED(9, x, y) = 0
              End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           If (x .Eq. xtest .And. y .Eq. ytest) Then
 
!           Write (*, '('' X_SD, Y_SD = '', 2i5)')
!           :                X_SD(x, y), Y_SD(x, y)
!           Write (*, '(''x, Storage byte, sum = '', 3i5)')
!           :                x, storage_byte, sum
!           Write (*, '(''pos_buffer = '', i4)') pos_buffer
!           Do fraction = 1, 9
!           Write (*, '(''f, INT_REBINNED(f, x, y) = '',
!           :                   2i5)') fraction, INT_REBINNED(fraction, x, y)
!           End Do
 
!           End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           End Do
 
        End Do
 
!     Close file
        Call FIO_FILECLOSEC (file_id, retstat)
 
        If (retstat .Ne. 0) Then
           Write (MESSAGE(1), '(''WARNING: Problem closing file: ' // &
             'Return status = '', i6)') retstat
           Call IO_WRITE (MESSAGE(1), status)
        Else
           dc_lut_defined = .True.
        End If
 
 
     End If

     End Subroutine F2D_IN2DCLUT
!********1*********2*********3*********4*********5*********6*********7**
!********1*********2*********3*********4*********5*********6*********7**
 
!+ F2D_SUBIN2DCLUT - FIT 2-D (sub-) INput Distortion Correction Look-up 
!    Table. Input 1 byte from buffer, inputting from file if necessary
     Subroutine F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, pos_buffer, &
       retstat, byte_value, status)
!  Description:
!    Reads a new buffer from file if necessary. Returns a byte from
!    the input buffer.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    03-Mar-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
!    Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: file_id ! "C" file identifier for output
     Integer, Intent(IN) :: max_buffer ! Size of byte buffer
!  Import/Export:
     Byte, Intent(INOUT) :: BUFFER(max_buffer) ! Output byte buffer
     Integer, Intent(INOUT) :: pos_buffer ! Position of read buffer
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status:
!    0 = Good status
!    1 = Bad status
     Byte, Intent(OUT) :: byte_value ! Value from the buffer
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number for
!      subroutine
!  Local Variables:
     Integer :: returned_bytes ! Number of bytes successfully input to
!    buffer
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Fill input buffer from file if necessary
     If (pos_buffer .Eq. max_buffer) Then
 
!     Fill buffer from file
        Call FIO_FILEREADC (file_id, max_buffer, max_buffer, 0, retstat, &
          returned_bytes, BUFFER)
        pos_buffer = 1
 
     Else
        pos_buffer = pos_buffer + 1
     End If
 
!  Return current byte from the buffer
     byte_value = BUFFER(pos_buffer)
 
     End Subroutine F2D_SUBIN2DCLUT
!********1*********2*********3*********4*********5*********6*********7**
 
