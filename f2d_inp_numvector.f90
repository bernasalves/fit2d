!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_inp_numvector.f90 *
!  *                       *
!  *************************
 
!+ F2D_INP_NUMVECTOR -  INPut NUMber of VECTOR to use as current vector
     Subroutine F2D_INP_NUMVECTOR (results, vec_num, status)
!  Description:
!    User input of number of vector to use as current vector, with display of 
!    the titles of all defined vectors.
!  Keywords:
!    Number.Vector, Vector.Number
!  Method:
!    Uses "GS_INPI"
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    09-Dec-2014: V0.36 Use result vectors data structure (Hammersley)
!    08-Dec-2014: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use GS_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Type(RESULT_VECTORS), Intent(IN) :: results ! Result vectors
!     Integer, Intent(IN) :: max_vectors ! Second dimension of "VECTORS"
!      array (maximum number of vectors which can be stored)
!     Integer, Intent(IN) :: num_vectors ! Number of values defined in the
!      "time"-series for each vector
!     Character(Len = *), Intent(IN) :: VECTOR_TITLES(max_vectors)
!      Titles for the 1-D data-sets
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: vec_num ! Number of vector to use
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: vector ! Loop variable for vectors
     Real :: xmin ! The minimum X-coordinate for the text
     Real :: ymin ! The minimum Y-coordiante for the text
     Real :: xmax ! The maximum X-coordinate for the text
     Real :: ymax ! The maximum Y-coordinate for the text
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(101) ! User text
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_NUMVECTOR ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Position to draw text
        Call GS_INQ_GPP (xmin, ymin, xmax, ymax, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''xmin, ymin, ymax, ymax = '', 4f12.4)') &
!          xmin, ymin, ymax, ymax
!        Write (*, '(''num_vectors = '', i6)') num_vectors
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

        vec_num = vec_num + 1
        If (vec_num .Gt. results%num_vectors) Then
           vec_num = 1
        End If

!     Turn-off clipping
        Call LG_CLIP (.False., status)

!     Set unit transform
        Call LG_DATAWINDOW (0.0, 0.0, 1.0, 1.0, status)
        Call LG_VIEWPORT (0.0, 0.0, 1.0, 1.0, status)
 
!     Output titles of defined vectors
        Call GS_BACKGROUND (status)

        MESSAGE(1) = 'Vector No.:  Title'
        Do vector = 1, Min(100, results%num_vectors)
           Write (MESSAGE(vector + 1), '(i6, '':         '', a20)') &
             vector, results%TITLES(vector)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(a)') MESSAGE(vector + 1)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

        End Do

        Call GS_TEXTSTYLE (Gs_font_publication, Gs_black, &
          (ymax - ymin) / Real (results%num_vectors + 3), 1.0, 0.0, &
          Lg_right, Lg_left, Lg_base, 0.0, 1.0, status)

        Call GS_TEXT (101, Min(101, results%num_vectors + 1), MESSAGE, &
          xmin, ymin, xmax, ymax, status)

        Call GS_INPI (.True., 1, results%num_vectors, .True., &
          'NUMBER OF CURRENT VECTOR', 1, &
          'Enter number of vector to view, output, etc.', 1, &
          'Enter integer within given range', vec_num, status)

     End If
 
!   Check that the vector is properly defined
     If (results%STARTS(vec_num) .Lt. 1 .Or. &
       results%STARTS(vec_num) .Gt. results%ENDS(vec_num) .Or. &
       results%ENDS(vec_num) .Gt. results%max_values) Then
        Call GS_FWARNING (1, 1, 'The vector is improperly defined', status)
     End If

     End Subroutine F2D_INP_NUMVECTOR
!********1*********2*********3*********4*********5*********6*********7*********8
