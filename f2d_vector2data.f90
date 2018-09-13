!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_vector2data.f90 *
!  *                     *
!  ***********************
 
!+ F2DVECTOR2DATA - FIT 2-D VECTOR 2 DATA array
     Subroutine F2D_VECTOR2DATA (results, &
!max_vec_values, max_vectors, num_vectors, &
!       STR_VECTORS, END_VECTORS, VECTORS, VECTOR_TITLES, 
       AXIS, vec_num, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
       DATA, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, status)
!  Description:
!    Transfers current vector to main data array.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    09-Dec-2014: V0.2 Use result vectors data structure (Hammersley)
!    08-Dec-2014: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Type(RESULT_VECTORS), Intent(IN) :: results ! Result vectors
!     Integer, Intent(IN) :: max_vec_values ! First dimension of "VECTORS"
!      array (maximum number of values which can be stored in a vector)
!     Integer, Intent(IN) :: max_vectors ! Second dimension of "VECTORS"
!      array (maximum number of vectors which can be stored)
!     Integer, Intent(IN) :: num_vectors ! Number of values defined in the
!      "time"-series for each vector
!     Integer, Intent(IN) :: STR_VECTORS(max_vectors) ! Starting defined
!      element for "VECTORS"
!     Integer, Intent(IN) :: END_VECTORS(max_vectors) ! End defined
!      elements for "VECTORS"
!     Real, Intent(IN) :: VECTORS(max_vec_values, max_vectors)
!      Multiple 1-D arrays of vector values
!     Character(Len = *), Intent(IN) :: VECTOR_TITLES(max_vectors)
!      Titles for the 1-D data-sets
     Real, Intent(IN) :: AXIS(results%max_values) ! Axis values for vectors
     Integer, Intent(IN) :: vec_num ! Number of vector to transfer
     Integer, Intent(IN) :: xmaxdat ! First Dimension size for "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second Dimension size for "DATA"
!  Export:
     Integer, Intent(OUT) :: xnumdat ! Number of defined elements in X-direction
     Integer, Intent(OUT) :: ynumdat ! Number of defined elements in X-direction
     Integer, Intent(OUT) :: xstrelm ! First X-element of defined region
     Integer, Intent(OUT) :: ystrelm ! First Y-element of defined region
     Integer, Intent(OUT) :: xendelm ! Last X-element of defined region
     Integer, Intent(OUT) :: yendelm ! Last Y-element of defined region
     Real, Intent(OUT) :: DATA(xmaxdat, ymaxdat) ! Main data array
     Real, Intent(OUT) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(OUT) :: YAXIS(ymaxdat) ! Y-axis values
     Character(Len = *), Intent(OUT) :: title ! Title for the graph
     Character(Len = *), Intent(OUT) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: zlabel ! Z-axis label for data
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: num_values ! Number of values to transfer
     Integer :: start ! Start of vector values to be transferred
     Integer :: x ! Loop variable for X-direction
!  Local Arrays:
!  Local Data:
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_VECTOR2DATA ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably defined
     If (results%max_values .Lt. 1) Then
        status = St_bad_dim1
     Else If (results%max_vectors .Lt. 1) Then
        status = St_bad_dim2
     Else If (vec_num .Lt. 1 .Or. vec_num .Gt. results%num_vectors) Then
        status = St_bad_int1
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
 
!     Add module number to status value
        status = St_mod_fit2d + status
 
!     Save details of subroutine call
        Call ST_SAVE ( 'Subroutine F2D_VECTOR2DATA ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        start = results%STARTS(vec_num)
        num_values = results%ENDS(vec_num) - start + 1

!     Transfer vector to data array
        Do x = 1, Min(num_values, xmaxdat)
           DATA(x, 1) = results%VECTORS(start + x - 1, vec_num)
        End Do

!     Transfer axis values 
        Do x = 1, Min(num_values, xmaxdat)
           XAXIS(x) = AXIS(start + x - 1)
        End Do

        YAXIS(1) = 0.0

!     Set scalars
        xnumdat = num_values
        ynumdat = 1
        xstrelm = 1
        ystrelm = 1
        xendelm = num_values
        yendelm = 1
        title = results%TITLES(vec_num)
        zlabel = 'Value'
        xlabel = 'Sample'
        ylabel = ' '

        Call IO_WRITE ('NOTE: Results vector transferred to main data', status)

     End If
 
     End Subroutine F2D_VECTOR2DATA
!********1*********2*********3*********4*********5*********6*********7*********8
