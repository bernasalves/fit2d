!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************
!  *              *
!  * f2d_axes.f90 *
!  *              *
!  ****************
 
!+ F2D_AXES - Fit 2-D: calculate AXES
     Subroutine F2D_AXES (maxdat, numdat, offset, step, AXIS, status)
!  Description:
!    Set axis values.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    20-Mar-2008: V0.2 Investigating crash on Windows XP system (Hammersley)
!    23-Dec-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: maxdat ! Dimension of array "AXIS"
     Integer, Intent(IN) :: numdat ! Number of axis elements to set
     Real, Intent(IN) :: offset ! Value for first element
     Real, Intent(IN) :: step ! Step between values
!  Import/Export:
     Real, Intent(INOUT) :: AXIS(maxdat) ! Axis values
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: elem ! Loop variable for axes elements
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_AXES ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (maxdat .Le. 0) Then        
        status = St_bad_dim1
     Else If (numdat .Lt. 1 .Or. numdat .Gt. maxdat) Then
        status = St_bad_adr1
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_AXES ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
         Do elem = 1, numdat
           AXIS(elem) = offset + Real(elem - 1) * step
        End Do
 
     End If
 
     End Subroutine F2D_AXES
!********1*********2*********3*********4*********5*********6*********7*********8
