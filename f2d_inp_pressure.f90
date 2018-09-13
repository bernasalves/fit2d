!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_inp_pressure.f90 *
!  *                      *
!  ************************
 
!+ F2D_INP_PRESSURE -  INPut PRESSURE on samples
     Subroutine F2D_INP_PRESSURE (pressure, status)
!  Description:
!    User input of experimental pressure.
!  Keywords:
!    Pressure
!  Method:
!    Uses "GS_INPR"
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Apr-2014: V0.2 Make "pressure" double precision (Hammersley)
!    02-Aug-2013: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
!  Import/Export:
     Double Precision, Intent(INOUT) :: pressure ! Pressure in GPa
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(4) ! User text
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_PRESSURE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        MESSAGE(1) = 'Enter the pressure on the sample(s) ' // &
          'in GPa'
        Call GS_INPD (.True., 0.0d0, 100000.0d0, .True., &
          'SAMPLE PRESSURE (GPa)', 1, MESSAGE, 1, &
          'Must be valid real number', pressure, status)
 
     End If
 
     End Subroutine F2D_INP_PRESSURE
!********1*********2*********3*********4*********5*********6*********7*********8
