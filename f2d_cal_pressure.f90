!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_cal_pressure.f90 *
!  *                      *
!  ************************
 
!+ F2D_CAL_PRESSURE - Fit2D: CALculate PRESSURE from volume decrease
     Subroutine F2D_CAL_PRESSURE (k_t, k_t_deriv, v_0divv, pressure, &
       status)
!  Description:
!    Calculates the pressure corresponding to a volume decrease "v" compared
!    "v_0": the volume at room pressure using the third order Birch-Murnaghan 
!    equation of state. 
!  Keywords:
!    Pressure.Volume_Decrease, Volume_Decrease.Pressure
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Apr-2013: V0.2 Output "pressure" in double precision (Hammersley)
!    01-Aug-2013: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Double Precision, Intent(IN) :: k_t ! Bulk modulus
     Double Precision, Intent(IN) :: k_t_deriv ! Derivative of bulk modulus 
!      with pressure
     Double Precision, Intent(IN) :: v_0divv ! v_0 / v
!  Import/Export:
!  Export:
     Double Precision, Intent(OUT) :: pressure ! Corresponding pressure to 
!      volume decrease
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
!  Local Arrays:
!    Local data:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CAL_PRESSURE ' // Version)
        Return
     End If
 
!  Check input arguments
     If (v_0divv .Le. 0.0) Then
        status = St_bad_real1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_CAL_PRESSURE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate pressure using third order Birch-Murnaghan equation of state
        pressure = 1.5 * k_t * (v_0divv**(7.0/3.0) - v_0divv**(5.0/3.0)) * &
          (1.0 + 0.75 * (k_t_deriv - 4.0) * (v_0divv**(2.0/3.0) - 1.0))
 
     End If
 
     End Subroutine F2D_CAL_PRESSURE
!********1*********2*********3*********4*********5*********6*********7*********8
