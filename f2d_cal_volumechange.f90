!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************************
!  *                          *
!  * f2d_cal_volumechange.f90 *
!  *                          *
!  ****************************
 
!+ F2D_CAL_VOLUMECHANGE - Fit2D: CALculate PRESSURE from volume decrease
     Subroutine F2D_CAL_VOLUMECHANGE (k_t, k_t_deriv, pressure, v_0divv, &
       status)
!  Description:
!    Findss the volume change v_0 divided by v corresponding to a given pressure
!    using the third order Birch-Murnaghan equation of state. 
!  Keywords:
!    Pressure.Volume_Decrease, Volume_Decrease.Pressure
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Apr-2014: V0.3 Use double precision (Hammersley)
!    10-Apr-2014: V0.2 Change to using S. Merkels method (Hammersley)
!    01-Aug-2013: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Double Precision, Intent(IN) :: k_t ! Bulk modulus
     Double Precision, Intent(IN) :: k_t_deriv ! Derivative of bulk modulus 
!      with pressure
     Double Precision, Intent(IN) :: pressure ! Pressure in GPa (?)
!  Import/Export:
!  Export:
     Double Precision, Intent(OUT) :: v_0divv ! v_0 / v
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
     Double Precision, Parameter :: Precision = 0.000001d0
!  Local Variables:
     Double Precision vdivv_0 ! v / v_0
     Double Precision vmax ! Upper volume
     Double Precision vmin ! Lower volume
     Double Precision test_p ! Test pressure
!  Local Arrays:
!    Local data:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CAL_VOLUMECHANGE ' // Version)
        Return
     End If
 
!  Check input arguments
     If (pressure .Lt. 0.0) Then
        status = St_bad_real1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_CAL_VOLUMECHANGE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate pressure
        vmin = 0.1d0
        vmax = 1.0d0
        
        Do While ((vmax - vmin) .Ge. Precision) 

           vdivv_0 = (vmin + vmax) / 2.0
           Call F2D_CAL_PRESSURE (k_t, k_t_deriv, 1.0d0 / vdivv_0, test_p, &
             status)

           If (test_p .Gt. pressure) Then
              vmin = vdivv_0
           Else
              vmax = vdivv_0
           End If

        End Do

     End If

!  Set return value for determining lattice parameters
     v_0divv = Real(1.0d0 / vdivv_0)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''vdivv_0 = '', f8.5)') vdivv_0
!     Write (*, '(''test_p = '', f8.5)') test_p
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_CAL_VOLUMECHANGE
!********1*********2*********3*********4*********5*********6*********7*********8
