!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_funtype.f90 *
!  *                 *
!  *******************
 
!+ F2D_FUNTYPE - FIT 2-D FUNction TYPE
     Subroutine F2D_FUNTYPE (maxpar, PARDES, numpar, feature, feattype, &
       firstpar, status)
!  Description:
!    Determines the type of feature which is feature number "feature" returning 
!    the result in "feattype" and the number of the parameter with the first 
!    parameter of that feature in "firstpar". If the first parameter of the 
!    required feature type cannot be found, then "feattype" and "firstpar" are 
!    returned with zero values.
!  Keywords:
!    Function~Type.Decode, Decode.Function~Type
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    04-Feb-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: maxpar ! The dimension size of "PARDES"
     Integer, Intent(IN) :: PARDES(maxpar) ! Contains packed information
!      concerning the type and purpose of each of the parameters
     Integer, Intent(IN) :: numpar ! Total of number of parameters in fit
     Integer, Intent(IN) :: feature ! The number of the feature to be determined
!  Export:
     Integer, Intent(OUT) :: feattype ! The type of the feature:
!      1 = Polynomial
!      2 = Gaussian
!      3 = Lorentzian
!      4 = Pseudo-Vogt
!      5 = Chebyshev polynomial
     Integer, Intent(OUT) :: firstpar ! This the number of the parameter
!      which is the first parameter for the requested feature
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Logical :: found ! .False. until the first parameter of the required
!      feature number has been found
!  Local Arrays:
!  External Functions:
     Integer, External :: Ma_exdigit ! Extracts a particular digit from a
!      decimal number
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FUNTYPE ' // Version)
        Return
     End If
 
!  Check input arguments
     If (maxpar .Le. 0) Then
        status = St_bad_dim1
     Else If (numpar .Le.0 .Or. numpar .Gt. maxpar) Then
        status = St_bad_adr1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_FUNTYPE ' // Version)
        Return
     End If
 
!  Find out type of feature from the first parameter of the feature
     found = .False.
     firstpar = 0
     Do While (.Not. found .And. firstpar .Lt. numpar)
 
        firstpar = firstpar + 1
 
        If ((Ma_exdigit(PARDES(firstpar), 2, status) * 10 + &
          Ma_exdigit(PARDES(firstpar), 1, status) .Eq. feature) .And. &
          (Ma_exdigit(PARDES(firstpar), 6, status) * 10 + &
          Ma_exdigit(PARDES(firstpar), 5, status) .Eq. 1)) Then
           found = .True.
           feattype = Ma_exdigit(PARDES(firstpar), 4, status) * 10 + &
             Ma_exdigit(PARDES(firstpar), 3, status)
        End If
 
     End Do
 
!  Set zero values if the first parameter of the feature was not found
     If (.Not. found) Then
        firstpar = 0
        feattype = 0
     End If
 
     End Subroutine F2D_FUNTYPE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 
 
 
