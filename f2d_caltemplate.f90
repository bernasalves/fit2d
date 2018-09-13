!********1*********2*********3*********4*********5*********6*********7*********8

!  ***********************
!  *                     *
!  * f2d_caltemplate.f90 *
!  *                     *
!  ***********************
 
!+ F2D_CALTEMPLATE: CALculate TEMPLATE function
     Subroutine F2D_CALTEMPLATE (xmaxtemplate, ymaxtemplate, xnumtemplate, &
       ynumtemplate, sigma, sub_pixels, TEMPLATE, status)
!  Description:
!    Calculates a template function (Gaussian) of a given "sigma"
!    and a given sub-pixel resolution ("sub_pixels").
!  Keywords:
!    Template~Function.Calculate
!  Method:
!    1 Initialise template array
!    2 Calculate function
!    3 Calculate mean value of function
!    4 Subtract mean
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Oct-1993: V0.1 Original, based on "F2D_CALIBRATEGRID" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxtemplate ! First dimension of template array
     Integer, Intent(IN) :: ymaxtemplate ! Second dimension of template array
     Integer, Intent(IN) :: xnumtemplate ! Number of elements for calculating
!      template in the X-direction
     Integer, Intent(IN) :: ynumtemplate ! Number of elements for calculating
!      template in the Y-direction
     Real, Intent(IN) :: sigma ! Standard deviation of Gaussian in pixels
     Integer, Intent(IN) :: sub_pixels ! Over-sampling of data in sub-pixels
!  Import/Export:
     Real, Intent(INOUT) :: TEMPLATE(xmaxtemplate, ymaxtemplate)
!      Template array, containing required size Gaussian, but with the mean 
!      removed to give zero total intensity
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Real :: mean ! Average value of template function
!  Local Arrays:
!    Internal Functions:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CALTEMPLATE ' // Version)
     Else
 
!     Initialise and calculate template function (Gaussian)
        Call MA_RVALUE (xmaxtemplate, ymaxtemplate, 1, 1, xnumtemplate, &
          ynumtemplate, 0.0, TEMPLATE, status)
 
        Call MA_2GAUSSIAN (xmaxtemplate, ymaxtemplate, 1, 1, xnumtemplate, &
          ynumtemplate, 1.0, Real(xnumtemplate) / 2.0, Real(ynumtemplate) / &
          2.0, sigma * Real(sub_pixels), sigma * Real(sub_pixels), 0.0, 4.0, &
          1, TEMPLATE, status)
 
!     Calculate mean value
        Call MA_RMEAN (xmaxtemplate, ymaxtemplate, TEMPLATE, 1, 1, &
          xnumtemplate, ynumtemplate, mean, status)
 
!     Divide by mean times number of values: normalisation
        Call MA_RCMULT (xmaxtemplate, ymaxtemplate, 1, 1, xnumtemplate, &
          ynumtemplate, 1.0 /(mean * Real(xnumtemplate * ynumtemplate)), &
          TEMPLATE, status)
 
!     Subtract remaining mean value
        Call MA_RCADD (xmaxtemplate, ymaxtemplate, 1, 1, xnumtemplate, &
          ynumtemplate, -1.0 / (Real(xnumtemplate * ynumtemplate)), TEMPLATE, &
          status)
 
     End If
 
     End Subroutine F2D_CALTEMPLATE
!********1*********2*********3*********4*********5*********6*********7*********8
