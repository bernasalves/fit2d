!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_fun1dsymmetry.f90 *
!  *                       *
!  *************************
 
!+ F2D_FUN1DSYMMETRY - FIT2D: FUNction residuals 1-D SYMMETRY calculation
     Subroutine F2D_FUN1DSYMMETRY (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_control, num_control, CONTROL, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       RESIDUALS, status)
!  Description:
!    Calculates objective function for "MA_MODELFIT". The
!    parameter order must not be altered.
!  Keywords:
!    Function.Fit.SYMMETRY, Fit~Function.Calculation
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Mar-2006: V0.2 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    26-May-1998: V0.1 Original, based on "F2D_LSQPOWDER" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc") (Not used)
     Logical, Intent(IN) :: mask_data ! .True., if a data mask exists
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The observed data to be
!      fitted
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! If "mask_data" is
!      .True. then this array defines masked-off elements
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: max_control ! Dimension of "CONTROL" array
     Integer, Intent(IN) :: num_control ! Number of control parameters for
!      the model
     Integer, Intent(IN) :: CONTROL(max_control) ! Special control
!      parameters to pass to the model calculating routine
     Integer, Intent(IN) :: max_par ! Dimension of parameters array
     Integer, Intent(IN) :: num_par ! Number of parameter values
     Real, Intent(IN) :: PARAMS(max_par) ! The function parameters:
!      symmetry centre
     Integer, Intent(IN) :: xmaxmod ! First dimension of "MODEL" array
     Integer, Intent(IN) :: ymaxmod ! Second dimension of "MODEL" array
     Logical, Intent(IN) :: model_align ! .True., if the model is to be
!      aligned to the data, in which case "xmaxmod" must be at least
!      "xendelm" and "ymaxmod" must be at least "yendelm".
!      Otherwise only "xendelm - xstrelm + 1" by
!      "yendelm - ystrelm + 1" elements are used
!  Export:
     Integer, Intent(OUT) :: mod_stat ! Return status variable:
!      0 = Good status
     Real, Intent(OUT) :: RESIDUALS(xmaxmod, ymaxmod) ! The calculated
!      residual values
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: element ! Loop variable for elements
     Integer :: lower ! Lower brackets pixel number
     Integer :: num_defined ! Number of defined values
     Integer :: upper ! Upper brackets pixel number
     Real :: average_residual ! Average residual for the defined coordinates
     Real :: residual ! Residual value for a coordinate
     Real :: sym_value ! Inerpolated symmetry value
     Real :: total ! Totaliser for calculating the average residual value
     Real :: x_reflection ! Reflected coordinate position
     Real :: x_symmetry ! The reflection coordinate
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''ENTERED F2D_FUN1DSYMMETRY'')')
!  Write (*, '(''xstrelm, ystrelm, xendelm, yendelm = '', 4i6)')
!  :    xstrelm, ystrelm, xendelm, yendelm
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  "Decode" variables
     x_symmetry = PARAMS(1)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''x_symmetry = '', g14.7)') x_symmetry
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Initialise counters
     num_defined = 0
     total = 0.0
 
!  First pass: set residuals were possible and calculate average residual value
     Do element = xstrelm, xendelm
 
        If (.Not. MASK(element, ystrelm)) Then
 
!        Find symmetry point
           x_reflection = x_symmetry + (x_symmetry - (Real(element) - 0.5))
 
!        Calculate bracketing elements
           lower = Int(x_reflection) + 1
           upper = lower + 1
 
           If (lower .Lt. 1 .Or. upper .Gt. xendelm) Then
              RESIDUALS(element, ystrelm) = 0.0
           Else If (MASK(lower, ystrelm) .Or. MASK(upper, ystrelm)) Then
              RESIDUALS(element, ystrelm) = 0.0
           Else
 
!           The bracketing symmetric values are available
 
!           Calculate symmetry value by linear interpolation
              lower = lower - 1
              upper = upper - 1
              sym_value = DATA(lower, ystrelm) * (Real(upper) - x_reflection) &
                + DATA(upper, ystrelm) * (x_reflection - Real(lower))
 
!           Calculate residual
              residual = DATA(element, ystrelm) - sym_value
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''element = '', i4, '' value = '', g14.7,
!           :             '' reflection = '', g14.7,
!           :             '' sym_value = '', g14.7)')
!           :             element, DATA(element, ystrelm), x_reflection,
!           :             sym_value
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              RESIDUALS(element, ystrelm) = residual
 
              num_defined = num_defined + 1
              total = total + Abs(residual)
 
           End If
 
        Else
           RESIDUALS(element, ystrelm) = 0.0
        End If
 
     End Do
 
     average_residual = total / Real(num_defined)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''average_residual = '', g14.7)') average_residual
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Second pass: set average residuals to undefined values
     Do element = xstrelm, xendelm
 
        If (.Not. MASK(element, ystrelm)) Then
 
!        Find symmetry point
           x_reflection = x_symmetry - (x_symmetry - (Real(element) - 0.5))
 
!        Calculate bracketing elements
           lower = Int(x_reflection) + 1
           upper = lower + 1
 
           If (lower .Lt. 1 .Or. upper .Gt. xendelm) Then
              RESIDUALS(element, ystrelm) = average_residual
           Else If (MASK(lower, ystrelm) .Or. MASK(upper, ystrelm)) Then
              RESIDUALS(element, ystrelm) = average_residual
           End If
 
        Else
           RESIDUALS(element, ystrelm) = average_residual
        End If
 
     End Do
 
!  Set good return status
     mod_stat = 0
 
     End Subroutine F2D_FUN1DSYMMETRY
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
