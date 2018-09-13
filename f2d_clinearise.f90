!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_clinearise.f90 *
!  *                    *
!  **********************
 
!+ F2D_CLINEARISE: Calibration LINEARISE intensity values
     Subroutine F2D_CLINEARISE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, status)
!  Description:
!    Corrects non-linearity in intensity scale by inputing from an ascii file
!    values from the raw intensity scale together with there true linear 
!    intensities. The data is corrected in place i.e. the raw intensities are
!    replaced with the linearised ones.
!  Keywords:
!    Calibrate.Linearise, Linearise.Calibration
!  Method:
!    1. Input non-linear and linear intensities from an ascii file
!    2. Fit polynomial
!    3. Correct data by interpolating curve
!  Deficiencies:
!    The correction takes a long time, a LUT could be used instead
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    24-Feb-1999: V0.8 Change "IO_INFREEFORMAT" to "FIO_IN_FREEFORMAT" 
!      (Hammersley)
!    01-Dec-1996: V0.7 Extra argument for "MA_CAL_POLNOMIAL" (Hammersley)
!    16-Nov-1996: V0.6 Convert to using "MA_CAL_POLYNOMIAL", so convert to 
!      single precision (Hammersley)
!    10-Nov-1994: V0.5 Limit to fitting order added to "F2D_POLYNOMIAL" 
!      (Hammersley)
!    04-Jul-1994: V0.4 Extrapolate beyond range of polynomial using linear 
!      interpolation (Hammersley)
!    28-Jun-1994: V0.3 Output graphical representation of data points and 
!      fitted polynomial for user approval (Hammersley)
!    03-May-1994: V0.2 Sort coordinates into ascending order prior to 
!      polynomial fitting (Hammersley)
!    29-Apr-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data values, on output 
!      corrected to a linear scale
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
     Integer, Parameter :: Max_coords = 500 ! Dimension for coordinate arrays
     Integer, Parameter :: Max_order = 50 ! Maximum order of fitted polynomial
!  Local Variables:
     Character(Len = 80) :: title ! File name entered
     Integer :: num_coords ! Number of input coordinates
     Integer :: order ! The order
     Integer stat ! Status return variable for "Allocate"
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Logical :: lower_calculated ! .True., if linearised value of the lower 
!      limit of the polynomial range has been calculated
     Logical :: upper_calculated ! .True., if linearised value of the upper 
!      limit of the polynomial range has been calculated
     Real :: linearised ! Linearised intensity value
     Real :: log_value ! The log of a pixel value
     Real :: lower_range ! Lower limit of calculated polynomial
     Real :: lower_value ! The linearised value for the lower limit of the 
!      polynomial range
     Real :: upper_range ! Upper limit of calculated polynomial
     Real :: upper_value ! The linearised value for the upper limit of the 
!      polynomial range
!  Local Arrays:
     Real, Allocatable :: COEFFICIENTS(:) ! Dynamic array for storing the 
!      polynomial coefficients
     Real, Allocatable :: POLYNOMIAL(:) ! Dynamic array for storing
!      polynomial coefficients for required order
     Real, Allocatable :: RESIDUALS(:) ! Dynamic array for storing the fit 
!      "RESIDUALS" residuals for different polynomial orders
     Real, Allocatable :: X_COORDINATES(:) ! Dynamic array for storing the 
!      angle coordinates
     Real, Allocatable :: WEIGHTS(:) ! Dynamic array for storing the coordinate 
!      weights
     Real, Allocatable :: Y_COORDINATES(:) ! Dynamic array for storing the 
!      intensity coordinates
!  Internal Functions:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CLINEARISE ' // Version)
     Else
 
!     Create dynamic arrays for input of coordinate arrays
        Allocate (X_COORDINATES(Max_coords), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CLINEARISE ' // Version)
           Return
        End If
        Allocate (Y_COORDINATES(Max_coords), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CLINEARISE ' // Version)
           Return
        End If
        Allocate (WEIGHTS(Max_coords), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CLINEARISE ' // Version)
           Return
        End If
        Allocate (COEFFICIENTS((Max_order + 1)**2), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CLINEARISE ' // Version)
           Return
        End If
        Allocate (RESIDUALS(Max_order + 1), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CLINEARISE ' // Version)
           Return
        End If
        Allocate (POLYNOMIAL(Max_order + 1), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CLINEARISE ' // Version)
           Return
        End If

!     Input Angle/Intensity coordinates
        num_coords = 0
        Call IO_WRITE ('INFO: To linearise the data it is ' // &
          'necessary to give the name of a file', status)
        Call IO_WRITE ('      which contains values of the ' // &
          'raw intensity values together with', status)
        Call IO_WRITE ('      corresponding values on the ' // &
          'required (linearised) scale. The raw', status)
        Call IO_WRITE ('      intensity values correspond ' // &
          'the X-coordinates and the corresponding', status)
        Call IO_WRITE ('      linearised intensities the ' // &
          'Y-coordinates.', status)
 
        Call FIO_IN_FREEFORMAT ('Enter name of file ' // &
          'containing non-linear and linear intensities values', Max_coords, &
          num_coords, X_COORDINATES, Y_COORDINATES, title, status)
 
!     Sort coordinates into strictly increasing X-order
        Call MA_SORT (Max_coords, num_coords, X_COORDINATES, Y_COORDINATES, &
          status)
 
!     Divide linearised intensity values by raw values to get correction factor
        Call MA_RDIVIDE (Max_coords, 1, 1, 1, num_coords, 1, &
          X_COORDINATES, Max_coords, 1, Y_COORDINATES, status)
 
!     Take log (10) of X-coordinates
        Call MA_LOGARITHM (Max_coords, 1, 1, 1, num_coords, 1, &
          X_COORDINATES, status)
 
!     Fit coordinates with polynomial
        Call F2D_POLYNOMIAL (0, Max_coords, num_coords, X_COORDINATES, &
          Y_COORDINATES, Max_order, WEIGHTS, COEFFICIENTS, RESIDUALS, &
          lower_range, upper_range, order, POLYNOMIAL, status)
 
!     Free dynamic array space
        Deallocate (X_COORDINATES)
        Deallocate (Y_COORDINATES)
        Deallocate (WEIGHTS)
        Deallocate (COEFFICIENTS)
        Deallocate (RESIDUALS)
 
!     Check that status is O.K.
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''lower_range = '', f12.5)') lower_range
!     Write (*, '(''upper_range = '', f12.5)') upper_range
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate linearised values for all elements in ROI
        lower_calculated = .False.
        upper_calculated = .False.
        Do y = ystrelm, yendelm
 
           Do x = xstrelm, xendelm
 
!           Convert to log(10) scale
              log_value = Alog10(Max(DATA(x, y), 1.0e-10))
 
              If (log_value .Le. lower_range) Then
 
!              Value below valid range of polynomial
                 If (.Not. lower_calculated) Then
 
!                 Need to calculate linearised point for lower limit of range
                    log_value = lower_range
                    Call MA_CAL_POLYNOMIAL (.False., lower_range, upper_range, &
                      order + 1, order, POLYNOMIAL, 1, 1, log_value, &
                      lower_value, status)
                    lower_value = lower_value * lower_range
                    lower_calculated = .True.
 
                 End If
 
!              Use linear extrapolation from end of range
                 DATA(x, y) = Real(lower_value) - (lower_range - DATA(x, y))
 
              Else If (log_value .Gt. upper_range) Then
 
!              Value below valid range of polynomial
                 If (.Not. upper_calculated) Then
 
!                 Need to calculate linearised point for lower limit
!                 of range
                    log_value = upper_range
                    Call MA_CAL_POLYNOMIAL (.False., lower_range, upper_range, &
                      order + 1, order, POLYNOMIAL, 1, 1, upper_range, &
                      upper_value, status)
                    upper_value = upper_value * Dble(upper_range)
                    upper_calculated = .True.
 
                 End If
 
!              Use linear extrapolation from end of range
                 DATA(x, y) = Real(upper_value) + (DATA(x, y) - upper_range)
 
              Else
 
!              Value is in normal range of polynomial
                 Call MA_CAL_POLYNOMIAL (.False., lower_range, upper_range, &
                   order + 1, order, POLYNOMIAL, 1, 1, log_value, &
                   linearised, status)
 
                 DATA(x, y) = linearised * DATA(x, y)
 
              End If
 
           End Do
 
        End Do
 
!     Free polynomial coefficient array
        Deallocate (POLYNOMIAL)
 
     End If
 
     End Subroutine F2D_CLINEARISE
!********1*********2*********3*********4*********5*********6*********7*********8
