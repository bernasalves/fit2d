!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_predictor.f90 *
!  *                   *
!  *********************
 
!+ F2D_PREDICTOR - Fit 2-D  apply PREDICTOR algorithm
     Subroutine F2D_PREDICTOR (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, XAXIS, YAXIS, DATA, mxstrelm, mystrelm, mxendelm, &
       myendelm, MXAXIS, MYAXIS, MDATA, status)
!  Description:
!    Applies one of a choice of "predictor" algorithms to
!    "DATA(xmaxdat, ymaxdat)" in the region "(xstrelm, ystrelm)" to
!    "(xendelm, yendelm)" by a number of pixels which is interactively
!    specified. The result is output in "MDATA(xmaxdat, ymaxdat)".
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    22-Apr-1998: V0.3 Add more predictors (Hammersley)
!    19-Jan-1997: V0.2 Add more predictors (Hammersley)
!    11-Jan-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if the graphics window is to be used 
!      for user prompts, messages and input
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array
!  Export:
     Integer, Intent(OUT) :: mxstrelm ! Defines X-start of region of interest
     Integer, Intent(OUT) :: mystrelm ! Defines Y-start of region of interest
     Integer, Intent(OUT) :: mxendelm ! Defines X-end of region of interest
     Integer, Intent(OUT) :: myendelm ! Defines Y-end of region of interest
     Real, Intent(OUT) :: MXAXIS(xmaxdat) ! X-axis values
     Real, Intent(OUT) :: MYAXIS(ymaxdat) ! Y-axis values
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Data values after predictor
!      algorithm
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer :: a ! First element value in pair to average and difference
     Integer :: average ! Truncated average of two values
     Integer :: b ! Second element value in pair to average and difference
     Integer :: iteration ! Loop variable for iterations
     Integer, Save :: num_average = 4 ! Number of averaging iterations
     Integer, Save :: predictor = 1 ! Choice of predictor algorithm:
!      If x is the current pixel, and a, b,c, d are the "previous"
!      pixels as shown:
!      a x
!      c b d
!
!      1 = x - a
!      2 = x - Int ( (a + b) / 2 )
!      3 = x - Int ( (a + b + c) / 3)
!      4 = x - Int ( (a + b + c + d) / 4)
!      5 = x - (a + b - c)
!      6 = x - Int ( (3a + 3b - 2c) / 4)
!      7 = x - Int ( (2a + 2b - c) / 3)
!      8 = Replace pairs of values with rounded average and
!      difference repeately to specified level, and store
!      resulting means using previous value estimator
!      9 = x - Int(a + b + 1) / 2)
!      10 = x - Int(a + b + c + 1) / 3)
!      11 = x - Int(a + b + c + d + 2) / 4)
!
!      (all in integer arithmetic)
     Integer :: previous ! Value of previous mean
     Integer :: x ! Loop variable for X-direction
     Integer :: x_jump ! Gap between pairs of elements to be averaged and 
!      differenced in the X-direction
     Integer :: x_two ! Second array index in X-direction for elements to be 
!      averaged and differenced
     Integer :: y ! Loop variable for Y-direction
     Integer :: y_jump ! Gap between pairs of elements to be averaged and 
!      differenced in the Y-direction
     Integer :: y_two ! Second array index in Y-direction for elements to be 
!      averaged and differenced
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(25) ! User text
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PREDICTOR ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm .Gt. xmaxdat .Or. xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Gt. ymaxdat .Or. yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_PREDICTOR ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Choose predictor algorithm
        MESSAGE(1) = 'Enter choice of predictor algorithm:'
        MESSAGE(2) = ' '
        MESSAGE(3) = 'If x is the current pixel, and a, b,c, d ' // &
          'are the "previous"'
        MESSAGE(4) = 'pixels as shown:'
        MESSAGE(5) = ' '
        MESSAGE(6) = '           a x'
        MESSAGE(7) = '           c b d'
        MESSAGE(8) = ' '
        MESSAGE(9) = '         1: predictor = x - a'
        MESSAGE(10) = '         2: predictor  = x - Int ( ' // '(a + b) / 2)'
        MESSAGE(11) = '         3: predictor  = x - Int ( ' // &
          '(a + b + c) / 3)'
        MESSAGE(12) = '         4: predictor  = x - Int ( ' // &
          '(a + b + c + d) / 4)'
        MESSAGE(13) = '         5: predictor  = x - ( ' // '(a + b - c)'
        MESSAGE(14) = '         6: predictor  = x - Int ( ' // &
          '(3a + 3b - 2c) / 4)'
        MESSAGE(15) = '         7: predictor  = x - Int ( ' // &
          '(2a + 2b - c) / 3)'
        MESSAGE(16) = '         8: For pairs of values a and b, ' // &
          'replace with a'' = Int((a+b)/2)'
        MESSAGE(17) = '            and b'' = a - b (this is reversable)'
        MESSAGE(18) = '            This is applied alternately ' // &
          'horizontallly and vertically to'
        MESSAGE(19) = '            a specified level, then the ' // &
          'series of a'' values are stored'
        MESSAGE(20) = '            as a simple previous value ' // &
          'differential'
        MESSAGE(21) = '         9: predictor  = x - Int ( ' // &
          '(a + b + 1) / 2)'
        MESSAGE(22) = '        10: predictor  = x - Int ( ' // &
          '(a + b + c + 1) / 3)'
        MESSAGE(23) = '        11: predictor  = x - Int ( ' // &
          '(a + b + c + d + 1) / 4)'
        MESSAGE(24) = ' '
        MESSAGE(25) = '(All in integer arithmetic)'
 
        If (gui) Then
 
           Call GS_INPI (.True., 1, 11, .True., &
             'PREDICTOR ALGORITHM TO APPLY', 25, MESSAGE, 1, &
             'Enter integer value within given range', predictor, status)
 
        Else
 
           Call IO_INPI (.True., 1, 11, .True., &
             'PREDICTOR ALGORITHM TO APPLY', 25, MESSAGE, 1, &
             'Enter integer value within given range', predictor, status)
 
        End If
 
        If (status .Eq. St_escapevalue) Then
 
           status = St_goodvalue
 
        Else If (status .Eq. St_goodvalue) Then
 
!        Apply predictor
 
           If (predictor .Ne. 8) Then
 
!           First element is the same
              MDATA(xstrelm, ystrelm) = DATA(xstrelm, ystrelm)
 
!           First row is always previous pixel
              Do x = xstrelm + 1, xendelm
                 MDATA(x, ystrelm) = Nint(DATA(x, ystrelm)) - Nint(DATA(x - 1, &
                   ystrelm))
              End Do
 
              If (predictor .Eq. 1) Then
 
!              Estimate = a
 
!              Loop over all other rows
                 Do y = ystrelm + 1, yendelm
 
!                 First pixel of each line
                    MDATA(xstrelm, y) = Nint(DATA(xstrelm, y)) - &
                      Nint(DATA(xstrelm, y - 1))
 
!                 General case
                    Do x = xstrelm + 1, xendelm
                       MDATA(x, y) = Nint(DATA(x, y)) - Nint(DATA(x - 1, y))
                    End Do
 
                 End Do
 
              Else If (predictor .Eq. 2) Then
 
!              Estimate = Int((a + b) / 2)
 
!              Loop over all other rows
                 Do y = ystrelm + 1, yendelm
 
!                 First pixel of each line
                    MDATA(xstrelm, y) = Nint(DATA(xstrelm, y)) - &
                      Nint(DATA(xstrelm, y - 1))
 
!                 General case
                    Do x = xstrelm + 1, xendelm
                       MDATA(x, y) = Nint(DATA(x, y)) - Int( (Nint(DATA(x - 1, &
                         y)) + Nint(DATA(x, y - 1))) / 2 )
                    End Do
 
                 End Do
 
              Else If (predictor .Eq. 9) Then
 
!              Estimate = Int((a + b + 1) / 2)
 
!              Loop over all other rows
                 Do y = ystrelm + 1, yendelm
 
!                 First pixel of each line
                    MDATA(xstrelm, y) = Nint(DATA(xstrelm, y)) - &
                      Nint(DATA(xstrelm, y - 1))
 
!                 General case
                    Do x = xstrelm + 1, xendelm
                       MDATA(x, y) = Nint(DATA(x, y)) - Int( (Nint(DATA(x - 1, &
                         y)) + Nint(DATA(x, y - 1)) + 1) / 2 )
                    End Do
 
                 End Do
 
              Else If (predictor .Eq. 3) Then
 
!              Estimate = Int((a + b + c) / 3)
 
!              Loop over all other rows
                 Do y = ystrelm + 1, yendelm
 
!                 First pixel of each line
6                    MDATA(xstrelm, y) = Nint(DATA(xstrelm, y)) - &
                      Nint(DATA(xstrelm, y - 1))
 
!                 General case
                    Do x = xstrelm + 1, xendelm
                       MDATA(x, y) = Nint(DATA(x, y)) - Int( (Nint(DATA(x - 1, &
                         y)) + Nint(DATA(x - 1, y - 1)) + Nint(DATA(x, y - &
                         1))) / 3 )
                    End Do
 
                 End Do
 
              Else If (predictor .Eq. 10) Then
 
!              Estimate = Int((a + b + c + 1) / 3)
 
!              Loop over all other rows
                 Do y = ystrelm + 1, yendelm
 
!                 First pixel of each line
                    MDATA(xstrelm, y) = Nint(DATA(xstrelm, y)) - &
                      Nint(DATA(xstrelm, y - 1))
 
!                 General case
                    Do x = xstrelm + 1, xendelm
                       MDATA(x, y) = Nint(DATA(x, y)) - Int( (Nint(DATA(x - 1, &
                         y)) + Nint(DATA(x - 1, y - 1)) + Nint(DATA(x, y - 1)) &
                         + 1) / 3 )
                    End Do
 
                 End Do
 
              Else If (predictor .Eq. 4) Then
 
!              Estimate = Int((a + b + c + d) / 4)
 
!              Loop over all other rows
                 Do y = ystrelm + 1, yendelm
 
!                 First pixel of each line
                    MDATA(xstrelm, y) = Nint(DATA(xstrelm, y)) - Int( &
                      (Nint(DATA(xstrelm, y - 1)) + Nint(DATA(xstrelm + 1, y - &
                      1)) ) / 2)
 
!                 General case
                    Do x = xstrelm + 1, xendelm - 1
                       MDATA(x, y) = Nint(DATA(x, y)) - Int( (Nint(DATA(x - 1, &
                         y)) + Nint(DATA(x - 1, y - 1)) + Nint(DATA(x, y - 1)) &
                         + Nint(DATA(x + 1, y - 1))) / 4 )
                    End Do
 
!                 Last pixel of each line
                    MDATA(xendelm, y) = Nint(DATA(xendelm, y)) - Int( &
                      (Nint(DATA(xendelm - 1, y)) + Nint(DATA(xendelm - 1, y - &
                      1)) + Nint(DATA(xendelm, y - 1)) ) / 3)
 
                 End Do
 
              Else If (predictor .Eq. 11) Then
 
!              Estimate = Int((a + b + c + d + 2) / 4)
 
!              Loop over all other rows
                 Do y = ystrelm + 1, yendelm
 
!                 First pixel of each line
                    MDATA(xstrelm, y) = Nint(DATA(xstrelm, y)) - Int( &
                      (Nint(DATA(xstrelm, y - 1)) + Nint(DATA(xstrelm + 1, y - &
                      1)) ) / 2)
 
!                 General case
                    Do x = xstrelm + 1, xendelm - 1
                       MDATA(x, y) = Nint(DATA(x, y)) - Int( (Nint(DATA(x - 1, &
                         y)) + Nint(DATA(x - 1, y - 1)) + Nint(DATA(x, y - 1)) &
                         + Nint(DATA(x + 1, y - 1)) + 2) / 4 )
                    End Do
 
!                 Last pixel of each line
                    MDATA(xendelm, y) = Nint(DATA(xendelm, y)) - Int( &
                      (Nint(DATA(xendelm - 1, y)) + Nint(DATA(xendelm - 1, y - &
                      1)) + Nint(DATA(xendelm, y - 1)) + 1) / 3)
 
                 End Do
 
              Else If (predictor .Eq. 5) Then
 
!              Estimate = a + b - c
 
!              Loop over all other rows
                 Do y = ystrelm + 1, yendelm
 
!                 First pixel of each line
                    MDATA(xstrelm, y) = Nint(DATA(xstrelm, y)) - &
                      Nint(DATA(xstrelm, y - 1))
 
!                 General case
                    Do x = xstrelm + 1, xendelm
                       MDATA(x, y) = Nint(DATA(x, y)) - ( Nint(DATA(x - 1, y)) &
                         - Nint(DATA(x - 1, y - 1)) + Nint(DATA(x, y - 1)))
                    End Do
 
                 End Do
 
              Else If (predictor .Eq. 6) Then
 
!              Estimate = Int((3a + 3b - 2c) / 4)
 
!              Loop over all other rows
                 Do y = ystrelm + 1, yendelm
 
!                 First pixel of each line
                    MDATA(xstrelm, y) = Nint(DATA(xstrelm, y)) - &
                      Nint(DATA(xstrelm, y - 1))
 
!                 General case
                    Do x = xstrelm + 1, xendelm
                       MDATA(x, y) = Nint(DATA(x, y)) - Int( (3 * Nint(DATA(x &
                         - 1, y)) - 2 * Nint(DATA(x - 1, y - 1)) + 3 * &
                         Nint(DATA(x, y - 1))) / 4 )
                    End Do
 
                 End Do
 
              Else If (predictor .Eq. 7) Then
 
!              Estimate = Int((2a + 2b - c) / 3)
 
!              Loop over all other rows
                 Do y = ystrelm + 1, yendelm
 
!                 First pixel of each line
                    MDATA(xstrelm, y) = Nint(DATA(xstrelm, y)) - &
                      Nint(DATA(xstrelm, y - 1))
 
!                 General case
                    Do x = xstrelm + 1, xendelm
                       MDATA(x, y) = Nint(DATA(x, y)) - Int( (2 * Nint(DATA(x &
                         - 1, y)) - Nint(DATA(x - 1, y - 1)) + 2 * &
                         Nint(DATA(x, y - 1))) / 3 )
                    End Do
 
                 End Do
 
              End If
 
           Else If (predictor .Eq. 8) Then
 
!           Replace pairs of pixels with rounded mean and difference
              MESSAGE(1) = 'Enter number of times pairs of ' // &
                'pixels should be replace with rounded'
              MESSAGE(2) = 'mean and differences'
              If (gui) Then
 
                 Call GS_INPI (.True., 1, 12, .True., &
                   'NUMBER OF AVERAGING ITERATIONS', 2, MESSAGE, 1, &
                   'Enter integer value within given range', num_average, &
                   status)
 
              Else
 
                 Call IO_INPI (.True., 1, 12, .True., &
                   'NUMBER OF AVERAGING ITERATIONS', 2, MESSAGE, 1, &
                   'Enter integer value within given range', num_average, &
                   status)
 
!              Copy data to memory array (for same behaviour as the other 
!              predictors i.e. output in memory)
                 Call MA_RCOPY (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, &
                   xendelm, yendelm, xmaxdat, ymaxdat, MDATA, status)
 
!              Apply averaging and difference algorithm
                 x_jump = 1
                 y_jump = 1
                 Do iteration = 1, num_average
 
!                 Check odd / even iteration
                    If (2 * (iteration / 2) .Ne. iteration) Then
 
!                    Horizontal iteration
                       Do y = ystrelm, yendelm, y_jump
 
                          Do x = xstrelm, xendelm, x_jump * 2
 
                             x_two = x + x_jump
                             If (x_two .Le. xendelm) Then
                                a = Nint(MDATA(x, y))
                                b = Nint(MDATA(x_two, y))
                                average = (a + b) / 2
                                MDATA(x_two, y) = a - b
                                MDATA(x, y) = average
                             End If
 
                          End Do
 
                       End Do
 
                    Else
 
!                    Vertical iteration
                       Do y = ystrelm, yendelm, y_jump * 2
 
                          Do x = xstrelm, xendelm, x_jump
 
                             y_two = y + y_jump
                             If (y_two .Le. yendelm) Then
                                a = Nint(MDATA(x, y))
                                b = Nint(MDATA(x, y_two))
                                average = (a + b) / 2
                                MDATA(x, y_two) = a - b
                                MDATA(x, y) = average
                             End If
 
                          End Do
 
                       End Do
 
!                    Increase jumps between elements
                       x_jump = x_jump * 2
                       y_jump = y_jump * 2
 
                    End If
 
                 End Do
 
!              Account for extra X-iteration
                 If (2 * (num_average / 2) .Ne. num_average) Then
                    x_jump = x_jump * 2
                 End If
 
!              Store remains means as differences with previous value
                 previous = 0
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''x_jump, y_jump = '', 2i6)')
!              :                x_jump, y_jump
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 Do y = ystrelm, yendelm, y_jump
 
                    Do x = xstrelm, xendelm, x_jump
 
                       a = Nint(MDATA(x, y))
                       MDATA(x, y) = a - previous
                       previous = a
 
                    End Do
 
                 End Do
 
              End If
 
           End If
 
!        Copy axis data
           Do x = 1, xendelm
              MXAXIS(x) = XAXIS(x)
           End Do
 
           Do y = 1, yendelm
              MYAXIS(y) = YAXIS(y)
           End Do
 
!        Set memory ROI
           mxstrelm = xstrelm
           mxendelm = xendelm
           mystrelm = ystrelm
           myendelm = yendelm
 
        End If
 
     End If
 
     End Subroutine F2D_PREDICTOR
!********1*********2*********3*********4*********5*********6*********7*********8
