!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************************
!  *                         *
!  * f2d_masksignificant.f90 *
!  *                         *
!  ***************************
 
!+ F2D_MASKSIGNIFICANT - Define mask elements depending on a statistical
!  significance criterion on the ROI data values
     Subroutine F2D_MASKSIGNIFICANT (xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, status)
!  Description:
!    The user defines a standard deviation significance level above mean in a 
!    moving window.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    17-Oct-2007: V0.1 Original, based of "F2D_MASKTHRESHOLD" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xnumdat ! Number of defined elements in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of defined elements in Y-direction
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      means that an element is masked-off from all operations which take
!      into account masking
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: stat ! Status return variable for "Allocate"
     Integer :: x ! Loop variable
     Integer, Save :: x_window = 51 ! Size of moving window in X-direction
     Integer :: xmaxout ! X-dimension size for "MEANS" and "SIGMAS" arrays
     Integer :: y ! Loop variable
     Integer, Save :: y_window = 51 ! Size of moving window in Y-direction
     Integer :: ymaxout ! Y-dimension size for "MEANS" and "SIGMAS" arrays
     Real, Save :: level = 5.0 ! Decision level
!  Local Arrays:
     Double Precision, Allocatable :: MEANS(:, :) ! Average level in window
     Double Precision, Allocatable :: SIGMAS(:, :) ! Standard deviation in 
!      window
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MASKSIGNIFICANT ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_MASKSIGNIFICANT ' // Version)
 
     Else
 
!     Input moving window size for statistical calculations
        Call GS_INPI (.True., 2, xnumdat, .True., &
          'X-SIZE OF MOVING WINDOW (PIXELS)', 1, &
          'Number of pixels (X-direction) of moving window for statistics', &
          1, 'Enter integer number within given range', x_window, status)
        Call GS_INPI (.True., 2, ynumdat, .True., &
          'Y-SIZE OF MOVING WINDOW (PIXELS)', 1, &
          'Number of pixels (Y-direction) of moving window for statistics', &
          1, 'Enter integer number within given range', y_window, status)
        Call GS_INPR (.True., 0.1, 10000.0, .True., &
          'STANDARD DEVIATION SIGNIFICANCE LEVEL ABOVE MEAN', 1, &
          'Level in standard deviations above mean for pixel to be masked', 1, &
          'Enter real number', level, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Check for user escape
        If (status .Eq. St_goodvalue) Then

           xmaxout = xendelm - xstrelm + 1
           ymaxout = yendelm - ystrelm + 1

!        Allocate work arrays
           Allocate (MEANS(xmaxout, ymaxout), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_MASKSIGNIFICANT ' // Version)
              Return
           End If
           Allocate (SIGMAS(xmaxout, ymaxout), Stat = stat)
           If (stat .Ne. 0) Then
              Deallocate (MEANS)
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_MASKSIGNIFICANT ' // Version)
              Return
           End If

!        Output working message if in GUI mode
           Call GS_FPROMPT (1, 1, 'CALCULATING MASK: PLEASE WAIT', status)
           Call GS_UPDATE (status)
 
!        Calculate mean and standard devition in a moving window around each 
!        pixel
           Call MA_WINDOW_MEANS_SIGMAS (.True., xmaxdat, ymaxdat, &
            xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, &
            x_window, y_window, xmaxout, ymaxout, MEANS, SIGMAS, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''MA_WINDOW_MEANS_SIGMAS finished'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!        Mask out pixels above the significance level          
           Do y = ystrelm, yendelm
 
              Do x = xstrelm, xendelm
 
                 If (.Not. MASK(x, y)) Then

                    If (DATA(x, y) - MEANS(x - xstrelm + 1, y - ystrelm + 1) &
                      .Gt. SIGMAS(x - xstrelm + 1, y - ystrelm + 1) * level) &
                      Then
                       MASK(x, y) = .True.
                    End If
 
                 End If

              End Do
 
           End Do
 
!        Free arrays
           Deallocate (MEANS)
           Deallocate (SIGMAS)

        End If
 
     End If
 
     End Subroutine F2D_MASKSIGNIFICANT
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
