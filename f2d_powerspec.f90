!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_powerspec.f90 *
!  *                   *
!  *********************
 
!+ F2D_POWERSPEC - Fit 2-D:  calculate POWER SPECtrum
     Subroutine F2D_POWERSPEC (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, XAXIS, YAXIS, DATA, memory_defined, mxnumdat, &
       mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, MXAXIS, MYAXIS, &
       MDATA, mtitle, mxlabel, mylabel, mzlabel, status)
!  Description:
!    Calculates power spectrum of the region of interest
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    28-Mar-2006: V0.2 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    13-Mar-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if the graphics window is to be
!      used for user prompts, messages and input
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
     Logical, Intent(OUT) :: memory_defined ! .True. if the memory is defined
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of memory
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of memory
     Integer, Intent(OUT) :: mxstrelm ! Defines X-start of memory ROI
     Integer, Intent(OUT) :: mystrelm ! Defines Y-start of memory ROI
     Integer, Intent(OUT) :: mxendelm ! Defines X-end of memory ROI
     Integer, Intent(OUT) :: myendelm ! Defines Y-end of memory ROI
     Real, Intent(OUT) :: MXAXIS(xmaxdat) ! X-axis values
     Real, Intent(OUT) :: MYAXIS(ymaxdat) ! Y-axis values
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Power spectrum values
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User text
     Integer stat ! Status return variable for "Allocate"
     Integer :: tmpdd2 ! Temporay variable for calculating powers of two
     Integer :: x ! Loop variable for X-direction
     Integer :: xtwopwr ! Number of elements in X-direction Log_2
     Integer :: y ! Loop variable for Y-direction
     Integer :: ytwopwr ! Number of elements in Y-direction Log_2
     Logical :: sort ! .True., if the FFT is to be sorted into normal frequency
!      order
     Logical :: wrkspc ! .True., if the twiddle factors are calculated
!  Local Arrays:
     Real, Allocatable :: XTWIDDLE(:) ! Dynamic array to hold X-direction
!      twiddle factors
     Real, Allocatable :: YTWIDDLE(:) ! Dynamic array to hold X-direction
!      twiddle factors
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_POWERSPEC ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_POWERSPEC ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead
 
!     Number of elements in ROI
        mxnumdat = xendelm - xstrelm + 1
        mynumdat = yendelm - ystrelm + 1
 
!     Check that the number of elements is a power of two in both
!     directions
 
!     Find "xtwopwr" such that "2**xtwopwr = mxnumdat"
        xtwopwr = 0
        tmpdd2 = mxnumdat
        Do While (tmpdd2 .Ge. 2)
           xtwopwr = xtwopwr + 1
           tmpdd2 = tmpdd2 / 2
        End Do
 
!     Find "ytwopwr" such that "2**ytwopwr = mynumdat"
        ytwopwr = 0
        tmpdd2 = mynumdat
        Do While (tmpdd2 .Ge. 2)
           ytwopwr = ytwopwr + 1
           tmpdd2 = tmpdd2 / 2
        End Do
 
        If (2**xtwopwr .Ne. mxnumdat .Or. 2**ytwopwr .Ne. mynumdat) Then
 
           If (2**xtwopwr .Ne. mxnumdat) Then
              Call IO_WRITE ('WARNING: The number of ' // &
                'elements in the ROI in the X-direction', status)
              Call IO_WRITE ('         is not a power of ' // &
                'two, which is a requirement of the', status)
              Call IO_WRITE ('         Radix-2 FFT', status)
           End If
 
           If (2**ytwopwr .Ne. mynumdat) Then
              Call IO_WRITE ('WARNING: The number of ' // &
                'elements in the ROI in the Y-direction', status)
              Call IO_WRITE ('         is not a power of ' // &
                'two, which is a requirement of the', status)
              Call IO_WRITE ('         Radix-2 FFT', status)
           End If
 
           Write (message, '(''INFO: Number of elements in ' // &
             'the ROI = '', i10, '', '', i10)') mxnumdat, mynumdat
           Call IO_WRITE (message, status)
 
           Return
        End If
 
!     Copy ROI to memory array
        Call MA_RMOVE (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
          yendelm, xmaxdat, ymaxdat, 1, 1, MDATA, status)
 
!     Allocate array space for complex exponential multiplying factors
        Allocate (XTWIDDLE(mxnumdat * 2), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_POWERSPEC ' // Version)
           Return
        End If
        Allocate (YTWIDDLE(mynumdat * 2), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_POWERSPEC ' // Version)
           Return
        End If

!     Calculate Fourier Transform
        wrkspc = .False.
        sort = .True.
        Call MA_2DFRFFT (xmaxdat, ymaxdat, mxnumdat, mynumdat, mynumdat, sort, &
          wrkspc, XTWIDDLE, YTWIDDLE, MDATA, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''After MA_2DFRFFT'')')
!     Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Free array space
        Deallocate (XTWIDDLE)
        Deallocate (YTWIDDLE)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''After IO_FREE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     To allow the transform to work in-place some shuffling is
!     needed because the first column contains both the 1-D
!     transforms of the D.C and highest frequencies
 
!     First calculate the sencond highest frequency components in the
!     second to last column to free the last column
        Do y = 1, mynumdat
           MDATA(mxnumdat - 1, y) = MDATA(mxnumdat - 1, y)**2 + &
             MDATA(mxnumdat, y)**2
        End Do
 
!     Calculate highest frequency components ("mxnumdat/2") in
!     last column to free first column
        MDATA(mxnumdat, 1) = MDATA(1, mynumdat / 2 + 1)**2
        Do y = 2, mynumdat / 2
           MDATA(mxnumdat, y) = MDATA(1, mynumdat + 2 - y)**2 + &
             MDATA(2, mynumdat + 2 - y)**2
        End Do
        MDATA(mxnumdat, mynumdat / 2 + 1) = MDATA(2, mynumdat / 2 + 1)**2
        Do y = 1, mynumdat / 2 - 1
           MDATA(mxnumdat, mynumdat + 1 - y) = MDATA(mxnumdat, y + 1)
        End Do
 
!     Calculate D.C. row components in first column
        MDATA(1, 1) = MDATA(1, 1)**2
        Do y = 2, mynumdat / 2
           MDATA(1, y) = MDATA(1, y)**2 + MDATA(2, y)**2
           MDATA(1, mynumdat + 2 - y) = MDATA(1, y)
        End Do
        MDATA(1, mynumdat / 2 + 1) = MDATA(2, 1)**2
 
!     Calculate other frequencies
        Do y = 1, mynumdat
 
           Do x = 2, mxnumdat / 2 - 1
              MDATA(x, y) = MDATA(x * 2 - 1, y)**2 + MDATA(x * 2, y)**2
           End Do
 
!        Transfer two highest frequency columns which have been saved
!        in the last two columns
           MDATA(mxnumdat / 2, y) = MDATA(mxnumdat - 1, y)
           MDATA(mxnumdat / 2 + 1, y) = MDATA(mxnumdat, y)
 
!        Set symmetric frequency values
           Do x = 1, mxnumdat / 2 - 1
              MDATA(mxnumdat + 1 - x, y) =  MDATA(x + 1, y)
           End Do
 
        End Do
 
!     Set titles etc.
        mtitle = 'Power Spectrum'
        mxlabel = 'Frequencies (X)'
        mylabel = 'Frequencies (Y)'
        mzlabel = 'Power'
 
!     Copy axis data
        Do x = 1, mxnumdat
           MXAXIS(x) = Real(x) - 0.5
        End Do
 
        Do y = 1, mynumdat
           MYAXIS(y) = Real(y) - 0.5
        End Do
 
!     Set memory ROI
        mxstrelm = 1
        mystrelm = 1
        mxendelm = mxnumdat
        myendelm = mynumdat
        memory_defined = .True.
 
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''End of Subroutine F2D_POWERSPEC'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_POWERSPEC
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
