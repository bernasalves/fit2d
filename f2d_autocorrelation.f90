!********1*********2*********3*********4*********5*********6*********7**
 
!  ***************************
!  *                         *
!  * f2d_autocorrelation.f90 *
!  *                         *
!  ***************************
 
!+ F2D_AUTOCORRELATION - Fit 2-D:  calculate AUTOCORRELATION function
     Subroutine F2D_AUTOCORRELATION (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, XAXIS, YAXIS, DATA, memory_defined, mxnumdat, &
       mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, MXAXIS, MYAXIS, &
       MDATA, mtitle, mxlabel, mylabel, mzlabel, status)
!  Description:
!    Calculates autocorrelation function of the active data region
!  Method:
!    Fourier transform
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    21-Mar-1997: V0.3 Option to centre output (Hammersley)
!    17-Mar-1997: V0.2 Use "MA_AUTOCORRELATION" to calculate circular 
!      autocorrelation function (Hammersley)
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
     Integer, Intent(IN) :: xstrelm ! Defines X-start of ROI
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of ROI
     Integer, Intent(IN) :: xendelm ! Defines X-end of ROI
     Integer, Intent(IN) :: yendelm ! Defines Y-end of ROI
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array
!  Export:
     Logical, Intent(OUT) :: memory_defined ! .True. if the memory contains data
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of memory
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of memory
     Integer, Intent(OUT) :: mxstrelm ! Defines X-start of ROI
     Integer, Intent(OUT) :: mystrelm ! Defines Y-start of ROI
     Integer, Intent(OUT) :: mxendelm ! Defines X-end of ROI
     Integer, Intent(OUT) :: myendelm ! Defines Y-end of ROI
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
     Integer :: tmpdd2 ! Temporay variable for calculating powers of two
     Integer :: x ! Loop variable for x-direction
     Integer :: xtwopwr ! Number of elements in X-direction Log_2
     Integer :: y ! Loop variable for Y-direction
     Integer :: ytwopwr ! Number of elements in Y-direction Log_2
     Logical, Save :: centre = .True. ! .True., if the output is to be centred 
!      such that the zero point of the auto-correlation is in the centre
!      of the output
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_AUTOCORRELATION ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_AUTOCORRELATION ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead
 
        Call IO_WRITE (' ', status)
        Call IO_WRITE ('NOTE: At present only circular ' // &
          'autocorrelation is offered, and', status)
        Call IO_WRITE ('      this is restricted to numbers ' // &
          'of elements which are powers', status)
        Call IO_WRITE ('      of two in length. This ' // &
          'restriction may be removed in a', status)
        Call IO_WRITE ('      future version.', status)
 
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
 
!     Check that the number of elements is a power of two
        If (2**xtwopwr .Ne. mxnumdat .Or. 2**ytwopwr .Ne. mynumdat) Then
 
!        Not a power of two
           Call IO_WRITE ('WARNING: The array sizes must ' // &
             'be powers of two in size.', status)
           Write (message, '(''INFO: Number of elements in ' // &
             'the ROI = '', i10, '', '', i10)') mxnumdat, mynumdat
           Call IO_WRITE (message, status)
 
           Return
        End If
 
!     Centre ouput
        Call IO_INPL (.True., 0, 1, .True., 'CENTRE OUTPUT', 1, &
          '"YES" to move zero point to centre of output', 1, &
          'Enter "YES" or "NO"', centre, status)
 
!     Copy ROI to memory array
        Call MA_RMOVE (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
          yendelm, xmaxdat, ymaxdat, 1, 1, MDATA, status)
 
!     Perform circular autocorrelation
        Call MA_AUTOCORRELATION (centre, xmaxdat, ymaxdat, mxnumdat, mynumdat, &
          mynumdat, MDATA, status)
 
!     Set titles etc.
        mtitle = 'Autocorrelation Function'
        mxlabel = 'X-direction'
        mylabel = 'Y-direction'
        mzlabel = 'Autocorrelation'
 
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
!  Write (*, '(''End of Subroutine F2D_AUTOCORRELATION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_AUTOCORRELATION
!********1*********2*********3*********4*********5*********6*********7**
 
 
