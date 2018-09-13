!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************
!  *               *
!  * f2d_rebin.f90 *
!  *               *
!  *****************
 
!+ F2D_REBIN - Fit 2-D  REBINning
     Subroutine F2D_REBIN (gui, masked_data, x_pixel_size, y_pixel_size, &
       xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, MASK, VARIANCES, &
       xstrelm, ystrelm, xendelm, yendelm, &
       variances_exist, retstat, OUTXAXIS, OUTYAXIS, OUTPUT, OUTVARIANCES, &
       xstrout, ystrout, xendout, yendout, xnumout, ynumout, mx_pixel_size, &
       my_pixel_size, status)
!  Description:
!    Rebins "DATA(xmaxdat, ymaxdat)" in the region ("xstrelm", "ystrelm") to
!    ("xendelm", "yendelm") by a number of pixels which is interactively 
!    specified. The result is output in "OUTPUT(xmaxdat, ymaxdat)". Similarly 
!    the variance array "VARIANCE" is rebinned and output in "OUTVARIANCES".
!    The values in "XAXIS" and "YAXIS" are averaged to give the output axes 
!    values in "OUTXAXIS" and "OUTYAXIS".
!  Method:
!    Uses "MA_RREBIN" to perform integer rebinning, and "MA_RSCALE" for 
!    non-integer re-binning.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    07-Jan-2015: V0.5 Re-bin mask (Hammersley)
!    23-Apr-2014: V0.4 Option of masked re-binning (Hammersley)
!    16-Apr-1999: V0.3 Graphical user interface option (Hammersley)
!    26-Mar-1994: V0.2 Non whole pixel rebinning enabled and rebinning may be 
!      to smaller or larger pixels (Hammersley)
!    17-Dec-1988: V0.1 Original, based on "FITREBIN" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if the graphical user interface is
!      to be used
     Logical, Intent(IN) :: masked_data ! .True., if the mask is to be used to
!      mask off pixels
     Real, Intent(IN) :: x_pixel_size ! Size of one unit (raw pixel) in
!      X-direction (metres)
     Real, Intent(IN) :: y_pixel_size ! Size of one unit (raw pixel) in
!      Y-direction (metres)
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array to be rebinned
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! .True., if a pixel is
!      masked-off
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! Variance array to be
!      rebinned
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Logical, Intent(IN) :: variances_exist ! .True., if variances arrays exist
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status variable:
!       0 = Good status
!      -1 = Cancel
     Real, Intent(OUT) :: OUTXAXIS(xmaxdat) ! Averaged X-axis values
     Real, Intent(OUT) :: OUTYAXIS(ymaxdat) ! Averaged Y-axis values
     Real, Intent(OUT) :: OUTPUT(xmaxdat, ymaxdat) ! Result of rebinning
     Real, Intent(OUT) :: OUTVARIANCES(xmaxdat, ymaxdat) ! Rebinned variance
!      array
     Integer, Intent(OUT) :: xstrout ! X-Start of rebinned output region
     Integer, Intent(OUT) :: ystrout ! Y-Start of rebinned output region
     Integer, Intent(OUT) :: xendout ! X-End of rebinned output region
     Integer, Intent(OUT) :: yendout ! Y-End of rebinned output region
     Integer, Intent(OUT) :: xnumout ! Number of X-pixels in the defined output
     Integer, Intent(OUT) :: ynumout ! Number of Y-pixels in the defined output
     Real, Intent(OUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(OUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Integer :: dummy ! Dummy variable for MA_RRBIN, when only 1-D rebinning 
!      is being performed
     Integer :: stat ! Status return variable for "Allocate"
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Logical :: whole_pixels ! .True., if the rebinning is for whole pixels
     Real, Save :: x_origin = 0.0 ! X-coordinate of reference point for
!      pixel scaling
     Real, Save :: xnumbin = 2.0 ! Rebinning factor in the X-direction in pixels
     Real, Save :: y_origin = 0.0 ! Y-coordinate of reference point for
!      pixel scaling
     Real, Save :: ynumbin = 2.0 ! Rebinning factor in the Y-direction in pixels
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(5)
     Logical*1, Allocatable :: TMP_MASK(:, :) ! Temporary mask
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_REBIN ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_REBIN ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     By default set bad status
        retstat = -1
 
!     Input number of pixels by which to rebin in each direction
        MESSAGE(1) = 'Enter number of input pixels (in the ' // &
          'X-direction) to be rebinned'
        MESSAGE(2) = '(scaled) into 1 output pixel. (If whole ' // &
          'numbers are specified for both'
        MESSAGE(3) = 'the X and the Y-directions the ' // &
          'operation will be much faster.'
        MESSAGE(4) = 'Values smaller than one make one input '// &
          'pixel expand to cover more than'
        MESSAGE(5) = 'one output pixel.'
        If (gui) Then
 
           Call GS_INPR (.True., 0.1, Real(xendelm - xstrelm + 1), .True., &
             'X REBIN NUMBER (NUMBER INPUT TO 1 OUTPUT)', 5, MESSAGE, 1, &
             'Enter value within given range', xnumbin, status)
           MESSAGE(1) = 'Enter number of input pixels (in the ' // &
             'X-direction) to be rebinned'
           Call GS_INPR (.True., 0.1, Real(yendelm - ystrelm + 1), .True., &
             'Y REBIN NUMBER (NUMBER INPUT TO 1 OUTPUT)', 5, MESSAGE, 1, &
             'Enter value within given range', ynumbin, status)
 
        Else
 
           Call IO_INPR (.True., 0.1, Real(xendelm - xstrelm + 1), .True., &
             'X REBIN NUMBER (NUMBER INPUT TO 1 OUTPUT)', 5, MESSAGE, 1, &
             'Enter value within given range', xnumbin, status)
           MESSAGE(1) = 'Enter number of input pixels (in the ' // &
             'X-direction) to be rebinned'
           Call IO_INPR (.True., 0.1, Real(yendelm - ystrelm + 1), .True., &
             'Y REBIN NUMBER (NUMBER INPUT TO 1 OUTPUT)', 5, MESSAGE, 1, &
             'Enter value within given range', ynumbin, status)
 
        End If
 
!     Calculate if rebinning is whole pixel rebinning or not
        whole_pixels = Abs(Real(Nint(xnumbin)) - xnumbin) .Lt. 1.0e-5 .And. &
          Abs(Real(Nint(ynumbin)) - ynumbin) .Lt. 1.0e-5
 
        If (whole_pixels) Then
 
!        Starting point for output of rebinning
           xstrout = 1
           ystrout = 1
 
           If (gui) Then
 
              Call GS_INPI (.True., 1, xmaxdat, .True., 'X START OUTPUT', 1, &
                'X Starting pixel for output', 1, 'Must be postive', xstrout, &
                status)
              Call GS_INPI (.True., 1, ymaxdat, .True., 'Y START OUTPUT', 1, &
                'Y Starting pixel for output', 1, 'Must be postive', ystrout, &
                status)
 
           Else
 
              Call IO_INPI (.True., 1, xmaxdat, .True., 'X START OUTPUT', 1, &
                'X Starting pixel for output', 1, 'Must be postive', xstrout, &
                status)
              Call IO_INPI (.True., 1, ymaxdat, .True., 'Y START OUTPUT', 1, &
                'Y Starting pixel for output', 1, 'Must be postive', ystrout, &
                status)
 
           End If
 
        Else
 
           If (gui) Then
 
!           Enter data coordinate unaffected by scaling
              Call GS_INPR (.True., 0.0, Real(xendelm), .True., &
                'X-REFERENCE POSITION FOR SCALING', 1, &
                'Enter X-data coordinate for reference ' // &
                'origin of scaling', 1, 'Must be postive', x_origin, status)
              Call GS_INPR (.True., 0.0, Real(yendelm), .True., &
                'Y-REFERENCE POSITION FOR SCALING', 1, &
                'Enter Y-data coordinate for reference ' // &
                'origin of scaling', 1, 'Must be postive', y_origin, status)
 
           Else
 
!           Enter data coordinate unaffected by scaling
              Call IO_INPR (.True., 0.0, Real(xendelm), .True., &
                'X-REFERENCE POSITION FOR SCALING', 1, &
                'Enter X-data coordinate for reference ' // &
                'origin of scaling', 1, 'Must be postive', x_origin, status)
              Call IO_INPR (.True., 0.0, Real(yendelm), .True., &
                'Y-REFERENCE POSITION FOR SCALING', 1, &
                'Enter Y-data coordinate for reference '// &
                'origin of scaling', 1, 'Must be postive', y_origin, status)
 
           End If
 
!        Enter limits for output region
           xstrout = 1
           ystrout = 1
           xendout = xmaxdat
           yendout = ymaxdat
 
           If (gui) Then
 
              Call GS_INPI (.True., 1, xmaxdat, .True., &
                'LOWER X-LIMIT FOR OUTPUT', 1, &
                'X lower pixel limit for output', 1, 'Must be postive', &
                xstrout, status)
              Call GS_INPI (.True., 1, ymaxdat, .True., &
                'LOWER Y-LIMIT FOR OUTPUT', 1, &
                'Y lower pixel limit for output', 1, 'Must be postive', &
                ystrout, status)
              Call GS_INPI (.True., xstrout, xmaxdat, .True., &
                'UPPER X-LIMIT FOR OUTPUT', 1, &
                'X upper pixel limit for output', 1, 'Must be postive', &
                xendout, status)
              Call GS_INPI (.True., ystrout, ymaxdat, .True., &
                'UPPER Y-LIMIT FOR OUTPUT', 1, &
                'Y upper pixel limit for output', 1, 'Must be postive', &
                yendout, status)
 
           Else
 
              Call IO_INPI (.True., 1, xmaxdat, .True., &
                'LOWER X-LIMIT FOR OUTPUT', 1, &
                'X lower pixel limit for output', 1, 'Must be postive', &
                xstrout, status)
              Call IO_INPI (.True., 1, ymaxdat, .True., &
                'LOWER Y-LIMIT FOR OUTPUT', 1, &
                'Y lower pixel limit for output', 1, 'Must be postive', &
                ystrout, status)
              Call IO_INPI (.True., xstrout, xmaxdat, .True., &
                'UPPER X-LIMIT FOR OUTPUT', 1, &
                'X upper pixel limit for output', 1, 'Must be postive', &
                xendout, status)
              Call IO_INPI (.True., ystrout, ymaxdat, .True., &
                'UPPER Y-LIMIT FOR OUTPUT', 1, &
                'Y upper pixel limit for output', 1, 'Must be postive', &
                yendout, status)
 
           End If
 
        End If
 
!     Check status
        If (status .Ne. St_goodvalue) Then
 
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
           End If
 
           Return
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Rebin by whole pixel method or otherwise as required
        If (whole_pixels) Then
 
!        Rebin data by set amount (no normalisation)
           Call MA_MRREBIN (masked_data, xmaxdat, ymaxdat, DATA, MASK, &
             xstrelm, ystrelm, xendelm, yendelm, Nint(xnumbin), Nint(ynumbin), &
             .False., xstrout, ystrout, &
             xmaxdat, ymaxdat, OUTPUT, xendout, yendout, status)
 
           If (variances_exist) Then
 
!           Rebin data variances by set amount (no normalisation)
              Call MA_MRREBIN (masked_data, xmaxdat, ymaxdat, VARIANCES, MASK, &
                xstrelm, ystrelm, xendelm, yendelm, &
                Nint(xnumbin), Nint(ynumbin), .False., &
                xstrout, ystrout, xmaxdat, ymaxdat, OUTVARIANCES, xendout, &
                yendout, status)
 
           End If
 
!        "Rebin" axis values using normalisation to give the average value
           Call MA_RREBIN (xmaxdat, 1, XAXIS, xstrelm, 1, xendelm, 1, &
             Nint(xnumbin), 1, .True., xstrout, 1, xmaxdat, 1, OUTXAXIS, &
             xendout, dummy, status)
           Call MA_RREBIN (ymaxdat, 1, YAXIS, ystrelm, 1, yendelm, 1, &
             Nint(ynumbin), 1, .True., ystrout, 1, ymaxdat, 1, OUTYAXIS, &
             yendout, dummy, status)
 
        Else
 
!        Non-whole pixel rebinning
           Call MA_RSCALE (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
             yendelm, 1.0/xnumbin, 1.0/ynumbin, x_origin, y_origin, xstrout, &
             ystrout, xendout, yendout, xmaxdat, ymaxdat, OUTPUT, status)
 
           If (variances_exist) Then
 
!           Rebin data variances by set amount (no normalisation)
              Call MA_RSCALE (xmaxdat, ymaxdat, VARIANCES, xstrelm, ystrelm, &
                xendelm, yendelm, 1.0 / xnumbin, 1.0 / ynumbin, x_origin, &
                y_origin, xstrout, ystrout, xendout, yendout, xmaxdat, &
                ymaxdat, OUTVARIANCES, status)
 
           End If
 
!        "Rebin" axis values using normalisation to give the average value
!        Call MA_RSCALE (xmaxdat, 1, XAXIS,
!        :          xstrelm, 1, xendelm, 1,
!        :          1.0/xnumbin, 1.0, x_origin, 0.5,
!        :          xstrout, 1, xendout, 1, xmaxdat, 1,
!        :          OUTXAXIS, status)
!        Call MA_RSCALE (ymaxdat, 1, YAXIS,
!        :          ystrelm, 1, yendelm, 1,
!        :          1.0/ynumbin, 1.0, y_origin, 0.5,
!        :          ystrout, 1, yendout, 1, ymaxdat, 1,
!        :          OUTYAXIS, status)
 
           Do x = 1, xendout
              OUTXAXIS(x) = Real(x) - 0.5
           End Do
 
           Do y = 1, yendout
              OUTYAXIS(y) = Real(y) - 0.5
           End Do
 
        End If

!     Set maximum defined output region
        xnumout = xendout
        ynumout = yendout

!     Allocate memory for temporary mask
        Allocate (TMP_MASK(xmaxdat, ymaxdat), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_REBIN ' // Version)
           Return
        End If

!     Re-bin mask
        Do y = 1, ynumout

           Do x = 1, xnumout
              TMP_MASK(x, y) = MASK(Int((Real(x) - 0.5) * xnumbin) + 1, &
                Int((Real(y) - 0.5) * ynumbin) + 1)
           End Do

        End Do

!     Copy "TMP_MASK" to mask
        Call MA_L1COPY (xmaxdat, ymaxdat, TMP_MASK, xstrelm, ystrelm, xendelm, &
          yendelm, xmaxdat, ymaxdat, MASK, status)

!     Free memory
        Deallocate (TMP_MASK)

!     Set output pixel sizes
        mx_pixel_size = x_pixel_size * xnumbin
        my_pixel_size = y_pixel_size * ynumbin
 
        retstat = 0
 
     End If
 
     End Subroutine F2D_REBIN
!********1*********2*********3*********4*********5*********6*********7*********8
