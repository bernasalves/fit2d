!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_statistics.f90 *
!  *                    *
!  **********************
 
!+ F2D_STATISTICS: calculate STATISTICS
     Subroutine F2D_STATISTICS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, DATA, status)
!  Description:
!    Calculates and displays statistics of ROI
!  Keywords:
!    Statistics.Add, Add.Statistics, Input.Statistics
!  Method:
!    Uses "MA_STATISTICS" to calculate statistics
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    08-Jan-2004: V0.9 Change text to refer to region of interest (Hammersley)
!    21-Aug-1998: V0.8 Replace "SYMBOL" calls with "VARIABLE" calls (Hammersley)
!    11-Aug-1998: V0.7 Changes to "IO_SET_SYMBOL" (Hammersley)
!    16-Dec-1996: V0.6 Avoid open strings crossing lines (Hammersley)
!    04-Jan-1996: V0.5 Changes for IBM AIX "xlf" compiler: Doesn't like "i" and 
!      "g" format descriptors without width specifiers (Hammersley)
!    21-Jan-1995: V0.4 Add "INFO: " indicator to start of output (Hammersley)
!    25-Oct-1994: V0.3 Store statistical results as program symbol values
!      (Hammersley)
!    04-Apr-1994: V0.2 Output more information (Hammersley)
!    20-Feb-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array
     Integer, Intent(IN) :: xstrelm ! Defines start of ROI in X-direction
     Integer, Intent(IN) :: ystrelm ! Defines start of ROI in Y-direction
     Integer, Intent(IN) :: xendelm ! Defines end of ROI in X-direction
     Integer, Intent(IN) :: yendelm ! Defines end of ROI in Y-direction
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-coordinate grid data
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-coordinate grid data
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data value array
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.9' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! Contains user messages
     Character(Len = 1) :: string_value ! Dummy value for "IO_SET_VARIABLE"
     Integer :: int_value ! Dummy value for "IO_SET_VARIABLE"
     Integer :: len_string ! Dummy value for "IO_SET_VARIABLE"
     Integer :: retstat ! Return status variable:
!      0 = Good status
!      1 = No more room in symbol store
!      2 = No more room for character strings
     Logical :: log_value ! Dummy value for "IO_SET_VARIABLE"
     Real :: maximum ! Maximum data value
     Real :: mean ! Average intensity
     Real :: minimum ! Minimum data value
     Real :: par1, par2, par3, par4 ! Unused, dummy variables
     Real :: rms ! Root mean square value
     Real :: sigma ! Standard deviation
     Real :: skewness ! Skewness parameter
     Real :: total ! Total intensity
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_STATISTICS ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Lt. 1 .Or. xstrelm .Gt. xendelm .Or. xendelm .Gt. &
       xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Lt. 1 .Or. ystrelm .Gt. yendelm .Or. yendelm .Gt. &
       ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_STATISTICS ' // Version)
     Else
 
!     Calculate statistics
        Call MA_STATISTICS (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
          yendelm, maximum, minimum, mean, total, rms, sigma, skewness, par1, &
          par2, par3, par4, status)
 
!     Output results
        Call IO_WRITE ( 'INFO: Statistics of the Region of Interest (ROI):', &
          status)
        Write (message, '(''INFO:  Data Coordinate limits: ' // &
          'Min ( '', 1pg10.2, '' , '', 1pg10.2, '')'')') XAXIS(xstrelm), &
          YAXIS(ystrelm)
        Call IO_WRITE (message, status)
        Write (message, '(''INFO:  Data Coordinate limits: ' // &
          'Max ( '', 1pg10.2, '' , '', 1pg10.2, '')'')') XAXIS(xendelm), &
          YAXIS(yendelm)
        Call IO_WRITE (message, status)
        Write (message, '(''INFO:  Pixel number limits: ' // &
          'Min ( '', i5,'' , '', i5, '') ' // &
          'Max ('', i5, '' , '', i5, '')'')') xstrelm, ystrelm, xendelm, &
          yendelm
        Call IO_WRITE (message, status)
        Write (message, '(''INFO:  Total number of data ' // &
          'values = '', i6, '' * '', i6, '' = '', i10)') (xendelm - xstrelm + &
          1), (yendelm - ystrelm + 1), (xendelm - xstrelm + 1)  * (yendelm - &
          ystrelm + 1)
        Call IO_WRITE (message, status)
        Write (message, '(''INFO:  Minimum data value = '', g12.5)') minimum
        Call IO_WRITE (message, status)
        Write (message, '(''INFO:  Maximum data value = '', g12.5)') maximum
        Call IO_WRITE (message, status)
        Write (message, '(''INFO:  Average (mean) data value = '', g12.5)') &
          mean
        Call IO_WRITE (message, status)
        Write (message, '(''INFO:  Sum of data values = '', g12.5)') total
        Call IO_WRITE (message, status)
        Write (message, '(''INFO:  Root mean square (RMS) value = '', g12.5)') &
          rms
        Call IO_WRITE (message, status)
        Write (message, '(''INFO:  Standard deviation value = '', g12.5)') &
          sigma
        Call IO_WRITE (message, status)
        Write (message, '(''INFO:  Skewness parameter = '', g12.5)') skewness
        Call IO_WRITE (message, status)
 
!     Store the results as program symbols
        Call IO_SET_VARIABLE ('##MINIMUM', 'r', int_value, log_value, minimum, &
          len_string, string_value, retstat, status)
        Call IO_SET_VARIABLE ('##MAXIMUM', 'r', int_value, log_value, maximum, &
          len_string, string_value, retstat, status)
        Call IO_SET_VARIABLE ('##MEAN', 'r', int_value, log_value, mean, &
          len_string, string_value, retstat, status)
        Call IO_SET_VARIABLE ('##MINIMUM', 'r', int_value, log_value, minimum, &
          len_string, string_value, retstat, status)
        Call IO_SET_VARIABLE ('##RMS', 'r', int_value, log_value, rms, &
          len_string, string_value, retstat, status)
        Call IO_SET_VARIABLE ('##SIGMA', 'r', int_value, log_value, sigma, &
          len_string, string_value, retstat, status)
        Call IO_SET_VARIABLE ('##SKEWNESS', 'r', int_value, log_value, &
          skewness, len_string, string_value, retstat, status)
        Call IO_SET_VARIABLE ('##TOTAL', 'r', int_value, log_value, total, &
          len_string, string_value, retstat, status)
 
     End If
 
     End Subroutine F2D_STATISTICS
!********1*********2*********3*********4*********5*********6*********7*********8
