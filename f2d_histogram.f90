!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_histogram.f90 *
!  *                   *
!  *********************
 
!+ F2D_HISTOGRAM - Fit 2-D  image HISTOGRAM
     Subroutine F2D_HISTOGRAM (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, title, mxstrelm, mystrelm, mxendelm, myendelm, &
       MDATA, MXAXIS, MYAXIS, mtitle, mxlabel, mylabel, mzlabel, status)
!  Description:
!    Produces frequency histogram of pixel values in the ROI.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Nov-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if the graphical user interface is
!      being used
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array to histogram
     Character(Len = *), Intent(IN) :: title ! Title for plot
!  Export:
     Integer, Intent(OUT) :: mxstrelm ! X-start of memory region of interest
     Integer, Intent(OUT) :: mystrelm ! Y-start of memory region of interest
     Integer, Intent(OUT) :: mxendelm ! X-end of memory region of interest
     Integer, Intent(OUT) :: myendelm ! Y-end of memory region of interest
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Output of histogram
     Real, Intent(OUT) :: MXAXIS(xmaxdat) ! Array containing memory
!      X-coordinates
     Real, Intent(OUT) :: MYAXIS(ymaxdat) ! Array containing memory
!      Y-coordinates
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: bin ! Hisogram bin to increment
     Integer :: num_bins ! Number of bins to use for the histogram
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Real :: bin_size ! Width of one bin
     Real :: minimum_value ! Minimum value found in data region, and
!      minimum value for calculating histogram
     Real :: maximum_value ! Maximum value found in data region, and
!      maximum value for calculating histogram
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_HISTOGRAM ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_HISTOGRAM ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Find out minimum and maximum data values
        Call MA_RMINMAX (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
          yendelm, minimum_value, maximum_value, status)
 
!     Inform user of minimum and maximum values
        Call IO_WRITE (' ', status)
        Write (message, &
          '(''INFO: Region of interest minimum value = '', g14.7)') &
          minimum_value
        Call IO_WRITE (message, status)
        Write (message, &
          '(''      Region of interest maximum value = '', g14.7)') &
          maximum_value
        Call IO_WRITE (message, status)
        Call IO_WRITE (' ', status)
 
        If (gui) Then
 
!        Minimum pixel value for calculating histogram
           Call GS_INPR (.True., minimum_value, maximum_value, .True., &
             'HISTOGRAM MINIMUM VALUE', 1, &
             'Enter minimum pixel value for calculating histogram', 1, &
             'Enter real number', minimum_value, status)
 
!        Maximum pixel value for calculating histogram
           Call GS_INPR (.True., minimum_value, maximum_value, .True., &
             'HISTOGRAM MAXIMUM VALUE', 1, &
             'Enter maximum pixel value for calculating histogram', 1, &
             'Enter real number', maximum_value, status)
 
        Else
 
!        Minimum pixel value for calculating histogram
           Call IO_INPR (.True., minimum_value, maximum_value, .True., &
             'HISTOGRAM MINIMUM VALUE', 1, &
             'Enter minimum pixel value for calculating histogram', 1, &
             'Enter real number', minimum_value, status)
 
!        Maximum pixel value for calculating histogram
           Call IO_INPR (.True., minimum_value, maximum_value, .True., &
             'HISTOGRAM MAXIMUM VALUE', 1, &
             'Enter maximum pixel value for calculating histogram', 1, &
             'Enter real number', maximum_value, status)
 
        End If
 
!     Default number of bins
        num_bins = Min((Nint(maximum_value) - Nint(minimum_value)), xmaxdat)
 
!     How many histogram bins
        If (gui) Then
 
           Call GS_INPI (.True., 1, xmaxdat, .True., &
             'NUMBER OF HISTOGRAM BINS', 1, &
             'Enter required number of histogram bins', 1, &
             'Enter real number', num_bins, status)
 
        Else
 
           Call IO_INPI (.True., 1, xmaxdat, .True., &
             'NUMBER OF HISTOGRAM BINS', 1, &
             'Enter required number of histogram bins', 1, &
             'Enter real number', num_bins, status)
 
        End If
 
!     Initialise differences array
        Do x = 1, num_bins
           MDATA(x, 1) = 0.0
        End Do
 
 
!     Calculate size of one bin
        bin_size = (maximum_value - minimum_value) / Real (num_bins)
 
!     Loop through data
        Do y = ystrelm, yendelm
 
           Do x = xstrelm, xendelm
 
!           Calculate histogram bin
              bin = Int((DATA(x, y) - minimum_value) / bin_size) + 1
 
!           Increment histogram
              If (bin .Ge. 1 .And. bin .Le. num_bins) Then
 
                 MDATA(bin, 1) = MDATA(bin, 1) + 1.0
              End If
 
           End Do
 
        End Do
 
!     Set output array sizes
        mxstrelm = 1
        mystrelm = 1
        mxendelm = num_bins
        myendelm = 1
 
!     Set output title and axis labels
        mtitle = Trim(title) // ': Frequency Histogram'
        mxlabel = 'Pixel Values'
        mylabel = ' '
        mzlabel = 'Frequency'
 
!     Set axis values
        Do x = 1, num_bins
           MXAXIS(x) = minimum_value + (Real(x) - 0.5) * bin_size
        End Do
        MYAXIS(1) = 1.0
 
     End If
 
     End Subroutine F2D_HISTOGRAM
!********1*********2*********3*********4*********5*********6*********7*********8
