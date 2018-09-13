!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_learnprofile.f90 *
!  *                      *
!  ************************
 
!+ F2D_LEARNPROFILE: LEARNs average peak PROFILE
     Subroutine F2D_LEARNPROFILE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, DATA, xmax_peaks, ymax_peaks, xnum_peaks, &
       ynum_peaks, X_2DPEAKS, Y_2DPEAKS, mxnumdat, mynumdat, MXAXIS, MYAXIS, &
       MDATA, mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, &
       mylabel, mzlabel, memory_exist, status)
!  Description:
!    From the corrdinates of the centre of the peaks stored in "X_2DPEAKS" and
!    "Y_2DPEAKS" the average profile at a user input resolution is calculated 
!    and output in the memory.
!  Keywords:
!    Peak~Profile.Learn, Profile.Peak.Learn, Learn.Peak~Profile
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    10-Nov-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: XAXIS(xmaxdat) ! The X-axis values
     Real, Intent(IN) :: YAXIS(xmaxdat) ! The Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Integer, Intent(IN) :: xmax_peaks ! First Dimension size of "X_PEAKS"
!      and "Y_PEAKS"
     Integer, Intent(IN) :: ymax_peaks ! Second Dimension size of "X_PEAKS"
!      and "Y_PEAKS"
     Integer, Intent(IN) :: xnum_peaks ! Number of peaks in X-direction of grid
     Integer, Intent(IN) :: ynum_peaks ! Number of peaks in Y-direction of grid
     Real, Intent(IN) :: X_2DPEAKS(xmax_peaks, ymax_peaks)
!      X-coordinates of peak centres
     Real, Intent(IN) :: Y_2DPEAKS(xmax_peaks, ymax_peaks)
!      Y-coordinates of peak centres
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
     Real, Intent(OUT) :: MXAXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(OUT) :: MYAXIS(ymaxdat) ! Array containing data Y-coordinates
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Array containing data to
!      be fitted
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Logical, Intent(OUT) :: memory_exist ! .True. if the memory contains data
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: num_peaks ! Number of peaks used to calculate average profile
     Integer, Save :: num_pixels = 11 ! Number of input pixels used for
!      calculating profile
     Integer :: num_subpixels ! Number of sub-pixels used for calculating
!      profile
     Integer, Save :: sub_pixels = 5 ! Sub-pixel resolution for calculating
!      profile
     Integer :: x ! Loop variable for X-direction
     Integer :: x_subpixel ! X-sub-pixel centre of peak
     Integer :: xdata ! X-data element to add to profile
     Integer :: xleft ! Number of elements to left of centre of profile
     Integer :: xpeak ! Loop variable for peaks in X-direction of grid
     Integer :: y ! Loop variable for Y-direction
     Integer :: y_subpixel ! Y-sub-pixel centre of peak
     Integer :: ydata ! Y-data element to add to profile
     Integer :: ydown ! Number of elements to below of centre of profile
     Integer :: ypeak ! Loop variable for peaks in Y-direction of grid
     Real :: x_pc ! X-pixel coordinate for peak centre
     Real :: y_pc ! Y-pixel coordinate for peak centre
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(4) ! User messages
!  Internal Functions:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LEARNPROFILE ' // Version)
     Else
 
!     Number of input pixels to use
        MESSAGE(1) = 'Enter size of profile to calculate in ' // &
          'terms of input pixels'
        MESSAGE(2) = 'i.e. the number of input pixels which' // &
          'will be used to calculate the profile'
        Call IO_INPI (.True., 1, 100, .True., &
          'SIZE OF PROFILE (INPUT PIXELS)', 2, MESSAGE, 1, &
          'Value must be within given range', num_pixels, status)
 
!     Number of sub-pixels to use
        MESSAGE(1) = 'Enter number of resolution samples per ' // &
          'pixel to be used to calculate'
        MESSAGE(2) = 'the average profile e.g. 2 means that ' // &
          'each output pixel corresponds to'
        MESSAGE(3) = 'half of the original pixel size'
        Call IO_INPI (.True., 1, 100, .True., &
          'NUMBER OF OVER-SAMPLING SUB-PIXELS', 3, MESSAGE, 1, &
          'Value must be within given range', sub_pixels, status)
 
!     Number of sub-pixels (elements) in profile side
        num_subpixels = num_pixels * sub_pixels
 
        If (num_subpixels .Gt. xmaxdat .Or. num_subpixels .Gt. ymaxdat) Then
           Call IO_WRITE ('WARNING: Program arrays are ' // &
             'not big enough to calculate required profile', status)
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Number of pixels of profile from centre pixel
        xleft = (num_subpixels + 1) / 2
        ydown = (num_subpixels + 1) / 2
 
!     Initialise values of profile
        mxnumdat = num_subpixels
        mynumdat = num_subpixels
        mxstrelm = 1
        mystrelm = 1
        mxendelm = num_subpixels
        myendelm = num_subpixels
        Call MA_RVALUE (xmaxdat, ymaxdat, 1, 1, mxnumdat, mynumdat, 0.0, &
          MDATA, status)
 
!     Loop through peaks
        num_peaks = 0
        Do ypeak = 1, ynum_peaks
 
           Do xpeak = 1, xnum_peaks
 
              If (X_2DPEAKS(xpeak, ypeak) .Gt. -1.7e38) Then
 
!              Add peak profile to summed peak profile
                 num_peaks = num_peaks + 1
 
!              Convert data coordinates to pixel coordinates for peak centres
                 Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, &
                   X_2DPEAKS(xpeak, ypeak), x_pc, status)
                 Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, &
                   Y_2DPEAKS(xpeak, ypeak), y_pc, status)
 
!              Convert pixel coordinates to sub-pixel numbers
                 x_subpixel = Int(x_pc * Real(sub_pixels)) + 1
                 y_subpixel = Int(y_pc * Real(sub_pixels)) + 1
 
                 Do y = 1, num_subpixels
 
                    ydata = (y_subpixel - ydown + y - 1) / sub_pixels + 1
 
                    If (ydata .Ge. ystrelm .And. ydata .Le. yendelm) Then
 
                       Do x = 1, num_subpixels
 
                          xdata = (x_subpixel - xleft + x - 1) / sub_pixels + 1
 
                          If (xdata .Ge. xstrelm .And. xdata .Le. xendelm) Then
                              MDATA(x, y) = MDATA(x, y) + DATA(xdata, ydata)
                           End If
 
                       End Do
 
                    End If
 
                 End Do
 
              End If
 
           End Do
 
        End Do
 
!     Normalise profile to average of a single profile
        Call MA_RCMULT (xmaxdat, ymaxdat, mxstrelm, mystrelm, mxendelm, &
          myendelm, 1.0 / Real(num_peaks), MDATA, status)
 
!     Set up axis data
        Do x = 1, mxnumdat
           MXAXIS(x) = Real(x) - 0.5
        End Do
 
        Do y = 1, mynumdat
           MYAXIS(y) = Real(y) - 0.5
        End Do
 
        mtitle = 'Average Peak Profile'
        mxlabel = 'X-direction'
        mylabel = 'Y-direction'
        mzlabel = 'Intensity'
        memory_exist = .True.
 
     End If
 
     End Subroutine F2D_LEARNPROFILE
!********1*********2*********3*********4*********5*********6*********7*********8
