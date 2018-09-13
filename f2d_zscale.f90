!********1*********2*********3*********4*********5*********6*********7*********8

!  ******************
!  *                *
!  * f2d_zscale.f90 *
!  *                *
!  ******************
 
!+ F2D_ZSCALE: Z-SCALE requirements
     Subroutine F2D_ZSCALE (status)
!  Description:
!    Set intensity scaling for image display
!  Keywords:
!    Image.Z-Scaling, Intensity~Scaling.Images
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    23-Feb-1999: V0.11 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.10 Change to use IO internal database routines (Hammersley)
!    13-Jan-1998: V0.9 Store Z-scaling requirements in data-base (Hammersley)
!    10-Sep-1996: V0.8 Set X/Y graph Y scale to logarithmic or linear if 
!      required (Hammersley)
!    18-Sep-1995: V0.7 Choice of logarithmic or linear image scaling added 
!      (Hammersley)
!    21-Jul-1995: V0.6 Add weak diffraction peak automatic scaling option 
!      (Hammersley)
!    20-Jun-1995: V0.5 Convert to GS graphics library (Hammersley)
!    29-Oct-1004: V0.4 Make sure that the minimum and maximum display range 
!      values have the correct relationship for "GR_SET_DDR" (Hammersley)
!    01-Jul-1994: V0.3 Include control over X/Y graphs (Hammersley)
!    14-Mar-1994: V0.2 Exit input loop if user escape is issued (Hammersley)
!    20-Aug-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.11' ! Version number
!  Local Variables:
     Integer :: retstat ! Data base return status variable:
!      0 = Good status
!      1 = No more room (for storage), or no more values
!      2 = Key not found for retrieval, or element doesn't exist
     Integer :: scale_mode ! Mode of scaling (thresholding) used to display 
!      image:
!        0 = Automatic full scaling
!        1 = Linear scaling within set limits (thresholds)
!        2 = Linear scaling with set minimum (threshold), but automatic maximum
!        3 = Linear scaling with set maximum (threshold), but automatic minimum
!        4 = "Diffraction peak" automatic scaling, designed to display weak 
!            diffraction peaks
     Logical :: continue ! .True., until input is good
     Logical :: logarithmic ! .True., if logarithmic scaling is to be used
     Logical :: x_linear ! .True., if the X-axis scale is to be linear
     Logical :: xmaxautoddr ! .True., if the X-maximum of the data display 
!      region is to be automatically controlled
     Logical :: xminautoddr ! .True., if the X-minimum of the data display 
!      region is to be automatically controlled
     Logical :: y_linear ! .True., if the Y-axis scale is to be linear
     Logical :: ymaxautoddr ! .True., if the Y-maximum of the data display 
!      region is to be automatically controlled
     Logical :: yminautoddr ! .True., if the Y-minimum of the data display 
!      region is to be automatically controlled
     Real :: min_image ! Minimum displayed value in image
     Real :: max_image ! Maximum displayed value in image
     Real :: xmaxddr ! The maximum X-coordinate of the data display region
     Real :: xminddr ! The minimum X-coordinate of the data display region
     Real :: ymaxddr ! The maximum Y-coordinate of the data display region
     Real :: yminddr ! The minimum Y-coordinate of the data display region
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(6) ! User messages
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ZSCALE ' // Version)
     Else
 
!     Inquire current scale mode
        Call GS_INQ_IMAGESCALE (scale_mode, min_image, max_image, status)
 
        continue = .True.
        Do While (continue .And. status .Eq. St_goodvalue)
 
!        Mode for Z-scaling
           MESSAGE(1)='Enter scaling mode for Z-axis ' // &
             '(intensity) of 2-D data:'
           MESSAGE(2)=' 0 = Automatic full data range'
           MESSAGE(3)=' 1 = User set minimum and maximum'
           MESSAGE(4)=' 2 = User set minimum, but automatic maximum'
           MESSAGE(5)=' 3 = Automatic minimum, and user set maximum'
           MESSAGE(6) = ' 4 = Automatic "weak diffraction peak" scaling'
           Call IO_INPI (.True., 0, 4, .True., 'IMAGE Z-SCALING MODE', 6, &
             MESSAGE, 1, 'Enter integer in given range', scale_mode, status)
 
           If (scale_mode .Eq. 1 .Or. scale_mode .Eq. 2) Then
 
!           Set minimum displayed value
              Call IO_INPR (.False., 0.0, 0.0, .True., &
                'MINIMUM DISPLAY VALUE', 1, &
                'Enter lowest data value in display range', 1, &
                'Enter valid real', min_image, status)
 
           End If
 
           If (scale_mode .Eq. 1 .Or. scale_mode .Eq. 3) Then
 
!           Set maximum displayed value
              Call IO_INPR (.False., 0.0, 0.0, .True., &
                'MAXIMUM DISPLAY VALUE', 1, &
                'Enter lowest data value in display range', 1, &
                'Enter valid real', max_image, status)
 
           End If
 
!        Check that the minimum of the display range is less than
!        the maximum
           If (scale_mode .Eq. 1 .And. min_image .Ge. max_image) Then
 
              Call IO_WRITE ('WARNING: The minimum of the ' // &
                'range has been set greater than or equal to the', status)
              Call IO_WRITE ('WARNING: maximum of the ' // &
                'display range. You must reset the values.', status)
           Else
              continue = .False.
           End If
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Find out present scaling: log/lin
        Call LG_INQ_LOGZSCALING (logarithmic, status)
 
        MESSAGE(1) = 'The false colour image display may use ' // &
          'either linear intensity scaling'
        MESSAGE(2) = '(the default), or logarithmic intensity ' // &
          'scaling. Here you can choose'
        MESSAGE(3) = 'which to use. Enter "YES" for logarithmic ' // &
          'scaling, "NO" for linear'
        MESSAGE(4) = 'scaling.'
        Call IO_INPL (.True., 0, 1, .True., 'LOGARITHMIC IMAGE SCALING', 4, &
          MESSAGE, 1, 'Enter "YES" or "NO"', logarithmic, status)
 
!     Set logarithmic or linear intensity scaling
        Call LG_SET_LOGZSCALING (logarithmic, status)
 
!     Inquire X/Y graph log/linear requirements
        Call GS_INQ_DATALOGLIN (x_linear, y_linear, status)
 
!     Set X/Y graph data log/linear requirements
        Call GS_SET_DATALOGLIN (x_linear, (.Not. logarithmic), status)
 
!     Set X/Y graph axes log/linear requirements
!     Call GS_SET_AXESLOGLIN (x_linear, (.Not. logarithmic),
!     :      status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Set required scale mode
        Call GS_SET_IMAGESCALE (scale_mode, min_image, max_image, status)
 
!     Inquire  current graphics display settings
        Call GS_INQ_AUTODDR (xminautoddr, yminautoddr, xmaxautoddr, &
          ymaxautoddr, status)
        Call GS_INQ_DDR (xminddr, yminddr, xmaxddr, ymaxddr, status)
 
!     Set X/Y graph Y-axis (Z) range
        If (scale_mode .Eq. 0) Then
           Call GS_SET_AUTODDR (.True., .True., .True., .True., status)
        Else If (scale_mode .Eq. 1) Then
           Call GS_SET_AUTODDR (.True., .False., .True., .False., status)
           Call GS_SET_DDR (xminddr, min_image, xmaxddr, max_image, status)
        Else If (scale_mode .Eq. 2) Then
           Call GS_SET_AUTODDR (.True., .False., .True., .True., status)
           Call GS_SET_DDR (xminddr, min_image, xmaxddr, min_image + 1.0, &
             status)
        Else If (scale_mode .Eq. 3) Then
           Call GS_SET_AUTODDR (.True., .True., .True., .False., status)
           Call GS_SET_DDR (xminddr, max_image - 1.0, xmaxddr, max_image, &
             status)
        End If
 
!     Store Z-scaling requirements in internal data-base
        Call IO_SET_IKEYVALUE ('Z_SCALING_MODE', scale_mode, retstat, status)
        Call IO_SET_RKEYVALUE ('Z_SCALING_MINIMUM', min_image, retstat, &
          status)
        Call IO_SET_RKEYVALUE ('Z_SCALING_MAXIMUM', max_image, retstat, &
          status)
        Call IO_SET_LKEYVALUE ('Z_SCALING_LOG', logarithmic, retstat, status)
 
     End If
 
     End Subroutine F2D_ZSCALE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
