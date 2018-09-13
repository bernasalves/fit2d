!********1*********2*********3*********4*********5*********6*********7**
 
!  *******************
!  *                 *
!  * f2d_sradial.f90 *
!  *                 *
!  *******************
 
!+ F2D_SRADIAL - FIT2D: Spatial distortion correction RADIAL profile from 2-D 
!  image
     Subroutine F2D_SRADIAL (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, DATA, MASK, variances_exist, EXPERIMENT, &
       lorentz_geometry, angular_scan, &
       radial_pixel_size, max_radial, num_radial, RAD_AXIS, PROFILE, PROVARS, &
       NUMPIXELS, status)
!  Description:
!    Calculates the average radial profile given the values in
!    "DATA" in the region from "(xstrelm, ystrelm)" to
!    "(xendelm, yendelm)". If "angular_scan = .True." then a scan
!    with equal angle size is calculated, otherwise a scan based on
!    equal radial distance on an orthogonal detector is calculated.
!    Data values are not to be included if they are "mask-off"
!    which is defined by .True. in the corresponding element of
!    "MASK". The profile is calculated from the centre given by
!    the coordinate "(x_beam, y_beam)" and allows for detector tilt,
!    which is defined as a tilt angle of "tilt" in a plane which is
!    at a rotation angle of "rotation" from the X-axis (in the ideal
!    detector plane). The image pixels are assumed to be regular rectangles.
!  Keywords:
!    Radial~Profile.Calculation, Calculate.Radial~Profile,
!    Profile.Radial.Calculation
!  Method:
!    The radial distance of the centre of each data pixel is
!    calculated, taking into account the detector tilt. First each
!    pixel is rotated to the "tilt plane", and then the radial
!    distance for the rotated pixel position is calculated, taking
!    into account the tilt (see 15:8).
!
!    Assuming the pixel to be aligned to the radial direction
!    (approximation), the start and end position of output pixels
!    "covered" by the input pixel is calculated. The intensity in the
!    input pixel is distributed in proportion to coverage of output
!    pixels. The fraction of input pixels added to each output is
!    recorded. At the end the counts in all bins are normalised by the
!    fractional number of contributing input bins.
!  Deficiencies:
!    The manner in which the counts from different input pixels are
!    divided up into different output pixels does not take account of
!    the pixel orientation.
!
!    Doesn't (yet) take into account differing number of
!    independent results when calculating variances.
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    28-Mar-2006: V0.9 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    13-Mar-2006: V0.8 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    16-Dec-1996: V0.7 Avoid open strings crossing lines (Hammersley)
!    02-Sep-1996: V0.6 Use new "F2D_CORR2_RADIAL" which does not use NAG code 
!      (Hammersley)
!    26-Feb-1996: V0.5 Changes to "F2D_IN_SPATIAL" (Hammersley)
!    23-Oct-1995: V0.4 Change to argument list (Hammersley)
!    01-Sep-1995: V0.3 Option of applying polarisation correction (Hammersley)
!    28-Aug-1995: V0.2 Correct for polarisation (Hammersley)
!    14-Mar-1995: V0.1 Original, based on "F2D_SRADIAL" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'io.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! 1st dimension of array "DATA" and "MASK"
     Integer, Intent(IN) :: ymaxdat ! 2nd dimension of array "DATA" and "MASK"
     Integer, Intent(IN) :: xstrelm ! X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Y-end of region of interest
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis data
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis data
     Real, Intent(IN) :: DATA(xmaxdat,ymaxdat) ! Data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Logical, Intent(IN) :: variances_exist ! .True., if the variances array
!      exists
     TYPE(EXPERIMENTAL_DETAILS), Intent(INOUT) :: EXPERIMENT ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: lorentz_geometry ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the
!        equivalent of a 2-theta scan i.e. detector at equal distance
     Logical, Intent(IN) :: angular_scan ! .True., if the 1-D scan is to be
!      produced in equal angle elements as opposed to equal radial distance
!      elements
     Real, Intent(IN) :: radial_pixel_size ! Size of pixel to be used for
!      the radial 1-D histogram. If an equal radial distance scan is to
!      be calculated ("angular_scan = .False.") the units are
!      metres, and for an equal angle pixel scan ("angular_scan =
!      .True.") the units are radians
     Integer, Intent(IN) :: max_radial ! Dimension of array "RAD_AXIS",
!      "PROFILE" and "NUMPIXELS"
!  Export:
     Integer, Intent(OUT) :: num_radial ! Number of defined radial profile
!      pixels
     Real, Intent(OUT) :: RAD_AXIS(max_radial) ! The world coordinate
!      positions for which the radial profile is to be calculated
     Real, Intent(OUT) :: PROFILE(max_radial) ! The averaged radial profile
     Real, Intent(OUT) :: PROVARS(max_radial) ! The estimated variance at
!      each radial profile position (only if "variances_exist" is .True.)
     Real, Intent(OUT) :: NUMPIXELS(max_radial) ! The number of pixels used
!      to average each radial profile pixel
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.9' ! Version number
!  Local Variables:
     Character(Len = 10) :: file_name ! Dummy variable
     Integer :: maxwork ! First dimension size of temporary arrays
     Integer :: retstat ! Return status from "F2D_IN_SPATIAL":
!      0 = Good status
!      1 = Bad status, grid too large for internal array
!      2 = Bad status, peaks missing from central cross
     Integer stat ! Status return variable for "Allocate"
     Integer :: x_low ! Lower X-pixel limit of spline region
     Integer :: x_up ! Upper X-pixel limit of spline  region
     Integer :: x_xnumknots ! Number of X-axis "knot" positions for 
!      X-distortion spline function
     Integer :: x_ynumknots ! Number of Y-axis "knot" positions for
!      X-distortion spline function
     Integer :: xmaxknots ! Maximum number of knot points in X-direction
     Integer :: y_low ! Lower Y-pixel limit of spline region
     Integer, Save :: y_rows = 500 ! 2nd dimension of temporary arrays, number
!      of rows of the array for which the distortions are calculated in one go
     Integer :: y_up ! Upper Y-pixel limit of spline region
     Integer :: y_xnumknots ! Number of X-axis "knot" positions for
!      Y-distortion spline function
     Integer :: y_ynumknots ! Number of Y-axis "knot" positions for
!      Y-distortion spline function
     Integer :: ymaxknots ! Maximum number of knot points in Y-direction
     Real :: cor_grid_spacing ! Grid spacing for correction function
     Real, Save :: overload_value = 99999.5 ! Value above which pixels are
!      considered to be over-loaded
     Real :: temp ! Temporary storage
     Real :: x_cor_size ! Size of corrected pixel in metres in X-direction
     Real :: x_maximum ! Maximum X-value applicable to spline interpolation
     Real :: x_minimum ! Minimum X-value applicable to spline interpolation
     Real :: y_cor_size ! Size of corrected pixel in metres in Y-direction
     Real :: y_maximum ! Maximum Y-value applicable to spline interpolation
     Real :: y_minimum ! Minimum Y-value applicable to spline interpolation
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(13) ! User messages
     Real, Allocatable :: X_COEFFS(:) ! Dynamic array "X_COEFFS"
     Real, Allocatable :: X_COORDINATES(:) ! Dynamic work array used to
!      store X-coordinates of edges of pixels
     Real, Allocatable :: X_DISTORTION(:, :) ! 2-D X-distortion values
     Real, Allocatable :: X_LAMBDA(:) ! Dynamic array "X_LAMBDA"
     Real, Allocatable :: X_MU(:) ! Dynamic array "X_MU"
     Real, Allocatable :: Y_COEFFS(:) ! Dynamic array "Y_COEFFS"
     Real, Allocatable :: Y_COORDINATES(:) ! Dynamic work array used to
!      store Y-coordinates of edges of pixels
     Real, Allocatable :: Y_DISTORTION(:, :) ! 2-D Y-distortion values 
     Real, Allocatable :: Y_LAMBDA(:) ! Dynamic array "Y_LAMBDA"
     Real, Allocatable :: Y_MU(:) ! Dynamic array "Y_MU"
!  Local Data:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_SRADIAL'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SRADIAL ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0 .Or. max_radial .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xendelm .Gt. xmaxdat .Or. &
       xstrelm .Gt. xendelm) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. yendelm .Gt. ymaxdat .Or. &
       ystrelm .Gt. yendelm) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_SRADIAL ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x/y beam = '', 2g)') x_beam, y_beam
!     Write (*, '(''x/y pixel size (metres) = '', 2g)') &
!       x_pixel_size, y_pixel_size
!     Write (*, '(''radial pixel size (metres) = '', g)') &
!       radial_pixel_size
!     Write (*, '(''rotation, tilt (degrees) = '', 2g)') &
!       rotation*180.0/Pi, tilt*180.0/Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Obtain dynamic memory for spline distortion surfaces
        xmaxknots = 100
        ymaxknots = 100
        Allocate (X_LAMBDA(xmaxknots + 1), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SRADIAL ' // Version)
           Return
        End If
        Allocate (Y_LAMBDA(xmaxknots + 1), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SRADIAL ' // Version)
           Return
        End If
        Allocate (X_MU(ymaxknots + 1), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SRADIAL ' // Version)
           Return
        End If
        Allocate (Y_MU(ymaxknots + 1), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SRADIAL ' // Version)
           Return
        End If
        Allocate (X_COEFFS((xmaxknots - 4) * (ymaxknots - 4)), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SRADIAL ' // Version)
           Return
        End If
        Allocate (Y_COEFFS((xmaxknots - 4) * (ymaxknots - 4)), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SRADIAL ' // Version)
           Return
        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input spatial distortion spline
        Call F2D_IN_SPATIAL (.False., xmaxknots, ymaxknots, file_name, &
          retstat, x_minimum, y_minimum, x_maximum, y_maximum, &
          cor_grid_spacing, x_cor_size, y_cor_size, x_xnumknots, x_ynumknots, &
          X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, &
          y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
 
!     Output message telling user of the corrected pixel sizes
        Write (MESSAGE(1), '(''INFO: The corrected pixel dimension in X ' // &
          'is '', f12.4, '' microns'')') x_cor_size * 1.0e6
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''INFO: The corrected pixel dimension in Y ' // &
          'is '', f12.4, '' microns'')') y_cor_size * 1.0e6
        Call IO_WRITE (MESSAGE(1), status)
        Call IO_WRITE (' ', status)
 
!     Find limits of valid pixel region (may be smaller than required ROI region
        Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, x_minimum, temp, status)
        x_low = Int(temp) + 1
        Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, x_maximum, temp, status)
        x_up = Int(temp)
        Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, y_minimum, temp, status)
        y_low = Int(temp) + 1
        Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, y_maximum, temp, status)
        y_up = Int(temp)
 
!     Set re-binning region to be the lesser of the ROI and the valid region
        If (xstrelm .Lt. x_low .Or. ystrelm .Lt. y_low .Or. xendelm .Gt. x_up &
          .Or. yendelm .Gt. y_up) Then
           Call IO_WRITE ('WARNING: Region Of Interest ' // &
             '(ROI) extends outside valid region of', status)
           Call IO_WRITE ('spline. Pixels outside the ' // &
             'valid region will be ignored.', status)
           Call IO_WRITE ('INFO: The valid region of ' // &
             'the spline correction function is:', status)
           Write (MESSAGE(1), '(''      ('', f8.2, '', '', ' // &
             'f8.2, '') to ('', f8.2, '', '', f8.2, '')'')') x_minimum, &
             y_minimum, x_maximum, y_maximum
           Call IO_WRITE (MESSAGE(1), status)
 
        End If
 
!     Set Intersection of ROI and valid spline region
        x_low = Max(x_low, xstrelm)
        y_low = Max(y_low, ystrelm)
        x_up = Min(x_up, xendelm)
        y_up = Min(y_up, yendelm)
 
!     Overloaded pixel value
        MESSAGE(1) = 'In order to avoid over-loaded pixels ' // &
          'being re-binned and their intensity'
        MESSAGE(2) = 'spread out to an undetermined value, you ' // &
          'can enter a "over-loaded" pixel'
        MESSAGE(3) = 'value. All input pixels which have this ' // &
          'value or more, will cause one or'
        MESSAGE(4) = 'more output pixels to be incremented by ' // &
          'the value regardless of the normal'
        MESSAGE(5) = 'proportional are re-binning algorithm. ' // &
          'Thus over-loaded pixels in the'
        MESSAGE(6) = 'output image can be easily identified ' // &
          'and ignored.'
        MESSAGE(7) = '(This can be turned-off by entering a ' // &
          'very large value.)'
        Call IO_INPR (.True., 0.0, 1.7e38, .True., 'OVER-LOADED PIXEL VALUE', &
          7, MESSAGE, 1, 'Enter a real value within given range', &
          overload_value, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Find out number of rows for which the distortions are to be
!     calculated in one go
        MESSAGE(1) = 'For efficiencies sake many rows of the '// &
          'distortion functions can be calculated'
        MESSAGE(2) = 'in one call to the NAG subroutine which '// &
          'performs the task. However, to store'
        MESSAGE(3) = 'the results extra memory is required. '// &
          '(The memory is allocated automatically'
        MESSAGE(4) = 'from the machines virtual memory and is '// &
          'de-allocated after the spatial'
        MESSAGE(5) = 'distortion correction is finished and the '// &
          'storage is no longer necessary.)'
        MESSAGE(6) = 'Thus, the value entered here choses a '// &
          'compromise between using more memory'
        MESSAGE(7) = 'and taking longer to calculate all the '// &
          'necessary distortion values. If the'
        MESSAGE(8) = 'machine has plenty of available memory '// &
          '(virtual) a larger number such as 100'
        MESSAGE(9) = 'may be more efficient. However, if the '// &
          'machine is short of virtual memory'
        MESSAGE(10) = 'e.g. you have already received an error '// &
          'message that memory allocated has'
        MESSAGE(11) = 'failed, then a smaler value will be '// 'appropriate.'
        MESSAGE(12) = '   NOTE: Too big a number may be ' // &
          'counter-productive as it may cause'
        MESSAGE(13) = 'excessive page faulting.'
        Call IO_INPI (.True., 2, yendelm - ystrelm + 2, .True., &
          'NUMBER OF ROWS OF DISTORTION FUNCTIONS TO CALCULATED '// &
          'IN A BLOCK', 1, MESSAGE, 1, 'Enter an integer within given range', &
          y_rows, status)
 
!     Obtain dynamic array space for internal arrays
        maxwork = xendelm - xstrelm + 1
        Allocate (X_DISTORTION(maxwork, y_rows), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SRADIAL ' // Version)
           Return
        End If
        Allocate (Y_DISTORTION(maxwork, y_rows), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SRADIAL ' // Version)
           Return
        End If
        Allocate (X_COORDINATES(maxwork), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SRADIAL ' // Version)
           Return
        End If
        Allocate (Y_COORDINATES(y_rows), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SRADIAL ' // Version)
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Re-bin 2-D region to 1-D scan, taking into account spatial distortion
        Call F2D_CORR2_RADIAL (x_low, y_low, x_up, y_up, x_cor_size, &
          y_cor_size, x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, &
          X_COEFFS, y_xnumknots, y_ynumknots, Y_LAMBDA, &
          Y_MU, Y_COEFFS, xmaxdat, ymaxdat, xstrelm, ystrelm, &
          xendelm, yendelm, XAXIS, YAXIS, DATA, MASK, overload_value, &
          variances_exist, EXPERIMENT, lorentz_geometry, angular_scan, &
          radial_pixel_size, max_radial, num_radial, maxwork, y_rows, &
          X_COORDINATES, Y_COORDINATES, X_DISTORTION, &
          Y_DISTORTION, RAD_AXIS, PROFILE, PROVARS, NUMPIXELS, status)
 
!     Free dynamic array space
        Deallocate (X_LAMBDA)
        Deallocate (Y_LAMBDA)
        Deallocate (X_MU)
        Deallocate (Y_MU)
        Deallocate (X_COEFFS)
        Deallocate (Y_COEFFS)
        Deallocate (X_DISTORTION)
        Deallocate (Y_DISTORTION)
        Deallocate (X_COORDINATES)
        Deallocate (Y_COORDINATES)
 
     End If
 
     End Subroutine F2D_SRADIAL
!********1*********2*********3*********4*********5*********6*********7**
 
 
 
