!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_gui_correction.f90 *
!  *                        *
!  **************************
 
!+ F2D_GUI_CORRECTION - FIT 2-D GUI Spatial distortion CORRECTION
     Subroutine F2D_GUI_CORRECTION (input_options, data_defined, memory_exist, &
       memory_defined, variances_exist, xmaxdat, ymaxdat, title, xlabel, &
       ylabel, zlabel, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
       x_pixel_size, y_pixel_size, mtitle, mxlabel, mylabel, mzlabel, &
       mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, &
       mx_pixel_size, my_pixel_size, experiment, status)
!  Description:
!    FIT2D GUI spatial distortion correction
!  Keywords:
!    Distortion.Spatial.Correction, Correction.Distortion.Correction,
!    Spatial~Distortion.Correction
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    25-Apr-2006: V0.14 Use Fortran-90 dynamically allocated arrays (Hammersley)
!    24-Apr-2006: V0.13 Add "input_options" and "experiment" structures 
!      (Hammersley)
!    07-Apr-2004: V0.12 Add option to correct dark current (Hammersley)
!    23-Feb-1999: V0.11 All data-base saving and recovering routines renamed
!      (Hammersley)
!    17-Feb-1999: V0.10 "F2D_GUI_INPUT" replaced by "FIO_GUI_INPUT" (Hammersley)
!    15-Dec-1998: V0.9 Change to use IO internal database routines (Hammersley)
!    11-Mar-1998: V0.8 Increase number of rows of distortion function
!      calculated together (Hammersley)
!    25-Feb-1998: V0.7 Return if "CANCEL" issued from correction form 
!      (Hammersley)
!    22-Feb-1998: V0.6 Investigating problem caused by this routine. Incorrect 
!      calls to data store corrected (Hammersley)
!    29-Jan-1998: V0.5 Automatic input of flat-field image (Hammersley)
!    27-Jan-1998: V0.4 Add optional GUI menu (Hammersley)
!    22-Aug-1996: V0.3 Change to "F2D_CORR_SPATIAL" (Hammersley)
!    29-Feb-1996: V0.2 Try to obtain spline file name from internal data-base 
!      (Hammersley)
!    26-Feb-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined ! .True., if data exists within
!      the program
     Logical, Intent(INOUT) :: memory_exist ! .True. if memory array exists
     Logical, Intent(INOUT) :: memory_defined ! .True. if the memory contains 
!      data
     Logical, Intent(INOUT) :: variances_exist ! .True., if a data variance
!      array is created
     Integer, Intent(INOUT) :: xmaxdat ! Dimension size in X-direction for
!      data arrays
     Integer, Intent(INOUT) :: ymaxdat ! Dimension size in Y-direction for
!      data arrays
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements defined in
!      Y-direction
!    Real X_AXIS(xmaxdat) ! X-axis values
!    Real Y_AXIS(ymaxdat) ! Y-axis values
!    Real DATA(xmaxdat, ymaxdat) ! The data values
!    Real VARIANCES(xmaxdat, ymaxdat) ! The estimated variance values
     Character(Len = *), Intent(INOUT) :: title ! Title for plot
     Character(Len = *), Intent(INOUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: zlabel ! Z-axis label for plot
     Real, Intent(INOUT) :: x_pixel_size ! Size of one unit (raw pixel) in
!      X-direction (metres)
     Real, Intent(INOUT) :: y_pixel_size ! Size of one unit (raw pixel) in
!      Y-direction (metres)
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
!  Export:
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
!    Real MX_AXIS(xmaxdat) ! Array containing data X-coordinates
!    Real MY_AXIS(ymaxdat) ! Array containing data Y-coordinates
!    Real MDATA(xmaxdat, ymaxdat) ! Array containing data to be fitted
!    Real MVARIANCES(xmaxdat, ymaxdat) ! Array containing variances in
!    the data values
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Real, Intent(OUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(OUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.14' ! Version number
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
     Integer, Parameter :: Max_prompt = 6 ! Dimension size of prompts
!  Local Variables:
     Logical, Save :: dc_correction = .False. ! .True., if a dark current
!      image is to be subtracted
     Character(Len = 256), Save :: dc_file = 'dark_current.bin'
!      Name of the dark current file
     Logical, Save :: ff_correction = .False. ! .True., if a flat-field
!      correction is to be applied
     Character(Len = 256), Save :: ff_file = 'flat_field.bin'
!      Name of flat-field file
     Logical, Save :: ff_scale = .False. ! .True., if the flat-field
!      image is to scaled
     Character(Len = 256), Save :: sd_file = 'spatial.spline'
!      Name of spatial distiortion file
     Integer :: db_stat ! Return status from "IO_INQ_*KEYVALUE"
     Integer :: len_string ! Defined length of string
     Integer :: retstat ! Return status variable:
!      0 = good status
     Integer stat ! Status return variable for "Allocate"
     Integer :: x_xnumknots ! Number of X-axis "knot" positions for
!      X-distortion spline function
     Integer :: x_ynumknots ! Number of Y-axis "knot" positions for
!      X-distortion spline function
     Integer :: xmaxknots ! Maximum number of knot points in X-direction
     Integer :: y_xnumknots ! Number of X-axis "knot" positions for
!      Y-distortion spline function
     Integer :: y_ynumknots ! Number of Y-axis "knot" positions for
!    Y-distortion spline function
     Integer :: ymaxknots ! Maximum number of knot points in Y-direction
     Logical, Save :: sd_correction = .False. ! .True., if a spatial
!      distortion correction is to be applied
     Real :: cor_grid_spacing ! Grid spacing for correction function
     Real, Save :: ff_scaler = 1000.0 ! Scaling to multiple flat-field
!      image with
     Real :: overload_value ! Value above which pixels are considered to
!      be over-loaded
     Real :: x_cor_size ! Size of corrected pixel in metres in X-direction
     Real :: x_maximum ! Maximum X-value applicable to spline interpolation
     Real :: x_minimum ! Minimum X-value applicable to spline interpolation
     Real :: y_cor_size ! Size of corrected pixel in metres in X-direction
     Real :: y_maximum ! Maximum Y-value applicable to spline interpolation
     Real :: y_minimum ! Minimum Y-value applicable to spline interpolation
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
     Real, Allocatable :: X_COEFFS(:) ! Pointer to dynamic array "X_COEFFS"
     Real, Allocatable :: X_LAMBDA(:) ! Pointer to dynamic array "X_LAMBDA"
     Real, Allocatable :: X_MU(:) ! Pointer to dynamic array "X_MU"
     Real, Allocatable :: Y_COEFFS(:) ! Pointer to dynamic array "Y_COEFFS"
     Real, Allocatable :: Y_LAMBDA(:) ! Pointer to dynamic array "Y_LAMBDA"
     Real, Allocatable :: Y_MU(:) ! Pointer to dynamic array "Y_MU"
!  External Functions:
!  Local Data:
!    Save:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_CORRECTION ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_GUI_CORRECTION ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_GUI_CORRECTION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Arguments would appear to be reasonable, go ahead.
 
!     Obtain overloaded intensity value from internal data-base
        Call IO_INQ_RKEYVALUE ('#OVERLOAD_VALUE', overload_value, db_stat, &
          status)
 
        If (db_stat .Ne. 0) Then
           overload_value = 100000.0
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Try to obtain name of spline file from the data-store
        Call IO_INQ_LKEYVALUE ('DARK_CURRENT_CORRECTION', dc_correction, &
          db_stat, status)
        Call IO_INQ_KEYVALUE ('DARK_CURRENT_FILE', len_string, dc_file, &
          db_stat, status)
        Call IO_INQ_LKEYVALUE ('FF_CORRECTION', ff_correction, db_stat, &
          status)
        Call IO_INQ_KEYVALUE ('FF_FILE', len_string, ff_file, db_stat, status)
        Call IO_INQ_LKEYVALUE ('FF_SCALE', ff_scale, db_stat, status)
        Call IO_INQ_RKEYVALUE ('FF_SCALER', ff_scaler, db_stat, status)
        Call IO_INQ_LKEYVALUE ('SD_CORRECTION', sd_correction, db_stat, &
          status)
        Call IO_INQ_KEYVALUE ('SD_FILE', len_string, sd_file, db_stat, status)
 
!     Allow user of set detector distortion control parameters
        Call F2D_INP_CORRECTION (dc_correction, dc_file, ff_correction, &
          ff_file, ff_scale, ff_scaler, sd_correction, sd_file, status)
 
!     Check return status
        If (status .Ne. St_goodvalue) Then
 
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
           End If
 
           Return
 
        End If
 
!     Save set-up in internal data-store
        Call IO_SET_LKEYVALUE ('DARK_CURRENT_CORRECTION', dc_correction, &
          db_stat, status)
        len_string = Len_trim(dc_file)
        Call IO_SET_KEYVALUE ('DARK_CURRENT_FILE', len_string, dc_file, &
          db_stat, status)
        Call IO_SET_LKEYVALUE ('FF_CORRECTION', ff_correction, db_stat, status)
        len_string = Len_trim(ff_file)
        Call IO_SET_KEYVALUE ('FF_FILE', len_string, ff_file, db_stat, status)
        Call IO_SET_LKEYVALUE ('FF_SCALE', ff_scale, db_stat, status)
        Call IO_SET_RKEYVALUE ('FF_SCALER', ff_scaler, db_stat, status)
        Call IO_SET_LKEYVALUE ('SD_CORRECTION', sd_correction, db_stat, status)
        len_string = Len_trim(sd_file)
        Call IO_SET_KEYVALUE ('SD_FILE', len_string, sd_file, db_stat, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        If (dc_correction) Then
 
           PROMPT(1) = 'INPUT OF DARK CURRENT FROM FILE:'
           PROMPT(2) = ' '
           PROMPT(3) = dc_file
           PROMPT(4) = ' '
           PROMPT(5) = ' '
           PROMPT(6) = ' '
           Call GS_FPROMPT (6, 6, PROMPT, status)
           Call GS_UPDATE (status)
           Call IO_SLEEP (2.0, status)
 
           PROMPT(1) = 'SELECT DARK CURRENT FILE'
           PROMPT(2) = '(click on "HELP" for list of formats)'
           memory_defined = .False.
           Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, -2147483647, &
             input_options, xmaxdat, &
             ymaxdat, variances_exist, memory_defined, dc_file, xnumdat, &
             ynumdat, %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), &
             %val(pMVARIANCES), mtitle, mxlabel, mylabel, mzlabel, &
             experiment, status)
 
           If (memory_defined) Then
 
              Call GS_BACKGROUND (status)
              Call GS_FPROMPT (1, 1, 'SUBTRACTING DARK CURRENT', status)
              Call GS_UPDATE (status)
 
!           Subtract dark current from data
              Call MA_RSUBTRACT (xmaxdat, ymaxdat, 1, 1, xnumdat, ynumdat, &
                %val(pMDATA), xmaxdat, ymaxdat, %val(pDATA), status)
 
           End If
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        If (ff_correction) Then
 
           PROMPT(1) = 'INPUT OF FLAT-FIELD FROM FILE:'
           PROMPT(2) = ' '
           PROMPT(3) = ff_file
           PROMPT(4) = ' '
           PROMPT(5) = ' '
           PROMPT(6) = ' '
           Call GS_FPROMPT (6, 6, PROMPT, status)
           Call GS_UPDATE (status)
           Call IO_SLEEP (3.0, status)
 
           PROMPT(1) = 'SELECT FLAT FIELD FILE'
           PROMPT(2) = '(click on "HELP" for list of formats)'
           memory_defined = .False.
           Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, -2147483647, &
             input_options, xmaxdat, &
             ymaxdat, variances_exist, memory_defined, ff_file, xnumdat, &
             ynumdat, %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), &
             %val(pMVARIANCES), mtitle, mxlabel, mylabel, mzlabel, &
             experiment, status)
 
           If (memory_defined) Then
 
              Call GS_BACKGROUND (status)
              Call GS_FPROMPT (1, 1, 'APPLYING FLAT-FIELD', status)
              Call GS_UPDATE (status)
 
!           Divide data by flat-field image
              Call MA_RDIVIDE (xmaxdat, ymaxdat, 1, 1, xnumdat, ynumdat, &
                %val(pMDATA), xmaxdat, ymaxdat, %val(pDATA), status)
 
              If (ff_scale) Then
 
                 Call GS_FPROMPT (1, 1, 'APPLYING SCALE FACTOR', status)
 
!              Force output
                 Call GS_UPDATE (status)
 
                 Call MA_RCMULT (xmaxdat, ymaxdat, 1, 1, xnumdat, ynumdat, &
                   ff_scaler, %val(pDATA), status)
 
              End If
 
           End If
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        If (sd_correction) Then
 
!        Get dynamic memory for spline coefficient arrays
           xmaxknots = 100
           ymaxknots = 100
           Allocate (X_LAMBDA(xmaxknots), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_GUI_CORRECTION ' // Version)
              Return
           End If
           Allocate (Y_LAMBDA(xmaxknots), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_GUI_CORRECTION ' // Version)
              Return
           End If
           Allocate (X_MU(ymaxknots), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_GUI_CORRECTION ' // Version)
              Return
           End If
           Allocate (Y_MU(ymaxknots), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_GUI_CORRECTION ' // Version)
              Return
           End If
           Allocate (X_COEFFS((xmaxknots - 4) * (ymaxknots - 4)), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_GUI_CORRECTION ' // Version)
              Return
           End If
           Allocate (Y_COEFFS((xmaxknots - 4) * (ymaxknots - 4)), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_GUI_CORRECTION ' // Version)
              Return
           End If
 
           Call GS_FPROMPT (1, 1, 'INPUTTING SPATIAL DISTORTION FUNCTION', &
             status)
           Call GS_UPDATE (status)
 
!        Input values of spline coefficients from file
           Call F2D_IN_SPATIAL (.True., xmaxknots, ymaxknots, sd_file, &
             retstat, x_minimum, y_minimum, x_maximum, y_maximum, &
             cor_grid_spacing, x_cor_size, y_cor_size, x_xnumknots, &
             x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
             y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
 
!        Check status
           If (retstat .Eq. 1) Then
 
!           Too many coefficients for program arrays
              Call GS_FWARNING ( 1, 1, 'TOO MANY COEFFICIENTS', status)
              Return
 
           Else If (retstat .Eq. 2) Then
 
!           Dynamic memory allocation failed
              Call GS_FWARNING ( 1, 1, 'WRONG TYPE OF FILE', status)
              Return
 
           Else If (retstat .Eq. 3) Then
 
!           Dynamic memory allocation failed
              Call GS_FWARNING ( 1, 1, 'PROBLEM OPENNING FILE', status)
              Return
           End If
 
!        Output user message
           MESSAGE(1) = 'CALCULATING CORRECTION TRANSFORMATION'
           MESSAGE(2) = '(ORIGINAL DATA TRANSFERRED TO THE MEMORY)'
           Call GS_FPROMPT (2, 2, MESSAGE, status)
 
!        Force output
           Call GS_UPDATE (status)
 
!        Apply spatial distortion correction functions to data
           Call F2D_CORR_SPATIAL (.False., .True., x_minimum, y_minimum, &
             x_maximum, y_maximum, x_cor_size, y_cor_size, x_xnumknots, &
             x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
             y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, &
             xmaxdat, ymaxdat, %val(pXAXIS), %val(pYAXIS), &
             %val(pDATA), xstrelm, ystrelm, xendelm, yendelm, 160, &
             overload_value, %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), &
             mxstrelm, mystrelm, mxendelm, myendelm, status)
           mxnumdat = Max(mxnumdat, mxendelm)
           mynumdat = Max(mynumdat, myendelm)
           memory_defined = .True.
           mtitle = title
           mxlabel = xlabel
           mylabel = ylabel
           mzlabel = zlabel
           mx_pixel_size = x_cor_size
           my_pixel_size = y_cor_size
 
!        Swap current data with "memory"
           Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
             xlabel, ylabel, zlabel, variances_exist, data_defined, &
             x_pixel_size, y_pixel_size, xstrelm, ystrelm, xendelm, yendelm, &
             mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, &
             mtitle, mxlabel, mylabel, mzlabel, memory_defined, mx_pixel_size, &
             my_pixel_size, status)
 
!        Output user message
           Call GS_FPROMPT (1, 1, 'CORRECTION COMPLETE', status)
           Call GS_UPDATE (status)
 
!        Free array space
           Deallocate (X_LAMBDA)
           Deallocate (Y_LAMBDA)
           Deallocate (X_MU)
           Deallocate (Y_MU)
           Deallocate (X_COEFFS)
           Deallocate (Y_COEFFS)
 
        End If
 
     End If

     mx_pixel_size = experiment%x_pixel_size
     my_pixel_size = experiment%y_pixel_size
      
     End Subroutine F2D_GUI_CORRECTION
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
