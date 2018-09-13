!********1*********2*********3*********4*********5*********6*********7**
 
!  *****************
!  *               *
!  * f2d_print.f90 *
!  *               *
!  *****************
 
!+ F2D_PRINT - FIT 2-D: PRINT data
     Subroutine F2D_PRINT (gui, print_type, mask_data, xmaxdat, ymaxdat, &
       X_AXIS, Y_AXIS, DATA, VARIANCES, MASK, title, xlabel, ylabel, zlabel, &
       variances_exist, xstrelm, ystrelm, xendelm, yendelm, status)
!  Description:
!    "Prints" the data to the hardcopy device acording to the specified print 
!    type.
!  Keywords:
!    Print~Graphics, Graphics~Print
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    12-Dec-2003: V0.10 Use "GS_FPROMPT" (Hammersley)
!    22-Jan-1998: V0.9 Add agruments to provide option of outputting
!      a masked image (Hammersley)
!    05-Jan-1997: V0.8 Draw banner with GUI buttons (Hammersley)
!    11-Nov-1996: V0.7 Add option of "3-D lines" output (Hammersley)
!    16-Apr-1996: V0.6 Close graphics file immediately for GUI (Hammersley)
!    19-Mar-1996: V0.5 Allow output and control of 1-D X/Y graphs (Hammersley)
!    11-Sep-1995: V0.4 Choice of graphical interface or keyboard interface 
!      (Hammersley)
!    07-Sep-1995: V0.3 Do not try to print if the PostScript file
!      is not open (Hammersley)
!    05-Jul-1995: V0.2 3-D surface option (Hammersley)
!    22-Jun-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic 'status' constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Logical, Intent(IN) :: gui ! .True., if the graphics window is to be
!      used for user prompts, messages and input
     Character(Len = *), Intent(IN) :: print_type ! Type, of graphics to
!      print: Supported types are:
!        "3-d lines"
!        "3-d surface"
!        "banner"
!        "image"
!        "contour"
!        "masked_image"
!        "x/y graph"
     Logical, Intent(IN) :: mask_data ! .True., if the mask is to be used to
!      mask off pixels in the image display
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! The estimated
!      variance values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! .True., if a pixel is
!      masked-off
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Logical, Intent(IN) :: variances_exist ! .True., if the variances exist
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.10' ! Version number
!  Local Variables:
     Logical :: file_open ! .True., if the graphics output file is open
!  Local Arrays:
!  External Functions:
     Integer, External :: St_errorcode
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PRINT ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_PRINT ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Arguments would appear to be reasonable, go ahead.

!     Deactive terminal workstation and activate hardcopy workstation
        Call GS_ON_PRINT (gui, file_open, status)
 
        If (file_open) Then
 
           If (print_type .Eq. '3-d lines') Then
 
!           Draw 3-d line view
              Call GS_3DLINES (.True., xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, &
                xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
                zlabel, status)
 
           Else If (print_type .Eq. '3-d surface') Then
 
!           Draw 3-d surface
              Call GS_3DSURFACE (xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, &
                xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
                zlabel, status)
 
           Else If (print_type .Eq. 'banner') Then
 
!           Draw fit2d banner
              Call F2D_DRAW_BANNER ('V*.**', .True., status)
 
           Else If (print_type .Eq. 'image') Then
 
!           Draw image to printer
              Call GS_PLOT (xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, xstrelm, &
                ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
                status)
 
           Else If (print_type .Eq. 'masked_image') Then
 
!           Draw image with data mask to printer
              Call GS_MPLOT (mask_data, xmaxdat, ymaxdat, DATA, MASK, X_AXIS, &
                Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                ylabel, zlabel, status)
 
           Else If (print_type .Eq. 'contour') Then
 
!           Draw contour plot
              Call GS_2DCONTOUR (xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, &
                xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
                zlabel, status)
 
           Else If (print_type .Eq. 'x/y graph') Then
 
!           Plot X-Y graph
              Call F2D_XYGRAPH (.False., xmaxdat, ymaxdat, xstrelm, ystrelm, &
                xendelm, yendelm, variances_exist, DATA, VARIANCES, X_AXIS, &
                Y_AXIS, title, xlabel, ylabel, zlabel, status)
 
           Else
 
              Call IO_WRITE ('WARNING: Unknown type of ' // &
                'graphics to output', status)
 
           End If
 
!        Deactive hardcopy workstation and activate terminal
!        workstation
           Call GS_OFF_PRINT (status)
 
           If (gui) Then
 
!           Close graphics file immediately (only one page per file)
              Call GS_CLOSE_PS (status)
 
!           Output user message
              Call GS_FPROMPT (1, 1, 'FINISHED WRITING FILE', status)
              Call GS_UPDATE (status)
 
           End If
 
           Call IO_WRITE ( 'INFO: Finished writing graphics file', status)
 
        End If
 
!     Check for bad file open status
        If (St_errorcode(status) .Eq. St_bad_openfile) Then
 
!        Re-set status
           Call ST_DEF_SYSTEM (status)
 
           If (gui) Then
 
!           Output warning message
              Call GS_FWARNING ( 1, 1, &
                'PROBLEM WRITING FILE: (SEE TERMINAL WINDOW)', status)
 
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_PRINT
!********1*********2*********3*********4*********5*********6*********7**
 
 
 
 
