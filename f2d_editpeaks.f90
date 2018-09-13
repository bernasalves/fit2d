!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_editpeaks.f90 *
!  *                   *
!  *********************
 
!+ F2D_EDITPEAKS - FIT 2-D EDIT PEAKS in list
     Subroutine F2D_EDITPEAKS (experiment, draw_bad_weak, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, MASK, XAXIS, YAXIS, xstrelm, &
       ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, max_peaks, &
       num_peaks, PEAKS, status)
!  Description:
!    Interactive addition or removal of peaks.
!  Keywords:
!    Peak~Search.Edit, Edit.Peak~Search
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Nov-2014: V0.9 Changes to "F2D_CLICK" (Hammersley)
!    15-Nov-2006: V0.8 Use "PEAK_STRUCTURE" to hold results (Hammersley)
!    17-Mar-2006: V0.7 Support for arbitrary aspect ratio windows (Hammersley)
!    13-Mar-2006: V0.6 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.5 Alter menu lay-out for landscape windows (Hammersley)
!    02-Jun-2003: V0.4 Tidy-up code (Hammersley)
!    26-Jun-1998: V0.3 Add peaks D-spacings array (Hammersley)
!    10-Mar-1997: V0.2 Change "F2D_GUI_ZSCALE" to cope with masked data 
!      (Hammersley)
!    09-Mar-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Logical, Intent(IN) :: draw_bad_weak ! .True., if bad but non-saturated
!      "peaks" are to be drawn
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xnumdat ! Number of defined pixels in the
!      X-direction
     Integer, Intent(IN) :: ynumdat ! Number of defined pixels in the
!      Y-direction
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Masked-off elements
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Integer, Intent(IN) :: max_peaks ! Dimension of peak arrays
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: xendelm ! The X-pixel number for the end of
!      the ROI
     Integer, Intent(INOUT) :: yendelm ! The Y-pixel number for the end of
!      the ROI
     Integer, Intent(INOUT) :: num_peaks ! Total peaks found, including
!      problem peaks
     Type(PEAK_STRUCTURE), Intent(INOUT) :: PEAKS(max_peaks) ! Peaks array
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.9' ! Version number
     Integer, Parameter :: Max_Coordinates = 200 ! Maximum number of
!      coordinates which can be defined
     Integer, Parameter :: Max_menu = 9 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Integer :: closest_peak ! Index of peak closest to removal coordinate
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_coordinates ! Number of coordinates entered by the user
     Integer :: num_menu ! Number of choices in menu
     Integer :: peak ! Loop variable for peaks
     Logical :: continue ! .True., until user wants to exit
     Logical :: geometry_defined ! .True.,  if geometry is defined
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Logical :: update_peaks ! .True., if the peak list needs to be redrawn
     Real :: angle ! Angle of a reflection
     Real :: distance_sq ! Squared distance between coordinate
!      of peak to remove and peak in the peak list
     Real :: min_distance ! Minimum squared distance between coordinate
!      of peak to remove and peak in the peak list
     Real :: radial_distance ! Distance from spot to beam centre radially
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_beam ! X-coordinate of beam in pixels
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_beam ! Y-coordinate of beam in pixels
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 16), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanation
     Character(Len = 80) :: MESSAGE(1) ! User messages
     Real :: X_COORDINATES(Max_Coordinates) ! X-coordinates of "peaks"
!      to add or remove
     Real :: Y_COORDINATES(Max_Coordinates) ! Y-coordinates of "peaks"
!      to add or remove
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 9) / 'EXIT', '?', 'HELP', 'ZOOM IN', &
       'UN-ZOOM', 'FULL UN-ZOOM', 'ADD PEAKS', 'REMOVE PEAKS', 'Z-SCALING' /
     Data (MENUTXT(item), item = 1, 9) / 'EXIT: Exit from sub-menu', &
       '?: Display list of commands with short description', &
       'HELP: Scrolled display of help text', &
       'ZOOM IN: Graphical region definition', &
       'UN-ZOOM: Make region displayed bigger', &
       'FULL UN-ZOOM: View whole of data image', &
       'ADD PEAKS: Add peaks to the peak list by graphical input', &
       'REMOVE PEAKS: Remove peaks from the list by graphical input', &
       'Z-SCALING: Automatic or user control of intensity display range' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_EDITPEAKS ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0 ) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_EDITPEAKS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Inquire window format
        Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!     Set menu layout style
        num_menu = Max_menu
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputing menu commands until requested to stop
 
        continue = .True.
        update_image = .False.
        update_peaks = .False.
        update_menu = .True.
        Do While (continue)
 
           If (update_image) Then
 
!           Draw image plot of data
              Call GS_MPLOT (.True., xmaxdat, ymaxdat, DATA, MASK, XAXIS, &
                YAXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                ylabel, zlabel, status)
              update_menu = .True.
              update_peaks = .True.
 
           End If
 
           If (update_peaks .And. num_peaks .Gt. 0) Then
 
              Call F2D_DRAW_PEAKS (draw_bad_weak, max_peaks, num_peaks, &
                PEAKS, status)
 
           End If
 
           If (update_menu) Then
 
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 13, 17, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_vertical, 3, 13, 17, status)
              End If
 
!           Draw menu
              Call GS_FMENU (1, 1, 'EDIT PEAK LIST', Max_menu, num_menu, MENU, &
                status)
 
           End If
 
!        By default no update
           update_image = .False.
           update_peaks = .False.
           update_menu = .False.
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 2, input_type, &
             command, x_coordinate, y_coordinate, status)
 
           If (input_type .Eq. Gs_resize) Then
 
!           Set update
              update_image = .True.
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Output the coordinate position and intensity
              Call F2D_CLICK (1, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, XAXIS, YAXIS, DATA, title, xlabel, ylabel, zlabel, &
                x_coordinate, y_coordinate, experiment, &
                update_image, update_menu, status)
 
           Else If (input_type .Eq. Gs_choice) Then
 
!           Carry out menu choices
              If (command .Eq. 'null') Then
 
                 Continue
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
                 update_image = .True.
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text
                 Call F2D_EDITPEAKSHELP (.True., status)
                 update_image = .True.
 
              Else If (command .Eq. 'ADD PEAKS') Then
 
!              Add peaks to peak list
                 num_coordinates = 0
                 MESSAGE(1) = 'Click on coordinates for new peaks'
                 Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, &
                   xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, XAXIS, &
                   YAXIS, title, xlabel, ylabel, zlabel, &
                   'CLICK ON PEAK CENTRES TO ADD', 1, MESSAGE, .False., &
                   Max_Coordinates, num_coordinates, X_COORDINATES, &
                   Y_COORDINATES, status)
 
                 If (status .Eq. St_escapevalue) Then
                    status = St_goodvalue
                 Else
 
                    If (num_peaks + num_coordinates .Gt. max_peaks) Then
 
!                    Too many peaks
                       Call GS_FWARNING ( 1, 1, &
                         'PEAK LIST FULL: NO MORE PEAKS CAN BE STORED', &
                         status)
                       num_coordinates = max_peaks - num_peaks
                    End If
 
                    Do peak = 1, num_coordinates
 
                       PEAKS(num_peaks + peak)%x_centre = X_COORDINATES(peak)
                       PEAKS(num_peaks + peak)%y_centre = Y_COORDINATES(peak)
                       PEAKS(num_peaks + peak)%status = 0
 
                       If (geometry_defined) Then
 
!                       Calculate d-spacing of peak
                          radial_distance = Sqrt( ((X_COORDINATES(peak) - &
                            experiment%x_beam) * experiment%x_pixel_size)**2 + &
                            ((Y_COORDINATES(peak) &
                            - experiment%y_beam) * experiment%y_pixel_size)**2)
                          angle = 0.5 * Atan(radial_distance / &
                            experiment%detector_distance)
                          PEAKS(num_peaks + peak)%d_spacing = &
                            (experiment%wavelength / &
                            (2.0 * Sin(angle))) * 1.0e10
                       Else
 
!                       Set obviously false value
                          PEAKS(num_peaks + peak)%d_spacing = -1.0
 
                       End If
 
                    End Do
                    num_peaks = num_peaks + num_coordinates
 
                    update_peaks = .True.
 
                 End If
 
                 update_menu = .True.
 
              Else If (command .Eq. 'REMOVE PEAKS') Then
 
!              Remove peaks from the peak list
                 num_coordinates = 0
                 MESSAGE(1) = 'Click on peaks to remove from list'
                 Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, &
                   xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, XAXIS, &
                   YAXIS, title, xlabel, ylabel, zlabel, &
                   'CLICK ON PEAK CENTRES TO REMOVE', 1, MESSAGE, .False., &
                   Max_Coordinates, num_coordinates, X_COORDINATES, &
                   Y_COORDINATES, status)
 
                 If (status .Eq. St_escapevalue) Then
                    status = St_goodvalue
                 Else
 
                    Do coordinate = 1, num_coordinates
 
!                    Find closest peak
                       min_distance = 1.7e38
                       closest_peak = -1
                       Do peak = 1, num_peaks
 
                          If (PEAKS(peak)%status .Eq. 0) Then
 
                             distance_sq = (X_COORDINATES(coordinate) - &
                               PEAKS(peak)%x)**2 + (Y_COORDINATES(coordinate) &
                               - PEAKS(peak)%y)**2
 
                             If (distance_sq .Lt. min_distance) Then
                                min_distance = distance_sq
                                closest_peak = peak
                             End If
 
                          End If
 
                       End Do
 
                       If (closest_peak .Ge. 1) Then
 
!                       Remove peak from list
                          Do peak = closest_peak, num_peaks - 1
                             PEAKS(peak) = PEAKS(peak + 1)
                          End Do
                          num_peaks = num_peaks - 1
 
                       End If
 
                    End Do
                    update_image = .True.
 
                 End If
 
                 update_menu = .True.
 
              Else If (command .Eq. 'FULL UN-ZOOM') Then
 
!              Set ROI to cover all data
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
                 update_image = .True.
 
              Else If (command .Eq. 'UN-ZOOM') Then
 
!              Increase size of 1-D region or ROI
                 Call GS_INP_UNZOOM (xnumdat, ynumdat, xstrelm, ystrelm, &
                   xendelm, yendelm, status)
 
                 update_image = .True.
 
              Else If (command .Eq. 'Z-SCALING') Then
 
!              User defined Z-scaling mode
                 Call F2D_GUI_ZSCALE (.True., xmaxdat, ymaxdat, xnumdat, &
                   ynumdat, XAXIS, YAXIS, DATA, MASK, title, xlabel, ylabel, &
                   zlabel, xstrelm, ystrelm, xendelm, yendelm, &
                   experiment, .False., x_coordinate, y_coordinate, status)
                 update_image = .False.
                 update_peaks = .True.
                 update_menu = .True.
 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two points
                 Call F2D_ZOOMIN (xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, &
                   YAXIS, DATA, title, xlabel, ylabel, zlabel, xstrelm, &
                   ystrelm, xendelm, yendelm, status)
 
                 update_image = .True.
 
              End If
 
           End If
 
!        Check status
           If (status .Eq. St_escapevalue) Then
 
!           Reset status system
              Call ST_DEF_SYSTEM (status)
 
           Else If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
     End If
 
     End Subroutine F2D_EDITPEAKS
!********1*********2*********3*********4*********5*********6*********7*********8
 
