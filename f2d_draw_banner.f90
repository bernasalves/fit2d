!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_draw_banner.f90 *
!  *                     *
!  ***********************
 
!+ F2D_DRAW_BANNER - FIT2D: DRAWs graphical BANNER
     Subroutine F2D_DRAW_BANNER (fit2d_version, gui, status)
!  Description:
!    Creates fit2d graphical banner and outputs it
!  Keywords:
!    Banner.Graphical
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    05-Jan-2015: V0.27 2015 edition! (Hammersley)
!    04-Dec-2014: V0.26 Add "Big Data Crusher" text (Hammersley)
!    14-Jan-2014: V0.25 2014 edition! (Hammersley)
!    18-Feb-2013: V0.24 2013 edition! (Hammersley)
!    05-Jan-2011: V0.23 2011 edition! (Hammersley)
!    05-Jan-2010: V0.22 2010 edition! (Hammersley)
!    12-Jan-2009: V0.21 2009 edition! (Hammersley)
!    07-Jan-2008: V0.20 2008 edition! (Hammersley)
!    14-May-2007: V0.19 Always update graphics at end of drawing (Hammersley)
!    22-Mar-2006: V0.18 Calculate logo drawing size from window aspect ratio
!      (Hammersley)
!    17-Mar-2006: V0.17 Cope with arbitrary aspect ratio windows (Hammersley)
!    16-Mar-2006: V0.16 Add text on text window (Hammersley)
!    10-Feb-2006: V0.15 Change years on copyright (Hammersley)
!    18-Feb-2003: V0.14 Change years on copyright (Hammersley)
!    10-Apr-2001: V0.13 Change years on copyright (Hammersley)
!    13-Nov-2000: V0.12 Comment out obsolete code (Hammersley)
!    26-May-2000: V0.11 Remove key protection (Hammersley)
!    25-Feb-1999: V0.10 Use "GS_DRAW_CHARACTER" to output banner characters 
!      (Hammersley)
!    05-Jan-1999: V0.9 Reduce character size slightly to improve output with 
!      Lan Workplace Pro (Hammersley)
!    25-May-1998: V0.8 Update copyright dates (Hammersley)
!    05-Jan-1997: V0.7 Change text for GUI, for "conditions of use" (Hammersley)
!    01-Apr-1996: V0.6 Add "CONDITIONS" and "HELP" buttons, and use
!      "menu" structure to draw buttons (Hammersley)
!    30-Jan-1996: V0.5 Reduce text height slightly (Hammersley)
!    21-Jan-1996: V0.4 Rename from "F2D_BANNER" (Hammersley)
!    16-Jan-1996: V0.3 Parameterise for variable aspect ratio screen 
!      (Hammersley)
!    20-Jun-1995: V0.2 Change to GS graphics library (Hammersley)
!    23-Feb-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Error status return variables
     Include 'gs_constants.inc' ! Graphics colours
     Include 'gs_menu.inc' ! Menu positions data-base
     Include 'gs_database.inc' ! Graphics data-base
!  Import:
     Character(Len = *), Intent(IN) :: fit2d_version ! Version of FIT2D
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is active
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.27' ! Version number
!  Local Variables:
     Integer :: item ! Loop variable
     Real :: gap ! Gap between edge and text
     Real :: height ! Height of banner
     Real :: height_fit2d ! Width of FIT2D logo
     Real :: height_multipler ! Multipler for height of characters
     Real :: line_spacing ! Distance between text lines
     Real :: th_gap ! Horizontal gap between inner and outer triangle
     Real :: tv_gap ! Horizontal gap between inner and outer triangle
     Real :: width ! Width of banner
     Real :: width_fit2d ! Width of FIT2D logo
     Real :: x_bdc ! X-position for "Big data Crusher" text
     Real :: x_scale ! Scaling to apply to X-dimensions
     Real :: x_start ! Translation to apply to X-dimensions
     Real :: xmax_banner ! X-maximum position for banner output
     Real :: xmax_box ! X-maximum position for box output
     Real :: xmax_triangle ! X-maximum position for triangle output
     Real :: xmin_banner ! X-minimum position for banner output
     Real :: xmin_box ! X-minimum position for box output
     Real :: xmin_triangle ! X-minimum position for triangle output
     Real :: y_bdc ! Y-position for "Big data Crusher" text
     Real :: y_scale ! Scaling to apply to Y-dimensions
     Real :: y_start ! Translation to apply to Y-dimensions
     Real :: y_text ! Y-coordinate for text output
     Real :: ymax_banner ! Y-maximum position for banner output
     Real :: ymax_box ! Y-maximum position for box output
     Real :: ymax_triangle ! Y-maximum position for triangle output
     Real :: ymin_banner ! Y-minimum position for banner output
     Real :: ymin_box ! Y-minimum position for box output
     Real :: ymin_triangle ! Y-minimum position for triangle output
     Real :: xmax_menu ! X-maximum position for output of menu
     Real :: xmin_menu ! X-minimum position for output of menu
     Real :: ymax_menu ! Y-maximum position for output of menu
     Real :: ymin_menu ! Y-minimum position for output of menu
!  Local Arrays:
     Character(Len = 15) :: MENU(4) ! Short text for buttons
!    Real X_COORDINATES(9) ! X-coordinates for drawing "FIT2D"
!    Real Y_COORDINATES(9) ! Y-coordinates for drawing "FIT2D"
!  Local Arrays:
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 4) / 'CONDITIONS', 'I ACCEPT', 'HELP', &
       'DON''T ACCEPT' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_DRAW_BANNER'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DRAW_BANNER ' // Version)
     Else
 
!- - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Set unity transformation
        Call LG_DATAWINDOW (0.0, 0.0, 1.0, 1.0, status)
        Call LG_VIEWPORT (0.0, 0.0, 1.0, 1.0, status)

!     Find maximum page coordinates
        xmin_banner = 0.0
        ymin_banner = 0.0
        Call LG_INQ_MAXPC (Gs_wkid_terminal, xmax_banner, ymax_banner, status)

!     Set unity transformation
        Call GS_SET_DDDR (0.0, 0.0, xmax_banner, ymax_banner, status)
        Call GS_SET_DGPP (0.0, 0.0, xmax_banner, ymax_banner, status)
 
!     Calculate general distance values
        width = xmax_banner - xmin_banner
        height = ymax_banner - ymin_banner
        gap = Min(width / 50.0, height / 50.0)
 
!     Position of box
        xmin_box = xmin_banner + gap
        ymin_box = ymin_banner + height * 0.6
        xmax_box = xmax_banner - gap
        ymax_box = ymax_banner - gap
 
!     Start of FIT2D logo
        x_start = xmin_box + gap * 3.5
        y_start = ymin_box + gap * 3.5
 
!     Position of triangle
        xmin_triangle = xmin_box
        ymin_triangle = ymin_banner + gap
        xmax_triangle = xmax_box
        ymax_triangle = ymin_box - gap
 
!     Calculate unit vector from top left hand corner towards centre of triangle
!     Set scale sizes
        If (ymax_banner / xmax_banner .Gt. 1.2) Then
           th_gap = gap * 2.0
           tv_gap = gap * 2.0
        Else If (xmax_banner/ ymax_banner .Gt. 1.2) Then
           th_gap = gap * 3.0
           tv_gap = gap * 1.3
        Else
           th_gap = gap * 2.5
           tv_gap = gap * 1.6
        End If
 
!     Line spacing and hence character height
        If (ymax_banner / xmax_banner .Gt. 1.2) Then
           line_spacing = (ymax_triangle - ymin_triangle - gap * 2.0) / 12.0
        Else If (xmax_banner/ ymax_banner .Gt. 1.2) Then
           line_spacing = (ymax_triangle - ymin_triangle - gap * 2.0) / 9.5
        Else
           line_spacing = (ymax_triangle - ymin_triangle - gap * 2.0) / 10.0
        End If
        y_text = ymax_triangle - gap * 2.0 - line_spacing
 
!     Set scale sizes for FIT2D characters
        width_fit2d = (xmax_box - xmin_box) - gap * 7.0
        height_fit2d = (ymax_box - ymin_box) - gap * 7.0

!        Write (*, '(''height_fit2d / width_fit2d = '', f12.6)') &
!         height_fit2d / width_fit2d

        x_scale = 0.434 * width_fit2d
        y_scale = 1.00 * height_fit2d 

!- - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Draw white background
!     Set solid white filling
        Call GS_FILLSTYLE (Lg_solid, Gs_white, status)
        Call GS_RECTANGLE (xmin_banner, ymin_banner, xmax_banner, ymax_banner, &
          .False., .True., status)
 
!     Draw orange box with blue border for title background
        Call GS_FILLSTYLE (Lg_solid, Gs_orange, status)
        Call GS_RECTANGLE (xmin_box, ymin_box, xmax_box, ymax_box, .False., &
          .True., status)

!     Draw "big data crusher
        x_bdc = x_start
        y_bdc = y_start
        Call GS_SET_CHAR_SCALING (x_scale / 3.3, y_scale / 3, status)
        Call GS_FILLSTYLE (Lg_solid, Gs_white, status)
        Call GS_DRAW_CHARACTER ('b', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER ('i', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER ('g', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER (' ', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER ('d', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER ('a', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER ('t', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER ('a', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER (' ', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER ('c', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER ('r', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER ('u', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER ('s', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER ('h', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER ('e', x_bdc, y_bdc, status)
        Call GS_DRAW_CHARACTER ('r', x_bdc, y_bdc, status)

!     Draw fit2d
        y_start = y_start + 0.07
        y_scale = y_scale * 0.78
        Call GS_SET_CHAR_SCALING (x_scale, y_scale, status)
        Call GS_FILLSTYLE (Lg_solid, Gs_blue, status)
        Call GS_DRAW_CHARACTER ('f', x_start, y_start, status)
        Call GS_FILLSTYLE (Lg_solid, Gs_magenta, status)
        Call GS_DRAW_CHARACTER ('i', x_start, y_start, status)
        Call GS_FILLSTYLE (Lg_solid, Gs_blue, status)
        Call GS_DRAW_CHARACTER ('t', x_start, y_start, status)
        Call GS_FILLSTYLE (Lg_solid, Gs_magenta, status)
        Call GS_DRAW_CHARACTER ('2', x_start, y_start, status)
        Call GS_FILLSTYLE (Lg_solid, Gs_blue, status)
        Call GS_DRAW_CHARACTER ('d', x_start, y_start, status)
 
!     Draw blue borders for title background
        Call GS_LINESTYLE (1, 1.0, Gs_blue, status)
        Call GS_RECTANGLE (xmin_box + gap, ymin_box + gap, xmax_box - gap, &
          ymax_box - gap, .True., .False., status)
        Call GS_RECTANGLE (xmin_box + gap * 2.0, ymin_box + gap * 2.0, &
          xmax_box - gap * 2.0, ymax_box - gap * 2.0, .True., .False., status)
        Call GS_RECTANGLE (xmin_box + gap * 3.0, ymin_box + gap * 3.0, &
          xmax_box - gap * 3.0, ymax_box - gap * 3.0, .True., .False., status)

!- - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Draw yellow triangle with grey border for text background
        Call GS_FILLSTYLE (Lg_solid, Gs_yellow, status)
        Call GS_TRIANGLE (xmin_triangle, ymax_triangle, xmax_triangle, &
          ymax_triangle, (xmin_triangle + xmax_triangle) / 2.0, ymin_triangle, &
          .False., .True., status)
        Call GS_LINESTYLE (1, 1.0, Gs_grey, status)
        Call GS_TRIANGLE ( xmin_triangle + th_gap, ymax_triangle - gap, &
          xmax_triangle - th_gap, ymax_triangle - gap, (xmin_triangle + &
          xmax_triangle) / 2.0, ymin_triangle + tv_gap, .True., .False., &
          status)
 
 !**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!       Write (*, '(''gs_width_test = '', f8.6)') gs_width_test
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Set character height depending on the width of the font
        If (gs_width_test .Le. 0.083) Then
           height_multipler = 0.65
        Else If (gs_width_test .Le. 0.10) Then
           height_multipler = 0.55
        Else
           height_multipler = 0.45
        End If

!     Reduce character size for very portrait windows
        If (ymax_banner / xmax_banner .Gt. 2.0) Then
           height_multipler = height_multipler * 0.5
        Else If (ymax_banner / xmax_banner .Gt. 1.6) Then
           height_multipler = height_multipler * 0.75
        End If

!     Set text style from information output, align horizontally to the centre
        Call GS_TEXTSTYLE (Gs_font_publication, Gs_black, line_spacing * &
          height_multipler, 1.0, 0.0, 0, 2, 0, 0.0, 1.0, status)

!     Output text
        Call LG_TEXT ((xmin_box + xmax_box) / 2.0, y_text, &
          'Copyright 1987-2015 Andy Hammersley / ESRF', status)
        y_text = y_text - line_spacing
        Call LG_TEXT ((xmin_box + xmax_box) / 2.0, y_text, &
          'Use of FIT2D implies acceptance of', status)
        y_text = y_text - line_spacing
        Call LG_TEXT ((xmin_box + xmax_box) / 2.0, y_text, &
          'the  " Conditions of Use "', status)
        y_text = y_text - line_spacing
        If (gui) Then
           Call LG_TEXT ((xmin_box + xmax_box) / 2.0, y_text, &
             '(Click on "CONDITIONS")', status)
        Else
           Call LG_TEXT ((xmin_box + xmax_box) / 2.0, y_text, &
             '(See Reference Manual)', status)
        End If
        y_text = y_text - line_spacing

!     Output warning text
        Call GS_TEXTSTYLE (Gs_font_publication, Gs_red, line_spacing * &
          height_multipler, 1.0, 0.0, 0, 2, 0, 0.0, 1.0, status)
        Call LG_TEXT ((xmin_box + xmax_box) / 2.0, y_text, &
          'Note: The text window can', status)
        y_text = y_text - line_spacing
        Call LG_TEXT ((xmin_box + xmax_box) / 2.0, y_text, &
          'contain important', status)
        y_text = y_text - line_spacing
        Call LG_TEXT ((xmin_box + xmax_box) / 2.0, y_text, &
          'information !', status)

        y_text = y_text - line_spacing
        Call GS_TEXTSTYLE (Gs_font_publication, Gs_black, line_spacing * &
          height_multipler, 1.0, 0.0, 0, 2, 0, 0.0, 1.0, status)
        Call LG_TEXT ((xmin_box + xmax_box) / 2.0, y_text, fit2d_version, &
          status)
 
!     Draw buttons if in GUI mode
        If (gui) Then
 
!        Set menu layout
           Call GS_SET_MENULAYOUT (1, 1, 6, 12, status)
 
!        Draw right-hand part of menu
           ymin_menu = ymin_banner + gap
           ymax_menu = ymin_banner + (ymax_banner - ymin_banner) / 8.0
           xmax_menu = xmax_banner - gap
           xmin_menu = xmax_menu - (xmax_banner - xmin_banner) / 3.5
           Call GS_MENU (1, 0, 'NO TITLE', 2, 2, MENU(3), xmin_menu, &
             ymin_menu, xmax_menu, ymax_menu, status)
 
!        Transfer coordinates of control bar buttons to correct
!        places within button positions arrays
           Do item = 2, 1, -1
              GS_X_MIN_BUTTONS(item + 2) = GS_X_MIN_BUTTONS(item)
              GS_Y_MIN_BUTTONS(item + 2) = GS_Y_MIN_BUTTONS(item)
              GS_X_MAX_BUTTONS(item + 2) = GS_X_MAX_BUTTONS(item)
              GS_Y_MAX_BUTTONS(item + 2) = GS_Y_MAX_BUTTONS(item)
           End Do
 
!        Draw left-hand part of menu
           xmin_menu = xmin_banner + gap
           ymin_menu = ymin_banner + gap
           xmax_menu = xmin_banner + (xmax_banner - xmin_banner) / 3.5
           ymax_menu = ymin_banner + (ymax_banner - ymin_banner) / 8.0
           Call GS_MENU (1, 0, 'NO TITLE', 2, 2, MENU, xmin_menu, ymin_menu, &
             xmax_menu, ymax_menu, status)
 
!        Set total number of button choices and the menu region to
!        cover the whole screen
           gs_x_min_menu = 0.0
           gs_y_min_menu = 0.0
           gs_x_max_menu = 1.0
           gs_y_max_menu = 1.0
           gs_drawn_num_menuchoices = 4
 
        End If
  
!     Update workstation
        Call GS_UPDATE (status)
 
     End If
 
     End Subroutine F2D_DRAW_BANNER
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
