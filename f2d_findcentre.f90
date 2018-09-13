!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_findcentre.f90 *
!  *                    *
!  **********************
 
!+ F2D_FINDCENTRE - Fit2D: FIND peak CENTRE positions
     Subroutine F2D_FINDCENTRE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, ynumtemplate, &
       TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, xnumsubtemplate, &
       ynumsubtemplate, sub_pixelling, SUBTEMPLATE, search_limit, &
       x_coordinate, y_coordinate, success, max_correlation, status)
!  Description:
!    Finds the best peak centre by cross-correlating the template function with 
!    the data. This is first done at the pixel level and then to sub-pixel 
!    accuracy.
!  Keywords:
!    Centre.Peak.Determination, Peak.Centre.Determination
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Oct-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First Dimension size for "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second Dimension size for "DATA"
     Integer, Intent(IN) :: xstrelm ! First X-element of data region
     Integer, Intent(IN) :: ystrelm ! First Y-element of data region
     Integer, Intent(IN) :: xendelm ! Last X-element of data region
     Integer, Intent(IN) :: yendelm ! Last Y-element of data region
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Array containing data
     Integer, Intent(IN) :: xmaxtemplate ! First Dimension size for "TEMPLATE"
     Integer, Intent(IN) :: ymaxtemplate ! Second Dimension size for "TEMPLATE"
     Integer, Intent(IN) :: xnumtemplate ! Number of elements in first
!      dimension in "TEMPLATE"
     Integer, Intent(IN) :: ynumtemplate ! Number of elements in second
!      dimension in "TEMPLATE"
     Real, Intent(IN) :: TEMPLATE(xmaxtemplate, ymaxtemplate)
!      Array containing template function
     Integer, Intent(IN) :: xmaxsubtemplate ! First Dimension size for
!       "TEMPLATE"
     Integer, Intent(IN) :: ymaxsubtemplate ! Second Dimension size for
!      "TEMPLATE"
     Integer, Intent(IN) :: xnumsubtemplate ! Number of elements in first
!      dimension in "SUBTEMPLATE"
     Integer, Intent(IN) :: ynumsubtemplate ! Number of elements in second
!      dimension in "SUBTEMPLATE"
     Real, Intent(IN) :: SUBTEMPLATE(xmaxsubtemplate, ymaxsubtemplate)
!      Array containing template function
     Integer, Intent(IN) :: search_limit ! Maximum number of sub-pixels that
!      the search may go from the start position
     Integer, Intent(IN) :: sub_pixelling ! Number of sub-pixels per pixel
!      used for sub-pixel template
!  Import/Export:
     Real, Intent(INOUT) :: x_coordinate ! X-coordinate for start of search
!      and of best centre on output
     Real, Intent(INOUT) :: y_coordinate ! Y-coordinate for start of search
!      and of best centre on output
!  Export:
     Logical, Intent(OUT) :: success ! .True., if centre is found within the
!      defined search region
     Real, Intent(OUT) :: max_correlation ! Maximum cross-correlation value
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: x_pixel ! X-centre in pixel numbers
     Integer :: x_subpixel ! X-centre in sub-pixel numbers
     Integer :: xendsearch ! X-upper limit for search
     Integer :: xstrsearch ! X-lower limit for search
     Integer :: y_pixel ! Y-centre in pixel numbers
     Integer :: y_subpixel ! Y-centre in sub-pixel numbers
     Integer :: yendsearch ! Y-upper limit for search
     Integer :: ystrsearch ! Y-lower limit for search
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FINDCENTRE ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0 .Or. xmaxtemplate .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0 .Or. ymaxtemplate .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xendelm .Gt. xmaxdat .Or. xstrelm .Gt. &
       xendelm) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. yendelm .Gt. ymaxdat .Or. ystrelm .Gt. &
       yendelm) Then
        status = St_bad_adr2
     Else If (sub_pixelling .Le. 0) Then
        status = St_bad_int1
     Else If (search_limit .Le. 0) Then
        status = St_bad_int1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_ma + status
        Call ST_SAVE ('Subroutine F2D_FINDCENTRE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Region is correctly defined
 
!     Convert pixel coordinates to pixel number
        x_pixel = Int(x_coordinate) + 1
        y_pixel = Int(y_coordinate) + 1
 
!     Calculate search region
        xstrsearch = x_coordinate - search_limit
        ystrsearch = y_coordinate - search_limit
        xendsearch = x_coordinate + search_limit
        yendsearch = y_coordinate + search_limit
 
!     Find best pixel position
        Call MA_CENTRE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, yendelm, &
          DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, ynumtemplate, &
          TEMPLATE, xstrsearch, ystrsearch, xendsearch, yendsearch, x_pixel, &
          y_pixel, status)
 
!     Convert pixel numbers to sub-pixel numbers (centre)
        x_subpixel = x_pixel * sub_pixelling - sub_pixelling / 2
        y_subpixel = y_pixel * sub_pixelling - sub_pixelling / 2
 
!     Calculate optimum centre to nearest sub-pixel
        Call MA_BESTCENTRE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, xmaxsubtemplate, ymaxsubtemplate, xnumsubtemplate, &
          ynumsubtemplate, sub_pixelling, sub_pixelling, SUBTEMPLATE, &
          sub_pixelling, sub_pixelling, x_subpixel, y_subpixel, success, &
          max_correlation, status)
 
!     Convert sub-pixel numbers to pixel coordinates
        x_coordinate = Real(x_subpixel) / Real(sub_pixelling) - 1.0 / &
          Real(sub_pixelling * 2)
        y_coordinate = Real(y_subpixel) / Real(sub_pixelling) - 1.0 / &
          Real(sub_pixelling * 2)
 
     End If
 
     End Subroutine F2D_FINDCENTRE
!********1*********2*********3*********4*********5*********6*********7*********8
