!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_createdata.f90 *
!  *                    *
!  **********************
 
!+ F2D_CREATEDATA: CREATE DATA region (blank)
     Subroutine F2D_CREATEDATA (variances_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, xstrelm, ystrelm, xendelm, yendelm, XAXIS, YAXIS, DATA, &
       VARIANCES, title, xlabel, ylabel, zlabel, status)
!  Description:
!    Initialises blank data region (for simulation)
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    09-Feb-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: variances_exist ! If .False. variance array does
!      not exist
     Integer, Intent(IN) :: xmaxdat ! First dimension of array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: xnumdat ! Defines X-extent of defined data
     Integer, Intent(OUT) :: ynumdat ! Defines Y-extent of defined data
     Integer, Intent(OUT) :: xstrelm ! Defines start of ROI in X-direction
     Integer, Intent(OUT) :: ystrelm ! Defines start of ROI in Y-direction
     Integer, Intent(OUT) :: xendelm ! Defines end of ROI in X-direction
     Integer, Intent(OUT) :: yendelm ! Defines end of ROI in Y-direction
     Real, Intent(OUT) :: XAXIS(xmaxdat) ! Array to contain X-coordinate
!      grid data
     Real, Intent(OUT) :: YAXIS(ymaxdat) ! Array to contain Y-coordinate
!      grid data
     Real, Intent(OUT) :: DATA(xmaxdat, ymaxdat) ! Array to contain data values
     Real, Intent(OUT) :: VARIANCES(xmaxdat, ymaxdat) ! Array to contain
!      data variance estimate values. Only used if "variances_exist" is .True.
     Character(Len = *), Intent(OUT) :: title ! Title for data
     Character(Len = *), Intent(OUT) :: xlabel ! Label for X-axis of data
     Character(Len = *), Intent(OUT) :: ylabel ! Label for Y-axis of data
     Character(Len = *), Intent(OUT) :: zlabel ! Label for Z-axis of data
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: x ! Loop variable
     Integer :: y ! Loop variable
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CREATEDATA ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_CREATEDATA ' // Version)
     Else
 
!     Input size of data
        xnumdat = xmaxdat
        Call IO_INPI (.True., 1, xmaxdat, .True., 'X DATA SIZE', 1, &
          'Enter number of elements for data region in X-dimension', 1, &
          'Must be within specified bounds', xnumdat, status)
        ynumdat = ymaxdat
        Call IO_INPI (.True., 1, ymaxdat, .True., 'Y DATA SIZE', 1, &
          'Enter number of elements for data region in Y-dimension', 1, &
          'Must be within specified bounds', ynumdat, status)
 
!     Set active data region to be all the data
        xstrelm = 1
        ystrelm = 1
        xendelm = xnumdat
        yendelm = ynumdat
 
!     Initialise data
        Call MA_RVALUE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, yendelm, &
          0.0, DATA, status)
 
        If (variances_exist) Then
 
!        Initialise data variances
           Call MA_RVALUE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, 0.0, VARIANCES, status)
 
        End If
 
!     Define axis values
        Do x = 1, xnumdat
           XAXIS(x) = Real(x) - 0.5
        End Do
        Do y = 1, ynumdat
           YAXIS(y) = Real(y) - 0.5
        End Do
 
!     Define text items
        title = 'Simulated Data'
        xlabel = 'Columns'
        ylabel = 'Rows'
        zlabel = 'Intensity'
 
     End If
 
     End Subroutine F2D_CREATEDATA
!********1*********2*********3*********4*********5*********6*********7*********8
