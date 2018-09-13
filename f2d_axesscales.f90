!********1*********2*********3*********4*********5*********6*********7**
 
!  **********************
!  *                    *
!  * f2d_axesscales.f90 *
!  *                    *
!  **********************
 
!+ F2D_AXESSCALES - Fit 2-D: change AXES SCALES
     Subroutine F2D_AXESSCALES (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, XAXIS, YAXIS, status)
!  Description:
!    Allows the user to change the axis scales for the X and Y axes
!    of displayed images.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    23-Dec-1997: V0.2 Calls to "F2D_AXES" (Hammersley)
!    08-Mar-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if the GUI interface is being used
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xnumdat ! Number of valid X-axis elements
     Integer, Intent(IN) :: ynumdat ! Number of valid X-axis elements
     Integer, Intent(IN) :: xstrelm ! Defines X-start of active data region
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of active data region
     Integer, Intent(IN) :: xendelm ! Defines X-end of active data region
     Integer, Intent(IN) :: yendelm ! Defines Y-end of active data region
!  Import/Export:
     Real, Intent(INOUT) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(INOUT) :: YAXIS(ymaxdat) ! Y-axis values
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Real :: x_offset ! Value of first X-axis element
     Real :: x_scale ! Increment between X-axis elements
     Real :: y_offset ! Value of first Y-axis element
     Real :: y_scale ! Increment between Y-axis elements
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(5)
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_AXESSCALES ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_AXESSCALES ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
 
!     Calculate present axis scales
        x_offset = XAXIS(1)
        If (xnumdat .Gt. 1) Then
           x_scale = XAXIS(2) - XAXIS(1)
        Else
           x_scale = 1.0
        End If
        y_offset = YAXIS(1)
        If (ynumdat .Gt. 1) Then
           y_scale = YAXIS(2) - YAXIS(1)
        Else
           y_scale = 1.0
        End If
 
!     Offset value for the X-axis
        MESSAGE(1) = 'Enter the axis value for the first element ' // &
          'of the X-axis. This value'
        MESSAGE(2) = 'corresponds to the axis value for the ' // &
          'MIDDLE of the first pixel. All'
        MESSAGE(3) = 'axis element values refer to the middle ' // &
          'of the pixels. The normal'
        MESSAGE(4) = 'default value is 0.5 for axes which are ' // &
          'number 1, 2, 3, etc.'
        If (gui) Then
           Call GS_INPR (.False., 0.0, 1.0, .True., &
             '1ST X-AXIS ELEMENT VALUE', 4, MESSAGE, 1, &
             'Enter valid real value', x_offset, status)
        Else
           Call IO_INPR (.False., 0.0, 1.0, .True., &
             '1ST X-AXIS ELEMENT VALUE', 4, MESSAGE, 1, &
             'Enter valid real value', x_offset, status)
        End If
 
!     Increment value for X-axis
        MESSAGE(1) = 'Enter the increment between axis values ' // &
          'for the X-axis.'
        MESSAGE(2) = 'Negative increments may be enetered, BUT ' // &
          'they may cuase'
        MESSAGE(3) = 'problems with some options which have not ' // &
          'been designed'
        MESSAGE(4) = 'with negative axes in mind ! A value of ' // &
          '1.0 corresponds'
        MESSAGE(5) = 'to a normal 1, 2, 3, scaling per pixel.'
        If (gui) Then
           Call GS_INPR (.False., 0.0, 1.0, .True., &
             'X-AXIS ELEMENT INCREMENT', 5, MESSAGE, 1, &
             'Enter valid real value', x_scale, status)
        Else
           Call IO_INPR (.False., 0.0, 1.0, .True., &
             'X-AXIS ELEMENT INCREMENT', 5, MESSAGE, 1, &
             'Enter valid real value', x_scale, status)
        End If
 
!     Offset value for the Y-axis
        MESSAGE(1) = 'Enter the axis value for the first element ' // &
          'of the Y-axis. This value'
        MESSAGE(2) = 'corresponds to the axis value for the ' // &
          'MIDDLE of the first pixel. All'
        MESSAGE(3) = 'axis element values refer to the middle ' // &
          'of the pixels. The normal'
        MESSAGE(4) = 'default value is 0.5 for axes which are ' // &
          'number 1, 2, 3, etc.'
        If (gui) Then
           Call GS_INPR (.False., 0.0, 1.0, .True., &
             '1ST Y-AXIS ELEMENT VALUE', 4, MESSAGE, 1, &
             'Enter valid real value', y_offset, status)
        Else
           Call IO_INPR (.False., 0.0, 1.0, .True., &
             '1ST Y-AXIS ELEMENT VALUE', 4, MESSAGE, 1, &
             'Enter valid real value', y_offset, status)
        End If
 
!     Increment value for Y-axis
        MESSAGE(1) = 'Enter the increment between axis values ' // &
          'for the Y-axis.'
        MESSAGE(2) = 'Negative increments may be enetered, BUT ' // &
          'they may cuase'
        MESSAGE(3) = 'problems with some options which have not ' // &
          'been designed'
        MESSAGE(4) = 'with negative axes in mind ! A value of ' // &
          '1.0 corresponds'
        MESSAGE(5) = 'to a normal 1, 2, 3, scaling per pixel.'
        If (gui) Then
           Call GS_INPR (.False., 0.0, 1.0, .True., &
             'Y-AXIS ELEMENT INCREMENT', 5, MESSAGE, 1, &
             'Enter valid real value', y_scale, status)
        Else
           Call IO_INPR (.False., 0.0, 1.0, .True., &
             'Y-AXIS ELEMENT INCREMENT', 5, MESSAGE, 1, &
             'Enter valid real value', y_scale, status)
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Check for user escape
        If (status .Eq. St_goodvalue) Then
 
!        Create new axes scales
           Call F2D_AXES (xmaxdat, xnumdat, x_offset, x_scale, XAXIS, status)
           Call F2D_AXES (ymaxdat, ynumdat, y_offset, y_scale, YAXIS, status)
 
        End If
 
     End If
 
     End Subroutine F2D_AXESSCALES
!********1*********2*********3*********4*********5*********6*********7**
