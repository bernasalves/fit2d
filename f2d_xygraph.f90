!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_xygraph.f90 *
!  *                 *
!  *******************
 
!+ F2D_XYGRAPH: X/Y GRAPHs
     Subroutine F2D_XYGRAPH (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, VARIANCES, X_AXIS, Y_AXIS, title, &
       xlabel, ylabel, zlabel, status)
!  Description:
!    Displays one or more X/Y graphs
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    05-Feb-1996: V0.4 Add "gui" control variable (Hammersley)
!    10-Dec-1995: V0.3 Use new interactive X/Y graph display routine 
!      (Hammersley)
!    20-Jun-1995: V0.2 Convert to GS graphics library (Hammersley)
!    07-Jun-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is to
!      be used
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Logical, Intent(IN) :: variances_exist ! .True., if variances exist
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! Corresponding
!      variance values
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Integer :: dummy ! Dummy variable, unused at present
     Integer :: graph ! Loop variable for graphs
     Logical :: continue ! .True., if the user wants to continue outputing the 
!      series of graphs
     Logical :: x_errors ! .True., means draw X-direction error bars
     Logical :: y_errors ! .True., means draw Y-direction error bars
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_XYGRAPH ' // Version)
     Else
 
!     No error output at present
        Call GS_INQ_ERRORBARS (x_errors, y_errors, status)
        Call GS_SET_ERRORBARS (.False., .False., status)
 
!     Display graph sequence until user is bored, or no more graphs
        continue = .True.
        graph = ystrelm
        Do While (graph .Le. yendelm .And. continue)
 
           If (gui) Then
 
!           Output X/Y graph with interactive control
              Call GS_INT_XYEGRAPH (xmaxdat, 1, 1, xstrelm, xendelm, X_AXIS, &
                DATA(1, graph), dummy, dummy, dummy, dummy, title, xlabel, &
                zlabel, status)
           Else
 
!           Output simple graph
              Call GS_XYSGRAPH (xmaxdat, xstrelm, xendelm, X_AXIS, DATA(1, &
                graph), title, xlabel, zlabel, status)
 
           End If
 
           graph = graph + 1
           If (graph .Le. yendelm) Then
 
              Call IO_INPL (.True., 0, 1, .True., 'CONTINUE', 1, &
                '"YES" for further 1-D graphs, "NO" to exit', 1, &
                'Enter "YES" or "NO"', continue, status)
 
           End If
 
        End Do
 
!     Re-set error bar output
        Call GS_SET_ERRORBARS (x_errors, y_errors, status)
 
     End If
 
     End Subroutine F2D_XYGRAPH
!********1*********2*********3*********4*********5*********6*********7*********8
 
