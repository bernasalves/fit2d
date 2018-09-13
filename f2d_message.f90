!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_message.f90 *
!  *                 *
!  *******************
 
!+ F2D_MESSAGE - FIT 2-D set-up MESSAGE output during a macro
     Subroutine F2D_MESSAGE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, status)
!  Description:
!    Inputs values to define an output message to be given to the user.
!  Method:
!    Usual "IO_INP*" routines
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    04-Sep-1996: V0.1 Original, based on "F2D_QUESTION" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'io_db.inc' ! I/O database
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Axis data for the Y-axis
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
     Integer, Parameter :: Max_help = 100 ! Dimension for user help text array
!  Local Variables:
     Integer :: line ! Loop variable for lines of text
!  Local Arrays:
     Character(Len = 80) :: HELP(Max_help) ! User help text
!  External Functions:
!  Local Data:
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(/1x,''Entry to Subroutine F2D_MESSAGE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_MESSAGE ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Input message text
     Call IO_WRITE ('INFO: Enter message text or user escape (\\) to exit', &
       status)
 
     If (io_output_macro) Then
        Write (io_unit_outmacro, '(''%!*\'')')
        Write (io_unit_outmacro, '(''%!*\ Message text (up to 100 lines)'')')
     End If
 
     line = 0
     Do While (status .Eq. St_goodvalue .And. line .Lt. Max_help)
 
        line = line + 1
        Call IO_INPC (.False., 'TEXT', 1, 'Enter user help text', 1, &
          'Enter valid character string', 1, HELP(line), status)
 
     End Do
     line = line - 1
 
!  Re-set status
     status = St_goodvalue
 
     If (io_output_macro) Then
        Write (io_unit_outmacro, '(''\\'')')
        Write (io_unit_outmacro, '(''%!*\'')')
        Write (io_unit_outmacro, '(''%!*\ End of message text'')')
        Write (io_unit_outmacro, '(''%!*\'')')
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Output message in graphics window
     Call GS_FPROMPT (Max_help, line, HELP, status)
     Call GS_UPDATE (status)
 
     End Subroutine F2D_MESSAGE
!********1*********2*********3*********4*********5*********6*********7*********8
 
