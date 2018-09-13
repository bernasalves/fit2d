!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_trailer.f90 *
!  *                 *
!  *******************
 
!+ F2D_TRAILER - FIT2D: Output trailer
     Subroutine F2D_TRAILER (fit2d_version, status)
!  Description:
!    Outputs trailer text at the end of the program.
!  Keywords:
!    Trailer.Graphical
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    12-Nov-2010: V0.8 Acknowledge CBFLIB use (Hammersley)
!    17-Feb-2001: V0.7 Make "fit2d_version" fixed length to keep "g77" compiler 
!      happy (Hammersley)
!    13-Nov-2000: V0.6 Cope with seven character "fit2d_version" length 
!      (Hammersley)
!    22-Sep-1999: V0.5 Make "fit2d_version" a constant length to make "g77" 
!      happy (Hammersley)
!    29-Jan-1998: V0.4 Add output of version number (Hammersley)
!    26-Nov-1996: V0.3 Remove acknowledgement to NAG (Hammersley)
!    28-Aug-1995: V0.2 Output logo (Hammersley)
!    01-Aug-1995: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Error status return variables
!  Import:
     Character(Len = 7), Intent(IN) :: fit2d_version ! FIT2D version number
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
!  Local Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_TRAILER'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_TRAILER ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Output logo
        Call IO_WRITE (' ', status)
        Call F2D_LOGO (.False., status)
 
        Call IO_WRITE (' ', status)
        Call IO_WRITE ('----------------------------------' // &
          '--------------------------------------------', status)
        Call IO_WRITE ('INFO: BYE: FIT2D ' // fit2d_version // &
          ' exited by user command', status)
        Call IO_WRITE (' ', status)
 
!     Output acknowledgements
        Call IO_WRITE ('INFO: Spline surface fitting uses ' // &
          'FITPACK, written by Paul Dierckx.', status)
        Call IO_WRITE ('      CBF file I/O uses CBFLIB by Paul Ellis ' // &
          'and Herb Bernstein.', status)
        Call IO_WRITE ('      The "PRESSURE CALIBRATION" interface is ' // &
          'based on the', status)
        Call IO_WRITE ('      HPdiff Applet by Sebastian Merkel.', status)
          Call IO_WRITE ('----------------------------------' // &
          '--------------------------------------------', status)
        Call IO_WRITE (' ', status)
 
     End If
 
     End Subroutine F2D_TRAILER
!********1*********2*********3*********4*********5*********6*********7*********8
  
 
 
 
 
