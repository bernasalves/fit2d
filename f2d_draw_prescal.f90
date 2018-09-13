!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_draw_prescal.f90 *
!  *                      *
!  ************************
 
!+ F2D_DRAW_PRESCAL - Fit2D: DRAW lines for PRESsure CALibrant peak positions
     Subroutine F2D_DRAW_PRESCAL (experiment, max_calibrants, num_calibrants, &
       CALIBRANT_NAMES, CALIBRANTS, pressure, baseline, status)
!  Description:
!  Keywords:
!  Method:
!  Usage:
!    General
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    03-Nov-2014: V0.6 Add extra figure to pressure text (Hammersley)
!    15-Apr-2014: V0.5 Make "pressure" double precision (Hammersley)
!    11-Apr-2014: V0.4 Add "baseline" control (Hammersley)
!    20-Dec-2013: V0.3 Store altered cell parameters (Hammersley)
!    18-Sep-2013: V0.2 Add "pressure" variable (Hammersley)
!    12-Sep-2013: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use LG_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Character(Len = *), Intent(IN) :: CALIBRANT_NAMES(Max_calibrants) 
! Name of calibrants
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of 
!      experiment (see "io.inc")
     Integer, Intent(IN) :: max_calibrants ! Dimension of "CALIBRANTS"
     Integer, Intent(IN) :: num_calibrants ! Number of defined calibrants
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: CALIBRANTS(max_calibrants) 
       ! Pressure calibrants (see "io.inc")
     Double Precision, Intent(IN) :: pressure ! Pressure on samples in GPa
     Real, Intent(IN) :: baseline ! Baseline for drawing peak positions
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
!  Local Variables:
     Integer :: calibrant ! Loop variable for calibrants
     Character(Len = 80) :: message ! Output messages
!  Local Arrays:
     Real :: A(max_calibrants) ! a of unit cell under pressure in Angstroms
     Real :: B(max_calibrants) ! b of unit cell under pressure in Angstroms
     Real :: C(max_calibrants) ! c of unit cell under pressure in Angstroms
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_DRAW_PRESCAL'')')
!     Write (*, '(''num_calibrant = '', i4)') num_calibrants
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status value
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DRAW_PRESCAL ' // Version)
        Return
     End If
 
!  Check import variables
     If (max_calibrants .Lt. 1) Then
        status = St_bad_dim1
     Else If (num_calibrants .Gt. max_calibrants) Then
        status = St_bad_adr1
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
 
!     Add module number to status value
        status = St_mod_gs + status
 
!     Save details of subroutine call
        Call ST_SAVE ('Subroutine F2D_DRAW_PRESCAL ' // Version)

     Else

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop over calibrants
        Do calibrant = 1, num_calibrants

!        Set line colour
           Call LG_LINECOLOUR (calibrant + 2, status)

           Call F2D_DRAW_PRESCAL2 (experiment, CALIBRANTS(calibrant), &
             pressure, baseline, A(calibrant), B(calibrant), C(calibrant), &
             status)

        End Do

     End If

!  Add pressure label
     Call LG_UNITYTRANSFORM (.True., status)

     Call LG_TEXTALIGNMENT (0, 0, status)
     Write (message, '(''Pressue = '', f8.4, '' GPa, Wavelength = '', &
       f8.5, '' Angstroms'')') pressure, experiment%wavelength* 1.0e10
     Call LG_TEXT (0.02, 0.98, message, status)

     Do calibrant = 1, num_calibrants

!     Set text colour
        Call LG_TEXTCOLOUR (calibrant + 2, status)

        Write (message, &
          '(a10, '': a = '', f7.4, '', b = '', f7.4, ' // &
          ''', c = '', f7.4)') &
          CALIBRANT_NAMES(calibrant), A(calibrant), B(calibrant), C(calibrant)
        Call LG_TEXT (0.02, 0.98 - Real(calibrant) * 0.02, message, status)

     End Do

     End Subroutine F2D_DRAW_PRESCAL
!********1*********2*********3*********4*********5*********6*********7*********8
