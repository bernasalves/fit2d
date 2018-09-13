!********1*********2*********3*********4*********5*********6*********7*********8

!  *************************
!  *                       *
!  * f2d_draw_prescal2.f90 *
!  *                       *
!  *************************
 
!+ F2D_DRAW_PRESCAL2 - Fit2D: DRAW lines for PRESsure CALibrant peak positions
     Subroutine F2D_DRAW_PRESCAL2 (experiment, calibrant, pressure, baseline, &
       a, b, c, status)
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
!    15-Apr-2014: V0.7 Change "pressure" to double precision (Hammersley)
!    11-Apr-2014: V0.6 Add "baseline" control (Hammersley)
!    20-Dec-2013: V0.5 Store altered cell parameters (Hammersley)
!    07-Oct-2013: V0.4 Add other lattices (Hammersley)
!    19-Sep-2013: V0.3 Output intensity adjusted peaks (Hammersley)
!    18-Sep-2013: V0.2 Calculate volume change based on pressure (Hammersley)
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
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of 
!      experiment (see "io.inc")
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: calibrant ! Pressure calibrant 
!      (see "io.inc")
     Double Precision, Intent(IN) :: pressure ! Pressure on samples in GPa
     Real, Intent(IN) :: baseline ! Baseline for drawing peak positions
!  Export:
     Real :: a ! a of unit cell under pressure in Angstroms
     Real :: b ! b of unit cell under pressure in Angstroms
     Real :: c ! c of unit cell under pressure in Angstroms
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.7' ! Version number
!  Local Variables:
     Real :: d ! D-spacing of hkl peak
     Real :: height ! Height to draw line for peak position
     Double Precision :: length ! length of unit cell
     Real :: one_d2 ! 1.0 / d**2
     Integer :: peak ! Loop variable for peaks
     Real :: start_line ! Y Position to start drawing peak lines 
     Real :: two_theta ! 2-theta angle of hkl peak
     Double Precision :: volume_p ! Volume of unit cell at pressure 
     Real :: xmaxdddr ! Maximum drawn data display value in X-direction
     Real :: xmindddr ! Minimum drawn data display value in X-direction
     Real :: ymaxdddr ! Maximum drawn data display value in Y-direction
     Real :: ymindddr ! Minimum drawn data display value in Y-direction
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_DRAW_PRESCAL2'')')
!  Call ST_OUT (status)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status value
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DRAW_PRESCAL2 ' // Version)
        Return
     End If

!  Find out drawn data display region
     Call GS_INQ_DDDR (xmindddr, ymindddr, xmaxdddr, ymaxdddr, status)

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Calculate volume change
     Call F2D_CAL_VOLUMECHANGE (calibrant%k0, calibrant%k0p, pressure, &
       calibrant%v_0divv, status)

     volume_p = calibrant%volume / calibrant%v_0divv
     length = volume_p**(1.0d0 / 3.0d0)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''v_0divv = '', f10.6)') calibrant%v_0divv
!     Write (*, '(''num_peaks = '', i6)') calibrant%num_peaks
!     Write (*, '(''volume_p = '', f10.6)') volume_p
!     Write (*, '(''length = '', f10.6)') length
!     Write (*, '(''wavelength = '', f10.6)') experiment%wavelength * 1.0e10
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''volume_p = '', 1pe12.5)') volume_p
!     Write (*, '(''calibrant%cell_length_a = '', 1pe12.5)') &
!       calibrant%cell_length_a
!     Write (*, '(''calibrant%cell_length_c = '', 1pe12.5)') &
!       calibrant%cell_length_c
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     Do peak = 1, calibrant%num_peaks

        If (calibrant%symmetry .Eq. io_cubic) Then
           
           a = length
           b = a
           c = a
           d = length / Sqrt(Real(calibrant%H(peak)**2 + &
             (calibrant%K(peak))**2 + calibrant%L(peak)**2))

        Else If (calibrant%symmetry .Eq. io_hexagonal) Then

           a = (2.0 * volume_p / (Sqrt(3.0) * &
             calibrant%cell_length_c / calibrant%cell_length_a))**(1.0 / 3.0)
           b = a
           c = a * calibrant%cell_length_c / calibrant%cell_length_a

           d = 1.0 / Sqrt( (4.0 / 3.0) * (Real(calibrant%H(peak))**2 + &
             Real(calibrant%H(peak)) * Real(calibrant%K(peak)) + &
             Real(calibrant%K(peak))**2) / a**2 + &
             Real(calibrant%L(peak))**2 / c**2)
        
        Else If (calibrant%symmetry .Eq. io_tetragonal) Then

           a = (volume_p /(calibrant%cell_length_c / calibrant%cell_length_a)) &
             **(1.0 / 3.0)
           b = a
           c = a

           d = 1.0 / Sqrt( &
             (Real(calibrant%H(peak))**2.0 + Real(calibrant%K(peak)**2)) / &
             a**2.0 + Real(calibrant%L(peak)**2) / c**2)

        Else If (calibrant%symmetry .Eq. io_orthorhombic) Then

           a = (volume_p /(calibrant%cell_length_b / calibrant%cell_length_a * &
             calibrant%cell_length_c / calibrant%cell_length_a))**(1.0 / 3.0)
           c = a * calibrant%cell_length_c / calibrant%cell_length_a
           b = a * calibrant%cell_length_b / calibrant%cell_length_a

           d = 1.0 / Sqrt(Real(calibrant%H(peak)**2) / a**2.0 + &
             Real(calibrant%K(peak)**2) / b**2.0 + &
             Real(calibrant%L(peak)**2) / c**2.0)

        Else If (calibrant%symmetry .Eq. io_monoclinic) Then

           a = (volume_p /(calibrant%cell_length_b / calibrant%cell_length_a * &
             calibrant%cell_length_c / calibrant%cell_length_a) * &
             Sin(calibrant%cell_beta * Pi / 180.0))**(1.0 / 3.0)
           c = a * calibrant%cell_length_c / calibrant%cell_length_a
           b = a * calibrant%cell_length_b / calibrant%cell_length_a

           d = 1.0 / Sqrt( &
             (1.0 / Sin(calibrant%cell_beta * Pi / 180.0)**2) * &
             Real(calibrant%H(peak)**2) / a**2.0 + &
             Real(calibrant%K(peak)**2) * &
             Sin(calibrant%cell_beta * Pi / 180.0)**2 / b**2.0 + &
             Real(calibrant%L(peak)**2) / c**2.0 - &
             (2.0 * Real(calibrant%H(peak)) * Real(calibrant%L(peak)) * &
             Cos(calibrant%cell_beta * Pi / 180.0) / a * c))

        Else If (calibrant%symmetry .Eq. io_rhombohedral) Then

           a = length
           b = a
           c = a
           one_d2 = (1.0 / length) * (Real(calibrant%H(peak)**2) + &
             Real(calibrant%K(peak)**2) + Real(calibrant%L(peak)**2) + &
             2.0 * Cos(calibrant%cell_alpha * Pi / 180.0) * & 
             (Real(calibrant%K(peak)) * Real(calibrant%L(peak)) + &
             Real(calibrant%H(peak)) * Real(calibrant%L(peak)) + &
             Real(calibrant%H(peak)) * Real(calibrant%K(peak))))
           d = 1.0 / Sqrt(one_d2)

        Else If (calibrant%symmetry .Eq. io_triclinic) Then

           Call GS_FWARNING (1, 1, 'TRICLINIC lattice not implemented', status)
           Return

        End If

        two_theta = 2.0 * Asin (experiment%wavelength / (2.0 * d * 1.0e-10)) &
          * 180.0 / Pi


!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''d = '', f10.4, '' Angstroms'')') d
!        Write (*, '(''two_theta = '', f10.4, '' degrees'')') two_theta
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

        start_line = ymindddr + (ymaxdddr - ymindddr) * baseline
        height = start_line + (ymaxdddr - ymindddr) * &
          calibrant%INTENSITIES(peak) / 100.0

!     Draw line at 2-theta position
        Call GS_LINE (two_theta, start_line, two_theta, height, status)

     End Do

     End Subroutine F2D_DRAW_PRESCAL2
!********1*********2*********3*********4*********5*********6*********7*********8
