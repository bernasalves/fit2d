!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************************
!  *                            *
!  * f2d_parallax_dspacings.f90 *
!  *                            *
!  ******************************
 
!+ F2D_PARALLAX_DSPACINGS -  applied PARALLAX modification to D-SPACINGS
     Subroutine F2D_PARALLAX_DSPACINGS (experiment, max_angles, &
       num_cali_rings, D_SPACINGS, status)
!  Description:
!    Allows the "CALIBRANT" option to be used taking into account the effect of 
!    parallax on angular position, by modifying the D-Spacings. To do this the 
!    angle of the ring is calculated, the angular change owing to parallax is 
!    found, and the D-spacing is modified to take this into account.
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    10-Jun-2013: V0.2 Use linear interpolation of parallax effect (Hammersley)
!    18-Apr-2013: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fitrings.inc' ! Powder rings least squares
!    fitting common, passes coordinate values to "F2D_LSQPOWDER"
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: max_angles ! Dimension of "D_SPACINGS" array
     Integer :: num_cali_rings ! Number of calibration powder rings defined
!  Import/Export:
     Real, Intent(INOUT) :: D_SPACINGS(Max_angles) ! Calibrant diffraction peak
!      d-spacings in order of distance in metres
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Real correction ! Correction to apply owing to parallax effect
     Integer ring ! Loop variable for diffraction rings
     Real two_theta ! Two Theta angle of diffraction rings
!  Local Arrays:
     Real :: PARALLAX_CORRECTION(0: 90) ! Parallax correction in degrees at each
!      degree from 0 to 85 degrees
!  Internal Functions:
     Real Linear_Interpolation ! Calculate linear interpolation of a pont at
!      'fraction', between values 'lower' at 0.0 and 'higher' at 1.0
     Real lower ! Lower value at 0.0
     Real higher ! Higher value at 1.0
     Real fraction ! Fractional distance inbetween 'lower' and 'higher'
     Linear_Interpolation(lower, higher, fraction) = &
       lower * (1.0 - fraction) + higher * fraction
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PARALLAX_DSPACINGS ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (max_angles .Le. 0) Then
        status = St_bad_dim1
     Else If (num_cali_rings .Lt. 1) Then
        status = St_bad_adr1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_PARALLAX_DSPACINGS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_PARALLAX_DSPACINGS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Calculate parallax effect angle from 0 to 85 degrees
        Call F2D_CAL_PARALLAX (experiment, PARALLAX_CORRECTION, status)

!     Modify D-spacing of each ring taking into account parallax
        Do ring = 1, num_cali_rings

!        Calculate angle in degrees of ring given the wavelength
           two_theta = 180.0 / Pi * 2.0 * &
             Asin (experiment%wavelength / (2.0 * D_SPACINGS(ring)))

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''D = '', 1pe12.5)') D_SPACINGS(ring)
!           Write (*, '(''two theta = '', f12.5)') two_theta
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

! Old code
!           correction = -PARALLAX_CORRECTION(Nint(two_theta))
! Linear interpolation
           correction = -(Linear_Interpolation( &
             PARALLAX_CORRECTION(Int(two_theta)), &
             PARALLAX_CORRECTION(Int(two_theta) + 1), &
             two_theta - Int(two_theta)))
           two_theta = two_theta * Pi / 180.0 + correction

!        Convert modified angle to equivalent D-spacings
           D_SPACINGS(ring) = experiment%wavelength / &
             (2.0 * Sin(two_theta / 2.0))

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''correction = '', f12.5)') correction
!           Write (*, '(''New D = '', 1pe12.5)') D_SPACINGS(ring)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

        End Do

     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_PARALLAX_DSPACINGS: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_PARALLAX_DSPACINGS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
