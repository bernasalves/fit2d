!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_cal_parallax.f90 *
!  *                      *
!  ************************
 
!+ F2D_CAL_PARALLAX -  CALculate PARALLAX correction from 0 to 85 degrees
     Subroutine F2D_CAL_PARALLAX (experiment, PARALLAX_CORRECTION, status)
!  Description:
!  Keywords:
!    Parallax.Correction, Correction.Parallax
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    03-Oct-2012: V0.2 Implementing (Hammersley)
!    25-Sep-2012: V0.1 Calculate parallax effect (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: PARALLAX_CORRECTION(0: 90) ! Effect of parallax
!       per degree from 0 to 85 degrees
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Real :: absorption ! Fraction of X-ray absorbed in detector
     Real :: attenuation ! Attenuation coefficient of detection material per
!      metre
     Integer :: angle ! Angle for parallax effect in degrees 
     Integer :: inc ! Increment for depth in detector
     Real :: com_absorption ! Centre of mass of the distance absorption
     Real :: distance ! Step distance through detector for an X-ray
     Real :: intensity ! Intensity (normalised) of X-ray through the detector
     Real :: parallax_distance ! Distance on detector of parallax effect
     Real :: path_length ! Path length of X-ray in the detector
     Real :: sum_absorption ! Sum of the absorption at different depths
     Real :: sum_com ! Sum of the centre of mass
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CAL_PARALLAX ' // Version)
        Return
     End If
 
     PARALLAX_CORRECTION(0) = 0.0

!  Attenuation coefficient
     attenuation = (1.0 / experiment%detection_depth) * Log(1.0 / &
       experiment%detection_attenuation)

     Do angle = 1, 85

!     Calculate path length in detector
        path_length = experiment%detection_depth / Cos(Real(angle) * Pi / 180.0)

!     Calculate centre of mass of absorption
        distance = path_length / 1000.0
        intensity = 1.0
        sum_com = 0.0
        sum_absorption = 0.0

        Do inc = 1, 1000
           absorption = intensity * &
             (1.0 - Exp(-distance * attenuation))
           sum_absorption = sum_absorption + absorption
           intensity = intensity - absorption
        End Do

!     Calculate centre of mass of absorption along detection path
        com_absorption = sum_com / sum_absorption

!     Calculate parallax distance on detector
        parallax_distance = com_absorption * Sin(Real(angle) * Pi / 180.0)

!     Calculate parallax correction angle in radians
        PARALLAX_CORRECTION(angle) = -Atan2(parallax_distance, &
          experiment%detector_distance)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''angle = '', i2, '' parallax = '', f12.5)') &
!          angle, PARALLAX_CORRECTION(angle) * 180.0 / Pi
!        Write (*, '(''path_length = '', f12.6)') path_length
!        Write (*, '(''com_absorption = '', f12.6)') com_absorption
!        Write (*, '(''parallax_distance = '', f12.6)') parallax_distance
!        Write (*, '(''intensity = '', f12.6)') intensity
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Do

!  At high angles do not calculate correction, but set to zero
     Do angle = 86, 90
        PARALLAX_CORRECTION(angle) = 0.0
     End Do

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_CAL_PARALLAX: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_CAL_PARALLAX
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
