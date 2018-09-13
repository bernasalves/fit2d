!********1*********2*********3*********4*********5*********6*********7**
 
!  ****************************
!  *                          *
!  * f2d_inp_detectortilt.f90 *
!  *                          *
!  ****************************
 
!+ F2D_INP_DETECTORTILT -  INPut angles defining DETECTOR TILT with respect to 
!  ideal detector plane which is orthogonal to the beam
     Subroutine F2D_INP_DETECTORTILT (gui, tilt_plane_rotation, tilt_angle, &
       status)
!  Description:
!    Input of angles describing detector tilt relative to ideal detector plane 
!    which is orthogonal to the beam. The tilt is described by two angles. 
!    "tilt_plane_rotation" is the angle from the X-axis to the tilt plane 
!    (anti-clockwise radians). "tilt_angle" is the rotation angle of the 
!    detector within the tilt plane anti-clockwise from the ideal detector 
!    plane.
!  Keywords:
!    Detector.Tilt, Tilt.Detector
!  Method:
!    Uses "IO_INPR"
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    24-Jan-1995: V0.1 Original (Hammersley)
!    11-Feb-1996: V0.2 Option of GUI (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is being 
!      used
!  Import/Export:
     Real, Intent(INOUT) :: tilt_plane_rotation ! Rotation angle of tilt plane
!      anti-clockwise from the X-axis (radians)
     Real, Intent(INOUT) :: tilt_angle ! Tilt angle of detector wrt to the
!      beam in the tilt plane (radians)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(9) ! User text
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_DETECTORTILT ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
 
!     Rotation angle of tilt plane and tilt angle of detector
        tilt_plane_rotation = tilt_plane_rotation * 180.0 / Pi
        MESSAGE(1) = 'The detector would ideally be ' // &
          'orthogonal to the direct X-ray beam,  but'
        MESSAGE(2) = 'in practice it is unlikely to be ' // &
          'completely  orthogonal,  but hopefully'
        MESSAGE(3) = 'any tilt is small.  To define the tilt ' // &
          'two angles  are  specified.  The'
        MESSAGE(4) = 'first  angle is the rotation  angle ' // &
          'within the ideal  orthogonal  plane'
        MESSAGE(5) = 'anti-clockwise from the X-axis ' // &
          '(horizontal rightwards, looking from the'
        MESSAGE(6) = 'sample to the detector).  This rotation ' // &
          'defines the "tilt plane" within'
        MESSAGE(7) = 'which the detector is tilted by a ' // &
          'single rotation angle. The beam is in'
        MESSAGE(8) = 'the tilt plane.  The tilt angle is the ' // &
          'angle  anti-clockwise  from  the'
        MESSAGE(9) = 'ideal orthogonal detector plane to the ' // &
          'actual tilted detector plane.'
 
        If (gui) Then
           Call GS_INPR (.True., -180.0, 180.0, .True., &
             'TILT PLANE ROTATION ANGLE (DEGREES)', 9, MESSAGE, 1, &
             'Must be valid real number within given range', &
             tilt_plane_rotation, status)
        Else
           Call IO_INPR (.True., -180.0, 180.0, .True., &
             'TILT PLANE ROTATION ANGLE (DEGREES)', 9, MESSAGE, 1, &
             'Must be valid real number within given range', &
             tilt_plane_rotation, status)
        End If
 
        tilt_plane_rotation = tilt_plane_rotation * Pi / 180.0
 
!     Detector tilt angle within tilt plane
        tilt_angle = tilt_angle * 180.0 / Pi
 
        If (gui) Then
           Call GS_INPR (.True., -180.0, 180.0, .True., &
             'DETECTOR TILT ANGLE (DEGREES)', 9, MESSAGE, 1, &
             'Must be valid real number within given range', tilt_angle, &
             status)
        Else
           Call IO_INPR (.True., -180.0, 180.0, .True., &
             'DETECTOR TILT ANGLE (DEGREES)', 9, MESSAGE, 1, &
             'Must be valid real number within given range', tilt_angle, &
             status)
        End If
 
        tilt_angle = tilt_angle * Pi / 180.0
 
     End If
 
     End Subroutine F2D_INP_DETECTORTILT
!********1*********2*********3*********4*********5*********6*********7**
