!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************************
!  *                            *
!  * f2d_inp_sampledistance.f90 *
!  *                            *
!  ******************************
 
!+ F2D_INP_SAMPLEDISTANCE -  INPut SAMPLE to detector DISTANCE
     Subroutine F2D_INP_SAMPLEDISTANCE (gui, sample_distance, status)
!  Description:
!    User input of sample to detector distance (intersection of beam with 
!    detector). The distance "sample_distance" is returned in metre units, 
!    whereas the user is prompted to enter the distance in millimetres.
!  Keywords:
!    Distance.Sample.Detector, Sample-Detector.Distance
!  Method:
!    Uses "IO_INPR"
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Feb-1996: V0.2 Option of GUI (Hammersley)
!    24-Jan-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is being 
!      used
!  Import/Export:
     Real, Intent(INOUT) :: sample_distance ! Distance from sample (powder)
!      to the detector (beam centre) in metres
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(4) ! User text
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_SAMPLEDISTANCE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input sample to detector distance
        sample_distance = sample_distance * 1000.0
        MESSAGE(1) = 'Enter  distance from the sample to  the  ' // &
          'detector in millimetres.  As the'
        MESSAGE(2) = 'detector may be tilted this is the ' // &
          'distance from the centre of the sample'
        MESSAGE(3) = 'to the intersection of the direct  beam ' // &
          'and  the  detector or in the case'
        MESSAGE(4) = 'of a spatially  corrected  image the grid mask.'
 
        If (gui) Then
           Call GS_INPR (.True., 0.1, 100000.0, .True., &
             'SAMPLE TO DETECTOR DISTANCE (MILLIMETRES)', 4, MESSAGE, 1, &
             'Must be valid real number within given range', sample_distance, &
             status)
        Else
           Call IO_INPR (.True., 0.1, 100000.0, .True., &
             'SAMPLE TO DETECTOR DISTANCE (MILLIMETRES)', 4, MESSAGE, 1, &
             'Must be valid real number within given range', sample_distance, &
             status)
        End If
 
        sample_distance = sample_distance / 1000.0
 
     End If
 
     End Subroutine F2D_INP_SAMPLEDISTANCE
!********1*********2*********3*********4*********5*********6*********7*********8
