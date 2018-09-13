!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * fio_override_input.f90 *
!  *                        *
!  **************************
 
!+ FIO_OVERRIDE_INPUT: OVER RIDE INPUT VALUES
     Subroutine FIO_OVERRIDE_INPUT (experiment, status)
!  Description:
!    Set experimental geometry
!  Keywords:
!    Over-ride.Input~Values, Input~Values.Over-ride
!  Method:
!    Stores geometry parameters in internal database
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    07-Oct-2011: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Real :: default_distance ! Default distance (metres)
     Real :: default_x_pixel_size ! Default X-pixel size (metres)
     Real :: default_y_pixel_size ! Default Y pixel size (metres)
     Real :: default_wavelength ! Default wavelength (metres)
     Logical, Save :: override_distance = .False. ! .True., if distance is
!      to be over-ridden with default value on input
     Logical, Save :: override_pixel_sizes = .False. ! .True., if
!      pixel_sizes are to be over-ridden with default value on input
     Logical, Save :: override_wavelength = .False. ! .True., if wavelength
!      is to be over-ridden with default value on input
     Integer :: retstat ! Return status variable:
!      0 = Good status
!      1 = No more room (for storage), or no more values
!      2 = Key not found for retrieval, or element doesn't exist
!      3 = Problem converting character string to a real value
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine FIO_OVERRIDE_INPUT ' // Version)
     Else
 
!     Over-ride input wavelength
        Call IO_INQ_LKEYVALUE ('OVERRIDE_WAVELENGTH', override_wavelength, &
          retstat, status)

        If (override_wavelength) Then

           Call IO_INQ_RKEYVALUE ('DEFAULT_WAVELENGTH', default_wavelength, &
             retstat, status)
           experiment%wavelength = default_wavelength
        End If

!     Over-ride distance
        Call IO_INQ_LKEYVALUE ('OVERRIDE_DISTANCE', override_distance, &
          retstat, status)

        If (override_distance) Then
           Call IO_INQ_RKEYVALUE ('DEFAULT_DETECTOR_DISTANCE', &
             default_distance, retstat, status)
           experiment%detector_distance = default_distance
        End If

!     Over-ride input pixel sizes
        Call IO_INQ_LKEYVALUE ('OVERRIDE_PIXEL_SIZES', override_pixel_sizes, &
          retstat, status)

        If (override_pixel_sizes) Then
           Call IO_INQ_RKEYVALUE ('DEFAULT_X_PIXEL_SIZE', &
             default_x_pixel_size, retstat, status)
           Call IO_INQ_RKEYVALUE ('DEFAULT_Y_PIXEL_SIZE', &
             default_y_pixel_size, retstat, status)
           experiment%x_pixel_size = default_x_pixel_size
           experiment%y_pixel_size = default_y_pixel_size
        End If
 
     End If
 
     End Subroutine FIO_OVERRIDE_INPUT
!********1*********2*********3*********4*********5*********6*********7*********8
