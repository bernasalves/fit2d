!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_savegeometry.f90 *
!  *                      *
!  ************************
 
!+ F2D_SAVEGEOMETRY: SAVE experimental GEOMETRY
     Subroutine F2D_SAVEGEOMETRY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, x_pixel_size, y_pixel_size, status)
!  Description:
!  Keywords:
!    Geometry.Experimental.Save, Save.Geometry.Experimental,
!    Store.Geometry.Experimental
!  Method:
!    Saves geometry parameters to a named file
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    11-Jan-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc' ! Circle least squares fitting common
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: x_pixel_size ! Size of a pixel in the X-direction
!      (metres)
     Real, Intent(IN) :: y_pixel_size ! Size of a pixel in the Y-direction
!      (metres)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
     Integer, Parameter :: Io_out = 20 ! Unit for output
!  Local Variables:
     Character(Len = 256), Save :: file_name = 'fit2d.geo'
!      Name of output file
     Integer :: retstat ! Return status variable
     Real :: detector_rotation ! Angle of rotation from ideal detector
!      X-axis (laboratory Y-axis) TO X-axis of actual detector i.e.
!      scanned film, or image plate (radians)
     Real :: sample_distance ! Distance from sample to detector (metres)
     Real :: tilt_rotation ! Rotation angle of tilt plane anti-clockwise 
!      from the X-axis (radians)
     Real :: tilt_angle ! Tilt angle of detector wrt to the beam in the tilt
!      plane (radians)
     Real :: wavelength ! Wavelength in metres
     Real :: x_beam_centre ! X-coordinate of beam centre (pixels)
     Real :: y_beam_centre ! Y-coordinate of beam centre (pixels)
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SAVEGEOMETRY ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xstrelm .Lt. 1 .Or. xstrelm .Gt. xendelm .Or. xendelm .Gt. xmaxdat) &
       Then
        status = St_bad_adr1
     Else If (ystrelm .Lt. 1 .Or. ystrelm .Gt. yendelm .Or. yendelm .Gt. &
       ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_SAVEGEOMETRY ' // Version)
     Else
 
!     Input name of file for saving geometrical parameters
        Call IO_INPC (.True., 'GEOMETRY FILE NAME', 1, &
          'Enter Name of file to save geometrical values', 1, &
          'Unacceptable input', 11, file_name, status)
        Call IO_OPEN_ASCIIFILE (.False., 'WRITE', Io_out, file_name, retstat, &
          status)
 
        If (retstat .Ne. 0) Then
           Return
        End If

!     Set default values if the geometry is un-defined
        x_beam_centre = Real(xstrelm + xendelm) / 2.0
        y_beam_centre = Real(ystrelm + yendelm) / 2.0
        sample_distance = 0.2
        wavelength = 1.488e-10
        tilt_rotation = 0.0
        tilt_angle = 0.0
        detector_rotation = 0.0
 
!     Get values from internal data-base if defined
        Call IO_INQ_RKEYVALUE ('WAVELENGTH', wavelength, retstat, status)
        Call IO_INQ_RKEYVALUE ('SAMPLE_DISTANCE', sample_distance, retstat, &
          status)
        Call IO_INQ_RKEYVALUE ('TILT_ROTATION', tilt_rotation, retstat, status)
        Call IO_INQ_RKEYVALUE ('TILT_ANGLE', tilt_angle, retstat, status)
        Call IO_INQ_RKEYVALUE ('DETECTOR_ROTATION', detector_rotation, &
          retstat, status)
        Call IO_INQ_RKEYVALUE ('X_BEAM_CENTRE', x_beam_centre, retstat, status)
        Call IO_INQ_RKEYVALUE ('Y_BEAM_CENTRE', y_beam_centre, retstat, status)
 
!     Output simple header
        Write (Io_out, '(''! FIT2D Experimental Geometry Parameter File'')')
        Write (Io_out, '('' '')')
 
!     Output values
        Write (Io_out, '(''WAVELENGTH = '', g12.5)') wavelength
        Write (Io_out, '(''SAMPLE_DISTANCE = '', g12.5)') sample_distance
        Write (Io_out, '(''TILT_ROTATION = '', g12.5)') tilt_rotation
        Write (Io_out, '(''TILT_ANGLE = '', g12.5)') tilt_angle
        Write (Io_out, '(''DETECTOR_ROTATION = '', g12.5)') detector_rotation
        Write (Io_out, '(''X_BEAM_CENTRE = '', g12.5)') x_beam_centre
        Write (Io_out, '(''Y_BEAM_CENTRE = '', g12.5)') y_beam_centre
        Write (Io_out, '(''X_PIXEL_SIZE = '', g12.5)') x_pixel_size
        Write (Io_out, '(''Y_PIXEL_SIZE = '', g12.5)') y_pixel_size
        Write (Io_out, '(''X_START_ELEMENT = '', i10)') xstrelm
        Write (Io_out, '(''Y_START_ELEMENT = '', i10)') ystrelm
        Write (Io_out, '(''X_END_ELEMENT = '', i10)') xendelm
        Write (Io_out, '(''Y_END_ELEMENT = '', i10)') yendelm
 
!     Close file
        Close (Io_out)
 
     End If
 
     End Subroutine F2D_SAVEGEOMETRY
!********1*********2*********3*********4*********5*********6*********7*********8
