!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_loadgeometry.f90 *
!  *                      *
!  ************************
 
!+ F2D_LOADGEOMETRY: SAVE experimental GEOMETRY
     Subroutine F2D_LOADGEOMETRY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, x_pixel_size, y_pixel_size, status)
!  Description:
!    User input of experimental geometry
!  Keywords:
!    Geometry.Experimental.Save, Save.Geometry.Experimental,
!    Store.Geometry.Experimental
!  Method:
!    Saves geometry parameters to a named file
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    07-Feb-2005: V0.2 Investigating problem inputting file (Hammersley)
!    12-Jan-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc' ! Circle least squares
!    fitting common
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(INOUT) :: x_pixel_size ! Size of a pixel in the X-direction 
!      (metres)
     Real, Intent(INOUT) :: y_pixel_size ! Size of a pixel in the Y-direction 
!      (metres)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
     Integer, Parameter :: io_in = 20 ! Unit for output
     Integer, Parameter :: Max_tokens = 20 ! Dimension of "TOKENS"
!  Local Variables:
     Character(Len = 256), Save :: file_name = 'fit2d.geo'
!      Name of output file
     Character(Len = 256) :: line ! Line of text input from file
     Integer :: iostat ! I/O status return variable
     Integer :: len_file_name ! Number of defined characters in file name
     Integer :: num_tokens ! Number of tokens found
     Integer :: retstat ! Return status variable
     Logical :: convert ! .True., if a token is converted successfully
     Logical :: more ! .True., whilst more lines to input
     Real :: detector_rotation ! Angle of rotation from ideal detector
!      X-axis (laboratory Y-axis) TO X-axis of actual detector i.e.
!      scanned film, or image plate (radians)
     Real :: sample_distance ! Distance from sample to detector (metres)
     Real :: tilt_rotation ! Rotation angle of tilt plane anti-clockwise from
!      the X-axis (radians)
     Real :: tilt_angle ! Tilt angle of detector wrt to the beam in the tilt
!      plane (radians)
     Real :: wavelength ! Wavelength in metres
     Real :: x_beam_centre ! X-coordinate of beam centre (pixels)
     Real :: y_beam_centre ! Y-coordinate of beam centre (pixels)
!  Local Arrays:
     Character(Len = 80) :: TOKENS(Max_tokens) ! Line as tokens
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LOADGEOMETRY ' // Version)
        Return
     End If
 
!  Input name of file for saving geometrical parameters
     Call IO_INPS (.True., 'GEOMETRY FILE NAME', 1, &
       'Enter Name of file containing geometrical values', 1, &
       'Unacceptable input', 11, len_file_name, file_name, status)
     Call IO_OPEN_ASCIIFILE (.False., 'READ', io_in, file_name, retstat, &
       status)
 
     If (retstat .Ne. 0) Then
 
        Call IO_WRITE ('WARNING: File not found', status)
        Return
 
     End If
 
!  Input header
     Read (Io_in, Fmt = '(a)', Iostat = iostat) line
     If (line .Ne. '! FIT2D Experimental Geometry Parameter File') Then
 
        Call IO_WRITE ('WARNING: File is not a FIT2D geometry file', status)
        Return
 
     End If
 
     more = .True.
     Do While (more)
 
!     Input line
        Read (Io_in, Fmt = '(a)', Iostat = iostat) line
 
        If (iostat .Ne. 0) Then
           more = .False.
        Else
 
!        Separate line into tokens
           Call IO_TOKENS (line, max_tokens, convert, num_tokens, TOKENS, &
             status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Line = '', a)') line
!        Write (*, '(''convert = '', l1, '', num_tokens = '', i3)')
!        :          convert, num_tokens
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           If (convert .And. num_tokens .Ge. 3) Then
 
              If (TOKENS(1) .Eq. 'WAVELENGTH') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (TOKENS(3), convert, wavelength, status)
 
!              Store value in internal data-base
                 Call IO_SET_RKEYVALUE ('WAVELENGTH', wavelength, retstat, &
                   status)
 
              Else If (TOKENS(1) .Eq. 'SAMPLE_DISTANCE') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (TOKENS(3), convert, sample_distance, status)
 
!              Store value in internal data-base
                 Call IO_SET_RKEYVALUE ('SAMPLE_DISTANCE', sample_distance, &
                   retstat, status)
 
              Else If (TOKENS(1) .Eq. 'TILT_ROTATION') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (TOKENS(3), convert, tilt_rotation, status)
 
!              Store value in internal data-base
                 Call IO_SET_RKEYVALUE ('TILT_ROTATION', tilt_rotation, &
                   retstat, status)
 
              Else If (TOKENS(1) .Eq. 'TILT_ANGLE') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (TOKENS(3), convert, tilt_angle, status)
 
!              Store value in internal data-base
                 Call IO_SET_RKEYVALUE ('TILT_ANGLE', tilt_angle, retstat, &
                   status)
 
              Else If (TOKENS(1) .Eq. 'DETECTOR_ROTATION') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (TOKENS(3), convert, detector_rotation, status)
 
!              Store value in internal data-base
                 Call IO_SET_RKEYVALUE ('DETECTOR_ROTATION', &
                   detector_rotation, retstat, status)
 
              Else If (TOKENS(1) .Eq. 'X_BEAM_CENTRE') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (TOKENS(3), convert, x_beam_centre, status)
 
!              Store value in internal data-base
                 Call IO_SET_RKEYVALUE ('X_BEAM_CENTRE', x_beam_centre, &
                   retstat, status)
 
              Else If (TOKENS(1) .Eq. 'Y_BEAM_CENTRE') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (TOKENS(3), convert, y_beam_centre, status)
 
!              Store value in internal data-base
                 Call IO_SET_RKEYVALUE ('Y_BEAM_CENTRE', y_beam_centre, &
                   retstat, status)
 
              Else If (TOKENS(1) .Eq. 'X_PIXEL_SIZE') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (TOKENS(3), convert, x_pixel_size, status)
 
              Else If (TOKENS(1) .Eq. 'Y_PIXEL_SIZE') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (TOKENS(3), convert, y_pixel_size, status)
 
              Else If (TOKENS(1) .Eq. 'X_START_ELEMENT') Then
 
!              Convert key value to an integer
                 Call IO_TOKTI (TOKENS(3), convert, xstrelm, status)
 
              Else If (TOKENS(1) .Eq. 'Y_START_ELEMENT') Then
 
!              Convert key value to an integer
                 Call IO_TOKTI (TOKENS(3), convert, ystrelm, status)
 
              Else If (TOKENS(1) .Eq. 'X_END_ELEMENT') Then
 
!              Convert key value to an integer
                 Call IO_TOKTI (TOKENS(3), convert, xendelm, status)
 
              Else If (TOKENS(1) .Eq. 'Y_END_ELEMENT') Then
 
!              Convert key value to an integer
                 Call IO_TOKTI (TOKENS(3), convert, yendelm, status)
 
              End If
 
           End If
 
        End If
 
     End Do
 
!  Close file
     Close (io_in)
 
     End Subroutine F2D_LOADGEOMETRY
!********1*********2*********3*********4*********5*********6*********7*********8
