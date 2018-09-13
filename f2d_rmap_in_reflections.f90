!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************************
!  *                             *
!  * f2d_rmap_in_reflections.f90 *
!  *                             *
!  *******************************
 
!+ F2D_RMAP_IN_REFLECTIONS: Reciprocal MAP input UB MATRIX
     Subroutine F2D_RMAP_IN_REFLECTIONS (input_options, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, &
       XAXIS, YAXIS, DATA, title, xlabel, ylabel, zlabel, experiment, retstat, &
       status)
!  Description:
!    Calculate UB matrix elements from input of unit cell and two
!    detector image input reflections.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    20-Jun-2006: V0.10 Take into account image sense (Hammersley)
!    24-Apr-2006: V0.9 Add "INPUT_OPTIONS" structure (Hammersley)
!    30-Mar-2006: V0.8 Changes to "F2D_GUI_UNITCELL" (Hammersley)
!    14-Mar-2006: V0.7 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    14-Feb-2005: V0.6 Set "retstat" value (Hammersley)
!    14-Oct-2005: V0.5 Add experiment geometry parameters (Hammersley)
!    13-Oct-2005: V0.4 Correct image display (Hammersley)
!    05-Oct-2005: V0.3 Changes to argument list (Hammersley)
!    21-Jul-2005: V0.2 Changes to call to "F2D_RMAP_CAL_UMATRIX" (Hammersley)
!    09-Jun-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
     Integer, Intent(IN) :: xmaxdat ! First dimension of array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array
!  Import/Export:
     Integer, Intent(INOUT) :: xnumdat ! Defines X-extent of defined data
     Integer, Intent(INOUT) :: ynumdat ! Defines Y-extent of defined data
     Real, Intent(INOUT) :: XAXIS(xmaxdat) ! Array to contain X-coordinate
!      grid data
     Real, Intent(INOUT) :: YAXIS(ymaxdat) ! Array to contain Y-coordinate
!      grid data
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Array to contain data
!      values
     Character(Len = *), Intent(INOUT) :: title ! Title for data
     Character(Len = *), Intent(INOUT) :: xlabel ! Label for X-axis of data
     Character(Len = *), Intent(INOUT) :: ylabel ! Label for Y-axis of data
     Character(Len = *), Intent(INOUT) :: zlabel ! Label for Z-axis of data
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
     Integer, Intent(OUT) :: retstat ! Status return:
!      0 = Good status
!      1 = Cancel used
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.10' ! Version number
!  Local Variables:
     Character(Len = 256) :: input_file = 'reflection.dat' ! Name of input file
     Character(Len = 80) :: message ! User messages
     Integer :: h1 = 1 ! h index of primary reflection
     Integer :: h2 = 2 ! h index of secondary reflection
     Integer :: k1 = 1 ! k index of primary reflection
     Integer :: k2 = 2 ! k index of secondary reflection
     Integer :: l1 = 1 ! l index of primary reflection
     Integer :: l2 = 1 ! l index of secondary reflection
     Integer :: len_string ! Length of returned string
     Integer :: num_coordinates ! Number of input coordinates
     Logical :: data_defined ! .True. if the data is defined
     Real :: chi_1 = -88.8867 ! Chi angle (degrees) of diffractometer for
!      primary reflection
     Real :: chi_2 = -88.9536 ! Chi angle (degrees) of diffractometer for
!      secondary reflection
     Real :: d_1 ! D-spacing of primary reflection in Angstroms
     Real :: d_2 ! D-spacing of secondary reflection in Angstroms
     Real :: degrees ! Dummy variable for "Radians" function
     Real :: dummy ! Dummy variable
     Real :: gamma = 90.0 ! gamma angle (degrees) of unit cell
     Real :: omega_1 = 24.3999 ! Omega angle (degrees) of diffractometer
!      for primary reflection
     Real :: omega_2 = 30.4021 ! Omega angle (degrees) of diffractometer
!      for secondary reflection
     Real :: phi_1 = 0.0 ! Phi angle (degrees) of diffractometer for
!      primary reflection
     Real :: phi_2 = 0.0 ! Phi angle (degrees) of diffractometer for
!      secondary reflection
     Real :: two_theta_1 = 49.7484 ! Two theta angle (degrees) of primary
!      refection
     Real :: two_theta_2 = 93.5294 ! Two theta angle (degrees) of secondary
!      refection
     Real :: x_coordinate ! Entered X-coordinate of click point
     Real :: y_coordinate ! Entered Y-coordinate of click point
!  Local Arrays:
     Character(Len = 80) :: PROMPT(2) ! User messages
     Real :: B_MATRIX (3, 3) ! B matrix of crystal unit cell diffractometer
     Real Z1(3) ! Cartesian coordinate of primary reflection
     Real Z2(3) ! Cartesian coordinate of secondary reflection
!  External Functions:
!  Local Data:
!    Internal Functions:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_IN_REFLECTIONS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
 
!     Input unit cell parameters
        Call F2D_GUI_UNITCELL (experiment, status)
 
!     Calculate B matrix from unit cell parameters
        Call F2D_RMAP_CAL_BMATRIX (experiment, B_MATRIX, status)

!     Try to get name of primary reflection file from data-base
        Call IO_INQ_KEYVALUE ('PRIMARY_REFLECTION_FILE', len_string, &
          input_file, retstat, status)
        
!     Input image with primary reflection
        PROMPT(1) = 'SELECT FILE CONTAINING PRIMARY REFLECTION'
        PROMPT(2) = '(click on "HELP" for list of formats)'
        Call FIO_GUI_INPUT (2, 2, PROMPT, 0, INPUT_OPTIONS, &
          xmaxdat, ymaxdat, .False., &
          data_defined, input_file, xnumdat, ynumdat, XAXIS, YAXIS, DATA, &
          dummy, title, xlabel, ylabel, zlabel, experiment, status)
 
!     Set name of primary reflection file from data-base
        Call IO_SET_KEYVALUE ('PRIMARY_REFLECTION_FILE', Len_trim(input_file), &
          input_file, retstat, status)

!     Display image
        Call GS_PLOT (xmaxdat, ymaxdat, DATA, XAXIS, YAXIS, 1, 1, xnumdat, &
          ynumdat, title, xlabel, ylabel, zlabel, status)
 
!     Click on primary reflection
        num_coordinates = 1
        Call GS_INPS_FCOORDINATES (.False., .True., xmaxdat, ymaxdat, 1, 1, &
          xnumdat, ynumdat, DATA, dummy, XAXIS, YAXIS, title, xlabel, ylabel, &
          zlabel, "CLICK ON PRIMARY REFLECTION", 1, "Click on the first peak &
          for orientation determination", .False., 1, num_coordinates, &
          x_coordinate, y_coordinate, status)
 
!     Input diffractometer angles of primary reflection
        Call F2D_GUI_REFLECTION (.True., 1, 'ENTER PRIMARY REFLECTION', &
          h1, k1, l1, experiment, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''experiment%view_from_sample = '', l1)') &
!          experiment%view_from_sample
!        Write (*, '(''experiment%vertical_2theta = '', l1)') &
!          experiment%vertical_2theta
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Calculate Cartesian coordinate of first reflection
        Call F2D_RMAP_CAL_CARTESIAN (0, x_coordinate, y_coordinate, &
          experiment, Z1, status)
 
!     Try to get name of primary reflection file from data-base
        Call IO_INQ_KEYVALUE ('SECONDARY_REFLECTION_FILE', len_string, &
          input_file, retstat, status)
        
!     Input image with secondary reflection
        PROMPT(1) = 'SELECT FILE CONTAINING SECONDARY REFLECTION'
        PROMPT(2) = '(click on "HELP" for list of formats)'
        Call FIO_GUI_INPUT (2, 2, PROMPT, 0, INPUT_OPTIONS, &
          xmaxdat, ymaxdat, .False., &
          data_defined, input_file, xnumdat, ynumdat, XAXIS, YAXIS, DATA, &
          dummy, title, xlabel, ylabel, zlabel, experiment, status)
 
!     Set name of secondary reflection file from data-base
        Call IO_SET_KEYVALUE ('SECONDARY_REFLECTION_FILE', &
          Len_trim(input_file), input_file, retstat, status)
        
!     Display image
        Call GS_PLOT (xmaxdat, ymaxdat, DATA, XAXIS, YAXIS, 1, 1, xnumdat, &
          ynumdat, title, xlabel, ylabel, zlabel, status)
 
!     Input image with secondary reflection
        num_coordinates = 1
        Call GS_INPS_FCOORDINATES (.False., .True., xmaxdat, ymaxdat, 1, 1, &
          xnumdat, ynumdat, DATA, dummy, XAXIS, YAXIS, title, xlabel, ylabel, &
          zlabel, "CLICK ON SECONDARY REFLECTION", 1, "Click on the second &
          peak for orientation determination", .False., 1, num_coordinates, &
          x_coordinate, y_coordinate, status)
 
!     Input diffractometer angles of secondary reflection
        Call F2D_GUI_REFLECTION (.True., 2, 'ENTER SECONDARY REFLECTION', &
          h2, k2, l2, experiment, status)
 
!     Calculate Cartesian coordinate of secondary reflection
        Call F2D_RMAP_CAL_CARTESIAN (0, x_coordinate, y_coordinate, &
          experiment, Z2, status)

!     Calculate UB matrix from 2 cartesian vectors and two HKL vectors
        Call F2D_RMAP_CAL_U3MATRIX (B_MATRIX, h1, k1, l1, Z1, &
          h2, k2, l2, Z2, experiment, status)

!     Set good status
        retstat = 0

     End If
 
     End Subroutine F2D_RMAP_IN_REFLECTIONS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
