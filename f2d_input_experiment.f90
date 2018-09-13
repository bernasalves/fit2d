!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************************
!  *                          *
!  * f2d_input_experiment.f90 *
!  *                          *
!  ****************************
 
!+ F2D_INPUT_EXPERIMENT -  INPUT EXPERIMENTal details
     Subroutine F2D_INPUT_EXPERIMENT (input_options, experiment, status)
!  Description:
!    User input of experimental geometry.
!  Keywords:
!    Geometry.Experimental, Experiment.Geometry
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    10-Nov-2006: V0.12 Set default name for input file (Hammersley)
!    17-Sep-2006: V0.11 Add "monitor" and "attenuation" items (Hammersley)
!    24-Apr-2006: V0.10 Input start and end diffractometer angles (Hammersley)
!    30-Mar-2006: V0.9 Calculate inverse UB matrix (Hammersley)
!    23-Mar-2006: V0.8 Output details of beam centre (Hammersley)
!    20-Mar-2006: V0.7 Add "INPUT OPTIONS" support (Hammersley)
!    16-Mar-2006: V0.6 Correct output of 2-theta rotation (Hammersley)
!    10-Mar-2006: V0.5 "EXPERIMENTAL_DETAILS" structure argument (Hammersley)
!    09-Mar-2006: V0.4 Output details of UB matrix if input (Hammersley)
!    08-Mar-2006: V0.3 Output details of unit cell if input (Hammersley)
!    04-Mar-2006: V0.2 Change structure name (Hammersley)
!    23-Feb-2006: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use MA_LIB
     Use GS_LIB
!     Use FIO_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.12' ! Version number 
!  Local Variables:
     Character(Len = 256), Save :: file_name = 'experimental_geometry.cif' 
!      File defining experimental geometry
     Integer :: retstat ! Status return variable
!  Local Arrays:
     Character(Len = 80) :: INFO(1) ! User information
     Character(Len = 80) :: MESSAGE(1) ! User text
!  Local Data Structures:
!  External Functions:
!  Local Data:

!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INPUT_EXPERIMENT ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8

!     Select file for input of auxiliary experimental geometry 
        MESSAGE(1) = &
          'SELECT CIF or CBF FILE containing experimental geometry'
        INFO(1) = 'The file must already exist'
        Call GS_FILESELECTION (1, 1, MESSAGE, &
          1, 1, INFO, 3, .False., retstat, file_name, status)

!     Inform user / Log file
        Call IO_WRITE ('Inputting experimental geometry from file:', status)
        Call IO_WRITE (file_name, status)

!     Input auxiliary experimental geometry information from file
        Call FIO_IN_AUXILIARY (input_options, file_name, retstat, experiment, &
          status)

        If (experiment%pixel_sizes_set) Then
           Write (MESSAGE(1), &
             '(''1st dimension pixel size = '', f12.5, '' microns'')') &
             experiment%x_pixel_size * 1.0e6
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), &
             '(''2nd dimension pixel size = '', f12.5, '' microns'')') &
             experiment%y_pixel_size * 1.0e6
           Call IO_WRITE (MESSAGE(1), status)
        End If


        If (experiment%wavelength_set) Then
           Write (MESSAGE(1), &
             '(''Wavelength = '', f12.5, '' Angstroms'')') &
             experiment%wavelength * 1.0e10
           Call IO_WRITE (MESSAGE(1), status)
        End If

        If (experiment%detector_distance_set) Then
           Write (MESSAGE(1),  '(''Sample to detector distance ' // &
             '= '', f12.5, '' metres'')') experiment%detector_distance
           Call IO_WRITE (MESSAGE(1), status)
        End If

        If (experiment%beam_centre_set) Then

           Write (MESSAGE(1),  '(''Beam centre = ( '', f8.2, '', '', f8.2,' // &
             ''') pixels'')') experiment%x_beam, experiment%y_beam 
           Call IO_WRITE (MESSAGE(1), status)

        End If

        If (experiment%two_theta_set) Then

           If (experiment%vertical_2theta) Then
              Write (MESSAGE(1), '(''Vertical two theta rotation'')') 
           Else
              Write (MESSAGE(1), '(''Horizontal two theta rotation'')') 
           End If
           Call IO_WRITE (MESSAGE(1), status)

           Write (MESSAGE(1), &
             '(''Two theta = '', f12.5, '' degrees'')') &
             experiment%two_theta * 180.0 / Pi
           Call IO_WRITE (MESSAGE(1), status)

        End If

        If (experiment%cell_set) Then

!        Calculate inverse of UB matrix
           Call MA_MATINVERSE (3, 3, experiment%UB_MATRIX, 3, retstat, &
             experiment%INV_UB, status)
 
           If (retstat .Ne. 0) Then
              Call GS_FWARNING (1, 1, 'The UB matrix could not ' // &
                'be inverted; re-enter values', status)
              experiment%ub_matrix_set = .False.
           Else
           
              Write (MESSAGE(1), '(''Cell length a = '', f12.5,' // &
                ''' Angstroms'')') experiment%cell_length_a * 1.0e10
              Call IO_WRITE (MESSAGE(1), status)
              Write (MESSAGE(1), '(''Cell length b = '', f12.5, ' // &
                ''' Angstroms'')') experiment%cell_length_b * 1.0e10
              Call IO_WRITE (MESSAGE(1), status)
              Write (MESSAGE(1), '(''Cell length c = '', f12.5, ' // &
                ''' Angstroms'')') experiment%cell_length_c * 1.0e10
              Call IO_WRITE (MESSAGE(1), status)
              Write (MESSAGE(1), '(''Cell angle alpha = '', f12.5, ' // &
                ''' degrees'')') experiment%cell_alpha * 180.0 / Pi
              Call IO_WRITE (MESSAGE(1), status)
              Write (MESSAGE(1), '(''Cell angle beta = '', f12.5, ' // &
                ''' degrees'')') experiment%cell_beta * 180.0 / Pi
              Call IO_WRITE (MESSAGE(1), status)
              Write (MESSAGE(1), '(''Cell angle gamma = '', f12.5, ' // &
                ''' degrees'')') experiment%cell_gamma * 180.0 / Pi
              Call IO_WRITE (MESSAGE(1), status)

           End If

        End If

        If (experiment%ub_matrix_set) Then
           
           Write (MESSAGE(1), '(''UB Matrix = '', 3g14.6)') &
             experiment%UB_MATRIX(1, 1), experiment%UB_MATRIX(1, 2), &
             experiment%UB_MATRIX(1, 3)
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''            '', 3g14.6)') &
             experiment%UB_MATRIX(2, 1), experiment%UB_MATRIX(2, 2), &
             experiment%UB_MATRIX(2, 3)
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''            '', 3g14.6)') &
             experiment%UB_MATRIX(3, 1), experiment%UB_MATRIX(3, 2), &
             experiment%UB_MATRIX(3, 3)
           Call IO_WRITE (MESSAGE(1), status)

        End If

        If (experiment%goniometer_set) Then
           
           Write (MESSAGE(1), '(''chi = '', f12.5, '' degrees'')') &
             experiment%chi_start * 180.0 / Pi_d
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''Phi = '', f12.5, '' degrees'')') &
             experiment%phi_start * 180.0 / Pi_d
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''Omega = '', f12.5, '' degrees'')') &
             experiment%omega_start * 180.0 / Pi_d
           Call IO_WRITE (MESSAGE(1), status)

        End If

     End If

     End Subroutine F2D_INPUT_EXPERIMENT
!********1*********2*********3*********4*********5*********6*********7*********8
