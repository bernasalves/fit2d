!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_def_experiment.f90 *
!  *                        *
!  **************************
 
!+ F2D_DEF_EXPERIMENT: define DEFault EXPERIMENT geometry and details
     Subroutine F2D_DEF_EXPERIMENT (experiment, status)
!  Description:
!    Set default experiment details including geometry
!  Keywords:
!    Experiment.Details.Default, Default.Experiment.Details,
!    Default.Experiment.Geometry
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    18-Sep-2006: V0.3 Add "monitor" and "attenuation" items (Hammersley)
!    24-Mar-2006: V0.2 Diffractometer start and end angles can now be defined
!      (Hammersley)
!    15-Mar-2006: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Export:
     Type(EXPERIMENTAL_DETAILS), Intent(OUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DEF_EXPERIMENT ' // Version)
     Else
 
!     Initialise with sensible experimental geometry values
        experiment%x_pixel_size = 1.0e-4
        experiment%y_pixel_size = 1.0e-4
        experiment%pixel_sizes_set = .True.

        experiment%detector_gain = 1.0

        experiment%wavelength_set = .True.
        experiment%wavelength = 1.0e-10

        experiment%polarisation_set = .True.
        experiment%correct_polarisation = .True.
        experiment%polarisation = 0.99

        experiment%detector_distance_set = .True.
        experiment%detector_distance = 0.300 

        experiment%beam_centre_set = .True.
        experiment%x_beam = 1024.0
        experiment%y_beam = 1024.0

        experiment%tilt_set = .True.
        experiment%detector_rotation = 0.0
        experiment%tilt_plane_rotation = 0.0
        experiment%tilt_angle = 0.0

        experiment%two_theta_set = .True.
        experiment%vertical_2theta = .True. 
        experiment%two_theta = 0.0

        experiment%goniometer_set = .False.
        experiment%chi_start = 0.0
        experiment%chi_end = 0.0
        experiment%phi_start = 0.0
        experiment%phi_end = 0.0
        experiment%omega_start = 0.0
        experiment%omega_end = 0.0

        experiment% image_orientation_set = .True.
        experiment%view_from_sample = .True.

        experiment%cell_set = .False. 
        experiment%cell_length_a = 6.14
        experiment%cell_length_b = 6.14
        experiment%cell_length_c = 6.14
        experiment%cell_alpha = 90.0
        experiment%cell_beta = 90.0
        experiment%cell_gamma = 90.0

        experiment%ub_matrix_set = .False.

        experiment%monitor_set = .False.
        experiment%attenuation_set = .False.

     End If

     End Subroutine F2D_DEF_EXPERIMENT
!********1*********2*********3*********4*********5*********6*********7*********8
