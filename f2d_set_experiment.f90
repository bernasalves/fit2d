!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_set_experiment.f90 *
!  *                        *
!  **************************
 
!+ F2D_SET_EXPERIMENT: SET experimental GEOMETRY
     Subroutine F2D_SET_EXPERIMENT (experiment, status)
!  Description:
!    Set experimental geometry into internal data-base
!  Keywords:
!    Geometry.Experimental.Set, Set.Geometry.Experimental,
!    Set.Geometry.Experimental
!  Method:
!    Stores geometry parameters in internal database
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Oct-2014: V0.9 Add "detector_offset"(Hammersley)
!    20-Jun-2006: V0.8 Add image orientation (Hammersley)
!    13-Mar-2006: V0.7 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    23-Feb-1999: V0.6 All data-base saving and recovering routines renamed
!      (Hammersley)
!    15-Dec-1998: V0.5 Change to use IO internal database routines (Hammersley)
!    02-Nov-1998: V0.4 Set values in data-base (Hammersley)
!    24-Jan-1995: V0.3 Remove saving pixel sizes, and add detector tilt angles, 
!      only set output values if the geomentry is defined (Hammersley)
!    23-Apr-1993: V0.2 Detector rotation angle included (Hammersley)
!    10-Mar-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'io.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.9' ! Version number
!  Local Variables:
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
        Call ST_SAVE ('Subroutine F2D_SET_EXPERIMENT ' // Version)
     Else

!     Set values from internal data-base if defined
        If (experiment%pixel_sizes_set) Then
           Call IO_SET_RKEYVALUE ('X_PIXEL_SIZE', experiment%x_pixel_size, &
             retstat, status)
           Call IO_SET_RKEYVALUE ('Y_PIXEL_SIZE', experiment%y_pixel_size, &
             retstat, status)
        End If

!     Detector gain
        Call IO_SET_RKEYVALUE ('DETECTOR_GAIN', experiment%detector_gain, &
          retstat, status)

!     Wavelength
        If (experiment%wavelength_set) Then
           Call IO_SET_RKEYVALUE ('WAVELENGTH', experiment%wavelength, &
             retstat, status)
        End If

!     Polarisation
        If (experiment%polarisation_set) Then
           Call IO_SET_RKEYVALUE ('POLARISATION', experiment%polarisation, &
             retstat, status)
           Call IO_SET_LKEYVALUE ('CORRECT_POLARISATION', &
             experiment%correct_polarisation, retstat, status)
        End If

!     Sample to detector distance
        If (experiment%detector_distance_set) Then
           Call IO_SET_RKEYVALUE ('DETECTOR_DISTANCE', &
             experiment%detector_distance, retstat, status)
        End If

!     Beam centre
        If (experiment%beam_centre_set) Then
           Call IO_SET_RKEYVALUE ('X_BEAM_CENTRE', experiment%x_beam, &
             retstat, status)
           Call IO_SET_RKEYVALUE ('Y_BEAM_CENTRE', experiment%y_beam, &
             retstat, status)
        End If

!     Detector non-orthogonality and rotation
        If (experiment%tilt_set) Then
           Call IO_SET_RKEYVALUE ('TILT_ROTATION', &
             experiment%tilt_plane_rotation, retstat, status)
           Call IO_SET_RKEYVALUE ('TILT_ANGLE', experiment%tilt_angle, &
             retstat, status)
        End If
        Call IO_SET_RKEYVALUE ('DETECTOR_ROTATION', &
          experiment%detector_rotation, retstat, status)

!     Detector 2-theta
        If (experiment%two_theta_set) Then
           Call IO_SET_DKEYVALUE ('DETECTOR_TWO_THETA', experiment%two_theta, &
             retstat, status)
           Call IO_SET_LKEYVALUE ('VERTICAL_2THETA', &
             experiment%vertical_2theta, retstat, status)
        End If

!     Goniometer angles
        If (experiment%goniometer_set) Then

           Call IO_SET_DKEYVALUE ('CHI_START', experiment%chi_start, &
             retstat, status)
           Call IO_SET_DKEYVALUE ('CHI_END', experiment%chi_end, &
             retstat, status)
           Call IO_SET_DKEYVALUE ('PHI_START', experiment%phi_start, &
             retstat, status)
           Call IO_SET_DKEYVALUE ('PHI_END', experiment%phi_end, &
             retstat, status)
           Call IO_SET_DKEYVALUE ('OMEGA_START', experiment%omega_start, &
             retstat, status)
           Call IO_SET_DKEYVALUE ('OMEGA_END', experiment%omega_end, &
             retstat, status)

        End If

!     Image orientation
        If (experiment%image_orientation_set) Then
           Call IO_SET_LKEYVALUE ('VIEW_FROM_SAMPLE', &
             experiment%view_from_sample, retstat, status)
        End If

!     Unit Cell
        If (experiment%cell_set) Then
           Call IO_SET_RKEYVALUE ('A_UNIT_CELL', experiment%cell_length_a, &
             retstat, status)
           Call IO_SET_RKEYVALUE ('B_UNIT_CELL', experiment%cell_length_b, &
             retstat, status)
           Call IO_SET_RKEYVALUE ('C_UNIT_CELL', experiment%cell_length_c, &
             retstat, status)
           Call IO_SET_RKEYVALUE ('ALPHA_UNIT_CELL', experiment%cell_alpha, &
             retstat, status)
           Call IO_SET_RKEYVALUE ('BETA_UNIT_CELL', experiment%cell_beta, &
             retstat, status)
           Call IO_SET_RKEYVALUE ('GAMMA_UNIT_CELL', experiment%cell_gamma, &
             retstat, status)

        End If

!     UB Matrix
        If (experiment%ub_matrix_set) Then

           Call IO_SET_RKEYVALUE ('UB_1_1', experiment%UB_MATRIX(1, 1), &
             retstat, status)
           Call IO_SET_RKEYVALUE ('UB_1_2', experiment%UB_MATRIX(1, 2), &
             retstat, status)
           Call IO_SET_RKEYVALUE ('UB_1_3', experiment%UB_MATRIX(1, 3), &
             retstat, status)
           Call IO_SET_RKEYVALUE ('UB_2_1', experiment%UB_MATRIX(2, 1), &
             retstat, status)
           Call IO_SET_RKEYVALUE ('UB_2_2', experiment%UB_MATRIX(2, 2), &
             retstat, status)
           Call IO_SET_RKEYVALUE ('UB_2_3', experiment%UB_MATRIX(2, 3), &
             retstat, status)
           Call IO_SET_RKEYVALUE ('UB_3_1', experiment%UB_MATRIX(3, 1), &
             retstat, status)
           Call IO_SET_RKEYVALUE ('UB_3_2', experiment%UB_MATRIX(3, 2), &
             retstat, status)
           Call IO_SET_RKEYVALUE ('UB_3_3', experiment%UB_MATRIX(3, 3), &
             retstat, status)

        End If
 
        Call IO_SET_RKEYVALUE ('DETECTOR_OFFSET', &
          experiment%detector_offset, retstat, status)

     End If
 
     End Subroutine F2D_SET_EXPERIMENT
!********1*********2*********3*********4*********5*********6*********7*********8
