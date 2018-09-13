!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_inq_experiment.f90 *
!  *                        *
!  **************************
 
!+ F2D_INQ_EXPERIMENT: INQUIRE experimental GEOMETRY
     Subroutine F2D_INQ_EXPERIMENT (experiment, status)
!  Description:
!    Set experimental experiment
!  Keywords:
!    Geometry.Experimental.Inquire, Inquire.Geometry.Experimental,
!    Inquire.Geometry.Experimental
!  Method:
!    Stores experiment parameters in internal database
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Oct-2014: V0.10 Add "detector_offset"(Hammersley)
!    20-Jun-2006: V0.9 Add image orientation (Hammersley)
!    10-Mar-2006: V0.8 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    23-Feb-1999: V0.7 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.6 Change to use IO internal database routines (Hammersley)
!    02-Nov-1998: V0.5 Beam centre is now stored in pixel units (Hammersley)
!    27-Oct-1998: V0.4 Get values from internal data-base if defined
!      (Hammersley)
!    24-Jan-1995: V0.3 Remove saving pixel sizes, and add detector tilt angles, 
!      only set output values if the geometry is defined (Hammersley)
!    23-Apr-1993: V0.2 Detector rotation included (Hammersley)
!    10-Mar-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'io.inc'
!  Import:
!  Import/Export:
!  Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.10' ! Version number
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
        Call ST_SAVE ('Subroutine F2D_INQ_EXPERIMENT ' // Version)
     Else
 
!     Set values from internal data-base if defined

!     Pixel sizes
        Call IO_INQ_RKEYVALUE ('X_PIXEL_SIZE', experiment%x_pixel_size, &
          retstat, status)
        If (retstat .Eq. 0) Then
           experiment%pixel_sizes_set = .True.
        End If
        Call IO_INQ_RKEYVALUE ('Y_PIXEL_SIZE', experiment%y_pixel_size, &
          retstat, status)

!     Detector gain
        Call IO_INQ_RKEYVALUE ('DETECTOR_GAIN', experiment%detector_gain, &
          retstat, status)
        If (retstat .Ne. 0) Then

!        Try old keyword
           Call IO_INQ_RKEYVALUE ('PEAK_SEARCH_DET_GAIN', &
             experiment%detector_gain, retstat, status)

        End If

!     Wavelength
        Call IO_INQ_RKEYVALUE ('WAVELENGTH', experiment%wavelength, retstat, &
          status)
        If (retstat .Eq. 0) Then
           experiment%wavelength_set = .True.
        End If

!     Polarisation
        Call IO_INQ_RKEYVALUE ('POLARISATION', experiment%polarisation, &
          retstat, status)
        If (retstat .Eq. 0) Then
           experiment%polarisation_set = .True.
        End If
        Call IO_INQ_LKEYVALUE ('CORRECT_POLARISATION', &
          experiment%correct_polarisation, retstat, status)

!     Sample to detector distance
        Call IO_INQ_RKEYVALUE ('DETECTOR_DISTANCE', &
          experiment%detector_distance, retstat, status)
        If (retstat .Eq. 0) Then
           experiment%detector_distance_set = .True.
        Else

!        Try old keyword
           Call IO_INQ_RKEYVALUE ('SAMPLE_DISTANCE', &
             experiment%detector_distance, retstat, status)
           If (retstat .Eq. 0) Then
              experiment%detector_distance_set = .True.
           End If

        End If

!     Beam centre
        Call IO_INQ_RKEYVALUE ('X_BEAM_CENTRE', experiment%x_beam, retstat, &
          status)
        If (retstat .Eq. 0) Then
           experiment%beam_centre_set = .True.
        End If
        Call IO_INQ_RKEYVALUE ('Y_BEAM_CENTRE', experiment%y_beam, retstat, &
          status)

!     Detector non-orthogonality and rotation
        Call IO_INQ_RKEYVALUE ('TILT_ROTATION', &
          experiment%tilt_plane_rotation, retstat, status)
        If (retstat .Eq. 0) Then
           experiment%tilt_set = .True.
        End If
        Call IO_INQ_RKEYVALUE ('TILT_ANGLE', experiment%tilt_angle, retstat, &
          status)
        Call IO_INQ_RKEYVALUE ('DETECTOR_ROTATION', &
          experiment%detector_rotation, retstat, status)

!     Detector 2-theta
        Call IO_INQ_DKEYVALUE ('DETECTOR_TWO_THETA', experiment%two_theta, &
          retstat, status)
        If (retstat .Eq. 0) Then
           experiment%two_theta_set = .True.
        End If
        Call IO_INQ_LKEYVALUE ('VERTICAL_2THETA', &
          experiment%vertical_2theta, retstat, status)

!     Goniometer angles
        Call IO_INQ_DKEYVALUE ('CHI_START', experiment%chi_start, &
          retstat, status)

        If (retstat .Eq. 0) Then

           experiment%goniometer_set = .True.
           Call IO_INQ_DKEYVALUE ('CHI_END', experiment%chi_end, &
             retstat, status)
           Call IO_INQ_DKEYVALUE ('PHI_START', experiment%phi_start, &
             retstat, status)
           Call IO_INQ_DKEYVALUE ('PHI_END', experiment%phi_end, &
             retstat, status)
           Call IO_INQ_DKEYVALUE ('OMEGA_START', experiment%omega_start, &
             retstat, status)
           Call IO_INQ_DKEYVALUE ('OMEGA_END', experiment%omega_end, &
             retstat, status)

        End If

!     Image orientation
        Call IO_INQ_LKEYVALUE ('VIEW_FROM_SAMPLE', &
          experiment%view_from_sample, retstat, status)
        If (retstat .Eq. 0) Then
           experiment%image_orientation_set = .True.
        End If

!     Unit Cell
        Call IO_INQ_RKEYVALUE ('A_UNIT_CELL', experiment%cell_length_a, &
          retstat, status)

        If (retstat .Eq. 0) Then
           experiment%cell_set = .True.

           Call IO_INQ_RKEYVALUE ('B_UNIT_CELL', experiment%cell_length_b, &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('C_UNIT_CELL', experiment%cell_length_c, &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('ALPHA_UNIT_CELL', experiment%cell_alpha, &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('BETA_UNIT_CELL', experiment%cell_beta, &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('GAMMA_UNIT_CELL', experiment%cell_gamma, &
             retstat, status)

        End If

!     UB Matrix
        Call IO_INQ_RKEYVALUE ('UB_1_1', experiment%UB_MATRIX(1, 1), retstat, &
          status)

        If (retstat .Eq. 0) Then

           experiment%ub_matrix_set = .True.
           Call IO_INQ_RKEYVALUE ('UB_1_2', experiment%UB_MATRIX(1, 2), &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('UB_1_3', experiment%UB_MATRIX(1, 3), &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('UB_2_1', experiment%UB_MATRIX(2, 1), &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('UB_2_2', experiment%UB_MATRIX(2, 2), &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('UB_2_3', experiment%UB_MATRIX(2, 3), &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('UB_3_1', experiment%UB_MATRIX(3, 1), &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('UB_3_2', experiment%UB_MATRIX(3, 2), &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('UB_3_3', experiment%UB_MATRIX(3, 3), &
             retstat, status)

        End If

        Call IO_INQ_RKEYVALUE ('DETECTOR_OFFSET', &
          experiment%detector_offset, retstat, status)

     End If
  
     End Subroutine F2D_INQ_EXPERIMENT
!********1*********2*********3*********4*********5*********6*********7*********8
 
