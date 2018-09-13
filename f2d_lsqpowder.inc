!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_lsqpowder.inc *
!  *                   *
!  *********************
 
!+ FIT2D: Powder RING FIT control parameters passing Common from
!    "F2D_LSQPOWDER" to "F2D_FUNPOWDER"
 
!  Description:
!    Because is used to minimise the fit function it is
!    necessary to pass some variable through common. This include file
!    makes that safer.
!  Keywords:
!    Fit.Common
!  Method:
!    Common blocks
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    22-Aug-1996: V0.4 Add option of refining the tilt or not
!      (Hammersley)
!    14-Jun-1996: V0.3 Add control of refining or not tilt, and
!      wavelength (Hammersley)
!    19-Feb-1995: V0.2 Choice of weighted fitting or not (Hammersley)
!    27-Jan-1995: V0.1 Original (Hammersley)
!  Global Constants:
     Integer, Parameter :: Max_parameters = 20 ! Dimension of the variable
!      parameters array
!    Global Variables:
     Integer :: f2d_num_rings ! Number of rings to fit
     Logical :: f2d_refine_beam_centre ! .True., if the beam centre
!      coordinates are to be refined
     Real :: f2d_x_beam ! X-coordinate of beam centre (metres)
     Real :: f2d_y_beam ! Y-coordinate of beam centre (metres)
     Logical :: f2d_weighted_fitting ! .True., if the fitting of tilt and
!      beam centre is to be weighted by the intensities
     Real :: f2d_detector_gain ! The effective gain of the detector which
!      produced the intensity values. This is used to estimate the
!      variance of each intensity value to give a justified weighting
     Logical :: f2d_refine_sample_distance ! .True., if the sample to
!      detector distance is to be refined
     Real :: f2d_sample_distance ! Sample to detector distance (metres)
     Logical :: f2d_refine_wavelength ! .True., if the wavelength
!      is to be refined
     Real :: f2d_wavelength ! Wavelength in metres
     Logical :: f2d_refine_tilt ! .True., if the detector
!      non-orthogonality is to be refined
     Real :: f2d_tilt_plane ! Angle of rotation for the tilt plane
!      in the Z = 0 plane (radians)
     Real :: f2d_tilt_angle ! Angle of detector tilt in the rotated
!      Y-direction (radians)
!    Global Arrays:
!    Common:
     Common / F2D_CONTROLPOWDER / f2d_num_rings, f2d_refine_beam_centre, &
       f2d_x_beam, f2d_y_beam, f2d_refine_sample_distance, &
       f2d_sample_distance, f2d_weighted_fitting, f2d_detector_gain, &
       f2d_refine_wavelength, f2d_wavelength, f2d_refine_tilt, f2d_tilt_plane, &
       f2d_tilt_angle
 
!  End of f2d_lsqpowder.inc
!********1*********2*********3*********4*********5*********6*********7*********8
