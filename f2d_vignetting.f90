!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_vignetting.f90 *
!  *                    *
!  **********************
 
!+ F2D_VIGNETTING: mask VIGNETTING correction
     Subroutine F2D_VIGNETTING (grid_spacing, xstrelm, ystrelm, xendelm, &
       yendelm, xmax_peaks, ymax_peaks, xnumpeaks, ynumpeaks, X_PEAKS, &
       Y_PEAKS, status)
!  Description:
!    Correct off-axis peak positions for changes in centres owing to shadowing 
!    from the mask.
!  Keywords:
!    Vignetting.Correction, Correction.Vignetting
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Feb-1996: V0.4 Changes to "F2D_INP_SAMPLEDISTANCE" (Hammersley)
!    02-Feb-1996: V0.3 Change to "F2D_INP_BEAMCENTRE" arguments (Hammersley)
!    31-Aug-1995: V0.2 Add information on thickness of ESRF grid masks 
!      (Hammersley)
!    12-Aug-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: grid_spacing ! Distance in microns from centre to
!      centre of grid holes
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Integer, Intent(IN) :: xmax_peaks ! First dimension size of "X_PEAKS" and 
!      "Y_PEAKS"
     Integer, Intent(IN) :: ymax_peaks ! Second dimension size of "X_PEAKS" and 
!      "Y_PEAKS"
     Integer, Intent(IN) :: xnumpeaks ! Number of peaks in X-direction of grid
     Integer, Intent(IN) :: ynumpeaks ! Number of peaks in Y-direction of grid
!  Import/Export:
     Real, Intent(INOUT) :: X_PEAKS(xmax_peaks, ymax_peaks)
!      X-coordinates of peak centres
     Real, Intent(INOUT) :: Y_PEAKS(xmax_peaks, ymax_peaks)
!      Y-coordinates of peak centres
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Integer :: xpeak ! Loop variable for peaks in X-direction of grid
     Integer :: ypeak ! Loop variable for peaks in Y-direction of grid
     Logical, Save :: first = .True. ! .True., if it is the first call
     Real :: distance ! Distance from the beam centre to a grid hole
     Real, Save :: mask_width = 255.0e-6 ! Width of mask (shadowing part) in 
!      metres
     Real, Save :: sample_distance = 0.140 ! Distance from X-ray source to mask
!      in metres
     Real, Save :: x_beam ! X-pixel coordinate of beam on detector
     Real :: x_grid_co ! X-grid coordinate of beam
     Real :: x_pixel_size ! Effective pixel size in the X-direction
     Real :: x_shift ! Change in peak centre in X-direction in metres
     Real :: x_unit ! X-component of unit vector from beam centre to a grid hole
     Real :: x_vector ! X-component of vector from beam centre to grid hole in 
!      metres
     Real, Save :: y_beam ! Y-pixel coordinate of beam on detector
     Real :: y_grid_co ! Y-grid coordinate of beam
     Real :: y_pixel_size ! Effective pixel size in the Y-direction
     Real :: y_shift ! Change in peak centre in X-direction in metres
     Real :: y_unit ! Y-component of unit vector from beam centre to a grid hole
     Real :: y_vector ! Y-component of vector from beam centre to grid hole in 
!      metres
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(10) ! User messages
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_VIGNETTING ' // Version)
     Else
 
!     Set default beam centre
        If (first) Then
           first = .False.
           x_beam = Real(xendelm + xstrelm) / 2.0
           y_beam = Real(yendelm + ystrelm) / 2.0
        End If
 
!     Enter parameters necessary for correction
        mask_width = mask_width * 1.0e6
        MESSAGE(1) = 'Enter the thickness of the grid mask ' // &
          'in microns, which causes off-axis'
        MESSAGE(2) = 'shadowing. If the grid mask has been ' // &
          'manufacturered with counter-sunk'
        MESSAGE(3) = 'holes, e.g. the brass masks for the ' // &
          'X-ray Image Intensifiers at the'
        MESSAGE(4) = 'Photon Factory, this will probably be ' // &
          'the thickness of the final small'
        MESSAGE(5) = 'hole. (This of course assumes that the ' // &
          'largest off-axis angle is small'
        MESSAGE(6) = 'enough that it is only the final part ' // &
          'which shadows the hole.)'
        MESSAGE(7) = ' '
        MESSAGE(8) = 'At the ESRF the thin copper plated masks ' // &
          'used for the Beryllium XRII have'
        MESSAGE(9) = 'been measured to be 255 microns thick, and ' // &
          'the solid copper mask used'
        MESSAGE(10) = 'for the larger alumimium XRII should ' // &
          'be 500 microns.'
        Call IO_INPR (.True., 1.0, 1.0e5, .True., 'MASK THICKNESS (microns)', &
          10, MESSAGE, 1, 'Value must be within given range', mask_width, &
          status)
        mask_width = mask_width / 1.0e6
 
!     Input distance from the sample to the mask
        Call F2D_INP_SAMPLEDISTANCE (.False., sample_distance, status)
 
!     Input peak centre by keyboard input
        Call IO_WRITE ('INFO: The beam centre does not ' // &
          'need to be specified accurately. An', status)
        Call IO_WRITE ('      approximate centre is appropriate.', status)
        Call F2D_INP_BEAMCENTRE (.False., x_beam, y_beam, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Find grid hole coordinate of beam centre
        Call F2D_GRIDBEAMCENTRE (x_beam, y_beam, xmax_peaks, ymax_peaks, &
          xnumpeaks, ynumpeaks, X_PEAKS, Y_PEAKS, x_grid_co, y_grid_co, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Correct for mask vignetting
        Do ypeak = 1, ynumpeaks
 
           Do xpeak = 1, xnumpeaks
 
!           Check that peak has been found
              If (X_PEAKS(xpeak, ypeak) .Gt. -1.7e38) Then
 
!              Calculate vector pixels from beam centre to peak
                 x_vector = X_PEAKS(xpeak, ypeak) - x_beam
                 y_vector = Y_PEAKS(xpeak, ypeak) - y_beam
 
!              Calculate unit vector for direction of peak hole relative to the 
!              centre
                 x_unit = x_vector / Sqrt(x_vector**2 + y_vector**2)
                 y_unit = y_vector / Sqrt(x_vector**2 + y_vector**2)
 
!              Calculate vector in metres from beam centre to peak
                 distance = Sqrt((Real(xpeak) - x_grid_co)**2 + (Real(ypeak) - &
                   y_grid_co)**2)
                 x_vector = x_unit * distance
                 y_vector = y_unit * distance
 
!              Correct X-component of peak centre
                 If (Abs(x_vector) .Gt. 1.0e-10) Then
                    x_shift = mask_width * x_vector / (sample_distance * 2.0)
                    x_pixel_size = x_vector / (X_PEAKS(xpeak, ypeak) - x_beam)
                    X_PEAKS(xpeak, ypeak) = X_PEAKS(xpeak, ypeak) - x_shift / &
                      x_pixel_size
                 End If
 
!              Correct Y-component of peak centre
                 If (Abs(y_vector) .Gt. 1.0e-10) Then
                    y_shift = mask_width * y_vector / (sample_distance * 2.0)
                    y_pixel_size = y_vector / (Y_PEAKS(xpeak, ypeak) - y_beam)
                    Y_PEAKS(xpeak, ypeak) = Y_PEAKS(xpeak, ypeak) - y_shift / &
                      y_pixel_size
                 End If
 
              End If
 
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_VIGNETTING
!********1*********2*********3*********4*********5*********6*********7*********8
 
