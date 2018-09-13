!*********1*********2*********3*********4*********5*********6*********7********8
 
!  *******************
!  *                 *
!  * f2d_lorentz.f90 *
!  *                 *
!  *******************
 
!+ F2D_LORENTZ - Fit 2-D apply LORENTZ correction to 1-D data
     Subroutine F2D_LORENTZ (lorentz_geometry, variances_exist, Max_scan, &
       num_scan, ANGLES, SCAN, VARIANCES, status)
!  Description:
!    Applies various sorts to Lorentz correction to 1-D data
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Feb-1996: V0.2 Correct geometrical distance effects (Hammersley)
!    23-Oct-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: lorentz_geometry ! The Lorentz correction is
!      dependent on the experiment geometry: 
!        0 = None
!        1 = Partial correction for an ideal powder to the equivalent of a 
!            2-theta scan i.e. detector at equal distance
     Logical, Intent(IN) :: variances_exist ! .True., if the variances array
!      exists
     Integer, Intent(IN) :: max_scan ! Dimension of scan, angle, and variance 
!      arrays
     Integer, Intent(IN) :: num_scan ! Number of defined scan bins
!  Import/Export:
     Real, Intent(INOUT) :: ANGLES(max_scan) ! Angular information in degrees 
!      for each scan bin
     Real, Intent(INOUT) :: SCAN(max_scan) ! Intensity value for each scan bin
     Real, Intent(INOUT) :: VARIANCES(max_scan) ! If "variances_exist" is
!      .True., this is the variance of each scan bin intensity
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: bin ! Loop variable for scan bins
     Real :: angle ! Angle of centre of scan bin in radians
     Real :: correction_factor ! Correction factor for each bin
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LORENTZ ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (Max_scan .Le. 0) Then
        status = St_bad_dim1
     Else If (num_scan .Lt. 1 .Or. num_scan .Gt. Max_scan) Then
        status = St_bad_adr1
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_LORENTZ ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Correct to a "2-theta" scan i.e. equal distance from sample
        If (lorentz_geometry .Eq. 1) Then
 
           Do bin = 1, num_scan
 
              angle = ANGLES(bin) * Pi / 180.0
              correction_factor = 1.0 / (Cos(angle))**3
              SCAN(bin) = SCAN(bin) * correction_factor
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''angle = '', g)') ANGLES(bin)
!           Write (*, '(''correction_factor = '', g)') &
!             correction_factor
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              If (variances_exist) Then
                 VARIANCES(bin) = VARIANCES(bin) * correction_factor * &
                   correction_factor
              End If
 
           End Do
 
        End If
 
     End If
 
     End Subroutine F2D_LORENTZ
!*********1*********2*********3*********4*********5*********6*********7********8
 
