!********1*********2*********3*********4*********5*********6*********7**
 
!  ****************
!  *              *
!  * f2d_full.f90 *
!  *              *
!  ****************
 
!+ F2D_FULL - FIT 2-D: FULL region
     Subroutine F2D_FULL (xnumdat, ynumdat, xstrelm, ystrelm, xendelm, &
       yendelm, status)
!  Description:
!    Sets full active data region, or full scale area for 1-D data-sets
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    15-Dec-1996: V0.2 Improve handling of 1-D data (Hammersley)
!    08-Mar-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements defined in
!      Y-direction
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: xendelm ! The X-pixel number for the end of
!      the ROI
     Integer, Intent(INOUT) :: yendelm ! The Y-pixel number for the end of
!      the ROI
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Logical :: xmax_auto ! .True., if the upper X-display range is automatic
     Logical :: xmin_auto ! .True., if the lower X-display range is automatic
     Logical :: ymax_auto ! .True., if the upper Y-display range is automatic
     Logical :: ymin_auto ! .True., if the lower Y-display range is automatic
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FULL ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_FULL ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Arguments would appear to be reasonable, go ahead.
 
!     Check the 1-D or 2-D nature of the data
        If (xendelm .Eq. xstrelm .Or. yendelm .Eq. ystrelm) Then
 
!        Find out present scaling
           Call GS_INQ_AUTODDR (xmin_auto, ymin_auto, xmax_auto, ymax_auto, &
             status)
 
           If ((.Not. xmin_auto) .Or. (.Not. ymin_auto) .Or. (.Not. ymin_auto) &
             .Or. (.Not. ymax_auto)) Then
 
!           The 1-D region is assumed to have been zoomed-in, so
!           set fully automatic
              Call GS_SET_AUTODDR (.True., .True., .True., .True., status)
 
              If (yendelm .Eq. ystrelm) Then
 
!              Set full 1-D X-region
                 xstrelm = 1
                 xendelm = xnumdat
 
              Else
 
!              Set full 1-D Y-region
                 ystrelm = 1
                 yendelm = ynumdat
 
              End If
 
           Else
 
!           The 1-D data is fully displayed, full 2-D
              xstrelm = 1
              ystrelm = 1
              xendelm = xnumdat
              yendelm = ynumdat
 
           End If
 
        Else
 
!        2-D data
           xstrelm = 1
           ystrelm = 1
           xendelm = xnumdat
           yendelm = ynumdat
 
        End If
 
     End If
 
     End Subroutine F2D_FULL
!********1*********2*********3*********4*********5*********6*********7**
 
 
