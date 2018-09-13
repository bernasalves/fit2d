!********1*********2*********3*********4*********5*********6*********7**
 
!  ***********************
!  *                     *
!  * f2d_fitgaussian.inc *
!  *                     *
!  ***********************
 
!+ FIT2D: GAUUSSIAN FIT data passing Common
 
!  Description:
!    Because "E04FCF" is used to minimise the fit function it is
!    necessary to pass some variable through common. This include file
!    makes that safer.
!  Keywords:
!    Fit.E04FCF.Common
!  Method:
!    Common blocks
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    07-Feb-1996: V0.1 Original (Hammersley)
!  Global Constants:
!    Global Variables:
     Integer :: fit_xstrelm ! X-start of data fitting region
     Integer :: fit_ystrelm ! Y-start of data fitting region
     Integer :: fit_xendelm ! X-end of data fitting region
     Integer :: fit_yendelm ! Y-end of data fitting region
     Integer :: fit_status ! Status variable
!    Global Arrays:
!    Common:
     Common / F2D_FITGAUSSIAN / fit_xstrelm, fit_ystrelm, fit_xendelm, &
       fit_yendelm, fit_status
 
!  End of f2d_fitgaussian.inc
!********1*********2*********3*********4*********5*********6*********7**
