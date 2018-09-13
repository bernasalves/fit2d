!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************
!  *               *
!  * f2d_fit2d.inc *
!  *               *
!  *****************
 
!+ FIT2D: FIT2D data array pointers passing Common
 
!  Description:
!    Stores pointers to dynamically allocated pointer arrays. If the
!    current data arrays and memory arrays are to be swapped through
!    exchange of pointers, then the arrays must be used through these
!    pointers and not be passed as subroutine arguments.
!
!  Keywords:
!    FIT2D.Common, Common.FIT2D, Pointers.FIT2D, FIT2D.Pointers
!  Method:
!    Common blocks
!  Deficiencies:
!    WARNING POINTERS NEED TO BE 64-bit for 64-bit systems
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    09-Dec-2014: V0.6 Remove Vector array from include (Hammersley)
!    10-Mar-2004: V0.5 Add Vector arrays (Hammersley)
!    16-Nov-1996: V0.4 Rename from "f2d_fit.inc", and exclude definitions 
!      concerning with fitting (Hammersley)
!    27-Apr-1996: V0.3 Add commented out code for 64-bit systems (Hammersley)
!    04-Feb-1996: V0.2 Use to allow "EXCHANGE" to swap pointers to arrays in 
!      subroutines (Hammersley)
!    18-Mar-1989: V0.1 Original (Hammersley)
!  Global Constants:
!    Global Variables:
 
!  Array sizes and region of interest
     Integer :: f2d_xmaxdat ! Maximum length of data array (Maximum channels)
     Integer :: f2d_ymaxdat ! Maximum number of frames
     Integer :: f2d_xnumdat ! Length of data array in X-direction, no.
!      of channels
     Integer :: f2d_ynumdat ! Length of data array in Y-direction, no. of frames
     Integer :: f2d_xstrelm ! X-starting element for operations
     Integer :: f2d_ystrelm ! Y-starting element for operations
     Integer :: f2d_xendelm ! X-end element for operations
     Integer :: f2d_yendelm ! Y-end element for operations
 
!  Pointers to program arrays (Converted to Integer*8 with sed on 64 bit 
!  machines
     Integer pDATA ! Pointer to dynamic array "DATA"
     Integer pVARIANCES ! Pointer to dynamic array "VARIANCES"
     Integer pMASK ! Pointer to dynamic array "MASK"
     Integer pMDATA ! Pointer to dynamic array "MDATA"
     Integer pMVARIANCES ! Pointer to dynamic array "MVARIANCES"
     Integer pMXAXIS ! Pointer to dynamic array "MXAXIS"
     Integer pMYAXIS ! Pointer to dynamic array "MYAXIS"
     Integer pVECTORS ! Pointer to dynamic array "VECTORS"
     Integer pWORK1 ! Pointer to dynamic array "WORK1"
     Integer pWORK2 ! Pointer to dynamic array "WORK2"
     Integer pXAXIS ! Pointer to dynamic array "XAXIS"
     Integer pYAXIS ! Pointer to dynamic array "YAXIS"
 
!    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  This might be necessary for 64-bit addressing machines
!    Integer*8 pDATA ! Pointer to dynamic array "DATA"
!    Integer*8 pVARIANCES ! Pointer to dynamic array "VARIANCES"
!    Integer*8 pMASK ! Pointer to dynamic array "MASK"
!    Integer*8 pMDATA ! Pointer to dynamic array "MDATA"
!    Integer*8 pMVARIANCES ! Pointer to dynamic array "MVARIANCES"
!    Integer*8 pMXAXIS ! Pointer to dynamic array "MXAXIS"
!    Integer*8 pMYAXIS ! Pointer to dynamic array "MYAXIS"
!    Integer*8 pVECTORS ! Pointer to dynamic array "VECTORS"
!    Integer*8 pWORK1 ! Pointer to dynamic array "WORK1"
!    Integer*8 pWORK2 ! Pointer to dynamic array "WORK2"
!    Integer*8 pXAXIS ! Pointer to dynamic array "XAXIS"
!    Integer*8 pYAXIS ! Pointer to dynamic array "YAXIS"
 
     Common / F2D_COM_ARRAYS / f2d_xmaxdat, f2d_ymaxdat, f2d_xnumdat, &
       f2d_ynumdat, f2d_xstrelm, f2d_ystrelm, f2d_xendelm, f2d_yendelm, pDATA, &
       pVARIANCES, pMASK, pXAXIS, pYAXIS, pMDATA, pMVARIANCES, pMXAXIS, &
       pMYAXIS, pWORK1, pWORK2
!    pVECTORS, 
 
!  End of f2d_fit2d.inc
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
