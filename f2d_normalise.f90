!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_normalise.f90 *
!  *                   *
!  *********************
 
!+ F2D_NORMALISE - Fit 2-D NORMALISE ROI to maximum element
     Subroutine F2D_NORMALISE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, MASK, DATA, VARIANCES, status)
!  Description:
!    Divides each element of "DATA(xmaxdat, ymaxdat)" in the region "(xstrelm, 
!    ystrelm)" to "(xendelm, yendelm)" by the initial maximum element value, 
!    provided that the maximum value is greater than 1.0e-19. If 
!    "variances_exist" is .True., then the variance array is divided by the 
!    square of the maximum value.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Dec-2014: V0.3 Include "MASK", and use masking (Hammersley)
!    16-Dec-1996: V0.2 Avoid open strings crossing lines (Hammersley)
!    25-Oct-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Logical, Intent(IN) :: variances_exist ! .True., if the variances array
!      exists
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! The data mask,
!      defining regions not to be affected, .True. = masked/bad data point
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data array to be normalised
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! Variance array
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User text
     Real :: maximum ! Maximum data value in region of interest
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_NORMALISE ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm .Gt. xmaxdat .Or. xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Gt. ymaxdat .Or. yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_NORMALISE ' // Version)
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Find maximum data value
        Call MA_RMAX (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
          yendelm, maximum, status)
 
!     Check that the maximum is positive and not too small
        If (maximum .Gt. 1.0e-19) Then
 
!        Divide all elements by the maximum value (multiply by its inverse)
!           Call MA_RCMULT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
!             yendelm, 1.0 / maximum, DATA, status)

           Where (.Not. MASK(xstrelm: xendelm, ystrelm: yendelm)) &
             DATA(xstrelm: xendelm, ystrelm: yendelm) = &
             DATA(xstrelm: xendelm, ystrelm: yendelm) * (1.0 / maximum)
 
 
!        If variances exist divide by the square of the number
           If (variances_exist) Then
!              Call MA_RCMULT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
!                yendelm, (1.0 / maximum)**2, VARIANCES, status)
              Where (.Not. MASK(xstrelm: xendelm, ystrelm: yendelm)) &
                VARIANCES(xstrelm: xendelm, ystrelm: yendelm) = &
                VARIANCES(xstrelm: xendelm, ystrelm: yendelm) * &
                (1.0 / maximum)**2

           End If
 
        Else
 
!        The maximum is too small (or negative for normalisation)
           Call IO_WRITE ('WARNING: The maximum value in the ' // &
             'ROI is too small (<1.0e-19) or', status)
           Call IO_WRITE ('         negative to be used ' // &
             'for normalisation', status)
           Write (message, '(''         Maximum element value = '', 1pe12.5)') &
             maximum
           Call IO_WRITE (message, status)
 
        End If
 
     End If
 
     End Subroutine F2D_NORMALISE
!********1*********2*********3*********4*********5*********6*********7*********8
 
