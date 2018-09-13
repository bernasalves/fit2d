!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_maskthreshold.f90 *
!  *                       *
!  *************************
 
!+ F2D_MASKTHRESHOLD - Define mask elements depending on a threshold criterion 
!    on the ROI data values
     Subroutine F2D_MASKTHRESHOLD (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MASK, status)
!  Description:
!    The user defines a threshold operation <GREATER THAN> or <LESS THAN> a 
!    defined constant, and all elements within the current ROI with values which
!    conform to the threshold operation will be set to be masked within the data
!    mask. Similarly all other pixels will be set to be unmasked.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    12-Nov-1996: V0.2 Option of graphical user interface, and change behaviour
!      so that pixel with values inside the threshold do not change their mask
!      state (Hammersley)
!    10-Apr-1995: V0.1 Original, based of "F2D_SELECTPIXEL" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      means that an element is masked-off from all operations which take
!      into account masking
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: x ! Loop variable
     Integer :: y ! Loop variable
     Logical, Save :: less_than = .True. ! .True., if the operation is to
!      be carried out on pixels with input values greater than an input operand,
!      otherwise the comparison is a less than
     Real, Save :: level = 1.0 ! Decision level
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(4) ! User messages
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MASKTHRESHOLD ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_MASKTHRESHOLD ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Output explanation
        Call IO_WRITE ('INFO: This option allows masked-off ' // &
          'elements with the ROI to be defined', status)
        Call IO_WRITE ('      automatically depending ' // &
          'on the data values. The user can set a ', status)
        Call IO_WRITE ('      threshold operation ' // &
          '(<GREATER THAN> or <LESS THAN>) and a', status)
        Call IO_WRITE ('      threshold level. All data ' // &
          'values which CONFORM WITH the defined', status)
        Call IO_WRITE ('      test will be set to be ' // &
          'masked-off. All others will be changed.', status)
 
!     Output warning message concerning change in behaviour
        If (.Not. gui) Then
 
           Call IO_WRITE (' ', status)
           Call IO_WRITE ('WARNING: This option used to ' // &
             'also "un-mask" elements; this behaviour', status)
           Call IO_WRITE ('         has been changed so ' // &
             'now elements are only masked or left', status)
           Call IO_WRITE ('         in their previous ' // &
             'state. To un-mask elements, use "CLEAR', status)
           Call IO_WRITE ('         MASK" before this command.', status)
           Call IO_WRITE (' ', status)
 
        End If
 
!     Input comparison type
        MESSAGE(1) = '"YES" if mask elements are to be ' // &
          'selected by a lesser than comparison'
        MESSAGE(2) = '(exclusive),'
        MESSAGE(3) = '"NO" if mask elements are to be ' // &
          'selected by a greater than comparison'
        MESSAGE(4) = '(exclusive)'
        If (gui) Then
           Call GS_INPL (.True., 0, 1, .True., 'LESS THAN COMPARISON', 4, &
             MESSAGE, 1, 'Enter "YES" or "NO"', less_than, status)
        Else
           Call IO_INPL (.True., 0, 1, .True., 'LESS THAN COMPARISON', 4, &
             MESSAGE, 1, 'Enter "YES" or "NO"', less_than, status)
        End If
 
!     Input cut-off level
        If (gui) Then
           Call GS_INPR (.False., 0.0, 0.0, .True., &
             'DECISION THRESHOLD DATA VALUE', 1, &
             'Input pixel data value for threshold decision', 1, &
             'Enter real number', level, status)
        Else
           Call IO_INPR (.False., 0.0, 0.0, .True., &
             'DECISION THRESHOLD DATA VALUE', 1, &
             'Input pixel data value for threshold decision', 1, &
             'Enter real number', level, status)
        End If
 
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Check for user escape
        If (status .Eq. St_goodvalue) Then
 
!        Output working message if in GUI mode
           If (gui) Then
 
              Call GS_FPROMPT (1, 1, 'CALCULATING MASK: PLEASE WAIT', status)
              Call GS_UPDATE (status)
 
           End If
 
 
           If (less_than) Then
 
              Do y = ystrelm, yendelm
 
                 Do x = xstrelm, xendelm
 
                    If (DATA(x, y) .Lt. level) Then
                       MASK(x, y) = .True.
                    End If
 
                 End Do
 
              End Do
 
           Else
 
              Do y = ystrelm, yendelm
 
                 Do x = xstrelm, xendelm
 
                    If (DATA(x, y) .Gt. level) Then
                       MASK(x, y) = .True.
                    End If
 
                 End Do
 
              End Do
 
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_MASKTHRESHOLD
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
