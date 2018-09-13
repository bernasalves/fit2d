!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_blockcopy.f90 *
!  *                   *
!  *********************
 
!+ F2D_BLOCKCOPY: BLOCK COPY data to and from internal memories
     Subroutine F2D_BLOCKCOPY (gui, variances_exist, xmaxdat, ymaxdat, &
       xnumdat, ynumdat, DATA, VARIANCES, status)
!  Description:
!    Copy a block or data to or from internal memory. Both the current data and 
!    the internal memory must be defined over the entire block regions.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    27-Feb-2004: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'f2d_internal.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if called from graphics user interface
     Logical, Intent(IN) :: variances_exist ! .True., if variance arrays exist
     Integer, Intent(IN) :: xmaxdat ! First dimension of array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array
     Integer, Intent(IN) :: xnumdat ! Defines X-extent of defined data
     Integer, Intent(IN) :: ynumdat ! Defines Y-extent of defined data
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Array to contain data
!      values
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! Array to contain data 
!      variance values. Only used if "variance_arrays" is .True. on input, and
!      only filled if "variance_arrays" is .True. on output
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: i ! Loop variable for internal memories
     Integer, Save :: memory_number = 1 ! Number of memory to use
     Logical, Save :: copy_to = .True. ! .True., if block copy is from the
!      current data array to the internal memory, otherwise copy from internal 
!      memory to the current data array
     Integer :: xendelm ! X-end pixel of block to copy
     Integer :: xnum ! Number of pixels on horizontal edge of block
     Integer :: xnummem ! Horizontal size of internal memory
     Integer, Save :: xstart = 1 ! X-start pixel for block output
     Integer :: xstrelm ! X-start pixel of block to copy
     Integer :: yendelm ! Y-end pixel of block to copy
     Integer :: ynum ! Number of pixels on vertical edge of block
     Integer :: ynummem ! Vertical size of internal memory
     Integer, Save :: ystart = 1 ! Y-start pixel for block output
     Integer :: ystrelm ! Y-start pixel of block to copy
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(7) ! User messages
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_BLOCKCOPY ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_BLOCKCOPY ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_BLOCKCOPY'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Check memories are defined
        If (num_store .Lt. 1) Then
           Call IO_WRITE ('WARNING: No internal memories ' // &
             'are currently defined', status)
           Call IO_WRITE ('         You can defined internal ' // &
             'memories with the "INTERNAL MEMORY" command', status)
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Output information on currently stored arrays
        Write (MESSAGE(1), '(''INFO: There are currently '', ' // &
          'i2, '' memories defined'')') num_store
        Call IO_WRITE (MESSAGE(1), status)
 
        Do i = 1, num_store
           Write (MESSAGE(1), '(''     '', i2, '': '', a70)') i, TITLES(i)
           Call IO_WRITE (MESSAGE(1), status)
        End Do
 
!     Block copy to or from internal memory
        MESSAGE(1) = 'You are given the choice of copying to ' // &
          'or from internal memories.'
        MESSAGE(2) = 'Enter "YES" to copy a block of data from ' // &
          'the current data array'
        MESSAGE(3) = 'to or of the defined internal memories.'
        MESSAGE(4) = 'Enter "NO" to copy a block of data from ' // &
          'of of the internal memories'
        MESSAGE(5) = 'to the current data array.'
        Call IO_INPL (.True., 0, 1, .True., &
          'COPY FROM CURRENT DATA TO INTERNAL MEMORY ("NO": TO ' // &
          'COPY FROM INTERNAL MEMORY)', 5, MESSAGE, 1, 'Enter "YES" or "NO"', &
          copy_to, status)
 
!     Number of internal memory to use
        MESSAGE(1) = 'Enter number of internal memory to use'
        Call IO_INPI (.True., 1, num_store, .True., 'INTERNAL STORE NUMBER', &
          1, MESSAGE, 1, 'Enter integer within given range', memory_number, &
          status)
 
!     Size of internal memory
        xnummem = X_ENDELM(memory_number) - X_STRELM(memory_number) + 1
        ynummem = Y_ENDELM(memory_number) - Y_STRELM(memory_number) + 1
 
        If (copy_to) Then
 
!        Initialise block
           xstrelm = 1
           ystrelm = 1
           xendelm = xnumdat
           yendelm = ynumdat
 
!        Input block to copy
           Call IO_INPI (.True., 1, xnumdat, .True., 'BLOCK X-LOWER LIMIT', 1, &
             'Enter integer to define lower X-limit of block to copy', &
             1, 'Enter integer within given range', xstrelm, status)
           Call IO_INPI (.True., 1, ynumdat, .True., 'BLOCK Y-LOWER LIMIT', 1, &
             'Enter integer to define lower Y-limit ' // &
             'of active data region', 1, 'Enter integer within given range', &
             ystrelm, status)
           Call IO_INPI (.True., xstrelm, xnumdat, .True., &
             'BLOCK X-UPPER LIMIT', 1, &
             'Enter integer to define upper X-limit of block to copy', &
             1, 'Enter integer within given range', xendelm, status)
           Call IO_INPI (.True., ystrelm, ynumdat, .True., &
             'BLOCK Y-UPPER LIMIT', 1, &
             'Enter integer to define upper Y-limit of block to copy', &
             1, 'Enter integer within given range', yendelm, status)
 
!        Input start of output region
           Call IO_INPI (.True., 1, xnummem, .True., 'OUTPUT X-START', 1, &
             'Enter integer to define X-pixel for start of block output',  1, &
             'Enter integer within given range', xstart, status)
           Call IO_INPI (.True., 1, ynummem, .True., 'OUTPUT Y-START', 1, &
             'Enter integer to define Y-pixel for start of block output', 1, &
             'Enter integer within given range', ystart, status)
 
        Else
 
!        Initialise block
           xstrelm = 1
           ystrelm = 1
           xendelm = xnummem
           yendelm = ynummem
 
!        Input block to copy
           Call IO_INPI (.True., 1, xnummem, .True., 'BLOCK X-LOWER LIMIT', 1, &
             'Enter integer to define lower X-limit of block to copy', &
             1, 'Enter integer within given range', xstrelm, status)
           Call IO_INPI (.True., 1, ynummem, .True., 'BLOCK Y-LOWER LIMIT', 1, &
             'Enter integer to define lower Y-limit ' // &
             'of active data region', 1, 'Enter integer within given range', &
             ystrelm, status)
           Call IO_INPI (.True., xstrelm, xnummem, .True., &
             'BLOCK X-UPPER LIMIT', 1, &
             'Enter integer to define upper X-limit of block to copy', &
             1, 'Enter integer within given range', xendelm, status)
           Call IO_INPI (.True., ystrelm, ynummem, .True., &
             'BLOCK Y-UPPER LIMIT', 1, &
             'Enter integer to define upper Y-limit of block to copy', &
             1, 'Enter integer within given range', yendelm, status)
 
!        Input start of output region
           Call IO_INPI (.True., 1, xnumdat, .True., 'OUTPUT X-START', 1, &
             'Enter integer to define X-pixel for start of block output', 1, &
             'Enter integer within given range', xstart, status)
           Call IO_INPI (.True., 1, ynumdat, .True., 'OUTPUT Y-START', 1, &
             'Enter integer to define Y-pixel for start of block output', 1, &
             'Enter integer within given range', ystart, status)
 
        End If
 
!     Check status
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Size of block to copy
        xnum = xendelm - xstrelm + 1
        ynum = yendelm - ystrelm + 1
 
!     Check that block is fully defined for both arrays
        If (copy_to) Then
 
           If (xstart + xnum - 1 .Gt. xnummem .Or. ystart + ynum - 1 .Gt. &
             ynummem) Then
              Call IO_WRITE ('WARNING: The block extends beyond ' // &
                'the end of the memory', status)
              Return
           End If
 
        Else
 
           If (xstart + xnum - 1 .Gt. xnumdat .Or. ystart + ynum - 1 .Gt. &
             ynumdat) Then
              Call IO_WRITE ('WARNING: The block extends beyond ' // &
                'the end of the current data array', status)
              Return
           End If
 
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_BLOCKCOPY: Before MA_RMOVE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Copy block of data
        If (copy_to) Then
 
!        Copy from current data array to internal memory
           Call MA_RMOVE (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
             yendelm, xnummem, ynummem, xstart, ystart, &
             %val(pDATA(memory_number)), status)
 
           If (variances_exist) Then
 
              Call MA_RMOVE (xmaxdat, ymaxdat, VARIANCES, xstrelm, ystrelm, &
                xendelm, yendelm, xnummem, ynummem, xstart, ystart, &
                %val(pVARIANCES(memory_number)), status)
 
           End If
 
        Else
 
!        Copy from internal memory to current data array
           Call MA_RMOVE (xnummem, ynummem, %val(pDATA(memory_number)), &
             xstrelm, ystrelm, xendelm, yendelm, xmaxdat, ymaxdat, xstart, &
             ystart, DATA, status)
 
           If (variances_exist) Then
 
              Call MA_RMOVE (xnummem, ynummem, &
                %val(pVARIANCES(memory_number)), xstrelm, ystrelm, xendelm, &
                yendelm, xmaxdat, ymaxdat, xstart, ystart, VARIANCES, status)
 
           End If
 
        End If
 
     End If
 
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''F2D_BLOCKCOPY: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_BLOCKCOPY
!********1*********2*********3*********4*********5*********6*********7*********8
