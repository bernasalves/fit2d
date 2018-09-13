!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_internalmemory.f90 *
!  *                        *
!  **************************
 
!+ F2D_INTERNALMEMORY: save or recover data from INTERNAL MEMORY
     Subroutine F2D_INTERNALMEMORY (xmaxdat, ymaxdat, variance_arrays, title, &
       data_exist, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
       XAXIS, YAXIS, DATA, VARIANCES, status)
!  Description:
!    The user may save the current ROI in dynamically created internal memory,
!    or may recover previously saved data.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Dec-1996: V0.2 Avoid open strings crossing lines (Hammersley)
!    27-Sep-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'f2d_internal.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array
     Logical, Intent(IN) :: variance_arrays ! .True., if variances arrays exist
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: title ! Title for data
     Logical, Intent(INOUT) :: data_exist ! .True., if data has been defined
     Integer, Intent(INOUT) :: xnumdat ! Defines X-extent of defined data
     Integer, Intent(INOUT) :: ynumdat ! Defines Y-extent of defined data
     Integer, Intent(INOUT) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(INOUT) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(INOUT) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(INOUT) :: yendelm ! Defines Y-end of region of interest
     Real, Intent(INOUT) :: XAXIS(xmaxdat) ! X-coordinate grid data
     Real, Intent(INOUT) :: YAXIS(ymaxdat) ! Y-coordinate grid data
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data value array
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! Data variances
!      Only used if "variance_arrays" is .True. on input, and only filled if 
!      "variance_arrays" is .True. on output
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number 
!  Local Variables:
     Integer :: i ! Loop variable
     Integer :: memory_number ! Number of memory to use
     Logical :: save_roi ! .True., if the ROI is to be stored in the
!    internal memory
     Real :: step ! Step between axis positions
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(7) ! User messages
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INTERNALMEMORY ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_INTERNALMEMORY ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Output information on currently stored arrays
        Write (MESSAGE(1), '(''INFO: There are currently '', ' // &
          'i2, '' regions of interest stored internally'')') num_store
        Call IO_WRITE (MESSAGE(1), status)
 
        Do i = 1, num_store
           Write (MESSAGE(1), '(''     '', i2, '': '', a70)') i, TITLES(i)
           Call IO_WRITE (MESSAGE(1), status)
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Ask if ROI is to be saved or recovered
        save_roi = num_store .Eq. 0
        MESSAGE(1) = 'You are given the choice of saving the ' // &
          'current data region of interest (ROI)'
        MESSAGE(2) = 'or recovering a previously saved region ' // &
          '(if one exists). Enter "YES" to'
        MESSAGE(3) = 'save the current data ROI within internal ' // &
          'program memory, or "NO" to recover'
        MESSAGE(4) = 'a previously saved ROI.  Note:  Each ' // &
          'time  a ROI is saved the program'
        MESSAGE(5) = 'needs to allocate more dynamic memory,  ' // &
          'so  this command should be used'
        MESSAGE(6) = 'with care,  and  may  fail if  the ' // &
          'computer system cannot allocate more'
        MESSAGE(7) = 'memory.'
        Call IO_INPL (.True., 0, 1, .True., 'SAVE CURRENT DATA ' // &
          'ROI ("NO": TO RECOVER FROM INTERNAL MEMORY)', 7, MESSAGE, 1, &
          'Enter "YES" or "NO"', save_roi, status)
 
        If (save_roi) Then
 
           If (.Not. data_exist) Then
 
              Call IO_WRITE ('WARNING: No data exists in ' // &
                'the program array at present', status)
              Return
           End If
 
!        Number of internal memory to use
           memory_number = Min(Max_store, num_store + 1)
           MESSAGE(1) = 'Enter number of internal memory  to  ' // &
             'use to store a region of interest.'
           MESSAGE(2) = 'By default a new memory will be used,  ' // &
             'unless they are  all  being used,'
           MESSAGE(3) = 'however each time a new memory is used ' // &
             'more virtual memory is necessary.'
           Call IO_INPI (.True., 1, Min(Max_store, num_store + 1), .True., &
             'INTERNAL STORE NUMBER', 3, MESSAGE, 1, &
             'Enter integer within given range', memory_number, status)
 
!        Free memory if previously allocated
           If (memory_number .Le. num_store) Then
 
              Call IO_FREE (pDATA(memory_number), status)
              If (variance_arrays) Then
                 Call IO_FREE (pVARIANCES(memory_number), status)
              End If
 
           End If
 
!        Create dynamic arrays
           Call IO_MALLOC ( (xendelm - xstrelm + 1) * (yendelm - ystrelm + 1) &
             * 4, pDATA(memory_number), status)
 
           If (variance_arrays) Then
              Call IO_MALLOC ((xendelm - xstrelm + 1) * (yendelm - ystrelm + 1 &
                ) * 4, pVARIANCES(memory_number), status)
           End If
 
!        Check status
           If (status .Ne. St_goodvalue) Then
              Return
           End If
 
!        Copy current ROI into internal array
           Call MA_RMOVE (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
             yendelm, xendelm - xstrelm + 1, yendelm - ystrelm + 1, 1, 1, &
             %val(pDATA(memory_number)), status)
           If (variance_arrays) Then
              Call MA_RMOVE (xmaxdat, ymaxdat, VARIANCES, xstrelm, ystrelm, &
                xendelm, yendelm, xendelm - xstrelm + 1, yendelm - ystrelm + &
                1, 1, 1, %val(pVARIANCES(memory_number)), status)
           End If
 
!        Store position of ROI
           X_STRELM(memory_number) = xstrelm
           Y_STRELM(memory_number) = ystrelm
           X_ENDELM(memory_number) = xendelm
           Y_ENDELM(memory_number) = yendelm
           TITLES(memory_number) = title
 
!        Update number of stored arrays
           num_store = Max(num_store, memory_number)
 
        Else
 
!        Recover previously saved ROI
           If (num_store .Ge. 1) Then
 
              memory_number = num_store
              MESSAGE(1) = 'Enter number of internal memory to ' // &
                'recover a previously stored region'
              MESSAGE(2) = 'of interest.'
              Call IO_INPI (.True., 1, num_store, .True., &
                'INTERNAL STORE NUMBER', 2, MESSAGE, 1, &
                'Enter integer within given range', memory_number, status)
 
!           Check status
              If (status .Ne. St_goodvalue) Then
                 Return
              End If
 
!           Recover position of ROI
              xstrelm = X_STRELM(memory_number)
              ystrelm = Y_STRELM(memory_number)
              xendelm = X_ENDELM(memory_number)
              yendelm = Y_ENDELM(memory_number)
 
!           Copy data from internal store to program data array
              Call MA_RMOVE ( xendelm - xstrelm + 1, yendelm - ystrelm + 1, &
                %val(pDATA(memory_number)), 1, 1, xendelm-xstrelm + 1, &
                yendelm-ystrelm + 1, xmaxdat, ymaxdat, xstrelm, ystrelm, DATA, &
                status)
 
              If (variance_arrays) Then
 
                 Call MA_RMOVE ( xendelm - xstrelm + 1, yendelm - ystrelm + 1, &
                   %val(pVARIANCES(memory_number)), 1, 1, xendelm - xstrelm + &
                   1, yendelm - ystrelm + 1, xmaxdat, ymaxdat, xstrelm, &
                   ystrelm, VARIANCES, status)
 
              End If
 
              title = TITLES(memory_number)
              data_exist = .True.
 
!           If the number of defined elements is greater than was previously the
!           case we need to update the number, and define the axis values
              If (xendelm .Gt. xnumdat) Then
 
                 If (xnumdat .Ge. 2) Then
 
                    step = XAXIS(i - 1) - XAXIS(i - 2)
                    Do i = xnumdat + 1, xendelm
                       XAXIS(i) = XAXIS(i - 1) + step
                    End Do
 
                 Else
 
                    Do i = 1, xendelm
                       XAXIS(i) = Real(i) - 0.5
                    End Do
 
                 End If
 
                 xnumdat = xendelm
 
              End If
 
              If (yendelm .Gt. ynumdat) Then
 
                 If (ynumdat .Ge. 2) Then
 
                    step = YAXIS(i - 1) - YAXIS(i - 2)
                    Do i = ynumdat + 1, yendelm
                       YAXIS(i) = YAXIS(i - 1) + step
                    End Do
 
                 Else
 
                    Do i = 1, yendelm
                       YAXIS(i) = Real(i) - 0.5
                    End Do
 
                 End If
 
                 ynumdat = yendelm
 
              End If
 
           Else
 
              Call IO_WRITE ('WARNING: No regions of ' // &
                'interest are presently stored internally', status)
 
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_INTERNALMEMORY
!********1*********2*********3*********4*********5*********6*********7*********8
