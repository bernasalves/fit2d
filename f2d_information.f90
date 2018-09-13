!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_information.f90 *
!  *                     *
!  ***********************
 
!+ F2D_INFORMATION - Fit 2-D INFORMATION on internal state
     Subroutine F2D_INFORMATION (xmaxdat, ymaxdat, data_exist, variance_array, &
       xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, memory_exist, &
       mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, status)
!  Description:
!    Outputs information on internal state of fit2d
!  Method:
!    Integer write and "IO_WRITE"
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    04-Feb-2005: V0.4 Change "ADR" to "ROI" (Hammersley)
!    16-Dec-1996: V0.3 Avoid open strings crossing lines (Hammersley)
!    03-Jan-1996: V0.2 Changes for IBM AIX "xlf" compiler, remove unnecessary 
!      commas in format statments (Hammersley)
!    27-May-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Logical, Intent(IN) :: data_exist ! .True., if data exists
     Logical, Intent(IN) :: variance_array ! .True., if variance arrays
!      exist
     Integer, Intent(IN) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements in Y-direction
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Logical, Intent(IN) :: memory_exist ! .True., if memory exists
     Integer, Intent(IN) :: mxnumdat ! Number of data elements in X-direction
     Integer, Intent(IN) :: mynumdat ! Number of data elements in Y-direction
     Integer, Intent(IN) :: mxstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: mystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: mxendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: myendelm ! Defines Y-end of region of interest
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! Used to store user messages
!  Local Arrays:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INFORMATION ' // Version)
     Else
 
!     Status is O.K.
 
!     Size of program arrays
        Write (message, '(''INFO: Program arrays have '', i10, ' // &
          ''' elements in the X-direction'')') xmaxdat
        Call IO_WRITE (message, status)
        Write (message, '(''INFO: Program arrays have '', i10, ' // &
          ''' elements in the Y-direction'')') ymaxdat
        Call IO_WRITE (message, status)
 
        If (variance_array) Then
           Call IO_WRITE ('INFO: Variance arrays exist, ' // &
             'error propagation will be performed.', status)
        Else
           Call IO_WRITE ('INFO: No variance arrays exist, '// &
             '(no error propagation).', status)
        End If
 
!     Information on current data
        If (data_exist) Then
 
           Call IO_WRITE ('INFO: Current data is defined', status)
           Write (message, '(''INFO: Current data; number of ' // &
             'defined elements in the X-direction '', i10)') xnumdat
           Call IO_WRITE (message, status)
           Write (message, '(''INFO: Current data; number of ' // &
             'defined elements in the Y-direction '', i10)') ynumdat
           Call IO_WRITE (message, status)
 
           Write (message, '(''INFO: Start element of ROI ' // &
             'in the X-direction is '', i10)') xstrelm
           Call IO_WRITE (message, status)
           Write (message, '(''INFO: Start element of ROI ' // &
             'in the Y-direction is '', i10)') ystrelm
           Call IO_WRITE (message, status)
           Write (message, '(''INFO: End element of ROI ' // &
             'in the X-direction is   '', i10)') xendelm
           Call IO_WRITE (message, status)
           Write (message, '(''INFO: End element of ROI ' // &
             'in the Y-direction is   '', i10)') yendelm
           Call IO_WRITE (message, status)
 
        Else
           Call IO_WRITE ('INFO: No current data', status)
        End If
 
!     Information on memory
        If (memory_exist) Then
           Call IO_WRITE ('INFO: Memory defined', status)
           Write (message, '(''INFO: Memory; number of ' // &
             'defined elements in the X-direction '', i10)') mxnumdat
           Call IO_WRITE (message, status)
           Write (message, '(''INFO: Memory; number of ' // &
             'defined elements in the Y-direction '', i10)') mynumdat
           Call IO_WRITE (message, status)
 
           Write (message, '(''INFO: Start element of memory ROI ' // &
             'in the X-direction is '', i10)') mxstrelm
           Call IO_WRITE (message, status)
           Write (message, '(''INFO: Start element of memory ROI ' // &
             'in the Y-direction is '', i10)') mystrelm
           Call IO_WRITE (message, status)
           Write (message, '(''INFO: End element of memory ROI ' // &
             'in the X-direction is   '', i10)') mxendelm
           Call IO_WRITE (message, status)
           Write (message, '(''INFO: End element of memory ROI ' // &
             'in the Y-direction is   '', i10)') myendelm
           Call IO_WRITE (message, status)
 
        Else
           Call IO_WRITE ('INFO: Memory is not defined', status)
        End If
 
     End If
 
     End Subroutine F2D_INFORMATION
!********1*********2*********3*********4*********5*********6*********7*********8
