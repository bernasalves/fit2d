!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************************
!  *                         *
!  * f2d_rmap_out_volume.f90 *
!  *                         *
!  ***************************
 
!+ F2D_RMAP_OUT_VOLUME: OUTput VOLUME
     Subroutine F2D_RMAP_OUT_VOLUME (input_file, xmaxmap, ymaxmap, zmaxmap, &
       xnummap, ynummap, znummap, MAP, NORMALISE, status)
!  Description:
!    Output volume
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    13-Nov-2006: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Character(Len=*), Intent(IN) :: input_file ! Name of input file
     Integer, Intent(IN) :: xmaxmap ! First dimension of reciprocal map
     Integer, Intent(IN) :: ymaxmap ! Second dimension of reciprocal map
     Integer, Intent(IN) :: zmaxmap ! Third dimension of reciprocal map
     Integer, Intent(IN) :: xnummap ! Number of defined pixels in X-direction
     Integer, Intent(IN) :: ynummap ! Number of defined pixels in Y-direction
     Integer, Intent(IN) :: znummap ! Number of defined pixels in Z-direction
     Real, Intent(IN) :: MAP(xmaxmap, ymaxmap, zmaxmap) ! Reciprocal map array
     Real, Intent(IN) :: NORMALISE(xmaxmap, ymaxmap, zmaxmap)
!      Normalisation array
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
     Integer, Parameter :: Out_unit = 10 ! Logical unit number for input
     Real, Parameter :: Scale = 0.00001 ! Scaling to apply to output figures
!  Local Variables:
     Character(Len = 256) info_file ! Name of "info" output file
     Character(Len = 256), Save :: output_file = 'fit2d.vol'
     Integer :: bytes_per_unit ! Number of bytes per record units
     Integer :: num_con ! Number of contributions to an average
     Integer :: record ! Record to output
     Integer :: record_length !
     Integer :: retstat ! Return status variable: 0 = success
     Integer :: stat ! Return status for memory allocation
     Integer :: x ! Loop variable for first dimension
     Integer :: y ! Loop variable for second dimension
     Integer :: z ! Loop variable for third dimension
     Real :: total ! Running total for calculating average
!  Local Arrays:
     Character(Len = 80) :: INFO(4) ! User info
     Character(Len = 80) :: PROMPT(2) ! User prompts
     Real, Dimension(:, :), Allocatable :: SLICE ! slice of the volume
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_OUT_VOLUME ' // Version)
     Else

!     Create default output file name
        Call IO_OUTFILE (0, input_file, '.vol', 'fit2d.vol', output_file, &
          status)
 
!       Use file selection tool to obtain file name from user
        PROMPT(1) = 'SELECT OUTPUT FILE FOR VOLUME'
        INFO(1) = 'Use the file selection tool to select a directory'
        INFO(2) = 'and an output file to contain the volume in binary format'
        Call GS_FILESELECTION (1, 1, PROMPT, 2, 2, INFO, 3, .False., &
          retstat, output_file, status)
 
!     Check return status
        If (retstat .Ne. 0) Then
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Allocate slice array
        Allocate (SLICE(xnummap, ynummap), Stat = stat)
        If (stat .Ne. 0) Then
           Call IO_WRITE ('ERROR: Failed to allocated array for SLICE', &
             status)
           Return
        End If
 
        Call IO_RECL (bytes_per_unit, status)
        record_length = (xnummap * ynummap * 4) / bytes_per_unit
        Open (Unit = 9, FILE = output_file, Access = 'DIRECT', &
          FORM = 'UNFORMATTED', RECL = record_length, &
          STATUS = 'UNKNOWN', IOSTAT = retstat)
        record = 1

        If (retstat .Ne. 0) Then
           Call IO_WRITE ('ERROR: Problem openning output file', status)
           Call IO_WRITE ('       ' // output_file, status)
           Write (*, '(''retstat = '', i6)') retstat
           Stop
        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Out_volume section
        Do z = 1, znummap

           Do y = 1, ynummap
 
              Do x = 1, xnummap
 
                 If (NORMALISE(x, y, z) .Gt. 0.0) Then
                    SLICE(x, y) = MAP(x, y, z) * Scale / NORMALISE(x, y, z)
                 Else
 
!                 Value is not defined, so try averaging adjacent values
                    num_con = 0
                    total = 0.0
 
                    If (x - 1 .Gt. 0) Then
 
                       If (y - 1 .Gt. 0) Then
 
                          If (NORMALISE(x - 1, y - 1, z) .Gt. 0.0) Then
                             total = MAP(x - 1, y - 1, z) / &
                               NORMALISE(x - 1, y - 1, z)
                             num_con = num_con + 1
                          End If
 
                       End If
 
                       If (y + 1 .Le. ynummap) Then
 
                          If (NORMALISE(x - 1, y + 1, z) .Gt. 0.0) Then
                             total = total + MAP(x - 1, y + 1, z) / &
                               NORMALISE(x - 1, y + 1, z)
                             num_con = num_con + 1
                          End If
 
                       End If
 
                    End If
 
                    If (x + 1 .Le. xnummap) Then
 
                       If (y - 1 .Gt. 0) Then
 
                          If (NORMALISE(x + 1, y - 1, z) .Gt. 0.0) Then
                             total = total + MAP(x + 1, y - 1, z) / &
                               NORMALISE(x + 1, y - 1, z)
                             num_con = num_con + 1
                          End If
 
                       End If
 
                       If (y + 1 .Le. ynummap) Then
                          
                          If (NORMALISE(x + 1, y + 1, z) .Gt. 0.0) Then
                             total = total + MAP(x + 1, y + 1, z) / &
                               NORMALISE(x + 1, y + 1, z)
                             num_con = num_con + 1
                          End If
                          
                       End If
 
                    End If
 
                    If (num_con .Gt. 0) Then
                       SLICE(x, y) = total * scale / Real(num_con)
                    Else
                       SLICE(x, y) = 0.0
                    End If
 
                 End If
 
              End Do
 
           End Do

           Write (9, REC = record) SLICE
           record = record + 1
 
        End Do

!     Close binary file
        Close (9)

!     Deallocate slice
        Deallocate (SLICE)

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Open ASCII file for output of header information
        info_file = Trim(output_file) // '.info'
        Call IO_OPEN_ASCIIFILE (.False., 'WRITE', Out_unit, &
          info_file, retstat, status)
                                     
        If (retstat .Eq. 0) Then
                                
           Write (Out_unit, '(''! FIT2D VOLUME INFO FILE'')')
           Write (Out_unit, '(''NUM_X = '', i8)') xnummap
           Write (Out_unit, '(''NUM_Y = '', i8)') ynummap
           Write (Out_unit, '(''NUM_Z = '', i8)') znummap
                                                       
           Close (Out_unit)
                           
        End If

     End If 
     
     End Subroutine F2D_RMAP_OUT_VOLUME
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
