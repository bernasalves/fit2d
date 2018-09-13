!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_ext_load_peaks.f90 *
!  *                        * 
!  **************************
 
!+ F2D_EXT_LOAD_PEAKS - FIT 2-D EXTreme LOAD PEAKS
     Subroutine F2D_EXT_LOAD_PEAKS (max_peaks, num_peaks, PEAKS, status)
!  Description:
!    Input peak information from a text file.
!  Keywords:
!    Peaks~Load, Load.Peaks
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    14-May-2008: V0.2 Output number of peaks input (Hammersley)
!    26-Oct-2007: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: max_peaks ! Dimension of peak arrays
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: num_peaks ! Total peaks found, including
!      problem peaks
     Type(PEAK_STRUCTURE), Intent(OUT) :: PEAKS(max_peaks) ! Peaks array
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
     Integer, Parameter :: Io_unit = 17 ! I/O unit for ASCII input
!  Local Variables:
     Character*(256), Save :: input_file = 'peaks.txt' ! File name for input
     Character*(256) :: line ! Line input from file
     Character*(80) :: message ! User message
     Integer iostat ! I/O status return variable
     Integer peak ! Loop variable for peaks
     Integer retstat ! Status return variable
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_EXT_LOAD_PEAKS ' // Version)
        Return
     End If
 
!  Select file for peak information input
     Call GS_FILESELECTION (1, 1, 'SELECT FILE FOR PEAK INFORMATION', 1, 1, &
       'Select a file for input of peak information (the file must exist)', & 
       3, .False., retstat, input_file, status)

!  Open file
     Call IO_OPEN_ASCIIFILE (.False., 'READ', Io_unit, input_file, retstat, &
       status)
 
     If (retstat .Eq. 0) Then

!     Input header line
        Read (Io_unit, '(a)') line

        If (line .Ne. 'FIT2D Peak Search result file:') Then
           Call GS_FWARNING (1, 1, 'THE FILE IS NOT A FIT2D PEAK LIST FILE', &
             status)
           Return
        End If

!     Input column labels line
        Read (Io_unit, '(a)') line

!     Input peaks
        peak = 1
        Do


!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''peak = '', i6)') peak
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

           Read (Io_unit, '(2f10.1, 2f12.1, 4f9.3)', IOSTAT = iostat) &
             PEAKS(peak)%x_centre, PEAKS(peak)%y_centre, &
             PEAKS(peak)%intensity, PEAKS(peak)%sigma, PEAKS(peak)%d_spacing, &
             PEAKS(peak)%a_star, PEAKS(peak)%b_star, PEAKS(peak)%c_star  

           If (iostat .Eq. 0 .And. peak .Lt. max_peaks) Then
              peak = peak + 1
           Else
              Exit
           End If
 
        End Do

        num_peaks = peak - 1

     End If

!  Close input file
     Close (Io_unit)

!  User info
     Write (message, '(''INFO: Number of peaks = '', i8)') num_peaks
     Call IO_WRITE (message, status)

     End Subroutine F2D_EXT_LOAD_PEAKS
!********1*********2*********3*********4*********5*********6*********7*********8

 
