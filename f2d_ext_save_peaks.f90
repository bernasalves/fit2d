!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_ext_save_peaks.f90 *
!  *                        * 
!  **************************
 
!+ F2D_EXT_SAVE_PEAKS - FIT 2-D EXTreme SAVE PEAKS
     Subroutine F2D_EXT_SAVE_PEAKS (max_peaks, num_peaks, PEAKS, status)
!  Description:
!    Save peak information to a text file.
!  Keywords:
!    Peaks~Save, Save.Peaks
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    25-Oct-2007: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: max_peaks ! Dimension of peak arrays
     Integer, Intent(IN) :: num_peaks ! Total peaks found, including
!      problem peaks
     Type(PEAK_STRUCTURE), Intent(IN) :: PEAKS(max_peaks) ! Peaks array
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
     Integer, Parameter :: Io_unit = 17 ! I/O unit for ASCII output of results
!  Local Variables:
     Character*(256), Save :: output_file = 'peaks.txt' ! File name for output
     Integer peak ! Loop variable for peaks
     Integer retstat ! Status return variable
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_EXT_SAVE_PEAKS ' // Version)
        Return
     End If
 
!  Check that peaks have been found
     If (num_peaks .Le. 0) Then

        Call GS_FWARNING (1, 1, &
          'NO PEAKS HAVE BEEN FOUND ! USE "PEAK SEARCH"', status)

     Else
 
!     Select file for peak information output
        Call GS_FILESELECTION (1, 1, 'SELECT FILE FOR PEAK INFORMATION', 1, 1, &
          'Select a file for output of peak information (may be a new file)', & 
          3, .False., retstat, output_file, status)

!     Open file
        Call IO_OPEN_ASCIIFILE (.False., 'WRITE', Io_unit, output_file, &
          retstat, status)
 
        If (retstat .Eq. 0) Then
 
           Write (Io_unit, '(''FIT2D Peak Search result file:'')')
           Write (Io_unit, '(''      X         Y       INTENSITY      ' // &
             'SIGMA    D (Ang)  a_star   b_star   c_star'')')
           Do peak = 1, num_peaks
 
              If (PEAKS(peak)%status .Eq. 0 .And. &
                PEAKS(peak)%intensity .Ge. 0.0) Then
                 Write (Io_unit, '(2f10.1, 2f12.1, 4f9.3)') &
                   PEAKS(peak)%x_centre, PEAKS(peak)%y_centre, &
                   PEAKS(peak)%intensity, PEAKS(peak)%sigma, &
                   PEAKS(peak)%d_spacing, &
                   PEAKS(peak)%a_star, PEAKS(peak)%b_star, PEAKS(peak)%c_star  
              End If
 
           End Do
 
!        Close output file
           Close (Io_unit)
 
        End If

     End If
 
     End Subroutine F2D_EXT_SAVE_PEAKS
!********1*********2*********3*********4*********5*********6*********7*********8

 
