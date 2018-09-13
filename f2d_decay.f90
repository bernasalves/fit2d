!********1*********2*********3*********4*********5*********6*********7**
 
!  *****************
!  *               *
!  * f2d_decay.f90 *
!  *               *
!  *****************
 
!+ F2D_DECAY: Decay correction (image plate)
     Subroutine F2D_DECAY (xmaxdat, ymaxdat, ynumdat, xstrelm, ystrelm, &
       xendelm, yendelm, variances_exist, DATA, VARIANCES, status)
!  Description:
!    Corrects image plate decay which can lead to non-uniform flat field 
!    response, owing to the image plate signal decay continuing whilst the 
!    plate is being scanned. For slow scanners this effect may be 10%.
!
!    The image plate decay for Fuji white and blue plates has been
!    found to follow the decay law:
!
!    Fractional Signal = 0.134276 * Exp(-t / 179.39) +
!    (1.0 - 0.134276) * Exp(-t / 6631.4)
!
!    From the time of the exposure, the time from the end of the exposure to the
!    start of the scan and the time for the scan, the decay for each line can be
!    calculated and corrected.
!  Keywords:
!    Decay.Correction, Correction.Decay
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    13-Dec-1994: V0.4 Allow flexible user control of all decay and scanning 
!      parameters (Hammersley)
!    04-Jul-1994: V0.3 Change coefficients of function to new lsq fitted values
!      (Hammersley)
!    28-Jun-1994: V0.2 Correct direction of correction (Hammersley)
!    23-Jun-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: ynumdat ! Number of lines of data in whole image
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Logical, Intent(IN) :: variances_exist ! .True., if error arrays exist
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The data values,
!      corrected on output
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! The data variance
!      values, corrected on output
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Integer, Save :: num_scanned = 1482 ! Number of scanned lines
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Logical, Save :: coarse_scan = .True. ! .True., if the image is a
!      coarse scan image
     Logical, Save :: full_control = .False. ! .True., if the user wants
!      full control over all ascepts of the decay correction
     Real :: correction ! Correction factor to be applied to a line of pixels
     Real :: decay ! Fractional decay, which effects a line of pixels
     Real, Save :: elapse_time = 60.0 ! Time from end of exposure to start
!      of scan (button pushed)
     Real, Save :: exposure = 10.0 ! Time in seconds for the exposure
     Real, Save :: fract_1 = 0.134264 ! Fraction of decay for first exponential
     Real :: maximum_time ! Time from the start of the exposure to the start 
!      of the laser scan
     Real :: minimum_time ! Time from the end of the exposure to the start of
!      the laser scan
     Real :: ref_decay ! Decay effecting the first line of pixels
     Real, Save :: scan_time = 120.0 ! Time taken to scan a set number of
!      image lines
     Real, Save :: startup_time = 36.0 ! The time from the start of the
!      scan (button pushed) to the start of the laser scanning the image plate
     Real, Save :: t_fast = 179.411 ! The fast decay time constant in seconds
     Real, Save :: t_slow = 6631.4 ! The slow decay time constant in seconds
     Real :: t1 ! Dummy variable for function 'Int_decay'
     Real :: t2 ! Dummy variable for function 'Int_decay'
     Real :: time_per_line ! Time in seconds to scan one line
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(8) ! User messages
!  Internal Functions:
     Real :: Int_decay ! Integral of decay
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
     Int_decay(t1, t2) = (-fract_1 * t_fast * Exp(-t2 / t_fast) - (1.0 - &
       fract_1) * t_slow * Exp(-t2 / t_slow) + fract_1 * t_fast * Exp(-t1 / &
       t_fast) + (1.0 - fract_1) * t_slow * Exp(-t1 / t_slow) ) / (t2 - t1)
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DECAY ' // Version)
     Else
 
!     Does the user want full control over all decay parameters
        MESSAGE(1) = 'The default decay correction is set up ' // &
          'for Fuji white or blue imaging'
        MESSAGE(2) = 'plates read out by the Molecular Dynamics ' // &
          '400E scanner. The decay'
        MESSAGE(3) = 'measurements were taken during the ESRF ' // &
          'Experimental Hall summer mode '
        MESSAGE(4) = 'operation. For another scanner, another ' // &
          'type of imaging plate, or for'
        MESSAGE(5) = 'data taken at a different temperature you ' // &
          'may want to re-define the'
        MESSAGE(6) = 'constants used to calculate the decay ' // 'correction.'
        MESSAGE(7) = 'Enter "YES" if you want to define ' // &
          'different decay fit constants or'
        MESSAGE(8) = 'scanner delay and read-out times.'
        Call IO_INPL (.True., 0, 1, .True., 'FULL USER CONTROL', 8, MESSAGE, &
          1, 'Enter "YES" or "NO"', full_control, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
        If (full_control) Then
 
!        Output explanatory message
           Call IO_WRITE (' ', status)
           Call IO_WRITE ('The decay is approximated by the ' // &
             'sum of two exponential decays. The', status)
           Call IO_WRITE ('fraction of remaining signal t ' // &
             'seconds after exposure F_t is defined by:', status)
           Call IO_WRITE (' ', status)
           Call IO_WRITE ('   F_t = P_fast * Exp(-t/T_fast)' // &
             '+ (1.0 - P_fast) * Exp(-t/T_slow)', status)
           Call IO_WRITE (' ', status)
           Call IO_WRITE ('where: P_fast is the proportion ' // &
             'of the fast decay component', status)
           Call IO_WRITE ('       T_fast is the time ' // &
             'constant of the fast decay (in seconds)', status)
           Call IO_WRITE ('       T_slow is the time ' // &
             'constant of the slow decay (in seconds)', status)
           Call IO_WRITE (' ', status)
 
!        Proportion of fast decay process
           MESSAGE(1) = 'Enter proportion that the fast decay ' // &
             'makes to the intensity decay (P_fast)'
           Call IO_INPR (.True., 0.0, 1.0, .True., 'PROPORTION FAST DECAY', 1, &
             MESSAGE, 1, 'Must be a valid real number within the given range', &
             fract_1, status)
 
!        Time constant of fast decay process
           MESSAGE(1) = 'Enter time constant (in seconds) of ' // &
             'the fast decay process (T_fast)'
           Call IO_INPR (.True., 0.0, 1.0e10, .True., &
             'TIME CONSTANT OF FAST DECAY (SECONDS)', 1, MESSAGE, 1, &
             'Must be a valid real number within the given range', t_fast, &
             status)
 
!        Time constant of slow decay process
           MESSAGE(1) = 'Enter time constant (in seconds) of ' // &
             'the slow decay process (T_slow)'
           Call IO_INPR (.True., 0.0, 1.0e10, .True., &
             'TIME CONSTANT OF SLOW DECAY (SECONDS)', 1, MESSAGE, 1, &
             'Must be a valid real number within the given range', t_slow, &
             status)
 
!        Output explanation of time parameters used to calculate the integral 
!        decay
           Call IO_WRITE (' ', status)
           Call IO_WRITE ('The program calculates for each ' // &
             'line in the image the average decay that', status)
           Call IO_WRITE ('has taken place from the longest ' // &
             'time from first exposure to the scan,', status)
           Call IO_WRITE ('to the shortest time from the ' // &
             'end of the exposure to the scan. To know', status)
           Call IO_WRITE ('this it needs to know how long ' // &
             'was the exposure, the time between the', status)
           Call IO_WRITE ('exposure and the user start of ' // &
             'the scan (both these questions are asked', status)
           Call IO_WRITE ('later), the time from the user ' // &
             'start of the scan (click with with the ', status)
           Call IO_WRITE ('mouse, or similar) to the ' // &
             'physical scan of the first line of the image,', status)
           Call IO_WRITE ('and the time to scan each line ' // &
             'of the image. The time to scan each line', status)
           Call IO_WRITE ('is calculated from the total ' // &
             'time of the physical scan divided by the', status)
           Call IO_WRITE ('number of scanned lines.', status)
           Call IO_WRITE (' ', status)
 
!        Start-up time in seconds
           MESSAGE(1) = 'Enter the number of seconds between ' // &
             'the user request for the scan and'
           MESSAGE(2) = 'the actual start of scanning the first ' // &
             'line in the image'
           Call IO_INPR (.True., 0.0, 1000.0, .True., &
             'START-UP TIME (SECONDS)', 2, MESSAGE, 1, &
             'Must be valid real number within given range', startup_time, &
             status)
 
!        Physical Scan time
           MESSAGE(1) = 'Enter the number of seconds it takes ' // &
             'to scan the image'
           Call IO_INPR (.True., 0.0, 3600.0, .True., &
             'IMAGE SCAN TIME (SECONDS)', 1, MESSAGE, 1, &
             'Must be valid real number within given range', scan_time, &
             status)
 
!        Number of scanned line corresponding to above time
           MESSAGE(1) = 'Enter the number of lines scanned in ' // &
             'the time entered above'
           Call IO_INPI (.True., 1, 50000, .True., 'NUMBER OF SCANNED LINES', &
             1, MESSAGE, 1, 'Must be valid integer number within given range', &
             num_scanned, status)
 
!        Calculate time taken to scan one line of the image
           time_per_line = scan_time / Real(num_scanned)
 
        Else
 
!        Enter Molecular Dynamics resolution mode
           Call IO_INPL (.True., 0, 1, .True., &
             '176 MICRON SCAN ("NO" FOR 88 MICRON)', 1, &
             'Enter scan resolution mode: "YES" for 176 micron, ' // &
             '"NO" for 88 micron', 1, 'Enter "YES" or "NO"', coarse_scan, &
             status)
 
!        Time per line for the different scan modes
           If (coarse_scan) Then
              time_per_line = 0.1
           Else
              time_per_line = 0.2429
           End If
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Length of exposure in seconds
        Call IO_INPR (.True., 0.0, 1.0e10, .True., &
          'EXPOSURE LENGTH (SECONDS)', 1, &
          'Enter the length of the exposure in seconds', 1, &
          'Must be valid real number', exposure, status)
 
!     Length of elapse time from end of exposure to scan
        MESSAGE(1) = 'Enter the time in seconds between the ' // &
          'end of the exposure and the time'
        MESSAGE(2) = 'that the scan was started (the scan ' // &
          'button was pressed on the'
        MESSAGE(3) = 'computer). (The program knows the delay ' // &
          'time between the button being'
        MESSAGE(4) = 'pressed and the physical start of the ' // &
          'laser scan on the image plate'
        Call IO_INPR (.True., 0.0, 1.0e10, .True., &
          'ELAPSE TIME (FROM END OF EXPOSURE TO SCAN, SECONDS)', 4, MESSAGE, &
          1, 'Must be valid real number', elapse_time, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Calculate time from start of exposure to start of laser scan
        minimum_time = elapse_time + startup_time
        maximum_time = minimum_time + exposure
 
!     Calculate reference decay fraction (for first line)
        ref_decay = Int_decay( minimum_time + Real(ynumdat - yendelm) * &
          time_per_line, maximum_time + Real(ynumdat - yendelm) * &
          time_per_line)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''ref_decay = '', f12.5)') ref_decay
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Go through every pixel, correcting the image
        Do y = yendelm - 1, ystrelm, -1
 
!        Calculate correction factor
           decay = Int_decay( minimum_time + Real(ynumdat - y) * &
             time_per_line, maximum_time + Real(ynumdat - y) * time_per_line)
           correction = ref_decay / decay
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''y, Correction = '', i4, f12.5)') y,
!        :          correction
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Apply correction factor to all pixels in the line
           Do x = xstrelm, xendelm
              DATA(x, y) = DATA(x, y) * correction
           End Do
 
!        Correct variance values if they exist
           If (variances_exist) Then
 
              Do x = xstrelm, xendelm
                 VARIANCES(x, y) = VARIANCES(x, y) * correction**2
              End Do
 
           End If
 
        End Do
 
!     Output information to user for checking
        Write (MESSAGE(1), '(''INFO: Maximum correction factor ' // &
          '= '', f9.5)') correction
        Call IO_WRITE (MESSAGE(1), status)
 
     End If
 
     End Subroutine F2D_DECAY
!********1*********2*********3*********4*********5*********6*********7**
