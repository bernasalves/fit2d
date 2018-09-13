!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************************
!  *                          *
!  * f2d_inp_polarisation.f90 *
!  *                          *
!  ****************************
 
!+ F2D_INP_POLARISATION -  INPut POLARISATION of beam on sample
     Subroutine F2D_INP_POLARISATION (correct_polarisation, polarisation, &
       lorentz_geometry, status)
!  Description:
!    User input of required polarisation and Lorentz effect corrections and beam
!    polarisation on sample. This is defined as the horizontal comonent of 
!    intensity minus the vertical component divided by the sum.
!  Keywords:
!    Polarisation.Input, Input.Polarisation~Factor, Lorentz.Correction, 
!    Input.Lorentz
!  Method:
!    Uses "IO_INPR"
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    23-Oct-1995: V0.3 Choice of no Lorentzian correction, or partial 
!      correction to the equivalent of a 2-theta scan. (Hammersley)
!    01-Sep-1995: V0.2 Option of polarisation correction (Hammersley)
!    28-Aug-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
!  Import/Export:
     Logical, Intent(INOUT) :: correct_polarisation ! .True., if the
!      polarisation correction is to be applied
     Real, Intent(INOUT) :: polarisation ! Polarisation, defined as
!      (I_h - I_v) / (I_h + I_v), where horizontal should normally correspond 
!      to the X-direction of the image
     Integer, Intent(INOUT) :: lorentz_geometry ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the equivalent of a 
!            2-theta scan i.e. detector at equal distance
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
     Integer, Parameter :: Max_choices = 2 ! Dimension size of Lorentz
!      choices array
!  Local Variables:
     Character(Len = 30) :: lorentz_choice ! Name of the choice of Lorentz
!      correction to be applied
     Integer :: item ! Loop variable for data statement
     Integer :: num_choices ! Number of available choices
     Logical :: not_ok ! .True., until a proper user command is entered
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(9) ! User text
     Character(Len = 30) :: CHOICES(Max_choices) ! Available colour choices
!  External Functions:
!  Local Data:
     Data (CHOICES(item), item = 1, 2) / 'NONE', &
       'PARTIAL POWDER (2-THETA SCAN)' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_POLARISATION ' // Version)
     Else If (lorentz_geometry .Lt. 0 .Or. lorentz_geometry .Gt. 1) Then
        status = St_mod_fit2d + St_bad_int1
        Call ST_SAVE ('Subroutine F2D_INP_POLARISATION ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Apply polarisation correction
        MESSAGE(1) = 'Enter  "YES" if you want to correct for ' // &
          'the effects of beam polarisation.'
        MESSAGE(2) = 'FIT2D will take into account the 2-D ' // &
          'effects as function of  both 2-theta'
        MESSAGE(3) = 'angle and azimuth angle.  However,  if  ' // &
          'you  correct  for  the effects of'
        MESSAGE(4) = 'polarisation here it  will  be important ' // &
          'to  make  sure that they are not'
        MESSAGE(5) = 'corrected a second time by subsequent ' // &
          'processing software !'
        Call IO_INPL (.True., 0, 1, .True., &
          'CORRECT FOR X-RAY BEAM POLARISATION', 5, MESSAGE, 1, &
          'Enter "YES" or "NO"', correct_polarisation, status)
 
        If (correct_polarisation) Then
 
!        Input beam polarisation at the sample
           MESSAGE(1) = 'Enter the "polarisation" of the main ' // &
             'beam on the sample. This is defined as'
           MESSAGE(2) = '(I_h - I_v) / (I_h + I_v),  where I_h ' // &
             'is  the horizontal component  of  the'
           MESSAGE(3) = 'intensity and I_v is  the vertical.  ' // &
             '(The horizontal should correspond with'
           MESSAGE(4) = 'the X-direction on  an image.  ' // &
             'Normally for  a synchrotron the polarisation'
           MESSAGE(5) = 'is positive and approaches 1.0.  e.g. ' // &
             'Station 9.6 at  the SRS Daresbury has'
           MESSAGE(6) = 'been measured to  have  a polarisation ' // &
             'of 0.86.  This  is  a value which is'
           MESSAGE(7) = 'dependent on the X-ray source, ' // &
             'beam-line mirrors, and on the monochromator.'
           MESSAGE(8) = 'The beam-line scientist should  be  ' // &
             'able  to  give  a good estimate of this'
           MESSAGE(9) = 'number.'
           Call IO_INPR (.True., -1.0, 1.0, .True., &
             'BEAM POLARISATION (AT SAMPLE)', 9, MESSAGE, 1, &
             'Must be valid real number within given range', polarisation, &
             status)
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''Ready for input of Lorentz Geometry'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Lorentz correction
        lorentz_choice = CHOICES(lorentz_geometry + 1)
        num_choices = Max_choices
        MESSAGE(1) = 'Enter the type of "Lorentz" correction ' // &
          'which you want to apply to the '
        MESSAGE(2) = 'output intensities. At present the ' // &
          'following choices are available:'
        MESSAGE(3) = ' '
        MESSAGE(4) = '   NONE: No correction factors applied'
        MESSAGE(5) = '   PARTIAL POWDER (2-THETA SCAN): Correct ' // &
          'intensities to be equivalent to'
        MESSAGE(6) = '      a 2-theta scan with a single ' // &
          'counter. This allows standard powder'
        MESSAGE(7) = '      diffraction software to apply their ' // &
          'own Lorentz corrections.'
        not_ok = .True.
        Do While (not_ok)
           Call IO_MENU (.True., 'TYPE OF LORENTZIAN CORRECTION TO APPLY', 7, &
             MESSAGE, 1, 'ENTER ONE OF AVAILABLE CHOICES', Max_choices, &
             num_choices, CHOICES, lorentz_choice, status)
           not_ok = lorentz_choice .Eq. 'null'
        End Do
 
!     Convert Lorentz choice to the "lorentz_geometry" number
        If (lorentz_choice .Eq. 'NONE') Then
           lorentz_geometry = 0
        Else If (lorentz_choice .Eq. 'PARTIAL POWDER (2-THETA SCAN)') Then
           lorentz_geometry = 1
        Else
           Call IO_WRITE ('ERROR: Unknown user choice', status)
        End If
 
     End If
 
     End Subroutine F2D_INP_POLARISATION
!********1*********2*********3*********4*********5*********6*********7*********8
