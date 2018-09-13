!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************************
!  *                          *
!  * f2d_rmap_orientation.f90 *
!  *                          *
!  ****************************
 
!+ F2D_RMAP_ORIENTATION: Reciprocal MAP plane ORIENTATION form
     Subroutine F2D_RMAP_ORIENTATION (xmaxmap, ymaxmap, zmaxmap, experiment, &
       xnummap, ynummap, znummap, LL, LR, UP, DIFFERENCE, C1, C5, CN, C2C1, &
       c2c1s, C3C1, c3c1s, C5C1, c5c1s, step, thickness, status)
!  Description:
!    User input of reciprocal map orientation
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    12-May-2009: V0.12 Implement easy volume definition form (Hammersley)
!    10-Oct-2008: V0.11 Option of easy volume definition (Hammersley)
!    21-Dec-2006: V0.10 Change calculation of "thickness" which was too small
!      (Hammersley)
!    22-Jun-2006: V0.9 Add slice "thickness" argument (Hammersley)
!    14-Mar-2006: V0.8 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    08-Nov-2005: V0.7 Export "LL, LR"" etc. (Hammersley)
!    13-May-2005: V0.6 Calculate "C5 - C1" difference vector (Hammersley)
!    12-May-2005: V0.5 Increase size of button text array (Hammersley)
!    11-May-2005: V0.4 Export "C2C1" etc. (Hammersley)
!    26-Apr-2005: V0.3 Add number of planes to construct (Hammersley)
!    22-Apr-2005: V0.2 Debugging: xnummap, ynummap defined improperly
!      (Hammersley)
!    20-Apr-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxmap ! First dimension of reciprocal map
     Integer, Intent(IN) :: ymaxmap ! Second dimension of reciprocal map
     Integer, Intent(IN) :: zmaxmap ! Third dimension of reciprocal map
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
     Integer, Intent(INOUT) :: xnummap ! Number of defined pixels in first
!      dimension of map
     Integer, Intent(INOUT) :: ynummap ! Number of defined pixels in second
!      dimension of map
     Integer, Intent(INOUT) :: znummap ! Number of defined pixels in third
!      dimension of map
!  Export:
     Real, Intent(OUT) :: LL(3) ! HKL of lower left-hand corner of volume to map
     Real, Intent(OUT) :: LR(3) ! HKL of lower right-hand corner of volume to
!       map
     Real, Intent(OUT) :: UP(3) ! HKL of point of upper boundary of first
!      section to map
     Real, Intent(OUT) :: DIFFERENCE(3) ! HKL difference between planes
     Real, Intent(OUT) :: C1(3) ! Starting coordinate of first section
     Real, Intent(OUT) :: C5(3) ! Difference vector between sections
     Real, Intent(OUT) :: CN(3) ! Normalised difference vector
     Real, Intent(OUT) :: C2C1(3) ! Difference vector C2 - C1
     Real, Intent(OUT) :: c2c1s ! Length of "C2C1"
     Real, Intent(OUT) :: C3C1(3) ! Difference vector C3 - C1
     Real, Intent(OUT) :: c3c1s ! Length of "C3C1"
     Real, Intent(OUT) :: C5C1(3) ! Difference vector C5 - C1
     Real, Intent(OUT) :: c5c1s ! Length of "C5C1"
     Real, Intent(OUT) :: step ! Step between voxels
     Real, Intent(OUT) :: thickness ! Thickness of a single section
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.12' ! Version number
     Integer, Parameter :: Max_chars = 43 ! Maximum number of characters in
!      a line
     Integer, Parameter :: Max_lines = 14 ! Number of lines in message
     Integer, Parameter :: Max_choices = 15 ! Maximum number of choices:
!      Note: Must of at least 5 more than 'num_choices' to allow for general
!      form buttons
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: dummy ! Dummy variable, not used
     Integer :: i ! Loop variable for dimensions
     Integer :: ierr ! Status return variable
     Real :: maximum_h ! Maximum H limit of reconstructed volume
     Real :: maximum_k ! Maximum K limit of reconstructed volume
     Real :: maximum_l ! Maximum L limit of reconstructed volume
     Real :: minimum_h ! Minimum H limit of reconstructed volume
     Real :: minimum_k ! Minimum K limit of reconstructed volume
     Real :: minimum_l ! Minimum L limit of reconstructed volume
     Integer :: num_choices ! Number of choices
     Integer :: retstat ! Return status variable
     Logical :: finished ! .True., when the input is finished O.K.
     Real :: c1s
     Real :: c2s
     Real :: c3proj
     Real :: c3s
     Real :: c4s
     Real :: cmax
     Real :: dome
     Real :: ddet
     Real :: prod
     Real :: scale ! Scaling for map
     Logical :: simple_volume_definition ! .True. if the volume is aligned to 
!      the hkl axes
     Real :: two_theta ! Two theta angle of position
!  Local Arrays:
     Character(Len = 22) :: BUTTONS(Max_choices) ! Choice buttons
     Character(Len = 80) :: ERROR(1) ! User error messages
     Character(Len = 60) :: FULL_PROMPTS(Max_choices) ! Choice full prompts
     Character(Len = 80) :: MESSAGES(1) ! User messages
     Character(Len = 70) :: PROMPT(2) ! User prompt text
     Character(Len = 10) :: STRINGS(2) ! Dummy string for call to "GS_FORM"
     Character(Len = 60) :: TEXT(Max_choices) ! Choice buttons
     Character(Len = Max_chars) :: TX(Max_lines) ! Text array
     Integer :: INTEGERS(Max_choices) ! Integer variables to be input
     Integer :: INTEGERS_LOWER(Max_choices) ! Lower bound of integer variables
     Integer :: INTEGERS_UPPER(Max_choices) ! Upper bound of integer variables
     Integer :: TYPES(Max_choices) ! Type of each variable:
!      1 = Integer
!      2 = Logical
!      3 = Real (single precision)
!      4 = Character string
!      5 = Colour index (GS)
!      6 = Data type (character string)
     Logical :: BOUND(Max_choices) ! .True., if variable range is bound
     Real :: C2(3) ! Cartesian coordinate of lower right-hand coordinate
     Real :: C3(3) ! Cartesian coordinate of upper coordinate
     Real :: C4(3) ! Cartesian coordinate of fourth corner
     Real :: REALS(Max_choices) ! Real variables to be input
     Real :: REALS_LOWER(Max_choices) ! Lower bound of real variables
     Real :: REALS_UPPER(Max_choices) ! Upper bound of real variables
!  External Functions:
!  Local Data:
     Data TX(   1) / ' ' /
     Data TX(   2) / '      -------------------------------'/
     Data TX(   3) / '      Welcome to the FIT2D Reciprocal'/
     Data TX(   4) / '      Map orientation form'/
     Data TX(   5) / '      -------------------------------'/
     Data TX(   6) / ' ' /
     Data TX(   7) / 'You can set the extent of the map and,'/
     Data TX(   8) / 'the orientation of the planes of terms'/
     Data TX(   9) / 'of three hkl positions.'/
     Data TX(  10) / ' ' /
     Data TX(  11) / '---------------'/
     Data TX(  12) / 'END OF HELP TEXT'/
     Data TX(  13) / '---------------'/
     Data TX(  14) / ' ' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_ORIENTATION ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
 
!     Try to get default values from internal data-base
        Call IO_INQ_LKEYVALUE ('SIMPLE_VOLUME_DEFINITION', &
          simple_volume_definition, retstat, status)
        Call IO_INQ_RKEYVALUE ('LOWER_LEFT_H', LL(1), retstat, status)
        Call IO_INQ_RKEYVALUE ('LOWER_LEFT_K', LL(2), retstat, status)
        Call IO_INQ_RKEYVALUE ('LOWER_LEFT_L', LL(3), retstat, status)
        Call IO_INQ_RKEYVALUE ('LOWER_RIGHT_H', LR(1), retstat, status)
        Call IO_INQ_RKEYVALUE ('LOWER_RIGHT_K', LR(2), retstat, status)
        Call IO_INQ_RKEYVALUE ('LOWER_RIGHT_L', LR(3), retstat, status)
        Call IO_INQ_RKEYVALUE ('UPPER_H', UP(1), retstat, status)
        Call IO_INQ_RKEYVALUE ('UPPER_K', UP(2), retstat, status)
        Call IO_INQ_RKEYVALUE ('UPPER_L', UP(3), retstat, status)
        Call IO_INQ_RKEYVALUE ('DIFFERENCE_H', DIFFERENCE(1), retstat, status)
        Call IO_INQ_RKEYVALUE ('DIFFERENCE_K', DIFFERENCE(2), retstat, status)
        Call IO_INQ_RKEYVALUE ('DIFFERENCE_L', DIFFERENCE(3), retstat, status)
        Call IO_INQ_IKEYVALUE ('RMAP_NUM_SECTIONS', znummap, retstat, status)
        znummap = Min(znummap, zmaxmap)

!     Find out if user wants simple hkl aligned volume
        Call GS_INPL (.True., 0, 1, .True., 'SIMPLE DEFINITION OF VOLUME', &
          1, '"YES" for hkl aligned volume, "NO" for arbitrary orientation', &
          1, 'Enter "YES" or "NO"', simple_volume_definition, status)
  
        finished = .False.
        Do While (.Not. finished)
 
           If (simple_volume_definition) Then

              num_choices = 10
              PROMPT(1) = 'RECIPROCAL SPACE'
              PROMPT(2) = 'VOLUME LIMITS'
 
              BUTTONS(1) = 'LOWER H'
              BUTTONS(2) = 'LOWER K'
              BUTTONS(3) = 'LOWER L'
              BUTTONS(4) = 'UPPER H'
              BUTTONS(5) = 'UPPER K'
              BUTTONS(6) = 'UPPER L'
              BUTTONS(7) = 'NUMBER PLANES'
 
              TYPES(1) = Gs_real
              TYPES(2) = Gs_real
              TYPES(3) = Gs_real
              TYPES(4) = Gs_real
              TYPES(5) = Gs_real
              TYPES(6) = Gs_real
              TYPES(7) = Gs_integer
 
              TEXT(1) = 'Minimum H limit of aligned volume'
              TEXT(2) = 'Minimum K limit of aligned volume'
              TEXT(3) = 'Minimum L limit of aligned volume'
              TEXT(4) = 'Maximum H limit of aligned volume'
              TEXT(5) = 'Maximum K limit of aligned volume'
              TEXT(6) = 'Maximum L limit of aligned volume'
              TEXT(7) = 'Number of planes in L direction to reconstruct'
 
              FULL_PROMPTS(1) = 'Enter minimum H value for the volume of ' // &
                'reciprocal space to reconstruct'
              FULL_PROMPTS(2) = 'Enter minimum K value for the volume of ' // &
                'reciprocal space to reconstruct'
              FULL_PROMPTS(3) = 'Enter minimum L value for the volume of ' // &
                'reciprocal space to reconstruct'
              FULL_PROMPTS(4) = 'Enter maximum H value for the volume of ' // &
                'reciprocal space to reconstruct'
              FULL_PROMPTS(5) = 'Enter maximum K value for the volume of ' // &
                'reciprocal space to reconstruct'
              FULL_PROMPTS(6) = 'Enter maximum L value for the volume of ' // &
                'reciprocal space to reconstruct'
              FULL_PROMPTS(7) = 'Enter number of planes to reconstruct'
 
              BOUND(1) = .True.
              BOUND(2) = .True.
              BOUND(3) = .True.
              BOUND(4) = .True.
              BOUND(5) = .True.
              BOUND(6) = .True.
              BOUND(7) = .True.
 
              REALS_LOWER(1) = -100.0
              REALS_UPPER(1) = 100.0
              REALS_LOWER(2) = -100.0
              REALS_UPPER(2) = 100.0
              REALS_LOWER(3) = -100.0
              REALS_UPPER(3) = 100.0
              REALS_LOWER(4) = -100.0
              REALS_UPPER(4) = 100.0
              REALS_LOWER(5) = -100.0
              REALS_UPPER(5) = 100.0
              REALS_LOWER(6) = -100.0
              REALS_UPPER(6) = 100.0
              INTEGERS_LOWER(7) = 1
              INTEGERS_UPPER(7) = zmaxmap
 
              REALS(1) = minimum_h
              REALS(2) = minimum_k
              REALS(3) = minimum_l
              REALS(4) = maximum_h
              REALS(5) = maximum_k
              REALS(6) = maximum_l
              INTEGERS(7) = Min(znummap, zmaxmap)
 
!           Output interactive graphical form
              Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, &
                Max_choices,num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, &
                BOUND, INTEGERS_LOWER, INTEGERS_UPPER, REALS_LOWER, &
                REALS_UPPER, INTEGERS, dummy, REALS, STRINGS, retstat, status)
 
!           Check for "CANCEL"
              If (retstat .Eq. -1) Then
                 status = St_escapevalue
                 Return
              End If
 
!           Set resulting values
              minimum_h = REALS(1)
              minimum_k = REALS(2)
              minimum_l = REALS(3)
              maximum_h = REALS(4)
              maximum_k = REALS(5)
              maximum_l = REALS(6)
              znummap = INTEGERS(7)

!           Set resulting values for arbitrary plane
              LL(1) = minimum_h
              LL(2) = minimum_k
              LL(3) = minimum_l
              LR(1) = maximum_h
              LR(2) = minimum_k
              LR(3) = minimum_l
              UP(1) = minimum_h
              UP(2) = maximum_l
              UP(3) = minimum_l

              DIFFERENCE(1) = 0
              DIFFERENCE(2) = 0
              DIFFERENCE(3) = (maximum_l - minimum_l) / &
                Real(Max(1, znummap - 1))

           Else

              num_choices = 10
              PROMPT(1) = 'FIRST SECTION LIMITS'
              PROMPT(2) = 'CONTROL FORM'
 
              BUTTONS(1) = 'LOWER LEFT-HAND H'
              BUTTONS(2) = 'LOWER LEFT-HAND K'
              BUTTONS(3) = 'LOWER LEFT-HAND L'
              BUTTONS(4) = 'LOWER RIGHT-HAND H'
              BUTTONS(5) = 'LOWER RIGHT-HAND K'
              BUTTONS(6) = 'LOWER RIGHT-HAND L'
              BUTTONS(7) = 'UPPER LEFT-HAND H'
              BUTTONS(8) = 'UPPER LEFT-HAND K'
              BUTTONS(9) = 'UPPER LEFT-HAND L'
              BUTTONS(10) = 'NUMBER SECTIONS'
 
              TYPES(1) = Gs_real
              TYPES(2) = Gs_real
              TYPES(3) = Gs_real
              TYPES(4) = Gs_real
              TYPES(5) = Gs_real
              TYPES(6) = Gs_real
              TYPES(7) = Gs_real
              TYPES(8) = Gs_real
              TYPES(9) = Gs_real
              TYPES(10) = Gs_integer
 
              TEXT(1) = 'H of lower left-hand corner of 1st section'
              TEXT(2) = 'K of lower left-hand corner of 1st section'
              TEXT(3) = 'L of lower left-hand corner of 1st section'
              TEXT(4) = 'H of lower right-hand corner of 1st section'
              TEXT(5) = 'K of lower right-hand corner of 1st section'
              TEXT(6) = 'L of lower right-hand corner of 1st section'
              TEXT(7) = 'H of upper left-hand corner  of 1st section'
              TEXT(8) = 'K of upper left-hand corner of 1st section'
              TEXT(9) = 'L of upper left-hand corner of 1st section'
              TEXT(10) = 'Number of sections to construct'
 
              FULL_PROMPTS(1) = 'Enter H value of the lower ' // &
                'left-hand corner of the first section'
              FULL_PROMPTS(2) = 'Enter K value of the lower ' // &
                'left-hand corner of the first section'
              FULL_PROMPTS(3) = 'Enter L value of the lower ' // &
                'left-hand corner of the first section'
              FULL_PROMPTS(4) = 'Enter H value of the lower ' // &
                'right-hand corner of the first section'
              FULL_PROMPTS(5) = 'Enter K value of the lower ' // &
                'right-hand corner of the first section'
              FULL_PROMPTS(6) = 'Enter L value of the lower ' // &
                'right-hand corner of the first section'
              FULL_PROMPTS(7) = 'Enter H value of the upper ' // &
                'left-hand corner of the first section'
              FULL_PROMPTS(8) = 'Enter K value of the upper ' // &
                'left-hand corner of the first section'
              FULL_PROMPTS(9) = 'Enter L value of the upper ' // &
                'left-hand corner of the first section'
              FULL_PROMPTS(10) = 'Enter number of sections to construct'
 
              BOUND(1) = .True.
              BOUND(2) = .True.
              BOUND(3) = .True.
              BOUND(4) = .True.
              BOUND(5) = .True.
              BOUND(6) = .True.
              BOUND(7) = .True.
              BOUND(8) = .True.
              BOUND(9) = .True.
              BOUND(10) = .True.
 
              REALS_LOWER(1) = -100.0
              REALS_UPPER(1) = 100.0
              REALS_LOWER(2) = -100.0
              REALS_UPPER(2) = 100.0
              REALS_LOWER(3) = -100.0
              REALS_UPPER(3) = 100.0
              REALS_LOWER(4) = -100.0
              REALS_UPPER(4) = 100.0
              REALS_LOWER(5) = -100.0
              REALS_UPPER(5) = 100.0
              REALS_LOWER(6) = -100.0
              REALS_UPPER(6) = 100.0
              REALS_LOWER(7) = -100.0
              REALS_UPPER(7) = 100.0
              REALS_LOWER(8) = -100.0
              REALS_UPPER(8) = 100.0
              REALS_LOWER(9) = -100.0
              REALS_UPPER(9) = 100.0
              INTEGERS_LOWER(10) = 1
              INTEGERS_UPPER(10) = zmaxmap
 
              REALS(1) = LL(1)
              REALS(2) = LL(2)
              REALS(3) = LL(3)
              REALS(4) = LR(1)
              REALS(5) = LR(2)
              REALS(6) = LR(3)
              REALS(7) = UP(1)
              REALS(8) = UP(2)
              REALS(9) = UP(3)
              INTEGERS(10) = Min(znummap, zmaxmap)
 
!           Output interactive graphical form
              Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, &
                Max_choices,num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, &
                BOUND, INTEGERS_LOWER, INTEGERS_UPPER, REALS_LOWER, &
                REALS_UPPER, INTEGERS, dummy, REALS, STRINGS, retstat, status)
 
!           Check for "CANCEL"
              If (retstat .Eq. -1) Then
                 status = St_escapevalue
                 Return
              End If
 
!           Set resulting values
              LL(1) = REALS(1)
              LL(2) = REALS(2)
              LL(3) = REALS(3)
              LR(1) = REALS(4)
              LR(2) = REALS(5)
              LR(3) = REALS(6)
              UP(1) = REALS(7)
              UP(2) = REALS(8)
              UP(3) = REALS(9)
              znummap = INTEGERS(10)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
              num_choices = 3
              PROMPT(1) = 'HKL DIFFERENCE'
              PROMPT(2) = 'CONTROL FORM'
 
              BUTTONS(1) = 'H DIFFERENCE'
              BUTTONS(2) = 'K DIFFERENCE'
              BUTTONS(3) = 'L DIFFERENCE'
 
              TYPES(1) = Gs_real
              TYPES(2) = Gs_real
              TYPES(3) = Gs_real
 
              TEXT(1) = 'h difference between sections'
              TEXT(2) = 'k difference between sections'
              TEXT(3) = 'l difference between sections'
 
              FULL_PROMPTS(1) = 'Enter difference in h between sections'
              FULL_PROMPTS(2) = 'Enter difference in k between sections'
              FULL_PROMPTS(3) = 'Enter difference in l between sections'
 
              BOUND(1) = .True.
              BOUND(2) = .True.
              BOUND(3) = .True.
 
              REALS_LOWER(1) = -100.0
              REALS_UPPER(1) = 100.0
              REALS_LOWER(2) = -100.0
              REALS_UPPER(2) = 100.0
              REALS_LOWER(3) = -100.0
              REALS_UPPER(3) = 100.0
 
              REALS(1) = DIFFERENCE(1)
              REALS(2) = DIFFERENCE(2)
              REALS(3) = DIFFERENCE(3)
 
!           Output interactive graphical form
              Call GS_FORM (2, 2, PROMPT, Max_lines, Max_lines, TX, &
                Max_choices, num_choices, BUTTONS, TYPES, TEXT, FULL_PROMPTS, &
                BOUND, dummy, dummy, REALS_LOWER, REALS_UPPER, dummy, dummy, &
                REALS, STRINGS, retstat, status)
 
!           Check for "CANCEL"
              If (retstat .Eq. -1) Then
                 status = St_escapevalue
                 Return
              End If
 
!           Set resulting values
              DIFFERENCE(1) = REALS(1)
              DIFFERENCE(2) = REALS(2)
              DIFFERENCE(3) = REALS(3)
 
           End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!        Convert the reciprocal-lattice coordinates to cartesian coordinates
           Call F2D_GMPRD (experiment%UB_MATRIX, LL, C1, 3, 3, 1)
           Call F2D_GMPRD (experiment%UB_MATRIX, LR, C2, 3, 3, 1)
           Call F2D_GMPRD (experiment%UB_MATRIX, UP, C3, 3, 3, 1)
           Call IO_WRITE ('INFO: Cartesian coordinates of ' // &
             'user-entered reciprocal-lattice coordinates:', status)
           Write (message, '(''      1: '', 3f10.4)') C1
           Call IO_WRITE (message, status)
           Write (message, '(''      2: '', 3f10.4)') C2
           Call IO_WRITE (message, status)
           Write (message, '(''      3: '', 3f10.4)') C3
           Call IO_WRITE (message, status)
 
!        Find the differences and their cross-product
           Do i = 1, 3
              C2C1(i) = C2(i) - C1(i)
              C3C1(i) = C3(i) - C1(i)
           End Do
           Call F2D_VPRODT(C2C1, C3C1, CN)
 
!        Normalise the cross-product and check that the points are not collinear
           Call F2D_NORMAL (CN, ierr)
 
           If (ierr .Ne. 0) Then
              Call GS_FWARNING (1, 1, 'Points defining plane are collinear', &
                status)
              Write (message, '(''C2C1(1: 3) = '', 3(g12.5, 1x))') C2C1
              Call IO_WRITE (message, status)
              Write (message, '(''C3C1(1: 3) = '', 3(g12.5, 1x))') C3C1
              Call IO_WRITE (message, status)
              Write (message, '(''CN(1: 3) = '', 3(g12.5, 1x))') CN
              Call IO_WRITE (message, status)
           Else
              finished = .True.
           End If
 
        End Do
 
!     Save values in internal data-base
        Call IO_SET_RKEYVALUE ('LOWER_LEFT_H', LL(1), retstat, status)
        Call IO_SET_RKEYVALUE ('LOWER_LEFT_K', LL(2), retstat, status)
        Call IO_SET_RKEYVALUE ('LOWER_LEFT_L', LL(3), retstat, status)
        Call IO_SET_RKEYVALUE ('LOWER_RIGHT_H', LR(1), retstat, status)
        Call IO_SET_RKEYVALUE ('LOWER_RIGHT_K', LR(2), retstat, status)
        Call IO_SET_RKEYVALUE ('LOWER_RIGHT_L', LR(3), retstat, status)
        Call IO_SET_RKEYVALUE ('UPPER_H', UP(1), retstat, status)
        Call IO_SET_RKEYVALUE ('UPPER_K', UP(2), retstat, status)
        Call IO_SET_RKEYVALUE ('UPPER_L', UP(3), retstat, status)
        Call IO_SET_RKEYVALUE ('DIFFERENCE_H', DIFFERENCE(1), retstat, status)
        Call IO_SET_RKEYVALUE ('DIFFERENCE_K', DIFFERENCE(2), retstat, status)
        Call IO_SET_RKEYVALUE ('DIFFERENCE_L', DIFFERENCE(3), retstat, status)
        Call IO_SET_IKEYVALUE ('RMAP_NUM_SECTIONS', znummap, retstat, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Ensure that the two limiting vectors in the plane are perpendicular,
!     correcting C3 if necessary.
!     Find the square of their moduli for later use.
        c2c1s = C2C1(1) * C2C1(1) + C2C1(2) * C2C1(2) + C2C1(3) * C2C1(3)
        c3proj = (C3C1(1) * C2C1(1) + C3C1(2) * C2C1(2) + C3C1(3) * C2C1(3)) / &
          Sqrt(c2c1s)
 
        Do i = 1, 3
           C3C1(i) = C3C1(i) - c3proj * C2C1(i) / Sqrt(c2c1s)
        End Do
 
        c3c1s = C3C1(1) * C3C1(1) + C3C1(2) * C3C1(2) + C3C1(3) * C3C1(3)
 
!     For successive sections, find the projection of the vector between
!     successive planes onto the normal [ = (C5.CN)CN ]
        Call F2D_GMPRD (experiment%UB_MATRIX, DIFFERENCE, C5, 3, 3, 1)
        prod = C5(1) * CN(1) + C5(2) * CN(2) + C5(3) * CN(3)
        Do i = 1, 3
           C5(i) = prod * CN(i)
        End Do
 
        Do i = 1, 3
           C5C1(i) = C5(i) - C1(i)
        End Do
        c5c1s = C5C1(1) * C5C1(1) + C5C1(2) * C5C1(2) + C5C1(3) * C5C1(3)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Find the step in each direction of the first section, but only save
!     "xnummap" and "ynummap" for later use
 
!     First find the largest scattering vector in the plotted region
        Do i = 1, 3
           C3(i) = C1(i) + C3C1(i)
           C4(i) = C2(i) + C3C1(i)
        End Do
 
        Call IO_WRITE ('INFO: Cartesian coordinate limits of ' // &
          'first section:', status)
        Write (message, '(''      1: '', 3f10.4)') C1
        Call IO_WRITE (message, status)
        Write (message, '(''      2: '', 3f10.4)') C2
        Call IO_WRITE (message, status)
        Write (message, '(''      3: '', 3f10.4)') C3
        Call IO_WRITE (message, status)
        Write (message, '(''      4: '', 3f10.4)') C4
        Call IO_WRITE (message, status)
 
        c1s = C1(1) * C1(1) + C1(2) * C1(2) + C1(3) * C1(3)
        c2s = C2(1) * C2(1) + C2(2) * C2(2) + C2(3) * C2(3)
        c3s = C3(1) * C3(1) + C3(2) * C3(2) + C3(3) * C3(3)
        c4s = C4(1) * C4(1) + C4(2) * C4(2) + C4(3) * C4(3)
        cmax = Sqrt(Max(c1s, c2s, c3s, c4s))
 
!     Assume that the step size is about 0.1 deg for all scans, and find what
!     this corresponds to in reciprocal-space units for the longest
!     scattering vector to be plotted
        dome = 0.1 * cmax * 3.1415926 / 180.0
 
!     For ID20 assume a step of 0.02 deg.
!     DOME = 0.02 * CMAX/RAD
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''X/Y pixel size = '', 2g12.5)')
!     :       x_pixel_size, y_pixel_size
!     Write (*, '(''sample_distance = '', g12.5)') sample_distance
!     Write (*, '(''wavelength = '', g12.5)') wavelength
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Find what the larger detector pixel spacing is in reciprocal-space units
        ddet = Max(experiment%x_pixel_size, experiment%y_pixel_size) / &
          (experiment%detector_distance * experiment%wavelength * 1.0e10)
 
!     two_theta = Atan2(Max(x_pixel_size, y_pixel_size),
!     :       sample_distance)
!     ddet = wavelength / (2.0 * Sin(two_theta / 2.0))
 
!     ddet = wavelength * 1.0e10 / (2.0 *
!     :       Sin( Atan2(Max(x_pixel_size, y_pixel_size),
!     :       sample_distance) / 2.0))
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''two_theta = '', g12.5)')
!     :       two_theta * 180.0 / 3.1415926
!     Write (*, '(''ddet = '', g12.5)') ddet
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     The factor sqrt(2) was found simply by trial and error.  We want good
!     resolution without too many gaps in the output array. Tried 1.0 for a 
!     while.
        step = Max(ddet, dome) / (3.0 * Sqrt(2.0))
        xnummap = Int(Sqrt(c2c1s) / step - 1.0)
        ynummap = Int(Sqrt(c3c1s) / step - 1.0)
        scale = Max(Real(xnummap) / Real(xmaxmap), &
          Real(ynummap) / Real(ymaxmap))
 
        If (scale .Gt. 1.0) Then
           xnummap = Real(xnummap) / scale
           ynummap = Real(ynummap) / scale
        End If
 
        Write (message, '(''INFO: dome = '', g12.5, '' ddet = '', g12.5)') &
          dome, ddet
        Call IO_WRITE (message, status)
        Write (message, '(''INFO: step = '', g12.5)') step
        Call IO_WRITE (message, status)
        Write (message, '(''INFO: Size of section (X/Y) = '', 2i6)') &
          xnummap, ynummap
        Call IO_WRITE (message, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Clear screen
        Call GS_BACKGROUND (status)

!     Thickness of a single section
!        thickness = Sqrt(C5(1)**2 + C5(2)**2 + C5(3)**2)
        thickness = Sqrt(c5c1s)
        MESSAGES(1) = 'Enter required thickness of the transformed slices'
        ERROR(1) = 'Enter valid number within given range'
        Call GS_INPR (.True., 0.0, thickness * 1.0001, .True., &
          'THICKNESS OF A SINGLE SECTION', 1, &
          MESSAGES, 1, ERROR, thickness, status)

     End If
 
     End Subroutine F2D_RMAP_ORIENTATION
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
