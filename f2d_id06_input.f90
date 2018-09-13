!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_id06_input.f90 *
!  *                    *
!  **********************
 
!+ F2D_ID06_INPUT: F2D: ID06 INPUT
     Subroutine F2D_ID06_INPUT ( &
       input_options, xmaxdat, ymaxdat, variances_exist, data_defined, &
       file_name, xnumdat, ynumdat, XAXIS, YAXIS, DATA, VARIANCES, &
       title, xlabel, ylabel, zlabel, experiment, status)
!  Description:
!    Inputs data from two files and subtracts background, and forms composite
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    29-Apr-2014: V0.3 Input number of images to input (Hammersley)
!    07-Mar-2014: V0.2 Set Y number of data points correctly! (Hammersley)
!    15-Jan-2014: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc' ! IO data definitions
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc' ! GS constants
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
     Integer, Intent(IN) :: xmaxdat ! First dimension of array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array
!  Import/Export:
     Logical, Intent(INOUT) :: variances_exist ! If .False. on input no
!      variance array will be input. If .True., on input, this will be set 
!      .False., if the file format does not provide variances
     Logical, Intent(INOUT) :: data_defined ! .True., if data has been defined
!  Export:
     Character(Len = *), Intent(OUT) :: file_name ! Full name of file
     Integer, Intent(OUT) :: xnumdat ! Defines X-extent of defined data
     Integer, Intent(OUT) :: ynumdat ! Defines Y-extent of defined data
     Real, Intent(OUT) :: XAXIS(xmaxdat) ! Array to contain X-coordinate data
     Real, Intent(OUT) :: YAXIS(ymaxdat) ! Array to contain Y-coordinate data
     Real, Intent(OUT) :: DATA(xmaxdat, ymaxdat) ! Array to contain data values
     Real, Intent(OUT) :: VARIANCES(xmaxdat, ymaxdat) ! Array to contain data 
!      variance values. Only used if "variances_exist" is .True. on input, and
!      only filled if "variances_exist" is .True. on output
     Character(Len = *), Intent(OUT) :: title ! Title for data
     Character(Len = *), Intent(OUT) :: xlabel ! Label for X-axis of data
     Character(Len = *), Intent(OUT) :: ylabel ! Label for Y-axis of data
     Character(Len = *), Intent(OUT) :: zlabel ! Label for Z-axis of data
     Type(EXPERIMENTAL_DETAILS), Intent(OUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Integer, Parameter :: Max_prompt = 2 ! Dimension of prompt text array
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Type(EXPERIMENTAL_DETAILS) :: exp_dummy ! Details of experiment for dark
!      current image
     Integer, Save :: num_images = 1 ! Number of data images to input
     Integer :: num_prompt ! Number of lines of prompt text to output
     Integer :: retstat ! Status return for allocate
     Logical :: temp_data_defined ! .True., if data has been input
     Character(Len = 256) :: temp_title ! Storage for original file title
     Integer :: x ! Loop variable for X-direction
     Integer :: xnumdark ! Number of X-pixels in dark current image
     Integer :: xnumdat2 ! Number of X-pixels in 2nd data image
     Integer :: y ! Loop variable for Y-direction
     Integer :: ynumdark ! Number of Y-pixels in dark current image
     Integer :: ynumdat2 ! Number of Y-pixels in 2nd data image
!  Local Arrays:
     Real, Allocatable :: DARK(:, :) ! Temporay array for detector dark current
     Real, Allocatable :: DATA2(:, :) ! Temporay array for second data image
     Character(Len = 132) :: PROMPT(Max_prompt) ! Use prompt text,
!      for the top of the file selection form
!     Real, Dimension(xmaxdat, ymaxdat) :: VARIANCES2 ! Temporay array for 2nd 
!      variances
     Real, Allocatable :: VARIANCES2(:, :) ! Temporay array for 2nd variances
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ID06_INPUT ' // Version)
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
        status = St_mod_fio + status
        Call ST_SAVE ('Subroutine F2D_ID06_INPUT ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_ID06_INPUT'')')
!       Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Number of data images to input
        Call GS_INPI (.True., 1, 2, .True., 'NUMBER OF DATA IMAGES', 1, &
          'Enter number of data images to input', 1, 'Enter 1 or 2', &
          num_images, status)

!     Allocate arrays
        If (num_images .Eq. 2) Then

           Allocate (DATA2(xmaxdat, ymaxdat), Stat = retstat)

           If (retstat .Ne. 0) Then
 
!           User warning message
              Call GS_FWARNING (1, 1, 'Failed to allocate ' // &
                'memory for 2nd data image. You must reduce the sizes', status)
              Return

           End If

           If (variances_exist) Then

              Allocate (VARIANCES2(xmaxdat, ymaxdat), Stat = retstat)

              If (retstat .Ne. 0) Then
 
!              User warning message
                 Call GS_FWARNING (1, 1, 'Failed to allocate memory ' // &
                   'for 2nd data image. You must reduce the sizes', status)
                 Return

              End If

           End If

        End If

        Allocate (DARK(xmaxdat, ymaxdat), Stat = retstat)

        If (retstat .Ne. 0) Then
 
!        User warning message
           Call GS_FWARNING (1, 1, 'Failed to allocate ' // &
             'memory for dark current. You must reduce the sizes', status)
           Return

        End If

!     Input first image data
        PROMPT(1) = 'SELECT FILE WITH FIRST IMAGE'
        PROMPT(2) = '(click on "HELP" for list of formats)'
        num_prompt = 2
        Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 0, &
          input_options, xmaxdat, &
          ymaxdat, variances_exist, temp_data_defined, file_name, &
          xnumdat, ynumdat, XAXIS, YAXIS, DATA, VARIANCES, &
          title, xlabel, ylabel, zlabel, experiment, status)
        temp_title = title


        If (temp_data_defined .And. num_images .Eq. 2) Then

!        Input second image data
           PROMPT(1) = 'SELECT FILE WITH SECOND IMAGE'
           PROMPT(2) = '(click on "HELP" for list of formats)'
           Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 0, &
             input_options, xmaxdat, &
             ymaxdat, variances_exist, temp_data_defined, file_name, &
             xnumdat2, ynumdat2, XAXIS, YAXIS, DATA2, VARIANCES2, &
             title, xlabel, ylabel, zlabel, exp_dummy, status)

        End If

        If (temp_data_defined) Then

!        Input dark current data
           PROMPT(1) = 'SELECT FILE WITH DETECTOR DARK CURRENT'
           PROMPT(2) = '(click on "HELP" for list of formats)'
           Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 0, &
             input_options, xmaxdat, &
             ymaxdat, variances_exist, temp_data_defined, file_name, &
             xnumdark, ynumdark, XAXIS, YAXIS, DARK, VARIANCES2, &
             title, xlabel, ylabel, zlabel, exp_dummy, status)

           If (temp_data_defined) Then
              
              If (xnumdat .Eq. xnumdark .And. ynumdat .Eq. ynumdark) Then

!              Subtract dark current
                 Do y = 1, ynumdat

                    Do x = 1, xnumdat
                       DATA(x, y) = DATA(x, y) - DARK(x, y)
                    End Do

                 End Do

              Else
                 Call GS_FWARNING (1, 1, 'IMAGES OF DIFFERING SIZES!', status)
              End If

              If (num_images .Eq. 2) Then

                 If (ynumdat * 2 .Le. ymaxdat) Then

                    If (xnumdat .Eq. xnumdat2 .And. ynumdat .Eq. ynumdat2) Then

!                    Form composite image
                       Do y = 1, ynumdat

                          Do x = 1, xnumdat
                             DATA(x, y + ynumdat) = DATA2(x, y) - DARK(x, y)
                          End Do

                       End Do

!                    Complete Y-axis values
                       Do y = 1, ynumdat
                          YAXIS(y + ynumdat) = YAXIS(ynumdat) + Real(y)
                       End Do

                       ynumdat = ynumdat * 2
                       data_defined = .True.
                       title = temp_title

                    Else
                       Call GS_FWARNING (1, 1, 'IMAGES OF DIFFERING SIZES!', &
                         status)
                    End If

                 Else
                    Call GS_FWARNING (1, 1, &
                      'ARRAYS TOO SMALL FOR COMPOSITE IMAGE', status)
                 End If

              Else

                 data_defined = .True.
                 title = temp_title

              End If

           End If
                 
        End If

!     Free temporary arrays
        Deallocate (DARK)

        If (num_images .Eq. 2) Then

           Deallocate (DATA2)

           If (variances_exist) Then
              Deallocate (VARIANCES2)
           End If

        End If

     End If
 
     End Subroutine F2D_ID06_INPUT
!********1*********2*********3*********4*********5*********6*********7*********8



