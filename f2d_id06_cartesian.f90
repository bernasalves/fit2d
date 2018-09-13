!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_id06_cartesian.f90 *
!  *                        *
!  **************************
 
!+ F2D_ID06_CARTESIAN - FIT2D: convert polar to CARTESIAN image
     Subroutine F2D_ID06_CARTESIAN (xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, DATA, experiment, &
       azimuth_start, azimuth_end, &
       mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, MEMORY, &
       memory_defined, mx_pixel_size, my_pixel_size, status)
!  Description:
!    Converts polar image to cartesian image.      
!  Keywords:
!    Polar.Cartesian
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    27-May-2014: V0.2 Try removing normalisation (Hammersley)
!    22-May-2014: V0.1 Original(Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA" and
!      "MASK"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA" and
!      "MASK"
     Integer, Intent(IN) :: xnumdat ! Number of defined X-elements
     Integer, Intent(IN) :: ynumdat ! Number of defined Y-elements
     Integer, Intent(IN) :: xstrelm ! X-start of active data region
     Integer, Intent(IN) :: ystrelm ! Y-start of active data region
     Integer, Intent(IN) :: xendelm ! X-end of active data region
     Integer, Intent(IN) :: yendelm ! Y-end of active data region
     Real, Intent(IN) :: DATA(xmaxdat,ymaxdat) ! Data values
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Real, Intent(IN) :: azimuth_start ! Azimuth at start of full input data 
!      region (radians)
     Real, Intent(IN) :: azimuth_end ! Azimuth at start of full input data 
!      region (radians)
!  Export:
     Integer, Intent(OUT) :: mxnumdat ! Number of defined X-elements
     Integer, Intent(OUT) :: mynumdat ! Number of defined Y-elements
     Integer, Intent(OUT) :: mxstrelm ! X-start of active data region
     Integer, Intent(OUT) :: mystrelm ! Y-start of active data region
     Integer, Intent(OUT) :: mxendelm ! X-end of active data region
     Integer, Intent(OUT) :: myendelm ! Y-end of active data region
     Real, Intent(OUT) :: MEMORY(xmaxdat, ymaxdat) ! Memory values
     Logical, Intent(OUT) :: memory_defined ! .True., if ...
     Real, Intent(OUT) :: mx_pixel_size ! Memory X-pixel size
     Real, Intent(OUT) :: my_pixel_size ! Memory Y-pixel size
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Real :: angle ! Angle of azimuth of line
     Real cos_angle ! Cosine of angle
     Real sin_angle ! Sine of angle
     Integer :: stat ! Status return variable for "Allocate"
     Integer :: x ! Loop variable for X-direction
     Real :: x_beam ! X-position for centre of pattern
     Integer :: x_out ! X-direction output pixel 
     Real :: xwc ! X-world coordinate of pixel
     Integer :: y ! Loop variable for Y-direction
     Real :: y_beam ! Y-position for centre of pattern
     Integer :: y_out ! Y-direction output pixel 
     Real :: ywc ! Y-world coordinate of pixel
!  Local Arrays:
     Real, allocatable :: NORMALISE(:, :) ! Normalisation array 
!  Local Data:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_ID06_CARTESIAN'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ID06_CARTESIAN ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xendelm .Gt. xmaxdat .Or. xstrelm .Gt. &
       xendelm) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. yendelm .Gt. ymaxdat .Or. ystrelm .Gt. &
       yendelm) Then
        status = St_bad_adr2
     End If

!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_ID06_CARTESIAN ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2d_id06_cartesian'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Set new beam centre
        x_beam = Real(xmaxdat) / 2.0
        y_beam = Real(ymaxdat) / 2.0

!     Allocate memory for normalisation array
        Allocate (NORMALISE(xmaxdat, ymaxdat), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_ma + St_bad_malloc
           Call ST_SAVE ('Subroutine F2d_ID06_CARTESIAN ' // Version)
           Return
        End If
        
!     Initialise arrays
        Call MA_RVALUE (xmaxdat, ymaxdat, 1, 1, xmaxdat, ymaxdat, 0.0, &
          MEMORY, status)
        Call MA_RVALUE (xmaxdat, ymaxdat, 1, 1, xmaxdat, ymaxdat, 0.0, &
          NORMALISE, status)

        Do y = ystrelm, yendelm

           angle = azimuth_start + &
             ((azimuth_end - azimuth_start) * (Real(y) - 0.5) / Real(ynumdat))
           cos_angle = Cos(angle)
           sin_angle = Sin(angle)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''y = '', i4, '' azimuth (degrees) = '', g14.7)') &
!             y, angle * 180.0 / Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

           Do x = xstrelm, xendelm
 
!           Distance in metres from centre of pixel to beam centre (X-only)
              xwc = (Real(x) - 0.5 - experiment%x_beam) * &
                experiment%x_pixel_size

!           Rotate coordinate about the azimuth of the line
              ywc = xwc * sin_angle
              xwc = xwc * cos_angle

              x_out = Int(x_beam + xwc / experiment%x_pixel_size) + 1
              y_out = Int(y_beam + ywc / experiment%x_pixel_size) + 1

              If (x_out .Ge. 1 .And. x_out .Le. xmaxdat .And. &
                y_out .Ge. 1 .And. y_out .Le. ymaxdat) Then
                 MEMORY(x_out, y_out) = MEMORY(x_out, y_out) + DATA(x, y)
                 NORMALISE (x_out, y_out) = NORMALISE (x_out, y_out) + 1.0
              End If

           End Do

        End Do
 
     End If
 
!  Normalise output
!     Do y = 1, ymaxdat

!        Do x = 1, xmaxdat

!           If (NORMALISE(x, y) .Gt. 0) Then
!              MEMORY(x, y) = MEMORY(x, y) / NORMALISE(x, y)
!           Else
!              MEMORY(x, y) = 0.0
!           End If

!        End Do

!     End Do
     
!  Free dynamic memory
     Deallocate (NORMALISE)

!  Set data extent
     memory_defined = .True.
     mxnumdat = xmaxdat
     mynumdat = xmaxdat
     mxstrelm = 1
     mystrelm = 1
     mxendelm = xmaxdat
     myendelm = ymaxdat
     mx_pixel_size = experiment%x_pixel_size
     my_pixel_size = experiment%x_pixel_size

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_ID06_CARTESIAN: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_ID06_CARTESIAN
!********1*********2*********3*********4*********5*********6*********7*********8
 
