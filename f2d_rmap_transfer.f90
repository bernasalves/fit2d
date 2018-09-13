!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_rmap_transfer.f90 *
!  *                       *
!  *************************
 
!+ F2D_RMAP_TRANSFER: Reciprocal TRANSFER map section to data array
     Subroutine F2D_RMAP_TRANSFER (LL, LR, UP, DIFFERENCE, xmaxmap, ymaxmap, &
       zmaxmap, xnummap, ynummap, znummap, MAP, NORMALISE, step, xmaxdat, &
       ymaxdat, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, XAXIS, &
       YAXIS, DATA, title, xlabel, ylabel, zlabel, status)
!  Description:
!    Transfer normalised section to data array
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    20-Jun-2006: V0.5 Set non-set pixels to zero (Hammersley)
!    08-Nov-2005: V0.4 Calculate axis values for simple planes (Hammersley)
!    07-Nov-2005: V0.3 Fill blank squares with surrounding values where
!      possible (Hammersley)
!    09-Jun-2005: V0.2 Set axes to step units (Hammersley)
!    21-Apr-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: LL(3) ! HKL of lower left-hand corner of volume to map
     Real, Intent(IN) :: LR(3) ! HKL of lower right-hand corner of volume to map
     Real, Intent(IN) :: UP(3) ! HKL of point of upper boundary of first section
!      to map
     Real, Intent(IN) :: DIFFERENCE(3) ! HKL difference between planes
     Integer, Intent(IN) :: xmaxmap ! First dimension of reciprocal map
     Integer, Intent(IN) :: ymaxmap ! Second dimension of reciprocal map
     Integer, Intent(IN) :: zmaxmap ! Third dimension of reciprocal map
     Integer, Intent(IN) :: xnummap ! Number of defined pixels in X-direction
     Integer, Intent(IN) :: ynummap ! Number of defined pixels in Y-direction
     Integer, Intent(IN) :: znummap ! Number of defined pixels in Z-direction
     Real, Intent(IN) :: MAP(xmaxmap, ymaxmap, zmaxmap) ! Reciprocal map array
     Real, Intent(IN) :: NORMALISE(xmaxmap, ymaxmap, zmaxmap)
!      Normalisation array
     Real, Intent(IN) :: step ! Step size along axes
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(OUT) :: ynumdat ! Number of data elements in Y-direction
     Integer, Intent(OUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(OUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(OUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(OUT) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(OUT) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(OUT) :: YAXIS(ymaxdat) ! Y-axis values
     Real, Intent(OUT) :: DATA(xmaxdat, ymaxdat) ! The data values
     Character(Len = *), Intent(OUT) :: title ! Title for plot
     Character(Len = *), Intent(OUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(OUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(OUT) :: zlabel ! Z-axis label for plot
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
     Real, Parameter :: Scale = 0.00001 ! Scaling to apply to output figures
!  Local Variables:
     Integer :: axis ! 0: if axis is changing in more than one indice
!      1: if axis is changing only in H direction
!      2: if axis is changing only in K direction
!      3: if axis is changing only in L direction
     Integer :: num_con ! Number of contributions to an average
     Integer :: x ! Loop variable for first dimension
     Integer :: y ! Loop variable for second dimension
     Integer, Save :: z = 1 ! Loop variable for third dimension
     Real :: total ! Running total for calculating average
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_TRANSFER ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input section to transfer
        Call GS_INPI (.True., 1, znummap, .True., 'SECTION TO TRANSFER', 1, &
          'Enter number of section / plane to transfer', 1, &
          'Enter integer within given range', z, status)
 
!     Set output array size
        xnumdat = Min(xmaxdat, xnummap)
        ynumdat = Min(ymaxdat, ynummap)
        xstrelm = 1
        ystrelm = 1
        xendelm = xnumdat
        yendelm = ynumdat
 
!     Transfer section
        Do y = 1, ynumdat
 
           Do x = 1, xnumdat
 
              If (NORMALISE(x, y, z) .Gt. 0.0) Then
                 DATA(x, y) = MAP(x, y, z) * Scale / NORMALISE(x, y, z)
              Else
 
!              Value is not defined, so try averaging adjacent values
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
                    DATA(x, y) = total * scale / Real(num_con)
                 Else
                    DATA(x, y) = 0.0
                 End If
 
              End If
 
           End Do
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Create X-axis data
        If (LR(1) - LL(1) .Ne. 0.0 .And. LR(2) - LL(2) .Eq. 0.0 .And. &
          LR(3) - LL(3) .Eq. 0.0) Then
           axis = 1
        Else If (LR(1) - LL(1) .Eq. 0.0 .And. LR(2) - LL(2) .Ne. 0.0 .And. &
          LR(3) - LL(3) .Eq. 0.0) Then
           axis = 2
        Else If (LR(1) - LL(1) .Eq. 0.0 .And. LR(2) - LL(2) .Eq. 0.0 .And. &
          LR(3) - LL(3) .Ne. 0.0) Then
           axis = 3
        Else
           axis = 0
        End If
 
        If (axis .Eq. 0) Then
 
           Do x = 1, xnumdat
              XAXIS(x) = Real(x) - 0.5
           End Do
 
           xlabel = 'Combined indices direction'
 
        Else
 
           Do x = 1, xnumdat
              XAXIS(x) = LL(axis) + Real(x - 1) * (LR(axis) - LL(axis)) / &
                Real(xnummap - 1)
           End Do
 
           If (axis .Eq. 1) Then
              xlabel = 'H indices direction'
           Else If (axis .Eq. 2) Then
              xlabel = 'K indices direction'
           Else If (axis .Eq. 3) Then
              xlabel = 'L indices direction'
           End If
 
        End If
 
!     Create Y-axis data
        If (UP(1) - LL(1) .Ne. 0.0 .And. UP(2) - LL(2) .Eq. 0.0 .And. &
          UP(3) - LL(3) .Eq. 0.0) Then
           axis = 1
        Else If (UP(1) - LL(1) .Eq. 0.0 .And. UP(2) - LL(2) .Ne. 0.0 .And. &
          UP(3) - LL(3) .Eq. 0.0) Then
           axis = 2
        Else If (UP(1) - LL(1) .Eq. 0.0 .And. UP(2) - LL(2) .Eq. 0.0 .And. &
          UP(3) - LL(3) .Ne. 0.0) Then
           axis = 3
        Else
           axis = 0
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Y-axis = '', i1)') axis
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        If (axis .Eq. 0) Then
 
           Do y = 1, ynumdat
              YAXIS(y) = Real(y) - 0.5
           End Do
 
           ylabel = 'Combined indices direction'
 
        Else
 
          Do y = 1, ynumdat
              YAXIS(y) = LL(axis) + Real(y - 1) * (UP(axis) - LL(axis)) / &
                Real(ynummap - 1)
           End Do
 
           If (axis .Eq. 1) Then
              ylabel = 'H indices direction'
           Else If (axis .Eq. 2) Then
              ylabel = 'K indices direction'
           Else If (axis .Eq. 3) Then
              ylabel = 'L indices direction'
           End If
 
        End If
 
!     Create title
        If (DIFFERENCE(1) .Ne. 0.0 .And. DIFFERENCE(2) .Eq. 0.0 .And. &
          DIFFERENCE(3) .Eq. 0.0) Then
           Write (title, '(''Plane H = '', g12.4)') LL(1) + Real(z - 1) * &
             DIFFERENCE(1)
        Else If (DIFFERENCE(1) .Eq. 0.0 .And. DIFFERENCE(2) .Ne. 0.0 .And. &
          DIFFERENCE(3) .Eq. 0.0) Then
           Write (title, '(''Plane K = '', g12.4)') LL(2) + Real(z - 1) * &
             DIFFERENCE(2)
        Else If (DIFFERENCE(1) .Eq. 0.0 .And. DIFFERENCE(2) .Eq. 0.0 .And. &
          DIFFERENCE(3) .Ne. 0.0) Then
           Write (title, '(''Plane L = '', g12.4)') LL(3) + Real(z - 1) * &
             DIFFERENCE(3)
        Else
           Write (title, '(''Combined HKL Plane'')')
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Increment z
        z = z + 1
        If (z .Gt. znummap) Then
           z = 1
        End If

        zlabel = ' '

     End If 
     
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Debug: Section transferred'')')
!  Write (*, '(''xnumdat, ynumdat = '', 2i6)') xnumdat, ynumdat
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_RMAP_TRANSFER
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
