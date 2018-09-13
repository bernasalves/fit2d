!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_linearise.f90 *
!  *                   *
!  *********************
 
!+ F2D_LINEARISE - Fit 2-D LINEARISE film data
     Subroutine F2D_LINEARISE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, VARIANCES, status)
!  Description:
!    If variance array exists the user is offered the possibility
!    to define the variances based on the Selwyn Granularity at
!    uniform optical density, the fog level and the saturation
!    level. The Selwyn Granularity is assumed to follow a power
!    law. Empirically the power seems to be about 0.6.
!
!    Applies non-linearity corrections to micro densitority film
!    data. The correction curve is approximated to a quadratic, but
!    accurate meassurements shows this works.
!  Method:
!    See MOSFLM help documentation:
!    D_corr = D_measured + C * D_measured**2
!    where C is 0.055 for Kodirex
!    C is 0.06 for Kodax No-screen C (or maybe 0.07)
!    C is 0.07 for CEA
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    13-Sep-1993: V0.3 Reduce "bad" variance value to stop overflow problems
!      (Hammersley)
!    19-Aug-1993: V0.2 Calculate variances, based on Selwyn granularity and 
!      power law (Hammersley)
!    21-Apr-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Logical, Intent(IN) :: variances_exist ! .True., if the variances array
!      exists
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data array
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! Variances array
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Real, Save :: area = 2500.0 ! Area of digitilisation aperature in
!      square microns
     Real, Save :: correction_constant = 0.07 ! The quadratic term correction 
!      constant for the film being used
     Real, Save :: film_fog_od = 0.16 ! Optical density of film fog
     Real :: lower_data ! Lowest relaible data value, based on film fog level
     Real :: optical_density ! Measured optical density of a pixel
     Real, Save :: power = 0.6 ! The exponent of the power law followed by the
!      Selwyn granularity as a function of optical density
     Real, Save :: saturation = 254.0 ! Data value considered to be saturated
     Real :: scaled_correction ! Correction factor scaled to pixel units
     Real, Save :: selwyn_granularity = 3.7 ! The Selwyn granularity at unit
!      optical density
     Real :: value_od1 = 128.0 ! The data value corresponding to an optical
!      density of one
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(5) ! User messages
!  External Functions:
     Integer, External :: St_errorcode ! Gives error code from "status" value
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LINEARISE ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm .Gt. xmaxdat .Or. xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Gt. ymaxdat .Or. yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_LINEARISE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Enter maximum optical density for recorded data
        Call IO_INPR (.True., 0.001, 1.0e6, .True., &
          'VALUE FOR UNIT OPTICAL DENSITY', 1, &
          'Enter data value corresponding to optical density ' // &
          'of one (usually 128 for Daresbury scanner)', 1, &
          'Enter value within given range', value_od1, status)
 
        If (variances_exist) Then
 
!        Selwyn Granularity
           MESSAGE(1) = 'Enter the Selwyn granularity for the ' // &
             'film being used:'
           MESSAGE(2) = 'e.g. MOSFLM uses 3.7 for Daresbury ' // &
             'scanned CEA film and suggests 2.5'
           MESSAGE(3) = 'for other CEA film (old MOSFLM ' // &
             'documentation suggests 2.3). Other'
           MESSAGE(4) = 'values from Morimoto and Uyeda ' // &
             '(Acta Cryst 16, pp. 1107, 1963):'
           MESSAGE(5) = 'Kodirex 3.2, Kodak No Screen 2.7'
           Call IO_INPR (.True., 0.1, 10.0, .True., 'SELWYN GRANULARITY', 5, &
             MESSAGE, 1, 'Enter value within given range', selwyn_granularity, &
             status)
 
!        Fog level for film
           MESSAGE(1) = 'Enter fog level in optical density ' // &
             'units of the film being used:'
           MESSAGE(2) = 'e.g. 0.16 for fresh CEA film, ' // &
             '0.30 for Kodirex, 0.20 for Kodak No Screen '
 
           Call IO_INPR (.True., 0.001, 2.0, .True., &
             'FILM FOG LEVEL (OD UNITS)', 2, MESSAGE, 1, &
             'Enter value within given range', film_fog_od, status)
           lower_data = film_fog_od * value_od1
 
!        Saturation value
           MESSAGE(1) = 'Enter saturation value for data:'
           MESSAGE(2) = 'e.g. highest reliable value, 254.0 for ' // &
             '0-255 microdensitometers'
           Call IO_INPR (.True., 0.001, 1.0e9, .True., &
             'DATA SATURATION VALUE', 2, MESSAGE, 1, &
             'Enter value within given range', saturation, status)
 
!        Power law of change in Selwyn granularity
           MESSAGE(1) = 'Enter exponent for power law ' // &
             'of Selwyn granularity as a function'
           MESSAGE(2) = 'of optical density:'
           MESSAGE(3) = 'Theoretically it should be 0.5, but ' // &
             'empirically 0.6 is found.'
           MESSAGE(4) = '(0.61, for Ilford Industrial G film)'
           Call IO_INPR (.True., 0.001, 2.0, .True., &
             'GRANULARITY POWER EXPONENT', 2, MESSAGE, 1, &
             'Enter value within given range', power, status)
 
!        Area of sample in square microns
           MESSAGE(1) = 'Enter area of digitilisation aperature ' // &
             'in square microns'
           MESSAGE(2) = 'e.g. enter 2500 for a 50*50 micron aperature'
           Call IO_INPR (.True., 1.0, 1.0e6, .True., 'APERATURE AREA', 2, &
             MESSAGE, 1, 'Enter value within given range', area, status)
 
        End If
 
!     Enter correction constant for film
        Call IO_INPR (.True., 0.001, 0.1, .True., 'FILM CORRECTION CONSTANT', &
          1, 'Enter correction constant (0.06 Kodak no-screen C,'// &
          ' 0.055 Kodirex, 0.07 CEA)', 1, 'Enter value within given range', &
          correction_constant, status)
 
!     Perform error progation BEFORE changes to data values
        If (variances_exist) Then
 
           scaled_correction = correction_constant / value_od1
           Do y = ystrelm, yendelm
 
              Do x = xstrelm, xendelm
 
                 If (DATA(x, y) .Lt. lower_data .Or. DATA(x, y) .Gt. &
                   saturation) Then
 
!                 Set variance very large (but not too large to
!                 avoid floating overflow problems in later operations
                    VARIANCES(x, y) = 1.0e10
 
                 Else
 
!                 Calculate variance based on Selwyn granularity
                    optical_density = DATA(x, y) / value_od1
                    VARIANCES(x, y) = selwyn_granularity**2 * value_od1**2 * &
                      optical_density**(2.0 * power) / (2.0 * area)
 
!                 Increase variance to account for linearisation
                    VARIANCES(x, y) = (1.0 + 2.0 * scaled_correction * DATA(x, &
                      y))**2 * VARIANCES(x, y)
 
                 End If
 
              End Do
 
           End Do
 
        End If
 
!     Correct data values
        Do y = ystrelm, yendelm
 
           Do x = xstrelm, xendelm
              optical_density = DATA(x, y) / value_od1
              DATA(x, y) = (optical_density + correction_constant * &
                optical_density**2) * value_od1
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_LINEARISE
!********1*********2*********3*********4*********5*********6*********7*********8
