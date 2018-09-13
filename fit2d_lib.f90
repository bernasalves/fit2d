! Module of Interfaces produced automatically by program interfaces
 
Module FIT2D_LIB
 
Interface
 
     Subroutine F2D_1D_SORT (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_1DCONSTRAINTS (max_parameters, num_parameters, PARNAMES, &
       CONSTRAINTS, PARAMS, status)
     Use IO_LIB
     Use GS_LIB
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: num_parameters
     Character(Len = *), Intent(IN) :: PARNAMES(max_parameters)
!  Import/Export:
     Logical, Intent(INOUT) :: CONSTRAINTS(max_parameters)
     Real, Intent(INOUT) :: PARAMS(max_parameters)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_1DFEATURE (pixel_size, feature, max_parameters, PARAMS, &
       PARAM_INFO, num_parameters, maxdat, strelm, endelm, poly_order, &
       min_poly, max_poly, AXIS, DATA, status)
     Use IO_LIB
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: pixel_size
     Integer, Intent(IN) :: feature
     Integer, Intent(IN) :: max_parameters
     Real, Intent(IN) :: PARAMS(max_parameters)
     Integer, Intent(IN) :: PARAM_INFO(max_parameters)
     Integer, Intent(IN) :: num_parameters
     Integer, Intent(IN) :: maxdat
     Integer, Intent(IN) :: strelm
     Integer, Intent(IN) :: endelm
     Integer, Intent(IN) :: poly_order
     Real, Intent(IN) :: min_poly
     Real, Intent(IN) :: max_poly
     Real, Intent(IN) :: AXIS(maxdat)
!  Import/Export:
     Real, Intent(INOUT) :: DATA(maxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_1DINITHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_1DINITIALISE (experiment, xmaxdat, ymaxdat, numdat, AXIS, &
       DATA, title, xlabel, ylabel, max_parameters, max_results, strelm, &
       endelm, y_value, params_exist, MASK, poly_order, min_poly, max_poly, &
       num_features, num_parameters, PARAMS, PARAMS_INFO, CONSTRAINTS, &
       PARNAMES, SCALE_FACTORS, num_results, RESNAMES, memory_defined, &
       mnumdat, mstrelm, mendelm, mtitle, mxlabel, mylabel, MAXIS, RESIDUALS, &
       status)
     Use IO_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: numdat
     Real, Intent(IN) :: AXIS(xmaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: max_results
!  Import/Export:
     Integer, Intent(INOUT) :: strelm
     Integer, Intent(INOUT) :: endelm
     Integer, Intent(INOUT) :: y_value
     Logical, Intent(INOUT) :: params_exist
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
!  Export:
     Integer, Intent(OUT) :: poly_order
     Real, Intent(OUT) :: min_poly
     Real, Intent(OUT) :: max_poly
     Integer, Intent(OUT) :: num_features
     Integer, Intent(OUT) :: num_parameters
     Real, Intent(OUT) :: PARAMS(max_parameters)
     Integer, Intent(OUT) :: PARAMS_INFO(max_parameters)
     Logical, Intent(OUT) :: CONSTRAINTS(max_parameters)
     Character(Len = *), Intent(OUT) :: PARNAMES(max_parameters)
     Real, Intent(OUT) :: SCALE_FACTORS(max_parameters)
     Integer, Intent(OUT) :: num_results
     Character(Len = *), Intent(OUT) :: RESNAMES(max_results)
     Logical, Intent(OUT) :: memory_defined
     Integer, Intent(OUT) :: mnumdat
     Integer, Intent(OUT) :: mstrelm
     Integer, Intent(OUT) :: mendelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Real, Intent(OUT) :: MAXIS(xmaxdat)
     Real, Intent(OUT) :: RESIDUALS(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_1DINTERPOLATE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, XAXIS, YAXIS, DATA, status)
     Use IO_LIB
     Use MA_LIB
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Real, Intent(INOUT) :: XAXIS(xmaxdat)
     Real, Intent(INOUT) :: YAXIS(xmaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat,ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_1DMASK (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, &
       X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, MASK, status)
     Use IO_LIB
     Use MA_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_1DMASKHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_1DMASKREGION (xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, &
       YAXIS, DATA, xstrelm, ystrelm, xendelm, yendelm, mask_data, title, &
       xlabel, ylabel, zlabel, MASK, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: mask_data
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_1DTRANSFORMS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, DATA, status)
     Use IO_LIB
     Use GS_LIB
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Real, Intent(INOUT) :: XAXIS(xmaxdat)
     Real, Intent(INOUT) :: YAXIS(ymaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_2DDISTORTION (xmax_peaks, ymax_peaks, xnumpeaks, &
       ynumpeaks, X_PEAKS, Y_PEAKS, grid_spacing, x_pixel_size, y_pixel_size, &
       X_DISTORTION, Y_DISTORTION, status)
     Use IO_LIB
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: xnumpeaks
     Integer, Intent(IN) :: ynumpeaks
     Real, Intent(IN) :: X_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: grid_spacing
!  Import/Export:
     Real, Intent(INOUT) :: x_pixel_size
     Real, Intent(INOUT) :: y_pixel_size
     Real, Intent(INOUT) :: X_DISTORTION(xmax_peaks, ymax_peaks)
     Real, Intent(INOUT) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_2THETATORINGS (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, title, xlabel, ylabel, zlabel, XAXIS, YAXIS, DATA, &
       experiment, apply_polarisation, memory_defined, mtitle, &
       mxlabel, mylabel, mzlabel, mxnumdat, mynumdat, mxstrelm, mystrelm, &
       mxendelm, myendelm, MXAXIS, MYAXIS, MDATA, mx_pixel_size, &
       my_pixel_size, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Logical, Intent(INOUT) :: apply_polarisation
!  Export:
     Logical, Intent(OUT) :: memory_defined
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_3DSTYLE (normal_updating, xmaxdat, ymaxdat, DATA_VALUES, &
       X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
       ylabel, zlabel, status)
     Implicit None
     Include 'gs.inc'
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: normal_updating
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA_VALUES(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_3DSURFACE (print_type, xmaxdat, ymaxdat, DATA_VALUES, &
       X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
       ylabel, zlabel, status)
     Implicit None
     Include 'gs.inc'
     Include 'st_symbols.inc'
!  Import:
     Character(Len = *), Intent(INOUT) :: print_type
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA_VALUES(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ADDPOWDERRING (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, experiment, angle_cone, intensity, sigma, DATA, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Real, Intent(IN) :: angle_cone
     Real, Intent(IN) :: intensity
     Real, Intent(IN) :: sigma
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ARCSLICE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, xnumdat, ynumdat, X_AXIS, Y_AXIS, DATA, VARIANCES, title, &
       xlabel, ylabel, zlabel, variances_exist, x_pixel_size, y_pixel_size, &
       mxnumdat, mynumdat, MX_AXIS, MY_AXIS, MDATA, MVARIANCES, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       memory_defined, mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MX_AXIS(xmaxdat)
     Real, Intent(OUT) :: MY_AXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat*ymaxdat)
     Real, Intent(OUT) :: MVARIANCES(xmaxdat*ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Logical, Intent(OUT) :: memory_defined
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ARRAYS (xmaxdat, ymaxdat, &
       memory_exist, variances_exist, shared_memory, memory_id, &
       mask_exist, &
       pDATA, pMASK, pXAXIS, pYAXIS, pVARIANCES, pMDATA, pMXAXIS, pMYAXIS, &
       pMVARIANCES, results, status)
     Use IO_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Logical, Intent(IN) :: memory_exist
     Logical, Intent(IN) :: variances_exist
     Logical, Intent(IN) :: shared_memory
     Integer, Intent(IN) :: memory_id
!  Import/Export:
!  Export:
     Logical, Intent(OUT) :: mask_exist
     Integer, Intent(OUT) :: pDATA
     Integer, Intent(OUT) :: pMASK
     Integer, Intent(OUT) :: pXAXIS
     Integer, Intent(OUT) :: pYAXIS
     Integer, Intent(OUT) :: pVARIANCES
     Integer, Intent(OUT) :: pMDATA
     Integer, Intent(OUT) :: pMXAXIS
     Integer, Intent(OUT) :: pMYAXIS
     Integer, Intent(OUT) :: pMVARIANCES
     Type(RESULT_VECTORS), Intent(OUT) :: results
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ASPECTRATIO (status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_AUTOCORRELATION (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, XAXIS, YAXIS, DATA, memory_defined, mxnumdat, &
       mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, MXAXIS, MYAXIS, &
       MDATA, mtitle, mxlabel, mylabel, mzlabel, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
!  Export:
     Logical, Intent(OUT) :: memory_defined
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: mystrelm
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: myendelm
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_AVERAGE (input_options, average, data_defined, &
       memory_exist, &
       memory_defined, variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, XAXIS, YAXIS, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
       x_pixel_size, y_pixel_size, xstrelm, ystrelm, xendelm, yendelm, &
       experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
     Logical, Intent(IN) :: average
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Real, Intent(INOUT) :: XAXIS(xmaxdat)
     Real, Intent(INOUT) :: YAXIS(ymaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Real, Intent(INOUT) :: x_pixel_size
     Real, Intent(INOUT) :: y_pixel_size
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_AXES (maxdat, numdat, offset, step, AXIS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: maxdat
     Integer, Intent(IN) :: numdat
     Real, Intent(IN) :: offset
     Real, Intent(IN) :: step
!  Import/Export:
     Real, Intent(INOUT) :: AXIS(maxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_AXESSCALES (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, XAXIS, YAXIS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Real, Intent(INOUT) :: XAXIS(xmaxdat)
     Real, Intent(INOUT) :: YAXIS(ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_BANNER (fit2d_version, gui, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Character(Len = *), Intent(IN) :: fit2d_version
     Logical, Intent(IN) :: gui
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_BANNERHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_BEAMCENTRE (gui, mask_data, xmaxdat, ymaxdat, DATA, MASK, &
       X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
       ylabel, zlabel, full_info, method_used, &
       experiment, radius1, radius2, angle, radial_error, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: full_info
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: method_used
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(OUT) :: radius1
     Real, Intent(OUT) :: radius2
     Real, Intent(OUT) :: angle
     Real, Intent(OUT) :: radial_error
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_BEAMCENTREHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_BEAMGAUSSIAN (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, &
       xendelm, yendelm, x_pixel, y_pixel, mode, retstat, x_beam, y_beam, &
       status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: x_pixel
     Integer, Intent(IN) :: y_pixel
     Integer, Intent(IN) :: mode
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: retstat
     Real, Intent(OUT) :: x_beam
     Real, Intent(OUT) :: y_beam
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_BLOCKCOPY (gui, variances_exist, xmaxdat, ymaxdat, &
       xnumdat, ynumdat, DATA, VARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'f2d_internal.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Logical, Intent(IN) :: variances_exist
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_BLUR (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, VARIANCES, MDATA, MVARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
!  Export:
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_BRAGG (status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CADD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CAKE (gui, xmaxdat, ymaxdat, data_defined, title, xlabel, &
       ylabel, zlabel, variances_exist, xnumdat, ynumdat, xstrelm, ystrelm, &
       xendelm, yendelm, experiment, lorentz_geometry, data_type, &
       memory_defined, mask_data, &
       mask_memory, mtitle, mxlabel, mylabel, mzlabel, mxnumdat, mynumdat, &
       mxstrelm, mystrelm, mxendelm, myendelm, mx_pixel_size, my_pixel_size, &
       memory_data_type, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Logical, Intent(INOUT) :: variances_exist
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: lorentz_geometry
     Integer, Intent(INOUT) :: data_type
!  Export:
     Logical, Intent(OUT) :: memory_defined
     Logical, Intent(OUT) :: mask_data
     Logical, Intent(OUT) :: mask_memory
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
     Integer, Intent(INOUT) :: memory_data_type
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_CAKEHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_CAL_CAKE (title, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MASK, X_AXIS, Y_AXIS, experiment, &
       lorentz_geometry, start_azimuth, &
       end_azimuth, inner_limit, outer_limit, &
       memory_defined, mxnumdat, mynumdat, MXAXIS, MYAXIS, MDATA, mxstrelm, &
       mystrelm, mxendelm, myendelm, az_pixel_size, rad_pixel_size, mtitle, &
       mxlabel, mylabel, retstat, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Character(Len = *), Intent(IN) :: title
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(INOUT) :: start_azimuth
     Real, Intent(INOUT) :: end_azimuth
     Real, Intent(INOUT) :: inner_limit
     Real, Intent(INOUT) :: outer_limit
     Integer, Intent(INOUT) :: lorentz_geometry
!  Export:
     Logical, Intent(OUT) :: memory_defined
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Real, Intent(OUT) :: az_pixel_size
     Real, Intent(OUT) :: rad_pixel_size
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Integer, Intent(OUT) :: retstat
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CAL_DISTORTION (x_minimum, y_minimum, x_maximum, &
       y_maximum, x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
       y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, xmaxdat, ymaxdat, &
       XAXIS, YAXIS, xstrelm, ystrelm, xendelm, yendelm, MXAXIS, MYAXIS, &
       MDATA, mxstrelm, mystrelm, mxendelm, myendelm, mtitle, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: x_minimum
     Real, Intent(IN) :: y_minimum
     Real, Intent(IN) :: x_maximum
     Real, Intent(IN) :: y_maximum
     Integer, Intent(IN) :: x_xnumknots
     Integer, Intent(IN) :: x_ynumknots
     Real, Intent(IN) :: X_LAMBDA(x_xnumknots)
     Real, Intent(IN) :: X_MU(x_ynumknots)
     Real, Intent(IN) :: X_COEFFS((x_xnumknots - 4) * (x_ynumknots - 4))
     Integer, Intent(IN) :: y_xnumknots
     Integer, Intent(IN) :: y_ynumknots
     Real, Intent(IN) :: Y_LAMBDA(y_xnumknots)
     Real, Intent(IN) :: Y_MU(y_ynumknots)
     Real, Intent(IN) :: Y_COEFFS((y_xnumknots - 4) * (y_ynumknots - 4))
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: ystrelm
!  Import/Export:
     Real, Intent(INOUT) :: MXAXIS(xmaxdat)
     Real, Intent(INOUT) :: MYAXIS(ymaxdat)
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat)
     Integer, Intent(INOUT) :: mxendelm
     Integer, Intent(INOUT) :: mxstrelm
     Integer, Intent(INOUT) :: myendelm
     Integer, Intent(INOUT) :: mystrelm
     Character(Len = *), Intent(INOUT) :: mtitle
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CAL_GISAXS (mask_data, scan_type, experiment, &
       geometrical_correction, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, MASK, X_REGION, Y_REGION, &
       method, var_exist, bin_size, max_bins, num_bins, retstat, AXIS, BINS, &
       BIN_VARS, NORMALISATION, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: scan_type
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: geometrical_correction
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_REGION(5)
     Real, Intent(IN) :: Y_REGION(5)
     Integer, Intent(IN) :: method
     Logical, Intent(IN) :: var_exist
     Real, Intent(IN) :: bin_size
     Integer, Intent(IN) :: max_bins
     Integer, Intent(IN) :: num_bins
!  Export:
     Integer, Intent(OUT) :: retstat
     Real, Intent(OUT) :: AXIS(max_bins)
     Real, Intent(OUT) :: BINS(max_bins)
     Real, Intent(OUT) :: BIN_VARS(max_bins)
     Real, Intent(OUT) :: NORMALISATION(num_bins)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CAL_ID06CAKE (title, xmaxdat, ymaxdat, &
       xnumdat, ynumdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MASK, X_AXIS, Y_AXIS, &
       azimuth_start, azimuth_end, experiment, lorentz_geometry, &
       start_azimuth, end_azimuth, inner_limit, outer_limit, &
       memory_defined, mxnumdat, mynumdat, MXAXIS, MYAXIS, MDATA, mxstrelm, &
       mystrelm, mxendelm, myendelm, az_pixel_size, rad_pixel_size, mtitle, &
       mxlabel, mylabel, retstat, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Character(Len = *), Intent(IN) :: title
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: azimuth_start
     Real, Intent(IN) :: azimuth_end
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(INOUT) :: start_azimuth
     Real, Intent(INOUT) :: end_azimuth
     Real, Intent(INOUT) :: inner_limit
     Real, Intent(INOUT) :: outer_limit
     Integer, Intent(INOUT) :: lorentz_geometry
!  Export:
     Logical, Intent(OUT) :: memory_defined
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Real, Intent(OUT) :: az_pixel_size
     Real, Intent(OUT) :: rad_pixel_size
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Integer, Intent(OUT) :: retstat
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CAL_INTEGRATE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MASK, conserve_intensity, scan_type, maximum_d, &
       experiment, inner_limit, outer_limit, &
       num_2theta, lorentz_geometry, max_radial, xmax_work, &
       rad_pixel_size, inner_2theta, WORK, R_THETA, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat,ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Logical, Intent(IN) :: conserve_intensity
     Integer, Intent(IN) :: scan_type
     Real, Intent(IN) :: maximum_d
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(IN) :: inner_limit
     Real, Intent(IN) :: outer_limit
     Integer, Intent(IN) :: num_2theta
     Integer, Intent(IN) :: lorentz_geometry
     Integer, Intent(IN) :: max_radial
     Integer, Intent(IN) :: xmax_work
!  Export:
     Real, Intent(OUT) :: rad_pixel_size
     Real, Intent(OUT) :: inner_2theta
     Real, Intent(OUT) :: WORK(xmax_work)
     Real, Intent(OUT) :: R_THETA(max_radial)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CAL_LUT_CAKE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, MASK, conserve_intensity, scan_type, maximum_d, &
       experiment, start_azimuth, end_azimuth, inner_limit, outer_limit, &
       num_2theta, num_azimuthal, lorentz_geometry, correct_parallax, &
       max_radial, max_azimuthal, xmax_work, &
       ymax_work, az_pixel_size, rad_pixel_size, inner_2theta, WORK, R_THETA, &
       status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat,ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Logical, Intent(IN) :: conserve_intensity
     Integer, Intent(IN) :: scan_type
     Real, Intent(IN) :: maximum_d
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(IN) :: start_azimuth
     Real, Intent(IN) :: end_azimuth
     Real, Intent(IN) :: inner_limit
     Real, Intent(IN) :: outer_limit
     Integer, Intent(IN) :: num_2theta
     Integer, Intent(IN) :: num_azimuthal
     Integer, Intent(IN) :: lorentz_geometry
     Logical, Intent(IN) :: correct_parallax
     Integer, Intent(IN) :: max_radial
     Integer, Intent(IN) :: max_azimuthal
     Integer, Intent(IN) :: xmax_work
     Integer, Intent(IN) :: ymax_work
!  Export:
     Real, Intent(OUT) :: az_pixel_size
     Real, Intent(OUT) :: rad_pixel_size
     Real, Intent(OUT) :: inner_2theta
     Real, Intent(OUT) :: WORK(xmax_work, ymax_work)
     Real, Intent(OUT) :: R_THETA(max_radial, max_azimuthal)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CAL_PARALLAX (experiment, PARALLAX_CORRECTION, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: PARALLAX_CORRECTION(0: 90)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CAL_PRESSURE (k_t, k_t_deriv, v_0divv, pressure, &
       status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Double Precision, Intent(IN) :: k_t
     Double Precision, Intent(IN) :: k_t_deriv
     Double Precision, Intent(IN) :: v_0divv
!  Import/Export:
!  Export:
     Double Precision, Intent(OUT) :: pressure
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CAL_PROJECTION (distance, x_pixel_size, y_pixel_size, &
       x_centre, y_centre, x_up, y_up, longitude, latitude, x_pc, y_pc, &
       status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: distance
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
     Real, Intent(IN) :: x_centre
     Real, Intent(IN) :: y_centre
     Real, Intent(IN) :: x_up
     Real, Intent(IN) :: y_up
     Real, Intent(IN) :: longitude
     Real, Intent(IN) :: latitude
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: x_pc
     Real, Intent(OUT) :: y_pc
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CAL_VOLUMECHANGE (k_t, k_t_deriv, pressure, v_0divv, &
       status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Double Precision, Intent(IN) :: k_t
     Double Precision, Intent(IN) :: k_t_deriv
     Double Precision, Intent(IN) :: pressure
!  Import/Export:
!  Export:
     Double Precision, Intent(OUT) :: v_0divv
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CAL2_CAKE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, MASK, conserve_intensity, scan_type, maximum_d, &
       experiment, start_azimuth, end_azimuth, inner_limit, outer_limit, &
       num_2theta, num_azimuthal, lorentz_geometry, correct_parallax, &
       max_radial, max_azimuthal, xmax_work, &
       ymax_work, az_pixel_size, rad_pixel_size, inner_2theta, WORK, R_THETA, &
       status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat,ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Logical, Intent(IN) :: conserve_intensity
     Integer, Intent(IN) :: scan_type
     Real, Intent(IN) :: maximum_d
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(IN) :: start_azimuth
     Real, Intent(IN) :: end_azimuth
     Real, Intent(IN) :: inner_limit
     Real, Intent(IN) :: outer_limit
     Integer, Intent(IN) :: num_2theta
     Integer, Intent(IN) :: num_azimuthal
     Integer, Intent(IN) :: lorentz_geometry
     Logical, Intent(IN) :: correct_parallax
     Integer, Intent(IN) :: max_radial
     Integer, Intent(IN) :: max_azimuthal
     Integer, Intent(IN) :: xmax_work
     Integer, Intent(IN) :: ymax_work
!  Export:
     Real, Intent(OUT) :: az_pixel_size
     Real, Intent(OUT) :: rad_pixel_size
     Real, Intent(OUT) :: inner_2theta
     Real, Intent(OUT) :: WORK(xmax_work, ymax_work)
     Real, Intent(OUT) :: R_THETA(max_radial, max_azimuthal)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CAL2_DISTORTION (mxstrelm, mystrelm, mxendelm, &
       myendelm, x_low, y_low, x_up, y_up, xnumknots, ynumknots, LAMBDA, MU, &
       COEFFS, mxmaxdat, mymaxdat, XAXIS, YAXIS, MDATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: mxstrelm
     Integer, Intent(IN) :: mystrelm
     Integer, Intent(IN) :: mxendelm
     Integer, Intent(IN) :: myendelm
     Integer, Intent(IN) :: x_low
     Integer, Intent(IN) :: y_low
     Integer, Intent(IN) :: x_up
     Integer, Intent(IN) :: y_up
     Integer, Intent(IN) :: xnumknots
     Integer, Intent(IN) :: ynumknots
     Real, Intent(IN) :: LAMBDA(xnumknots)
     Real, Intent(IN) :: MU(ynumknots)
     Real, Intent(IN) :: COEFFS((xnumknots - 4) * (ynumknots - 4))
     Integer, Intent(IN) :: mxmaxdat
     Integer, Intent(IN) :: mymaxdat
     Real, Intent(IN) :: XAXIS(mxmaxdat)
     Real, Intent(IN) :: YAXIS(mymaxdat)
!  Export:
     Real, Intent(OUT) :: MDATA(mxmaxdat, mymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CAL2_ID06CAKE (xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, &
       yendelm, DATA, MASK, conserve_intensity, scan_type, maximum_d, &
       azimuth_start, azimuth_end, experiment, start_azimuth, end_azimuth, &
       inner_limit, outer_limit, &
       num_2theta, num_azimuthal, lorentz_geometry, correct_parallax, &
       max_radial, max_azimuthal, xmax_work, &
       ymax_work, az_pixel_size, rad_pixel_size, inner_2theta, WORK, R_THETA, &
       status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat,ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Logical, Intent(IN) :: conserve_intensity
     Integer, Intent(IN) :: scan_type
     Real, Intent(IN) :: maximum_d
     Real, Intent(IN) :: azimuth_start
     Real, Intent(IN) :: azimuth_end
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(IN) :: start_azimuth
     Real, Intent(IN) :: end_azimuth
     Real, Intent(IN) :: inner_limit
     Real, Intent(IN) :: outer_limit
     Integer, Intent(IN) :: num_2theta
     Integer, Intent(IN) :: num_azimuthal
     Integer, Intent(IN) :: lorentz_geometry
     Logical, Intent(IN) :: correct_parallax
     Integer, Intent(IN) :: max_radial
     Integer, Intent(IN) :: max_azimuthal
     Integer, Intent(IN) :: xmax_work
     Integer, Intent(IN) :: ymax_work
!  Export:
     Real, Intent(OUT) :: az_pixel_size
     Real, Intent(OUT) :: rad_pixel_size
     Real, Intent(OUT) :: inner_2theta
     Real, Intent(OUT) :: WORK(xmax_work, ymax_work)
     Real, Intent(OUT) :: R_THETA(max_radial, max_azimuthal)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CALABSORPTION ( radius_detect, width_detect, &
       absorb_detect, radius_protect, width_protect, absorb_protect, &
       source_distance, x_pixel_size, max_pixels, PROFILE, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: radius_detect
     Real, Intent(IN) :: width_detect
     Real, Intent(IN) :: absorb_detect
     Real, Intent(IN) :: radius_protect
     Real, Intent(IN) :: absorb_protect
     Real, Intent(IN) :: width_protect
     Real, Intent(IN) :: source_distance
     Real, Intent(IN) :: x_pixel_size
     Integer, Intent(IN) :: max_pixels
!  Import/Export:
!  Export:
     Real, Intent(INOUT) :: PROFILE(0: max_pixels)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CALCULATOR (status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PUSHSTACK (value, max_stack, stack_pointer, STACK, status)
     Implicit None
!  Import:
     Complex :: value
     Integer :: max_stack
!  Import/Export:
     Integer :: stack_pointer
     Complex :: STACK(max_stack)
!  Status:
     Integer :: status
End Subroutine
 
     Subroutine F2D_CALFLATFIELD (max_pixels, PROFILE, x_centre, y_centre, &
       x_pixel_size, y_pixel_size, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: max_pixels
     Real, Intent(IN) :: PROFILE(max_pixels)
     Real, Intent(IN) :: x_centre
     Real, Intent(IN) :: y_centre
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CALIBRANT (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, XAXIS, YAXIS, DATA, MASK, title, xlabel, ylabel, &
       zlabel, experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fitrings.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CALIBRANT_USER (max_angles, retstat, num_cali_rings, &
       D_SPACINGS, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: max_angles
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: retstat
     Integer, Intent(OUT) :: num_cali_rings
     Real, Intent(OUT) :: D_SPACINGS(max_angles)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CALIBRATE2DGRID (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, &
       num_display, xmax_peaks, ymax_peaks, grid_spacing, x_pixel_size, &
       y_pixel_size, retstat, num_peaks, xnumpeaks, ynumpeaks, X_2DPEAKS, &
       Y_2DPEAKS, X_DISTORTION, Y_DISTORTION, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Integer, Intent(IN) :: num_display
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
!  Import/Export:
     Real, Intent(INOUT) :: grid_spacing
     Real, Intent(INOUT) :: x_pixel_size
     Real, Intent(INOUT) :: y_pixel_size
!  Export:
     Integer, Intent(OUT) :: retstat
     Integer, Intent(OUT) :: num_peaks
     Integer, Intent(OUT) :: xnumpeaks
     Integer, Intent(OUT) :: ynumpeaks
     Real, Intent(OUT) :: X_2DPEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(OUT) :: Y_2DPEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(OUT) :: X_DISTORTION(xmax_peaks, ymax_peaks)
     Real, Intent(OUT) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CALIBRATION (memory_exist, variances_exist, input_file, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, YAXIS, DATA, VARIANCES, &
       xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
       x_pixel_size, y_pixel_size, mxnumdat, mynumdat, MXAXIS, MYAXIS, MDATA, &
       MVARIANCES, mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, &
       mylabel, mzlabel, memory_defined, mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: memory_exist
     Logical, Intent(IN) :: variances_exist
     Character(Len = *), Intent(IN) :: input_file
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: ystrelm
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
!  Import/Export:
     Integer, Intent(INOUT) :: mxnumdat
     Integer, Intent(INOUT) :: mynumdat
     Real, Intent(INOUT) :: MXAXIS(xmaxdat)
     Real, Intent(INOUT) :: MYAXIS(ymaxdat)
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(INOUT) :: mxendelm
     Integer, Intent(INOUT) :: mxstrelm
     Integer, Intent(INOUT) :: myendelm
     Integer, Intent(INOUT) :: mystrelm
     Character(Len = *), Intent(INOUT) :: mtitle
     Character(Len = *), Intent(INOUT) :: mxlabel
     Character(Len = *), Intent(INOUT) :: mylabel
     Character(Len = *), Intent(INOUT) :: mzlabel
     Logical, Intent(INOUT) :: memory_defined
     Real, Intent(INOUT) :: mx_pixel_size
     Real, Intent(INOUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CALPROFILE (lower_range, upper_range, max_order, order, &
       POLYNOMIAL, distance, x_pixel_size, absorption, max_pixels, PROFILE, &
       status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: lower_range
     Real, Intent(IN) :: upper_range
     Integer, Intent(IN) :: max_order
     Integer, Intent(IN) :: order
     Real, Intent(IN) :: POLYNOMIAL(max_order + 1)
     Real, Intent(IN) :: distance
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: absorption
     Integer, Intent(IN) :: max_pixels
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: PROFILE(0: max_pixels)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CALTEMPLATE (xmaxtemplate, ymaxtemplate, xnumtemplate, &
       ynumtemplate, sigma, sub_pixels, TEMPLATE, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxtemplate
     Integer, Intent(IN) :: ymaxtemplate
     Integer, Intent(IN) :: xnumtemplate
     Integer, Intent(IN) :: ynumtemplate
     Real, Intent(IN) :: sigma
     Integer, Intent(IN) :: sub_pixels
!  Import/Export:
     Real, Intent(INOUT) :: TEMPLATE(xmaxtemplate, ymaxtemplate)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CDIV (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, VARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CENTREOFMASS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, retstat, x_com, y_com, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: retstat
     Real, Intent(OUT) :: x_com
     Real, Intent(OUT) :: y_com
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_CHANGES (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_CHEBYSHEV (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, X_AXIS, Y_AXIS, MASK, FIT, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: FIT(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CLICK (data_type, xmaxdat, ymaxdat, &
       xstrelm, ystrelm, xendelm, yendelm, X_AXIS, Y_AXIS, DATA, &
       title, xlabel, ylabel, zlabel, x_coordinate, y_coordinate, experiment, &
       update_image, update_menu, status)
     Use IO_LIB
     Use LG_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: data_type
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Real, Intent(IN) :: x_coordinate
     Real, Intent(IN) :: y_coordinate
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
!  Export:
     Logical, Intent(OUT) :: update_image
     Logical, Intent(OUT) :: update_menu
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CLINEARISE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CMULT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, VARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_COLOURS (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
       variances_exist, xstrelm, ystrelm, xendelm, yendelm, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_COLOURSHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_COMPOSITE (input_options, &
       data_defined, memory_exist, memory_defined, &
       variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
       xlabel, ylabel, zlabel, experiment, xstrelm, ystrelm, &
       xendelm, yendelm, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CONCATENATION (status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'io_db.inc'
!  Import:
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_CONDITIONSHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_CONSTRAIN (max_parameters, num_parameters, PARNAMES, &
       CONSTRAINTS, PARAMS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: num_parameters
     Character(Len = *), Intent(IN) :: PARNAMES(max_parameters)
!  Import/Export:
     Logical, Intent(INOUT) :: CONSTRAINTS(max_parameters)
     Real, Intent(INOUT) :: PARAMS(max_parameters)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CONTINUE (print_type, xmaxdat, ymaxdat, X_AXIS, Y_AXIS, &
       DATA, VARIANCES, title, xlabel, ylabel, zlabel, variances_exist, &
       xstrelm, ystrelm, xendelm, yendelm, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Character(Len = *), Intent(IN) :: print_type
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CORR_FAST (gui, x_cor_size, y_cor_size, xmax_lut, &
       ymax_lut, xnum_lut, ynum_lut, X_SD, Y_SD, INT_REBINNED, xmaxdat, &
       ymaxdat, XAXIS, YAXIS, DATA, xstrelm, ystrelm, xendelm, yendelm, &
       overload_value, MXAXIS, MYAXIS, MDATA, mxstrelm, mystrelm, mxendelm, &
       myendelm, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Real, Intent(IN) :: x_cor_size
     Real, Intent(IN) :: y_cor_size
     Integer, Intent(IN) :: xmax_lut
     Integer, Intent(IN) :: ymax_lut
     Integer, Intent(IN) :: xnum_lut
     Integer, Intent(IN) :: ynum_lut
     Byte, Intent(IN) :: X_SD(xmax_lut, ymax_lut)
     Byte, Intent(IN) :: Y_SD(xmax_lut, ymax_lut)
     Byte, Intent(IN) :: INT_REBINNED(9, xmax_lut, ymax_lut)
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: ystrelm
!  Import/Export:
     Real, Intent(INOUT) :: overload_value
     Real, Intent(INOUT) :: MXAXIS(xmaxdat)
     Real, Intent(INOUT) :: MYAXIS(ymaxdat)
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat)
     Integer, Intent(INOUT) :: mxstrelm
     Integer, Intent(INOUT) :: mystrelm
     Integer, Intent(INOUT) :: mxendelm
     Integer, Intent(INOUT) :: myendelm
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CORR_SPATIAL (fitpack, gui, x_minimum, y_minimum, &
       x_maximum, y_maximum, x_cor_size, y_cor_size, x_xnumknots, x_ynumknots, &
       X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, &
       Y_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, xstrelm, ystrelm, &
       xendelm, yendelm, y_rows, overload_value, MXAXIS, MYAXIS, MDATA, &
       mxstrelm, mystrelm, mxendelm, myendelm, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: fitpack
     Logical, Intent(IN) :: gui
     Real, Intent(IN) :: x_minimum
     Real, Intent(IN) :: y_minimum
     Real, Intent(IN) :: x_maximum
     Real, Intent(IN) :: y_maximum
     Real, Intent(IN) :: x_cor_size
     Real, Intent(IN) :: y_cor_size
     Integer, Intent(IN) :: x_xnumknots
     Integer, Intent(IN) :: x_ynumknots
     Real, Intent(IN) :: X_LAMBDA(x_xnumknots)
     Real, Intent(IN) :: X_MU(x_ynumknots)
     Real, Intent(IN) :: X_COEFFS((x_xnumknots - 4) * (x_ynumknots - 4))
     Integer, Intent(IN) :: y_xnumknots
     Integer, Intent(IN) :: y_ynumknots
     Real, Intent(IN) :: Y_LAMBDA(y_xnumknots)
     Real, Intent(IN) :: Y_MU(y_ynumknots)
     Real, Intent(IN) :: Y_COEFFS((y_xnumknots - 4) * (y_ynumknots - 4))
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: ystrelm
!  Import/Export:
     Integer, Intent(INOUT) :: y_rows
     Real, Intent(INOUT) :: overload_value
     Real, Intent(INOUT) :: MXAXIS(xmaxdat)
     Real, Intent(INOUT) :: MYAXIS(ymaxdat)
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat)
     Integer, Intent(INOUT) :: mxstrelm
     Integer, Intent(INOUT) :: mystrelm
     Integer, Intent(INOUT) :: mxendelm
     Integer, Intent(INOUT) :: myendelm
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CORR2_RADIAL (x_low, y_low, x_up, y_up, x_cor_size, &
       y_cor_size, x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
       y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, xmaxdat, ymaxdat, &
       xstrelm, ystrelm, xendelm, yendelm, XAXIS, YAXIS, DATA, MASK, &
       overload_value, variances_exist, experiment, lorentz_geometry, &
       angular_scan, radial_pixel_size, max_radial, num_radial, maxwork, &
       y_rows, X_COORDINATES, Y_COORDINATES, X_DISTORTION, Y_DISTORTION, &
       RAD_AXIS, PROFILE, PROVARS, NUMPIXELS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: x_low
     Integer, Intent(IN) :: y_low
     Integer, Intent(IN) :: x_up
     Integer, Intent(IN) :: y_up
     Real, Intent(IN) :: x_cor_size
     Real, Intent(IN) :: y_cor_size
     Integer, Intent(IN) :: x_xnumknots
     Integer, Intent(IN) :: x_ynumknots
     Real, Intent(IN) :: X_LAMBDA(x_xnumknots)
     Real, Intent(IN) :: X_MU(x_ynumknots)
     Real, Intent(IN) :: X_COEFFS((x_xnumknots - 4) * (x_ynumknots - 4))
     Integer, Intent(IN) :: y_xnumknots
     Integer, Intent(IN) :: y_ynumknots
     Real, Intent(IN) :: Y_LAMBDA(y_xnumknots)
     Real, Intent(IN) :: Y_MU(y_ynumknots)
     Real, Intent(IN) :: Y_COEFFS((y_xnumknots - 4) * (y_ynumknots - 4))
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: overload_value
     Logical, Intent(IN) :: variances_exist
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Integer, Intent(IN) :: lorentz_geometry
     Logical, Intent(IN) :: angular_scan
     Real, Intent(IN) :: radial_pixel_size
     Integer, Intent(IN) :: max_radial
     Integer, Intent(IN) :: maxwork
     Integer, Intent(IN) :: y_rows
!  Export:
     Real, Intent(OUT) :: X_COORDINATES(maxwork)
     Real, Intent(OUT) :: Y_COORDINATES(y_rows)
     Real, Intent(OUT) :: X_DISTORTION(maxwork, y_rows)
     Real, Intent(OUT) :: Y_DISTORTION(maxwork, y_rows)
     Integer, Intent(OUT) :: num_radial
     Real, Intent(OUT) :: RAD_AXIS(max_radial)
     Real, Intent(OUT) :: PROFILE(max_radial)
     Real, Intent(OUT) :: PROVARS(max_radial)
     Real, Intent(OUT) :: NUMPIXELS(max_radial)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CORR5_SPATIAL (xstrelm, ystrelm, xendelm, yendelm, &
       overload_value, x_low, y_low, x_up, y_up, x_xnumknots, x_ynumknots, &
       X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, &
       Y_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, maxxwork, y_rows, &
       X_COORDINATES, Y_COORDINATES, X_DISTORTION, Y_DISTORTION, MDATA, &
       status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: overload_value
     Integer, Intent(IN) :: x_low
     Integer, Intent(IN) :: y_low
     Integer, Intent(IN) :: x_up
     Integer, Intent(IN) :: y_up
     Integer, Intent(IN) :: x_xnumknots
     Integer, Intent(IN) :: x_ynumknots
     Real, Intent(IN) :: X_LAMBDA(x_xnumknots)
     Real, Intent(IN) :: X_MU(x_ynumknots)
     Real, Intent(IN) :: X_COEFFS((x_xnumknots - 4) * (x_ynumknots - 4))
     Integer, Intent(IN) :: y_xnumknots
     Integer, Intent(IN) :: y_ynumknots
     Real, Intent(IN) :: Y_LAMBDA(y_xnumknots)
     Real, Intent(IN) :: Y_MU(y_ynumknots)
     Real, Intent(IN) :: Y_COEFFS((y_xnumknots - 4) * (y_ynumknots - 4))
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: maxxwork
     Integer, Intent(IN) :: y_rows
!  Import/Export:
     Real, Intent(INOUT) :: X_COORDINATES(maxxwork)
     Real, Intent(INOUT) :: Y_COORDINATES(y_rows)
     Real, Intent(INOUT) :: X_DISTORTION(maxxwork, y_rows)
     Real, Intent(INOUT) :: Y_DISTORTION(maxxwork, y_rows)
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CORR6_SPATIAL (xstrelm, ystrelm, xendelm, yendelm, &
       overload_value, x_low, y_low, x_up, y_up, x_xnumknots, x_ynumknots, &
       X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, &
       Y_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, maxxwork, y_rows, &
       X_COORDINATES, Y_COORDINATES, X_DISTORTION, Y_DISTORTION, MDATA, &
       status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: overload_value
     Integer, Intent(IN) :: x_low
     Integer, Intent(IN) :: y_low
     Integer, Intent(IN) :: x_up
     Integer, Intent(IN) :: y_up
     Integer, Intent(IN) :: x_xnumknots
     Integer, Intent(IN) :: x_ynumknots
     Real, Intent(IN) :: X_LAMBDA(x_xnumknots)
     Real, Intent(IN) :: X_MU(x_ynumknots)
     Real, Intent(IN) :: X_COEFFS((x_xnumknots - 4) * (x_ynumknots - 4))
     Integer, Intent(IN) :: y_xnumknots
     Integer, Intent(IN) :: y_ynumknots
     Real, Intent(IN) :: Y_LAMBDA(y_xnumknots)
     Real, Intent(IN) :: Y_MU(y_ynumknots)
     Real, Intent(IN) :: Y_COEFFS((y_xnumknots - 4) * (y_ynumknots - 4))
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: maxxwork
     Integer, Intent(IN) :: y_rows
!  Import/Export:
     Real, Intent(INOUT) :: X_COORDINATES(maxxwork)
     Real, Intent(INOUT) :: Y_COORDINATES(y_rows)
     Real, Intent(INOUT) :: X_DISTORTION(maxxwork * y_rows)
     Real, Intent(INOUT) :: Y_DISTORTION(maxxwork * y_rows)
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CREATEDATA (variances_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, xstrelm, ystrelm, xendelm, yendelm, XAXIS, YAXIS, DATA, &
       VARIANCES, title, xlabel, ylabel, zlabel, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: variances_exist
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: xnumdat
     Integer, Intent(OUT) :: ynumdat
     Integer, Intent(OUT) :: xstrelm
     Integer, Intent(OUT) :: ystrelm
     Integer, Intent(OUT) :: xendelm
     Integer, Intent(OUT) :: yendelm
     Real, Intent(OUT) :: XAXIS(xmaxdat)
     Real, Intent(OUT) :: YAXIS(ymaxdat)
     Real, Intent(OUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(OUT) :: title
     Character(Len = *), Intent(OUT) :: xlabel
     Character(Len = *), Intent(OUT) :: ylabel
     Character(Len = *), Intent(OUT) :: zlabel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_CYCLICSHIFT (xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, &
       VARIANCES, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, title, &
       xlabel, ylabel, zlabel, variances_exist, retstat, memory_exist, MXAXIS, &
       MYAXIS, MDATA, MASK, MVARIANCES, mxnumdat, mynumdat, &
       mxstrelm, mystrelm, mxendelm, myendelm, mtitle, &
       mxlabel, mylabel, mzlabel, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
!  Export:
     Integer, Intent(OUT) :: retstat
     Logical, Intent(OUT) :: memory_exist
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: mystrelm
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: myendelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DECAY (xmaxdat, ymaxdat, ynumdat, xstrelm, ystrelm, &
       xendelm, yendelm, variances_exist, DATA, VARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DEF_EXPERIMENT (experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Export:
     Type(EXPERIMENTAL_DETAILS), Intent(OUT) :: experiment
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DEF_FS (input_options, ask_increment, variances_exist, &
       xmaxdat, ymaxdat, retstat, xnumdat, ynumdat, XAXIS, YAXIS, DATA, &
       VARIANCES, title, xlabel, ylabel, zlabel, xstrelm, ystrelm, &
       xendelm, yendelm, first_file, first_image, last_file, last_image, &
       start_value, end_value, prefix, variable_characters, num_characters, &
       postfix, extension, file_increment, experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Logical, Intent(INOUT) :: ask_increment
     Logical, Intent(INOUT) :: variances_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
!  Import/Export:
     Integer, Intent(INOUT) :: retstat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Real, Intent(INOUT) :: XAXIS(xmaxdat)
     Real, Intent(INOUT) :: YAXIS(ymaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Character(Len = *), Intent(OUT) :: first_file
     Integer, Intent(INOUT) :: first_image
     Character(Len = *), Intent(OUT) :: last_file
     Integer, Intent(INOUT) :: last_image
     Integer, Intent(OUT) :: start_value
     Integer, Intent(OUT) :: end_value
     Character(Len = *), Intent(OUT) :: prefix
     Logical, Intent(OUT) :: variable_characters
     Integer, Intent(OUT) :: num_characters
     Character(Len = *), Intent(OUT) :: postfix
     Character(Len = *), Intent(OUT) :: extension
     Integer, Intent(OUT) :: file_increment
     Type(EXPERIMENTAL_DETAILS), Intent(OUT) :: experiment
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DEF_GEOMETRY (experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DEF_INTERNALMEMORY (status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'f2d_internal.inc'
!  Import:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DESTROYPEAKS ( xmax_peaks, ymax_peaks, xnum_peaks, &
       ynum_peaks, X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: xnum_peaks
     Integer, Intent(IN) :: ynum_peaks
!  Import/Export:
     Real, Intent(INOUT) :: X_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(INOUT) :: Y_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(INOUT) :: X_DISTORTION(xmax_peaks, ymax_peaks)
     Real, Intent(INOUT) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DIFFRACTION (EXPERIMENT, a_star, a_theta, &
       a_phi, b_star, b_theta, b_phi, c_star, c_theta, c_phi, h_minimum, &
       h_maximum, k_minimum, k_maximum, l_minimum, l_maximum, max_coordinates, &
       num_coordinates, num_coord2, X_COORDINATES, Y_COORDINATES, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     TYPE(EXPERIMENTAL_DETAILS), Intent(IN) :: EXPERIMENT
     Real, Intent(IN) :: a_star
     Real, Intent(IN) :: a_theta
     Real, Intent(IN) :: a_phi
     Real, Intent(IN) :: b_star
     Real, Intent(IN) :: b_theta
     Real, Intent(IN) :: b_phi
     Real, Intent(IN) :: c_star
     Real, Intent(IN) :: c_theta
     Real, Intent(IN) :: c_phi
     Integer, Intent(IN) :: h_minimum
     Integer, Intent(IN) :: h_maximum
     Integer, Intent(IN) :: k_minimum
     Integer, Intent(IN) :: k_maximum
     Integer, Intent(IN) :: l_minimum
     Integer, Intent(IN) :: l_maximum
     Integer, Intent(IN) :: max_coordinates
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: num_coordinates
     Integer, Intent(OUT) :: num_coord2
     Real, Intent(OUT) :: X_COORDINATES(max_coordinates, 2)
     Real, Intent(OUT) :: Y_COORDINATES(max_coordinates, 2)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DIFPATTERN (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
       experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DISPLAY2DDISTORTION (xmax_peaks, ymax_peaks, xnum_peaks, &
       ynum_peaks, X_DISTORTION, Y_DISTORTION, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: xnum_peaks
     Integer, Intent(IN) :: ynum_peaks
     Real, Intent(IN) :: X_DISTORTION(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DISPLAYLIMITS (status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DISPLAY (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, MASK, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
       variances_exist, experiment, xstrelm, ystrelm, xendelm, &
       yendelm, mxnumdat, mynumdat, MX_AXIS, MY_AXIS, MDATA, MVARIANCES, &
       mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, &
       mzlabel, memory_defined, mx_pixel_size, my_pixel_size, print_type, &
       status)
     Use IO_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Character(Len = *), Intent(INOUT) :: print_type
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MX_AXIS(xmaxdat)
     Real, Intent(OUT) :: MY_AXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Logical, Intent(OUT) :: memory_defined
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_DISPLAYHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_DIST2MEM (xmax_peaks, ymax_peaks, xnum_peaks, ynum_peaks, &
       X_DISTORTION, Y_DISTORTION, xmaxdat, ymaxdat, mxnumdat, mynumdat, &
       MXAXIS, MYAXIS, MDATA, mxstrelm, mystrelm, mxendelm, myendelm, mtitle, &
       mxlabel, mylabel, mzlabel, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: xnum_peaks
     Integer, Intent(IN) :: ynum_peaks
     Real, Intent(IN) :: X_DISTORTION(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DISTANCE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, xnumdat, ynumdat, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, &
       zlabel, x_pixel_size, y_pixel_size, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DIVIDE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, MDATA, MVARIANCES, MASK, DATA, VARIANCES, &
       status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
     Real, Intent(IN) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: MVARIANCES(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DRAW_BANNER (fit2d_version, gui, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'gs_menu.inc'
     Include 'gs_database.inc'
!  Import:
     Character(Len = *), Intent(IN) :: fit2d_version
     Logical, Intent(IN) :: gui
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DRAW_CAKE (undraw, experiment, &
       start_azimuth, end_azimuth, inner_limit, outer_limit, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: undraw
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(IN) :: start_azimuth
     Real, Intent(IN) :: end_azimuth
     Real, Intent(IN) :: inner_limit
     Real, Intent(IN) :: outer_limit
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DRAW_CAKE_CMD (EXPERIMENT, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     TYPE(EXPERIMENTAL_DETAILS), Intent(IN) :: EXPERIMENT
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DRAW_ID06_CAKE (undraw, experiment, &
       start_azimuth, end_azimuth, inner_limit, outer_limit, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: undraw
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(IN) :: start_azimuth
     Real, Intent(IN) :: end_azimuth
     Real, Intent(IN) :: inner_limit
     Real, Intent(IN) :: outer_limit
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DRAW_ID06_LINES (experiment, max_angles, num_cali_rings, &
       D_SPACINGS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Integer, Intent(IN) :: max_angles
     Integer, Intent(IN) :: num_cali_rings
     Real, Intent(IN) :: D_SPACINGS(max_angles)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DRAW_LATTICE (experiment, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'io.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DRAW_PEAKS (draw_bad_weak, max_peaks, num_peaks, PEAKS, &
       status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'io.inc'
!  Import:
     Logical, Intent(IN) :: draw_bad_weak
     Integer, Intent(IN) :: max_peaks
     Integer, Intent(IN) :: num_peaks
     Type(PEAK_STRUCTURE), Intent(IN) :: PEAKS(max_peaks)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DRAW_PRESCAL (experiment, max_calibrants, num_calibrants, &
       CALIBRANT_NAMES, CALIBRANTS, pressure, baseline, status)
     Use IO_LIB
     Use LG_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Character(Len = *), Intent(IN) :: CALIBRANT_NAMES(Max_calibrants)
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Integer, Intent(IN) :: max_calibrants
     Integer, Intent(IN) :: num_calibrants
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: CALIBRANTS(max_calibrants)
     Double Precision, Intent(IN) :: pressure
     Real, Intent(IN) :: baseline
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_DRAW_PRESCAL2 (experiment, calibrant, pressure, baseline, &
       a, b, c, status)
     Use IO_LIB
     Use LG_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: calibrant
     Double Precision, Intent(IN) :: pressure
     Real, Intent(IN) :: baseline
!  Export:
     Real :: a
     Real :: b
     Real :: c
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_EDITPEAKS (experiment, draw_bad_weak, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, MASK, XAXIS, YAXIS, xstrelm, &
       ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, max_peaks, &
       num_peaks, PEAKS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: draw_bad_weak
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Integer, Intent(IN) :: max_peaks
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Integer, Intent(INOUT) :: num_peaks
     Type(PEAK_STRUCTURE), Intent(INOUT) :: PEAKS(max_peaks)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_EDITPEAKSHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_ELLIPSE (x_pixel_size, y_pixel_size, x_ellipse, y_ellipse, &
       radius1, radius2, angle1, colour, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
     Real, Intent(IN) :: x_ellipse
     Real, Intent(IN) :: y_ellipse
     Real, Intent(IN) :: radius1
     Real, Intent(IN) :: radius2
     Real, Intent(IN) :: angle1
     Integer, Intent(IN) :: colour
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ENTROPY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, WORK, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
!  Export:
     Integer, Intent(OUT) :: WORK(-65535: 65535)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
       xlabel, ylabel, zlabel, variances_exist, data_defined, x_pixel_size, &
       y_pixel_size, xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, &
       mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, &
       mzlabel, memory_defined, mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import/Export:
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: data_defined
     Real, Intent(INOUT) :: x_pixel_size
     Real, Intent(INOUT) :: y_pixel_size
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Integer, Intent(INOUT) :: mxnumdat
     Integer, Intent(INOUT) :: mynumdat
     Integer, Intent(INOUT) :: mxendelm
     Integer, Intent(INOUT) :: mxstrelm
     Integer, Intent(INOUT) :: myendelm
     Integer, Intent(INOUT) :: mystrelm
     Character(Len = *), Intent(INOUT) :: mtitle
     Character(Len = *), Intent(INOUT) :: mxlabel
     Character(Len = *), Intent(INOUT) :: mylabel
     Character(Len = *), Intent(INOUT) :: mzlabel
     Logical, Intent(INOUT) :: memory_defined
     Real, Intent(INOUT) :: mx_pixel_size
     Real, Intent(INOUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_EXT_3DVIEW (max_peaks, num_peaks, PEAKS, experiment, status)
     Implicit None
     Include 'gs.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: max_peaks
     Integer, Intent(IN) :: num_peaks
     Type(PEAK_STRUCTURE), Intent(IN) :: PEAKS(max_peaks)
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_EXT_PEAKSEARCH (input_options, xmaxdat, ymaxdat, max_peaks,&
       xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, experiment, &
       XAXIS, YAXIS, DATA, VARIANCES, MXAXIS, MYAXIS, MDATA, MVARIANCES, MASK,&
       title, xlabel, ylabel, zlabel, variances_exist, data_defined, &
       num_peaks, PEAKS, draw_bad_weak, status)
     Use IO_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: max_peaks
!  Import/Export:
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(INOUT) :: XAXIS(xmaxdat)
     Real, Intent(INOUT) :: YAXIS(ymaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: MXAXIS(xmaxdat)
     Real, Intent(INOUT) :: MYAXIS(ymaxdat)
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: variances_exist
!  Export:
     Integer, Intent(OUT) :: num_peaks
     Type(PEAK_STRUCTURE), Intent(OUT) :: PEAKS(max_peaks)
     Logical, Intent(OUT) :: draw_bad_weak
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_EXT_LOAD_PEAKS (max_peaks, num_peaks, PEAKS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: max_peaks
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: num_peaks
     Type(PEAK_STRUCTURE), Intent(OUT) :: PEAKS(max_peaks)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_EXT_SAVE_PEAKS (max_peaks, num_peaks, PEAKS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: max_peaks
     Integer, Intent(IN) :: num_peaks
     Type(PEAK_STRUCTURE), Intent(IN) :: PEAKS(max_peaks)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_EXT_TRANSFORM (experiment, two_theta, chi, phi, omega, &
       max_peaks, start_peak, num_peaks, PEAKS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Double Precision, Intent(IN) :: two_theta
     Double Precision, Intent(IN) :: chi
     Double Precision, Intent(IN) :: phi
     Double Precision, Intent(IN) :: omega
     Integer, Intent(IN) :: max_peaks
     Integer, Intent(IN) :: start_peak
     Integer, Intent(IN) :: num_peaks
!  Import/Export:
     Type(PEAK_STRUCTURE), Intent(INOUT) :: PEAKS(max_peaks)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_EXTEND (gui, xmaxdat, ymaxdat, variance_exist, xnumdat, &
       ynumdat, xstrelm, ystrelm, xendelm, yendelm, XAXIS, YAXIS, DATA, MASK, &
       VARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Logical, Intent(IN) :: variance_exist
!  Import/Export:
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Real, Intent(INOUT) :: XAXIS(xmaxdat)
     Real, Intent(INOUT) :: YAXIS(ymaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FALSEPEAK (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, DATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FEATURE (EXPERIMENT, feature, &
       max_parameters, PARAMS, PARAM_INFO, num_parameters, xmaxdat, ymaxdat, &
       xstrelm, ystrelm, xendelm, yendelm, x_order, y_order, xmin_poly, &
       ymin_poly, xmax_poly, ymax_poly, XAXIS, YAXIS, DATA, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     TYPE(EXPERIMENTAL_DETAILS), Intent(IN) :: EXPERIMENT
     Integer, Intent(IN) :: feature
     Integer, Intent(IN) :: max_parameters
     Real, Intent(IN) :: PARAMS(max_parameters)
     Integer, Intent(IN) :: PARAM_INFO(max_parameters)
     Integer, Intent(IN) :: num_parameters
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: x_order
     Integer, Intent(IN) :: y_order
     Real, Intent(IN) :: xmin_poly
     Real, Intent(IN) :: ymin_poly
     Real, Intent(IN) :: xmax_poly
     Real, Intent(IN) :: ymax_poly
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FILESEQUENCE (status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'io_db.inc'
!  Import:
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FIND_LINE_COORDS (xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, experiment, low_centre, &
       d_spacing, half_search_distance, azimuth_start, azimuth_end, &
       num_sections, integrated_ints, max_coordinates, num_coordinates, &
       X_COORDINATES, Y_COORDINATES, INTENSITIES, AZIMUTHS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: low_centre
     Real, Intent(IN) :: d_spacing
     Real, Intent(IN) :: half_search_distance
     Real, Intent(IN) :: azimuth_start
     Real, Intent(IN) :: azimuth_end
     Integer, Intent(IN) :: num_sections
     Logical, Intent(IN) :: integrated_ints
     Integer, Intent(IN) :: max_coordinates
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: num_coordinates
     Real, Intent(OUT) :: X_COORDINATES(max_coordinates)
     Real, Intent(OUT) :: Y_COORDINATES(max_coordinates)
     Real, Intent(OUT) :: AZIMUTHS(max_coordinates)
     Real, Intent(OUT) :: INTENSITIES(max_coordinates)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FIND_LINES (full_info, xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, experiment, low_centre, &
       max_rings, num_rings, D_SPACINGS, half_search_distance, &
       azimuth_start, azimuth_end, &
       num_sections, integrated_ints, max_coordinates, NUM_COORDINATES, &
       X_COORDINATES, Y_COORDINATES, INTENSITIES, AZIMUTHS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: full_info
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Logical, Intent(IN) :: low_centre
     Integer, Intent(IN) :: max_rings
     Integer, Intent(IN) :: num_rings
     Real, Intent(IN) :: D_SPACINGS(max_rings)
     Real, Intent(IN) :: half_search_distance
     Real, Intent(IN) :: azimuth_start
     Real, Intent(IN) :: azimuth_end
     Integer, Intent(IN) :: num_sections
     Logical, Intent(IN) :: integrated_ints
     Integer, Intent(IN) :: max_coordinates
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: NUM_COORDINATES(max_rings)
     Real, Intent(OUT) :: X_COORDINATES(max_coordinates, max_rings)
     Real, Intent(OUT) :: Y_COORDINATES(max_coordinates, max_rings)
     Real, Intent(OUT) :: INTENSITIES(max_coordinates, max_rings)
     Real, Intent(OUT) :: AZIMUTHS(max_coordinates, max_rings)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FIND2DGRID (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, ynumtemplate, &
       TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, xnumsubtemplate, &
       ynumsubtemplate, sub_pixelling, SUBTEMPLATE, search_limit, &
       detect_ratio, x_start, y_start, x_axis1, y_axis1, x_axis2, y_axis2, &
       output_frequency, xmax_peaks, ymax_peaks, ref_correlation, retstat, &
       xnum_grid, ynum_grid, num_peaks, num_left, num_down, X_2DPEAKS, &
       Y_2DPEAKS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xmaxtemplate
     Integer, Intent(IN) :: ymaxtemplate
     Integer, Intent(IN) :: xnumtemplate
     Integer, Intent(IN) :: ynumtemplate
     Real, Intent(IN) :: TEMPLATE(xmaxtemplate, ymaxtemplate)
     Integer, Intent(IN) :: xmaxsubtemplate
     Integer, Intent(IN) :: ymaxsubtemplate
     Integer, Intent(IN) :: xnumsubtemplate
     Integer, Intent(IN) :: ynumsubtemplate
     Integer, Intent(IN) :: sub_pixelling
     Real, Intent(IN) :: SUBTEMPLATE(xmaxsubtemplate, ymaxsubtemplate)
     Integer, Intent(IN) :: search_limit
     Real, Intent(IN) :: detect_ratio
     Real, Intent(IN) :: x_start
     Real, Intent(IN) :: y_start
     Real, Intent(IN) :: x_axis1
     Real, Intent(IN) :: y_axis1
     Real, Intent(IN) :: x_axis2
     Real, Intent(IN) :: y_axis2
     Integer, Intent(IN) :: output_frequency
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
!  Import/Export:
     Real, Intent(INOUT) :: ref_correlation
!  Export:
     Integer, Intent(OUT) :: retstat
     Integer, Intent(OUT) :: xnum_grid
     Integer, Intent(OUT) :: ynum_grid
     Integer, Intent(OUT) :: num_peaks
     Integer, Intent(OUT) :: num_left
     Integer, Intent(OUT) :: num_down
     Real, Intent(OUT) :: X_2DPEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(OUT) :: Y_2DPEAKS(xmax_peaks, ymax_peaks)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FINDCENTRE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, ynumtemplate, &
       TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, xnumsubtemplate, &
       ynumsubtemplate, sub_pixelling, SUBTEMPLATE, search_limit, &
       x_coordinate, y_coordinate, success, max_correlation, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xmaxtemplate
     Integer, Intent(IN) :: ymaxtemplate
     Integer, Intent(IN) :: xnumtemplate
     Integer, Intent(IN) :: ynumtemplate
     Real, Intent(IN) :: TEMPLATE(xmaxtemplate, ymaxtemplate)
     Integer, Intent(IN) :: xmaxsubtemplate
     Integer, Intent(IN) :: ymaxsubtemplate
     Integer, Intent(IN) :: xnumsubtemplate
     Integer, Intent(IN) :: ynumsubtemplate
     Real, Intent(IN) :: SUBTEMPLATE(xmaxsubtemplate, ymaxsubtemplate)
     Integer, Intent(IN) :: search_limit
     Integer, Intent(IN) :: sub_pixelling
!  Import/Export:
     Real, Intent(INOUT) :: x_coordinate
     Real, Intent(INOUT) :: y_coordinate
!  Export:
     Logical, Intent(OUT) :: success
     Real, Intent(OUT) :: max_correlation
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FINDLINE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, ynumtemplate, &
       TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, xnumsubtemplate, &
       ynumsubtemplate, sub_pixelling, SUBTEMPLATE, search_limit, &
       detect_ratio, search_direction, xmax_peaks, ymax_peaks, &
       ref_correlation, num_peaks, num_grid, X_2DPEAKS, Y_2DPEAKS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xmaxtemplate
     Integer, Intent(IN) :: ymaxtemplate
     Integer, Intent(IN) :: xnumtemplate
     Integer, Intent(IN) :: ynumtemplate
     Real, Intent(IN) :: TEMPLATE(xmaxtemplate, ymaxtemplate)
     Integer, Intent(IN) :: xmaxsubtemplate
     Integer, Intent(IN) :: ymaxsubtemplate
     Integer, Intent(IN) :: xnumsubtemplate
     Integer, Intent(IN) :: ynumsubtemplate
     Integer, Intent(IN) :: sub_pixelling
     Real, Intent(IN) :: SUBTEMPLATE(xmaxsubtemplate, ymaxsubtemplate)
     Integer, Intent(IN) :: search_limit
     Real, Intent(IN) :: detect_ratio
     Integer, Intent(IN) :: search_direction
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
!  Import/Export:
     Real, Intent(INOUT) :: ref_correlation
     Integer, Intent(INOUT) :: num_peaks
!  Export:
     Integer, Intent(OUT) :: num_grid
     Real, Intent(OUT) :: X_2DPEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(OUT) :: Y_2DPEAKS(xmax_peaks, ymax_peaks)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FINDLLQUAD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, ynumtemplate, &
       TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, xnumsubtemplate, &
       ynumsubtemplate, sub_pixelling, SUBTEMPLATE, search_limit, &
       detect_ratio, output_frequency, xmax_peaks, ymax_peaks, num_left, &
       num_down, ref_correlation, num_peaks, X_2DPEAKS, Y_2DPEAKS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xmaxtemplate
     Integer, Intent(IN) :: ymaxtemplate
     Integer, Intent(IN) :: xnumtemplate
     Integer, Intent(IN) :: ynumtemplate
     Real, Intent(IN) :: TEMPLATE(xmaxtemplate, ymaxtemplate)
     Integer, Intent(IN) :: xmaxsubtemplate
     Integer, Intent(IN) :: ymaxsubtemplate
     Integer, Intent(IN) :: xnumsubtemplate
     Integer, Intent(IN) :: ynumsubtemplate
     Integer, Intent(IN) :: sub_pixelling
     Real, Intent(IN) :: SUBTEMPLATE(xmaxsubtemplate, ymaxsubtemplate)
     Integer, Intent(IN) :: search_limit
     Real, Intent(IN) :: detect_ratio
     Integer, Intent(IN) :: output_frequency
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: num_left
     Integer, Intent(IN) :: num_down
!  Import/Export:
     Real, Intent(INOUT) :: ref_correlation
     Integer, Intent(INOUT) :: num_peaks
!  Export:
     Real, Intent(OUT) :: X_2DPEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(OUT) :: Y_2DPEAKS(xmax_peaks, ymax_peaks)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FINDLRQUAD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, ynumtemplate, &
       TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, xnumsubtemplate, &
       ynumsubtemplate, sub_pixelling, SUBTEMPLATE, search_limit, &
       detect_ratio, output_frequency, xmax_peaks, ymax_peaks, num_right, &
       num_down, ref_correlation, num_peaks, X_2DPEAKS, Y_2DPEAKS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xmaxtemplate
     Integer, Intent(IN) :: ymaxtemplate
     Integer, Intent(IN) :: xnumtemplate
     Integer, Intent(IN) :: ynumtemplate
     Real, Intent(IN) :: TEMPLATE(xmaxtemplate, ymaxtemplate)
     Integer, Intent(IN) :: xmaxsubtemplate
     Integer, Intent(IN) :: ymaxsubtemplate
     Integer, Intent(IN) :: xnumsubtemplate
     Integer, Intent(IN) :: ynumsubtemplate
     Integer, Intent(IN) :: sub_pixelling
     Real, Intent(IN) :: SUBTEMPLATE(xmaxsubtemplate, ymaxsubtemplate)
     Integer, Intent(IN) :: search_limit
     Real, Intent(IN) :: detect_ratio
     Integer, Intent(IN) :: output_frequency
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: num_right
     Integer, Intent(IN) :: num_down
!  Import/Export:
     Real, Intent(INOUT) :: ref_correlation
     Integer, Intent(INOUT) :: num_peaks
!  Export:
     Real, Intent(OUT) :: X_2DPEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(OUT) :: Y_2DPEAKS(xmax_peaks, ymax_peaks)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FINDRINGS (full_info, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MASK, experiment, &
       max_rings, num_rings, RADIA, half_search_distance, &
       num_sections, integrated_ints, max_coordinates, NUM_COORDINATES, &
       X_COORDINATES, Y_COORDINATES, INTENSITIES, AZIMUTHS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: full_info
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(IN) :: max_rings
     Integer, Intent(IN) :: num_rings
     Real, Intent(IN) :: RADIA(max_rings)
     Real, Intent(IN) :: half_search_distance
     Integer, Intent(IN) :: num_sections
     Logical, Intent(IN) :: integrated_ints
     Integer, Intent(IN) :: max_coordinates
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: NUM_COORDINATES(max_rings)
     Real, Intent(OUT) :: X_COORDINATES(max_coordinates, max_rings)
     Real, Intent(OUT) :: Y_COORDINATES(max_coordinates, max_rings)
     Real, Intent(OUT) :: INTENSITIES(max_coordinates, max_rings)
     Real, Intent(OUT) :: AZIMUTHS(max_coordinates, max_rings)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FINDROTATE (xmaxdat, ymaxdat, xendelm, yendelm, X_AXIS, &
       Y_AXIS, xmax_peaks, ymax_peaks, xnum_grid, ynum_grid, num_left, &
       num_down, X_2DPEAKS, Y_2DPEAKS, X_DISTORTION, Y_DISTORTION, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: xnum_grid
     Integer, Intent(IN) :: ynum_grid
     Integer, Intent(IN) :: num_left
     Integer, Intent(IN) :: num_down
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: X_2DPEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(OUT) :: Y_2DPEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(OUT) :: X_DISTORTION(xmax_peaks, ymax_peaks)
     Real, Intent(OUT) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FINDULQUAD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, ynumtemplate, &
       TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, xnumsubtemplate, &
       ynumsubtemplate, sub_pixelling, SUBTEMPLATE, search_limit, &
       detect_ratio, output_frequency, xmax_peaks, ymax_peaks, num_left, &
       num_up, ref_correlation, num_peaks, X_2DPEAKS, Y_2DPEAKS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xmaxtemplate
     Integer, Intent(IN) :: ymaxtemplate
     Integer, Intent(IN) :: xnumtemplate
     Integer, Intent(IN) :: ynumtemplate
     Real, Intent(IN) :: TEMPLATE(xmaxtemplate, ymaxtemplate)
     Integer, Intent(IN) :: xmaxsubtemplate
     Integer, Intent(IN) :: ymaxsubtemplate
     Integer, Intent(IN) :: xnumsubtemplate
     Integer, Intent(IN) :: ynumsubtemplate
     Integer, Intent(IN) :: sub_pixelling
     Real, Intent(IN) :: SUBTEMPLATE(xmaxsubtemplate, ymaxsubtemplate)
     Integer, Intent(IN) :: search_limit
     Real, Intent(IN) :: detect_ratio
     Integer, Intent(IN) :: output_frequency
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: num_left
     Integer, Intent(IN) :: num_up
!  Import/Export:
     Real, Intent(INOUT) :: ref_correlation
     Integer, Intent(INOUT) :: num_peaks
!  Export:
     Real, Intent(OUT) :: X_2DPEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(OUT) :: Y_2DPEAKS(xmax_peaks, ymax_peaks)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FINDURQUAD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, ynumtemplate, &
       TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, xnumsubtemplate, &
       ynumsubtemplate, sub_pixelling, SUBTEMPLATE, search_limit, &
       detect_ratio, output_frequency, xmax_peaks, ymax_peaks, num_right, &
       num_up, ref_correlation, num_peaks, X_2DPEAKS, Y_2DPEAKS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xmaxtemplate
     Integer, Intent(IN) :: ymaxtemplate
     Integer, Intent(IN) :: xnumtemplate
     Integer, Intent(IN) :: ynumtemplate
     Real, Intent(IN) :: TEMPLATE(xmaxtemplate, ymaxtemplate)
     Integer, Intent(IN) :: xmaxsubtemplate
     Integer, Intent(IN) :: ymaxsubtemplate
     Integer, Intent(IN) :: xnumsubtemplate
     Integer, Intent(IN) :: ynumsubtemplate
     Integer, Intent(IN) :: sub_pixelling
     Real, Intent(IN) :: SUBTEMPLATE(xmaxsubtemplate, ymaxsubtemplate)
     Integer, Intent(IN) :: search_limit
     Real, Intent(IN) :: detect_ratio
     Integer, Intent(IN) :: output_frequency
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: num_right
     Integer, Intent(IN) :: num_up
!  Import/Export:
     Real, Intent(INOUT) :: ref_correlation
     Integer, Intent(INOUT) :: num_peaks
!  Export:
     Real, Intent(OUT) :: X_2DPEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(OUT) :: Y_2DPEAKS(xmax_peaks, ymax_peaks)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FIT (variances_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, &
       title, xlabel, ylabel, zlabel, experiment, xstrelm, &
       ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, &
       mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, memory_exist, &
       mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
     Include 'f2d_lsqfit2d.inc'
!  Import:
     Logical, Intent(IN) :: variances_exist
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
     TYPE(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: yendelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: mxnumdat
     Integer, Intent(INOUT) :: mynumdat
     Integer, Intent(INOUT) :: mxendelm
     Integer, Intent(INOUT) :: mxstrelm
     Integer, Intent(INOUT) :: myendelm
     Integer, Intent(INOUT) :: mystrelm
     Character(Len = *), Intent(INOUT) :: mtitle
     Character(Len = *), Intent(INOUT) :: mxlabel
     Character(Len = *), Intent(INOUT) :: mylabel
     Character(Len = *), Intent(INOUT) :: mzlabel
     Logical, Intent(INOUT) :: memory_exist
     Real, Intent(INOUT) :: mx_pixel_size
     Real, Intent(INOUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FIT2DGRID (xmax_peaks, ymax_peaks, xnum_peaks, &
       ynum_peaks, closeness, xmin, ymin, xmax, ymax, xmaxknots, ymaxknots, &
       X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, spatial_exist, &
       x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, &
       y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: xnum_peaks
     Integer, Intent(IN) :: ynum_peaks
     Real, Intent(IN) :: closeness
     Real, Intent(IN) :: xmin
     Real, Intent(IN) :: ymin
     Real, Intent(IN) :: xmax
     Real, Intent(IN) :: ymax
     Integer, Intent(IN) :: xmaxknots
     Integer, Intent(IN) :: ymaxknots
     Real, Intent(IN) :: X_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: X_DISTORTION(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
!  Export:
     Logical, Intent(OUT) :: spatial_exist
     Integer, Intent(OUT) :: x_xnumknots
     Integer, Intent(OUT) :: x_ynumknots
     Real, Intent(OUT) :: X_LAMBDA(xmaxknots)
     Real, Intent(OUT) :: X_MU(ymaxknots)
     Real, Intent(OUT) :: X_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
     Integer, Intent(OUT) :: y_xnumknots
     Integer, Intent(OUT) :: y_ynumknots
     Real, Intent(OUT) :: Y_LAMBDA(xmaxknots)
     Real, Intent(OUT) :: Y_MU(ymaxknots)
     Real, Intent(OUT) :: Y_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FIT2DTEST (xmax_peaks, ymax_peaks, xnum_peaks, ynum_peaks, &
       X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, xmaxknots, ymaxknots, &
       x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, &
       y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: xnum_peaks
     Integer, Intent(IN) :: ynum_peaks
     Real, Intent(IN) :: X_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: X_DISTORTION(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
     Integer, Intent(IN) :: xmaxknots
     Integer, Intent(IN) :: ymaxknots
     Integer, Intent(IN) :: x_xnumknots
     Integer, Intent(IN) :: x_ynumknots
     Real, Intent(IN) :: X_LAMBDA(xmaxknots)
     Real, Intent(IN) :: X_MU(ymaxknots)
     Real, Intent(IN) :: X_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
     Integer, Intent(IN) :: y_xnumknots
     Integer, Intent(IN) :: y_ynumknots
     Real, Intent(IN) :: Y_LAMBDA(xmaxknots)
     Real, Intent(IN) :: Y_MU(ymaxknots)
     Real, Intent(IN) :: Y_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FITCALIBRANT (full_info, max_rings, num_rings, D_SPACINGS, &
       weighted_fitting, reject_outliers, reject_value, &
       refine_beam_centre, refine_sample_distance, refine_wavelength, &
       refine_tilt, experiment, radial_error, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc'
!  Import:
     Logical, Intent(IN) :: full_info
     Integer, Intent(IN) :: max_rings
     Integer, Intent(IN) :: num_rings
     Real, Intent(IN) :: D_SPACINGS(max_rings)
     Logical, Intent(IN) :: weighted_fitting
     Logical, Intent(IN) :: reject_outliers
     Real, Intent(IN) :: reject_value
     Logical, Intent(IN) :: refine_beam_centre
     Logical, Intent(IN) :: refine_sample_distance
     Logical, Intent(IN) :: refine_wavelength
     Logical, Intent(IN) :: refine_tilt
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
     Real, Intent(OUT) :: radial_error
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FITCHEBYSHEV (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, X_AXIS, Y_AXIS, MASK, xmax_coefficients, &
       ymax_coefficients, x_order, y_order, retstat, x_minimum, y_minimum, &
       x_maximum, y_maximum, COEFFICIENTS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xmax_coefficients
     Integer, Intent(IN) :: ymax_coefficients
     Integer, Intent(IN) :: x_order
     Integer, Intent(IN) :: y_order
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: retstat
     Real, Intent(OUT) :: x_minimum
     Real, Intent(OUT) :: y_minimum
     Real, Intent(OUT) :: x_maximum
     Real, Intent(OUT) :: y_maximum
     Real, Intent(OUT) :: COEFFICIENTS(ymax_coefficients, xmax_coefficients)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SUBFITCHEBY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, X_AXIS, Y_AXIS, MASK, total, y_total, num_rows, XNUM, &
       XVALUES, YVALUES, ZVALUES)
     Implicit None
!  Import:
     Integer :: xmaxdat
     Integer :: ymaxdat
     Integer :: xstrelm
     Integer :: ystrelm
     Integer :: xendelm
     Integer :: yendelm
     Real :: DATA(xmaxdat, ymaxdat)
     Real :: X_AXIS(xmaxdat)
     Real :: Y_AXIS(ymaxdat)
     Logical*1 :: MASK(xmaxdat, ymaxdat)
     Integer :: total
     Integer :: y_total
!  Import/Export:
!  Export:
     Integer :: num_rows
     Integer :: XNUM(y_total + 1)
     Real :: XVALUES(total)
     Real :: YVALUES(y_total)
     Real :: ZVALUES(total)
     Integer :: coordinate
     Integer :: x
     Integer :: y
     Logical :: values_set
End Subroutine
 
     Subroutine F2D_FITPOWDER (full_info, max_angles, num_rings, &
       weighted_fitting, detector_gain, reject_outliers, reject_value, &
       refine_beam_centre, refine_sample_distance, refine_tilt, &
       sample_distance, x_beam, y_beam, ANGLE_CONES, tilt_plane_rotation, &
       tilt_angle, radial_error, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc'
     Include 'f2d_lsqpowder.inc'
!  Import:
     Logical, Intent(IN) :: full_info
     Integer, Intent(IN) :: max_angles
     Integer, Intent(IN) :: num_rings
     Logical, Intent(IN) :: weighted_fitting
     Real, Intent(IN) :: detector_gain
     Logical, Intent(IN) :: reject_outliers
     Real, Intent(IN) :: reject_value
     Logical, Intent(IN) :: refine_beam_centre
     Logical, Intent(IN) :: refine_sample_distance
     Logical, Intent(IN) :: refine_tilt
!  Import/Export:
     Real, Intent(INOUT) :: sample_distance
     Real, Intent(INOUT) :: x_beam
     Real, Intent(INOUT) :: y_beam
     Real, Intent(INOUT) :: ANGLE_CONES(max_angles)
     Real, Intent(INOUT) :: tilt_plane_rotation
     Real, Intent(INOUT) :: tilt_angle
!  Export:
     Real, Intent(OUT) :: radial_error
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FLATFIELD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, X_AXIS, Y_AXIS, x_pixel_size, y_pixel_size, DATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
!  Import/Export:
     Real, Intent(INOUT) :: x_pixel_size
     Real, Intent(INOUT) :: y_pixel_size
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FLTFIELD (max_coords, num_subtract, correction, &
       X_SUBTRACT, Y_SUBTRACT, status)
     Implicit None
!  Import:
     Integer, Intent(IN) :: max_coords
     Integer, Intent(IN) :: num_subtract
     Real, Intent(IN) :: correction
     Real, Intent(IN) :: X_SUBTRACT(max_coords)
!  Import/Export:
     Real, Intent(INOUT) :: Y_SUBTRACT(max_coords)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FLIP (variances_exist, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, VARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: variances_exist
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat,ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat,ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FREEARRAYS (memory_exist, variances_exist, mask_exist, &
       shared_memory, memory_id, pDATA, pXAXIS, pYAXIS, pVARIANCES, pMASK, &
       pMDATA, pMXAXIS, pMYAXIS, pMVARIANCES, results, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'io.inc'
!  Import:
     Logical, Intent(IN) :: memory_exist
     Logical, Intent(IN) :: variances_exist
     Logical, Intent(IN) :: mask_exist
     Logical, Intent(IN) :: shared_memory
     Integer, Intent(IN) :: memory_id
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: pDATA
     Integer, Intent(OUT) :: pXAXIS
     Integer, Intent(OUT) :: pYAXIS
     Integer, Intent(OUT) :: pVARIANCES
     Integer, Intent(OUT) :: pMASK
     Integer, Intent(OUT) :: pMDATA
     Integer, Intent(OUT) :: pMXAXIS
     Integer, Intent(OUT) :: pMYAXIS
     Integer, Intent(OUT) :: pMVARIANCES
     Type(RESULT_VECTORS), Intent(INOUT) :: results
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FSINPUT (input_options, &
       data_defined, memory_exist, memory_defined, &
       variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, &
       YAXIS, DATA, VARIANCES, MASK, title, xlabel, ylabel, zlabel, &
       x_pixel_size, y_pixel_size, xstrelm, ystrelm, xendelm, yendelm, &
       mxnumdat, mynumdat, MXAXIS, MYAXIS, MDATA, MVARIANCES, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Real, Intent(INOUT) :: XAXIS(xmaxdat)
     Real, Intent(INOUT) :: YAXIS(ymaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Real, Intent(INOUT) :: x_pixel_size
     Real, Intent(INOUT) :: y_pixel_size
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FSINTEGRATE (input_options, &
       data_defined, memory_exist, memory_defined, &
       variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, &
       YAXIS, DATA, VARIANCES, MASK, title, xlabel, ylabel, zlabel, &
       experiment, xstrelm, ystrelm, xendelm, yendelm, &
       mxnumdat, mynumdat, MXAXIS, MYAXIS, MDATA, MVARIANCES, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Real, Intent(INOUT) :: XAXIS(xmaxdat)
     Real, Intent(INOUT) :: YAXIS(ymaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FSPROJECTIONS (input_options, &
       data_defined, memory_exist, memory_defined, &
       variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, &
       YAXIS, DATA, VARIANCES, MASK, title, xlabel, ylabel, zlabel, &
       experiment, xstrelm, ystrelm, xendelm, yendelm, &
       mxnumdat, mynumdat, MXAXIS, MYAXIS, MDATA, MVARIANCES, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Real, Intent(INOUT) :: XAXIS(xmaxdat)
     Real, Intent(INOUT) :: YAXIS(ymaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FSSTATISTICS (input_options, data_defined, &
       variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, &
       YAXIS, DATA, VARIANCES, MASK, title, xlabel, ylabel, zlabel, &
       experiment, xstrelm, ystrelm, xendelm, yendelm, results, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Real, Intent(INOUT) :: XAXIS(xmaxdat)
     Real, Intent(INOUT) :: YAXIS(ymaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Type(RESULT_VECTORS), Intent(INOUT) :: results
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FULL (xnumdat, ynumdat, xstrelm, ystrelm, xendelm, &
       yendelm, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FUN1DSYMMETRY (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_control, num_control, CONTROL, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       RESIDUALS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: max_control
     Integer, Intent(IN) :: num_control
     Integer, Intent(IN) :: CONTROL(max_control)
     Integer, Intent(IN) :: max_par
     Integer, Intent(IN) :: num_par
     Real, Intent(IN) :: PARAMS(max_par)
     Integer, Intent(IN) :: xmaxmod
     Integer, Intent(IN) :: ymaxmod
     Logical, Intent(IN) :: model_align
!  Export:
     Integer, Intent(OUT) :: mod_stat
     Real, Intent(OUT) :: RESIDUALS(xmaxmod, ymaxmod)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FUNCALIBRANT (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_control, num_control, CONTROL, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       RESIDUALS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc'
     Include 'f2d_lsqpowder.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: max_control
     Integer, Intent(IN) :: num_control
     Integer, Intent(IN) :: CONTROL(max_control)
     Integer, Intent(IN) :: max_par
     Integer, Intent(IN) :: num_par
     Real, Intent(IN) :: PARAMS(max_par)
     Integer, Intent(IN) :: xmaxmod
     Integer, Intent(IN) :: ymaxmod
     Logical, Intent(IN) :: model_align
!  Export:
     Integer, Intent(OUT) :: mod_stat
     Real, Intent(OUT) :: RESIDUALS(xmaxmod, ymaxmod)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FUNCIRCLE (EXPERIMENT, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_control, num_control, CONTROL, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       MODEL, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc'
!  Import:
     TYPE(EXPERIMENTAL_DETAILS), Intent(IN) :: EXPERIMENT
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: max_control
     Integer, Intent(IN) :: num_control
     Integer, Intent(IN) :: CONTROL(max_control)
     Integer, Intent(IN) :: max_par
     Integer, Intent(IN) :: num_par
     Real, Intent(IN) :: PARAMS(num_par)
     Integer, Intent(IN) :: xmaxmod
     Integer, Intent(IN) :: ymaxmod
     Logical, Intent(IN) :: model_align
!  Export:
     Integer, Intent(OUT) :: mod_stat
     Real, Intent(OUT) :: MODEL(xmaxmod, ymaxmod)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FUNELLIPSE (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_control, num_control, CONTROL, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       MODEL, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: max_control
     Integer, Intent(IN) :: num_control
     Integer, Intent(IN) :: CONTROL(max_control)
     Integer, Intent(IN) :: max_par
     Integer, Intent(IN) :: num_par
     Real, Intent(IN) :: PARAMS(max_par)
     Integer, Intent(IN) :: xmaxmod
     Integer, Intent(IN) :: ymaxmod
     Logical, Intent(IN) :: model_align
!  Export:
     Integer, Intent(OUT) :: mod_stat
     Real, Intent(OUT) :: MODEL(xmaxmod, ymaxmod)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FUNFIT2D (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_parameters, num_parameters, &
       PARAM_INFO, max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, &
       mod_stat, MODEL, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_lsqfit2d.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: num_parameters
     Integer, Intent(IN) :: PARAM_INFO(max_parameters)
     Integer, Intent(IN) :: max_par
     Integer, Intent(IN) :: num_par
     Real, Intent(IN) :: PARAMS(max_par)
     Integer, Intent(IN) :: xmaxmod
     Integer, Intent(IN) :: ymaxmod
     Logical, Intent(IN) :: model_align
!  Export:
     Integer, Intent(OUT) :: mod_stat
     Real, Intent(OUT) :: MODEL(xmaxmod, ymaxmod)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FUNGAUSSIAN (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_control, num_control, CONTROL, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       MODEL, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: max_control
     Integer, Intent(IN) :: num_control
     Integer, Intent(IN) :: CONTROL(max_control)
     Integer, Intent(IN) :: max_par
     Integer, Intent(IN) :: num_par
     Real, Intent(IN) :: PARAMS(max_par)
     Integer, Intent(IN) :: xmaxmod
     Integer, Intent(IN) :: ymaxmod
     Logical, Intent(IN) :: model_align
!  Export:
     Integer, Intent(OUT) :: mod_stat
     Real, Intent(OUT) :: MODEL(xmaxmod, ymaxmod)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FUNID06CALIBRANT (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_control, num_control, CONTROL, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       RESIDUALS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc'
     Include 'f2d_lsqpowder.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: max_control
     Integer, Intent(IN) :: num_control
     Integer, Intent(IN) :: CONTROL(max_control)
     Integer, Intent(IN) :: max_par
     Integer, Intent(IN) :: num_par
     Real, Intent(IN) :: PARAMS(max_par)
     Integer, Intent(IN) :: xmaxmod
     Integer, Intent(IN) :: ymaxmod
     Logical, Intent(IN) :: model_align
!  Export:
     Integer, Intent(OUT) :: mod_stat
     Real, Intent(OUT) :: RESIDUALS(xmaxmod, ymaxmod)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FUNMFIT (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, xstrelm, &
       ystrelm, xendelm, yendelm, max_parameters, num_parameters, PARAM_INFO, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       MODEL, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_lsqmfit.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: num_parameters
     Integer, Intent(IN) :: PARAM_INFO(max_parameters)
     Integer, Intent(IN) :: max_par
     Integer, Intent(IN) :: num_par
     Real, Intent(IN) :: PARAMS(max_par)
     Integer, Intent(IN) :: xmaxmod
     Integer, Intent(IN) :: ymaxmod
     Logical, Intent(IN) :: model_align
!  Export:
     Integer, Intent(OUT) :: mod_stat
     Real, Intent(OUT) :: MODEL(xmaxmod, ymaxmod)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FUNPOLARISATION (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_control, num_control, CONTROL, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       RESIDUALS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc'
     Include 'f2d_lsqpowder.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: max_control
     Integer, Intent(IN) :: num_control
     Integer, Intent(IN) :: CONTROL(max_control)
     Integer, Intent(IN) :: max_par
     Integer, Intent(IN) :: num_par
     Real, Intent(IN) :: PARAMS(max_par)
     Integer, Intent(IN) :: xmaxmod
     Integer, Intent(IN) :: ymaxmod
     Logical, Intent(IN) :: model_align
!  Export:
     Integer, Intent(OUT) :: mod_stat
     Real, Intent(OUT) :: RESIDUALS(xmaxmod, ymaxmod)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FUNPOWDER (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_control, num_control, CONTROL, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       RESIDUALS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc'
     Include 'f2d_lsqpowder.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: max_control
     Integer, Intent(IN) :: num_control
     Integer, Intent(IN) :: CONTROL(max_control)
     Integer, Intent(IN) :: max_par
     Integer, Intent(IN) :: num_par
     Real, Intent(IN) :: PARAMS(max_par)
     Integer, Intent(IN) :: xmaxmod
     Integer, Intent(IN) :: ymaxmod
     Logical, Intent(IN) :: model_align
!  Export:
     Integer, Intent(OUT) :: mod_stat
     Real, Intent(OUT) :: RESIDUALS(xmaxmod, ymaxmod)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FUNTYPE (maxpar, PARDES, numpar, feature, feattype, &
       firstpar, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: maxpar
     Integer, Intent(IN) :: PARDES(maxpar)
     Integer, Intent(IN) :: numpar
     Integer, Intent(IN) :: feature
!  Export:
     Integer, Intent(OUT) :: feattype
     Integer, Intent(OUT) :: firstpar
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_FUJI (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, VARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GAUSSIAN (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, DATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GEOMETRY (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
       experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GRIDBEAMCENTRE (x_beam, y_beam, xmax_peaks, ymax_peaks, &
       xnumpeaks, ynumpeaks, X_PEAKS, Y_PEAKS, x_grid_co, y_grid_co, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: x_beam
     Real, Intent(IN) :: y_beam
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: xnumpeaks
     Integer, Intent(IN) :: ynumpeaks
     Real, Intent(IN) :: X_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_PEAKS(xmax_peaks, ymax_peaks)
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: x_grid_co
     Real, Intent(OUT) :: y_grid_co
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GSTATISTICS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, X_AXIS, Y_AXIS, DATA, MASK, title, xlabel, ylabel, zlabel, &
       status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI (fit2d_version, gui, output_graphics, shared_memory, &
       memory_id, input_file, data_defined, memory_exist, memory_defined, &
       variance_exist, mask_exist, log_file_open, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, input_options, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, results, status)
     Use IO_LIB
     Use LG_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Character(Len = *), Intent(IN) :: fit2d_version
     Logical, Intent(IN) :: gui
     Logical, Intent(IN) :: output_graphics
     Logical, Intent(IN) :: shared_memory
     Integer, Intent(IN) :: memory_id
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variance_exist
     Logical, Intent(INOUT) :: mask_exist
     Logical, Intent(INOUT) :: log_file_open
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Type(INPUT_OPTIONS_STRUCTURE), Intent(INOUT) :: input_options
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Integer, Intent(INOUT) :: mxnumdat
     Integer, Intent(INOUT) :: mynumdat
     Integer, Intent(INOUT) :: mxendelm
     Integer, Intent(INOUT) :: mxstrelm
     Integer, Intent(INOUT) :: myendelm
     Integer, Intent(INOUT) :: mystrelm
     Character(Len = *), Intent(INOUT) :: mtitle
     Character(Len = *), Intent(INOUT) :: mxlabel
     Character(Len = *), Intent(INOUT) :: mylabel
     Character(Len = *), Intent(INOUT) :: mzlabel
     Real, Intent(INOUT) :: mx_pixel_size
     Real, Intent(INOUT) :: my_pixel_size
     Type(RESULT_VECTORS), Intent(INOUT) :: results
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_2DFIT (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, x_label, y_label, z_label, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mx_label, my_label, mz_label, &
       mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
     Include 'f2d_lsqfit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variance_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: x_label
     Character(Len = *), Intent(INOUT) :: y_label
     Character(Len = *), Intent(INOUT) :: z_label
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Integer, Intent(INOUT) :: mxnumdat
     Integer, Intent(INOUT) :: mynumdat
     Integer, Intent(INOUT) :: mxendelm
     Integer, Intent(INOUT) :: mxstrelm
     Integer, Intent(INOUT) :: myendelm
     Integer, Intent(INOUT) :: mystrelm
     Character(Len = *), Intent(INOUT) :: mtitle
     Character(Len = *), Intent(INOUT) :: mx_label
     Character(Len = *), Intent(INOUT) :: my_label
     Character(Len = *), Intent(INOUT) :: mz_label
     Real, Intent(INOUT) :: mx_pixel_size
     Real, Intent(INOUT) :: my_pixel_size
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_GUI_2DFITHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_GUI_CORRECTION (input_options, data_defined, memory_exist, &
       memory_defined, variances_exist, xmaxdat, ymaxdat, title, xlabel, &
       ylabel, zlabel, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
       x_pixel_size, y_pixel_size, mtitle, mxlabel, mylabel, mzlabel, &
       mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, &
       mx_pixel_size, my_pixel_size, experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variances_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Real, Intent(INOUT) :: x_pixel_size
     Real, Intent(INOUT) :: y_pixel_size
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_EXPERIMENT (detector_offset, experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: detector_offset
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_EXTREME_XTAL (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
     Use IO_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_FILTER ( data_defined, memory_exist, memory_defined, &
       variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
       xlabel, ylabel, zlabel, experiment, xstrelm, ystrelm, &
       xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
       myendelm, mtitle, mxlabel, mylabel, mzlabel, mx_pixel_size, &
       my_pixel_size, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variance_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_FS (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, results, status)
     Use IO_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
     Type(RESULT_VECTORS), Intent(INOUT) :: results
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_GUI_FSHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_GUI_GEOMETRIC (data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variance_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_GRID (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, DATA, title, xlabel, ylabel, zlabel, xstrelm, ystrelm, xendelm, &
       yendelm, experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_ID06LVP (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_IP (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
     Use IO_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_GUI_IPHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_GUI_MACROS (status)
     Use IO_LIB
     Use GS_LIB
     Implicit None
     Include 'st_symbols.inc'
     Include 'io_db.inc'
     Include 'gs_constants.inc'
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_GUI_MACROSHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_GUI_MATHS (experiment, &
       data_defined, memory_exist, memory_defined, &
       variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, &
       mxnumdat, mynumdat, MX_AXIS, MY_AXIS, MDATA, MVARIANCES, &
       mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, &
       mzlabel, mx_pixel_size, my_pixel_size, &
       X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
       xstrelm, ystrelm, xendelm, yendelm, DATA, VARIANCES, MASK, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: data_defined
     Logical, Intent(IN) :: memory_exist
     Logical, Intent(IN) :: memory_defined
     Logical, Intent(IN) :: variance_exist
     Logical, Intent(IN) :: mask_exist
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: mxnumdat
     Integer, Intent(IN) :: mynumdat
     Real, Intent(IN) :: MX_AXIS(xmaxdat)
     Real, Intent(IN) :: MY_AXIS(ymaxdat)
     Real, Intent(IN) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: MVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: mxendelm
     Integer, Intent(IN) :: mxstrelm
     Integer, Intent(IN) :: myendelm
     Integer, Intent(IN) :: mystrelm
     Character(Len = *), Intent(IN) :: mtitle
     Character(Len = *), Intent(IN) :: mxlabel
     Character(Len = *), Intent(IN) :: mylabel
     Character(Len = *), Intent(IN) :: mzlabel
     Real, Intent(IN) :: mx_pixel_size
     Real, Intent(IN) :: my_pixel_size
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_MFIT (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, x_label, y_label, z_label, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mx_label, my_label, mz_label, &
       mx_pixel_size, my_pixel_size, results, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
     Include 'f2d_lsqmfit.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variance_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: x_label
     Character(Len = *), Intent(INOUT) :: y_label
     Character(Len = *), Intent(INOUT) :: z_label
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Integer, Intent(INOUT) :: mxnumdat
     Integer, Intent(INOUT) :: mynumdat
     Integer, Intent(INOUT) :: mxendelm
     Integer, Intent(INOUT) :: mxstrelm
     Integer, Intent(INOUT) :: myendelm
     Integer, Intent(INOUT) :: mystrelm
     Character(Len = *), Intent(INOUT) :: mtitle
     Character(Len = *), Intent(INOUT) :: mx_label
     Character(Len = *), Intent(INOUT) :: my_label
     Character(Len = *), Intent(INOUT) :: mz_label
     Real, Intent(INOUT) :: mx_pixel_size
     Real, Intent(INOUT) :: my_pixel_size
     Type(RESULT_VECTORS), Intent(INOUT) :: results
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_GUI_MFITHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_GUI_PARAMETERS (gui, EXPERIMENT, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, Y_AXIS, DATA, &
       title, xlabel, ylabel, zlabel, max_parameters, max_results, xstrelm, &
       ystrelm, xendelm, yendelm, MASK, x_order, y_order, xmin_poly, &
       ymin_poly, xmax_poly, ymax_poly, num_features, num_parameters, PARAMS, &
       PARAM_INFO, CONSTRAINTS, PARNAMES, SCALE_FACTORS, num_results, &
       RESNAMES, MODEL, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     TYPE(EXPERIMENTAL_DETAILS), Intent(INOUT) :: EXPERIMENT
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: max_results
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
!  Export:
     Integer, Intent(OUT) :: x_order
     Integer, Intent(OUT) :: y_order
     Real, Intent(OUT) :: xmin_poly
     Real, Intent(OUT) :: ymin_poly
     Real, Intent(OUT) :: xmax_poly
     Real, Intent(OUT) :: ymax_poly
     Integer, Intent(OUT) :: num_features
     Integer, Intent(OUT) :: num_parameters
     Real, Intent(OUT) :: PARAMS(max_parameters)
     Integer, Intent(OUT) :: PARAM_INFO(max_parameters)
     Logical, Intent(OUT) :: CONSTRAINTS(max_parameters)
     Character(Len = *), Intent(OUT) :: PARNAMES(max_parameters)
     Real, Intent(OUT) :: SCALE_FACTORS(max_parameters)
     Integer, Intent(OUT) :: num_results
     Character(Len = *), Intent(OUT) :: RESNAMES(max_results)
     Real, Intent(OUT) :: MODEL(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_PD (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
     Use IO_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variance_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_GUI_PDHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_GUI_PEAKSEARCH (allow_edit, current_file, ask_change, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, MASK, XAXIS, YAXIS, xstrelm, &
       ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, max_peaks, &
       experiment, num_peaks, MDATA, PEAKS, draw_bad_weak, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: allow_edit
     Character(Len = *), Intent(IN) :: current_file
     Logical, Intent(IN) :: ask_change
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Integer, Intent(IN) :: max_peaks
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: num_peaks
!  Export:
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Type(PEAK_STRUCTURE), Intent(OUT) :: PEAKS(max_peaks)
     Logical, Intent(OUT) :: draw_bad_weak
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_PRESSURECAL (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variance_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_PROJECTION (mask_data, xmaxdat, ymaxdat, title, &
       xlabel, ylabel, zlabel, variances_exist, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, DATA, MASK, xstrelm, ystrelm, xendelm, yendelm, experiment, &
       memory_defined, mtitle, mxlabel, mylabel, mzlabel, &
       mxnumdat, mynumdat, MX_AXIS, MDATA, MVARIANCES, mxstrelm, mystrelm, &
       mxendelm, myendelm, mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Import/Export:
     Logical, Intent(INOUT) :: mask_data
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Logical, Intent(INOUT) :: variances_exist
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Real, Intent(INOUT) :: X_AXIS(xmaxdat)
     Real, Intent(INOUT) :: Y_AXIS(ymaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
     Logical, Intent(OUT) :: memory_defined
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MX_AXIS(xmaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_RECIPROCAL (input_file, data_defined, memory_exist, &
       memory_defined, variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, input_options, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Type(INPUT_OPTIONS_STRUCTURE), Intent(INOUT) :: input_options
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_GUI_RECIPROCALHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_GUI_REFLECTION (image, num_reflection, prompt, h, k, l, &
       EXPERIMENT, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: image
     Integer, Intent(INOUT) :: num_reflection
     Character(Len = *), Intent(IN) :: prompt
!  Import/Export:
     Integer, Intent(INOUT) :: h
     Integer, Intent(INOUT) :: k
     Integer, Intent(INOUT) :: l
     TYPE(EXPERIMENTAL_DETAILS), Intent(OUT) :: EXPERIMENT
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_ROI (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
       variances_exist, experiment, xstrelm, ystrelm, xendelm, &
       yendelm, print_type, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Character(Len = *), Intent(OUT) :: print_type
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_GUI_ROIHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_GUI_SAXS (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
     Use IO_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_GUI_SAXSHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_GUI_SCALEDSUB (mask_data, experiment, xmaxdat, &
       ymaxdat, xstrelm, ystrelm, xendelm, yendelm, X_AXIS, Y_AXIS, MDATA, &
       MASK, title, xlabel, ylabel, zlabel, DATA, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: mask_data
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: MDATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_GUI_SCALEDSUBHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_GUI_SEQUENCE (inmacro_file, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'io_db.inc'
!  Import:
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: inmacro_file
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_SIZES (xmaxdat, ymaxdat, memory_exist, &
       variance_arrays, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: variance_arrays
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_SETUP (input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, input_options, &
       xstrelm, ystrelm, xendelm, yendelm, status)
     Use IO_LIB
     Use MA_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variance_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Type(INPUT_OPTIONS_STRUCTURE), Intent(INOUT) :: input_options
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_TEST (input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
     Use IO_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variance_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_UNITCELL (experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
!  Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_GUI_VECHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_GUI_VECTORS (input_options, experiment, results, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
       DATA, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Import/Export:
     Type(RESULT_VECTORS), Intent(INOUT) :: results
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Export:
     Integer, Intent(OUT) :: xnumdat
     Integer, Intent(OUT) :: ynumdat
     Integer, Intent(OUT) :: xstrelm
     Integer, Intent(OUT) :: ystrelm
     Integer, Intent(OUT) :: xendelm
     Integer, Intent(OUT) :: yendelm
     Real, Intent(OUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: XAXIS(xmaxdat)
     Real, Intent(OUT) :: YAXIS(ymaxdat)
     Character(Len = *), Intent(OUT) :: title
     Character(Len = *), Intent(OUT) :: xlabel
     Character(Len = *), Intent(OUT) :: ylabel
     Character(Len = *), Intent(OUT) :: zlabel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_GUI_XTALHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_GUI_XTALLOGRAPHY (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
     Use IO_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variance_exist
     Logical, Intent(INOUT) :: mask_exist
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_GUI_ZSCALE (mask_data, xmaxdat, ymaxdat, xnumdat, ynumdat, &
       X_AXIS, Y_AXIS, DATA, MASK, title, xlabel, ylabel, zlabel, xstrelm, &
       ystrelm, xendelm, yendelm, experiment, lut_click, x_pc, y_pc, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: lut_click
     Real, Intent(IN) :: x_pc
     Real, Intent(IN) :: y_pc
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_GUIHELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
    Subroutine F2D_HELP (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine f2d_help_calibrate (status)
     Implicit None
!  Status:
     Integer :: status
End Subroutine
 
     Subroutine F2D_HISTOGRAM (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, title, mxstrelm, mystrelm, mxendelm, myendelm, &
       MDATA, MXAXIS, MYAXIS, mtitle, mxlabel, mylabel, mzlabel, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
!  Export:
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: mystrelm
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: myendelm
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_I2C (status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'io_db.inc'
!  Import:
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ID06_CAKE (gui, xmaxdat, ymaxdat, &
       azimuth_start, azimuth_end, data_defined, title, &
       xlabel, ylabel, zlabel, variances_exist, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, experiment, &
       lorentz_geometry, data_type, memory_defined, mask_data, &
       mask_memory, mtitle, mxlabel, mylabel, mzlabel, mxnumdat, mynumdat, &
       mxstrelm, mystrelm, mxendelm, myendelm, mx_pixel_size, my_pixel_size, &
       memory_data_type, status)
     Use IO_LIB
     Use MA_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: azimuth_start
     Real, Intent(IN) :: azimuth_end
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Logical, Intent(INOUT) :: variances_exist
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: lorentz_geometry
     Integer, Intent(INOUT) :: data_type
!  Export:
     Logical, Intent(OUT) :: memory_defined
     Logical, Intent(OUT) :: mask_data
     Logical, Intent(OUT) :: mask_memory
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
     Integer, Intent(INOUT) :: memory_data_type
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ID06_CALIBRANT3 (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, XAXIS, YAXIS, DATA, MASK, &
       title, xlabel, ylabel, zlabel, azimuth_start, azimuth_end, experiment, &
       status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fitrings.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Real, Intent(IN) :: azimuth_start
     Real, Intent(IN) :: azimuth_end
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ID06_CALIBRANT4 (xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, XAXIS, YAXIS, DATA, MASK, &
       title, xlabel, ylabel, zlabel, azimuth_start, azimuth_end, &
       calibrant_sample, max_cali_rings, num_cali_rings, D_SPACINGS, &
       refine_detector_offset, refine_beam_centre, refine_detector_distance, &
       refine_tilt, refine_wavelength, reject_outliers, experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fitrings.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Real, Intent(IN) :: azimuth_start
     Real, Intent(IN) :: azimuth_end
     Character(Len = *), Intent(IN) :: calibrant_sample
     Integer :: max_cali_rings
     Integer :: num_cali_rings
     Real, Intent(IN) :: D_SPACINGS(max_cali_rings)
     Logical, Intent(IN) :: refine_detector_offset
     Logical, Intent(IN) :: refine_beam_centre
     Logical, Intent(IN) :: refine_detector_distance
     Logical, Intent(IN) :: refine_tilt
     Logical, Intent(IN) :: refine_wavelength
     Logical, Intent(IN) :: reject_outliers
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ID06_CARTESIAN (xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, DATA, experiment, &
       azimuth_start, azimuth_end, &
       mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, MEMORY, &
       memory_defined, mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat,ymaxdat)
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(IN) :: azimuth_start
     Real, Intent(IN) :: azimuth_end
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: mystrelm
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: myendelm
     Real, Intent(OUT) :: MEMORY(xmaxdat, ymaxdat)
     Logical, Intent(OUT) :: memory_defined
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ID06_INPUT ( &
       input_options, xmaxdat, ymaxdat, variances_exist, data_defined, &
       file_name, xnumdat, ynumdat, XAXIS, YAXIS, DATA, VARIANCES, &
       title, xlabel, ylabel, zlabel, experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Import/Export:
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: data_defined
!  Export:
     Character(Len = *), Intent(OUT) :: file_name
     Integer, Intent(OUT) :: xnumdat
     Integer, Intent(OUT) :: ynumdat
     Real, Intent(OUT) :: XAXIS(xmaxdat)
     Real, Intent(OUT) :: YAXIS(ymaxdat)
     Real, Intent(OUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(OUT) :: title
     Character(Len = *), Intent(OUT) :: xlabel
     Character(Len = *), Intent(OUT) :: ylabel
     Character(Len = *), Intent(OUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(OUT) :: experiment
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ID06_LINEPAIRS (max_coordinates, max_rings, num_rings, &
       NUM_COORDINATES, X_POLAR, AZIMUTHS, &
       num_pairs, X_PAIRS, PAIR_DIFS, AZI_PAIRS, min_pos, max_pos, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: max_coordinates
     Integer, Intent(IN) :: max_rings
     Integer, Intent(IN) :: num_rings
     Integer, Intent(IN) :: NUM_COORDINATES(max_rings)
     Real, Intent(IN) :: X_POLAR(max_coordinates, max_rings)
     Real, Intent(IN) :: AZIMUTHS(max_coordinates, max_rings)
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: num_pairs
     Real, Intent(OUT) :: X_PAIRS(max_coordinates)
     Real, Intent(OUT) :: PAIR_DIFS(max_coordinates)
     Real, Intent(OUT) :: AZI_PAIRS(max_coordinates)
     Integer, Intent(OUT) :: min_pos
     Integer, Intent(OUT) :: max_pos
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ID06_LSQCALIBRANT (full_info, max_rings, num_rings, &
       D_SPACINGS, &
       weighted_fitting, refine_detector_offset, refine_beam_centre, &
       refine_detector_distance, refine_wavelength, refine_tilt, &
       experiment, radial_error, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc'
     Include 'f2d_lsqpowder.inc'
!  Import:
     Logical, Intent(IN) :: full_info
     Integer, Intent(IN) :: max_rings
     Integer, Intent(IN) :: num_rings
     Real, Intent(IN) :: D_SPACINGS(max_rings)
     Logical, Intent(IN) :: weighted_fitting
     Logical, Intent(IN) :: refine_detector_offset
     Logical, Intent(IN) :: refine_beam_centre
     Logical, Intent(IN) :: refine_detector_distance
     Logical, Intent(IN) :: refine_wavelength
     Logical, Intent(IN) :: refine_tilt
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
     Real, Intent(OUT) :: radial_error
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ID06_DISPLAY (max_coordinates, max_rings, num_rings, &
       NUM_COORDINATES, X_COORDINATES, Y_COORDINATES, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: max_coordinates
     Integer, Intent(IN) :: max_rings
     Integer, Intent(IN) :: num_rings
     Integer, Intent(IN) :: NUM_COORDINATES(max_rings)
     Real, Intent(IN) :: X_COORDINATES(max_coordinates, max_rings)
     Real, Intent(IN) :: Y_COORDINATES(max_coordinates, max_rings)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_IDEALSPACE (xmax_peaks, ymax_peaks, xnumpeaks, ynumpeaks, &
       X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: xnumpeaks
     Integer, Intent(IN) :: ynumpeaks
!  Import/Export:
     Real, Intent(INOUT) :: X_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(INOUT) :: Y_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(INOUT) :: X_DISTORTION(xmax_peaks, ymax_peaks)
     Real, Intent(INOUT) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_IMAGE (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, MASK, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
       variances_exist, experiment, xstrelm, ystrelm, xendelm, &
       yendelm, mxnumdat, mynumdat, MX_AXIS, MY_AXIS, MDATA, MVARIANCES, &
       mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, &
       mzlabel, memory_defined, mx_pixel_size, my_pixel_size, print_type, &
       status)
     Use IO_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MX_AXIS(xmaxdat)
     Real, Intent(OUT) :: MY_AXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Logical, Intent(OUT) :: memory_defined
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
     Character(Len = *), Intent(OUT) :: print_type
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_IN_SPATIAL (gui, xmaxknots, ymaxknots, file_name, retstat, &
       x_min, y_min, x_max, y_max, cor_grid_spacing, x_pixel_size, &
       y_pixel_size, x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
       y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxknots
     Integer, Intent(IN) :: ymaxknots
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: file_name
!  Export:
     Integer, Intent(OUT) :: retstat
     Real, Intent(OUT) :: x_min
     Real, Intent(OUT) :: y_min
     Real, Intent(OUT) :: x_max
     Real, Intent(OUT) :: y_max
     Real, Intent(OUT) :: cor_grid_spacing
     Real, Intent(OUT) :: x_pixel_size
     Real, Intent(OUT) :: y_pixel_size
     Integer, Intent(OUT) :: x_xnumknots
     Integer, Intent(OUT) :: x_ynumknots
     Real, Intent(OUT) :: X_LAMBDA(xmaxknots)
     Real, Intent(OUT) :: X_MU(ymaxknots)
     Real, Intent(OUT) :: X_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
     Integer, Intent(OUT) :: y_xnumknots
     Integer, Intent(OUT) :: y_ynumknots
     Real, Intent(OUT) :: Y_LAMBDA(xmaxknots)
     Real, Intent(OUT) :: Y_MU(ymaxknots)
     Real, Intent(OUT) :: Y_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_IN2DCLUT (input_file, retstat, x_cor_size, y_cor_size, &
       xmax_lut, ymax_lut, xnum_lut, ynum_lut, X_SD, Y_SD, INT_REBINNED, &
       xmaxdat, ymaxdat, INVERSE_FF, dc_lut_defined, flat_field_defined, &
       status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Character(Len = *), Intent(IN) :: input_file
     Integer, Intent(IN) :: xmax_lut
     Integer, Intent(IN) :: ymax_lut
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: retstat
     Real, Intent(OUT) :: x_cor_size
     Real, Intent(OUT) :: y_cor_size
     Integer, Intent(OUT) :: xnum_lut
     Integer, Intent(OUT) :: ynum_lut
     Byte, Intent(OUT) :: X_SD(xmax_lut, ymax_lut)
     Byte, Intent(OUT) :: Y_SD(xmax_lut, ymax_lut)
     Byte, Intent(OUT) :: INT_REBINNED(9, xmax_lut, ymax_lut)
     Real, Intent(OUT) :: INVERSE_FF(xmaxdat, ymaxdat)
     Logical, Intent(OUT) :: dc_lut_defined
     Logical, Intent(OUT) :: flat_field_defined
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SUBIN2DCLUT (file_id, max_buffer, BUFFER, pos_buffer, &
       retstat, byte_value, status)
     Implicit None
!  Import:
     Integer, Intent(IN) :: file_id
     Integer, Intent(IN) :: max_buffer
!  Import/Export:
     Byte, Intent(INOUT) :: BUFFER(max_buffer)
     Integer, Intent(INOUT) :: pos_buffer
!  Export:
     Integer, Intent(OUT) :: retstat
     Byte, Intent(OUT) :: byte_value
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INFORMATION (xmaxdat, ymaxdat, data_exist, variance_array, &
       xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, memory_exist, &
       mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Logical, Intent(IN) :: data_exist
     Logical, Intent(IN) :: variance_array
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: memory_exist
     Integer, Intent(IN) :: mxnumdat
     Integer, Intent(IN) :: mynumdat
     Integer, Intent(IN) :: mxstrelm
     Integer, Intent(IN) :: mystrelm
     Integer, Intent(IN) :: mxendelm
     Integer, Intent(IN) :: myendelm
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INITGRIDSEARCH (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, &
       num_display, x_start, y_start, x_axis1, y_axis1, x_axis2, y_axis2, &
       status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Integer, Intent(IN) :: num_display
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: x_start
     Real, Intent(OUT) :: y_start
     Real, Intent(OUT) :: x_axis1
     Real, Intent(OUT) :: y_axis1
     Real, Intent(OUT) :: x_axis2
     Real, Intent(OUT) :: y_axis2
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_BEAMCENTRE (gui, x_beam, y_beam, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
!  Import/Export:
     Real, Intent(INOUT) :: x_beam
     Real, Intent(INOUT) :: y_beam
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_CALIBRANT (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, mask_data, XAXIS, YAXIS, DATA, MASK, title, xlabel, &
       ylabel, zlabel, experiment, calibrant_sample, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: mask_data
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
!  Export:
     Character(Len = *), Intent(OUT) :: calibrant_sample
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_CAKE (xmaxdat, ymaxdat, DATA, MASK, X_AXIS, Y_AXIS, &
       xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
       experiment, start_azimuth, end_azimuth, inner_limit, outer_limit, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
     Real, Intent(OUT) :: start_azimuth
     Real, Intent(OUT) :: end_azimuth
     Real, Intent(OUT) :: inner_limit
     Real, Intent(OUT) :: outer_limit
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_CORRECTION (dc_correction, dc_file, ff_correction, &
       ff_file, ff_scale, ff_scaler, sd_correction, sd_file, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import/Export:
     Logical, Intent(INOUT) :: dc_correction
     Character(Len = *), Intent(INOUT) :: dc_file
     Logical, Intent(INOUT) :: ff_correction
     Character(Len = *), Intent(INOUT) :: ff_file
     Logical, Intent(INOUT) :: ff_scale
     Real, Intent(INOUT) :: ff_scaler
     Logical, Intent(INOUT) :: sd_correction
     Character(Len = *), Intent(INOUT) :: sd_file
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_DATASTORE (output_graphics, experiment, input_options, &
       status)
     Use IO_LIB
     Use LG_LIB
     Use GS_LIB
     Implicit None
     Include 'gs.inc'
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: output_graphics
!  Import/Export:
!  Export:
     Type(EXPERIMENTAL_DETAILS), Intent(OUT) :: experiment
     Type(INPUT_OPTIONS_STRUCTURE), Intent(OUT) :: input_options
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_DATATYPE (default, data_type, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: default
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: data_type
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_DCLUT (gui, input_file, retstat, x_cor_size, &
       y_cor_size, xmax_lut, ymax_lut, xnum_lut, ynum_lut, X_SD, Y_SD, &
       INT_REBINNED, xmaxdat, ymaxdat, xnumdat, ynumdat, xstrelm, ystrelm, &
       xendelm, yendelm, INVERSE_FF, title, dc_lut_defined, &
       flat_field_defined, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Character(Len = *), Intent(IN) :: input_file
     Integer, Intent(IN) :: xmax_lut
     Integer, Intent(IN) :: ymax_lut
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: retstat
     Real, Intent(OUT) :: x_cor_size
     Real, Intent(OUT) :: y_cor_size
     Integer, Intent(OUT) :: xnum_lut
     Integer, Intent(OUT) :: ynum_lut
     Byte, Intent(OUT) :: X_SD(xmax_lut, ymax_lut)
     Byte, Intent(OUT) :: Y_SD(xmax_lut, ymax_lut)
     Byte, Intent(OUT) :: INT_REBINNED(9, xmax_lut, ymax_lut)
     Integer, Intent(OUT) :: xnumdat
     Integer, Intent(OUT) :: ynumdat
     Integer, Intent(OUT) :: xstrelm
     Integer, Intent(OUT) :: ystrelm
     Integer, Intent(OUT) :: xendelm
     Integer, Intent(OUT) :: yendelm
     Real, Intent(OUT) :: INVERSE_FF(xmaxdat, ymaxdat)
     Character(Len = *), Intent(OUT) :: title
     Logical, Intent(OUT) :: dc_lut_defined
     Logical, Intent(OUT) :: flat_field_defined
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_DETECTOR (experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_DETECTORTILT (gui, tilt_plane_rotation, tilt_angle, &
       status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
!  Import/Export:
     Real, Intent(INOUT) :: tilt_plane_rotation
     Real, Intent(INOUT) :: tilt_angle
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_ELLIPSE (full_info, xmaxdat, ymaxdat, xstrelm, &
       ystrelm, xendelm, yendelm, X_AXIS, Y_AXIS, DATA, MASK, title, xlabel, &
       ylabel, zlabel, experiment, retstat, &
       radius1, radius2, angle1, half_search_distance, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: full_info
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
     Integer, Intent(OUT) :: retstat
     Real, Intent(OUT) :: radius1
     Real, Intent(OUT) :: radius2
     Real, Intent(OUT) :: angle1
     Real, Intent(OUT) :: half_search_distance
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_ID06_AZIMUTHS (azimuth_start, azimuth_end, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     Real, Intent(INOUT) :: azimuth_start
     Real, Intent(INOUT) :: azimuth_end
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_ID06_CAKE (xmaxdat, ymaxdat, xnumdat, ynumdat, &
       DATA, MASK, X_AXIS, &
       Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
       zlabel, azimuth_start, azimuth_end, experiment, &
       start_azimuth, end_azimuth, inner_limit, outer_limit, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Real, Intent(IN) :: azimuth_start
     Real, Intent(IN) :: azimuth_end
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
     Real, Intent(OUT) :: start_azimuth
     Real, Intent(OUT) :: end_azimuth
     Real, Intent(OUT) :: inner_limit
     Real, Intent(OUT) :: outer_limit
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_NUMVECTOR (results, vec_num, status)
     Use IO_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'gs_constants.inc'
     Include 'st_symbols.inc'
!  Import:
     Type(RESULT_VECTORS), Intent(IN) :: results
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: vec_num
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_PARAMETERS (xmaxdat, ymaxdat, max_parameters, &
       max_results, xnumdat, ynumdat, MASK, PARAMS, PARAM_INFO, CONSTRAINTS, &
       PARNAMES, SCALE_FACTORS, RESNAMES, num_features, num_parameters, &
       num_results, x_order, y_order, xmin_poly, ymin_poly, xmax_poly, &
       ymax_poly, weighted_fit, alpha, itsperpar, evolve, disfreq, fastdis, &
       haltcrit, haltval, parexist, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: max_results
!  Export:
     Integer, Intent(OUT) :: xnumdat
     Integer, Intent(OUT) :: ynumdat
     Logical*1, Intent(OUT) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: PARAMS(max_parameters)
     Integer, Intent(OUT) :: PARAM_INFO(max_parameters)
     Logical, Intent(OUT) :: CONSTRAINTS(max_parameters)
     Character(Len = *), Intent(OUT) :: PARNAMES(max_parameters)
     Real, Intent(OUT) :: SCALE_FACTORS(max_parameters)
     Character(Len = *), Intent(OUT) :: RESNAMES(max_results)
     Integer, Intent(OUT) :: num_features
     Integer, Intent(OUT) :: num_parameters
     Integer, Intent(OUT) :: num_results
     Integer, Intent(OUT) :: x_order
     Integer, Intent(OUT) :: y_order
     Real, Intent(OUT) :: xmin_poly
     Real, Intent(OUT) :: ymin_poly
     Real, Intent(OUT) :: xmax_poly
     Real, Intent(OUT) :: ymax_poly
     Logical, Intent(OUT) :: weighted_fit
     Real, Intent(OUT) :: alpha
     Real, Intent(OUT) :: itsperpar
     Logical, Intent(OUT) :: evolve
     Integer, Intent(OUT) :: disfreq
     Logical, Intent(OUT) :: fastdis
     Integer, Intent(OUT) :: haltcrit
     Real, Intent(OUT) :: haltval
     Logical, Intent(OUT) :: parexist
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_PIXELSIZES (gui, x_pixel_size, y_pixel_size, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
!  Import/Export:
     Real, Intent(INOUT) :: x_pixel_size
     Real, Intent(INOUT) :: y_pixel_size
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_POLARISATION (correct_polarisation, polarisation, &
       lorentz_geometry, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
     Logical, Intent(INOUT) :: correct_polarisation
     Real, Intent(INOUT) :: polarisation
     Integer, Intent(INOUT) :: lorentz_geometry
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_PRESSURE (pressure, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
     Double Precision, Intent(INOUT) :: pressure
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_PROJECTION (mask_data, gisaxs, x_beam, y_beam, &
       xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, yendelm, X_AXIS, Y_AXIS, &
       DATA, MASK, title, xlabel, ylabel, zlabel, x_pixel_size, y_pixel_size, &
       X_REGION, Y_REGION, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: mask_data
     Logical, Intent(IN) :: gisaxs
     Real, Intent(IN) :: x_beam
     Real, Intent(IN) :: y_beam
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: X_REGION(5)
     Real, Intent(OUT) :: Y_REGION(5)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_SAMPLEDISTANCE (gui, sample_distance, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
!  Import/Export:
     Real, Intent(INOUT) :: sample_distance
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INP_WAVELENGTH (gui, wavelength, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
!  Import/Export:
     Real, Intent(INOUT) :: wavelength
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INPRADIA (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, MASK, X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
       full_info, x_pixel_size, y_pixel_size, x_beam, y_beam, radius1, &
       radius2, angle1, half_search_distance, Max_radia, num_radia, RADIA, &
       status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: full_info
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
     Real, Intent(IN) :: x_beam
     Real, Intent(IN) :: y_beam
     Real, Intent(IN) :: radius1
     Real, Intent(IN) :: radius2
     Real, Intent(IN) :: angle1
     Real, Intent(IN) :: half_search_distance
     Integer, Intent(IN) :: Max_radia
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: num_radia
     Real, Intent(OUT) :: RADIA(Max_radia)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INPUT_EXPERIMENT (input_options, experiment, status)
     Use IO_LIB
     Use MA_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INPUT_OPTIONS (INPUT_OPTIONS, status)
     Use IO_LIB
     Use MA_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     TYPE(INPUT_OPTIONS_STRUCTURE), Intent(INOUT) :: INPUT_OPTIONS
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INPUTMODEL (EXPERIMENT, xmaxdat, ymaxdat, &
       xnumdat, ynumdat, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, zlabel, &
       xmaxcoeff, ymaxcoeff, maxpeak, maxorders, xstrelm, ystrelm, xendelm, &
       yendelm, MASK, RESIDUALS, polynomial_defined, x_order, y_order, &
       xmin_poly, ymin_poly, xmax_poly, ymax_poly, COEFFICIENTS, xpolcen, &
       ypolcen, numpeak, PEAKTYPE, XPEAKCEN, YPEAKCEN, PEAKMAX, ORIENT, &
       MAJHWHM, MINHWHM, TWINMAX, rowline_defined, loworder, highorder, &
       xrowcen, yrowcen, rowline_angle, ppangle, ppashift, ratio, sigradius, &
       sigtheta, INTENSITIES, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: EXPERIMENT
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Integer, Intent(IN) :: xmaxcoeff
     Integer, Intent(IN) :: ymaxcoeff
     Integer, Intent(IN) :: maxpeak
     Integer, Intent(IN) :: maxorders
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: RESIDUALS(xmaxdat, ymaxdat)
!  Export:
     Logical, Intent(OUT) :: polynomial_defined
     Integer, Intent(OUT) :: x_order
     Integer, Intent(OUT) :: y_order
     Real, Intent(OUT) :: xmin_poly
     Real, Intent(OUT) :: ymin_poly
     Real, Intent(OUT) :: xmax_poly
     Real, Intent(OUT) :: ymax_poly
     Real, Intent(OUT) :: COEFFICIENTS(ymaxcoeff, xmaxcoeff)
     Real, Intent(OUT) :: xpolcen
     Real, Intent(OUT) :: ypolcen
     Integer, Intent(OUT) :: numpeak
     Integer, Intent(OUT) :: PEAKTYPE(maxpeak)
     Real, Intent(OUT) :: XPEAKCEN(maxpeak)
     Real, Intent(OUT) :: YPEAKCEN(maxpeak)
     Real, Intent(OUT) :: PEAKMAX(maxpeak)
     Real, Intent(OUT) :: ORIENT(maxpeak)
     Real, Intent(OUT) :: MAJHWHM(maxpeak)
     Real, Intent(OUT) :: MINHWHM(maxpeak)
     Real, Intent(OUT) :: TWINMAX(maxpeak)
     Logical, Intent(OUT) :: rowline_defined
     Integer, Intent(OUT) :: loworder
     Integer, Intent(OUT) :: highorder
     Real, Intent(OUT) :: xrowcen
     Real, Intent(OUT) :: yrowcen
     Real, Intent(OUT) :: rowline_angle
     Real, Intent(OUT) :: ppangle
     Real, Intent(OUT) :: ppashift
     Real, Intent(OUT) :: ratio
     Real, Intent(OUT) :: sigradius
     Real, Intent(OUT) :: sigtheta
     Real, Intent(OUT) :: INTENSITIES(-maxorders: maxorders)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INPUTROWLINE (EXPERIMENT, xmaxdat, &
       ymaxdat, xstrelm, ystrelm, xendelm, yendelm, XAXIS, YAXIS, DATA, &
       maxorders, MODEL, loworder, highorder, x_centre, y_centre, xrowcen, &
       yrowcen, rowline_angle, ppangle, ppashift, ratio, sigradius, sigtheta, &
       INTENSITIES, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     TYPE(EXPERIMENTAL_DETAILS), Intent(INOUT) :: EXPERIMENT
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: maxorders
!  Import/Export:
     Real, Intent(INOUT) :: MODEL(xmaxdat, ymaxdat)
!  Export:
     Integer, Intent(OUT) :: loworder
     Integer, Intent(OUT) :: highorder
     Real, Intent(OUT) :: x_centre
     Real, Intent(OUT) :: y_centre
     Real, Intent(OUT) :: xrowcen
     Real, Intent(OUT) :: yrowcen
     Real, Intent(OUT) :: rowline_angle
     Real, Intent(OUT) :: ppangle
     Real, Intent(OUT) :: ppashift
     Real, Intent(OUT) :: ratio
     Real, Intent(OUT) :: sigradius
     Real, Intent(OUT) :: sigtheta
     Real, Intent(OUT) :: INTENSITIES(-maxorders: maxorders)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INQ_EXPERIMENT (experiment, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'io.inc'
!  Import:
!  Import/Export:
!  Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INQ_INTEGRATE (input_file, xmaxdat, ymaxdat, &
       xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, experiment, &
       outer_limit, scan_type, maximum_d, conserve_intensity, &
       correct_geometry, outer_angle, &
       num_2theta, lorentz_geometry, rad_pixel_size, outer_q, &
       save_parameters, parameter_file, use_lut, correct_parallax, status)
     Use IO_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Character(Len = *), Intent(IN) :: input_file
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: lorentz_geometry
     Real, Intent(INOUT) :: outer_limit
     Integer, Intent(INOUT) :: scan_type
     Real, Intent(INOUT) :: maximum_d
     Logical, Intent(INOUT) :: conserve_intensity
     Logical, Intent(INOUT) :: correct_geometry
!  Export:
     Real, Intent(OUT) :: rad_pixel_size
     Real, Intent(OUT) :: outer_angle
     Integer, Intent(OUT) :: num_2theta
     Real, Intent(OUT) :: outer_q
     Logical, Intent(OUT) :: save_parameters
     Character(Len = *), Intent(OUT) :: parameter_file
     Logical, Intent(OUT) :: use_lut
     Logical, Intent(OUT) :: correct_parallax
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INTEGRATE (input_file, xmaxdat, ymaxdat, data_defined, &
       title, xlabel, &
       ylabel, zlabel, variances_exist, xnumdat, ynumdat, X_AXIS, Y_AXIS, &
       DATA, MASK, xstrelm, ystrelm, xendelm, yendelm, experiment, &
       lorentz_geometry, &
       memory_defined, mtitle, mxlabel, mylabel, mzlabel, mxnumdat, mynumdat, &
       MX_AXIS, MY_AXIS, MDATA, mxstrelm, mystrelm, mxendelm, myendelm, &
       mx_pixel_size, my_pixel_size, status)
     Use IO_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Character(Len = *), Intent(IN) :: input_file
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Logical, Intent(INOUT) :: variances_exist
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Real, Intent(INOUT) :: X_AXIS(xmaxdat)
     Real, Intent(INOUT) :: Y_AXIS(ymaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: lorentz_geometry
!  Export:
     Logical, Intent(OUT) :: memory_defined
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MX_AXIS(xmaxdat)
     Real, Intent(OUT) :: MY_AXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_INTERNALMEMORY (xmaxdat, ymaxdat, variance_arrays, title, &
       data_exist, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
       XAXIS, YAXIS, DATA, VARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'f2d_internal.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Logical, Intent(IN) :: variance_arrays
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: title
     Logical, Intent(INOUT) :: data_exist
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Real, Intent(INOUT) :: XAXIS(xmaxdat)
     Real, Intent(INOUT) :: YAXIS(ymaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_KEYBOARD (fit2d_version, gui, output_graphics, &
       shared_memory, memory_id, input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, log_file_open, xmaxdat, &
       ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       input_options, xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, &
       mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, &
       mzlabel, mx_pixel_size, my_pixel_size, results, status)
     Use MA_LIB
     Use LG_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'io_db.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Character(Len = *), Intent(IN) :: fit2d_version
     Logical, Intent(IN) :: gui
     Logical, Intent(IN) :: output_graphics
     Logical, Intent(IN) :: shared_memory
     Integer, Intent(IN) :: memory_id
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variance_exist
     Logical, Intent(INOUT) :: mask_exist
     Logical, Intent(INOUT) :: log_file_open
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Type(INPUT_OPTIONS_STRUCTURE), Intent(INOUT) :: input_options
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Integer, Intent(INOUT) :: mxnumdat
     Integer, Intent(INOUT) :: mynumdat
     Integer, Intent(INOUT) :: mxendelm
     Integer, Intent(INOUT) :: mxstrelm
     Integer, Intent(INOUT) :: myendelm
     Integer, Intent(INOUT) :: mystrelm
     Character(Len = *), Intent(INOUT) :: mtitle
     Character(Len = *), Intent(INOUT) :: mxlabel
     Character(Len = *), Intent(INOUT) :: mylabel
     Character(Len = *), Intent(INOUT) :: mzlabel
     Real, Intent(INOUT) :: mx_pixel_size
     Real, Intent(INOUT) :: my_pixel_size
     Type(RESULT_VECTORS), Intent(INOUT) :: results
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_KEYSUB (max_menu, num_menu, MENUTXT, fit2d_version, gui, &
       output_graphics, shared_memory, memory_id, command, display_command, &
       print_type, input_file, data_defined, memory_exist, memory_defined, &
       variance_exist, mask_exist, log_file_open, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, input_options, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, results, warning, continue, status)
     Implicit None
     Include 'gs.inc'
     Include 'st_symbols.inc'
     Include 'io_db.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Integer, Intent(IN) :: max_menu
     Integer, Intent(IN) :: num_menu
     Character(Len = *), Intent(IN) :: MENUTXT(Max_menu)
     Character(Len = *), Intent(IN) :: fit2d_version
     Logical, Intent(IN) :: gui
     Logical, Intent(IN) :: output_graphics
     Logical, Intent(IN) :: shared_memory
     Integer, Intent(IN) :: memory_id
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: command
     Character(Len = *), Intent(INOUT) :: display_command
     Character(Len = *), Intent(INOUT) :: print_type
     Character(Len = *), Intent(INOUT) :: input_file
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: memory_exist
     Logical, Intent(INOUT) :: memory_defined
     Logical, Intent(INOUT) :: variance_exist
     Logical, Intent(INOUT) :: mask_exist
     Logical, Intent(INOUT) :: log_file_open
     Integer, Intent(INOUT) :: xmaxdat
     Integer, Intent(INOUT) :: ymaxdat
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Type(INPUT_OPTIONS_STRUCTURE), Intent(INOUT) :: input_options
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Integer, Intent(INOUT) :: mxnumdat
     Integer, Intent(INOUT) :: mynumdat
     Integer, Intent(INOUT) :: mxendelm
     Integer, Intent(INOUT) :: mxstrelm
     Integer, Intent(INOUT) :: myendelm
     Integer, Intent(INOUT) :: mystrelm
     Character(Len = *), Intent(INOUT) :: mtitle
     Character(Len = *), Intent(INOUT) :: mxlabel
     Character(Len = *), Intent(INOUT) :: mylabel
     Character(Len = *), Intent(INOUT) :: mzlabel
     Real, Intent(INOUT) :: mx_pixel_size
     Real, Intent(INOUT) :: my_pixel_size
     Type(RESULT_VECTORS), Intent(INOUT) :: results
!  Export:
     Integer, Intent(OUT) :: warning
     Logical, Intent(OUT) :: continue
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LEARNPROFILE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, DATA, xmax_peaks, ymax_peaks, xnum_peaks, &
       ynum_peaks, X_2DPEAKS, Y_2DPEAKS, mxnumdat, mynumdat, MXAXIS, MYAXIS, &
       MDATA, mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, &
       mylabel, mzlabel, memory_exist, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(xmaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: xnum_peaks
     Integer, Intent(IN) :: ynum_peaks
     Real, Intent(IN) :: X_2DPEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_2DPEAKS(xmax_peaks, ymax_peaks)
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Logical, Intent(OUT) :: memory_exist
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LINEARISE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, VARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LISTVARIABLES (status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LOADGEOMETRY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, x_pixel_size, y_pixel_size, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Real, Intent(INOUT) :: x_pixel_size
     Real, Intent(INOUT) :: y_pixel_size
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LOADMODEL (max_parameters, params_exist, PARAMS, &
       PARAM_INFO, CONSTRAINTS, PARNAMES, SCALE_FACTORS, num_features, &
       num_parameters, x_order, y_order, xmin_poly, ymin_poly, xmax_poly, &
       ymax_poly, weighted_fit, alpha, itsperpar, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: max_parameters
!  Export:
     Logical, Intent(OUT) :: params_exist
     Real, Intent(OUT) :: PARAMS(max_parameters)
     Integer, Intent(OUT) :: PARAM_INFO(max_parameters)
     Logical, Intent(OUT) :: CONSTRAINTS(max_parameters)
     Character(Len = *), Intent(OUT) :: PARNAMES(max_parameters)
     Real, Intent(OUT) :: SCALE_FACTORS(max_parameters)
     Integer, Intent(OUT) :: num_features
     Integer, Intent(OUT) :: num_parameters
     Integer, Intent(OUT) :: x_order
     Integer, Intent(OUT) :: y_order
     Real, Intent(OUT) :: xmin_poly
     Real, Intent(OUT) :: ymin_poly
     Real, Intent(OUT) :: xmax_poly
     Real, Intent(OUT) :: ymax_poly
     Logical, Intent(OUT) :: weighted_fit
     Real, Intent(OUT) :: alpha
     Real, Intent(OUT) :: itsperpar
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LOGARITHM (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, variances_exist, MASK, DATA, VARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
    Subroutine F2D_LOGO (gui, status)
    Implicit None
! Import:
    Logical gui
! Status:
    Integer status
End Subroutine
 
     Subroutine F2D_LORENTZ (lorentz_geometry, variances_exist, Max_scan, &
       num_scan, ANGLES, SCAN, VARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: lorentz_geometry
     Logical, Intent(IN) :: variances_exist
     Integer, Intent(IN) :: max_scan
     Integer, Intent(IN) :: num_scan
!  Import/Export:
     Real, Intent(INOUT) :: ANGLES(max_scan)
     Real, Intent(INOUT) :: SCAN(max_scan)
     Real, Intent(INOUT) :: VARIANCES(max_scan)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LSQ1DSYMMETRY (maxdat, strelm, endelm, DATA, MASK, &
       x_symmetry, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: maxdat
     Integer, Intent(IN) :: strelm
     Integer, Intent(IN) :: endelm
     Real, Intent(IN) :: DATA(maxdat)
     Logical*1, Intent(IN) :: MASK(maxdat)
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: x_symmetry
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LSQCALIBRANT (full_info, max_rings, num_rings, D_SPACINGS, &
       weighted_fitting, refine_beam_centre, &
       refine_sample_distance, refine_wavelength, refine_tilt, &
       experiment, radial_error, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc'
     Include 'f2d_lsqpowder.inc'
!  Import:
     Logical, Intent(IN) :: full_info
     Integer, Intent(IN) :: max_rings
     Integer, Intent(IN) :: num_rings
     Real, Intent(IN) :: D_SPACINGS(max_rings)
     Logical, Intent(IN) :: weighted_fitting
     Logical, Intent(IN) :: refine_beam_centre
     Logical, Intent(IN) :: refine_sample_distance
     Logical, Intent(IN) :: refine_wavelength
     Logical, Intent(IN) :: refine_tilt
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
     Real, Intent(OUT) :: radial_error
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LSQCIRCLE (max_coordinates, num_coordinates, &
       X_COORDINATES, Y_COORDINATES, x_centre, y_centre, radius, radial_error, &
       status)
     Use IO_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc'
!  Import:
     Integer, Intent(IN) :: Max_coordinates
     Integer, Intent(IN) :: num_coordinates
     Real, Intent(IN) :: X_COORDINATES(Max_coordinates)
     Real, Intent(IN) :: Y_COORDINATES(Max_coordinates)
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: x_centre
     Real, Intent(OUT) :: y_centre
     Real, Intent(OUT) :: radius
     Real, Intent(OUT) :: radial_error
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LSQELLIPSE (max_coordinates, num_coordinates, &
       X_COORDINATES, Y_COORDINATES, x_centre, y_centre, radius1, radius2, &
       angle1, radial_error, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc'
!  Import:
     Integer, Intent(IN) :: Max_coordinates
     Integer, Intent(IN) :: num_coordinates
     Real, Intent(IN) :: X_COORDINATES(Max_coordinates)
     Real, Intent(IN) :: Y_COORDINATES(Max_coordinates)
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: x_centre
     Real, Intent(OUT) :: y_centre
     Real, Intent(OUT) :: radius1
     Real, Intent(OUT) :: radius2
     Real, Intent(OUT) :: angle1
     Real, Intent(OUT) :: radial_error
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LSQFIT2D (EXPERIMENT, xmaxdat, ymaxdat, &
       XAXIS, YAXIS, DATA, VARIANCES, MASK, xstrelm, ystrelm, xendelm, &
       yendelm, alpha, weighted_fit, itsperpar, max_parameters, &
       num_parameters, max_results, num_results, PARAM_INFO, CONSTRAINTS, &
       SCALE_FACTORS, PARAMS, PARERRORS, RESULTS, RESERRORS, RESNAMES, &
       num_fun_calls, chisqr, MODEL, COVARIANCE, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_lsqfit2d.inc'
!  Import:
     TYPE(EXPERIMENTAL_DETAILS), Intent(IN) :: EXPERIMENT
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: alpha
     Logical, Intent(IN) :: weighted_fit
     Real, Intent(IN) :: itsperpar
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: num_parameters
     Integer, Intent(IN) :: max_results
     Integer, Intent(IN) :: PARAM_INFO(Max_parameters)
     Logical, Intent(IN) :: CONSTRAINTS(Max_parameters)
     Real, Intent(IN) :: SCALE_FACTORS(Max_parameters)
!  Import/Export:
     Real, Intent(INOUT) :: PARAMS(Max_parameters)
!  Export:
     Integer, Intent(OUT) :: num_results
     Real, Intent(INOUT) :: PARERRORS(max_parameters)
     Real, Intent(OUT) :: RESULTS(max_results)
     Real, Intent(OUT) :: RESERRORS (max_results)
     Character(Len = *), Intent(OUT) :: RESNAMES(max_results)
     Integer, Intent(OUT) :: num_fun_calls
     Real, Intent(INOUT) :: chisqr
     Real, Intent(OUT) :: MODEL(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: COVARIANCE(max_parameters, max_parameters)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LSQGAUSSIAN (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, &
       xendelm, yendelm, x_centre, y_centre, intensity, sigma1, sigma2, angle, &
       background, retstat, chi_squared, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Real, Intent(INOUT) :: x_centre
     Real, Intent(INOUT) :: y_centre
     Real, Intent(INOUT) :: intensity
     Real, Intent(INOUT) :: sigma1
     Real, Intent(INOUT) :: sigma2
     Real, Intent(INOUT) :: angle
     Real, Intent(INOUT) :: background
!  Export:
     Integer, Intent(OUT) :: retstat
     Real, Intent(OUT) :: chi_squared
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LSQMFIT ( &
       y_row, x_pixel_size, &
       variances_exist, maxdat, AXIS, DATA, VARIANCES, MASK, strelm, endelm, &
       title, x_label, y_label, alpha, fitting_info, weighted_fit, itsperpar, &
       display, max_parameters, num_parameters, num_1dfeatures, max_results, &
       max_text, num_results, PARNAMES, PARAM_INFO, CONSTRAINTS, &
       SCALE_FACTORS, PARAMS, PARERRORS, results_exist, FRESULTS, RESERRORS, &
       RESNAMES, num_fun_calls, chisqr, MODEL, COVARIANCE, num_text, TEXT, &
       print_type, WEIGHTS, results, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_lsqmfit.inc'
!  Import:
     Integer, Intent(IN) :: y_row
     Real, Intent(IN) :: x_pixel_size
     Logical, Intent(IN) :: variances_exist
     Integer, Intent(IN) :: maxdat
     Real, Intent(IN) :: AXIS(maxdat)
     Real, Intent(IN) :: DATA(maxdat)
     Real, Intent(IN) :: VARIANCES(maxdat)
     Logical*1, Intent(IN) :: MASK(maxdat)
     Integer, Intent(IN) :: strelm
     Integer, Intent(IN) :: endelm
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: x_label
     Character(Len = *), Intent(IN) :: y_label
     Real, Intent(IN) :: alpha
     Integer, Intent(IN) :: fitting_info
     Logical, Intent(IN) :: weighted_fit
     Real, Intent(IN) :: itsperpar
     Logical, Intent(IN) :: display
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: num_parameters
     Integer, Intent(IN) :: num_1dfeatures
     Integer, Intent(IN) :: max_results
     Integer, Intent(IN) :: max_text
     Character(Len = *), Intent(IN) :: PARNAMES(max_parameters)
     Integer, Intent(IN) :: PARAM_INFO(max_parameters)
     Logical, Intent(IN) :: CONSTRAINTS(max_parameters)
     Real, Intent(IN) :: SCALE_FACTORS(max_parameters)
!  Import/Export:
     Real, Intent(INOUT) :: PARAMS(max_parameters)
!  Export:
     Integer, Intent(OUT) :: num_results
     Real, Intent(INOUT) :: PARERRORS(max_parameters)
     Logical, Intent(OUT) :: results_exist
     Real, Intent(OUT) :: FRESULTS(max_results)
     Real, Intent(INOUT) :: RESERRORS (max_results)
     Character(Len = *), Intent(INOUT) :: RESNAMES(max_results)
     Integer, Intent(OUT) :: num_fun_calls
     Real, Intent(INOUT) :: chisqr
     Real, Intent(OUT) :: MODEL(maxdat)
     Real, Intent(INOUT) :: COVARIANCE(max_parameters, max_parameters)
     Integer, Intent(OUT) :: num_text
     Character(Len = *), Intent(OUT) :: TEXT(max_text)
     Character(Len = *), Intent(OUT) :: print_type
     Real, Intent(OUT) :: WEIGHTS(endelm)
     Type(RESULT_VECTORS), Intent(INOUT) :: results
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LSQPOLARISATION (sample_distance, max_rings, num_rings, &
       max_rcoordinates, polarisation_factor, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc'
     Include 'f2d_lsqpowder.inc'
!  Import:
     Real, Intent(IN) :: sample_distance
     Integer, Intent(IN) :: max_rings
     Integer, Intent(IN) :: num_rings
     Integer, Intent(IN) :: max_rcoordinates
!  Import/Export:
     Real, Intent(INOUT) :: polarisation_factor
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_LSQPOWDER (full_info, max_angles, num_rings, &
       weighted_fitting, detector_gain, refine_beam_centre, &
       refine_sample_distance, refine_tilt, sample_distance, x_beam, y_beam, &
       ANGLE_CONES, tilt_plane_rotation, tilt_angle, radial_error, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc'
     Include 'f2d_lsqpowder.inc'
!  Import:
     Logical, Intent(IN) :: full_info
     Integer, Intent(IN) :: max_angles
     Integer, Intent(IN) :: num_rings
     Logical, Intent(IN) :: weighted_fitting
     Real, Intent(IN) :: detector_gain
     Logical, Intent(IN) :: refine_beam_centre
     Logical, Intent(IN) :: refine_sample_distance
     Logical, Intent(IN) :: refine_tilt
!  Import/Export:
     Real, Intent(INOUT) :: sample_distance
     Real, Intent(INOUT) :: x_beam
     Real, Intent(INOUT) :: y_beam
     Real, Intent(INOUT) :: ANGLE_CONES(max_angles)
     Real, Intent(INOUT) :: tilt_plane_rotation
     Real, Intent(INOUT) :: tilt_angle
!  Export:
     Real, Intent(OUT) :: radial_error
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MAIN
!      Result: the Mar file header appears to be incorrect. It's coded for
     Use IO_LIB
     Use LG_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
!  Status:
End Subroutine
 
     Subroutine F2D_MASK (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, &
       X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, MASK, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MASKARC (xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, xstrelm, &
       ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, MASK, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MASKCOLOUR (status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MASKGROW (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, MASK, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MASKINVERT (xmaxdat, ymaxdat, xnumdat, ynumdat, xstrelm, &
       ystrelm, xendelm, yendelm, MASK, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MASKMEMORY (title, xlabel, ylabel, zlabel, xmaxdat, &
       ymaxdat, xnumdat, ynumdat, XAXIS, YAXIS, MASK, mtitle, mxlabel, &
       mylabel, mzlabel, mxstrelm, mystrelm, mxendelm, myendelm, mxnumdat, &
       mynumdat, MXAXIS, MYAXIS, MEMORY, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
!  Import/Export:
!  Export:
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MEMORY(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MASKPEAKS (xmaxdat, ymaxdat, X_AXIS, Y_AXIS, DATA, &
       xstrelm, ystrelm, xendelm, yendelm, xnumdat, ynumdat, mask_radius, &
       title, xlabel, ylabel, zlabel, MASK, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: mask_radius
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MASKPOLYGON (xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, &
       xstrelm, ystrelm, xendelm, yendelm, mask_data, title, xlabel, ylabel, &
       zlabel, MASK, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: mask_data
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MASKSIGNIFICANT (xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MASKSTATS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, MASK, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MASKTHRESHOLD (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MASK, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MEDIANFILTER (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MDATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
!  Export:
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MESSAGE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'io_db.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MFITDISPLAY (pixel_size, variances_exist, xmaxdat, &
       ymaxdat, AXIS, DATA, VARIANCES, MASK, MODEL, strelm, endelm, y_row, &
       str_optimised, end_optimised, title, x_label, y_label, max_parameters, &
       num_parameters, num_1dfeatures, PARAM_INFO, PARAMS, poly_order, &
       min_poly, max_poly, max_text, num_text, TEXT, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: pixel_size
     Logical, Intent(IN) :: variances_exist
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: AXIS(xmaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: MODEL(xmaxdat)
     Integer, Intent(IN) :: strelm
     Integer, Intent(IN) :: endelm
     Integer, Intent(IN) :: str_optimised
     Integer, Intent(IN) :: end_optimised
     Integer, Intent(IN) :: y_row
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: x_label
     Character(Len = *), Intent(IN) :: y_label
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: num_parameters
     Integer, Intent(IN) :: num_1dfeatures
     Integer, Intent(IN) :: PARAM_INFO(max_parameters)
     Real, Intent(IN) :: PARAMS(max_parameters)
     Integer, Intent(IN) :: poly_order
     Real, Intent(IN) :: min_poly
     Real, Intent(IN) :: max_poly
     Integer, Intent(IN) :: max_text
     Integer, Intent(IN) :: num_text
     Character(Len = *), Intent(IN) :: TEXT(max_text)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SUBMFITDISPLAY (pixel_size, variances_exist, maxdat, AXIS, &
       DATA, VARIANCES, MASK, MODEL, strelm, endelm, str_optimised, &
       end_optimised, title, x_label, y_label, max_parameters, num_parameters, &
       num_1dfeatures, PARAM_INFO, PARAMS, poly_order, min_poly, max_poly, &
       max_text, num_text, TEXT, WORK, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: pixel_size
     Logical, Intent(IN) :: variances_exist
     Integer, Intent(IN) :: maxdat
     Real, Intent(IN) :: AXIS(maxdat)
     Real, Intent(IN) :: DATA(maxdat)
     Real, Intent(IN) :: VARIANCES(maxdat)
     Logical*1, Intent(IN) :: MASK(maxdat)
     Real, Intent(IN) :: MODEL(maxdat)
     Integer, Intent(IN) :: strelm
     Integer, Intent(IN) :: endelm
     Integer, Intent(IN) :: str_optimised
     Integer, Intent(IN) :: end_optimised
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: x_label
     Character(Len = *), Intent(IN) :: y_label
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: num_parameters
     Integer, Intent(IN) :: num_1dfeatures
     Integer, Intent(IN) :: PARAM_INFO(max_parameters)
     Real, Intent(IN) :: PARAMS(max_parameters)
     Integer, Intent(IN) :: poly_order
     Real, Intent(IN) :: min_poly
     Real, Intent(IN) :: max_poly
     Integer, Intent(IN) :: max_text
     Integer, Intent(IN) :: num_text
     Character(Len = *), Intent(IN) :: TEXT(max_text)
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: WORK(endelm)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MFITOPTIMISE ( &
       x_pixel_size, &
       variances_exist, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, VARIANCES, MASK, &
       xstrelm, xendelm, ystrelm, yendelm, y_start, title, x_label, y_label, &
       alpha, fitting_info, weighted_fit, itsperpar, display_frequency, &
       model_evolution, max_parameters, num_parameters, num_1dfeatures, &
       max_results, max_text, num_results, PARNAMES, PARAM_INFO, CONSTRAINTS, &
       SCALE_FACTORS, PAR_BASE, PARAMS, PARERRORS, results_exist, FRESULTS, &
       RESERRORS, RESNAMES, num_fun_calls, chisqr, mxnumdat, mynumdat, &
       mxstrelm, mystrelm, mxendelm, myendelm, MXAXIS, MYAXIS, MODEL, &
       COVARIANCE, num_text, TEXT, print_type, results, status)
     Use IO_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: x_pixel_size
     Logical, Intent(IN) :: variances_exist
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(xmaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: y_start
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: x_label
     Character(Len = *), Intent(IN) :: y_label
     Real, Intent(IN) :: alpha
     Integer, Intent(IN) :: fitting_info
     Logical, Intent(IN) :: weighted_fit
     Real, Intent(IN) :: itsperpar
     Integer, Intent(IN) :: display_frequency
     Logical, Intent(IN) :: model_evolution
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: num_parameters
     Integer, Intent(IN) :: num_1dfeatures
     Integer, Intent(IN) :: max_results
     Integer, Intent(IN) :: max_text
     Integer, Intent(IN) :: num_results
     Character(Len = *), Intent(IN) :: PARNAMES(max_parameters)
     Integer, Intent(IN) :: PARAM_INFO(max_parameters)
     Logical, Intent(IN) :: CONSTRAINTS(max_parameters)
     Real, Intent(IN) :: SCALE_FACTORS(max_parameters)
!  Import/Export:
     Real, Intent(INOUT) :: PAR_BASE(max_parameters)
!  Export:
     Real, Intent(OUT) :: PARAMS(max_parameters)
     Real, Intent(OUT) :: PARERRORS(max_parameters)
     Logical, Intent(OUT) :: results_exist
     Real, Intent(OUT) :: FRESULTS(max_results)
     Real, Intent(OUT) :: RESERRORS (max_results)
     Character(Len = *), Intent(OUT) :: RESNAMES(max_results)
     Integer, Intent(OUT) :: num_fun_calls
     Real, Intent(OUT) :: chisqr
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: mystrelm
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: myendelm
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(xmaxdat)
     Real, Intent(OUT) :: MODEL(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: COVARIANCE(max_parameters, max_parameters)
     Integer, Intent(OUT) :: num_text
     Character(Len = *), Intent(OUT) :: TEXT(max_text)
     Character(Len = *), Intent(OUT) :: print_type
     Type(RESULT_VECTORS), Intent(INOUT) :: results
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MFITSETUP (variances_exist, alpha, fitting_info, &
       itsperpar, display_frequency, weighted_fit, model_evolution, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: variances_exist
!  Import/Export:
     Real, Intent(INOUT) :: alpha
     Integer, Intent(INOUT) :: fitting_info
     Real, Intent(INOUT) :: itsperpar
     Integer, Intent(INOUT) :: display_frequency
     Logical, Intent(INOUT) :: weighted_fit
     Logical, Intent(INOUT) :: model_evolution
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MOVE (xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, VARIANCES, &
       xstrelm, ystrelm, xendelm, yendelm, variances_exist, x_pixel_size, &
       y_pixel_size, OUTXAXIS, OUTYAXIS, OUTPUT, OUTVARIANCES, xstrout, &
       ystrout, xendout, yendout, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat,ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat,ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
!  Export:
     Real, Intent(OUT) :: OUTXAXIS(xmaxdat)
     Real, Intent(OUT) :: OUTYAXIS(ymaxdat)
     Real, Intent(OUT) :: OUTPUT(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: OUTVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: xstrout
     Integer, Intent(OUT) :: ystrout
     Integer, Intent(OUT) :: xendout
     Integer, Intent(OUT) :: yendout
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_MSET_OPTIONS (fit2d_version, xmaxdat, ymaxdat, &
       data_defined, memory_exist, variance_arrays, output_graphics, gui, &
       shared_memory, memory_id, graphics_macro_mode, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Character(Len = *), Intent(IN) :: fit2d_version
!  Export:
     Integer, Intent(OUT) :: xmaxdat
     Integer, Intent(OUT) :: ymaxdat
     Logical, Intent(OUT) :: data_defined
     Logical, Intent(OUT) :: memory_exist
     Logical, Intent(OUT) :: variance_arrays
     Logical, Intent(OUT) :: output_graphics
     Logical, Intent(OUT) :: gui
     Logical, Intent(OUT) :: shared_memory
     Integer, Intent(OUT) :: memory_id
     Logical, Intent(OUT) :: graphics_macro_mode
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_NORMALISE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, MASK, DATA, VARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_NUMBERS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, zlabel, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_OPEN_INMACRO (inmacro_file, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: inmacro_file
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_OPEN_OUTMACRO (outmacro_file, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: outmacro_file
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_OPEN_LOG (status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_OPTIONS (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, zlabel, experiment, &
       variances_exist, xstrelm, ystrelm, xendelm, yendelm, &
       print_type, status)
     Implicit None
     Include 'gs.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Logical, Intent(IN) :: variances_exist
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Character(Len = *), Intent(INOUT) :: print_type
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_OFFSET (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, VARIANCES, MDATA, MVARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Real, Intent(IN) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: MVARIANCES(xmaxdat, ymaxdat)
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_OUT_DCLUT (gui, input_file, x_cor_size, y_cor_size, &
       xmax_lut, ymax_lut, xnum_lut, ynum_lut, X_SD, Y_SD, INT_REBINNED, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, retstat, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Character(Len = *), Intent(IN) :: input_file
     Real, Intent(IN) :: x_cor_size
     Real, Intent(IN) :: y_cor_size
     Integer, Intent(IN) :: xmax_lut
     Integer, Intent(IN) :: ymax_lut
     Integer, Intent(IN) :: xnum_lut
     Integer, Intent(IN) :: ynum_lut
     Byte, Intent(IN) :: X_SD(xmax_lut, ymax_lut)
     Byte, Intent(IN) :: Y_SD(xmax_lut, ymax_lut)
     Byte, Intent(IN) :: INT_REBINNED(9, xmax_lut, ymax_lut)
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: retstat
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SUBOUTDCLUT (file_id, max_buffer, byte_value, pos_buffer, &
       BUFFER, retstat, status)
     Implicit None
!  Import:
     Integer, Intent(IN) :: file_id
     Integer, Intent(IN) :: max_buffer
     Byte, Intent(IN) :: byte_value
!  Import/Export:
     Integer, Intent(INOUT) :: pos_buffer
     Byte, Intent(INOUT) :: BUFFER(max_buffer)
!  Export:
     Integer, Intent(OUT) :: retstat
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_OUT_PARAMETERS (xmaxdat, ymaxdat, xnumdat, ynumdat, MASK, &
       max_parameters, max_results, PARAMS, PARAM_INFO, CONSTRAINTS, PARNAMES, &
       SCALE_FACTORS, RESNAMES, num_features, num_parameters, num_results, &
       x_order, y_order, xmin_poly, ymin_poly, xmax_poly, ymax_poly, &
       weighted_fit, alpha, itsperpar, evolve, disfreq, fastdis, haltcrit, &
       haltval, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: max_results
     Real, Intent(IN) :: PARAMS(max_parameters)
     Logical, Intent(IN) :: CONSTRAINTS(max_parameters)
     Character(Len = *), Intent(IN) :: PARNAMES(max_parameters)
     Real, Intent(IN) :: SCALE_FACTORS(max_parameters)
     Character(Len = *), Intent(IN) :: RESNAMES(max_results)
     Integer, Intent(IN) :: num_features
     Integer, Intent(IN) :: num_parameters
     Integer, Intent(IN) :: num_results
     Integer, Intent(IN) :: x_order
     Integer, Intent(IN) :: y_order
     Real, Intent(IN) :: xmin_poly
     Real, Intent(IN) :: ymin_poly
     Real, Intent(IN) :: xmax_poly
     Real, Intent(IN) :: ymax_poly
     Logical, Intent(IN) :: weighted_fit
     Real, Intent(IN) :: alpha
     Real, Intent(IN) :: itsperpar
     Logical, Intent(IN) :: evolve
     Integer, Intent(IN) :: disfreq
     Logical, Intent(IN) :: fastdis
     Integer, Intent(IN) :: haltcrit
     Real, Intent(IN) :: haltval
!  Import/Export:
     Integer, Intent(INOUT) :: PARAM_INFO(max_parameters)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_OUT_SPATIAL (distorted_spline, x_min, y_min, x_max, y_max, &
       cor_grid_spacing, x_cor_size, y_cor_size, xmaxknots, ymaxknots, &
       x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, &
       y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: distorted_spline
     Real, Intent(IN) :: x_min
     Real, Intent(IN) :: y_min
     Real, Intent(IN) :: x_max
     Real, Intent(IN) :: y_max
     Real, Intent(IN) :: cor_grid_spacing
     Real, Intent(IN) :: x_cor_size
     Real, Intent(IN) :: y_cor_size
     Integer, Intent(IN) :: xmaxknots
     Integer, Intent(IN) :: ymaxknots
     Integer, Intent(IN) :: x_xnumknots
     Integer, Intent(IN) :: x_ynumknots
     Real, Intent(IN) :: X_LAMBDA(xmaxknots)
     Real, Intent(IN) :: X_MU(ymaxknots)
     Real, Intent(IN) :: X_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
     Integer, Intent(IN) :: y_xnumknots
     Integer, Intent(IN) :: y_ynumknots
     Real, Intent(IN) :: Y_LAMBDA(xmaxknots)
     Real, Intent(IN) :: Y_MU(ymaxknots)
     Real, Intent(IN) :: Y_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PARALLAX_DSPACINGS (experiment, max_angles, &
       num_cali_rings, D_SPACINGS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fitrings.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(IN) :: max_angles
     Integer :: num_cali_rings
!  Import/Export:
     Real, Intent(INOUT) :: D_SPACINGS(Max_angles)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PARAMETERS (experiment, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, YAXIS, DATA, VARIANCES, &
       title, xlabel, ylabel, zlabel, max_parameters, max_results, xstrelm, &
       ystrelm, xendelm, yendelm, MASK, PARAMS, PARAM_INFO, CONSTRAINTS, &
       PARNAMES, SCALE_FACTORS, RESNAMES, num_features, num_parameters, &
       num_results, x_order, y_order, xmin_poly, ymin_poly, xmax_poly, &
       ymax_poly, parexist, weighted_fit, alpha, itsperpar, evolve, disfreq, &
       fastdis, haltcrit, haltval, MODEL, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: max_results
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
!  Export:
     Real, Intent(OUT) :: PARAMS(max_parameters)
     Integer, Intent(OUT) :: PARAM_INFO(max_parameters)
     Logical, Intent(OUT) :: CONSTRAINTS(max_parameters)
     Character(Len = *), Intent(OUT) :: PARNAMES(max_parameters)
     Real, Intent(OUT) :: SCALE_FACTORS(max_parameters)
     Character(Len = *), Intent(OUT) :: RESNAMES(max_results)
     Integer, Intent(OUT) :: num_features
     Integer, Intent(OUT) :: num_parameters
     Integer, Intent(OUT) :: num_results
     Integer, Intent(OUT) :: x_order
     Integer, Intent(OUT) :: y_order
     Real, Intent(OUT) :: xmin_poly
     Real, Intent(OUT) :: ymin_poly
     Real, Intent(OUT) :: xmax_poly
     Real, Intent(OUT) :: ymax_poly
     Logical, Intent(OUT) :: parexist
     Logical, Intent(OUT) :: weighted_fit
     Real, Intent(OUT) :: alpha
     Real, Intent(OUT) :: itsperpar
     Logical, Intent(OUT) :: evolve
     Integer, Intent(OUT) :: disfreq
     Logical, Intent(OUT) :: fastdis
     Integer, Intent(OUT) :: haltcrit
     Real, Intent(OUT) :: haltval
     Real, Intent(OUT) :: MODEL(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PEAKINTEGRATE2 (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MASK, gain, mode, SIGNIFICANT, peak, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'io.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: gain
     Type(PEAK_SEARCH_CONTROL), Intent(IN) :: mode
     Logical*1, Intent(IN) :: SIGNIFICANT(xendelm, yendelm)
!  Import/Export:
     Type(PEAK_STRUCTURE), Intent(OUT) :: peak
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PEAKRZ (a_star, a_theta, a_phi, b_star, b_theta, c_star, &
       b_phi, c_theta, c_phi, h, k, l, r_coordinate, z_coordinate, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: a_star
     Real, Intent(IN) :: a_theta
     Real, Intent(IN) :: a_phi
     Real, Intent(IN) :: b_star
     Real, Intent(IN) :: b_theta
     Real, Intent(IN) :: b_phi
     Real, Intent(IN) :: c_star
     Real, Intent(IN) :: c_theta
     Real, Intent(IN) :: c_phi
     Integer, Intent(IN) :: h
     Integer, Intent(IN) :: k
     Integer, Intent(IN) :: l
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: r_coordinate
     Real, Intent(OUT) :: z_coordinate
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PEAKSEARCH2 (experiment, mask_data, mode, &
       xmaxdat, ymaxdat, DATA, MASK, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, max_peaks, num_peaks, &
       MDATA, retstat, num_saturated, num_bad, PEAKS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Logical, Intent(IN) :: mask_data
     Type(PEAK_SEARCH_CONTROL), Intent(IN) :: mode
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: max_peaks
!  Import/Export:
     Integer, Intent(INOUT) :: num_peaks
!  Export:
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: retstat
     Integer, Intent(OUT) :: num_saturated
     Integer, Intent(OUT) :: num_bad
     Type(PEAK_STRUCTURE), Intent(OUT) :: PEAKS(max_peaks)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PEAKXY (experiment, a_star, a_theta, a_phi, &
       b_star, b_theta, c_star, b_phi, c_theta, c_phi, h, k, l, minus, &
       intersect, x_pixel, y_pixel, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Real, Intent(IN) :: a_star
     Real, Intent(IN) :: a_theta
     Real, Intent(IN) :: a_phi
     Real, Intent(IN) :: b_star
     Real, Intent(IN) :: b_theta
     Real, Intent(IN) :: b_phi
     Real, Intent(IN) :: c_star
     Real, Intent(IN) :: c_theta
     Real, Intent(IN) :: c_phi
     Integer, Intent(IN) :: h
     Integer, Intent(IN) :: k
     Integer, Intent(IN) :: l
     Logical, Intent(IN) :: minus
!  Import/Export:
!  Export:
     Logical, Intent(OUT) :: intersect
     Real, Intent(OUT) :: x_pixel
     Real, Intent(OUT) :: y_pixel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PEEP (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, X_AXIS, Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, &
       zlabel, variances_exist, experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PGAUINT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, peak_intensity, xcenpeak, ycenpeak, x_centre, &
       y_centre, sigradius, angsig, WORKR, intensity, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: peak_intensity
     Real, Intent(IN) :: xcenpeak
     Real, Intent(IN) :: ycenpeak
     Real, Intent(IN) :: x_centre
     Real, Intent(IN) :: y_centre
     Real, Intent(IN) :: sigradius
     Real, Intent(IN) :: angsig
!  Import/Export:
     Real, Intent(INOUT) :: WORKR(xendelm, yendelm)
     Real, Intent(INOUT) :: intensity
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PIXEL_VALUE (xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, &
       status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PIXELREGION (xmaxdat, ymaxdat, xnumdat, ynumdat, xstrelm, &
       ystrelm, xendelm, yendelm, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PIXELXY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, X_AXIS, Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, &
       zlabel, variances_exist, EXPERIMENT, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
     TYPE(EXPERIMENTAL_DETAILS), Intent(IN) :: EXPERIMENT
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PLOT_PRES (mask_data, xmaxdat, ymaxdat, DATA, MASK, &
       X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, &
       x_axis_label, y_axis_label, z_axis_label, experiment, &
       max_calibrants, num_calibrants, CALIBRANT_NAMES, CALIBRANTS, &
       pressure, baseline, status)
     Use IO_LIB
     Use LG_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: x_axis_label
     Character(Len = *), Intent(IN) :: y_axis_label
     Character(Len = *), Intent(IN) :: z_axis_label
     Character(Len = *), Intent(IN) :: CALIBRANT_NAMES(Max_calibrants)
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Integer, Intent(IN) :: max_calibrants
     Integer, Intent(IN) :: num_calibrants
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: CALIBRANTS(max_calibrants)
     Double Precision, Intent(IN) :: pressure
     Real, Intent(IN) :: baseline
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_POLAR_CARTESIAN (max_rings, max_coordinates, &
       num_rings, NUM_COORDINATES, x_beam, x_pixel_size, AZIMUTHS, &
       X_COORDINATES, Y_COORDINATES, X_CARTESIAN, Y_CARTESIAN, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: max_rings
     Integer, Intent(IN) :: max_coordinates
     Integer, Intent(IN) :: num_rings
     Integer, Intent(IN) :: NUM_COORDINATES(max_rings)
     Real, Intent(IN) :: x_beam
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: AZIMUTHS(max_coordinates, max_rings)
     Real, Intent(IN) :: X_COORDINATES(max_coordinates, max_rings)
     Real, Intent(IN) :: Y_COORDINATES(max_coordinates, max_rings)
!  Import/Export:
     Real, Intent(OUT) :: X_CARTESIAN(max_coordinates, max_rings)
     Real, Intent(OUT) :: Y_CARTESIAN(max_coordinates, max_rings)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_POLARISATION (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, experiment, variances_exist, DATA, VARIANCES, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_POLYFIT (max_coordinates, num_coordinates, X_COORDINATES, &
       Y_COORDINATES, max_order, orderplus1, COEFFICIENTS, order, POLYNOMIAL, &
       status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: max_coordinates
     Integer, Intent(IN) :: num_coordinates
     Real, Intent(IN) :: X_COORDINATES(max_coordinates)
     Real, Intent(IN) :: Y_COORDINATES(max_coordinates)
     Integer, Intent(IN) :: max_order
     Integer, Intent(IN) :: orderplus1
     Real, Intent(IN) :: COEFFICIENTS(max_order + 1, max_order + 1)
!  Import/Export:
     Integer, Intent(INOUT) :: order
     Real, Intent(INOUT) :: POLYNOMIAL(max_order + 1)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_POLYFIT2 (max_coordinates, num_coordinates, X_COORDINATES, &
       Y_COORDINATES, max_order, COEFFICIENTS, order, max_curve, POLYNOMIAL, &
       y_minimum, y_maximum, X_PLOT, Y_PLOT, status)
     Implicit None
!  Import:
     Integer, Intent(IN) :: max_coordinates
     Integer, Intent(IN) :: num_coordinates
     Real, Intent(IN) :: X_COORDINATES(max_coordinates)
     Real, Intent(IN) :: Y_COORDINATES(max_coordinates)
     Integer, Intent(IN) :: max_order
     Real, Intent(IN) :: COEFFICIENTS(max_order + 1, max_order + 1)
     Integer, Intent(IN) :: order
     Integer, Intent(IN) :: max_curve
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: POLYNOMIAL(max_order+1)
     Real, Intent(OUT) :: y_minimum
     Real, Intent(OUT) :: y_maximum
     Real, Intent(OUT) :: X_PLOT(max_curve, 2)
     Real, Intent(OUT) :: Y_PLOT(max_curve, 2)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_POLYNOMIAL (lim_order, max_coordinates, num_coordinates, &
       X_COORDINATES, Y_COORDINATES, max_order, WEIGHTS, COEFFICIENTS, &
       RESIDUALS, lower_range, upper_range, order, POLYNOMIAL, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: lim_order
     Integer, Intent(IN) :: max_coordinates
     Integer, Intent(IN) :: num_coordinates
     Real, Intent(IN) :: X_COORDINATES(max_coordinates)
     Real, Intent(IN) :: Y_COORDINATES(max_coordinates)
     Integer, Intent(IN) :: max_order
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: WEIGHTS(max_coordinates)
     Real, Intent(OUT) :: COEFFICIENTS(max_order + 1, max_order + 1)
     Real, Intent(OUT) :: RESIDUALS(max_order + 1)
     Real, Intent(OUT) :: lower_range
     Real, Intent(OUT) :: upper_range
     Integer, Intent(OUT) :: order
     Real, Intent(OUT) :: POLYNOMIAL(max_order + 1)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_POSITION (status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_POSTSCRIPT (status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_POWDERDIFFRACTION (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, XAXIS, YAXIS, DATA, MASK, variances_exist, title, &
       max_radial, experiment, lorentz_geometry, num_radial, RADAXIS, &
       RPROFILE, PROERRS, radial_pixel_size, rtitle, rxlabel, rzlabel, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'io.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Logical, Intent(IN) :: variances_exist
     Character(Len = *), Intent(IN) :: title
     Integer, Intent(IN) :: max_radial
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(INOUT) :: lorentz_geometry
!  Export:
     Integer, Intent(OUT) :: num_radial
     Real, Intent(OUT) :: RADAXIS(max_radial)
     Real, Intent(OUT) :: RPROFILE(max_radial)
     Real, Intent(OUT) :: PROERRS(max_radial)
     Real, Intent(OUT) :: radial_pixel_size
     Character(Len = *), Intent(OUT) :: rtitle
     Character(Len = *), Intent(OUT) :: rxlabel
     Character(Len = *), Intent(OUT) :: rzlabel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_POWDERRING (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, experiment, DATA, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_POWER (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, MASK, DATA, VARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_POWERSPEC (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, XAXIS, YAXIS, DATA, memory_defined, mxnumdat, &
       mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, MXAXIS, MYAXIS, &
       MDATA, mtitle, mxlabel, mylabel, mzlabel, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
!  Export:
     Logical, Intent(OUT) :: memory_defined
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: mystrelm
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: myendelm
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PREDICTOR (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, XAXIS, YAXIS, DATA, mxstrelm, mystrelm, mxendelm, &
       myendelm, MXAXIS, MYAXIS, MDATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
!  Export:
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: mystrelm
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: myendelm
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PRINT (gui, print_type, mask_data, xmaxdat, ymaxdat, &
       X_AXIS, Y_AXIS, DATA, VARIANCES, MASK, title, xlabel, ylabel, zlabel, &
       variances_exist, xstrelm, ystrelm, xendelm, yendelm, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Character(Len = *), Intent(IN) :: print_type
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PROJECTION (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, X_AXIS, Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, &
       zlabel, variances_exist, x_pixel_size, y_pixel_size, mxnumdat, &
       mynumdat, MX_AXIS, MY_AXIS, MDATA, MVARIANCES, mxstrelm, mystrelm, &
       mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, memory_defined, &
       mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MX_AXIS(xmaxdat)
     Real, Intent(OUT) :: MY_AXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Logical, Intent(OUT) :: memory_defined
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_PROJECTFIT (mask_data, xmaxdat, ymaxdat, DATA, MASK, &
       X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
       ylabel, zlabel, experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: mask_data
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
!  Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_QUESTION (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'io_db.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RADIAL (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, DATA, MASK, variances_exist, RADAXIS, RPROFILE, &
       PROERRS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Logical, Intent(IN) :: variances_exist
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: RADAXIS(xmaxdat)
     Real, Intent(OUT) :: RPROFILE(xmaxdat)
     Real, Intent(OUT) :: PROERRS(xmaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_REBIN (gui, masked_data, x_pixel_size, y_pixel_size, &
       xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, MASK, VARIANCES, &
       xstrelm, ystrelm, xendelm, yendelm, &
       variances_exist, retstat, OUTXAXIS, OUTYAXIS, OUTPUT, OUTVARIANCES, &
       xstrout, ystrout, xendout, yendout, xnumout, ynumout, mx_pixel_size, &
       my_pixel_size, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Logical, Intent(IN) :: masked_data
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
!  Export:
     Integer, Intent(OUT) :: retstat
     Real, Intent(OUT) :: OUTXAXIS(xmaxdat)
     Real, Intent(OUT) :: OUTYAXIS(ymaxdat)
     Real, Intent(OUT) :: OUTPUT(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: OUTVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: xstrout
     Integer, Intent(OUT) :: ystrout
     Integer, Intent(OUT) :: xendout
     Integer, Intent(OUT) :: yendout
     Integer, Intent(OUT) :: xnumout
     Integer, Intent(OUT) :: ynumout
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RECIP2UNIT (a_star, b_star, c_star, alpha_star, beta_star, &
       gamma_star, volume_star, a, b, c, alpha, beta, gamma, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Double Precision, Intent(IN) :: a_star
     Double Precision, Intent(IN) :: b_star
     Double Precision, Intent(IN) :: c_star
     Double Precision, Intent(IN) :: alpha_star
     Double Precision, Intent(IN) :: beta_star
     Double Precision, Intent(IN) :: gamma_star
!  Import/Export:
!  Export:
     Double Precision, Intent(OUT) :: volume_star
     Double Precision, Intent(OUT) :: a
     Double Precision, Intent(OUT) :: b
     Double Precision, Intent(OUT) :: c
     Double Precision, Intent(OUT) :: alpha
     Double Precision, Intent(OUT) :: beta
     Double Precision, Intent(OUT) :: gamma
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RECIPROCAL (status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_REFLECT (gui, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, &
       VARIANCES, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, title, &
       xlabel, ylabel, zlabel, variances_exist, x_pixel_size, y_pixel_size, &
       retstat, memory_exist, MXAXIS, MYAXIS, MDATA, MVARIANCES, mxnumdat, &
       mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, &
       mylabel, mzlabel, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
!  Import/Export:
     Real, Intent(INOUT) :: x_pixel_size
     Real, Intent(INOUT) :: y_pixel_size
!  Export:
     Integer, Intent(OUT) :: retstat
     Logical, Intent(OUT) :: memory_exist
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: mystrelm
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: myendelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RESIDUALS (xmax_peaks, ymax_peaks, xnum_peaks, ynum_peaks, &
       X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, x_xnumknots, x_ynumknots, &
       X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, &
       Y_COEFFS, xmaxdat, ymaxdat, MXAXIS, MYAXIS, MDATA, mxstrelm, mystrelm, &
       mxendelm, myendelm, mtitle, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: xnum_peaks
     Integer, Intent(IN) :: ynum_peaks
     Real, Intent(IN) :: X_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: X_DISTORTION(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
     Integer, Intent(IN) :: x_xnumknots
     Integer, Intent(IN) :: x_ynumknots
     Real, Intent(IN) :: X_LAMBDA(x_xnumknots)
     Real, Intent(IN) :: X_MU(x_ynumknots)
     Real, Intent(IN) :: X_COEFFS((x_xnumknots - 4) * (x_ynumknots - 4))
     Integer, Intent(IN) :: y_xnumknots
     Integer, Intent(IN) :: y_ynumknots
     Real, Intent(IN) :: Y_LAMBDA(y_xnumknots)
     Real, Intent(IN) :: Y_MU(y_ynumknots)
     Real, Intent(IN) :: Y_COEFFS((y_xnumknots - 4) * (y_ynumknots - 4))
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Import/Export:
     Real, Intent(INOUT) :: MXAXIS(xmaxdat)
     Real, Intent(INOUT) :: MYAXIS(ymaxdat)
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat)
     Integer, Intent(INOUT) :: mxendelm
     Integer, Intent(INOUT) :: mxstrelm
     Integer, Intent(INOUT) :: myendelm
     Integer, Intent(INOUT) :: mystrelm
     Character(Len = *), Intent(INOUT) :: mtitle
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RINGCOORDS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, MASK, experiment, &
       radius, half_search_distance, num_sections, integrated_ints, &
       max_coordinates, max_profiles, num_profiles, PROFILES, WORK, &
       num_coordinates, X_COORDINATES, Y_COORDINATES, INTENSITIES, AZIMUTHS, &
       status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(IN) :: radius
     Real, Intent(IN) :: half_search_distance
     Integer, Intent(IN) :: num_sections
     Logical, Intent(IN) :: integrated_ints
     Integer, Intent(IN) :: max_coordinates
     Integer, Intent(IN) :: max_profiles
     Integer, Intent(IN) :: num_profiles
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: PROFILES(max_profiles, max_coordinates)
     Real, Intent(OUT) :: WORK(max_profiles, max_coordinates)
     Integer, Intent(OUT) :: num_coordinates
     Real, Intent(OUT) :: X_COORDINATES(max_coordinates)
     Real, Intent(OUT) :: Y_COORDINATES(max_coordinates)
     Real, Intent(OUT) :: INTENSITIES(max_coordinates)
     Real, Intent(OUT) :: AZIMUTHS(max_coordinates)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RINGOUTLIERS (full_info, Max_angles, num_rings, &
       sample_distance, x_beam, y_beam, ANGLE_CONES, tilt_plane_rotation, &
       tilt_angle, radial_error, reject_value, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc'
     Include 'f2d_lsqpowder.inc'
!  Import:
     Logical, Intent(IN) :: full_info
     Integer, Intent(IN) :: max_angles
     Integer, Intent(IN) :: num_rings
     Real, Intent(IN) :: reject_value
     Real, Intent(IN) :: sample_distance
     Real, Intent(IN) :: x_beam
     Real, Intent(IN) :: y_beam
     Real, Intent(IN) :: ANGLE_CONES(max_angles)
     Real, Intent(IN) :: tilt_plane_rotation
     Real, Intent(IN) :: tilt_angle
     Real, Intent(IN) :: radial_error
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_ANGLES (EXPERIMENT, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_CAL_ANGLES (x_coordinate, y_coordinate, experiment, &
       two_theta, phi, chi, omega, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: x_coordinate
     Real, Intent(IN) :: y_coordinate
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
     Real, Intent(INOUT) :: two_theta
     Real, Intent(INOUT) :: phi
     Real, Intent(INOUT) :: chi
     Real, Intent(INOUT) :: omega
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_CAL_BMATRIX (experiment, B_MATRIX, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: B_MATRIX (3, 3)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_CAL_CARTESIAN (info, x_coordinate, y_coordinate, &
       experiment, Z, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: info
     Real, Intent(IN) :: x_coordinate
     Real, Intent(IN) :: y_coordinate
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: Z(3)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_CAL_COORDINATE (x_coordinate, y_coordinate, &
       experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Real, Intent(IN) :: x_coordinate
     Real, Intent(IN) :: y_coordinate
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_CAL_UBMATRIX (experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_CAL_U2MATRIX (B_MATRIX, h_1, k_1, l_1, &
       two_theta_1, phi_1, chi_1, omega_1, h_2, k_2, l_2, two_theta_2, phi_2, &
       chi_2, omega_2, experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: B_MATRIX (3, 3)
     Integer, Intent(IN) :: h_1
     Integer, Intent(IN) :: k_1
     Integer, Intent(IN) :: l_1
     Double Precision, Intent(IN) :: two_theta_1
     Double Precision, Intent(IN) :: phi_1
     Double Precision, Intent(IN) :: chi_1
     Double Precision, Intent(IN) :: omega_1
     Integer, Intent(IN) :: h_2
     Integer, Intent(IN) :: k_2
     Integer, Intent(IN) :: l_2
     Double Precision, Intent(IN) :: two_theta_2
     Double Precision, Intent(IN) :: phi_2
     Double Precision, Intent(IN) :: chi_2
     Double Precision, Intent(IN) :: omega_2
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_CAL_U_PHI (wavelength, B_MATRIX, h, k, l, &
       two_theta_r, phi_r, chi_r, omega_r, U_PHI, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: wavelength
     Real, Intent(IN) :: B_MATRIX (3, 3)
     Integer, Intent(IN) :: h
     Integer, Intent(IN) :: k
     Integer, Intent(IN) :: l
     Double Precision, Intent(IN) :: two_theta_r
     Double Precision, Intent(IN) :: phi_r
     Double Precision, Intent(IN) :: chi_r
     Double Precision, Intent(IN) :: omega_r
!  Import/Export:
     Real U_PHI(3)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_CAL_U3MATRIX (B_MATRIX, h_1, k_1, l_1, Z1, &
       h_2, k_2, l_2, Z2, experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: B_MATRIX (3, 3)
     Integer, Intent(IN) :: h_1
     Integer, Intent(IN) :: k_1
     Integer, Intent(IN) :: l_1
     Real, Intent(IN) :: Z1(3)
     Integer, Intent(IN) :: h_2
     Integer, Intent(IN) :: k_2
     Integer, Intent(IN) :: l_2
     Real, Intent(IN) :: Z2(3)
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_CAL_UMATRIX (B_MATRIX, h_1, k_1, l_1, &
       two_theta_1, phi_1, chi_1, omega_1, h_2, k_2, l_2, two_theta_2, phi_2, &
       chi_2, omega_2, experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: B_MATRIX (3, 3)
     Integer, Intent(IN) :: h_1
     Integer, Intent(IN) :: k_1
     Integer, Intent(IN) :: l_1
     Real, Intent(IN) :: two_theta_1
     Real, Intent(IN) :: phi_1
     Real, Intent(IN) :: chi_1
     Real, Intent(IN) :: omega_1
     Integer, Intent(IN) :: h_2
     Integer, Intent(IN) :: k_2
     Integer, Intent(IN) :: l_2
     Real, Intent(IN) :: two_theta_2
     Real, Intent(IN) :: phi_2
     Real, Intent(IN) :: chi_2
     Real, Intent(IN) :: omega_2
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_CAL_UBH (wavelength, B_MATRIX, H, A4COBS, ISIGN, VAB)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: wavelength
     Real, Intent(IN) :: B_MATRIX (3, 3)
     Real, Intent(IN) :: H(3, 2)
     Real, Intent(IN) :: A4COBS(4, 2)
     Integer, Intent(IN) :: ISIGN(2)
!  Export:
     Real, Intent(OUT) :: VAB(3, 2)
End Subroutine
 
     Subroutine F2D_RMAP_CAL_UBSUB (B_MATRIX, H12, H12O, UB_MATRIX, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: B_MATRIX (3, 3)
     Real, Intent(IN) :: H12(3, 2)
     Real, Intent(IN) :: H12O(3, 2)
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: UB_MATRIX (3, 3)
!  Status:
    Integer status
End Subroutine
 
     Subroutine NORMV (V1, V1N, V1M)
!  Import:
     Real, Intent(IN) :: V1(3)
     Real, Intent(OUT) :: V1N(3)
     Real, Intent(OUT) :: V1M
End Subroutine
 
     Subroutine PRODV (V1, V2, V3)
!  Import:
     Real, Intent(IN) :: V1(3)
     Real, Intent(IN) :: V2(3)
     Real, Intent(OUT) :: V3(3)
End Subroutine
 
     Subroutine TR1P (X, Y, XYZ)
!  Import:
     Real, Intent(IN) :: X(3)
     Real, Intent(IN) :: Y(3)
     Real, Intent(OUT) :: XYZ(3, 3)
End Subroutine
 
     SUBROUTINE F2D_GMPRD (A, B, R, N, M, L)
End Subroutine
 
              SUBROUTINE F2D_GMTRA (A, R, N, M)
End Subroutine
 
                    SUBROUTINE F2D_PHIMAT (phi, DUM)
End Subroutine
 
                    Subroutine F2D_CHIMAT (chi, DUM)
End Subroutine
 
                    SUBROUTINE F2D_NORMAL (V, IERR)
End Subroutine
 
                       SUBROUTINE F2D_VPRODT (V1, V2, V3)
End Subroutine
 
     Subroutine F2D_RMAP_IN_REFLECTIONS (input_options, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, &
       XAXIS, YAXIS, DATA, title, xlabel, ylabel, zlabel, experiment, retstat, &
       status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Import/Export:
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Real, Intent(INOUT) :: XAXIS(xmaxdat)
     Real, Intent(INOUT) :: YAXIS(ymaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
     Integer, Intent(OUT) :: retstat
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_INITIALISE (xmaxmap, ymaxmap, zmaxmap, MAP, &
       NORMALISE, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     Integer, Intent(INOUT) :: xmaxmap
     Integer, Intent(INOUT) :: ymaxmap
     Integer, Intent(INOUT) :: zmaxmap
!  Export:
     Real, Intent(OUT) :: MAP(xmaxmap, ymaxmap, zmaxmap)
     Real, Intent(OUT) :: NORMALISE(xmaxmap, ymaxmap, zmaxmap)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_INUBMATRIX (INPUT_OPTIONS, xmaxdat, ymaxdat, &
       xnumdat, ynumdat, XAXIS, YAXIS, DATA, title, xlabel, ylabel, zlabel, &
       experiment, retstat, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     TYPE(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: INPUT_OPTIONS
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
!  Import/Export:
     TYPE(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
     Integer, Intent(OUT) :: retstat
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_MAPSERIES (xmaxmap, ymaxmap, zmaxmap, xnummap, &
       ynummap, znummap, C1, C2C1, c2c1s, C3C1, c3c1s, C5, C5C1, c5c1s, CN, &
       step, thickness, xmaxdat, ymaxdat, xnumdat, ynumdat, xstrelm, ystrelm, &
       xendelm, yendelm, input_options, experiment, &
       XAXIS, YAXIS, DATA, VARIANCES, MXAXIS, MYAXIS, MEMORY, MVARIANCES, MASK,&
       title, xlabel, ylabel, zlabel, variances_exist, data_defined, &
       mask_exist, MAP, NORMALISE, status)
     Use IO_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Integer, Intent(IN) :: xmaxmap
     Integer, Intent(IN) :: ymaxmap
     Integer, Intent(IN) :: zmaxmap
     Integer, Intent(IN) :: xnummap
     Integer, Intent(IN) :: ynummap
     Integer, Intent(IN) :: znummap
     Real, Intent(IN) :: C1(3)
     Real, Intent(IN) :: C2C1(3)
     Real, Intent(IN) :: c2c1s
     Real, Intent(IN) :: C3C1(3)
     Real, Intent(IN) :: c3c1s
     Real, Intent(IN) :: C5(3)
     Real, Intent(IN) :: C5C1(3)
     Real, Intent(IN) :: c5c1s
     Real, Intent(IN) :: CN(3)
     Real, Intent(IN) :: step
     Real, Intent(IN) :: thickness
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Import/Export:
     Integer, Intent(INOUT) :: xnumdat
     Integer, Intent(INOUT) :: ynumdat
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
     Type(INPUT_OPTIONS_STRUCTURE), Intent(INOUT) :: input_options
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Real, Intent(INOUT) :: XAXIS(xmaxdat)
     Real, Intent(INOUT) :: YAXIS(ymaxdat)
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: MXAXIS(xmaxdat)
     Real, Intent(INOUT) :: MYAXIS(ymaxdat)
     Real, Intent(INOUT) :: MEMORY(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(INOUT) :: title
     Character(Len = *), Intent(INOUT) :: xlabel
     Character(Len = *), Intent(INOUT) :: ylabel
     Character(Len = *), Intent(INOUT) :: zlabel
     Logical, Intent(INOUT) :: data_defined
     Logical, Intent(INOUT) :: variances_exist
     Logical, Intent(INOUT) :: mask_exist
!  Export:
     Real, Intent(OUT) :: MAP(xmaxmap, ymaxmap, zmaxmap)
     Real, Intent(OUT) :: NORMALISE(xmaxmap, ymaxmap, zmaxmap)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_ORIENTATION (xmaxmap, ymaxmap, zmaxmap, experiment, &
       xnummap, ynummap, znummap, LL, LR, UP, DIFFERENCE, C1, C5, CN, C2C1, &
       c2c1s, C3C1, c3c1s, C5C1, c5c1s, step, thickness, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxmap
     Integer, Intent(IN) :: ymaxmap
     Integer, Intent(IN) :: zmaxmap
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
     Integer, Intent(INOUT) :: xnummap
     Integer, Intent(INOUT) :: ynummap
     Integer, Intent(INOUT) :: znummap
!  Export:
     Real, Intent(OUT) :: LL(3)
     Real, Intent(OUT) :: LR(3)
     Real, Intent(OUT) :: UP(3)
     Real, Intent(OUT) :: DIFFERENCE(3)
     Real, Intent(OUT) :: C1(3)
     Real, Intent(OUT) :: C5(3)
     Real, Intent(OUT) :: CN(3)
     Real, Intent(OUT) :: C2C1(3)
     Real, Intent(OUT) :: c2c1s
     Real, Intent(OUT) :: C3C1(3)
     Real, Intent(OUT) :: c3c1s
     Real, Intent(OUT) :: C5C1(3)
     Real, Intent(OUT) :: c5c1s
     Real, Intent(OUT) :: step
     Real, Intent(OUT) :: thickness
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_OUT_VOLUME (input_file, xmaxmap, ymaxmap, zmaxmap, &
       xnummap, ynummap, znummap, MAP, NORMALISE, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Character(Len=*), Intent(IN) :: input_file
     Integer, Intent(IN) :: xmaxmap
     Integer, Intent(IN) :: ymaxmap
     Integer, Intent(IN) :: zmaxmap
     Integer, Intent(IN) :: xnummap
     Integer, Intent(IN) :: ynummap
     Integer, Intent(IN) :: znummap
     Real, Intent(IN) :: MAP(xmaxmap, ymaxmap, zmaxmap)
     Real, Intent(IN) :: NORMALISE(xmaxmap, ymaxmap, zmaxmap)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_SIZE (xmaxmap, ymaxmap, zmaxmap, retstat, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     Integer, Intent(INOUT) :: xmaxmap
     Integer, Intent(INOUT) :: ymaxmap
     Integer, Intent(INOUT) :: zmaxmap
!  Export:
     Integer, Intent(OUT) :: retstat
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_TRANSFER (LL, LR, UP, DIFFERENCE, xmaxmap, ymaxmap, &
       zmaxmap, xnummap, ynummap, znummap, MAP, NORMALISE, step, xmaxdat, &
       ymaxdat, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, XAXIS, &
       YAXIS, DATA, title, xlabel, ylabel, zlabel, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: LL(3)
     Real, Intent(IN) :: LR(3)
     Real, Intent(IN) :: UP(3)
     Real, Intent(IN) :: DIFFERENCE(3)
     Integer, Intent(IN) :: xmaxmap
     Integer, Intent(IN) :: ymaxmap
     Integer, Intent(IN) :: zmaxmap
     Integer, Intent(IN) :: xnummap
     Integer, Intent(IN) :: ynummap
     Integer, Intent(IN) :: znummap
     Real, Intent(IN) :: MAP(xmaxmap, ymaxmap, zmaxmap)
     Real, Intent(IN) :: NORMALISE(xmaxmap, ymaxmap, zmaxmap)
     Real, Intent(IN) :: step
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: xnumdat
     Integer, Intent(OUT) :: ynumdat
     Integer, Intent(OUT) :: xstrelm
     Integer, Intent(OUT) :: ystrelm
     Integer, Intent(OUT) :: xendelm
     Integer, Intent(OUT) :: yendelm
     Real, Intent(OUT) :: XAXIS(xmaxdat)
     Real, Intent(OUT) :: YAXIS(ymaxdat)
     Real, Intent(OUT) :: DATA(xmaxdat, ymaxdat)
     Character(Len = *), Intent(OUT) :: title
     Character(Len = *), Intent(OUT) :: xlabel
     Character(Len = *), Intent(OUT) :: ylabel
     Character(Len = *), Intent(OUT) :: zlabel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_TRANSFORM (xmaxmap, ymaxmap, zmaxmap, xnummap, &
       ynummap, znummap, C1, C2C1, c2c1s, C3C1, c3c1s, C5, CN, step, &
       thickness, two_theta, chi, phi, omega, &
       xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, yendelm, &
       experiment, inv_wave_angstroms, DATA, MASK, MAP, NORMALISE, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_fit2d.inc'
!  Import:
     Integer, Intent(IN) :: xmaxmap
     Integer, Intent(IN) :: ymaxmap
     Integer, Intent(IN) :: zmaxmap
     Integer, Intent(IN) :: xnummap
     Integer, Intent(IN) :: ynummap
     Integer, Intent(IN) :: znummap
     Real, Intent(IN) :: C1(3)
     Real, Intent(IN) :: C2C1(3)
     Real, Intent(IN) :: c2c1s
     Real, Intent(IN) :: C3C1(3)
     Real, Intent(IN) :: c3c1s
     Real, Intent(IN) :: C5(3)
     Real, Intent(IN) :: CN(3)
     Real, Intent(IN) :: step
     Real, Intent(IN) :: thickness
     Double Precision, Intent(IN) :: two_theta
     Double Precision, Intent(IN) :: chi
     Double Precision, Intent(IN) :: phi
     Double Precision, Intent(IN) :: omega
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Real, Intent(IN) :: inv_wave_angstroms
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: MAP(xmaxmap, ymaxmap, zmaxmap)
     Real, Intent(OUT) :: NORMALISE(xmaxmap, ymaxmap, zmaxmap)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_UBMATRIX (UB, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: UB (3, 3)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RMAP_XTALANGLES (experiment, &
       start_chi, start_phi, start_omega, start_2theta, delta_chi, delta_phi, &
       delta_omega, delta_2theta, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Double Precision, Intent(INOUT) :: start_chi
     Double Precision, Intent(INOUT) :: start_phi
     Double Precision, Intent(INOUT) :: start_omega
     Double Precision, Intent(INOUT) :: start_2theta
     Double Precision, Intent(INOUT) :: delta_chi
     Double Precision, Intent(INOUT) :: delta_phi
     Double Precision, Intent(INOUT) :: delta_omega
     Double Precision, Intent(INOUT) :: delta_2theta
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ROLLINGBALL (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MDATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
!  Export:
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ROTATELUT (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
       variances_exist, xstrelm, ystrelm, xendelm, yendelm, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ROWLINE (experiment, xmaxdat, ymaxdat, &
       xstrelm, ystrelm, xendelm, yendelm, method, maxorders, loworder, &
       highorder, x_centre, y_centre, xrowcen, yrowcen, rowline_angle, &
       ppangle, ppashift, ratio, sigradius, sigtheta, INTENSITIES, XAXIS, &
       YAXIS, DATA, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: method
     Integer, Intent(IN) :: maxorders
     Integer, Intent(IN) :: loworder
     Integer, Intent(IN) :: highorder
     Real, Intent(IN) :: x_centre
     Real, Intent(IN) :: y_centre
     Real, Intent(IN) :: xrowcen
     Real, Intent(IN) :: yrowcen
     Real, Intent(IN) :: rowline_angle
     Real, Intent(IN) :: ppangle
     Real, Intent(IN) :: ppashift
     Real, Intent(IN) :: ratio
     Real, Intent(IN) :: sigradius
     Real, Intent(IN) :: sigtheta
     Real, Intent(IN) :: INTENSITIES(-maxorders: maxorders)
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RTHETA (title, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MASK, experiment, max_angular, &
       max_radial, lorentz_geometry, num_azimuthal, &
       num_radial, ANGULAR_AXIS, RADIAL_AXIS, R_THETA, azimuth_pixel_size, &
       radial_pixel_size, mtitle, mxlabel, mylabel, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Character(Len = *), Intent(IN) :: title
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
     Integer, Intent(IN) :: max_angular
     Integer, Intent(IN) :: max_radial
!  Import/Export:
     Integer, Intent(INOUT) :: lorentz_geometry
!  Export:
     Integer, Intent(OUT) :: num_azimuthal
     Integer, Intent(OUT) :: num_radial
     Real, Intent(OUT) :: ANGULAR_AXIS(max_angular)
     Real, Intent(OUT) :: RADIAL_AXIS(max_radial)
     Real, Intent(OUT) :: R_THETA(max_angular, max_radial)
     Real, Intent(OUT) :: radial_pixel_size
     Real, Intent(OUT) :: azimuth_pixel_size
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_RTHETA2 (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, MASK, conserve_intensity, angular_scan, experiment, &
       lorentz_geometry, maximum_2theta, &
       maximum_radius, max_angular, max_radial, num_angular, num_radial, &
       xmax_work, ymax_work, WORK, R_THETA, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat,ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Logical, Intent(IN) :: conserve_intensity
     Logical, Intent(IN) :: angular_scan
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Integer, Intent(IN) :: lorentz_geometry
     Real, Intent(IN) :: maximum_2theta
     Real, Intent(IN) :: maximum_radius
     Integer, Intent(IN) :: max_angular
     Integer, Intent(IN) :: max_radial
     Integer, Intent(IN) :: num_angular
     Integer, Intent(IN) :: num_radial
     Integer, Intent(IN) :: xmax_work
     Integer, Intent(IN) :: ymax_work
!  Export:
     Real, Intent(OUT) :: WORK(xmax_work, ymax_work)
     Real, Intent(OUT) :: R_THETA(max_angular, max_radial)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SATURATED (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SAVE_INTEGRATION_PARAMETERS (input_file, &
       experiment, xstrelm, ystrelm, &
       xendelm, yendelm, outer_limit, scan_type, maximum_d, &
       conserve_intensity, correct_geometry, outer_angle, &
       num_2theta, lorentz_geometry, rad_pixel_size, outer_q, &
       parameter_file, status)
     Use IO_LIB
     Use GS_LIB
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Character(Len = *), Intent(IN) :: input_file
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: outer_limit
     Integer, Intent(IN) :: scan_type
     Real, Intent(IN) :: maximum_d
     Logical, Intent(IN) :: conserve_intensity
     Logical, Intent(IN) :: correct_geometry
     Real, Intent(IN) :: outer_angle
     Integer, Intent(IN) :: num_2theta
     Integer, Intent(IN) :: lorentz_geometry
     Real, Intent(IN) :: rad_pixel_size
     Real, Intent(IN) :: outer_q
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: parameter_file
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SAVE2DPEAKS ( xmax_peaks, ymax_peaks, xnum_peaks, &
       ynum_peaks, X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: xnum_peaks
     Integer, Intent(IN) :: ynum_peaks
     Real, Intent(IN) :: X_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: X_DISTORTION(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SAVEGEOMETRY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, x_pixel_size, y_pixel_size, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SAVEMODEL (max_parameters, PARAMS, PARAM_INFO, &
       CONSTRAINTS, PARNAMES, SCALE_FACTORS, num_features, num_parameters, &
       x_order, y_order, xmin_poly, ymin_poly, xmax_poly, ymax_poly, &
       weighted_fit, alpha, itsperpar, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc'
!  Import:
     Integer, Intent(IN) :: max_parameters
     Real, Intent(IN) :: PARAMS(max_parameters)
     Integer, Intent(IN) :: PARAM_INFO(max_parameters)
     Logical, Intent(IN) :: CONSTRAINTS(max_parameters)
     Character(Len = *), Intent(IN) :: PARNAMES(max_parameters)
     Real, Intent(IN) :: SCALE_FACTORS(max_parameters)
     Integer, Intent(IN) :: num_features
     Integer, Intent(IN) :: num_parameters
     Integer, Intent(IN) :: x_order
     Integer, Intent(IN) :: y_order
     Real, Intent(IN) :: xmin_poly
     Real, Intent(IN) :: ymin_poly
     Real, Intent(IN) :: xmax_poly
     Real, Intent(IN) :: ymax_poly
     Logical, Intent(IN) :: weighted_fit
     Real, Intent(IN) :: alpha
     Real, Intent(IN) :: itsperpar
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SCALE (max_parameters, num_parameters, PARNAMES, PARAMS, &
       SCALE_FACTORS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: max_parameters
     Integer, Intent(IN) :: num_parameters
     Character(Len = *), Intent(IN) :: PARNAMES(max_parameters)
     Real, Intent(IN) :: PARAMS(max_parameters)
!  Import/Export:
     Real, Intent(INOUT) :: SCALE_FACTORS(max_parameters)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SCAN (output_graphics, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_comm.inc'
!  Import:
     Logical, Intent(IN) :: output_graphics
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SCANF (unit, line, ok, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'f2d_comm.inc'
!  Import:
     Integer, Intent(IN) :: unit
     Integer, Intent(IN) :: line
!  Export:
     Integer, Intent(OUT) :: ok
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SDLUT ( x_minimum, y_minimum, x_maximum, y_maximum, &
       x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, &
       y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, &
       xmax_lut, ymax_lut, xnum_lut, ynum_lut, X_SD, Y_SD, INT_REBINNED, &
       sd_lut_defined, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: x_minimum
     Real, Intent(IN) :: y_minimum
     Real, Intent(IN) :: x_maximum
     Real, Intent(IN) :: y_maximum
     Integer, Intent(IN) :: x_xnumknots
     Integer, Intent(IN) :: x_ynumknots
     Real, Intent(IN) :: X_LAMBDA(x_xnumknots)
     Real, Intent(IN) :: X_MU(x_ynumknots)
     Real, Intent(IN) :: X_COEFFS((x_xnumknots - 4) * (x_ynumknots - 4))
     Integer, Intent(IN) :: y_xnumknots
     Integer, Intent(IN) :: y_ynumknots
     Real, Intent(IN) :: Y_LAMBDA(y_xnumknots)
     Real, Intent(IN) :: Y_MU(y_ynumknots)
     Real, Intent(IN) :: Y_COEFFS((y_xnumknots - 4) * (y_ynumknots - 4))
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Integer, Intent(IN) :: xmax_lut
     Integer, Intent(IN) :: ymax_lut
     Integer, Intent(IN) :: xnum_lut
     Integer, Intent(IN) :: ynum_lut
!  Import/Export:
!  Export:
     Byte, Intent(OUT) :: X_SD(xmax_lut, ymax_lut)
     Byte, Intent(OUT) :: Y_SD(xmax_lut, ymax_lut)
     Byte, Intent(OUT) :: INT_REBINNED(9, xmax_lut, ymax_lut)
     Logical, Intent(OUT) :: sd_lut_defined
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SDLUTSUB ( x_minimum, y_minimum, x_maximum, y_maximum, &
       x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, &
       y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, &
       maxwork, y_rows, xmax_lut, ymax_lut, xnum_lut, ynum_lut, X_COORDINATES, &
       Y_COORDINATES, X_DISTORTION, Y_DISTORTION, X_SD, Y_SD, INT_REBINNED, &
       sd_lut_defined, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: x_minimum
     Real, Intent(IN) :: y_minimum
     Real, Intent(IN) :: x_maximum
     Real, Intent(IN) :: y_maximum
     Integer, Intent(IN) :: x_xnumknots
     Integer, Intent(IN) :: x_ynumknots
     Real, Intent(IN) :: X_LAMBDA(x_xnumknots)
     Real, Intent(IN) :: X_MU(x_ynumknots)
     Real, Intent(IN) :: X_COEFFS((x_xnumknots - 4) * (x_ynumknots - 4))
     Integer, Intent(IN) :: y_xnumknots
     Integer, Intent(IN) :: y_ynumknots
     Real, Intent(IN) :: Y_LAMBDA(y_xnumknots)
     Real, Intent(IN) :: Y_MU(y_ynumknots)
     Real, Intent(IN) :: Y_COEFFS((y_xnumknots - 4) * (y_ynumknots - 4))
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Integer, Intent(IN) :: maxwork
     Integer, Intent(IN) :: y_rows
     Integer, Intent(IN) :: xmax_lut
     Integer, Intent(IN) :: ymax_lut
     Integer, Intent(IN) :: xnum_lut
     Integer, Intent(IN) :: ynum_lut
!  Import/Export:
     Real, Intent(INOUT) :: X_COORDINATES(maxwork)
     Real, Intent(INOUT) :: Y_COORDINATES(y_rows)
     Real, Intent(INOUT) :: X_DISTORTION(maxwork * y_rows)
     Real, Intent(INOUT) :: Y_DISTORTION(maxwork * y_rows)
!  Export:
     Byte, Intent(OUT) :: X_SD(xmax_lut, ymax_lut)
     Byte, Intent(OUT) :: Y_SD(xmax_lut, ymax_lut)
     Byte, Intent(OUT) :: INT_REBINNED(9, xmax_lut, ymax_lut)
     Logical, Intent(OUT) :: sd_lut_defined
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SDADD1 (byte_value)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
     Byte, Intent(INOUT) :: byte_value
!  Export:
!  Status:
End Subroutine
 
     Subroutine F2D_SDSUB1 (byte_value)
     Implicit None
!  Import:
!  Import/Export:
     Byte, Intent(INOUT) :: byte_value
!  Export:
!  Status:
End Subroutine
 
     Subroutine F2D_SELECTPIXEL (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SEQUENCE (inmacro_file, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'io_db.inc'
!  Import:
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: inmacro_file
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SET_EXPERIMENT (experiment, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'io.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SETUP (poisson, mlm, weight, alpha, itsperpar, evolve, &
       disfreq, contint, reverse, fastdis, haltcrit, haltval, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
     Logical, Intent(INOUT) :: poisson
     Logical, Intent(INOUT) :: mlm
     Logical, Intent(INOUT) :: weight
     Real, Intent(INOUT) :: alpha
     Real, Intent(INOUT) ::  itsperpar
     Logical, Intent(INOUT) :: evolve
     Integer, Intent(INOUT) :: disfreq
     Integer, Intent(INOUT) :: contint
     Logical, Intent(INOUT) :: reverse
     Logical, Intent(INOUT) :: fastdis
     Integer, Intent(INOUT) :: haltcrit
     Real, Intent(INOUT) :: haltval
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SLEEP (status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SLICE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, xnumdat, ynumdat, X_AXIS, Y_AXIS, DATA, VARIANCES, title, &
       xlabel, ylabel, zlabel, variances_exist, x_pixel_size, y_pixel_size, &
       mxnumdat, mynumdat, MX_AXIS, MY_AXIS, MDATA, MVARIANCES, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       memory_defined, mx_pixel_size, my_pixel_size, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Real, Intent(OUT) :: MX_AXIS(xmaxdat)
     Real, Intent(OUT) :: MY_AXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: myendelm
     Integer, Intent(OUT) :: mystrelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
     Logical, Intent(OUT) :: memory_defined
     Real, Intent(OUT) :: mx_pixel_size
     Real, Intent(OUT) :: my_pixel_size
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SPATIALFILTER (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MDATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
!  Export:
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SPHERICAL_MOD (xmaxdat, ymaxdat, title, xlabel, ylabel, &
       zlabel, xnumdat, ynumdat, XAXIS, YAXIS, xstrelm, ystrelm, xendelm, &
       yendelm, x_beam, y_beam, DATA, status)
     Use IO_LIB
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: x_beam
     Real, Intent(IN) :: y_beam
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SRADIAL (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, DATA, MASK, variances_exist, EXPERIMENT, &
       lorentz_geometry, angular_scan, &
       radial_pixel_size, max_radial, num_radial, RAD_AXIS, PROFILE, PROVARS, &
       NUMPIXELS, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'io.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat,ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Logical, Intent(IN) :: variances_exist
     TYPE(EXPERIMENTAL_DETAILS), Intent(INOUT) :: EXPERIMENT
     Integer, Intent(IN) :: lorentz_geometry
     Logical, Intent(IN) :: angular_scan
     Real, Intent(IN) :: radial_pixel_size
     Integer, Intent(IN) :: max_radial
!  Export:
     Integer, Intent(OUT) :: num_radial
     Real, Intent(OUT) :: RAD_AXIS(max_radial)
     Real, Intent(OUT) :: PROFILE(max_radial)
     Real, Intent(OUT) :: PROVARS(max_radial)
     Real, Intent(OUT) :: NUMPIXELS(max_radial)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_STATISTICS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, DATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_STOP_WATCH (status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SURFACE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
       memory_defined, SURFACE, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
!  Export:
     Logical, Intent(OUT) :: memory_defined
     Real, Intent(OUT) :: SURFACE(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_SYMFUN (rad_pixel_size, maxrad, numrad, PROFILE, &
       x_pixel_size, y_pixel_size, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: rad_pixel_size
     Integer, Intent(IN) :: maxrad
     Integer, Intent(IN) :: numrad
     Real, Intent(IN) :: PROFILE(maxrad)
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: DATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_THRESHOLD (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, MASK, DATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_TILTCENTRE (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, X_AXIS, Y_AXIS, DATA, MASK, title, xlabel, ylabel, &
       zlabel, experiment, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_TILTINFO (x_pixel_size, y_pixel_size, num_coordinates, &
       x_ellipse, y_ellipse, radius1, radius2, angle1, radial_error1, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
     Integer, Intent(IN) :: num_coordinates
     Real, Intent(IN) :: x_ellipse
     Real, Intent(IN) :: y_ellipse
     Real, Intent(IN) :: radius1
     Real, Intent(IN) :: radius2
     Real, Intent(IN) :: angle1
     Real, Intent(IN) :: radial_error1
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_TILT2INFO (x_pixel_size, y_pixel_size, max_rings, &
       num_rings, x_beam, y_beam, ANGLE_CONES, rotation, tilt, radial_error, &
       status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
     Integer, Intent(IN) :: max_rings
     Integer, Intent(IN) :: num_rings
     Real, Intent(IN) :: x_beam
     Real, Intent(IN) :: y_beam
     Real, Intent(IN) :: ANGLE_CONES(max_rings)
     Real, Intent(IN) :: rotation
     Real, Intent(IN) :: tilt
     Real, Intent(IN) :: radial_error
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_TILTDISPLAY (x_pixel_size, y_pixel_size, max_coordinates, &
       max_rings, num_rings, NUM_COORDINATES, X_COORDINATES, Y_COORDINATES, &
       status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: x_pixel_size
     Real, Intent(IN) :: y_pixel_size
     Integer, Intent(IN) :: max_coordinates
     Integer, Intent(IN) :: max_rings
     Integer, Intent(IN) :: num_rings
     Integer, Intent(IN) :: NUM_COORDINATES(max_rings)
     Real, Intent(IN) :: X_COORDINATES(Max_coordinates, Max_rings)
     Real, Intent(IN) :: Y_COORDINATES(Max_coordinates, Max_rings)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_TILTRADIAL (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, MASK, variances_exist, experiment, &
       lorentz_geometry, angular_scan, radial_pixel_size, &
       max_radial, num_radial, RAD_AXIS, PROFILE, PROVARS, NUMPIXELS, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Logical, Intent(IN) :: variances_exist
     TYPE(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment
     Integer, Intent(IN) :: lorentz_geometry
     Logical, Intent(IN) :: angular_scan
     Real, Intent(IN) :: radial_pixel_size
     Integer, Intent(IN) :: max_radial
!  Export:
     Integer, Intent(OUT) :: num_radial
     Real, Intent(OUT) :: RAD_AXIS(max_radial)
     Real, Intent(OUT) :: PROFILE(max_radial)
     Real, Intent(OUT) :: PROVARS(max_radial)
     Real, Intent(OUT) :: NUMPIXELS(max_radial)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_TRAILER (fit2d_version, status)
     Use IO_LIB
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Character(Len = 7), Intent(IN) :: fit2d_version
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_TRANSFORM (gui, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, &
       VARIANCES, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, title, &
       xlabel, ylabel, zlabel, variances_exist, x_pixel_size, y_pixel_size, &
       retstat, memory_exist, MXAXIS, MYAXIS, MDATA, MVARIANCES, mxnumdat, &
       mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, &
       mylabel, mzlabel, status)
     Implicit None
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
!  Import/Export:
     Real, Intent(INOUT) :: x_pixel_size
     Real, Intent(INOUT) :: y_pixel_size
!  Export:
     Integer, Intent(OUT) :: retstat
     Logical, Intent(OUT) :: memory_exist
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(INOUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: mystrelm
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: myendelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_TRANSPOSE (gui, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, &
       VARIANCES, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, title, &
       xlabel, ylabel, zlabel, variances_exist, retstat, memory_exist, MXAXIS, &
       MYAXIS, MDATA, MASK, MVARIANCES, mxnumdat, mynumdat, &
       mxstrelm, mystrelm, mxendelm, myendelm, &
       mtitle, mxlabel, mylabel, mzlabel, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Real, Intent(IN) :: XAXIS(xmaxdat)
     Real, Intent(IN) :: YAXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
     Logical, Intent(IN) :: variances_exist
!  Export:
     Integer, Intent(OUT) :: retstat
     Logical, Intent(OUT) :: memory_exist
     Real, Intent(OUT) :: MXAXIS(xmaxdat)
     Real, Intent(OUT) :: MYAXIS(ymaxdat)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat)
     Integer, Intent(OUT) :: mxnumdat
     Integer, Intent(OUT) :: mynumdat
     Integer, Intent(OUT) :: mxstrelm
     Integer, Intent(OUT) :: mystrelm
     Integer, Intent(OUT) :: mxendelm
     Integer, Intent(OUT) :: myendelm
     Character(Len = *), Intent(OUT) :: mtitle
     Character(Len = *), Intent(OUT) :: mxlabel
     Character(Len = *), Intent(OUT) :: mylabel
     Character(Len = *), Intent(OUT) :: mzlabel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_TRIANGLE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_UNINORMALISE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, MASK, VARIANCES, maxwork, TOTAL, SUM, &
       MDATA, MVARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat)
     Integer, Intent(IN) :: maxwork
!  Export:
     Integer, Intent(OUT) :: TOTAL(maxwork)
     Real, Intent(OUT) :: SUM(maxwork)
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_UNIT2RECIPROCAL (a, b, c, alpha, beta, gamma, volume, &
       a_star, b_star, c_star, alpha_star, beta_star, gamma_star, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Double Precision, Intent(IN) :: a
     Double Precision, Intent(IN) :: b
     Double Precision, Intent(IN) :: c
     Double Precision, Intent(IN) :: alpha
     Double Precision, Intent(IN) :: beta
     Double Precision, Intent(IN) :: gamma
!  Import/Export:
!  Export:
     Double Precision, Intent(OUT) :: volume
     Double Precision, Intent(OUT) :: a_star
     Double Precision, Intent(OUT) :: b_star
     Double Precision, Intent(OUT) :: c_star
     Double Precision, Intent(OUT) :: alpha_star
     Double Precision, Intent(OUT) :: beta_star
     Double Precision, Intent(OUT) :: gamma_star
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_UNDEFINE (status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_VARIANCES (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, VARIANCES, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
!  Export:
     Real, Intent(OUT) :: VARIANCES(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_VECTOR2DATA (results, &
       AXIS, vec_num, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
       DATA, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, status)
     Implicit None
     Include 'io.inc'
     Include 'st_symbols.inc'
!  Import:
     Type(RESULT_VECTORS), Intent(IN) :: results
     Real, Intent(IN) :: AXIS(results%max_values)
     Integer, Intent(IN) :: vec_num
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
!  Export:
     Integer, Intent(OUT) :: xnumdat
     Integer, Intent(OUT) :: ynumdat
     Integer, Intent(OUT) :: xstrelm
     Integer, Intent(OUT) :: ystrelm
     Integer, Intent(OUT) :: xendelm
     Integer, Intent(OUT) :: yendelm
     Real, Intent(OUT) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(OUT) :: XAXIS(xmaxdat)
     Real, Intent(OUT) :: YAXIS(ymaxdat)
     Character(Len = *), Intent(OUT) :: title
     Character(Len = *), Intent(OUT) :: xlabel
     Character(Len = *), Intent(OUT) :: ylabel
     Character(Len = *), Intent(OUT) :: zlabel
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_VIEW2DPEAKS ( xmax_peaks, ymax_peaks, xnum_peaks, &
       ynum_peaks, X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: xnum_peaks
     Integer, Intent(IN) :: ynum_peaks
     Real, Intent(IN) :: X_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: X_DISTORTION(xmax_peaks, ymax_peaks)
     Real, Intent(IN) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_VIGNETTING (grid_spacing, xstrelm, ystrelm, xendelm, &
       yendelm, xmax_peaks, ymax_peaks, xnumpeaks, ynumpeaks, X_PEAKS, &
       Y_PEAKS, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: grid_spacing
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: xmax_peaks
     Integer, Intent(IN) :: ymax_peaks
     Integer, Intent(IN) :: xnumpeaks
     Integer, Intent(IN) :: ynumpeaks
!  Import/Export:
     Real, Intent(INOUT) :: X_PEAKS(xmax_peaks, ymax_peaks)
     Real, Intent(INOUT) :: Y_PEAKS(xmax_peaks, ymax_peaks)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_XRIIFLATFIELD (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, x_pixel_size, y_pixel_size, DATA, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
!  Import/Export:
     Real, Intent(INOUT) :: x_pixel_size
     Real, Intent(INOUT) :: y_pixel_size
!  Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat)
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_XYGRAPH (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, VARIANCES, X_AXIS, Y_AXIS, title, &
       xlabel, ylabel, zlabel, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Logical, Intent(IN) :: variances_exist
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat)
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ZOOMIN (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, DATA, title, xlabel, ylabel, zlabel, xstrelm, ystrelm, xendelm, &
       yendelm, status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat
     Integer, Intent(IN) :: ymaxdat
     Integer, Intent(IN) :: xnumdat
     Integer, Intent(IN) :: ynumdat
     Real, Intent(IN) :: X_AXIS(xmaxdat)
     Real, Intent(IN) :: Y_AXIS(ymaxdat)
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat)
     Character(Len = *), Intent(IN) :: title
     Character(Len = *), Intent(IN) :: xlabel
     Character(Len = *), Intent(IN) :: ylabel
     Character(Len = *), Intent(IN) :: zlabel
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm
     Integer, Intent(INOUT) :: ystrelm
     Integer, Intent(INOUT) :: xendelm
     Integer, Intent(INOUT) :: yendelm
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
     Subroutine F2D_ZSCALE (status)
     Implicit None
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status
End Subroutine
 
End Interface
End Module
