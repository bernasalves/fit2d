!********1*********2*********3*********4*********5*********6*********7*********8


!  ******************
!  *                *
!  *  f2d_main.f90  *
!  *                *
!  ******************
 
!+ FIT2D - FITting (2-Dimensional)
     Subroutine F2D_MAIN
!  Description:
!    Interactive fitting of input or simulated multiple peak data with noise and
!    a dc level. Many other data analysis operations are available.

!  Method:
!    Menu driven program to select different options
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    05-Jul-2016: V17.007 Investigating array divide problem (Hammersley)
!    28-Jan-2015: V17.006 New Linux version (Hammersley)
!    27-Jan-2015: V17.005 Re-link with CBFLIB 0.9.5 and HDF5(?) (Hammersley)
!    07-Jan-2015: V17.004 Geometry "FLIP" command now flips any mask 
!      (Hammersley)
!    05-Jan-2015: V17.003 2015 edition. Geometry "TRANSPOSE" command now 
!      transposes any data mask (Hammersley)
!    12-Dec-2014: V17.002 Add "CYCLIC SHIFT" command in "GEOMETRIC" menu
!      (Hammersley)
!    11-Dec-2014: V17.001 Use masking in "MATHS" menu operations (Hammersley)
!    11-Dec-2014: V16.041 Go back to old version for Windows (Problem getting
!      Fortran allocation to work properly for "MASK") (Hammersley)
!    10-Dec-2014: V16.040 Investigating possible problem with Linux X-server
!      (Hammersley)
!    09-Dec-2014: V16.039 Use data structure and Fortran allocation for result
!      vectors (Hammersley)
!    08-Dec-2014: V16.038 Add "TRANSFER" and "SET VECTOR" commands to "VECTORS
!     menu (Hammersley)
!    05-Dec-2014: V16.037 Linux tested version, 3-D surface seems fine. Option
!      to output text file of vector values in "VECTORS" menu (Hammersley)
!    04-Dec-2014: V16.036 Re-imported from Linux version. Use Fortran memory
!      allocation in LG raster mode (Hammersley)
!    03-Dec-2014: V16.035 Further debugging Linux version. O.K. (Hammersley)
!    02-Dec-2014: V16.034 Debugging Linux version (Hammersley)
!    28-Nov-2014: V16.033 Input overflow tables for new Bruker format
!      (Hammersley)
!    27-Nov-2014: V16.032 Correction to ID06 CAKE array size. Option to set 
!      detector offset in experiment GUI. Click D-spacing in normal powder
!      diffraction integrated data (Hammersley)
!    27-Nov-2014: V16.031 Investigating problem with "CALIBRANT" on standard 
!      silicon powder sample; go back to old code (Hammersley)
!    26-Nov-2014: V16.030 Corrections for ID06LVP interface (Hammersley)
!    26-Nov-2014: V16.029 Try to re-instate new "GS_FILESELECTION" (Hammersley)
!    19-Nov-2014: V16.028 Go back to old version of "GS_FILESELECTION" owing to
!      introduced errors and for compiling definite version (Hammersley)
!    18-Nov-2014: V16.027 Changes to "GS_FILESELECTION" to allow only the 
!      directory to be chosen without a file name (Hammersley)
!    10-Nov-2014: V16.026 Testing "MULTI-CHIPLOT" option to "OUTPUT" choices
!      (Hammersley)
!    07-Nov-2014: V16.025 Add "MULTI-CHIPLOT" option to "OUTPUT" choices
!      (Hammersley)
!    06-Nov-2014: V16.024 Investigating problem with inputting recent Bruker 
!      file (Hammersley)
!    03-Nov-2014: V16.023 Add acknowledgements to "EXIT" text (Hammersley)
!    03-Nov-2014: V16.022 Add extra figure to "PRESSURE CALIBRATION" pressure 
!      text (Hammersley)
!    31-Oct-2014: V16.021 Correct ID06 "CALIBRANT" coordinate search 
!      (Hammersley)
!    30-Oct-2014: V16.020 ID06 "CALIBRANT": Add "detector_offset" and allow
!      X and Y beam centre to vary (Hammersley)
!    16-Oct-2014: V16.019 ID06 "CALIBRANT" finished(?)(Hammersley)
!    16-Oct-2014: V16.018 Correction to ID06 "CALIBRANT" (Hammersley)
!    16-Oct-2014: V16.017 Changes to ID06 "CALIBRANT" (Hammersley)
!    15-Oct-2014: V16.016 Testing ID06 "CALIBRANT" (Hammersley)
!    06-Oct-2014  V16.015 Testing ID06 "CALIBRANT" (Hammersley)
!    16-Sep-2014: V16.014 Debugging ID06 "CALIBRANT" (Hammersley)
!    15-Sep-2014: V16.013 Add polar fitting to ID06 "CALIBRANT" (Hammersley)
!    05-Sep-2014: V16.012 Temporarily disable ID06 interface (Hammersley)
!    12-Aug-2014: V16.010 Use new ID06 calibration procdure (Hammersley)
!    28-Jul-2014: V16.009 Temporarily disable ID06 interface (Hammersley)
!    23-Jul-2014: V16.008 Further changes to "F2D_ID06_CALIBRANT2" (Hammersley)
!    09-Jul-2014: V16.007 Changes to "F2D_ID06_CALIBRANT2" (Hammersley)
!    27-Jun-2014: V16.006 Investigating problem inputting edf image (Hammersley)
!    20-Jun-2014: V16.005 Use two reflections to estimate detector distance and 
!      beam centre in ID06 "CALIBRANT" (Hammersley)
!    11-Jun-2014: V16.004 Static test data for ID06 (Hammersley)
!    06-Jun-2014: V16.003 Test ID06 "CALIBRANT" with alternative focii 
!      (Hammersley)
!    27-May-2014: V16.002 Increase scale factor for X-beam centre owing to
!      larger error for ID06 polar data (Hammersley)
!    22-May-2014: V16.001 De-bugging IDO6 "CAKE" integration(Hammersley)
!    19-May-2014: V16.000 Compile on Windows 7 sytem, with new compiler versions
!      and new libraries (Hammersley)
!    02-May-2014: V15.248 De-bugging/testing ID06 "CAKE" (Hammersley)
!    30-Apr-2014: V15.247 De-bugging/testing ID06 "CALIBRANT" (Hammersley)
!    29-Apr-2014: V15.246 Changes to "INPUT" in "ID06" interface (Hammersley)
!    23-Apr-2014: V15.245 Add "RE-BIN" to "ID06" interface (Hammersley)
!    15-Apr-2014: V15.244 Correct Hexagonal lattice (Hammersley)
!    10-Apr-2014: V15.243 Changes to "PRESSURE CALIBRATION" interface
!      (Hammersley)
!    01-Apr-2014: V15.242 Improve "ID06 LVP" interface (Hammersley)
!    07-Mar-2014: V15.241 Correct "INPUT RAW" option (Hammersley)
!    06-Mar-2014: V15.240 Initial "user" test version "ID06LVP" interface
!      (Hammersley)
!    04-Mar-2014: V15.239 Debugging "CALIBRANT" for "ID06LVP" interface
!      (Hammersley)
!    19-Feb-2014: V15.238 Implementing "CALIBRANT" for "ID06LVP" interface
!      (Hammersley)
!    10-Feb-2014: V15.237 Implementing "CALIBRANT" for "ID06LVP" interface
!      (Hammersley)
!    24-Jan-2014: V15.236 Add "INPUT RAW" and "INPUT" options for "ID06 LVP" 
!      interface(Hammersley)
!    14-Jan-2014: V15.235 Support for ID06 LVP (Hammersley)
!    20-Dec-2013: V15.234 Add output of altered lattice parameters to "PRESSURE 
!      CALIBRATION" output (Hammersley)
!    19-Dec-2013: V15.233 Recovered version. (Hammersley)
!    11-Oct-2013: V15.232 Add "REMOVE CALIBRANT" and other changes in PRESSURE 
!      CALIBRATION interface (Hammersley)
!    07-Oct-2013: V15.231 Add support of other lattices in PRESSURE CALIBRATION
!      interface (Hammersley)
!    20-Sep-2013: V15.230 Release version of "PRESSURE CALIBRATION" interface
!      (Hammersley)
!    18-Sep-2013: V15.229 Test version of "PRESSURE CALIBRATION" interface
!      (Hammersley)
!    13-Sep-2013: V15.228 Investigate network input times versus buffer length
!      for input of "Klora" files (Hammersley)
!    02-Aug-2013: V15.227 Increase size of file names store for GUI
!      file selection (Hammersley)
!    31-Jul-2013: V15.226 Input cell from JCPDS V4 file (Hammersley)
!    25-Jul-2013: V15.225 Add "PRESSURE CALIBRATION" interface (Hammersley)
!    24-Apr-2013: V15.217 Output RMS residual for "CALIBRANT" option
!      (Hammersley)
!    23-Apr-2013: V15.216 Investigating program stopping when <ENTER> is pressed
!      for form entry. Problem solved  (Hammersley)
!    18-Apr-2013: V15.215 Allow "CALIBRANT" option to take into account
!      parallax angular correction (Hammersley)
!    12-Apr-2013: V15.214 Correct over-riding of sample to detector distance
!      (Hammersley)
!    08-Apr-2013: V15.213 Changes to MA routines (Hammersley)
!    20-Mar-2013: V15.212 Add Silver Behenate to calibrants list (Hammersley) 
!    18-Mar-2013: V15.211 Change conversion factor for wavelength in Angstroms 
!      into KeV from 12.39852 to 12.3984193 (Hammersley) 
!    18-Feb-2013: V15.208 2013 version (Hammersley)
!    16-Nov-2012: V15.198 Testing parallax correction (Hammersley)
!    20-Sep-2012: V15.190 Option to correct angular position for detector 
!      parallax (Hammersley)
!    27-Aug-2012: V15.188 Implementation of look-up table based integration
!      (Hammersley)
!    23-Aug-2012: V15.187 Implementing look-up table based integration
!      (Hammersley)
!    02-Jul-2012: V15.186 Changes to "F2D_INTEGRATE" (Hammersley)
!    15-Jun-2012: V15.185 Changes to "IO" routines (Hammersley)
!    14-Jun-2012: V15.184 Changes to "IO" routines (Hammersley)
!    09-May-2012: V15.183 Re-compile (Hammersley)
!    26-Apr-2012: V15.182 Changes to "PD" GUI (Hammersley)
!    13-Mar-2012: V15.181 Changes to "GS" routines (Hammersley)
!    13-Feb-2012: V15.180 Changes to "MA" routines (Hammersley)
!    19-Jan-2012: V15.179 Re-compile (Hammersley)
!    20-Oct-2011: V15.178 Add PowerCIF as input format (Hammersley)
!    18-Oct-2011: V15.177 Output of integration parameters to file added. Add
!      "FULLPROF" output format (Hammersley)
!    07-Oct-2011: V15.176 Implement over-ride of wavelength and detector
!      distance (Hammersley)
!    03-Oct-2011: V15.175 Investigating failure on start-up (Hammersley)
!    29-Sep-2011: V15.174 Solved crash (too long string in LG_X11_TEXT). 
!      Developing LUT method for cake and integration (Hammersley)
!    29-Sep-2011: V15.173 Investigating crash (Hammersley)
!    21-Sep-2011: V15.172 Add "main" and "memory" (Hammersley)
!    15-Sep-2011: V15.171 Use "Len_name" for file name lengths (Hammersley)
!    15-Sep-2011: V15.170 Fix "MA_MASKGROW" (Hammersley)
!    14-Sep-2011: V15.169 Changes to "MA_MASKGROW" (Hammersley)
!    10-Feb-2011: V15.149 -gmm version for SAXS interface (Hammersley)
!    01-Feb-2011: V15.148 Changes to IO routines (Hammersley)
!    26-Jan-2011: V15.147 Further changes to FIO routines (Hammersley)
!    20-Jan-2011: V15.146 Changes to FIO routines (Hammersley)
!    14-Jan-2011: V15.145 Changes to SAXS interface (Hammersley)
!    05-Jan-2011: V15.144 2011 Version (Hammersley)
!    12-Nov-2010: V15.136 Acknowledge CBFLIB use (Hammersley)
!    12-Nov-2010: V15.136 Acknowledge CBFLIB use (Hammersley)
!    02-Nov-2010: V15.135 Changes to BSL input (Hammersley)
!    26-Oct-2010: V15.134 Cope with comment lines (start with #) at end of BSL
!      header files (Hammersley)
!    22-Oct-2010: V15.133 Changes to BSL input routine (Hammersley)
!    20-Oct-2010: V15.132 Changes to GS routines (Hammersley)
!    07-Oct-2010: V15.131 Version to allow graphical menus to work in "No 
!     Graphics" mode (Hammersley)
!    01-Oct-2010: V15.130 Changes to GS routines (Hammersley)
!    28-Sep-2010: V15.129 Changes to MA routines (Hammersley)
!    20-Oct-2010: V15.128 Changes to FIO routines (Hammersley)
!    06-Oct-2010: V15.127 Changes to GS routines (Hammersley)
!    12-Aug-2010: V15.126 Changes to IO routines (Hammersley)
!    05-Aug-2010: V15.125 Re-compile for Linux (Hammersley)
!    15-Jul-2010: V15.124 Changes to GS routines (Hammersley)
!    08-Jul-2010: V15.125 Changes to GS routines (Hammersley)
!    01-Jul-2010: V15.122 Changes to FIO routines (Hammersley)
!    28-Jun-2010: V15.121 Changes to IO routines (Hammersley)
!    05-Feb-2010: V15.108 Correct "MEMORY NOT DEFINED" bug for "MATHS" 
!      sub-menu within the SAX/GISAXS interface (Hammersley)
!    01-Feb-2010: V15.107 Re-write of same peak identification routine 
!      (Hammersley) 
!    26-Jan-2010: V15.106 Testing same peak identification routine (Hammersley)
!    22-Jan-2010: V15.105 Debugging same peak routine (Hammersley)
!    18-Jan-2010: V15.104 Add same peak identification routine (Hammersley)
!    12-Jan-2010: V15.103 Add peak sorting routine (Hammersley)
!    05-Jan-2010: V15.102 2010 Edition (Hammersley)
!    15-Dec-2009: V15.101 Changes to "FIO" routines (Hammersley)
!    03-Dec-2009: V15.100 Changes to "GS" routines (Hammersley)
!    18-Nov-2009: V15.099 Test Macro input (Hammersley)
!    06-Nov-2009: V15.098 Use "SEqvNoCase" to make graphics menus case 
!      insensitive e.g. for macro input (Hammersley)
!    15-Oct-2009: V15.095 Debugging problem with macro output of "PowderCIF"
!      format data (Hammersley)
!    14-Aug-2009: V15.081 Recompile with Intel Fortran 11.1.038 (Hammersley)
!    25-Jun-2009: V15.080 Add to "STATISTICS" command, no coordinate input
!      corresponding to statistics on the whole ROI (Hammersley)
!    16-Jun-2009: V15.079 Remove file I/O debug messages, as problem seems to be
!      compiler /system related (Hammersley)
!    11-Jun-2009: V15.078 Correct "edf" error message when file to be input is 
!      missing. (Network file system problem now suspected for "COMPOSITE"
!      problems. But No! There is a problem with NFS, but locally copied files
!      lead to same problem on Windows, but not on Linux.) (Hammersley)
!    08-Jun-2009: V15.077 Re-compile everything in debug mode (Hammersley)
!    03-Jun-2009: V15.076 Testing "C" "fopen" routine (Hammersley)
!    20-May-2009: V15.075 Continue debugging "COMPOSITE" crash after 3782 
!      images ! (Hammersley)
!    19-May-2009: V15.074 Debugging "COMPOSITE" crash after 3782 images !
!      (Hammersley)
!    18-May-2009: V15.073 Temporarily remove "COMPOSITE" rebinning code for 
!      test (Hammersley)
!    12-May-2009: V15.072 Investigating problems with very larger "COMPOSITE" 
!      pictures (Hammersley)
!    05-May-2009: V15.071 Tidy up "File Series" code (Hammersley)
!    04-May-2009: V15.070 Debugging crash in "COMPOSITE" (Hammersley)
!    21-Apr-2009: V15.069 Increase size of file store to 1048576 characters to
!      allow input from directories with thousands of files (28000 present 
!      record!) (Hammersley)
!    20-Apr-2009: V15.068 Changes to "GS" routines (Hammersley)
!    09-Apr-2009: V15.067 Solved and corrected crash when outputting CBF same 
!      size as arrays (Hammersley)
!    08-Apr-2009: V15.066 Checking crash when outputting CBF same size as arrays
!      (Hammersley)
!    03-Mar-2009: V15.065 Write "Klora" files with data type = "FLOATVALUE"
!      (Hammersley)
!    27-Jan-2009: V15.060 Cope with "mar2560" files (Hammersley)
!    23-Jan-2009: V15.059 Re-instate "SINGLE CRYSTAL" interface (Hammersley)
!    12-Jan-2009: V15.058 2009 edition ! (Hammersley)
!    22-Dec-2008: V15.057 Testing Mac macro error (Hammersley)
!    10-Dec-2008: V15.056 Changes to peak search routines (Hammersley)
!    01-Dec-2008: V15.055 Remove temporarily "SINGLE CRYSTAL" interface
!      (Hammersley) 
!    27-Nov-2008: V15.054 Changes to input routines (Hammersley) 
!    17-Nov-2008: V15.053 Option of 1-D ASCII in GUI input (Hammersley)
!    12-Nov-2008: V15.052 Changes in MA routines (Hammersley)
!    06-Nov-2008: V15.051 Changes in IO routines (Hammersley)
!    24-Oct-2008: V15.050 Changes in IO routines (Hammersley)
!    20-Oct-2008: V15.049 Testing macro input (Hammersley)
!    17-Oct-2008: V15.048 Investigate problem with "O.K" return not being 
!      recorded in "GS_INPS" calls (Hammersley)
!    14-Oct-2008: V15.047 Continue simpler volume definition in RSM
!      (Hammersley)
!    08-Oct-2008: V15.046 Option for simpler volume definition in RSM
!      (Hammersley)
!    02-Oct-2008: V15.045 Re-compile for BM28 (Hammersley)
!    26-Sep-2008: V15.044 Changes to Single crytal interface (Hammersley)
!    16-Sep-2008: V15.043 Changes to RSM (Hammersley)
!    10-Sep-2008: V15.042 Re-compile for Rennes users (Hammersley)
!    26-Aug-2008: V15.041 Changes to IO library (Hammersley)
!    22-Aug-2008: V15.040 Changes to IO library (Hammersley)
!    19-Aug-2008: V15.039 Changes to peak search routines (Hammersley)
!    24-Jul-2008: V15.038 Re-compile for Linux (Hammersley)
!    24-Jun-2008: V15.037 Investigating crash on Linux macros (Hammersley)
!    09-Jun-2008: V15.036 Re-build for Linux (Hammersley)
!    02-Jun-2008: V15.035 Remove "Single Crystal" interface temporarily for 
!      testing of Linux macro problem (?) (Hammersley)
!    16-May-2008: V15.030 Add tri-clinic lattice drawing calculations
!      (Hammersley)
!    14-May-2008: V15.030 Changes to lattice drawing (Hammersley)
!    16-Apr-2008: V15.025 Add lattice option to 3-D peaks (Hammersley)
!    15-Apr-2008: V15.024 Debugging "GS_INP_MENUCHOICE" (Hammersley)
!    14-Apr-2008: V15.023 Debugging "GS_INP_MENUCHOICE" (Hammersley)
!    11-Apr-2008: V15.022 Debugging "LG_INP_EVENT" (Hammersley)
!    09-Apr-2008: V15.021 Testing "GS_INP_MENUCHOICE" (Hammersley)
!    08-Apr-2008: V15.020 Allow 3-D peak to be rotated with mouse button down 
!      (Hammersley)
!    20-Mar-2008: V15.019 Changes to "TRANSFORM" output region to avoid crash
!      when the memory has not been previously used (Hammersley)
!    19-Mar-2008: V15.018 Investigate crash with "TRANSFORM" (Hammersley)
!    07-Mar-2008: V15.016 Re-compile for Red Hat Enterprise Linux (Hammersley)
!    04-Mar-2008: V15.015 Changes to 3-D peak viewer (Hammersley)
!    18-Feb-2008: V15.014 Changes to "IO" routines (Hammersley)
!    12-Feb-2008: V15.013 Test "img" input (Hammersley)
!    07-Feb-2008: V15.012 Tweak peak search algorithm (Hammersley)
!    28-Jan-2008: V15.011 Changes to "F2D" and "FOI" routines (Hammersley)
!    15-Jan-2008: V15.010 Output position of data maximum in statistics command
!      (Hammersley)
!    07-Jan-2008: V15.009 2008 edition ! (Hammersley)
!    19-Dec-2007: V15.008 Fix error in LG event input (Hammersley)
!    12-Dec-2007: V15.007 Changes to GS event input (Hammersley)
!    10-Dec-2007: V15.006 Changes to LG library (Hammersley)
!    04-Dec-2007: V15.005 Changes to I/O library (Hammersley)
!    22-Nov-2007: V15.004 Speed-up 3-D peak viewer (Hammersley)
!    18-Nov-2007: V15.003 Change default scale of 3-D peak display (Hammersley)
!    09-Nov-2007: V15.002 Correct peak positions (Hammersley)
!    26-Oct-2007: V15.001 New peak search algorithm (Hammersley)
!    17-Oct-2007: V14.118 Add "SIGNIFICANT PIXELS" command to "MASK" menu
!      (Hammersley) 
!    12-Oct-2007: V14.117 Increase "title", and label sizes from 80 characters 
!      to 256 characters (Hammersley)
!    01-Oct-2007: V14.116 Correct command list on keyboard menu (Hammersley)
!    28-Sep-2007: V14.115 Test "LOAD MASK" command (Hammersley)
!    18-Sep-2007: V14.114 Add "LOAD MASK" command to keyboard interface 
!      (Hammersley)
!    10-Sep-2007: V14.113 Fix bug in peak search (Hammersley)
!    04-Sep-2007: V14.112 Changes to "F2D" routines (Hammersley)
!    29-Aug-2007: V14.111 Changes to "F2D" routines (Hammersley)
!    23-Aug-2007: V14.110 Changes to MA routines (Hammersley)
!    01-Aug-2007: V14.109 Increase size of command line arguments to 1024 
!      characters (previously 80 characters) (Hammersley)
!    19-Jun-2007: V14.108 Testing exit from graphics input (Hammersley)
!    18-Jun-2007: V14.107 Add "Size" keyword to "Klora" format output
!      (Hammersley)
!    14-Jun-2007: V14.106 Investigating problem inputting "Klora" format file 
!      produced by FIT2D ! (Hammersley)
!    04-Jun-2007: V14.105 Problem inputting "Klora" file (Hammersley)
!    29-May-2007: V14.104 Re-compile for Mac-OSX (Hammersley)
!    22-May-2007: V14.103 Fix problem in "IO_FILETYPE" (Hammersley)
!    18-May-2007: V14.102 Re-compile Linux version (Hammersley)
!    14-May-2007: V14.101 Problem on opening graphics on fail-safe session but 
!      not KDE session ? Investigating (Hammersley)
!    09-May-2007: V14.100 Re-compile on Linux system (Hammersley)
!    24-Apr-2007: V14.099 Blank graphics on Intel Mac ? (Hammersley)
!    17-Apr-2007: V14.098 Corrected routine to find home directory for Intel
!      Macintosh (Hammersley) 
!    16-Apr-2007: V14.097 Investigateing Mac "Bus error" on exit (Hammersley)
!    13-Apr-2007: V17.096 Re-compile and link for Intel Mac (Hammersley)
!    27-Mar-2007: V14.095 Re-link with new CBF library (Hammersley)
!    23-Mar-2007: V14.094 Test input of new CBF file (unsupport compression)
!      (Hammersley)
!    21-Mar-2007: V14.093 Changes to "RSM" interface (Hammersley)
!    16-Mar-2007: V14.092 Remove diagnostics from peak search (Hammersley)
!    12-Mar-2007: V14.091 Testing (Hammersley)
!    02-Mar-2007: V14.091 Add diagnostics to peak search (Hammersley)
!    27-Feb-2007: V14.090 Changes to peak search algorithm (Hammersley)
!    19-Feb-2007: V14.089 "F2D_GUI_ZSCALE" fixed (Hammersley)
!    16-Feb-2007: V14.088 Problem with "F2D_GUI_ZSCALE" (Hammersley)
!    12-Feb-2007: V14.087 Testing new peak search code (Hammersley)
!    07-Feb-2007: V14.086 Re-compile with CBFlib 0.7.7 (Hammersley)
!    01-Feb-2007: V14.085 Update I/O library (Hammersley)
!    29-Jan-2007: V14.084 Test new RSM code (Hammersley)
!    23-Jan-2007: V14.083 Use hacked CBFlib to write real array CBF's 
!      (Hammersley)
!    19-Jan-2007: V14.082 Save real arrays in CBF format (Hammersley)
!    18-Jan-2007: V14.081 Add Herb's code for reference beam-centre (Hammersley)
!    12-Jan-2007: V14.080 Input defaults from CIF file for hkl clicking in "RSM"
!      (Hammersley)
!    10-Jan-2007: V14.079 Add "DISPLAY" to "RSM" interface (Hammersley) 
!    05-Jan-2007: V14.078 Change calculation of "thickness" in RSM which was 
!      too small (Hammersley)
!    20-Dec-2006: V14.077 Transpose UB-matrix input from CIF files (previously
!      was wrong) (Hammersley)
!    18-Dec-2006: V14:076 (Limited) User version (Hammersley)
!    13-Dec-2006: V14:075 Still test / debugging peak search (Hammersley)
!    09-Dec-2006: V14.074 Debugging peak search (Hammersley)
!    07-Dec-2006: V14.073 Testing peak search (Hammersley)
!    30-Nov-2006: V14.072 New peak search code (Hammersley)
!    22-Nov-2006: V14.071 Remove background subtraction from "EX" peak search
!      (Hammersley)
!    16-Nov-2006: V14.070 User test version (Hammersley)
!    15-Nov-2006: V14.069 Add "EXTREME XTALLOGRAPHY" interface (Hammersley)
!    13-Nov-2006: V14.068 Add "SAVE VOLUME" command to RSM interface 
!      (Hammersley)
!    10-Nov-2006: V14.067 "ROLLING BALL" filter tested. Re-instate "RSM"
!      interface (Hammersley)
!    08-Nov-2006: V14.066 Re-instate "ROLLING BALL" filter (Hammersley)
!    07-Nov-2006: V14.065 Testing 1-D sort (Hammersley)
!    27-Oct-2006: V14.064 Optimised version (Hammersley)
!    25-Oct-2006: V14.063 Compile in debug mode (Hammersley)
!    20-Oct-2006: V14.062 Changes to "F2D_1D_SORT" (Hammersley)
!    16-Oct-2006: V14.061 Debug Sort (Hammersley)
!    12-Oct-2006: V14.060 Add "1-D SORT" command (Hammersley) 
!    10-Oct-2006: V14.059 Re-compile for users (Hammersley)
!    25-Sep-2006: V14.058 Add selection of background image (Hammersley)
!    22-Sep-2006: V14.057 Add dark current subraction from "RSM" (Hammersley)
!    21-Sep-2006: V14.056 Correct input of Y-direction pixel size from 
!      auxilliary CIF file (Hammersley)
!    20-Sep-2006: V14.055 Re-instate "RECIPROCAL SPACE MAPPING" interface
!      (Hammersley)
!    19-Sep-2006: V14.054 User version (not tested) (Hammersley)
!    18-Sep-2006: V14.053 Set default values for monitor and attenuation not
!      set (Hammersley)     
!    17-Sep-2006: V14.052 Add "monitor" and "attenuation" items to
!      "F2D_INPUT_EXPERIMENT" (Hammersley)
!    15-Sep-2006: V14.051 Input "diffrn_attenuator.scale" if possible 
!      (Hammersley)
!    14-Sep-2006: V14.050 Add "monitor" and "attenuation" items to
!      "F2D_INP_DATASTORE" (Hammersley)
!    12-Sep-2006: V14.049 Input "diffrn_source.current" if possible (Hammersley)
!    11-Sep-2006: V14.048 Add support for monitor and attenuation normalisation
!      to "RECIPROCAL SPACE MAPPING" code (Hammersley)
!    09-Sep-2006: V14.047 User release (Hammersley)
!    06-Sep-2006: V14.046 Correct "F2D_RMAP_TRANSFORM" (Hammersley)
!    01-Aug-2006: V14.045 Changes to "EXPERIMENTAL_DETAILS" structure 
!      (Hammersley)
!    29-Aug-2006: V14.044 Add rotation about beam (Hammersley)
!    25-Aug-2006: V14.043 Debug reciprocal mapping (Hammersley)
!    28-Jul-2006: V14.042 Test version with "RECIPROCAL SPACE MAPPING" 
!      interface  (Hammersley)
!    27-Jul-2006: V14.041 User version without "RECIPROCAL SPACE MAPPING" 
!      interface  (Hammersley)
!    26-Jul-2006: V14.040 Changes to FIO_FILEIO routines (Hammersley)
!    24-Jul-2006: V14.039 Testing BSL file input (Hammersley)
!    19-Jul-2006: V14.038 Re-remove "RECIPROCAL SPACE MAPPING" interface and
!      debug problem inputting BSL files (Hammersley)
!    13-Jul-2006: V14.037 Changes to IO system (Hammersley)
!    10-Jul-2006: V14.036 Changes to "RECIPROCAL SPACE MAPPING" transform
!      (Hammersley)
!    07-Jul-2006: V14.035 Re-instate "RECIPROCAL SPACE MAPPING" interface 
!      (Hammersley)
!    06-Jul-2006: V14.034 Temporaily remove "RECIPROCAL SPACE MAPPING" interface
!      (Hammersley)
!    29-Jun-2006: V14.033 Investigate non-working input of TIFF sequence images,
!      apart from the first one (Hammersley)
!    23-Jun-2006: V14.032 Stop reciprocal map size being reduced (Hammersley)
!    22-Jun-2006: V14.031 Make reciprocal space mapping user questions
!      consistent (Hammersley) 
!    20-Jun-2006: V14.030 Add input of left-right flipping when calculating 
!      UB-matrix from images (Hammersley)
!    14-Jun-2006: V14.029 New version CBFLIB (Hammersley)
!    09-Jun-2006: V14.028 Test transform (Hammersley)
!    06-Jun-2006: V14.027 Changes to I/O routines (Hammersley)
!    30-May-2006: V14.026 Re-compile (Hammersley)
!    12-May-2006: V14.025 Test CIF beam centre input (Hammersley)
!    09-May-2006: V14.024 Enable mirror image code (Hammersley)
!    28-Apr-2006: V14.023 Add masking to "RECIPROCAL SPACE MAPPING" interface
!      (Hammersley)
!    27-Apr-2006: V14.022 Remove Mar top-bottom flipping (Hammersley)
!    25-Apr-2006: V14.021 Correct setting of number of colour table levels
!      (Hammersley)
!    24-Apr-2006: V14.020 Input "experiment" details for Mar format files
!      (Hammersley)
!    21-Apr-2006: V14.019 Correct error in directory path name (Hammersley)
!    20-Apr-2006: V14.018 UB matrix from clicked 2-D peaks implemented
!      (Hammersley)
!    19-Apr-2006: V14.017 Test changes to UB calculation (Hammersley)
!    18-Apr-2006: V14.016 "Slen" removed completely (Hammersley)
!    12-Apr-2006: V14.015 Remove "Slen" from FIO routines (Hammersley)
!    11-Apr-2006: V14.014 Remove "Slen" from FIT2D routines (Hammersley)
!    10-Apr-2006: V14.013 Debug new indexing code (Hammersley)
!    05-Apr-2006: V14.012 Test updated I/O routines (Hammersley)
!    04-Apr-2006: V14.011 Update IO routines (Hammersley)
!    03-Apr-2006: V14.010 Use F90 version of "maketext" (Hammersley)
!    28-Mar-2006: V14.009 Correct input of diffractometer angles in "RECIPROCAL
!      MAPPING INTERFACE" (Hammersley)
!    28-Mar-2006: V14.008 Changes to dynamic memory allocation (Hammersley)
!    23-Mar-2006: V14.007 Changes to allow for vertical look-up tables
!      (Hammersley)
!    22-Mar-2006: V14.006 Debugging resizing problems (Hammersley)
!    21-Mar-2006: V14.005 Changes to dynamic memory allocation (Hammersley)
!    20-Mar-2006: V14.004 Add "INPUT OPTIONS" support (Hammersley)
!    17-Mar-2006: V14.003 Window position remembered. (Hammersley)
!    16-Mar-2006: V14.002 Correct output of two theta angle. Window close button
!      is now disabled. (Hammersley)
!    15-Mar-2006: V14.001 Testing changes (Hammersley)
!    13-Mar-2006: V14.000 Many changes owing to use of "experiment" structure
!      to pass experimental details throughout program (Hammersley)
!    10-Mar-2006: V13.018 Control input of auxiliary information from
!      CBF/CIF files (Hammersley)
!    09-Mar-2006: V13.017 Input UB matrix from CIF's (Hammersley)
!    08-Mar-2006: V13.016 Get sense of images from CBF files for reciprocal 
!      space mapping (Hammersley)
!    06-Mar-2006: V13.015 Test output of CBF's (Hammersley)
!    04-Mar-2006: V13.014 Convert real data to integer and vice versa for CBF's
!      (Hammersley)
!    03-Mar-2006: V13.013 Add option to output CBF file with optional
!      auxiliary information (Hammersley)
!    02-Mar-2006: V13.012 New version of CBF library. Input pixel sizes from 
!      CBF files (Hammersley)
!    26-Feb-2006: V13.011 Add code to input pixel sizes for CBF files
!      (Hammersley)
!    24-Feb-2006: V13.010 Input pixel size for ADSC files (Hammersley)
!    23-Feb-2006: V13.009 Input more experiemental geometry information from
!      CIF file (Hammersley)
!    22-Feb-2006: V13.008 Use data structure for experiemental geometry input
!      (Hammersley)
!    18-Feb-2006: V13.007 Use mixed "C" / Fortran I/O libraries (Hammersley)
!    16-Feb-2006: V13.006 Add "FILE INPUT GEOMETRY" option to "SETUP" interface
!      (Hammersley)
!    15-Feb-2006: V13.005 Input auxiliary files in "MAP SERIES" (Hammersley)
!    13-Feb-2006: V13.004 Re-compile for DA1 (Hammersley)
!    10-Feb-2006: V13.003 Windows Intel compiler version (Hammersley)
!    01-Feb-2006: V13.002 Windows Fortran-90 version (Hammersley)
!    23-Jan-2006: V13.001 Fortran-90 version (Hammersley)

!    19-Aug-1998: V10.0 Replacement of all "SYMBOL" routines with
!      "VARIABLE" routines, so now all internal variables have
!      a type and are stored in integer, logical, floating point,
!      or character string form (Hammersley)
!    21-Aug-1998: V10.1 Change name of keyboard commands
!      "DEFINE SYMBOL", "LIST SYMBOLS" etc. to "DEFINE VARIABLES",
!      "LIST VARIABLES", etc. (Hammersley)
!    24-Aug-1998: V10.2 Correct "DEFINE VARIABLES" command (Hammersley)
!    25-Aug-1998: V10.3 command-line options "-dform" and "-dmenu"
!      added to allow control of saving forms and menus to PostScript
!      files (Hammersley)
!    28-Aug-1998: V10.4 Stop warning message being produce when
!      inputting TIFF files (Hammersley)
!    31-Aug-1998: V10.5 Save experimental geometry changes in internal
!      data-base. Hopefully this will stop strange re-settings of
!      the sample to detector distance (Hammersley)
!    04-Sep-1998: V10.6 Explicitly output mask or not when outputing
!      to a PostScript file. Previously this was crashing on SGI 5.3
!      and giving strange output on Solaris. PostScript image output
!      with log scale and masking implemented. (Hammersley)
!    25-Sep-1998: V10.7 Correct values set to internal program variables
!      by the GUI "STATISTICS" command. Correct GUI "INPUT" routine
!      so that the "GAS 2-D" format is automatically recognised again.
!      Output equivalent D-spacing when user clicks on a 1-D 2-theta
!      scan with the experimental geometry set (Hammersley)
!    30-Sep-1998: V10.8 Changes made for Windows 95 version (Hammersley)
!    05-Oct-1998: V10.9 Changes to graphics to try to prevent memory
!      "leak" on WIN32 (Hammersley)
!    09-Oct-1998: V10.10 Correct value of loop counter in macro files.
!      (Previously since V10.1 the first time round the loop the
!      counter value was always 0, and not the desired value.)
!      Treat blanks within quoted strings as significant. Recognise
!      indented variables i.e. indented within macro DO loops.
!      Improve output of "LIST VARIABLES" command; the length of
!      defined strings is now output. (Hammersley)
!    19-Oct-1998: V10.11 "F2D_VARIABLE" replaced by "IO_VARIABLE" 
!      (Hammersley)
!    22-Oct-1998: V10.12 Memory leaks related to X-11 image display
!      removed. If fitted Voigtian sigma or HWHM parameters were
!      set below zero by the fitting algorithm they are set and
!      displayed as zero. Extra GUI warning message added if fitting
!      does not exit properly. "MFIT" residuals graph is displayed again
!      properly (error introduced in V10.8) (Hammersley)
!    23-Oct-1998: V10.13 Calculate "Voigtian" integrated intensity
!      using Lorentzian formula when the "sigma" parameter is zero (Hammersley)
!    23-Oct-1998: V10.14 Save beam centre position in internal
!      data-base when set using "BEAM CENTRE". (Previously this
!      was sometimes forgotten, and had to be reset later.)
!      Stop Windows NT version crashing when trying to display
!      1-D graphs with negative values with Log scaling. (Hammersley)
!    26-Oct-1998: V10.15 Make command-line expression interpreter
!      cope properly with minus signs which are part of negative
!      exponentials of numeric constants. Allow quoted string input
!      input from the command line (On Unix the single quotes have to
!      be escaped.) (Hammersley)
!    27-Oct-1998: V10.16 Save pixel sizes in internal data-base and
!      recover on start-up. "INPUT (1-D)" command added to "FILE
!      SERIES" interface to allow the input of a series of 1-D
!      data-sets to form a 2-D "image" e.g. for viewing the output
!      of the "FILE SERIES" "INTEGRATE" command (Hammersley)
!    28-Oct-1998: V10.17 Add "SCALED SUB" option to "MATHS" menu
!      to allow interactive scaled subtraction of the memory data
!      from the current data (Hammersley)
!    29-Oct-1998: V10.18 Add "VECTORS" array to FIT2D. Allow
!      "MFIT" to save the results of fits to the vectors array,
!      and add "VECTORS" viewing command (Hammersley)
!    30-Oct-1998: V10.19 Improve calculation of Voigtian FWHM.
!      MFIT fitting now automatically fits in both directions from
!      a choosen initialisation row. The fit model parameters can
!      be allowed to evolve or not (Hammersley)
!    02-Nov-1998: V10.20 Change "VECTORS" command to use full menu
!      for "INPUT"/"OUTPUT" and display of 1-D vectors (Hammersley)
!    02-Nov-1998: V10.21 Correct generation of file names when
!      there is a post-fix component prior to the extension (Hammersley)
!    03-Nov-1998: V10.22 Remove all calls to "GS_SET_MESSAGESTYLE" and set 
!      default to black text on white background (Hammersley)
!    21-Nov-1998: V10.23 Stop Linux version crashing when the
!      default is accepted in the keyboard interface. Generate
!      "SEQUENCE" variable names correctly and allow data type to be
!      specified by the user, and support direct generation of
!      floating point values. (Hammersley)
!    25-Nov-1998: V10.24 Add new colour table (Black-Purple-Magenta-
!      Red-Orange-Yellow-White). Make "FIT" menu take beam centre
!      set in other interfaces by default. (Hammersley)
!    27-Nov-1998: V10.25 "TRANSFORM" command added to "IMAGE
!      PROCESSING" interface to allow easy rotation and translation
!      of images, using most appropriate algorithm automatically.
!      Remove "-static" from Linux link since S.u.S.e. crashes on
!      RedHat when "getpwuid" is called using static code ! (Hammersley)
!    28-Nov-1998: V10.26 Set help messages to fixed width font,
!      so that characters line up. Reduce X11 output by filtering
!      out attribute commands which are already set (should be
!      noticably faster on slow X-terminals). (Hammersley)
!    29-Nov-1998: V10.27 Add "REFLECT" command to GUI "IMAGE
!      PROCESSING" menu (Hammersley)
!    01-Dec-1998: V10.28 Add "PowderCIF" output format (Hammersley)
!    02-Dec-1998: V10.29 Calculate and output an R-factor value
!      for MFIT fits (Hammersley)
!    03-Dec-1998: V10.30 Use "Purify" on WNT version and remove
!      memory leaks and uninitialised memory reads (Hammersley)
!    04-Dec-1998: V10.31 Correct Directory path style for Windows
!      systems (Hammersley)
!    09-Dec-1998: V10.32 Check "IO_READC" version is correct
!      for HP700 (Hammersley)
!    11-Dec-1998: V10.33 Changes to Windows image graphics output to
!      try to reduce memory loss problem on W95. Unfortunately, it
!      doesn't seem to help (Hammersley)
!    15-Dec-1998: V10.34 Internal data-base routines changed to IO
!      system routines (Hammersley)
!    16-Dec-1998: V10.35 Correct storage of "MFIT_ITERATIONS" value
!      in internal database. Previously was stored as an integer
!      although it should be a real (Hammersley)
!    17-Dec-1998: V10.36 Investigating file selection tool problem on
!      WNT. Sometimes directories are treated as files. Problem fixed
!      and entry of single character for hard drives added. (Hammersley)
!    18-Dec-1998: V10.37 Add code so that the Fuji BAS input routine
!      can cope with single byte per pixel data. Corrected input
!      of single byte binary data. (Hammersley)
!    23-Dec-1998: V10.38 Make Windows NT version update image when
!      the colour table is changed (since the colours are static,
!      the image must be re-drawn) (Hammersley)
!    28-Dec-1998: V10.39 Changes to X-11 image display code, so that
!      images will be displayed correctly on Linux 24-bit "TrueColor"
!      screens (Hammersley)
!    04-Jan-1999: V10.40 Output clicked coordinate position in two
!      click mode as well as for one-click mode (Hammersley)
!    05-Jan-1999: V10.41 Reduce character size relative to width
!      available for text, to improve output with Lan Workplace Pro (Hammersley)
!    06-Jan-1999: V10.42 Adjust character sizes to font widths to
!      allow larger characters in messages, etc. when the font
!      width is narrow, and smaller characters when the font width
!      is wider. Get "C" "gets" input working for Windows Version (Hammersley)
!    14-Jan-1999: V10.43 Re-convert all ".text" files with new
!      "maketext", ready for Fortran-90 conversion (Hammersley)
!    19-Jan-1999: V10.44 "DISPLAY" "PROJECTION" command use new
!      simpler and faster code to check if pixels are within the
!      rectangular region. Restructure minimisation routines for
!      conversion to Fortran-90 (Hammersley)
!    22-Jan-1999: V10.45 Reduce size of fixed width font text
!      relative to maximum width. PostScript support for fixed width
!      fonts (Hammersley)
!    25-Jan-1999: V10.46 Add new ID-1 machine (Hammersley)
!    27-Jan-1999: V10.47 Don't reflect horizontally TIFF files on
!      input, only Molecular dynamics ImageQuant files. (Previously
!      all were reflected, which meant that FIT2D output was input
!      left - right reflected.) (Hammersley)
!    29-Jan-1999: V10.48 Changes to "GS_TEXT" so that a choice
!      of fonts can be used (Hammersley)
!    03-Feb-1999: V10.49 Add ID-9 machine, changes to FFT code (Hammersley)
!    15-Feb-1999: V10.50 Add new PC (Hammersley)
!    17-Feb-1999: V10.51 File input now in FIO module (Hammersley)
!    24-Feb-1999: V10.52 All data-base saving and recovering routines
!      renamed (Hammersley)
!    25-Feb-1999: V10.53 Use "GS_DRAW_CHARACTER" to create banner (Hammersley)
!    26-Feb-1999: V10.54 Minor changes for F90 version (Hammersley)
!    27-Feb-1999: V10.55 F90 version starts to use modules to give
!      interface checking (Hammersley)
!    01-Mar-1999: V10.56 "PAUSE" command now works properly on WIN32 
!      (Hammersley)
!    02-Mar-1999: V10.57 "SET GEOMETRY" command now corrected so that
!      the values are stored within the internal data-base (Hammersley)
!    03-Mar-1999: V10.58 Extra ID-11 systems added, and changes made
!      to file selection component to improve support for Windows
!      and the case that a text string contains a directory path (Hammersley)
!    19-Mar-1999: V10.59 Checking input of FIT2D format file (Hammersley)
!    25-Mar-1999: V10.60 Get X-11 output on 24-bit eXceed 6.1 correct
!      (Hammersley)
!    26-Mar-1999: V10.61 Change raster graphics line drawing algorithm
!      (Hammersley)
!    09-Apr-1999: V10.62 Add option to "CALIBRATION" menu to create false peaks
!      (Hammersley)
!    13-Apr-1999: V10.63 Add choice of Alumina (Al2O3) to calibrant list. Make 
!      binary output header files save the binary file name without any path,
!      and make the input routines accept longer lines, and close the file 
!      properly in the case of an error. Stop possibility of spy-glass producing
!      error messages when in log scaling mode. Correct error in BSL input 
!      routine which meant that only the first image in a data-set was input 
!      whichever one was asked for. Remember size of program arrays between
!      calls. (Hammersley)
!    14-Apr-1999: V10.64 Add support for SRI-CAT system. Make 2-D polynomial 
!      initialisation work when complete rows are masked-off. (Hammersley)
!    16-Apr-1999: V10.65 Add "RE-BIN" option to "IMAGE PROCESSING" "GEOMETRIC" 
!      menu (Hammersley)
!    17-Apr-1999: V10.66 Add "2-D FITTING" interface (Hammersley)
!    29-Apr-1999: V10.67 Changes made to system inquiry routine, and "SPHERICAL 
!      MOD" command added to SAXS/GISAXS interface (Hammersley)
!    30-Apr-1999: V10.68 Correct bug introduced in V10.67 which stops FIT2D 
!      format data being input (Hammersley)
!    04-May-1999: V10.69 Output background message for "MACROS/LOG FILE" 
!      interface (Hammersley)
!    07-May-1999: V10.70 Fix problem introduced in V10.69 with inputting 
!      argument list on HP systems. Increase maximum number of strips which may 
!      be input in a TIFF image to 1000 (Hammersley)
!    11-Jun-1999: V10.71 Investigate problem inputting zoomed in
!    TIFF image in "FILE SERIES" "COMPOSITE" command (Hammersley)
!    11-Jun-1999: V10.72 Version for HP which doesn't use PA RISC 2.0
!    so will run O.K. on PA RISC 1.1 machines e.g. silvers (Hammersley)
!    13-Jul-1999: V10.73 EDF files, now recognised and input
!    automatically in GUI (Hammersley)
!    04-Aug-1999: V10.74 Investigating problem of inputting two
!    particular old FIT2D format files (Hammersley)
!    09-Aug-1999: V10.75 Correct rotated text back to horizontal for
!    MFIT results output when fitted parameters are added to the
!    output (Hammersley)
!    18-Aug-1999: V10.76 New version for Alpha (Hammersley)
!    20-Sep-1999: V10.77 Increase maximum size of X-direction of internal 
!      arrays to allow input of very large 1-D arrays (Hammersley)
!    23-Sep-1999: V10.78 Correction to new Linux version to define size of 
!      Fortran record correctly (Hammersley)
!    24-Sep-1999: V10.79 Correct definition of machine in record length routine
!      so that Solaris version works properly e.g. for GSAS output (Hammersley)
!    29-Sep-1999: V10.80 Changes to GS library names (Hammersley)
!    30-Sep-1999: V10.81 Automatically remove trailing slashes from directory 
!      paths when user wants to climb directory structure. Automatically climb 
!      directory structure when default directory doesn't exist (which happens 
!      because of stored default directories being removed.) (Hammersley)
!    01-Oct-1999: V10.82 Checking input of "KLORA" files on Linux systems 
!      (Hammersley)
!    12-Oct-1999: V10.83 Use new "C" based I/O system for inputting
!      "KLORA" files, and move routines to FIO (Hammersley)
!    15-Nov-1999: V10.84 New distribution, addition of by-pass system 
!      (Hammersley)
!    18-Nov-1999: V10.85 Changes to names of graphics routines (Hammersley)
!    22-Nov-1999: V10.86 Further changes to graphics routines (Hammersley)
!    25-Nov-1999: V10.87 Further changes to graphics routines (Hammersley)
!    30-Nov-1999: V10.88 Attach icon to Windows version and further changes to
!      graphics (Hammersley)
!    09-Dec-1999: V10.89 Change to "LG" routines (Hammersley)
!    21-Dec-1999: V10.90 Investigate problem in "FILE SERIES" "COMPOSITE" 
!      command when selecting a ROI and inputting a background image 
!      (Hammersley)
!    14-Apr-2000: V10.91 Changes to library names (Hammersley)
!    17-Apr-2000: V10.92 All "EXPG_MA_*" routine renamed to "MA_*" (Hammersley)
!    19-Apr-2000: V10.93 All "EXPG_IO_*" routine renamed to "IO_*" (Hammersley)
!    23-May-2000: V10.94 Changes to "IO" routines (Hammersley)
!    26-May-2000: V10.95 Remove key protection (Hammersley)
!    27-Jul-2000: V10.96 Problem with command-line arguments should hopefully be
!      cured (Hammersley)
!    03-Aug-2000: V10.97 Re-compile for all platforms (Hammersley)
!    13-Oct-2000: V10.98 Correct error which caused rotation to stall for 
!      rectangular arrays (Hammersley)
!    08-Nov-2000: V10.99 Faster output of 3-D surfacing (owing to faster output
!      of triangles) (Hammersley)
!    10-Nov-2000: V10.100 All "expg" references have been removed (Hammersley)
!    13-Nov-2000: V10.101 Debugging raster triangle drawing (Hammersley)
!    03-Feb-2001: V10.102 Investigating problem in "QUESTION" command when 
!      defining an input file. Only some of the characters are kept. The length
!      of input characters was not being stored. The "QUESTION" command is now 
!      fixed for "INPUT FILE" and "OUTPUT FILE" input. Try to only use static 
!      libraries for HP-UX-10 (problem with "libtermcap.sl") (Hammersley)
!    08-Feb-2001: V10.103 Change to graphics to try to stop resources loss
!      on Windows 95/98/ME systems (Hammersley)
!    09-Feb-2001: V10.104 Add "SUMMATION" command to "FILE SERIES"
!      interface. Add "MATHS" command to "SAX / GISAXS" interface (Hammersley)
!    17-Feb-2001: V10.105 Re-compile for "LINUX" to check out 24-bit
!      X-11 support (Hammersley)
!    19-Feb-2001: V10.106 Windows version: changes to graphics to reduce
!      (may be stop) resource losses. Slight changes to default colour
!      definitions in LG system (Hammersley)
!    21-Feb-2001: V10.107 Remove 24-bit X-11 diagnostic output (Hammersley)
!    22-Feb-2001: V10.108 Add support of 32-bits per pixel TIFF (Hammersley)
!    02-Mar-2001: V10.109 Debug sub-string problems found by Compaq
!      compiler (Hammersley)
!    05-Mar-2001: V10.110 Still debugging sub-string problems found by
!      Compaq compiler. Problem solved in "IO_ACCESS_DATABASE" (Hammersley)
!    13-Mar-2001: V10.111 Investigating 24-bit "Truecolor" output problem
!      on RedHat Linux system (Hammersley)
!    14-Mar-2001: V10.112 Correct display problem for 24-bit "Truecolor"
!      displays which use "little-endian" 4-bytes per pixel images e.g.
!      RedHat Linux 6.2 system (Hammersley)
!    02-Apr-2001: V10.113 Add support for direct input of Bruker format
!      files (although the file format is due to be changed !) (Hammersley)
!    03-Apr-2001: V10.114 Add input of overflow pixel values for the
!      Bruker format (Hammersley)
!    09-Apr-2001: V10.115 Option to output simple "KLORA" format file 
!      (Hammersley)
!    10-Apr-2001: V10.116 Change to copyright years (Hammersley)
!    20-Apr-2001: V10.117 Add option to "SAXS/GISAXS" menu to allow
!      the region of interest to be remembered between input images (Hammersley)
!    24-Apr-2001: V10.118 Re-compile for Sun Solaris to try to remove
!      shared libraries for Fortran (Hammersley)
!    04-May-2001: V10.119 Correct problem with input of Bruker format
!      when there is 1 byte per pixel (Hammersley)
!    18-Jun-2001: V10.120 Try to re-compile for Linux without "ncurses"
!      as a shared library (Hammersley)
!    19-Jun-2001: V10.121 Add support for direct input of ADSC detector
!      format files (Hammersley)
!    06-Jul-2001: V10.122 Add support for input of Rigaku R-AXIS format
!      (Hammersley)
!    12-Jul-2001: V10.123 Implementing input of Rigaku R-axis format 
!      (Hammersley)
!    13-Jul-2001: V10.124 Add input of Rigaku R-axis format to keyboard
!      menu (Hammersley)
!    18-Jul-2001: V10.125 Debugging error when translating image (Hammersley)
!    19-Jul-2001: V10.126 Testing image translation, problem in
!      defining axes after translation fixed (this only occured if the
!      output area was bigger than the defined data area) (Hammersley)
!    19-Jul-2001: V10.127 Change to BSL file format output, so now the
!      byte ordering is indicated (Hammersley)
!    20-Jul-2001: V10.128 Change BSL file format input to support either
!      byte ordering and 2-byte integer data types (Hammersley)
!    25-Jul-2001: V10.129 Option of user defined series of D-spacings
!      for "CALIBRANT" option (Hammersley)
!    27-Jul-2001: V10.130 Re-compile for IBM AIX (slight changes to
!      libraries) (Hammersley)
!    01-Aug-2001: V10.131 Backslashes ("\") should now appear properly
!      in PostScript output. (Previously they disappeared, or could
!      lead to newlines, tabs, and similar erroneous effects) (Hammersley)
!    06-Aug-2001: V10.132 Minor changes to GS library (Hammersley)
!    24-Aug-2001: V10.133 Change to using "c89" on Sun Solaris systems.
!      (Previously, "cc" was used which doesn't seem to be even ANSI)
!      (Hammersley)
!    27-Aug-2001: V10.134 Investigating problem inputting mar2300 file
!      (Paul Ellis's test file) (Hammersley)
!    10-Oct-2001: V10.135 Changed for Windows systems where the double
!      backslash escape didn't work. This works when the only the first
!      two characters are specificially compared, but not otherwise. This
!      is worrying ! The double backslash should now work as the escape
!      symbol for keyboard input (Hammersley)
!    11-Oct-2001: V10.136 Investigating crash on Windows 98 system when
!      initialising model for 2-D Gaussian. The number of returned
!      coordinates was being set, but was using the constant 1, so the
!      Windows compiler was right to complain; and the others ?
!      (Same problem existed in MFIT initialisation) (Hammersley)
!    18-Feb-2003: V11.000 Re-compile for Windows, and fix some "CANCEL"
!      problems found by Manolis Pantos (Hammersley)
!    11-Apr-2003: V11.001 Allow sample to detector distances of up to 100m
!      (Hammersley)
!    14-Apr-2003: V11.002 Correct bug(let) when 0 rotation and 0 translation
!    transform applied to data. Investigate problem inputting BSL files
!      with extension "308" (Hammersley)
!    15-Apr-2003: V11.003 Remove file input diagnostics and re-compile
!      optimised version (Hammersley)
!    27-May-2003: V11.004 Investigating problem with negative range angles
!      in Cake integration (Hammersley)
!    04-Jul-2003: V11.005 Investigate problem with peak search crash. In
!      fact only old version crashes, but no obvious changes have been
!      made to code (?). (Hammersley)
!    08-Jul-2003: V11.006 Change Mar input code owing to problem
!      inputting re-written Mar files i.e. files written by "automar".
!      Result: the Mar file header appears to be incorrect. It's coded for
!      uncompressed data, whereas the data is clearly compressed. According
!      to Claudio Klein all Mar345 files are compressed, so set compression
!      (Hammersley)
!    11-Jul-2003: V11.007 Change format in "f2d_cal2_cake" owing to Mac OSX
!      crash. Mac OSX user test version. Correct powder ring out-lier
!      rejection code, which was not rejecting the right positions (Hammersley)
!    16-Jul-2003: V11.008 Add recognition of ".mar1800" files (Hammersley)
!    21-Jul-2003: V11.009 Correct problem with cake not correctly re-binning
!    negative angle azimuth ranges (Hammersley)
!    22-Jul-2003: V11.010 Correct azimuthal range calculation for CAKE
!      command. Previously, angles before start were being forgotten
!      (Hammersley)
!    01-Aug-2003: V11.011 Problem inputting from directory with 7046 files
!    owing to limit of 65535 characters in string. Try increasing limit
!    to 200000 and check fail message (Hammersley)
!    06-Aug-2003: V11.012 Test inputting from directory with too many files
!      (Hammersley)
!    18-Aug-2003: V11.013 Check that the keyboard 2-theta integration gives
!      the same results as the GUI (Hammersley)
!    02-Oct-2003: V11.014 Re-compile for Windows-XP system (Hammersley)
!    10-Oct-2003: V11.015 Re-compile for Linux Suse 8.2 system (Hammersley)
!    22-Oct-2003: V11.016 Changes to "Io_input_key" (Hammersley)
!    29-Oct-2003: V11.017 Option of re-directing input to command for pipe
!      sub-process control (Hammersley)
!    31-Oct-2003: V11.018 Test re-directed input (Hammersley)
!    03-Nov-2003: V11.019 Correct command line input (Hammersley)
!    06-Nov-2003: V12.001 Change to use "Io_readline" on Unix systems
!      (Hammersley)
!    12-Nov-2003: V12.002 Option of giving identifer to shared memory segment 
!      (Hammersley)
!    13-Nov-2003: V12.003 Separate subroutine to Free memory (Hammersley)
!    18-Nov-2003: V12.004 Add mask array to shared memory arrays (Hammersley)
!    26-Nov-2003: V12.005 Correct problem in input of coordinates for
!      2-D fit model (Hammersley)
!    01-Dec-2003: V12.006 Debug shared memory use (Hammersley)
!    03-Dec-2003: V12.007 Change default so markers are not drawn (Hammersley)
!    12-Dec-2003: V12.008 Make sure image is re-drawn when help messages
!      are output during "CALIBRANT" choice (Hammersley)
!    15-Dec-2003: V12.009 Change references to "active data region" (ADR)
!      to refer to "region of interest" (ROI) (Hammersley)
!    22-Dec-2003: V12.010 Investigating problem inputting "FIT2D" format
!      file (Hammersley)
!    05-Jan-2004: V12.011 Minor syntax changes (Hammersley)
!    08-Jan-2004: V12.012 Change text referring to "active data region (ADR)"
!      the refer to "region of interest (ROI)" (Hammersley)
!    14-Jan-2004: V12.013 Remove diagnostics from FIT2D file input code,
!      problem was clearly corrupted file probably from using ftp from Unix
!      to PC in ASCII mode (Hammersley)
!    03-Feb-2004: V12.014 Correct file selection problem with Windows version
!      (Hammersley)
!    04-Feb-2004: V12.015 Output diagnostics to try to identify Exceed
!      versions which contain X-server information bug and those where this
!      has been corrected (Hammersley)
!    06-Feb-2004: V12.016 Try to correct eXceed X-server bug only for old
!      exceed versions (Hammersley)
!    11-Feb-2004: V12.017 Change method to find out byte offset values for
!      output of images. Now write primary colour rectangles to screen
!      and read back pixel values. This seems to be the only way to get
!      Hummingbird eXceed to tell the truth. Notably visualList.red_mask etc.
!      are set wrongly. (Hammersley)
!    19-Feb-2004: V12.018 Apparently still problems with eXceed. Compile
!      new version with diagnostics, so as to be sure of version (Hammersley)
!    20-Feb-2004: V12.019 Colours O.K, but character input is often not
!      being input correctly in the GUI. This however works on some X-servers
!      (Hammersley)
!    23-Feb-2004: V12.020 Further investigation of X-server character input
!      problem (Hammersley)
!    24-Feb-2004: V12.021 Investigation shows that something in the
!      "syntax.h" file is causing X-11 to screw up ! So use "WIN32" defined
!      to get rid of dubious defines. In fact the problem is from the stupid
!      way that X-11 defines and uses "True". It is defined as 1, and not
!      "~0" which is the "proper" ANSI-C way of doing things. This causes a
!      problem with event input, probably owing to "XSendEvent" using "True".
!      So for X-11 "True" is re-defined to 1. X-11 input now works (Hammersley)
!    25-Feb-2004: V12.022 Correct output region for spatial correction
!      of regions not starting at pixel 1. Correct crash owing to weak peak
!      algorithm suffering from rounding errors leading to calculation
!      of the square root of a negative number. Support right-hand side
!      menus for lanspace orientated windows. Fix error in "PROJECTION"
!      command for coordinate input (Hammersley)
!    26-Feb-2004: V12.023 Test spatial distortion correction for offset
!      images (Hammersley)
!    27-Feb-2004: V12.024 Add "BLOCK COPY" command to keyboard menu
!      for more efficient access to internal memories (Hammersley)
!    01-Mar-2004: V12.025 Set default value for output file name in
!      keyboard menu (Hammersley)
!    02-Mar-2004: V12.026 Fix start-up problem for X-11 version, as
!      colour table was not defined to default prior to creating graphics
!      window (Hammersley)
!    03-Mar-2004: V12.027 Correct left to right precedence ordering
!      of arithmetic operations in expression evaluations. Add "SET-UP"
!      interface with possibility to over-ride file header information.
!      Up-date internal help information. (Hammersley)
!    05-Mar-2004: V12.028 Correct non-cancelling cancel at start of cake
!      form input. Correct error message when files fail to input in
!      "FILE SERIES" commands. Correct colour table for X-11 8-bit
!      pseudo-color output (Hammersley)
!    09-Mar-2004: V12.029 Correct X-11 code for 8-bit "Pseudocolor" displays
!      (Hammersley)
!    10-Mar-2004: V12.030 Create mask at start-up, change in way the
!      vector array is defined internally (Hammersley)
!    12-Mar-2004: V12.031 Options to input and output mask files (Hammersley)
!    15-Mar-2004: V12.032 Testing mask input (Hammersley)
!    19-Mar-2004: V12.033 Correct user messages for "INTERNAL MEMORY" and
!      "BLOCK COPY" commands. Change "TRANSFORM" command so that defined
!      output region is not reduced by a small transform output region.
!      "SAVE MASK" implemented. Flip Mar image plate data top to bottom
!      to agree with Mar software orientation, and FreeThinker (Hammersley)
!    24-Mar-2004: V12.034 Add option in "CAKE" form to default always to one
!      degree azimuthal bins (Hammersley)
!    07-Apr-2004: V12.035 Add warning message when "edf" extension files
!      are not valid edf files. Correct input of ROI of TIFF files (Hammersley)
!    16-Apr-2004: V12.036 New user version (Hammersley)
!    30-Jun-2004: V12.037 Improve error reporting for "IO_READLINE"
!      to investigate problem of failure from perl script (but not
!      C-shell script) (Hammersley)
!    06-Jul-2004: V12.038 Test version for perl script problem (Hammersley)
!    15-Jul-2004: V12.039 New test version for perl script problem
!      (Hammersley)
!    19-Jul-2004: V12.040 Add "SET PIXEL VALUE" command to keyboard
!      interface. Add support in the "Klora" data format for unsigned
!      byte input. Add support for old Mar format images of 2300 x 2300
!      pixels and 3450 x 3450 pixels (Hammersley)
!    26-Jul-2004: V12.041 Add "DRAW CAKE" command to keyboard interface. 
!      (Hammersley)
!    24-Aug-2004: V12.042 Add "2-D ASCII" free format support in GUI
!      (Hammersley)
!    26-Aug-2004: V12.043 Add "2-D ASCII" output format (Hammersley)
!    31-Aug-2004: V12.044 Use "e" format for "2-D ASCII" output (Hammersley)
!    15-Sep-2004: V12.045 Parameter file I/O (Hammersley)
!    26-Oct-2004: V12.046 Option of integer output for "2-D ASCII" output
!      (Hammersley)
!    26-Oct-2004: V12.047 Change integer output to 10I8 (Hammersley)
!    28-Oct-2004: V12.048 Option to output floating point TIFF images
!      (Hammersley)
!    03-Nov-2004: V12.049 Correct output of floating point TIFF images
!      (Hammersley)
!    17-Nov-2004: V12.050 Option of "PROJECTIONS" in "FILE SERIES"
!      interface (Hammersley)
!    18-Nov-2004: V12.051 Implementing "PROJECTIONS" in "FILE SERIES"
!      interface (Hammersley)
!    19-Nov-2004: V12.052 Correct first file of "PROJECTIONS" in
!      "FILE SERIES" interface (Hammersley)
!    23-Nov-2004: V12.053 Testing problem with macro running under Linux
!      (Hammersley)
!    26-Nov-2004: V12.054 New Linux version (Hammersley)
!    13-Dec-2004: V12.055 Test version (Hammersley)
!    15-Dec-2004: V12.056 Fix input problem (Hammersley)
!    22-Dec-2004: V12.057 Updating graphics module (Hammersley)
!    06-Jan-2005: V12.058 Allow input from pipes (Linux) (Hammersley)
!    10-Jan-2005: V12.059 Change to X-11 updating code (Hammersley)
!    11-Jan-2005: V12.060 Output geometry file (Hammersley)
!    14-Jan-2005: V12.061 Input geometry file (Hammersley)
!    20-Jan-2005: V12.062 Testing geometry file (Hammersley)
!    04-Feb-2005: V12.063 Testing (Hammersley)
!    07-Feb-2005: V12.064 Investigating problem of inputting geometry file
!      (Hammersley)
!    07-Feb-2005: V12.065 Increase maximum number of strips which can be
!      input in a TIFF image (Hammersley)
!    10-Feb-2005: V12.066 Testing MFIT problem: works fine on Windows but
!      weird error on Linux ! (Hammersley)
!    11-Feb-2005: V12.067 Correct MFIT problem (non-intialised variable)
!      (Hammersley)
!    14-Feb-2005: V12.068 Remove diagnositcs from Mar input routine
!      (Hammersley)
!    15-Feb-2005: V12.069 Correct beam centre position when outside image
!      for cake integration (Hammersley)
!    23-Feb-2005: V12.070 Add "GROW MASK" command to MASK menu (Hammersley)
!    24-Feb-2005: V12.071 Add full path to default name for Chiplot output
!      files (Hammersley)
!    16-Mar-2005: V12.072 Correct error in Powder Diffraction Interface
!      input which meant that files of different type could not be input for
!      dark current subtraction and flat field correction (Hammersley)
!    17-Mar-2005: V12.073 Add "INVERT MASK" command to mask menu
!      (Hammersley)
!    21-Mar-2005: V12.074 Add "RECIPROCAL SPACE MAPPING" inferface
!      (Hammersley)
!    31-Mar-2005: V12.075 Decrease minimum wavelength limit to 0.00001
!      Angstroms as previous limit was too high for TEM 2D diffraction work
!    04-Apr-2005: V12.076 Add warning message on development phase of
!      "RECIPROCAL SPACE MAPPING" inferface (Hammersley)
!    05-Apr-2005: V12.077 Reduce minimum wavelength to 1.0e-4 Angstroms
!      in "CALIBRANT" input form (Hammersley)
!    07-Apr-2005: V12.078 Minor changes to code for Mac OSX version to keep
!      IBM xlf compiler happy (Hammersley)
!    13-Apr-2005: V12.079 Try adding "-static" to Mac link to avoid need
!      for xlf dynamic libraries (Hammersley)
!    19-Apr-2005: V12.080 Give up and use "ld" with all .a files specifically
!      included to avoid need for xlf dynamic libraries on MacOSX (Hammersley)
!    20-Apr-2005: V12.081 Try changing Linux link to exclude shared libraries
!      as much as possible (but this may cause problems) (Hammersley)
!    22-Apr-2005: V12.082 Implementing "MAP SERIES" (Hammersley)
!    25-Apr-2005: V12.083 Debugging crystal orientation  (Hammersley)
!    27-Apr-2005: V12.084 Reciprocal mapping ready for testing
!      (Hammersley)
!    10-May-2005: V12.085 Correct error on "TRANSFER SECTION TO MEMORY"
!      (Hammersley)
!    11-May-2005: V12.086 De-bugging reciprocal mapping (Hammersley)
!    12-May-2005: V12.087 Reciprocal mapping apparantly working (Hammersley)
!    13-May-2005: V12.088 Convert reciprocal mapping angles to radians
!      and optimise code (Hammersley)
!    02-Jun-2005: V12.089 Add support for horizontal and vertical
!      detector movement in "RECIPROCAL SPACE MAPPING" (Hammersley)
!    08-Jun-2005: V12.090 Correct angle calculations to take into account
!      skipping files in "RECIPROCAL SPACE MAPPING". Correct "Bruker"
!      input routines so that a bad file does not stop subsequent input.
!      (Hammersley)
!    09-Jun-2005: V12.091 Set output section axes in "RECIPROCAL SPACE
!      MAPPING" to "step" units (Hammersley)
!    10-Jun-2005: V12.092 Implementing UB matrix calculation from
!      2 reflections (Hammersley)
!    12-Jul-2005: V12.093 Implement Busing and Levy UB matrix determining
!      algorithm (Hammersley)
!    21-Jul-2005: V12.094 Debugging UB matrix calculation (Hammersley)
!    26-Jul-2005: V12.095 Add option to output program state to a
!      named file (Hammersley)
!    29-Jul-2005: V12.096 Add option to input program state from a
!      named file (Hammersley)
!    16-Aug-2005: V12.097 Calculate B matrix with double precision
!      arithmetic (Hammersley)
!    05-Sep-2005: V12.098 Add option to click on images for determining
!      UB matrix (Hammersley)
!    06-Sep-2005: V12.099 Calculate angles from images (Hammersley)
!    07-Sep-2005: V12.100 Continue implementation (Hammersley)
!    27-Sep-2005: V12.101 Correct coordinate input (Hammersley)
!    29-Sep-2005: V12.102 Calculate pseudo-angles (Hammersley)
!    30-Sep-2005: V12.103 Testing (Hammersley)
!    06-Oct-2005: V12.104 Testing (Hammersley)
!    10-Oct-2005: V12.105 Testing (Hammersley)
!    12-Oct-2005: V12.106 Increase maximum number of files which can
!      be handled by the file selection component to 40000 (Hammersley)
!    13-Oct-2005: V12.107 Correct image display when calculating UB matrix
!      by clicking on two reflections (Hammersley)
!    17-Oct-2005: V12.108 Output equivalent hkl position in "RECIPROCAL
!      MAPPING" interface (Hammersley)
!    20-Oct-2005: V12.109 Debugging reciprocal space mapping (Hammersley)
!    21-Oct-2005: V12.110 Convert to subroutine, called from a "C"
!      routine (Hammersley)
!    25-Oct-2005: V12.111 Correct reciprocal mapping for vertical detector
!      geometry (Hammersley)
!    26-Oct-2005: V12.112 Testing reciprocal mapping (Hammersley)
!    27-Oct-2005: V12.113 Testing reciprocal mapping (Hammersley)
!    28-Oct-2005: V12.114 Option to mirror images in reciprocal mapping 
!      transform (Hammersley)
!    04-Nov-2005: V12.115 Testing mirror images in reciprocal mapping transform 
!      (Hammersley)
!    07-Nov-2005: V12.116 Fill in any missing pixels in reciprocal map with
!      surrounding average value (Hammersley)
!    08-Nov-2005: V12.117 Set axis values for simple planes in reciprocal
!      space mapping (Hammersley)
!    14-Nov-2005: V12.118 Option to input UB matrix from a text file
!      (Hammersley)
!    21-Nov-2005: V12.119 Changes to "F2D_RMAP_INUBMATRIX" (Hammersley)
!    01-Dec-2005: V12.120 Correct "F2D_RMAP_INUBMATRIX" (Hammersley)
!    07-Dec-2005: V12.121 Update parser (Hammersley)
!  Modules:
     Use IO_LIB
!  Use MA_LIB
     Use LG_LIB
     Use GS_LIB
!     Use FIT2D_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc' ! IO data 
!  Global Constants:
     Include 'st_symbols.inc' ! Status system symbolic constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Program array pointers
!  Import:
!    Integer num_args ! Number of input arguments
!    Character*(*) ARGUMENTS(*) ! Input arguments
!  Status:
!  Local Constants:
     Character(Len = 7), Parameter :: Version = 'V17.007' ! Version number
!     Integer, Parameter :: Max_vec_values = 3000 ! First dimension of 
!       "VECTORS" 
!      array (maximum number of values which can be stored in a vector)
!     Integer, Parameter :: Max_vectors = 50 ! Second dimension of "VECTORS"
!      array (maximum number of vectors which can be stored)
!  05-Apr-1988 Arrays now obtained by dynamic allocation
!  f2d_xmaxdat and f2d_ymaxdat are now variables (Hammersley)
!    Integer f2d_xmaxdat ! Maximum length of data array (Maximum channels)
!    Parameter (f2d_xmaxdat = 256)
!    Integer f2d_ymaxdat ! Maximum number of frames
!    Parameter (f2d_ymaxdat = 256) 
!  Local Variables:
     Character(Len = Len_name) :: input_file = 'no_data.dat' ! Input file name
     Character(Len = Len_name) :: mtitle ! title for data in memory
     Character(Len = Len_name) :: mxlabel ! Label for X-axis of data in memory
     Character(Len = Len_name) :: mylabel ! Label for Y-axis of data in memory
     Character(Len = Len_name) :: mzlabel ! Label for Z-axis of data in memory
     Character(Len = Len_name) :: title ! title for data
     Character(Len = Len_name) :: xlabel ! Label for X-axis of data
     Character(Len = Len_name) :: ylabel ! Label for Y-axis of data
     Character(Len = Len_name) :: zlabel ! Label for Z-axis of data (the data 
!      values)
     Integer :: i ! Loop variable for initialising vector limits
     Integer :: memory_id ! Identifier of shared memory (if used)
     Integer :: mxendelm ! End of X-region for data in memory
     Integer :: mxnumdat ! Length of data array in memory in X-direction
     Integer :: mxstrelm ! Start of X-region for data in memory
     Integer :: myendelm ! End of Y-region for data in memory
     Integer :: mynumdat ! Length of data array in memory in Y-direction
     Integer :: mystrelm ! Start of Y-region for data in memory
     Integer :: retstat ! "IO_LOAD_DATABASE" Routine return status:
!      -1 = File not found
!       0 = Good status file found and loaded
!       1 = Problem storing values
     Integer :: status ! Status return variable
     Integer :: window_format ! Format of workstation window:
!      0 = A4 portrait (smallish)
!      1 = A4 landscape (smallish)
!      2 = A4 portrait (maximum)
!      3 = A4 landscape (maximum)
!      4 = Square (smallish)
!      5 = Square (maximum)
!      6 = Full screen
     Integer :: num_vectors ! Number of values defined in the
!      "time"-series for each vector
     Integer :: return_status ! "IO_SAVE_DATABASE" return status:
!      -1 = File not found
!       0 = Good status file found and loaded
!       1 = Problem storing values
     Integer xend_win ! X-pixel for end of window
     Integer xstr_win ! X-pixel for start of window
     Integer yend_win ! Y-pixel for end of window
     Integer ystr_win ! Y-pixel for start of window
     Logical :: data_defined ! .True., if data exists within the program
     Logical :: graphics_macro_mode ! .True., if graphics user interface is to 
!      be used in macro-mode
     Logical :: gui ! .True., if the graphical user interface is to be used
     Logical :: log_file_open = .False. ! .True., if a log file is open
     Logical :: mask_exist ! .True., if the mask array exists
     Logical :: memory_exist = .True. ! .True. if memory array exists
     Logical :: memory_defined ! .True. if the memory contains defined data
     Logical :: output_graphics ! .True., if graphics are to be output
     Logical :: shared_memory ! .True., if shared memory is to used
     Logical :: success ! .True., if window position returned
     Logical :: variances_exist ! .True., if a data variance array is created
     Real :: height ! Height of window in page coordinates 
     Real :: mx_pixel_size = 0.000100 ! Size of a corrected pixel in the
!      memory data in the X-direction (metres)
     Real :: my_pixel_size = 0.000100 ! Size of a corrected pixel in the
!      memory data in the Y-direction (metres)
     Real :: width ! Width of window in page coordinates 
     Real :: x_pixel_size = 0.000100 ! Size of a corrected pixel in the
!      current data in the X-direction (metres)
     Real :: y_pixel_size = 0.000100 ! Size of a corrected pixel in the
!      current data in the Y-direction (metres)
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(20) ! User messages
!     Character(Len = Len_name) :: VECTOR_TITLES(Max_vectors) ! Title for 
!       vectors
!     Integer :: END_VECTORS(Max_vectors) ! End defined elements for "VECTORS"
!     Integer :: STR_VECTORS(Max_vectors) ! Starting defined element
!      for "VECTORS" ROI
!  05-Apr-1988 Arrays now obtained by dynamic allocation (Hammersley)
!    Logical*1 MASK(f2d_xmaxdat, f2d_ymaxdat) ! Data mask, .True.,
!    indicates masked data point (i.e. point not considered in fitting)
!    Real DATA(f2d_xmaxdat, f2d_ymaxdat) ! Array to contain data
!    Real VARIANCES(f2d_xmaxdat, f2d_ymaxdat) ! Array to contain
!    variances in data
!    Real XAXIS(f2d_xmaxdat) ! Array to contain X-coordinates
!    values for data
!    Real YAXIS(f2d_ymaxdat) ! Array to contain Y-coordinates
!    values for data
!    Real MDATA(f2d_xmaxdat, f2d_ymaxdat) ! Memory array to contain data
!    Real MVARIANCES(f2d_xmaxdat, f2d_ymaxdat) ! Memory array to
!      contain variances in data
!    Real MXAXIS(f2d_xmaxdat) ! Memory array to contain
!      X-coordinates values for data
!    Real MYAXIS(f2d_ymaxdat) ! Memory Array to contain
!      Y-coordinates values for data
!    Real WORK(f2d_xmaxdat, f2d_ymaxdat) ! Work array
!  Local Data Structures:
     Type(EXPERIMENTAL_DETAILS) :: experiment ! Experimental details
     Type(INPUT_OPTIONS_STRUCTURE) :: input_options ! Control of input of 
!      auxiliary experimental data
     Type(IMAGE_DATA), Pointer :: main
     Type(IMAGE_DATA), Pointer :: memory
     Type(RESULT_VECTORS) :: results ! Result vectors of fitting and other 
!      multi-operations
     Type(IMAGE_DATA), Pointer :: tmp_data
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Start of FIT2D'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!**TEST***TEST Exchange main and memory data-sets
!     tmp_data => main
!     main => memory
!     memory => tmp_data
!**TEST***TEST

!  Set up initial default values
     data_defined = .False.
     mask_exist = .False.
     memory_defined = .False.

!  Initialise status system (and I/O system)
     Call ST_DEF_SYSTEM (status)

!  Initialise light graphics system closed
     Call LG_SET_CLOSED (status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''After ST_DEF_SYSTEM'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Set command line options for I/O system
     Call IO_MSET_OPTIONS (status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''After IO_MSET_OPTIONS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Load internal data base from file
     Call IO_LOAD_DATABASE ('~/.fit2d.def', retstat, status)
 
     If (retstat .Gt. 0) Then
        Call IO_WRITE ('WARNING: Problem inputting internal database', status)
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''After IO_LOAD_DATABASE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Set number of lines to print in a "page" to stop pager on start-up
     Call IO_SET_NUMLINES (20, status)

!  Write header to user
     Call IO_WRITE (' ', status)
     Write (MESSAGE, '( ' // &
       '''                      --------------------------------''/' // &
       '''                      PROGRAM  FIT2D  Version: '', a7, /' // &
       ''' '' /' //&
       '''                           "Big Data" Crusher!!!'' /' // &
       '''                      ________________________________''/' // &
       '/' // &
       '''        Copyright 1987-2015 Andy Hammersley / ESRF ' // &
       '(hammersley@esrf.eu)''//' // &
       '''FIT2D: 2-D Detector Calibration/ Correction; File ' // &
       're-formatting; 2-D Fitting''//' // &
       '''                  YOU CAN ALWAYS ENTER:  ?  OR  HELP ''/' // &
       '''              FOR FURTHER EXPLANATION OF REQUIRED INPUT''//' // &
       '''              No commercial software used ! ''//' // &
       '''   Version 17: !!! "GEOMETRIC" and "MATHS" operations now use ' // &
       'MASKING !!!''/' // &
       '''   Version 16: New compiler versions''/' // &
       '''   Version 15: Updated peak search algorithm'')') Version
     Call IO_TEXT (19, MESSAGE, status)
 
!  Write end of header indicator
     Call IO_WRITE ('---------------------------------------' // &
       '---------------------------------------', status)
 
!  Set command line options for FIT2D system, and initialise graphics
     Call F2D_MSET_OPTIONS (Version, f2d_xmaxdat, f2d_ymaxdat, data_defined, &
       memory_exist, variances_exist, output_graphics, gui, shared_memory, &
       memory_id, graphics_macro_mode, status)
 
!  Set internal memories un-defined
     Call F2D_DEF_INTERNALMEMORY (status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Before F2D_INP_DATASTORE'')')
!     Write (*, '(''status = ''i12)') status
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Input default values from default file "~/.fit2d.def"
     Call F2D_INP_DATASTORE (output_graphics, experiment, input_options, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!    Write (*, '(''After F2D_INDATASTORE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     If (output_graphics) Then
 
!     Set window title
        Call LG_WINDOW_TITLE ('FIT2D GRAPHICS WINDOW', status)
 
!     Set larger default graphics display region for portrait windows
        Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
        If (window_format .Eq. Gs_A4portrait) Then
           Call GS_SET_GPP (0.08, 0.3529, 0.6271, 0.92, status)
        Else If (window_format .Eq. Gs_A4landscape) Then
           Call GS_SET_GPP (0.08, 0.15, 0.92, 0.6271, status)
        End If
 
     End If
 
!  Check for "user escape"
     If (status .Eq. St_escapevalue) Then
 
!     Immediate exit from program
        Call ST_DEF_SYSTEM (status)
        Call IO_WRITE ('NOTE: "User escape" issued on start-up', status)
        Call IO_WRITE ('      Exit from FIT2D: Bye !', status)
        Stop
 
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Before call to F2D_ARRAYS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Create Dynamic array for program with initial sizes
     Call F2D_ARRAYS (f2d_xmaxdat, f2d_ymaxdat, &
       memory_exist, variances_exist, shared_memory, memory_id, &
       mask_exist, &
       pDATA, pMASK, pXAXIS, pYAXIS, pVARIANCES, pMDATA, pMXAXIS, pMYAXIS, &
       pMVARIANCES, results, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Test VECTORS assignment
!     results%VECTORS(1, 1) = 5.0
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     If (data_defined) Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Before calls to F2D_AXES'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Set data region to full data
        f2d_xnumdat = f2d_xmaxdat
        f2d_ynumdat = f2d_ymaxdat 
        f2d_xstrelm = 1
        f2d_ystrelm = 1
        f2d_xendelm = f2d_xmaxdat
        f2d_yendelm = f2d_ymaxdat
 
!     Define axis scales
        Call F2D_AXES (f2d_xmaxdat, f2d_xnumdat, 0.5, 1.0, %val(pXAXIS), status)
        Call F2D_AXES (f2d_ymaxdat, f2d_ynumdat, 0.5, 1.0, %val(pYAXIS), status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''After calls to F2D_AXES'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     Else
 
!     Initialise ROI to impossible values to force definition first time
        f2d_xendelm = f2d_xmaxdat * 2
        f2d_xnumdat = f2d_xmaxdat
        f2d_ynumdat = f2d_ymaxdat
 
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Before MA_RVALUE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Initialise vectors array
!     Call MA_RVALUE (max_vec_values, max_vectors, 1, 1, max_vec_values, &
!       max_vectors, 0.0, %val(pVECTORS), status)
!     num_vectors = 0
 
!  Set out of range limits to defined vector ranges
!     Do i = 1, max_vectors
!        STR_VECTORS(i) = max_vec_values + 1
!        END_VECTORS(i) = 0
!     End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Graphics User Interface
     If (gui .Or. graphics_macro_mode) Then
 
!     Interactive image display and manipulation
!     N.B. Program arrays now passed through common
        Call F2D_GUI (Version, .True., output_graphics, shared_memory, &
          memory_id, input_file, data_defined, memory_exist, memory_defined, &
          variances_exist, mask_exist, log_file_open, f2d_xmaxdat, &
          f2d_ymaxdat, f2d_xnumdat, f2d_ynumdat, title, xlabel, ylabel, &
          zlabel, experiment, input_options, f2d_xstrelm, f2d_ystrelm, &
          f2d_xendelm, f2d_yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, &
          mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
          mx_pixel_size, my_pixel_size, results, status)
!max_vec_values, max_vectors, &
!          num_vectors, STR_VECTORS, END_VECTORS, VECTOR_TITLES, status)
 
     Else
 
!     Keyboard interface main menu
        Call F2D_KEYBOARD (Version, gui, output_graphics, shared_memory, &
          memory_id, input_file, data_defined, memory_exist, memory_defined, &
          variances_exist, mask_exist, log_file_open, f2d_xmaxdat, &
          f2d_ymaxdat, f2d_xnumdat, f2d_ynumdat, title, xlabel, ylabel, &
          zlabel, experiment, input_options, f2d_xstrelm, f2d_ystrelm, &
          f2d_xendelm, f2d_yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, &
          mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
          mx_pixel_size, my_pixel_size, results, status) 
!max_vec_values, max_vectors, &
!          num_vectors, STR_VECTORS, END_VECTORS, VECTOR_TITLES, status)
  
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  CLOSE DOWN
!  ----------

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_MAIN: Close down section'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Check if log file is open
     If (log_file_open) Then
        Call IO_CLOSE_LOGFILE (status)
     End If

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_MAIN: Before LG_WINDOW_POSITION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     If (Lg_inq_open(status)) Then
        Call LG_WINDOW_POSITION (success, xstr_win, ystr_win, xend_win, &
          yend_win)

        If (success) Then

!        Save window position in internal data-base
           Call IO_SET_IKEYVALUE ('X_START_WINDOW', xstr_win, retstat, status)
           Call IO_SET_IKEYVALUE ('Y_START_WINDOW', ystr_win, retstat, status)
           Call IO_SET_IKEYVALUE ('X_END_WINDOW', xend_win, retstat, status)
           Call IO_SET_IKEYVALUE ('Y_END_WINDOW', yend_win, retstat, status)
           
        End If

     End If

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_MAIN: After LG_WINDOW_POSITION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Close Graphics
     If (output_graphics) Then
        Call GS_CLOSE_GRAPHICS (status)
     End If
 
!  Free existing internal arrays
     Call F2D_FREEARRAYS (memory_exist, variances_exist, mask_exist, &
       shared_memory, memory_id, pDATA, pXAXIS, pYAXIS, pVARIANCES, pMASK, &
       pMDATA, pMXAXIS, pMYAXIS, pMVARIANCES, results, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_MAIN: Before F2D_SET_EXPERIMENT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Set current values for experiment into internal data-base 
     Call F2D_SET_EXPERIMENT (experiment, status)

!  Save internal data-base to default file
     Call IO_SAVE_DATABASE ('~/.fit2d.def', return_status, status)
 
     If (return_status .Ne. 0) Then
        Call IO_WRITE ('WARNING: Problem saving default values', status)
     End If
 
!  Output trailer
     Call F2D_TRAILER (Version, status)
 
!  Check status
     Call ST_OUT (status)
  
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_MAIN: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_MAIN
!********1*********2*********3*********4*********5*********6*********7*********8

