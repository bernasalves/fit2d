!********1*********2*********3*********4*********5*********6*********7**
 
!  f2d_help_calibrate
 
!  DO NOT ALTER THIS FILE
 
!  This file has been created automatically by "maketext"
!  from the ASCII text file:  f2d_help_calibrate.text
!  which should be edited and maketext should be re-run to
!  create a new version of the subroutine.
 
!+ f2d_help_calibrate interactive user help/message display facility
     Subroutine f2d_help_calibrate (status)
 
!  Type Definitions:
     Implicit None
 
!  Status:
     Integer :: status ! Status return variable
 
!  Local constants:
     Integer, Parameter :: max_chars =  79 ! Maximum number of characters
!      in a line
     Integer, Parameter :: max_lines =   77 ! Number of lines in message
!  Local arrays:
     Character(Len = max_chars) :: MESSAGE(max_lines) ! Text array
!  External Functions:
     Integer, External :: St_good ! Returns good status value
!  Local Data:
     Data MESSAGE(  1)(1:60)/ &
       '------------------------------------------------------------'/
     Data MESSAGE(  1)(61:) / '-------------------'/
     Data MESSAGE(  2)(1:60)/ &
       '-         BEGINNING OF fit2d:calibration SUB-MENU HELP TEXT,'/
     Data MESSAGE(  2)(61:) / ' SEE MAIN'/
     Data MESSAGE(  3) / &
       '-                     MENU "HELP" FOR HELP COMMANDS'/
     Data MESSAGE(  4)(1:60)/ &
       '------------------------------------------------------------'/
     Data MESSAGE(  4)(61:) / '------------------'/
     Data MESSAGE(  5) / ' ' /
     Data MESSAGE(  6)(1:60)/ &
       'The calibration sub-menu allows the calculation and subseque'/
     Data MESSAGE(  6)(61:) / 'nt application of'/
     Data MESSAGE(  7)(1:60)/ &
       'spatial and other calibration functions. Prior to being able'/
     Data MESSAGE(  7)(61:) / ' to apply'/
     Data MESSAGE(  8)(1:60)/ &
       'correcting functions to data, it is necessary to define the '/
     Data MESSAGE(  8)(61:) / 'calibration'/
     Data MESSAGE(  9)(1:60)/ &
       'functions using data with known properties i.e. the results '/
     Data MESSAGE(  9)(61:) / 'of controlled'/
     Data MESSAGE( 10) / 'calibration experiments.'/
     Data MESSAGE( 11) / ' ' /
     Data MESSAGE( 12)(1:60)/ &
       '------------------------------------------------------------'/
     Data MESSAGE( 12)(61:) / '-------------------'/
     Data MESSAGE( 13) / &
       '                              FIT2D:CALIBRATION COMMANDS'/
     Data MESSAGE( 14)(1:60)/ &
       '------------------------------------------------------------'/
     Data MESSAGE( 14)(61:) / '-------------------'/
     Data MESSAGE( 15) / ' ' /
     Data MESSAGE( 16) / '"APPLY SPATIAL CORRECTION"'/
     Data MESSAGE( 17) / '--------------------------'/
     Data MESSAGE( 18) / ' ' /
     Data MESSAGE( 19)(1:60)/ &
       'Apply a (previously calculated) spatial correction function '/
     Data MESSAGE( 19)(61:) / 'to the data'/
     Data MESSAGE( 20)(1:60)/ &
       'in the current data-set, within the current "Acitve Data Reg'/
     Data MESSAGE( 20)(61:) / 'ion". The spatial'/
     Data MESSAGE( 21)(1:60)/ &
       'correction function must have been previously calculated usi'/
     Data MESSAGE( 21)(61:) / 'ng "FIT GRID".'/
     Data MESSAGE( 22)(1:60)/ &
       'The corrected data is output in the memory (Use "EXCHANGE" i'/
     Data MESSAGE( 22)(61:) / 'n main menu to'/
     Data MESSAGE( 23) / 'make the corrected image the current data-set.)'/
     Data MESSAGE( 24) / ' ' /
     Data MESSAGE( 25)(1:60)/ &
       'The position of the centre of the edges of each input pixel '/
     Data MESSAGE( 25)(61:) / 'is calculated for'/
     Data MESSAGE( 26)(1:59)/ &
       'its transformed position in the corrected image. It''s inten'/
     Data MESSAGE( 26)(60:) / 'sity is distributed'/
     Data MESSAGE( 27) / 'in proportion to the covered corrected pixels.'/
     Data MESSAGE( 28) / ' ' /
     Data MESSAGE( 29) / '"EXIT"'/
     Data MESSAGE( 30) / '------'/
     Data MESSAGE( 31) / ' ' /
     Data MESSAGE( 32) / &
       'Exit calibration sub-menu, and return to FIT2D main menu.'/
     Data MESSAGE( 33) / ' ' /
     Data MESSAGE( 34) / '"FIT GRID"'/
     Data MESSAGE( 35) / '----------'/
     Data MESSAGE( 36) / ' ' /
     Data MESSAGE( 37)(1:60)/ &
       'Calibrate spatial distortion function by calculating centres'/
     Data MESSAGE( 37)(61:) / ' of a grid of'/
     Data MESSAGE( 38)(1:60)/ &
       'spots, whose ideal position is known. The peak searching is '/
     Data MESSAGE( 38)(61:) / 'restricted to the'/
     Data MESSAGE( 39)(1:60)/ &
       '"Region Of Interest" (ROI). The ROI should be set so that gr'/
     Data MESSAGE( 39)(61:) / 'id spots cover the'/
     Data MESSAGE( 40)(1:60)/ &
       'entire ROI, if not the peak searching will get lost, as it b'/
     Data MESSAGE( 40)(61:) / 'ases the search for'/
     Data MESSAGE( 41)(1:60)/ &
       'each peak on the position of previous peaks. (Experience wil'/
     Data MESSAGE( 41)(61:) / 'l show if the'/
     Data MESSAGE( 42) / 'method needs to be made more robust.)'/
     Data MESSAGE( 43) / ' ' /
     Data MESSAGE( 44)(1:60)/ &
       'At present the user initialises the search by graphically cl'/
     Data MESSAGE( 44)(61:) / 'icking on the'/
     Data MESSAGE( 45)(1:60)/ &
       'centre of the lower-left-hand peak and a peak horizontally, '/
     Data MESSAGE( 45)(61:) / 'and a peak'/
     Data MESSAGE( 46)(1:60)/ &
       'vertically from this peak. If the ROI is very big only a sub'/
     Data MESSAGE( 46)(61:) / '-set of the data'/
     Data MESSAGE( 47)(1:60)/ &
       'will be displayed for this graphical input; allowing the cen'/
     Data MESSAGE( 47)(61:) / 'tres to be defined'/
     Data MESSAGE( 48) / 'with reasonable accuracy.'/
     Data MESSAGE( 49) / ' ' /
     Data MESSAGE( 50)(1:60)/ &
       'At present the peaks are assumed to be Gaussian and the user'/
     Data MESSAGE( 50)(61:) / ' must entered a'/
     Data MESSAGE( 51)(1:60)/ &
       'standard deviation peak width for the spots. A suitable valu'/
     Data MESSAGE( 51)(61:) / 'e can be found by'/
     Data MESSAGE( 52)(1:60)/ &
       'previously fitting a number of spots with a 2-D Gaussian fun'/
     Data MESSAGE( 52)(61:) / 'ction (FIT2D main'/
     Data MESSAGE( 53)(1:60)/ &
       'menu "FIT" command.) The correctness of this value is not cr'/
     Data MESSAGE( 53)(61:) / 'ucial, however for'/
     Data MESSAGE( 54) / 'best results it should be close.'/
     Data MESSAGE( 55) / ' ' /
     Data MESSAGE( 56)(1:60)/ &
       'Having calculated the centres of all the spots to the requir'/
     Data MESSAGE( 56)(61:) / 'ed accuracy two'/
     Data MESSAGE( 57)(1:60)/ &
       '2-D interpolation functions are defined: one for the X-disto'/
     Data MESSAGE( 57)(61:) / 'rtion and one for'/
     Data MESSAGE( 58)(1:60)/ &
       'the Y-distortion. At present these are defined by bicubic sp'/
     Data MESSAGE( 58)(61:) / 'lines. The user'/
     Data MESSAGE( 59)(1:60)/ &
       'can specify the required average deviation of the spline fun'/
     Data MESSAGE( 59)(61:) / 'ction form the'/
     Data MESSAGE( 60) / 'measured positions.'/
     Data MESSAGE( 61) / ' ' /
     Data MESSAGE( 62) / '"HELP"'/
     Data MESSAGE( 63) / '------'/
     Data MESSAGE( 64) / ' ' /
     Data MESSAGE( 65)(1:58)/ &
       'This help text, controlled by a ''pager'' allowing backwards'/
     Data MESSAGE( 65)(59:) / ' and forwards'/
     Data MESSAGE( 66)(1:60)/ &
       'scrolling, searching for keywords, and many other possibilit'/
     Data MESSAGE( 66)(61:) / 'ies. (Type ?'/
     Data MESSAGE( 67) / &
       'more information on the pager, and the available commands.)'/
     Data MESSAGE( 68) / ' ' /
     Data MESSAGE( 69) / '"QUIT"'/
     Data MESSAGE( 70) / '------'/
     Data MESSAGE( 71) / ' ' /
     Data MESSAGE( 72) / 'See "EXIT"'/
     Data MESSAGE( 73) / ' ' /
     Data MESSAGE( 74)(1:60)/ &
       '------------------------------------------------------------'/
     Data MESSAGE( 74)(61:) / '-------------------'/
     Data MESSAGE( 75)(1:60)/ &
       '-             END OF fit2d:calibration SUB-MENU HELP TEXT, S'/
     Data MESSAGE( 75)(61:) / 'EE MAIN MENU "HELP"'/
     Data MESSAGE( 76) / '-             FOR USAGE OF HELP'/
     Data MESSAGE( 77)(1:60)/ &
       '------------------------------------------------------------'/
     Data MESSAGE( 77)(61:) / '-------------------'/
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status value
     If (status .Eq. St_good()) Then
        Call IO_TEXT (max_lines, MESSAGE, status)
     End If
 
     End
 
!********1*********2*********3*********4*********5*********6*********7**
