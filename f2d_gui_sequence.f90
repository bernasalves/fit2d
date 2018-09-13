!********1*********2*********3*********4*********5*********6*********7**
 
!  ************************
!  *                      *
!  * f2d_gui_sequence.f90 *
!  *                      *
!  ************************
 
!+ F2D_GUI_SEQUENCE: GUI SEQUENCE of automatic processing
     Subroutine F2D_GUI_SEQUENCE (inmacro_file, status)
!  Description:
!    Runs a macro repeatedly e.g. on a sequence of files
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    11-Jun-1999: V0.5 Initialise first and last file names to blanks
!      (Hammersley)
!    21-Aug-1998: V0.4 Replace "SYMBOL" calls with "VARIABLE" calls (Hammersley)
!    11-Aug-1998: V0.3 Changes to "IO_SET_SYMBOL" (Hammersley)
!    22-Feb-1998: V0.2 Take account of any unchanging part of the file name 
!      prior to any extension (Hammersley)
!    22-Jan-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'io_db.inc'
!  Import:
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: inmacro_file ! File name of input
!      macro file
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Character(Len = 256), Save :: extension ! File name extension
     Character(Len = 256), Save :: first_file_name = ' ' ! Full name of first 
!      file
     Character(Len = 256), Save :: last_file_name = ' ' ! Full name of last
!      file
     Character(Len = 80) :: message ! User text
     Character(Len = 40), Save :: out_extension ! Output file extension
     Character(Len = 256) :: out_file ! The output file name
     Character(Len = 1) :: string_value ! Dummy value for "IO_SET_VARIABLE"
     Character(Len = 256) :: symbol_value ! Value for a file name
     Character(Len = 256), Save :: postfix ! Fixed end of file names
     Character(Len = 256), Save :: prefix ! Fixed start of file names
     Integer, Save :: count ! Counter for the sequence
     Integer, Save :: end_value ! Value at end of sequence
     Integer, Save :: increment ! Step value in sequence
     Integer :: int_value ! Dummy value for "IO_SET_VARIABLE"
     Integer :: len_string ! Dummy value for "IO_SET_VARIABLE"
     Integer, Save :: num_characters ! Number of characters in numerical part
     Integer :: retstat ! Return status for file:
!      0 = Good status
!      1 = Cancel was chosen (no file)
!      2 = Bad status (no file)
     Integer :: start_value ! Value at start of sequence
     Logical :: log_value ! Dummy value for "IO_SET_VARIABLE"
     Logical, Save :: variable_characters ! .True., if the number of
!      characters in the numerical part of the names changes
     Real :: real_value ! Dummy value for "IO_SET_VARIABLE"
!  Local Arrays:
     Character(Len = 80) :: INFO(3) ! User info
     Character(Len = 80) :: PROMPT(2) ! User prompts
!  External Functions:
     Character(Len = 20), External :: Io_itoc ! Convert integer to character
!    string
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_SEQUENCE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     If not in sequence mode ask for the name of the macro file
!     and the number of times to run it
        If (.Not. io_sequence) Then
 
!        Check that a macro is not presently being written
           If (io_output_macro) Then
 
              Call IO_WRITE ('WARNING: The "SEQUENCE" ' // &
                'command cannot be used within a macro. (It is', status)
              Call IO_WRITE ('         used to repeately ' // &
                'run a defined macro.)', status)
              Return
           End If
 
!        Output warning message
           Call GS_FWARNING (1, 1, &
             'AT PRESENT ONLY WORKS WITH #IN AND #OUT MACROS', status)
 
!        Use file selection tool to obtain file name from user
           PROMPT(1) = 'SELECT MACRO FILE '
           PROMPT(2) = 'TO RUN'
           INFO(1) = 'Use the file selection tool to select ' // 'a directory'
           INFO(2) = 'and a macro file to run.'
           INFO(3) = 'Normally macro files end with the' // &
             'extension ".mac".'
           Call GS_FILESELECTION (2, 2, PROMPT, 3, 3, INFO, 1, .False., &
             retstat, inmacro_file, status)
 
!        Check return status
           If (retstat .Ne. 0) Then
              Return
           End If
 
!        Use file selection tool to select first file in sequence
           PROMPT(1) = 'SELECT FIRST INPUT'
           PROMPT(2) = 'FILE IN THE SEQUENCE'
           INFO(1) = 'Use the file selection tool to select ' // 'a directory'
           INFO(2) = 'and the first file in the sequence.'
           Call GS_FILESELECTION (2, 2, PROMPT, 2, 2, INFO, 1, .False., &
             retstat, first_file_name, status)
 
!        Check return status
           If (retstat .Ne. 0) Then
              Return
           End If
 
!        Use file selection tool to select last file in sequence
           PROMPT(1) = 'SELECT LAST INPUT'
           PROMPT(2) = 'FILE IN THE SEQUENCE'
           INFO(1) = 'Use the file selection tool to select'
           INFO(2) = 'the last file in the sequence.'
           Call GS_FILESELECTION (2, 2, PROMPT, 2, 2, INFO, 1, .False., &
             retstat, last_file_name, status)
 
!        Check return status
           If (retstat .Ne. 0) Then
              Return
           End If
 
!        Deduce components of file sequence
           Call IO_FILESEQUENCE ( first_file_name, last_file_name, retstat, &
             start_value, end_value, increment, prefix, variable_characters, &
             num_characters, postfix, extension, status)
 
           If (retstat .Ne. 0) Then
 
              Call GS_FWARNING (1, 1, &
                'COULD NOT DEDUCE REQUIRED FILE SEQUENCE', status)
              Return
 
           End If
 
           If (Len_trim(postfix) .Gt. 0) Then
 
!           Add the 'postfix' to the extension
              If (Len_trim(extension) .Gt. 0) Then
                 extension = Trim(postfix) // Trim(extension)
              Else
                 extension = Trim(postfix)
              End If
 
           End If
 
!        Clear file selection graphics
           Call GS_BACKGROUND (status)
 
!        Increment between files
           Call GS_INPI (.True., 1, Max(1, end_value - start_value), .True., &
             'FILE INCREMENT', 1, 'Enter the step between input files', 1, &
             'Enter an integer number', increment, status)
 
!        Output file extension
           Call GS_INPC (.True., 'OUTPUT FILES EXTENSION', 1, &
             'Enter the extension for output files', 1, &
             'Enter valid text string', 1, out_extension, status)
 
!        set-up loop variables
           count = start_value
 
!        Check status
           If (status .Eq. St_escapevalue) Then
 
              Call ST_DEF_SYSTEM (status)
              Return
 
           End If
 
!        Set sequence mode for I/O system (this is the important line)
           io_sequence = .True.
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Carry out one iteration of loop sequence if in sequence mode
        If (io_sequence) Then
 
           If ((count .Le. end_value .And. increment .Ge. 0) .Or. (count .Ge. &
             end_value .And. increment .Le. 0)) Then
 
!           Output loop number to user
              Write (message, '(''INFO: Sequence counter value = '', i8)') &
                count
              Call IO_WRITE (' ', status)
              Call IO_WRITE (message, status)
 
!           Set symbol "##COUNT" to current counter value
              Call IO_SET_VARIABLE ('##COUNT', 'i', count, log_value, &
                real_value, len_string, string_value, retstat, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''prefix = '', a)') prefix
!           Write (*, '(''variable_characters, num_characters = '',
!           :             l, i3)') variable_characters, num_characters
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Generate input file
              Call IO_FILENAME (prefix, count, .Not. variable_characters, &
                num_characters, extension, retstat, symbol_value, status)
 
!           Set symbol "#IN" to input file
              Call IO_SET_VARIABLE ('#IN', 's', int_value, log_value, &
                real_value, Len_trim(symbol_value), symbol_value, retstat, &
                status)
 
!           Output input file to user
              Call IO_WRITE ('      Input file = ' // symbol_value, status)
 
!           Generate output file
              Call IO_FILEEXTENSION (symbol_value, out_extension, retstat, &
                out_file, status)
 
!           Set symbol "#OUT" to output file
              Call IO_SET_VARIABLE ('#OUT', 's', int_value, log_value, &
                real_value, Len_trim(out_file), out_file, retstat, status)
 
!           Output input file to user
              Call IO_WRITE ('      Output file = ' // out_file, status)
 
!           Set I/O system into input macro file state
              Call IO_OPEN_INMACRO (.False., inmacro_file, status)
 
!           Increment loop count value
              count = count + increment
 
           Else
              io_sequence = .False.
              Call IO_WRITE ( 'INFO: *** END OF SEQUENCE ***', status)
              Call IO_WRITE (' ', status)
 
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_GUI_SEQUENCE
!********1*********2*********3*********4*********5*********6*********7**
