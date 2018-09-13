!*********1*********2*********3*********4*********5*********6*********7**
 
!  *****************
!  *               *
!  * f2d_scanf.f90 *
!  *               *
!  *****************
 
!+ F2D_SCANF - Fit 2-D: Scan (what does the F mean ?)
     Subroutine F2D_SCANF (unit, line, ok, status)
!  Description:
!    Inputs from "unit" line number "line".
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    16-Nov-1999: V0.39 Add ID-20 systems (Hammersley)
!    15-Nov-1999: V0.38 Add ID-30 WNT system (Hammersley)
!    28-Oct-1999: V0.37 Add ID-13 WNT systems (Hammersley)
!    01-Oct-1999: V0.36 Add more ESRF systems (Hammersley)
!    21-Sep-1999: V0.35 Add more ESRF systems (Hammersley)
!    18-Aug-1999: V0.34 Add new system (Hammersley)
!    07-May-1999: V0.33 Add new ESRF systems (Hammersley)
!    22-Apr-1999: V0.32 Add detector pool system (Hammersley)
!    14-Apr-1999: V0.31 Add new system (Hammersley)
!    25-Mar-1999: V0.30 Add new system (Hammersley)
!    03-Mar-1999: V0.29 Add new ID-11 systems (Hammersley)
!    15-Feb-1999: V0.28 Add my new PC (Hammersley)
!    03-Feb-1999: V0.27 Add new machine for ID-9 (Hammersley)
!    25-Jan-1999: V0.26 Add new machine for ID-1 (Hammersley)
!    23-Nov-1998: V0.25 Add INPG key (Hammersley)
!    12-Nov-1998: V0.24 Add BioCAT systems (Hammersley)
!    29-Oct-1998: V0.23 Add new systems (Hammersley)
!    07-Jul-1998: V0.22 Add WNT PC (Hammersley)
!    02-Jul-1998: V0.21 Add ID-30 PC (Hammersley)
!    26-Jun-1998: V0.20 Add LTPCM / CNRS machine (Hammersley)
!    06-May-1998: V0.19 Add Dubble machines (Hammersley)
!    25-Feb-1998: V0.18 Add new machines (Hammersley)
!    19-Jan-1998: V0.17 Add ETH Zurich machines (Hammersley)
!    20-Dec-1997: V0.16 Avoid continuations lines above 19 lines (Hammersley)
!    17-Dec-1997: V0.15 Add list of beam-line machines (Hammersley)
!    04-Dec-1997: V0.14 Add new system (Hammersley)
!    17-Nov-1997: V0.13 Add "rose" (Hammersley)
!    05-Nov-1997: V0.12 Add "silver0" (Hammersley)
!    02-Sep-1997: V0.11 Change "micro1" and "micro2" to "micro01"
!      and "micro02", and add "micro03" and "micro04" (Hammersley)
!    17-Jun-1997: V0.10 Add "calamar" (Hammersley)
!    11-Feb-1997: V0.9 Add Linux system "expglin" (Hammersley)
!    28-Nov-1996: V0.8 Add Swiss-Norwegian beam-line systems (Hammersley)
!    03-Oct-1996: V0.7 Add "mufid1" and "mufid*" systems (Hammersley)
!    10-Sep-1996: V0.6 Add "laue" and "ska" to by-pass list (Hammersley)
!    02-Feb-1996: V0.5 Add "coral" to list (Hammersley)
!    14-Dec-1995: V0.4 Add more ESRF systems (Hammersley)
!    16-Oct-1995: V0.3 Add fuss1 to by-pass list (Hammersley)
!    01-Aug-1995: V0.2 Add more ESRF systems (Hammersley)
!    23-Jun-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'f2d_comm.inc'
!  Import:
     Integer, Intent(IN) :: unit ! Unit number
     Integer, Intent(IN) :: line ! Line number
!  Export:
     Integer, Intent(OUT) :: ok ! Return status:
!      0 = O.K.
!      1 = Environment variable not defined
!      2 = Bad value
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.39' ! Version number
!  Local Variables:
     Character(Len = 256) :: system ! Name of system
     Character(Len = 256) :: user_name ! Name of user
     Integer :: user_id ! Identifier number of user
!  Local Arrays:
!  External Functions:
     Character(Len = 80), External :: Io_nodename
     Integer, External :: Io_scanf
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SCANF ' // Version)
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Before Io_nodename'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Find out name of system
        system = Io_nodename (status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''After Io_nodename'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     User
        Call IO_LOGINNAME (user_name, user_id, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''After IO_LOGINNAME'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Allow certain systems to always be O.K.
        If ( system .Eq. 'alhambra' .Or. system .Eq. 'ali' .Or. system .Eq. &
          'alpha.esrf.fr' .Or. system .Eq. 'amber' .Or. system .Eq. 'amber0' &
          .Or. system .Eq. 'amber1' .Or. system .Eq. 'amber2' .Or. system .Eq. &
          'amber3' .Or. system .Eq. 'amber4' .Or. system .Eq. 'amber5' .Or. &
          system .Eq. 'amber6' .Or. system .Eq. 'amber7' .Or. system .Eq. &
          'amber8' .Or. system .Eq. 'anomal1' .Or. system .Eq. 'artemis' .Or. &
          system .Eq. 'asp01' .Or. system .Eq. 'asp02' .Or. system .Eq. &
          'athena' .Or. system .Eq. 'azure' .Or. system .Eq. 'beige' .Or. &
          system .Eq. 'bla01' .Or. system .Eq. 'bla02' .Or. system .Eq. 'bl19' &
          .Or. system .Eq. 'bruno' ) Then
           ok = 0
        Else If ( system .Eq. 'calamar' .Or. system .Eq. 'CANARY' .Or. system &
          .Eq. 'CARAMBA' .Or. system .Eq. 'CIVETTA' .Or. system .Eq. 'classic' &
          .Or. system .Eq. 'cmtd05' .Or. system .Eq. 'cmtid17' .Or. system &
          .Eq. 'cmtid19' .Or. system .Eq. 'cmtid22' .Or. system .Eq. 'coral' &
          .Or. system .Eq. 'crys1' .Or. system .Eq. 'crys2' .Or. system .Eq. &
          'crys3' .Or. system .Eq. 'crys4' .Or. system .Eq. 'crys5' .Or. &
          system .Eq. 'crys6' .Or. system .Eq. 'cyan' ) Then
           ok = 0
        Else If ( system .Eq. 'd29temp3' .Or. system .Eq. 'd2cc' .Or. system &
          .Eq. 'd2mat' .Or. system .Eq. 'detpca31' .Or. system .Eq. &
          'diffract1' .Or. system .Eq. 'diffract2' .Or. system .Eq. &
          'diffract3' .Or. system .Eq. 'diffract4' .Or. system .Eq. &
          'diffract5' .Or. system .Eq. 'diffract6' .Or. system .Eq. &
          'diffract7' .Or. system .Eq. 'diffract8' .Or. system .Eq. &
          'diffract9' .Or. system .Eq. 'diffract10' .Or. system .Eq. &
          'diffract11' .Or. system .Eq. 'diffract12' .Or. system .Eq. &
          'diffract13' .Or. system .Eq. 'diffract14' .Or. system .Eq. &
          'diffract15' .Or. system .Eq. 'diffract16' ) Then
           ok = 0
        Else If ( system .Eq. 'elan' .Or. system .Eq. 'emerald' .Or. system &
          .Eq. 'expga' .Or. system .Eq. 'expgb' .Or. system .Eq. 'expgc' .Or. &
          system .Eq. 'expgd' .Or. system .Eq. 'expge' .Or. system .Eq. &
          'expgg' .Or. system .Eq. 'expgh' .Or. system .Eq. 'expgi' .Or. &
          system .Eq. 'expgj' .Or. system .Eq. 'expgpch.esrf.fr' .Or. system &
          .Eq. 'EXPGPCH' .Or. system .Eq. 'expgpco' .Or. system .Eq. 'EXPGPCO' &
          .Or. system .Eq. 'expglin' .Or. system .Eq. 'fastscan' .Or. system &
          .Eq. 'fourflusher' .Or. system .Eq. 'freja' .Or. system .Eq. 'fuss1' &
          .Or. system .Eq. 'fuss2' .Or. system .Eq. 'galilee' .Or. system .Eq. &
          'ghicio' .Or. system .Eq. 'gold1' .Or. system .Eq. 'gold2' .Or. &
          system .Eq. 'gold3' .Or. system .Eq. 'gold4' .Or. system .Eq. &
          'gold5' .Or. system .Eq. 'gold6' ) Then
           ok = 0
        Else If ( system .Eq. 'goteborg' .Or. system .Eq. 'green' .Or. system &
          .Eq. 'guy' .Or. system .Eq. 'guy1' .Or. system .Eq. 'guy2' .Or. &
          system .Eq. 'guy3' .Or. system .Eq. 'hera' .Or. system .Eq. 'hermes' &
          .Or. system .Eq. 'hexbow' .Or. system .Eq. 'hexhelm' .Or. system &
          .Eq. 'hexhelm2' .Or. system .Eq. 'hexport' .Or. system .Eq. &
          'hexstar' .Or. system .Eq. 'imax' .Or. system .Eq. 'ID13PC1' .Or. &
          system .Eq. 'ID13PC2' .Or. system .Eq. 'ID13PC3' .Or. system .Eq. &
          'ID13PC4' .Or. system .Eq. 'id13sgi1' .Or. system .Eq. 'id13sgi2' &
          .Or. system .Eq. 'inel1' .Or. system .Eq. 'inel2' .Or. system .Eq. &
          'inel3' .Or. system .Eq. 'inel4' .Or. system .Eq. 'inel5' .Or. &
          system .Eq. 'inel6' .Or. system .Eq. 'isg-dp2' .Or. system .Eq. &
          'jerome1' .Or. system .Eq. 'jerome2' .Or. system .Eq. 'jerome3' .Or. &
          system .Eq. 'jerome4' ) Then
           ok = 0
        Else If ( system .Eq. 'kirstin' .Or. system .Eq. 'kuffner' .Or. system &
          .Eq. 'laue' .Or. system .Eq. 'lottie' .Or. system .Eq. 'lottie2' &
          .Or. system .Eq. 'marccd.esrf.fr' .Or. system .Eq. 'max' .Or. system &
          .Eq. 'med1' .Or. system .Eq. 'med2' .Or. system .Eq. 'med3' .Or. &
          system .Eq. 'med4' .Or. system .Eq. 'med5' .Or. system .Eq. 'med6' &
          .Or. system .Eq. 'med7' .Or. system .Eq. 'med8' ) Then
           ok = 0
        Else If ( system .Eq. 'megabar' .Or. system .Eq. 'micro01' .Or. system &
          .Eq. 'micro02' .Or. system .Eq. 'micro03' .Or. system .Eq. 'micro04' &
          .Or. system .Eq. 'moesa' .Or. system .Eq. 'moesb' .Or. system .Eq. &
          'moesc' .Or. system .Eq. 'moesd' .Or. system .Eq. 'moese' .Or. &
          system .Eq. 'moesf' .Or. system .Eq. 'mufid1' .Or. system .Eq. &
          'mufid2' .Or. system .Eq. 'mufid3' .Or. system .Eq. 'mufid4' .Or. &
          system .Eq. 'mufid5' .Or. system .Eq. 'mufid6' ) Then
           ok = 0
        Else If ( system .Eq. 'navy' .Or. system .Eq. 'noot' .Or. system .Eq. &
          'olive' .Or. system .Eq. 'optics1' .Or. system .Eq. 'optics2' .Or. &
          system .Eq. 'optics3' .Or. system .Eq. 'optics4' .Or. system .Eq. &
          'pcvisit.esrf.fr' .Or. system .Eq. 'PCRIEKEL' .Or. system .Eq. &
          'pcvisit' .Or. system .Eq. 'pi-id11' .Or. system .Eq. 'pi-id13' .Or. &
          system .Eq. 'pink' .Or. system .Eq. 'pongo' .Or. system .Eq. &
          'PRIMER' .Or. system .Eq. 'purple' .Or. system .Eq. 'quadriga' .Or. &
          system .Eq. 'rose' ) Then
           ok = 0
        Else If ( system .Eq. 'salmon' .Or. system .Eq. 'saxs1' .Or. system &
          .Eq. 'saxs2' .Or. system .Eq. 'saxs3' .Or. system .Eq. 'saxs4' .Or. &
          system .Eq. 'saxs5' .Or. system .Eq. 'saxs6' .Or. system .Eq. &
          'saxs7' .Or. system .Eq. 'saxs8' .Or. system .Eq. 'siemens2' .Or. &
          system .Eq. 'silver0' .Or. system .Eq. 'silver1' .Or. system .Eq. &
          'silver2' .Or. system .Eq. 'silver3' .Or. system .Eq. 'silver4' .Or. &
          system .Eq. 'silver5' .Or. system .Eq. 'silver6' .Or. system .Eq. &
          'silver7' .Or. system .Eq. 'silver8' .Or. system .Eq. 'silver9' .Or. &
          system .Eq. 'silver10' .Or. system .Eq. 'ska' .Or. system .Eq. &
          'ska2' .Or. system .Eq. 'ska3' .Or. system .Eq. 'ska4' ) Then
           ok = 0
        Else If ( system .Eq. 'spect10' .Or. system .Eq. 'spect13' .Or. system &
          .Eq. 'spectro1' .Or. system .Eq. 'spectro2' .Or. system .Eq. &
          'spectro3' .Or. system .Eq. 'spectro4' .Or. system .Eq. 'spectro5' &
          .Or. system .Eq. 'spectro6' .Or. system .Eq. 'spectro7' .Or. system &
          .Eq. 'spectro8' .Or. system .Eq. 'spectro9' .Or. system .Eq. &
          'spectro10' .Or. system .Eq. 'spectroa' .Or. system .Eq. 'spectrob' &
          .Or. system .Eq. 'stompy' .Or. system .Eq. 'stompy2' .Or. system &
          .Eq. 'surf1' .Or. system .Eq. 'surf2' .Or. system .Eq. 'surf3' .Or. &
          system .Eq. 'surf4' .Or. system .Eq. 'surf5' .Or. system .Eq. &
          'surf6' ) Then
           ok = 0
 
        Else If ( system .Eq. 'tina' .Or. system .Eq. 'tina1' .Or. system .Eq. &
          'tina2' .Or. system .Eq. 'tina3' .Or. system .Eq. 'tinc' .Or. system &
          .Eq. 'tinc2' .Or. system .Eq. 'tomod5' .Or. system .Eq. 'tomoid17' &
          .Or. system .Eq. 'tomoid19' .Or. system .Eq. 'tomoid22' .Or. system &
          .Eq. 'topo1' .Or. system .Eq. 'topo2' .Or. system .Eq. 'topo3' .Or. &
          system .Eq. 'topo4' .Or. system .Eq. 'ubaye' .Or. system .Eq. &
          'violet' .Or. system .Eq. 'veleta' .Or. system .Eq. 'woggle' .Or. &
          system .Eq. 'xaus1' .Or. system .Eq. 'xrma' .Or. system .Eq. 'xrmb' &
          .Or. system .Eq. 'xrmb.esrf.fr' .Or. system .Eq. 'xrmc' .Or. system &
          .Eq. 'zapata' ) Then
           ok = 0
 
!        Daresbury systems
        Else If ( system .Eq. 'dlsr' .Or. system .Eq. 'ncds161' .Or. system &
          .Eq. 'ncds21' .Or. system .Eq. 'ncds82' ) Then
           ok = 0
 
!        Dubble systems
        Else If ( system .Eq. 'dubble1' .Or. system .Eq. 'dubble2' .Or. system &
          .Eq. 'dubble2a' .Or. system .Eq. 'dubble3' .Or. system .Eq. &
          'dubble4' .Or. system .Eq. 'wimbras' ) Then
           ok = 0
 
!        Swiss-Norwegian beam-line system
        Else If ( system .Eq. 'orbe' .Or. system .Eq. 'embld1' .Or. system &
          .Eq. 'esalen' .Or. system .Eq. 'snblsg2' ) Then
           ok = 0
 
!        ILL systems
        Else If ( system .Eq. 'ad1.ill.fr' .Or. system .Eq. 'pu' ) Then
           ok = 0
 
!        ETH Zurich system
        Else If ( system .Eq. 'azurite' .Or. system .Eq. 'hauyn' .Or. system &
          .Eq. 'olivine' .Or. system .Eq. 'onyx' .Or. system .Eq. 'topaz' ) &
          Then
           ok = 0
 
!        LTPCM - INPG CNRS Grenoble machine
        Else If ( system .Eq. 'agora' .Or. system .Eq. 'cybele' .Or. system &
          .Eq. 'flore' ) Then
           ok = 0
 
!        APS BioCAT and SRICAT systems
        Else If ( system .Eq. 'biocat1' .Or. system .Eq. 'biosgo2' .Or. system &
          .Eq. 'ccd1' .Or. system .Eq. 'id1' .Or. system .Eq. 'id2' .Or. &
          system .Eq. 'ID3' .Or. system .Eq. 'muscle2' .Or. system .Eq. &
          'sricat' .Or. system .Eq. 'tom1' ) Then
           ok = 0
        Else
 
           ok = Io_scanf (system, user_name, user_id, unit, line, status)
 
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''After system names'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''ok = '', i3)') ok
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Set some important common values
        f2d_ok = 666
 
     End If
 
     End Subroutine F2D_SCANF
!********1*********2*********3*********4*********5*********6*********7**
 
 
 
 
 
