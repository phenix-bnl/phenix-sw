*     CDF file explains the lines in the GFFGO.DAT file
*         no action is taken
*-- Author :    Anita Trivedi and A. K. Mohanty  30/01/95
*     ======================================================================
*
>Name pisamenu
*
>MENU pisamenu
>Guidance
Explains special PISA commands in pisa.kumac file.
 
**********************************************************************
>Command BBC
*
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='BCAL'
+
OPT1  'Possible user option STCK' C D=' '
>Guidance
Inserts the Beam-Beam Counter hodoscope.
The code for this detector set is
currently under development at Hiroshima University by
Toru Sugitate (IN%"sugitate@hiroh2.hepl.hiroshima-u.ac.jp").
The use of the STCK option determines whether the track
number of the particle hits is stored in the FKIN data structure 
for later use in STAF. 

>Action PISA_ACT
 
**********************************************************************
>Command SVX
*
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='ELEM'
+
OPT1  'Possible user option STCK' C D=' '
>Guidance
Inserts the Silicon Tracker Detector 

>Action PISA_ACT
 
**********************************************************************
>Command NTC
*
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='ELEM'
+
OPT1  'Possible user option NEUT' C D=' '
OPT2  'Possible user option WDBG' C D=' '
>Guidance
Inserts the New Timing Counter 

>Action PISA_ACT

**********************************************************************
>Command RXN
*
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='ELEM'
+
OPT1  'Possible user option STCK' C D=' '
>Guidance
Inserts the RXN reaction plane detector 

>Action PISA_ACT

**********************************************************************
>Command FCL
*
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='ELEM'
+
OPT1  'Possible user option STCK' C D=' '
>Guidance
Inserts the forward calorimeter 

>Action PISA_ACT

**********************************************************************

>Command HBD
*
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='ELEM'
+
OPT1  'Possible user option STCK' C D=' '
>Guidance
Inserts the HBD counter, WIS version

>Action PISA_ACT

**********************************************************************

>Command TPC
*
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='ELEM'
+
OPT1  'Possible user option STCK' C D=' '
>Guidance
Inserts the HBD/TPC counter

>Action PISA_ACT

**********************************************************************
>Command AER
*
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='QCAL'
+
OPT1  'Possible user option STCK' C D=' '
>Guidance
Inserts the Aerogel detector.

>Action PISA_ACT
 
*********************************************************************
>Command CRK
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_PZ'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='CCAL'
+
OPT1  'Possible user option 1' C D='C2H6'
OPT2  'Possible user option 2' R D=0.0
 
>Guidance
Standard input is CRK 'FULL' 'P_PZ' 'FULL' 'CCAL' 'C2H6' 0.0
The insertion of the CRK line means that the RICH detector will
be placed in the central arm.  The geometry parameters of the
RICH are in the crk_par segment of the phnx.par file.  The first
four fields in the CRK line (STD1 to STD4) should remain as
written.  The fifth field (C2H6) specifies the radiator gas,
in this case Ethane.  The sixth field is set at 0.0.  For further
information contact Y. Akiba (IN%"akiba@sgs0.hirg.bnl.gov")
or see the CRK_2.DOC file.  For information about the PISORP
analysis of the RICH detector set, see the CRKANA.DOC file.
If the CRK line is omitted then the space occupied by that
volume will be filled with helium instead.
 
>Action PISA_ACT
 
**********************************************************************
 
>Command detectors
>PARAMETERS
XXX_   'Type HELP DETECTORS for general information.'  C D=' '
 
>Guidance
This is NOT a GGFGO_DAT command but a way of providing documentation.
 
Detector sets are specified by three letter codes followed by a space.
After the space, there MUST be four standard options.  For example,
 
CRK  'FULL' 'P_PZ' 'FULL' 'CCAL'
 
This line indicates that the RICH detector set will be inserted.
The first 'FULL' is always required for all detectors, at least for now.
Then the 'P_PZ' indicates the range of hit parameters to be stored.
The second 'FULL' indicates that the hits parameters are to be stored
during the call to GUSTEP.
The 'CCAL' indicates that the hits parameters, plus certain others, are
to be determined in CRK_DGI and written out to the FOUT file for PISORP.
 
In general these first four parameters (the "standard options") should
not be changed by the user without first talking to the subsystem
programmer.
 
HOWEVER, the user may change the third parameter to 'NONE' if the user
does not want the hits of this detector set to be stored during GUSTEP.
Similarly, the user may change the fourth parameter to 'NONE' if the
user does not want the hits, and other determined parameters, to be
written out to the PHNX.DAT file.
 
By specifying 'NONE' as the third parameter, one will have the benefit
of having the detector set in place without spending time or space in
storing its hit information.  Of course, this presumes that the user does
not need that detector's hit information for his/her subsequent analysis.
 
>Action PISA_ACT
 
**********************************************************************
 
>Command DOUT
>Parameters
+
DST1  'First data structure output'   C D=' '
DST2  'Second data structure output'  C D=' '
DST3  'Third data structure output'   C D=' '
DST4  'Fourth data structure output'  C D=' '
 
>Guidance
Standard input line is DOUT 'DIGI' 
List of data structures to be written to FOUT file.
Standard input line DOUT 'DIGI' will the detector
output data structure.  Other possibilities are
'KINE' 'VERT' 'JXYZ' and 'HITS'
If the DOUT line is omitted then only the output
data in each detector set's "DIGI" subroutine will
be written to the FOUT file.  NOTE: The FOUT file
can become very large if all the DOUT possibilities
are utilized.
 
>Action PISA_ACT
 
**********************************************************************
 
>Command EMC
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='FULL'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='ECAL'
THRS  'Energy threshold option'  C D='AUAU'
PBGL  'Cerenkov or no Cerenkov'  C D=' '
 
>Guidance
Standard input line is EMC 'ON' 'FULL' 'FULL' 'FULL' 'ECAL'
This command line puts in the EMCal detector set which
consists the Lead Shish-Kebab, the Lead Glass, and
optionally the high resolution crystals.  Gabor David
(IN%"DAVID1@BNLDAG", 516-282-3016) is the contact
person for this code.  Extensive comments are given
in the EMC and EMC_DIGI decks of the EMCFORTR.CMZ file.
The first set of comment lines is reproduced here, and
a text document file is in preparation.
A fifth option THRS is available for re-setting the
energy threshold in certain EMCal tracking media.
c
c     Update: December 27, 1993,  G. David
c
c     Rewritten for new geometry (octants) and better lead glass
c     geometry code: Dec. 93, G. David
c
c     Overhaul in October 1996, G. David.  In addition to many
c     changes in the geometry the entire EMCal PISA philosophy is changed
c     in order to get in line with the rest of PHENIX.  This involves
c     among other things changes in variables passed in UDETPAR, etc.
c     If you want to go back to the previous versions, use
c     EMCOLD -- EMC_DIGI  (and EMC_GINI)
c     However, this version uses
c     EMC -- EMC_HITS
c
c     ********************************
c
c     The PHENIX EMCal consists of up to 8 "walls" (or "octants").
c     Minimum 1, maximum 6 of them is Shish-Kebab; the number of
c     Shish-Kebab walls is determined by the (PHNX.PAR) parameter
c     EMC_WALLS.  Note that you have to define at least one
c     Shish-Kebab wall.
c
c     >>>>>>> Since 1995 the official word for "wall" is SECTOR ************
c
c     The last 2 sectors (7 and 8) are lead glass, and they get
c     defined whenever bit 2 of EMC_OPT (see below) is set.
c     That means that you can still have lead glass even if
c     EMC_WALLS = 1, i.e. only one Shish-Kebab is defined
c
c     EMC_OPT is the EMCal option (subdetectors required) bit pattern:
c     1: Shish-Kebab, 2: PbGl
 
>Action PISA_ACT
 
**********************************************************************
 
>Command FMCI
>Parameters
INFILE  'Binary file for input of event particles'  C D=' '
 
>Guidance
A standard input line could be FMCI '/u1/maguire/hijet.bin' .
This command specifies the name of the Monte Carlo input
file for particle kinematics and ID information.
there are standard interfaces for HIJET and VENUS event files.
See the HIJET and VENUS help commands for further information.

>Action PISA_ACT
 
**********************************************************************
 
>COMMAND FOUT
>Parameters
OFNAME  'Output file name'  C D='phnx.dat'
 
>Guidance
Standard input line is FOUT 'phnx.dat'
This command specifies the name of the output file
which will contain all the data structures and
"DIGI" subroutine output information.  The data
structures to be written are selected by the
DOUT control line.
 
>Action PISA_ACT
 
**********************************************************************
 
>Command FPAR
>Parameters
PARFILE 'Name of namelist parameter file' C D='phnx.par'
 
>Guidance
Standard input line is FPAR 'phnx.par'
All the active and passive volumes have their geometry
parameters specified in the namelist file called phnx.par.
 
>Action PISA_ACT
 
**********************************************************************
 
>Command GEOP
>Parameters
+
CENT    'Central magnet iron yoke'   C  D=' '
ENDC    'Endcap magnet iron yoke'    C  D=' '
PIPE    'Beam pipe'                  C  D=' '
NOSE    'Copper nosecone'	     C  D=' '
PLUG    'Copper flower pot'	     C  D=' '
CARR    'Central arm carriages'      C  D=' ' 
NTSH    'Neutron shield (obsolete)'  C  D=' '
PBSH    'Lead shield (obsolete)'     C  D=' '
BHTP    'Beam heater tape (obsolete)' C D=' ' 
>Guidance
Standard input line is GEOP 'CENT' 'ENDC' 'PIPE' 'NOSE' 'PLUG' 'CARR'
 
These parameters control the insertion or omission of the
central magnet iron, the endcap magnet iron, and the
beam pipe.  The actual geometry and media of these volumes
are controlled by the parameters in the phnx.par file.
The magnet yoke construction in PISA is described in the
magnet.doc text file, and the beampipe construction is
described in the pipe.doc file.
The copper nosecone reduces pion--->muon weak decay backgrounds.
The copper "plug" reduces backround into first muon tracking station.
The central arm carriages support the central arm detector subsystems.
The neutron shield, lead shield, and beam heater tape volumes
are no longer in the current muon arm design.  They are being
retained for optional studies.

NOTE: It is possible to have the iron yoke and no field,
or the field and no iron yoke.  See the GEOM, CENT, and
ENDC commands for other information.
 
GEOP stands for "passive volumes".
 
>Action PISA_ACT
 
**********************************************************************
 
>Command ITR
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='ETOT'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='TRKS'
+
OPT1  'Possible user option 1' I D=0
OPT2  'Possible user option 2' C D='NPAT'
OPT3  'Possible user option 3' I D=0
 
>Guidance
Standard input is ITR 'FULL' 'ETOT' 'FULL' 'TRKS' 0 'NPAT' 0
This line controls the insertion of the ITR detector set
consisting of DC1, DC2, and PAD1.  The present version in PISA
for this set is that described in the CDR consisting of 18
subsections each of DC1, DC2, and PAD1 arranged in two arms of
90 degrees each.  For previous information on the drift chamber
design, consult the ITR.DOC text file from Nikolai Smirnov.
For information on user options OPT1, OPT2, and OPT3 consult
the DC_TRACK.DOC text files which describes the pattern
recognition and momentum analysis features available in PISA
and PISORP.
 
>Action PISA_ACT
 
*********************************************************************
>Command MUI 
>Parameters
STD1 'Standard option 1' C D='FULL'
STD2 'Standard option 2' C D='P_ID'
STD3 'Standard option 3' C D='FULL'
STD4 'Standard option 4' C D='NCAL' 
STD5 'Standard option 5' C D='0.0'
STD6 'Standard option 6' C D='0.0'
STD7 'Standard option 7' C D='0.0'
STD8 'Standard option 8' C D='STCK'
STD9 'Standard option 9' C D='NNEU' 

>Action PISA_ACT

*********************************************************************
>Command MUPC 
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='PCAL'
 
>Guidance
Control line to insert the Pad chamber for Muon arm trigger upgrade
in to the PHENIX geometry.
>Action PISA_ACT

*********************************************************************
>Command NCC
*
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='ELEM'

>Guidance
Control line to insert the NCC calorimeter in to the PHENIX 
geometry.
>Action PISA_ACT
 
**********************************************************************
>Command MPC
*
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='ELEM'

>Guidance
Control line to insert the MPC calorimeter in to the PHENIX 
geometry.
>Action PISA_ACT

**********************************************************************
>Command MPCX
*
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='PCAL'

>Guidance
Control line to insert the MPC calorimeter (w/MPC-EX) in to the PHENIX 
geometry.
>Action PISA_ACT
 
**********************************************************************
>Command MXPS
*
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='PCAL'

>Guidance
Control line to insert the MPC-EX preshower in to the PHENIX 
geometry.
>Action PISA_ACT

**********************************************************************
>Command EXAB
*
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='PCAL'

>Guidance
Control line to enable absorber hits for MPC-EX preshower
>Action PISA_ACT

**********************************************************************
>Command EXNT
*
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='PCAL'

>Guidance
Control line to enable entry particles for MPC-EX preshower
>Action PISA_ACT

**********************************************************************
>Command RLT
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='PCAL'
 
>Guidance
Control line to insert the RPC chambers for Relative Luminosity Telescope 
in to the PHENIX geometry.
>Action PISA_ACT

********************************************************************
>Command TFW
>Parameters
STD1 'Standard option 1' C D='FULL'
STD2 'Standard option 2' C D='P_ID'
STD3 'Standard option 3' C D='FULL'
STD4 'Standard option 4' C D='WCAL'
STD5 'Standard option 5' C D='0.0'
STD6 'Standard option 6' C D='0.0'
STD7 'Standard option 7' C D='0.0'
STD8 'Standard option 8' C D='STCK'
STD9 'Standard option 9' C D='NNEU' 

>Action PISA_ACT

********************************************************************

>Command MUM
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='MCAL'
STD5  'Standard option 5'  C D='0.0'
STD6  'Standard option 6'  C D='0.0'
STD7  'Standard option 7'  C D='0.0'
STD8  'Standard option 8'  C D='STCK' 
STD9  'Standard option 9'  C D='NNEU' 
 
>Guidance
The control line is intended to insert the muon
endcap tracking detector geometry into PISA.
 
>Action PISA_ACT
 
*********************************************************************
 
>Command PAD
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='PCAL'
 
>Guidance
Control line to insert the PC2/PC3 outer
tracking system in to the PHENIX geometry.
>Action PISA_ACT
 
*********************************************************************
 
>Command TRD
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='TCAL'
 
>Guidance
Control line to insert the TEC outer
tracking system in to the PHENIX geometry.
>Action PISA_ACT
 
**********************************************************************
>Command TOF
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='FCAL'
STD5  'Standard option 5'  C D='0.0'
STD6  'Standard option 6'  C D='0.0'
 

>Action PISA_ACT
 
********************************************************************** 
>Command STEE
>Parameters
+
DST1 'First data structure to be created internally'   C D=' '
DST2 'Second data structure to be created internally'  C D=' '
DST3 'Third data structure to be created internally'   C D=' '
DST4 'Fourth data structure to be created internally'  C D=' '
 
>Guidance
Standard input line is STEE 'KINE' 'HITS' 'JXYZ' 'DIGI' .
This line controls which data structures GEANT will create
internally.  Unless the structures are created internally
then one cannot make use of them interactively (e.g. with
the DHITS [HITS data structure] or DXYZ [JXYZ data strucure]
commands) nor can one write them out with the DOUT command.
 
>Action PISA_ACT
 
**********************************************************************
 
>Command VER
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='P_ID'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='VCAL'
+
OPT1  'Possible user option STCK' C D=' ' 
>Guidance
Standard input line is EMC 'FULL' P_ID' 'FULL' 'VCAL'
This control line inserts the Silicon Vertex detector
into the PISA geometery.  This detector set also goes
by the acronym MVD (Multiplicity and Vertex Detector).

The use of the STCK option determines whether the track
number of the particle hits is stored in the FKIN data structure 
for later use in STAF. 

An extensive text document file, VERTEX.DOC, explains
the construction and analysis procedures for this
detector set. Below are the first few lines from this
document file:
               The Si Vertex detector in PISA
               ______________________________
 
               John P. Sullivan
               P2, MS D456
               Los Alamos National Lab
               Los Alamos, NM 87545
               Phone :(505) 665-5963
               FAX   :(505) 665-4986
               bitnet: SULLIVAN@LAMPF.LANL.GOV
               (most of the code was originally written
                by Ju Kang at UCRiverside)
 
I. Geometry
___________
 
 
    The vertex detector geometry is defined in subroutine ver (deck
//verfortr/verfortr/ver in cmz). The vertex detector is constructed from
ladders, where a "ladder" consists of an inner and outer detector plane
(Si), a rohacell support structure which holds them together, and an
electronics package made from alumina. As with other detectors, the key
 
>Action PISA_ACT

**************************************************************************
 
>Command MNAM
 
>Parameters
 
MAGNM   'Name of magnetic field map file'	 C D='3D++'

>Guidance

Name of magnetic field map file. This command was introduced
to allow for the multiple names which can be assigned to different
combinations of inner and outer field strengths.

>Action PISA_ACT

*************************************************************************
 
>Command MAGF
 
>Parameters
 
MAGF1   'Choice of 2D or 3D map'	 C D='3D01'
MAGF2	'Scale factor'			 R D=1.00
MAGF3   'Number of map files'            I D=0001
+
MAGF4   'Radial cut-off'                 R D=0.0
MAGF5   'Abs(Z) cut-off'                 R D=0.0
MAGF6   'Low momentum cut-off factor'    R D=0.0
 
>Guidance
 

This control line determines which magnetic field map is used.
The Run1/Run2 3D map is selected with a 3D01 choice as the first option field.
The Run3 3D map is selected with a 3D03 choice as the first option field.  An
old (February 1997) 2D map is chosen with a QUAD value;
equivalently 2DIM can be used.  The scale factor can be set
to be 0 to have no effective magnetic field.  With the QUAD (2DIM)
field choice ONLY
you can specify subsidiary magnetic fields.  There are optional fields on this
control line, which can be used to speed up certain central tracking
simulations. These fields will stop the tracking at a certain R or Z value, or
if it appears that the particle will not have enough transverse mommentum to
get into the active detector region starting with DC1. If you would like to
test out those speed-up options, please contact one of the core simulation
group presons.  For Central Arm only use, our recommendation is to have
the radial cut-off as 600.0 and the longitudinal cut-off as 400.0 cm,
and to have the low momentum cut-off factor as 1.0 .
 
>Action PISA_ACT
 
*************************************************************************
 
*>Command GEOM
*>Parameters
*GEOM1   'GEOM1'		I D=1
*GEOM2	'GEOM2'		I D=1
*GEOM3   'GEOM3' 	I D=3
*GEOM4   'GEOM4'		I D=0
*GEOM5   'GEOM5'		I D=1
*GEOM6 	'GEOM6'		I D=1
*
*>Guidance
*
*	Int medium ! Ext medium ! Geom print ! Hall medium
 
 
*>Action PISA_ACT
 
************************************************************************
 
*>Command AUTO
*>Parameters
*AUTO1 'AUTO1'	I D=1
 
*>Guidance
 
*	New automatic TMED parameter
 
*>Action PISA_ACT     
 
*****************************************************************************
 
>Command ZDC
>Parameters
STD1  'Standard option 1'  C D='FULL'
STD2  'Standard option 2'  C D='ETOT'
STD3  'Standard option 3'  C D='FULL'
STD4  'Standard option 4'  C D='ZCAL'
+
OPT1  'Possible user option 1' C D='FRG1'
OPT2  'Possible user option 2' C D='HB00'
OPT3  'Value of DX field'   R D=42.7
 
>Guidance
Standard input is ZDC 'FULL' 'ETOT' 'FULL' 'ZCAL' 'FRG1' 'HB00' 42.7

>Action PISA_ACT     

*****************************************************************************
>Command RUNN
>Parameters
RUNN1   'Input file run number'          I D=-1
+
RUNN2	'Output file number'	         I D=-1
RUNN3   'Project file number'            I D=0
RUNN4   'Version number'                 I D=0
 
>Guidance
Standard (default if not present) input is RUNN -1 -1 0 0

>Action PISA_ACT 

*****************************************************************************
>Command PHCONV
>Parameters
RMIN  'Minimum radius for the photon conversion'  R D=0.0  R=0.0:1000.0
RMAX  'Maximum radius for the photon conversion'  R D=400. R=0.0:1000.0
+
IOPT  'Optional distribution choice'              I D=0  R=-100:+100
 
>Guidance
Establish the range of radii to have a forced conversion
of the special photon particle #56.

>Action PISA_ACT 

*****************************************************************************
>Command SETRHIC
>Parameters
RHICRUN     'RHIC Run number'  I D=3  I=1:10000
RHICSUBRUN  'Extra parameter if needed within a given run'  I D=1 I=1:10000
 
>Guidance
Set the RHICRUN and RHICSUBRUN values for possible changes in the
detector geoemtry setup on a year-to-year basis

>Action PISA_ACT 

*****************************************************************************
>Command PISAFILE
>Parameters
FILENAME 'pisa output root filename'  C D='PISAEvent.root'
>Guidance
Set the name of the pisa output root file

>Action PISA_ACT 
