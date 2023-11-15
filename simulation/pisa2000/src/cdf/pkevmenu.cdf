*
*     CDF file which for the moment only creates a menu to handle defining
*       sources of PISA events at the KUIP level.
*
*    Original version by Brian Cole for commands SMOOTH, DIAMOND, TEXT_FILE
*     ======================================================================
*
>Name PKEVMENU
*
>MENU PISA_EVENT
>Guidance
Define and control PISA events sources

>Command SMOOTH
>Parameters
MULT    'Event multiplicity'        I  D=1     R=1:5
+
PMIN    'Minimum momentum (GeV)'    R  D=0.1    R=0.0:100.0
PMAX    'Maximum momentum (GeV)'    R  D=0.5    R=0.0:100.0
THMIN   'Minimum THETA (deg.)'      R  D=65.0    R=0.0:180.0
THMAX   'Maximum THETA (deg.)'      R  D=115.0   R=0.0:180.0
PHIMIN  'Minimum PHI (deg.)'        R  D=-90.0  R=-999.0:999.0
PHIMAX  'Maximum PHI (deg.)'        R  D=270.0   R=-999.0:999.0
P1ID    'Particle #1 ID'            I  D=8      R=0:100
P2ID    'Particle #2 ID'            I  D=8      R=0:100
P3ID    'Particle #3 ID'            I  D=8      R=0:100
P4ID    'Particle #4 ID'            I  D=8      R=0:100
P5ID    'Particle #5 ID'            I  D=8      R=0:100

>Guidance
Generate event with a flat (P, THETA, PHI) distribution.

MULT particles are generated with the chosen types and with
kinematic variables P, THETA, and PHI within the specified
ranges using a uniform distribution. NO acceptance cut is made.

>Action PKEVNT

*******************************************************************************

>Command TEXT_FILE
>Parameters
+
CHFILE   'Name of OSCAR TEXT file'                   C   D='oscar.input'
PHCONV   'Photon converter position if present'      R   D=-1.0    R=-1.0:100.0
IPOPSUB  'Number of particles per subevent'          I   D=100     R=1:1000
>Guidance

Tells pisa to open up the user written file oscar.input which 
needs to be an oscar1999A compliant file.
See the web page: http://www-cunuke.phys.columbia.edu/OSCAR/docs/file/cascade_output_format/osc99a.html for a definition
of an oscar compliant file.
The optional parameter PHCONV is used if one wants to reset the
event vertex from a photon converter position back to the beam
axis position using the particle's momemtum vector.  In that
case PHCONV should be set to the converter position radius which
was 29.0037 in Run2.  The second optional parameter ZRNDM provides
an additional event-by-event Z randomization.  A positive value
of ZRNDM corresponds to the sigma of a Gaussian distribution,
centered at 0.0 vertex.
A negative value of ZRNDM corresponds to a uniform +/- ZRNDM
distribuution about 0.0 vertex.

>Action PKEVNT

*******************************************************************************
>Command OLDEVENT

>Guidance
Switch to PISA 2.0 event input mode controlled by KINE.
The GUEVGEN routine is routed by the KINE = IKINE value.

PISA 3.0 event generators will be controlled by KUIP.
This PISA has a KUIP event menu for several event types.

>Action PKEVNT

*******************************************************************************
>Command CFM_SNGP
>Parameters
+
ISELECT  'Forced acceptance switch'            I D=0 R=0:1
IRANPHI  'Do azimuth random instead of cone'   I D=0 R=0:1

>Guidance
Reads event parameters from the cfmgmc.input text file.
An example of this file is given:

 5. 0.95  80.  30.  6.   2.  1.  2.  3. 0.10

This corresponds to:
  5.    ===>  mu+ GEANT ID number
  0.95  ===>  initial momentum ptot (GeV/c)
  80.   ===>  centroid theta angle, OR lowest theta angle if IRANPHI = 1
  30.   ===>  centroid phi angle, OR largest theta angle if IRANPHI = 1
  6.    ===>  emission cone for random angle, not used if IRANPHI = 1
  2.    ===>  number of particles of this ID type in this "event"
  1.    ===>  vertex x position (cm)
  2.    ===>  vertex y position (cm)
  3.    ===>  vertex z position (cm)
 0.10   ===>  fractional momentum increment for each particle in this event

Put the last parameter to be 0.0 to have all particles at the same ptot.
All "events" will use the same parameters contained in this single line.
The input text file can be edited interactively (use K/EDIT cfmgmc.input).
In this way you can test different kinematics conditions one at a time.
For multi-parameter multi-line input, see the CFM_MULTI command.

The ISELECT = 1 switch requires that the particle be in DC/PC1/PC3
in order for the event to be written to the ROOT output file.  The 
default is to not have forced acceptance.  This switch only makes
sense for Central Arm simulations.

The IRANPHI = 1 switch chooses a pure azimuthal randomization instead
of the default Theta cone randomization. All angles are in degree units.

>Action PKEVNT

*******************************************************************************
>Command CFM_MULTI

>Guidance
Reads event parameters from the cfmgmc.input text file.
An example of this file is given:

 5. 0.95  80.  30.  6.   2.  1.  2.  3. 0.10
 6. 0.95  80.  30.  6.   2.  1.  2.  3. 0.10

The parameters are the same as in the CFM_SNGP type event.
The difference is that each "event" reads one successive line.
The input text file can be edited interactively (use K/EDIT cfmgmc.input).
In this way you can test different kinematics conditions one at a time.

>Action PKEVNT

*******************************************************************************
>Command VENUS
>Parameters
IPOPSU   'Number of particles per subevent'       I   D=100    R=1:10000
+
CHFILE   'Name of VENUS output file'              C   D='FMCI line name'

>Guidance
Reads primary particle input from a VENUS output file.
The name of the VENUS output file is optional here.
If not given, it must be defined on the FMCI entry in PISA.KUMAC .
A VENUS file name given here overrides the FMCI line name.
Note that all the events in a file may be "recycled"
by re-issuing the VENUS command.  Because the random number
has changed in GEANT, this "recycling" may be useful in some
types of simulations (e.g. looking at photon conversion effects).
This event type requires a kinematics/id filter  event.par  file.
An example of such a file  event.par  is given here:

 c************************************************************
 c       event.par (for kinematic and particle ID cuts)
 c************************************************************
 $epar
  y_min = -1.e+20,
  y_max = +1.e+20,
  p_min = 0.010,
  p_max = +1.e+20,
  pt_min = 0.00,
  pt_max = +1.e+20,
  the_min = +9.00,
  the_max = +39.00,
  phi_min = -180.00,
  phi_max = +180.00,
  vrms = 0.0, 0.0, 0.0,
  iincl = +2,
  include = 8, 9, 18*0,
  north_south = 0,
 $end

This example takes only pi+ and pi- within the specified limits.
To accept all IDs, delete the last two lines before the $end .

>Action PKEVNT

*******************************************************************************
>Command HIJET
>Parameters
IPOPSU   'Number of particles per subevent'       I   D=100    R=1:10000
+
CHFILE   'Name of HIJET output file'              C   D='FMCI line name'

>Guidance
Reads primary particle input from a HIJET output file.
The name of the HIJET output file is optional here.
If not given, it must be defined on the FMCI entry in PISA.KUMAC .
A HIJET file name given here overrides the FMCI line name.
Note that all the events in a file may be "recycled"
by re-issuing the HIJET command.  Because the random number
has changed in GEANT, this "recycling" may be useful in some
types of simulations (e.g. looking at photon conversion effects).
See also the XY_RANDOM option as part of the "recycling" mode.
This event type requires a kinematics/id filter  event.par  file.
An example of such a file  event.par  is given here:

 c************************************************************
 c       event.par (for kinematic and particle ID cuts)
 c************************************************************
 $epar
  y_min = -1.e+20,
  y_max = +1.e+20,
  p_min = 0.010,
  p_max = +1.e+20,
  pt_min = 0.00,
  pt_max = +1.e+20,
  the_min = +9.00,
  the_max = +39.00,
  phi_min = -180.00,
  phi_max = +180.00,
  vrms = 0.0, 0.0, 0.0,
  iincl = +2,
  include = 8, 9, 18*0,
  north_south = 0,
 $end

This example takes only pi+ and pi- within the specified limits.

>Action PKEVNT

*******************************************************************************
>Command LUCIAE
>Parameters
IPOPSU   'Number of particles per subevent'       I   D=100    R=1:10000
+
CHFILE   'Name of LUCIAE output file'              C   D='FMCI line name'

>Guidance
Reads primary particle input from a LUCIAE output file.
The name of the LUCIAE output file is optional here.
If not given, it must be defined on the FMCI entry in PISA.KUMAC .
A LUCIAE file name given here overrides the FMCI line name.
Note that all the events in a file may be "recycled"
by re-issuing the LUCIAE command.  Because the random number
has changed in GEANT, this "recycling" may be useful in some
types of simulations (e.g. looking at photon conversion effects).
See also the XY_RANDOM option as part of the "recycling" mode.
This event type requires a kinematics/id filter  event.par  file.
An example of such a file  event.par  is given here:

 c************************************************************
 c       event.par (for kinematic and particle ID cuts)
 c************************************************************
 $epar
  y_min = -1.e+20,
  y_max = +1.e+20,
  p_min = 0.010,
  p_max = +1.e+20,
  pt_min = 0.00,
  pt_max = +1.e+20,
  the_min = +9.00,
  the_max = +39.00,
  phi_min = -180.00,
  phi_max = +180.00,
  vrms = 0.0, 0.0, 0.0,
  iincl = +2,
  include = 8, 9, 18*0,
  north_south = 0,
 $end

This example takes only pi+ and pi- within the specified limits.

>Action PKEVNT

*******************************************************************************
>Command OSCAR
>Parameters
CHFILE   'Name of OSCAR ROOT file'             C   D='FMCI line name'
+
ISTART   'Starting event in NTUPLE'            I   D=1  R=1:10000000
NCLONE   'Number of clones (recycles)'         I   D=1  R=1:100
STAGE    'Staged OSCAR switch'                 I   D=0  R=0:1
PHCONV   'Photon converter position if present'    R   D=-1.0    R=-1.0:100.0

>Guidance
Reads primary particle input from an OSCAR ROOT file (NTUPLE).
The name of the OSCAR ROOT file is optional here.
If not given, it must be defined on the FMCI entry in PISA.KUMAC .
An OSCAR file name given here overrides the FMCI line name.
All of the particles in a give event will be processed.

The optional parameter NCLONE tells how many times to clone,
or recycle the OSCAR input file.

Stage = 0 means PDG values for particle ID.
Stage = 1 means GEANT values for particle ID.

The optional parameter PHCONV is used if one wants to reset the
event vertex from a photon converter position back to the beam
axis position using the particle's momemtum vector.  In that
case PHCONV should be set to the converter position radius which
was 29.0037 in Run2.

>Action PKEVNT

*******************************************************************************
>Command HIJING
>Parameters
IPOPSU   'Number of particles per subevent'       I   D=100    R=1:10000
+
CHFILE   'Name of HIJING output file'             C   D='FMCI line name'

>Guidance
Reads primary particle input from a HIJING output file.
The name of the HIJING output file is optional here.
If not given, it must be defined on the FMCI entry in PISA.KUMAC .
A HIJING file name given here overrides the FMCI line name.
Note that all the events in a file may be "recycled"
by re-issuing the HIJING command.  Because the random number
has changed in GEANT, this "recycling" may be useful in some
types of simulations (e.g. looking at photon conversion effects).
See also the XY_RANDOM option as part of the "recycling" mode.
This event type requires a kinematics/id filter  event.par  file.
An example of such a file  event.par  is given here:

 c************************************************************
 c       event.par (for kinematic and particle ID cuts)
 c************************************************************
 $epar
  y_min = -1.e+20,
  y_max = +1.e+20,
  p_min = 0.010,
  p_max = +1.e+20,
  pt_min = 0.00,
  pt_max = +1.e+20,
  the_min = +9.00,
  the_max = +39.00,
  phi_min = -180.00,
  phi_max = +180.00,
  vrms = 0.0, 0.0, 0.0,
  iincl = +2,
  include = 8, 9, 18*0,
  north_south = 0,
 $end

This example takes only pi+ and pi- within the specified limits.

>Action PKEVNT

*******************************************************************************
>Command VNI
>Parameters
IPOPSU   'Number of particles per subevent'       I   D=100    R=1:10000
+
CHFILE   'Name of VNI output file'             C   D='FMCI line name'

>Guidance
Reads primary particle input from a VNI output file.
The name of the VNI output file is optional here.
If not given, it must be defined on the FMCI entry in PISA.KUMAC .
A VNI file name given here overrides the FMCI line name.
Note that all the events in a file may be "recycled"
by re-issuing the VNI command.  Because the random number
has changed in GEANT, this "recycling" may be useful in some
types of simulations (e.g. looking at photon conversion effects).
See also the XY_RANDOM option as part of the "recycling" mode.
This event type requires a kinematics/id filter  event.par  file
as in HIJING.

>Action PKEVNT

*******************************************************************************
>Command RQMD
>Parameters
IPOPSU   'Number of particles per subevent'       I   D=100    R=-10000:10000
+
CHFILE   'Name of RQMD output file'               C   D='FMCI line name'

>Guidance
Reads primary particle input from a RQMD output file.
The name of the RQMD output file is optional here.
Note that all the events in a file may be "recycled"
by re-issuing the RQMD command.  Because the random number
has changed in GEANT, this "recycling" may be useful in some
types of simulations (e.g. looking at photon conversion effects).
See also the XY_RANDOM option as part of the "recycling" mode.
This event type requires a kinematics/id filter  event.par  file.
An example of such a file  event.par  is given here:

 c************************************************************
 c       event.par (for kinematic and particle ID cuts)
 c************************************************************
 $epar
  y_min = -1.e+20,
  y_max = +1.e+20,
  p_min = 0.010,
  p_max = +1.e+20,
  pt_min = 0.00,
  pt_max = +1.e+20,
  the_min = +9.00,
  the_max = +39.00,
  phi_min = -180.00,
  phi_max = +180.00,
  vrms = 0.0, 0.0, 0.0,
  iincl = +2,
  include = 8, 9, 18*0,
  north_south = 0,
 $end

This example takes only pi+ and pi- within the specified limits.

>Action PKEVNT

*******************************************************************************

>Command NEXUS
>Parameters
IPOPSU   'Number of particles per subevent'       I   D=100    R=1:10000

>Guidance
Reads primary particle input from a NEXUS ASCII output file.

>Action PKEVNT

*******************************************************************************

>Command AUVENUS
>Parameters
IPOPSU   'Number of particles per subevent'       I   D=100    R=1:10000

>Guidance
Interface for reading Au VENUS event files.
Paul Kirk (LSU) is the primary contact person for these events.

>Action PKEVNT

*******************************************************************************

>Command PYTHIA
>Parameters
IPOPSU   'Number of particles per subevent'       I   D=100    R=1:10000

>Guidance
Interface for reading Pythia event files.
Yuji Goto is the primary contact person for these events.

>Action PKEVNT

*******************************************************************************

>Command PHPYTHIA
>Parameters
IPOPSU   'Number of particles per subevent'       I   D=100    R=1:10000
+
CHFILE   'Name of PHPythia file'                  C   D='pythia.root'

>Guidance
Interface for reading PHPythia event files.
Mickey Chiu is the primary contact person for these events.

>Action PKEVNT

*******************************************************************************

>Command MCDIM
>Parameters
MXTOT  'Total number of particles'    I  D=1       R=1:10000
SQRT_S 'Momentum value (?)'           R  D=0.0     R=0.0:100.0
ECUT   'Energy cut (?)'               R  D=0.0     R=0.0:100.0
TMIN   'Minimum theta angle'          R  D=0.0     R=0.0:180.0
TMAX   'Maximum theta angle'          R  D=37.0    R=0.0:180.0
+
IPOPSU   'Number of particles per subevent'       I   D=100    R=1:10000

>Guidance
Dimuon generator from Soren Sorensen

>action PKEVNT

*******************************************************************************
>Command MCMUM
>Parameters
MXTOT  'Total number of particles'    I  D=1       R=1:10000
IDD    'Particle choice (0,1,-1)'     I  D=1       R=-1,1
P_MIN  'Minimum momentum'             R  D=0.0     R=0.0:100.0
P_MAX  'Maximum momentum'             R  D=10.0    R=0.0:100.0
TH_MIN 'Minimum theta value'          R  D=0.0     R=0.0:180.0
TH_MAX 'Maximum theta value'          R  D=37.0    R=0.0:180.0
PH_MIN 'Minimum phi angle'            R  D=0.0     R=0.0:360.0
PH_MAX 'Maximum phi angle'            R  D=360.0   R=0.0:360.0
+
IPOPSU 'Number of particles in subevent'  I D=100  R=1:20000

>Guidance
Particle distribution generator from Soren Sorensen
IDD = 0 selects mu+ and mu-.
IDD = 1 selects mu+ only.
IDD = -1 selects mu- only.

>action PKEVNT

*******************************************************************************
>Command MCRAP
>Parameters
MXTOT  'Total number of particles'    I  D=1       R=1:10000
ID     'Particle ID'                  I  D=5       R=-10000:10000
Y_MIN  'Minimum rapidity'             R  D=0.0     R=-10.0:10.0
Y_MAX  'Maximum rapidity'             R  D=10.0    R=-10.0:10.0
PT_MIN 'Minimum P_T value'            R  D=0.0     R=-100.0:100.0
PT_MAX 'Maximum P_T value'            R  D=10.0    R=0.0:100.0
PH_MIN 'Minimum phi angle'            R  D=0.0     R=-360.0:360.0
PH_MAX 'Maximum phi angle'            R  D=360.0   R=0.0:360.0
TEMPERATURE 'Temperature for transverse mass' R D=0.0  R=0.0:1000.0
+
IPOPSU 'Number of particles in subevent'  I D=100  R=1:20000

>Guidance
Rapidity distribution generator from Soren Sorensen

>action PKEVNT

*******************************************************************************
>Command STPLV2
>Parameters
V2CONST 'V2 anisotropy'                 R  D=0.0      R=-100.0:100.0
+
IDPOLAR 'ID of particle to polarize'    I  D= 0       R=-1:2000
RHO000  'Rest frame polarization constant' R D=0.3333333 R=-1.0:1.0
PZCONST 'Polarization constant'         R  D=0.0      R=-100.0:100.0

>Guidance
Set polarization and reaction plane v2 anisotropy constants

V2CONST is constant V2 as long as V2CONST is greater than -0.9.

If V2CONST = -1, then the recombination model dependence for v2 with pT is used.

If V2CONST = -2, then ShinIchi's formula for v2 with pT and rapidity is used.

IDPOLAR is the particular meson decay which is being polarized along the
impact parameter.  If IDPOLAR is set to -1, then all particles
decaying into two daughters will be polarized in the parent rest frame.

The RHO000 constant sets the scale of this polarization according
to a pair transverse momentum dependent parameterization.
Possible values for RHO000 are 1/3 for no polarization, or limits of 0 and 1.
See the Liang and Wang paper (arXiv:nucl-th/0411101v1).  If RHO000 is set
to a negative value, then the polarization parameter will be -RHO000,
independent of momentum.

PZCONST is another method of polarization which is under development, i.e.
do not use it.

>action PKEVNT

*******************************************************************************
>Command PM_RHIC
>Parameters
+
IEORMU  'Choice of muon (=1), electron (=0), or kaon (=2) decay particles'  I  D=1 R=0:2
NORTH_SOUTH  'Muon Arm combination for angular filter'  I   D=0  R=0:1
THEMIN   'Minimum angle for accepted particle'    R   D=0.0     R=0.0:180.0
THEMAX   'Maximum angle for accepted particle'    R   D=180.0   R=0.0:180.0
P_MIN    'Minimum momentum for accepted particle' R   D=0.0     R=0.0:100.0

>Guidance
Event source is an external file ASCII file called dimuon.dat.
This file is produced by the LANL RHIC_ALL program.
The choice of IEORMU 0, 1, or 2 indicates that the particles are e, mu, or k.
The NORTH_SOUTH option 0 means the angular limits are for the North Arm only.
The NORTH_SOUTH option 1 means both North and South Arms angles are used.
The THEMIN is the minium allowed angle (in degrees) for an accepted particle.
The THEMAX is the maximum allowed angle (in degrees) for an accepted particle.
The angle limits are reflected about 90 degrees if NORTH_SOUTH = 1.
The P_MIN is the minimum momentum for an accepted particle.

>Action PKEVNT

*******************************************************************************

>Command SNGL_PHI
>Parameters
+
IEORMU   'Choice of muon (=0), electron (=1) decay, or kaon (=2,3)'   I   D=1  R=0:3
ISELEC   'Event filter selection on accepted particle'  I   D=3  R=1:3
YMIN     'Minimum rapidity for Phi meson'         R   D=-9999.  R=-9999.:9999.
YMAX     'Maximum rapidity for Phi meson'         R   D=+9999.  R=-9999.:9999.
PTMIN    'Minimum transverse momentum for Phi meson'  R  D=0.0  R=0.0:9999.
PTMAX    'Maximum transverse momentum for Phi meson'  R  D=+9999.  R=0.0:9999.
PHIMIN   'Minimum phi angle for accepted particle'    R   D=0.0     R=-360.0:360.0
PHIMAX   'Maximum phi angle for accepted particle'    R   D=360.0   R=-360.0:360.0
THEMIN   'Minimum theta angle for accepted particle'    R   D=0.0     R=0.0:180.0
THEMAX   'Maximum theta angle for accepted particle'    R   D=180.0   R=0.0:180.0
PMIN     'Minimum momentum for accepted particle' R   D=0.0     R=0.0:100.0
>Guidance
Event source is the internal SNGL_PHI generator for the Phi meson decay.
The P_T and Y are from uniformly distributed within the limits given.
The choice of ISELEC = 1 means at least one decay particle is in the filter.
The choice of ISELEC = 2 means both decay particles are in the filter.
The choice of ISELEC = 3 means that both decay particles in the PC1/PC2/PC3

>Action PKEVNT

*******************************************************************************

>Command SNGL_NEUTRAL
>Parameters
+
NEUTRAL  'Choice of pizero (=7) or eta (=17) two photon decay'   I   D=7  R=7:17
ISELEC   'Event filter selection on accepted particle'  I   D=3  R=1:4
YMIN     'Minimum rapidity for Neutral meson'         R   D=-0.6  R=-9999.:9999.
YMAX     'Maximum rapidity for Neutral meson'         R   D=+0.6  R=-9999.:9999.
PTMIN    'Minimum transverse momentum for Neutral meson'  R  D=0.0  R=0.0:9999.
PTMAX    'Maximum transverse momentum for Neutral meson'  R  D=+10.  R=0.0:9999.
PHIMIN   'Minimum azimuthal angle for accepted particle'    R   D=-90.0  R=-999.0:999.0
PHIMAX   'Maximum azimuthal for accepted particle'    R   D=270.0   R=-999.0:999.0
THEMIN   'Minimum polar angle for accepted particle'    R   D=65.0  R=0.0:180.0
THEMAX   'Maximum polar angle for accepted particle'    R   D=115.0 R=0.0:180.0
PMIN     'Minimum momentum for accepted particle' R   D=0.1     R=0.0:100.0
ZWIDTH   'Z Vertex variation: + Gaussian, - Uniform'    R   D=0.0   R=-50:50
RANSKIP  'Number of times (millions) to pre-call GRNDM' I   D=0     R=0:100
>Guidance
Event source is the internal SNGL_NEUTRAL generator for the Neutral meson decay.
The P_T and Y are from uniformly distributed within the limits given.
The choice of ISELEC = 1 means at least one decay photon is in the filter.
The choice of ISELEC = 2 means both decay photons are in the filter.
The choice of ISELEC = 3 means both decay photons are headed toward the EMCal sectors.
The choice of ISELEC = 4 means both decay photons enter the EMCal sectors (forced accept mode)

>Action PKEVNT

*******************************************************************************

>Command SNGL_JPSI
>Parameters
+
IEORMU  'Choices are 0,1 or 10,11 '   I   D=0  R=0:11
ISELEC   'Event filter selection on accepted particle'  I   D=3  R=1:3
YMIN     'Minimum rapidity for J/Psi meson'         R   D=-9999.  R=-9999.:9999.
YMAX     'Maximum rapidity for J/Psi meson'         R   D=+9999.  R=-9999.:9999.
PTMIN    'Minimum transverse momentum for J/Psi meson'  R  D=0.0  R=0.0:9999.
PTMAX    'Maximum transverse momentum for J/Psi meson'  R  D=+9999.  R=0.0:9999.
THEMIN   'Minimum angle for accepted particle'    R   D=0.0     R=0.0:180.0
THEMAX   'Maximum angle for accepted particle'    R   D=180.0   R=0.0:180.0
PMIN     'Minimum momentum for accepted particle' R   D=0.0     R=0.0:100.0
>Guidance
Event source is the internal SNGL_JPSI generator for the J/Psi meson decay.
IEORMU = 0 or 1 means J/PSI into dielectron or dimuon decay, respectively.
IEORMU = 10 or 11 means PSI-PRIME into dielectron or dimuon decay.
The P_T and Y are from uniformly distributed within the limits given.
The choice of ISELEC = 1 means at least one decay particle is in the filter.
The choice of ISELEC = 2 means both decay particles are in the filter.
The choice of ISELEC = 3 means that both decay particles in the PC1/PC2/PC3
acceptance.

>Action PKEVNT

*******************************************************************************

>Command RV_PHI
>Parameters
+
IEORMU  'Choice of muon (=0), electron (=1) decay, or kaon (=2,3)'   I   D=1  R=0:3
ISELEC  'Event filter selection on accepted particle'  I   D=0  R=0:3
THEMIN   'Minimum angle for accepted particle'    R   D=0.0     R=0.0:180.0
THEMAX   'Maximum angle for accepted particle'    R   D=180.0   R=0.0:180.0
P_MIN    'Minimum momentum for accepted particle' R   D=0.0     R=0.0:100.0
Y_MIN    'Minimum rapidity for Phi meson'         R   D=-9999.  R=-9999.:9999.
Y_MAX    'Maximum rapidity for Phi meson'         R   D=+9999.  R=-9999.:9999.
NUM_RV   'Number of particles per event'          I   D=1       R=-100:100
>Guidance
Event source is the internal RV_PHI generator for the phi meson decay.
The P_T and Y are from an interpolation of Ramona Vogt's yield tables.
The choice of ISELEC = 0 means all decay particles are tracked (no filter).
The choice of ISELEC = 1 means at least one decay particle is in the filter.
The choice of ISELEC = 2 means both decay particles are in the filter.
The choice of ISELEC = 3 means that both decay particles in the PC1/PC2/PC3
acceptance.
The filter is given by the THEMIN, THEMAX, and P_MIN parameters.

>Action PKEVNT

*******************************************************************************

>Command RV_CHI
>Parameters
+
IEORMU  'Choice of muon (=0) or electron (=1) decay'   I   D=1  R=0:1
ISELEC  'Event filter selection on accepted particle'  I   D=0  R=0:3
THEMIN   'Minimum angle for accepted particle'    R   D=0.0     R=0.0:180.0
THEMAX   'Maximum angle for accepted particle'    R   D=180.0   R=0.0:180.0
P_MIN    'Minimum momentum for accepted particle' R   D=0.0     R=0.0:100.0
Y_MIN    'Minimum rapidity for Chi meson'         R   D=-9999.  R=-9999.:9999.
Y_MAX    'Maximum rapidity for Chi meson'         R   D=+9999.  R=-9999.:9999.
NUM_RV   'Number of particles per event'          I   D=1       R=1:100
>Guidance
Event source is the internal RV_CHI generator for the Chi meson decay.
The P_T and Y are from an interpolation of Ramona Vogt's yield tables for
the J/Psi.  The Chi (3.51 GeV) decays into J/Psi-photon pair
The choice of ISELEC = 0 means all decay particles are tracked (no filter).
The choice of ISELEC = 1 means at least one J/Psi decay particle is in the filter.
The choice of ISELEC = 2 means both decay J/Psi particles are in the filter.
The choice of ISELEC = 3 means that both J/Psi decay particles in the PC1/PC2/PC3
acceptance.
The filter is given by the THEMIN, THEMAX, and P_MIN parameters.

>Action PKEVNT

*******************************************************************************

>Command RV_JPSI
>Parameters
+
IEORMU  'Choices are 0,1 or 10,11 '   I   D=1  R=0:11
ISELEC  'Event filter selection on accepted particle'  I   D=0  R=0:3
THEMIN   'Minimum angle for accepted particle'    R   D=0.0     R=0.0:180.0
THEMAX   'Maximum angle for accepted particle'    R   D=180.0   R=0.0:180.0
P_MIN    'Minimum momentum for accepted particle' R   D=0.0     R=0.0:100.0
PHIMIN1  'Minium azimuth in arm 1 for particle'   R   D=-90.0  R=-999:999.
PHIMAX1  'Maximum azimuth in arm 1 for particle'  R   D=+270.0  R=-999:999.
PHIMIN2  'Minium azimuth in arm 2 for particle'   R   D=-90.0  R=-999:999.
PHIMAX2  'Maximum azimuth in arm 2 for particle'  R   D=+270.0  R=-999:999.


>Guidance
Event source is the internal RV_JPSI generator for  J/Psi meson decay.
The P_T and Y are from an interpolation of Ramona Vogt's yield tables.
IEORMU = 0 or 1 means J/PSI into dielectron or dimuon decay, respectively.
IEORMU = 10 or 11 means PSI-PRIME into dielectron or dimuon decay.
The Psi-prime P_T and Y yield tables are taken as same as for the J/Psi.
The choice of ISELEC = 0 means all decay particles are tracked (no filter).
The choice of ISELEC = 1 means at least one decay particle is in the filter.
The choice of ISELEC = 2 means both decay particles are in the filter.
The choice of ISELEC = 3 means that both decay particles in the PC1/PC2/PC3
acceptance.
The filter is given by the THEMIN, THEMAX, P_MIN, PHIMIN and PHIMAX  parameters.

>Action PKEVNT

*******************************************************************************
>Command UA1
>Parameters
+
IPOPSU   'Number of particles per subevent'       I   D=100    R=1:10000

>Guidance
Kang-Seto Scaling Function generates "HIJET-like" events.
Events currently will have pi+, pi-, and pi0 as requested.
In future more particle choices may be added.
See the file UA1_EVT.DOC for more information.

Particles in this event type are filtered by the ua1event.par file.
An example ua1event.par is given for "double HIJET" pi+ and pi- only:
 $ua1_par
  ntotal = 18000,
  nhrndm  = 6000,
  iincl = 2,
  include = 8, 9, 18*0,
  y_sig = 2.5,
  the_minc = 67.0,
  the_maxc = 115.0,
  the_minn = 50.0,
  the_maxn = 130.0,
  p_minn = 0.01,
  p_minc = 0.09,
  pt_max = 5.0,
  north_south = 0,
 $end

If the ua1event.par file is not in the user's subdirectory, then
an internal set of parameters will be used as below:

 $ua1_par
  integer iincl /3/          ! number of included particles
  integer include(20) /7, 8, 9, 17*0/  ! input list of included GEANT IDs
  INTEGER NTOTAL /9000/      ! number of particles for single HIJET density
  INTEGER NHRNDM /10000/     ! size of random array
  REAL THE_MINC /0.0/        ! minimum charged particle theta
  REAL THE_MAXC /180./       ! maximum charged particle theta
  REAL THE_MINN /0.0/        ! minimum neutral particle theta
  REAL THE_MAXN /180./       ! maximum neutral particle theta
  REAL P_MINC /0.0/          ! minimum charged particle momentum
  REAL P_MINN /0.0/          ! minimum neutral particle momentum
  REAL Y_SIG /2.5/           ! Gaussian rapidity width sigma
  REAL PT_MAX /5.0/          ! maximum transverse momentum
 $end

>Action PKEVNT

*******************************************************************************
>Command QED_EE
>Parameters
ISTATX	 'Command word for event control'	 I   D=0    R=0:11
+
IMP      'Value for forced impact paramter'      R   D=0.   R=0.:40000.
INVM     'Value for forced invariant mass'       R   D=0.   R=0.:10000.
RAPID    'Value for forced rapidity'             R   D=0.   R=0.:0.9999

>Guidance
An E-E+ QED event generator. Written by M. Fatyga.

The variable ISTATX gives the user control over the selection of the other
event variables. The values of ISTATX can be:
	0.....full event, one pair
	1.....force the impact parameter, IMP; one pair
	2.....force the invariant mass, INVM; one pair
	3.....force the rapidity, RAPID; one pair
	10....full event, multiple pairs
	11....force the impact parameter; multiple pairs
In each case, the value forced should be given by the user.
Note: forcing one value forces all above it in the list; i.e. the imapct
parameter can be set alone, but setting the rapidity requires that both
of the other values be set as well.

>Action PKEVNT
*******************************************************************************
>Command BEAM_GAS
>Parameters
+
IPOPSU	   'Number of particles per subevent'    I   D=10  R=0:10000
HFILE      'Flag for including Hydrogen file'    I   D=1   R=0:1
HEFILE     'Flag for including Helium file'      I   D=0   R=0:1
NFILE      'Flag for including Nitrogen file'    I   D=0   R=0:1   
TRUE_DIS   'Flag for true distributions'         I   D=0   R=0:1

>Guidance
Interface for reading beam gas event files.
Paul Kirk (LSU) is the primary contact person for these events.

IPOPSU is the number of primary particles per subevent.
HFILE = 1 indicates the hydrogen event file will be read.
HEFILE = 1 indicates the helium event file will be read.
NFILE = 1 indicates the nitrogen event file will be read.

>Action PKEVNT
*******************************************************************************
>Command XY_RANDOM
>Parameters
IXYRND   'XY randomization option for P_T'       I   D=0    R=0:2

>Guidance
Command to change the XY randomization of the primary event particles.
For IXYRND = 0, there will be no XY randomization of the P_T values.
For IXYRND = 1, all particles will have their transverse momentum
vector rotated by the same angle PHI, with a different angle PHI
at the start of each new event.  In this manner, correlations in
the event generator are preserved.
For IXYRND = 2, each particle will have a different PHI rotation
angle.  This will tend to destroy correlations in the event
generator, assuming that those exist and are important.
This option is meant, but not limited to, especially to be used
when HIJET or VENUS input files are "recycled".

>Action PKEVNT

*******************************************************************************
>Command GEN_EVT
>Parameters
+
IPOPSU   'Number of particles per subevent'       I   D=100    R=1:10000

>Guidance
GENERIC Event generator for PISA.  It is assumed that the user has
provided a special object module gen_evt.o to fill the pre-buffers in the
/evgen/ common blocks.  See the gen_evt template subroutine in the local cvs
../pisa/src/phnxcore subdirectory for further instructions.  DO NOT commit
any modified version of this file to the repository.

>Action PKEVNT

*******************************************************************************
>Command DUO_GEN_EVT
>Parameters
+
IPOPSU   'Number of particles per subevent'       I   D=100    R=1:10000

>Guidance
Dual particle generator (C.F. Maguire original author).  This event will launch
correlated pairs of particles at specified angles.  Such "events" can be useful
for testing various resolutions in either the central or the muon arms, for all
subsystems.  The routine duo_gen_evt will be called, and this routine obtains
its input from the duo.par namelist file which must be provided by the user.
An example of this file is provided below.  The parameter names are largely
self-explantory.  The second particle will always be at a fixed angle in theta
and in phi away from the first particle, as given in the duo_theta and the
duo_phi lines.  If either the duo_ran_th1 or the duo_ran_phi1 are positive,
then the first particle theta and/or phi angles will be uniformly randomized
according to the size of duo_ran variable.  The centroids of the random
distribution will be the angles given in the duo_theta and duo_phi lines.

 $duo_par
  duo_npairs = 1,
  duo_partid = 2,2,
  duo_theta  = 80.0, 80.0,
  duo_phi    = 3.0, 9.0,
  duo_ran_th1 = 0.0,
  duo_ran_ph1 = 0.0,
  duo_ptot   = 0.5, 0.5,
  duo_zvert  = 0.0, 0.0,
  duo_ran_z1 = 0.0,
 $end

>Action PKEVNT

*****************************************************************************
>Command STAGE
>Parameters
NSTAGE  'Number of this stage'  I D=1  R=1:9
ZSTAGE  'Z Boundary for this stage'  R D=60.0 R=-1500.0:+1500.0
PSTAGE  'Minimum momentum for output from this stage' R D=0.5 R=0.0:100.0
+
FILTER  'Filter condition (0 is all, 1 is primary only)' I D=0 R=0:5 

>Guidance
Allows for doing a simulation in stages as a function of Z,
with particles having output momentum at least as large as PSTAGE.
Filter condition 0 takes all particle, condition 1 takes primaries
only plus decay muons checking at the Z boundary layers.  Condition
2 is the same as 1 except that the checking is done at each Z step.

>Action PKEVNT

*****************************************************************************
>Command STACK_STAGE
>Parameters
NSTAGE  'Number of this stage'  I D=1  R=0:9
ZSTAGE  'Z Boundary for this stage'  R D=60.0 R=-1500.0:+1500.0
PSTAGE  'Minimum momentum for output from this stage' R D=0.5 R=0.0:100.0
NCLONES 'Number of clones to make at this stage' I D=5 R=1:9
+
FILTER  'Filter condition (0 is all, 1 is primary only)' I D=0 R=0:5 

>Guidance
Allows for doing a simulation in stages as a function of Z,
with particles having output momentum at least as large as PSTAGE.
Filter condition 0 takes all particle, condition 1 takes primaries
only plus decay muons checking at the Z boundary layers.  Condition
2 is the same as 1 except that the checking is done at each Z step
Particles meeting the kinematics condition are killed at the stage
boundary, and then put on the GEANT stack in the same event for
a total of NCLONES times.

>Action PKEVNT

*****************************************************************************
>Command MUIWR
>Parameters
MuIDLayer  'Required MuID Layer'  I D=0  R=0:6

>Guidance
The event output will not be written to the PISAEvent.root
file unless there is at least one particle in the event
which is found in the specified MuIDLayer.  A value of
0 has the effect of accepting all events.  A value of
6 means that the particle was in the sixth MuID layer
which exists in simulation but not in the real detector.
So particles getting to the sixth MuID layer may
actually have gone through the preceding five real layers. 

>Action PKEVNT

*****************************************************************************
>Command NSKIP
>Parameters
NumSkip  'Number of input events to skip'  I D=0

>Guidance
Sets the common block variable IKINE2(1) to the value of NumSkip.
This will be the number of events the input loop will skip before
processing events with PTRIG.

Highly experimental, use at your own risk!  [Dave Winter]

>Action PKEVNT
