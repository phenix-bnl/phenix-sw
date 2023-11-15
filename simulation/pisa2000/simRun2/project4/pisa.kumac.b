*
*  PISA kumac file for Run2 Simulation Project #4 (Central Arm HIJING Simulations)
*                  Muon Arms not installed,  no PLUG or Beam  Collar
*                  Use 3D map, helium bag, Photon Shields
*                  The MAGF control line limits the tracking to R < 750 cm, |Z| < 375 cm
*                  in order to speed up the simulation.  The |Z| limits are good for the BBC background
*                  The RNDM iji 000 line should be in the run-by-run pisa.input file
*                  NOTE: Some of Project #4 will use the CONV photon converter in the GEOP line
*
* Random number seeds (using internal GEANT seeds from 001 to 215)
*        keep the second input as 0 all the time, change only the first

* The RNDM ijk 000 line should be placed in the run-by-run pisa.input file
* This makes the pisa.kumac file run-independent, except for the converter option
*  
*
*
* Tracking thresholds
*
CUTS  .001 .001 .010 .010 .010 .001 .001 1.e+4 1.e+4 .010 4.e-7
*
* NOTE: As of December 1, 1999 the compressed ZEBRA output format is not used by default
*       The FOUT line will be used only if the SWIT(1) value is changed to 6 in gffgo.dat
*
FOUT 'phnx.dat.cZ'              ! Name of output hits file
FPAR 'phnx.par'                 ! Name of namelist parameter file for geometry
STEE 'KINE' 'HITS' 'DIGI'       ! Output data structure control
DOUT 'DIGI'                     ! Output data structure control
MAGF '3D01' 1.00 0001 750. 375.
*GEOP 'ENDC' 'CENT' 'PIPE' 'NOSE' 'HBAG' 'PHSH'
GEOP 'ENDC' 'CENT' 'PIPE' 'NOSE' 'HBAG' 'PHSH' 'CONV'
DCAY  1                         ! GEANT command for decay on
ANNI  1                         ! GEANT command for annihilation on
BREM  1                         ! GEANT command for bremsstrahlung on
COMP  1                         ! GEANT command for Compton scattering on
LOSS  2                         ! GEANT command for Landau fluctuations on
DRAY  0                         ! GEANT command for Delta no ray (Landau is on)
HADR  4                         ! GEANT command for hadrons using FLUKA
MULS  1                         ! GEANT command for multiple scattering on
PAIR  1                         ! GEANT command for pair production on
PHOT  1                         ! GEANT command for photoelectric effect on
MUNU  0                         ! GEANT recommendation when HADR 4 is used
***************************************************************************
*
*	To install a detector turn the switch 'ON'
*
***************************************************************************
VER  'ON' 'FULL' 'P_ID' 'FULL' 'VCAL' 'STCK'   ! MVD on with track stack used
BBC  'ON' 'FULL' 'ETOT' 'FULL' 'BCAL' 'STCK'   ! BBC on with track stack used
ITR  'ON' 'IT96' 'ETOT' 'FULL' 'TRKS'          ! Latest version of Dch and PC1
CRK  'ON' 'FULL' 'P_PZ' 'FULL' 'CCAL' 'CO2 '   ! RICH with CO2 radiator gas
PAD  'ON' 'PC98' 'P_ID' 'FULL' 'PCAL'          ! Latest version of PC2/PC3
TRD  'ON' 'FULL' 'P_ID' 'FULL' 'TCAL'          ! This is the TEC
TOF  'ON' 'FULL' 'P_ID' 'FULL' 'FCAL' 0.0 0.0  ! Time of Flight
EMC  'ON' 'FULL' 'FULL' 'FULL' 'ECAL' 'AUAU' 'CTRK'  ! EMCal, H.I. with Cerenkov
MUM  'OFF' 'FULL' 'ETOT' 'FULL' 'MCAL' 0. 0. 0. 'STCK' 'NNEU'  ! Muon trackers
MUI  'OFF' 'FULL' 'ETOT' 'FULL' 'NCAL' 0. 0. 0. 'STCK' 'NNEU'  ! Muon identifier
