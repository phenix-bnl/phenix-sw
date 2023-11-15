      SUBROUTINE MAT_MIXT_MED

c    Mod. 25-Nov-1992 to include concrete for new muon identifier  Gus Petitt
C    JPS Oct 4-5, 1993 -- added GCFLAG and only print out material parameters
C    when IDEBUG.NE.0 and also direct write statements to LOUT from GCUNIT.
C    JPS Aug 5, 1994 -- Remove definitions of materials used in MVD -- moved to
C                       subroutine VERMATDEF (which is called from VER -- not here)

C    C.F. Maguire   August 11, 1997    Major clean-up of obsolete tracking media
C                                      Add in magnetic copper, low and high threshold

C    C.F. Maguire   March 27, 1998     Change EMCal media to 8xx from 9xx to be
C                                      consistent with volume numbering convention
c
c    Hubert  June 2008 added non-sensitive silicon
c    RCS Oct2010 added SS310 for muon absorbers

      IMPLICIT NONE

#include "gcflag.inc"
#include "gcunit.inc"
#include "guphnx.inc"

      INTEGER NMAT,ISVOL,IFIELD,NWBUF

      REAL FIELDM,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN,
     1  UBUF(10)

C     Material Parameter:
C                 Na      I

c     REAL ABAF(2), ZBAF(2), WBAF(2)
      real ABAF(2) /22.989, 126.9045 /
      real ZBAF(2) / 11.  ,  53.     /
      real WBAF(2) / 1. ,  1. /
      REAL    acsi(2)/132.9054,126.9045/      ! Cs and I A
      REAL    zcsi(2)/55.0,53.0/              ! Cs and I Z
      REAL    wcsi(2)/1.0,1.0/                ! Cs and I weight
      REAL    abaf2(2)/137.34,18.9984/         ! Ba and F A
      REAL    zbaf2(2)/56.0,9.0/               ! Ba and F Z
      REAL    wbaf2(2)/1.0,2.0/                ! Ba and F weight
C  for Intermediate tracker (ITR)
      REAL AMYLAR(3),ZMYLAR(3),WMYLAR(3),
     &  ACHGAS(3),ZCHGAS(3),WCHGAS(3)
C   end ITR
C  for Transition Radiation Detector (TRD)
      REAL STMI, ROEF,FOILTH, GAPTH, FIELMF, IFLMF, EPSI
      REAL AFOAM(2), ZFOAM(2), WFOAM(2),
     *          APOLN(2),ZPOLN(2),WPOLN(2),
     *          ATRDGS(3),ZTRDGS(3),WTRDGS(3)
C   end TRD

C**************************************************************************
C-Toru Sugitate  10 June 1993
C       Quartz (SiO2) Mixture Parameters for the beam-beam detector:

        REAL AQUARTZ(2)/  28.09,  16.00 /
        REAL ZQUARTZ(2)/  14.  ,   8.   /
        REAL WQUARTZ(2)/   1.  ,   2.   /
        REAL DQUARTZ   /   2.64         /
 
C**************************************************************************
C-Mickey Chiu  23 Mar 2010
C       PEEK plastic for beam-pipe stand

        REAL APEEK(3)/12.0107,1.00795,15.9994/  ! PEEK Atomic weights A
        REAL ZPEEK(3)/6.,1.,8./                 ! PEEK Z
        REAL WPEEK(3)/19.,12.,3./               ! PEEK weights
        REAL DPEEK   /1.320/
 
C**************************************************************************

c        Cerenkov definitions (Y.Akiba + K.Shigaki)

c Description:
c     Define mixed materials and tracking media used in CRK detector of PISA
c     medium id and material id from 500 throught 599 are assigned to CRK
c     The following materials and medium are defined

c     Material:
c       ID=501   NAME=C2H6                   ethane
c       ID=502   NAME=CH4                    methane
c       ID=503   NAME=CO2                    CO2
c       ID=504   NAME=FR13                   freon 13
c       ID=511   NAME=RICH MIR. SUBSTRATE    mirror substrate
c     Medium
c       ID=501   NAME=C2H6                   ethane
c       ID=502   NAME=CH4                    methane
c       ID=503   NAME=CO2                    CO2
c       ID=504   NAME=FR13                   freon 13
c       ID=511   NAME=RICH MIR. SUBSTRATE    mirror substrate

c Input:
c     None
c Output:
c     None
c Side effect:
c     Imate = 501, 502, 503, 504, 511
c     Imed  = 501, 502, 503, 504, 511 are defined.

C>>>>DECLARATIONS
c Parameters for material definition
c  Ethan
c     real AC2H6(2),ZC2H6(2),DC2H6,WC2H6(2) ! C2H6
      real dens_ethan
      parameter (dens_ethan=30./22400.)
      real AC2H6(2)/12.011,1.008/
      real ZC2H6(2)/6.0,1.0/
      real WC2H6(2)/2.0,6.0/
      real DC2H6/dens_ethan/
c Methane
c     real ACH4(2), ZCH4(2), DCH4, WCH4(2)  ! CH4
      real dens_methane
      parameter (dens_methane=16./22400.)
      real ACH4(2)/12.011,1.008/
      real ZCH4(2)/6.0,1.0/
      real WCH4(2)/1.0,4.0/
      real DCH4/dens_methane/
c CO2

      real dens_co2
      parameter (dens_co2=44./22400.)
      real ACO2(2)/12.011,15.9994/
      real ZCO2(2)/6.0,8.0/
      real WCO2(2)/1.0,2.0/
      real DCO2/dens_co2/
c Freon 13
c freon 13 is CClF3
      real dens_fr13
      parameter (dens_fr13=104.5/22400.)
      real Afr13(3)/12.011,18.9984,35.4527/
      real Zfr13(3)/6.0,9.0,17.0/
      real Wfr13(3)/1.0,3.0,1.0/
      real Dfr13/dens_fr13/
c mirror substrate; 02.Dec.96 by K.Shigaki
c  atomic weights of H, C, N, and O:
      real amif(4) / 1.008,  12.011,  14.01,   16.00 /
c  mass numbers of H, C, N, and O:
      real zmif(4) / 1.,      6.,      7.,      8. /
c  proportions in terms of weight:
      real wmif(4) / 0.06110, 0.77604, 0.06669, 0.09617 /
c  density
      real dmif / 0.28969 /     ! this is in fact areal density (g/cm^2)
                                ! thickness should be set at 1 cm in crk.f
C**************************************************************************
C TERRY AWES  Lead Glass

C *** DEFINITION OF MATERIALS FOR THE LEAD-GLASS CALORIMETER ***

c     REAL AP, ZP, WP, AEFF, ZEFF, WS, WT, DP, DS, DT
c     DIMENSION AP(3),ZP(3),WP(3),AEFF(2),ZEFF(2),WS(2),WT(2)

C --- Plastic (lucite) for cover plates (C5H8O2) ---
      real AP(3) /12.,1.,16./
      real ZP(3) /6.,1.,8./
      real WP(3) /5.,8.,2./
      real DP /1.18/
C --- The PbO and SiO2 effective A and Z (see GEANT3 manual CONS 110-1)
      real AEFF(2) /193.49,21.65/
      real ZEFF(2) /76.70,10.80/
C --- The Lead-glass for SAPHIR SF5 glass(55% PbO + 45% SiO2) ---
      real WS(2) /0.55,0.45/
      real DS /4.01/
C --- The Lead-glass for the towers TF1 glass (51% PbO + 49% SiO2) ---
      real WT(2) /0.51,0.49/
      real DT /3.85/
C**************************************************************************
c**************************************************************************
c Arne Claussen's lead glass (GD, Dec. 22, 1993)
*-- Author :  Arne Claussen / Gabor David
c*******************************************************************
cgd****CMZ :          22/07/93  16.52.17  by  Arne Claussen
cgd****-- Author :    Arne Claussen   12/12/90

C*******************************************************************
C*          SUPMAT-Routine for SUPERSIM package
C*******************************************************************

C this routine sets up the materials for the Pb-glass detector setup.
C define the TF1-pb glass mixture properties
C     TF1 consists of 51% Pb and 49% SiO2
C        Pb  is A=193.49 Z=76.70
C        Ge  is A= 21.65  Z=10.80
 
      REAL*4 APBG(2),ZPBG(2),WPBG(2),DPBG
 
C define the Mylar material (C5H4O2), ignore the aluminium
 
      REAL*4 AMY(3), ZMY(3), WMY(3), DMY
 
C define the PVC material (C2H3Cl)
 
      REAL*4 APVC(3), ZPVC(3), WPVC(3), DPVC
 
C define the Polyamid material (C2H3Cl)
 
      REAL*4 APAM(4), ZPAM(4), WPAM(4), DPAM
 
C define the Epoxid stuff, the formula is calculated as a mean of the
C components, also the density (C27.2 H29.6 O6.3 N0.2)
 
      REAL*4 AEPH(4), ZEPH(4), WEPH(4), DEPH
 
C define the "Neusilber" material, the weights are given in portions of mass.
C It consists of 9.5% Ni, 2.3% Sn and 88.2% Cu.
 
      REAL*4 ANSI(3), ZNSI(3), WNSI(3), DNSI
 
C define the Silicon (a simple type) as a chemical sum formula.
 
      REAL*4 ASil(3), ZSil(3), WSil(3), DSil

c End of new lead glass deifintions (GD Dec 22, 1993)
c**************************************************************************
C  Concrete for muon identifier added 25-Nov-1992  Gus Petitt

C  Composition of concrete for muon end-cap calorimetry.  TAKE CONCRETE
C   PARAMETERS FROM PARTICLE DATA TABLES.
C   Assume shielding concrete has following composition by mass:
C   Assume density = 2.5 g/cm**3
C     Ca   6.0%
C     Si  32.5%
C       O   52.0%
C       Na   1.5%
C       Fe   4.0%
C       Al   4.0%
C       ---------
C    Total 100.0%

        REAL*4 ACONCRETE(6)/40.08,28.0855,15.999,22.991,55.85,26.98/,
     1         ZCONCRETE(6)/20.,14.,8.,11.,26.,13./,
     1         WCONCRETE(6)/0.06,.325,.52,.015,.04,.04/
      real CONC_DENS /2.5/
C**************************************************************************
c Lead glass DATA
      DATA APBG /193.49, 21.65/
      DATA ZPBG /76.70 , 10.80/
      DATA WPBG /0.51  , 0.49  /
      DATA DPBG /3.85/
      DATA AMY /12., 1., 16./
      DATA ZMY / 6., 1.,  8./
      DATA WMY / 5., 4.,  2./
      DATA DMY / 1.39/
      DATA APVC /12., 1., 35.45/
      DATA ZPVC / 6., 1., 17./
      DATA WPVC / 2., 3.,  1./
      DATA DPVC / 1.05/
      DATA APAM /12., 1., 14., 16./
      DATA ZPAM / 6., 1.,  7.,  8./
      DATA WPAM / 1., 1.,  1.,  1./
      DATA DPAM / 1.09/
      DATA AEPH /12., 1., 16., 14./
      DATA ZEPH / 6., 1.,  8.,  7./
      DATA WEPH /27., 29., 6., 1./
      DATA DEPH / 1.05/
      DATA ANSI /58.71, 118.69, 63.54/
      DATA ZNSI /28.,    50.,   29.  /
      DATA WNSI /0.095,  0.023, 0.882/
      DATA DNSI / 8.9/
      DATA ASil /28.09, 16.00, 1.00/
      DATA ZSil /14.  ,  8.  , 1.  /
      DATA WSil / 2.  ,  1.  , 6.  /
      DATA DSil /1.12/



c     Stephen C. Johnson
c     Defining the materials for the FCAL:

      real aFcl(3) /1, 12, 207.2/
      real zFcl(3) /1, 6, 82/
c     scj Ratio by weights of H,C,and Pb in the FCAL
      real wFcl(3) /.00146, .0175, .98104/
      real densityFcl /9.6/

C---- NCC/FOCAL material -----
      real AG10(5), ZG10(5), WG10(5)
      DATA AG10   /28.09,16.,12.01,1.01,16./,
     &     ZG10   /14.,8.,6.,1.,8./,
     &     WG10   /0.28,0.32,0.218,0.037,0.145/
      real DG10  /1.7/
      
C      REAL focut /0.001/!/0.0001/ ! set to 1MeV for now ! 100 keV tracking cutoff for FoCal 
      REAL focut /0.00001/! 10 keV tracking cutoff for FoCal 
C---- END NCC/FOCAL
C**************************************************************************
c     Material definition of the new muon absorbers - Oct 2010, Ralf Seidl
c
c     New absorber made of SS310

      real ASS310(5)  /55.845,51.996,58.693,24.3050,28.0855/
      real ZSS310(5)  /26.,24.,28.,12.,14./
      real DSS310(5)  /7.87,7.18,8.90,1.74,2.33/
      real WSS310(5)  /0.515,0.25,0.20,0.02,0.015/


C**************************************************************************
c     Material definition of the brass nosecone, CDA836 alloy
c
c     New absorber made of SS310
      real CDA836_A(4)     /63.546,118.710,207.2,65.38/
      real CDA836_Z(4)     /29.,50.,82.,30./
      real CDA836_density  /8.83/
      real CDA836_weights(4) /0.85,0.05,0.05,0.05/

C**************************************************************************

      WRITE ( LOUT,* ) ' ENTER MATMXDEF'

c   Define materials needed for tracking media with different thresholds

      nwbuf = 0
      
      call gsmate(28,'Tungsten_NCC$',183.85,74.0,19.3,
     1              0.35,10.3,ubuf,nwbuf)
      call gsmate(29,'Lead_NCC$',207.19,82.0,11.35,
     1              0.56,18.5,ubuf,nwbuf)
      call gsmate(30,'Copper_NCC$',63.55,29.0,8.96,
     1              1.43,17.1,ubuf,nwbuf)
      call gsmate(31,'Silicon_NCC$',28.09,14.0,2.33,
     1              9.36,45.49,ubuf,nwbuf)
      call gsmate(34,'Aluminum_NCC$',26.98,13.0,2.70,
     1              8.90,37.2,ubuf,nwbuf)
C      call gsmate(37,'Copper for nose cone$',63.55,29.0,8.96,
C     1              1.43,17.1,ubuf,nwbuf)
      call GSMIXT(37,'Brass_nosecone$',CDA836_A,CDA836_Z,
     1       CDA836_density,4,CDA836_weights)
      call gsmate(40,'High Mag Iron$',55.85,26.0,7.87,
     1              1.76,17.1,ubuf,nwbuf)
 19   call gsmate(41,'High Non-mag Iron$',55.85,26.0,7.87,
     1              1.76,17.1,ubuf,nwbuf)
      call gsmate(45,'Low Mag Iron$',55.85,26.0,7.87,
     1              1.76,17.1,ubuf,nwbuf)
      call gsmate(46,'Low Non-mag Iron$',55.85,26.0,7.87,
     1              1.76,17.1,ubuf,nwbuf)
      call gsmate(50,'SILICON$',28.09,14.0,2.33,
     1              9.36,45.49,ubuf,nwbuf)
      CALL GSMIXT(56,'SODIUM IODIDE$', ABAF, ZBAF, 3.670, -2, WBAF)
      call gsmate(57,'SCINTILLATOR$',6.670,3.60,1.180,
     1              0.344E02,0.708E02,ubuf,nwbuf)
      call gsmate(72,'TOF PMT Base$',18.14,9.065,1.7,
     1              19.4,56.72,ubuf,nwbuf)
      call gsmate(73,'MYLAR$',12.88,6.456,1.39,
     1              28.7,56.1,ubuf,nwbuf)
      call gsmate(98,'SCINTILLATOR$',6.670,3.60,1.180,
     1              0.344E02,0.708E02,ubuf,nwbuf)
      call gsmate(99,'Lead_Sub_Conv$',207.19,82.0,11.35,
     1              0.56,18.5,ubuf,nwbuf)

C**************************************************************************
c************************************************************************
C-Toru Sugitate     10 June 1993
c   For the beam-beam detector
C     Define Quartz for the beam-beam detector.

      Call GSMIXT (201,'Quartz-BBQ$',AQUARTZ,ZQUARTZ,DQUARTZ,
     1             -2,WQUARTZ)
      Call GSMIXT (202,'Quartz-PMT$',AQUARTZ,ZQUARTZ,DQUARTZ,
     2             -2,WQUARTZ)

c   Tracking medium #201 -- Quartz for BBQ (sensitive detector)

        NMAT    = 201     ! Quartz
        ISVOL   = 1       ! sensitive for BBQ
        IFIELD  = 1       ! Magnetic field
        FIELDM  = 20.0    ! max field
        TMAXFD  = 45.0    ! maximum angle due to field (one step) in degrees
        DMAXMS  = 0.2     ! max disp. due to mulsct. in one step (cm)
        DEEMAX  = 0.1     ! max fractional energy loss in one step
        EPSIL   = 0.01    ! tracking precision (cm)
        STMIN   = 0.1     ! min step due to e loss or mulsct. (cm)
        UBUF(1) = 0.      ! tracking stop switch
        CaLL GSTMED(201,'Quartz-BBQ$',NMAT,ISVOL,IFIELD,FIELDM,TMAXFD,
     1           DMAXMS,DEEMAX,EPSIL,STMIN,UBUF,NWBUF)
        ISVOL   = 0       ! non-sensitive for PMT
        CaLL GSTMED(202,'Quartz-PMT$',NMAT,ISVOL,IFIELD,FIELDM,TMAXFD,
     1           DMAXMS,DEEMAX,EPSIL,STMIN,UBUF,NWBUF)

c************************************************************************
C**************************************************************************

C        Cerenkov executables

C>>>>BEGIN

C STEP 1...define materials

c ethane
      call GSMIXT( 501,'C2H6$',AC2H6,ZC2H6, DC2H6,-2,WC2H6)
c methane
      call GSMIXT( 502,'CH4$',ACH4,ZCH4, DCH4,-2,WCH4)
c CO2
      call GSMIXT( 503,'CO2$',ACO2,ZCO2, DCO2,-2,WCO2)
c FR13
      call GSMIXT( 504,'FR13$',AFR13,ZFR13, DFR13,-3,WFR13)
c mirror substrate
      call gsmixt( 511,'RICH MIR. SUBSTRATE$',amif,zmif,dmif,4,wmif)

C STEP 2...define tracking mediums

c define common parameters
      ISVOL  = 0                !Not a sensitive volume
      IFIELD = 1                !Magnet is ON. GRKUTA will be used
      FIELDM = 0.5              !Max Mag. Field is 0.5 KG in RICH region
      TMAXFD = 5.0              !Max bend angle due to magnetic filed
      DMAXMS = 0.1              !Max Mult. scat. is 1 mm
      DEEMAX = 0.1              !Max energy loss is 10%
      EPSIL  = 0.2              !Tracking precison is 2mm
      STMIN  = 1.00             !Minimum step size is 1cm
c ethane
      nmat = 501               !Material is Ethane
      CALL GSTMED (501,'C2H6$', nmat, ISVOL, IFIELD, FIELDM, TMAXFD,
     +      DMAXMS,DEEMAX, EPSIL, STMIN, UBUF, 3)
c methane
      nmat = 502               !Material is Methane
      CALL GSTMED (502,'CH4$', nmat, ISVOL,  IFIELD, FIELDM, TMAXFD,
     +      DMAXMS,DEEMAX, EPSIL, STMIN, UBUF, 3)
c CO2
      nmat = 503               !Material is Methane
      CALL GSTMED (503,'CO2$', nmat, ISVOL,  IFIELD, FIELDM, TMAXFD,
     +      DMAXMS,DEEMAX, EPSIL, STMIN, UBUF, 3)
c Freon 13
      nmat = 504               !Material is Methane
      CALL GSTMED (504,'FR13$', nmat, ISVOL,  IFIELD, FIELDM, TMAXFD,
     +      DMAXMS,DEEMAX, EPSIL, STMIN, UBUF, 3)
c mirror substrate
      nmat   = 511
      isvol  = 0                ! not sensitive
      ifield = 1                ! magnetic field
      fieldm = 0.5              ! max field
      tmaxfd = 5.0              ! maximum angle due to field (one step) in degrees
      dmaxms = 0.1              ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1              ! max fractional energy loss in one step
      epsil  = 0.02             ! tracking precision (cm)
      stmin  = 0.1              ! min step due to e loss or mulsct. (cm)
      call gstmed(511,'RICH MIR. SUBSTRATE$',nmat,isvol,ifield,fieldm,
     &     tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

C        End of Cerenkov executables
c====================================================================

c   Replace liquid nitrogen and helium in standard material table
c    with gas

      call gsmate(3,'HELIUM (GAS)$',4.0,2.0,0.0001785,
     1               0.5299e06,3.355E05,ubuf,nwbuf)
      call gsmate(7,'NITROGEN (GAS)$',14.01,7.0,0.00125,
     1               0.30392e05,6.789e04,ubuf,nwbuf)

 1000 CONTINUE


c   Define tracking media

c store additional tracking options (as seen by GUSTEP) in user array ubuf:

c     ubuf(1): stop switch   0: continue tracking
c                  1: stop particle

      nwbuf=1

c   Tracking media # 1 - Helium
      nmat = 3 ! helium
      isvol = 0   ! not sensitive
      ifield = 1  ! magnetic field
      fieldm = 5.0  ! max field
      tmaxfd = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2    ! max disp. due to mulsct. in one step (cm)
      deemax =  0.1   ! max fractional energy loss in one step
      epsil =   0.01  ! tracking precision (cm)
      stmin =   0.1   ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.    ! tracking stop switch
      call gstmed(1,'HELIUM$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c   Tracking media # 2 - scint
      nmat = 57   ! scintillator
      isvol = 1   ! sensitive
      ifield = 0  ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 0.50  ! max disp. due to mulsct. in one step (cm)
      deemax = 0.05  ! max fractional energy loss in one step
      epsil = 0.01   ! tracking precision (cm)
      stmin = 0.1 ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(2,'SCINT $',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
      call gstmed(98,'SCINT low$',98,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c     Special Thresholds for Scintillator

      call gstpar(98,'CUTELE',0.0001)      ! electron cut at 100 keV
      call gstpar(98,'CUTGAM',0.0001)      ! gamma cut at 100 keV
      call gstpar(98,'BCUTE',0.0001)       ! e brem cut at 100 keV
      call gstpar(98,'CUTMUO',0.01)        ! muon cut at 10 MeV
      call gstpar(98,'BCUTM',0.01)         ! muon brem cut at 10 MeV
      call gstpar(98,'DCUTE',1.e+4)        ! e delta ray cut (Landau ON)
      call gstpar(98,'DCUTM',1.e+4)        ! muon delta ray cut (Landau ON)

c   Tracking media # 5 - Magnet coil (Iron)
      nmat = 10   ! iron
      isvol = 0   ! not sensitive
      ifield = 1  ! magnetic field
      fieldm = 5.0  ! max field
      tmaxfd = 0.3    ! maximum angle due to field (one step) in degrees
      dmaxms = 0.5    ! max disp. due to mulsct. in one step (cm)
      deemax =  0.2   ! max fractional energy loss in one step
      epsil =   0.1   ! tracking precision (cm)
      stmin =   0.1   ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(5,'YOKE $',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c   Tracking media # 6 - Air
      nmat = 15   ! Air
      isvol = 0   ! not sensitive
      ifield = 0  ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 2.0   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.2   ! max fractional energy loss in one step
      epsil = .1  ! tracking precision (cm)
      stmin = 2.0 ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(6,'AIR $',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
 

c   Tracking media # 9 - Lead
      nmat = 13   ! lead
      isvol = 0   ! not sensitive sensitive
      ifield = 0  ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 2.0   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.2   ! max fractional energy loss in one step
      epsil = .1  ! tracking precision (cm)
      stmin = 0.5 ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(9,'PB low$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c     Special Thresholds for Lead number 9

      call gstpar(9,'CUTELE',0.0001)      ! electron cut at 100 keV
      call gstpar(9,'CUTGAM',0.0001)      ! gamma cut at 100 keV
      call gstpar(9,'BCUTE',0.0001)       ! e brem cut at 100 keV
      call gstpar(9,'CUTMUO',0.01)        ! muon cut at 10 MeV
      call gstpar(9,'BCUTM',0.01)         ! muon brem cut at 10 MeV
      call gstpar(9,'DCUTE',1.e+4)        ! e delta ray cut (Landau ON)
      call gstpar(9,'DCUTM',1.e+4)        ! muon delta ray cut (Landau ON)

c   Tracking media # 99 - Lead subconverter with high thresholds
      nmat = 99   ! Average lead
      isvol = 0   ! not sensitive sensitive
      ifield = 0  ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 2.0   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.2   ! max fractional energy loss in one step
      epsil = .1  ! tracking precision (cm)
      stmin = 0.5 ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(99,'PB high$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c     Special Thresholds for Lead number 99

      call gstpar(99,'CUTELE',0.001)      ! electron cut at 1 MeV
      call gstpar(99,'CUTGAM',0.001)      ! gamma cut at 1 MeV
      call gstpar(99,'BCUTE',0.001)       ! e brem cut at 1 MeV
      call gstpar(99,'CUTMUO',0.01)        ! muon cut at 10 MeV
      call gstpar(99,'BCUTM',0.01)         ! muon brem cut at 10 MeV
      call gstpar(99,'DCUTE',1.e+4)        ! e delta ray cut (Landau ON)
      call gstpar(99,'DCUTM',1.e+4)        ! muon delta ray cut (Landau ON)

c   Tracking media # 10 - silicon
      nmat = 50   ! really silicon !
      isvol = 1   ! sensitive
      ifield = 1  ! magnetic field  ! There will be a field (CFM)
      fieldm = 5.0  ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil = .001   ! tracking precision (cm)
      stmin = 0.01 ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(10,'Silicon$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c   Tracking media # 11 - passive silicon for (i)(f)vtx
      nmat = 50   ! really silicon !
      isvol = 0   ! *** non-sensitive ***
      ifield = 1  ! magnetic field  ! There will be a field (CFM)
      fieldm = 5.0  ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil = .001   ! tracking precision (cm)
      stmin = 0.5 ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(11,'Silicon passive$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c   Tracking media # 15 - Cu for Coils
      nmat = 11   ! copper
      isvol = 0   ! not sensitive
      ifield = 1  !  magnetic field
      fieldm = 5.0   ! max field
      tmaxfd = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2    ! max disp. due to mulsct. in one step (cm)
      deemax =  0.1   ! max fractional energy loss in one step
      epsil =   0.1   ! tracking precision (cm)
      stmin =   0.1   ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(15,'CU$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c   Tracking media # 16 - vacuum (for inside the pipe)
      nmat = 16   ! GEANT vacuum
      isvol = 0   ! not sensitive
      ifield = 0  ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 30.0  ! max disp. due to mulsct. in one step (cm)
      deemax = 1.0   ! max fractional energy loss in one step
      epsil = 1.  ! tracking precision (cm)
      stmin = 20. ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(16,'VACUUM$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c   Tracking media # 17 - Cu for nosecone
      nmat = 37   ! copper
      isvol = 0   ! not sensitive
      ifield = 1  ! magnetic field
      fieldm = 5.0   ! max field
      tmaxfd = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2    ! max disp. due to mulsct. in one step (cm)
      deemax =  0.1   ! max fractional energy loss in one step
      epsil =   0.1   ! tracking precision (cm)
      stmin =   0.1   ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(17,'CuNose$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c     Special Thresholds for Copper Medium 17 for nosecone

      call gstpar(17,'CUTELE',0.050)      ! electron cut at 50 MeV
      call gstpar(17,'CUTGAM',0.050)      ! gamma cut at 50 MeV
      call gstpar(17,'BCUTE',0.050)       ! e brem cut at 50 MeV
      call gstpar(17,'CUTMUO',0.05)        ! muon cut at 50 MeV
      call gstpar(17,'BCUTM',0.05)         ! muon brem cut at 50 MeV
      call gstpar(17,'DCUTE',1.e+4)       ! e delta ray cut (Landau ON)
      call gstpar(17,'DCUTM',1.e+4)       ! muon delta ray cut (Landau ON)
      call gstpar(17,'CUTHAD',0.050)      ! charged hadron cut at 50 MeV
      call gstpar(17,'CUTNEU',0.050)      ! neutral hadron cut at 50 MeV 


c   Tracking media # 18 - Air + field (low field in PC3 and beyond)
      nmat = 15   ! Air
      isvol = 0   ! not sensitive
      ifield = 1  ! magnetic field
      fieldm = 0.1  ! max field
      tmaxfd = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2    ! max disp. due to mulsct. in one step (cm)
      deemax =  0.1   ! max fractional energy loss in one step
      epsil =   0.1   ! tracking precision (cm)
      stmin =   0.1   ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(18,'AIRLOWF$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)


c   Tracking media # 19 - Air + field (high field)
      nmat = 15   ! Air
      isvol = 0   ! not sensitive
      ifield = 1  ! magnetic field
      fieldm = 5.0  ! max field
      tmaxfd = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2    ! max disp. due to mulsct. in one step (cm)
      deemax =  0.1   ! max fractional energy loss in one step
      epsil =   0.01  ! tracking precision (cm)
      stmin =   0.1   ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(19,'AIRF$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)


c   Tracking media # 24 - PMT Base in TOF
      nmat = 72   ! G10- plate
      isvol = 0   ! insensitive
      ifield = 1  ! magnetic field
      fieldm = 0.1  ! max field
      tmaxfd = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.5   ! max fractional energy loss in one step
      epsil = 0.1 ! tracking precision (cm)
      stmin = 0.2 ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(24,'MPC FRAME $',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)


c   Tracking media # 25 - mpc mylar planes (used in RICH)
      nmat = 73   ! mylar
      isvol = 0   ! insensitive
      ifield = 2  ! no magnetic field
      fieldm = 1.0  ! max field
      tmaxfd = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.5   ! max fractional energy loss in one step
      epsil = 0.1  ! tracking precision (cm)
      stmin = 0.1  ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(25,'MPC MYLR PLN $',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c   Tracking media # 26 - Aluminum for frames, etc.  CFM: October, 1994
      nmat = 9 ! aluminium
      isvol = 0   ! insensitive
      ifield = 1  ! magnetic field
      fieldm = 5.0  ! max field
      tmaxfd = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.5   ! max fractional energy loss in one step
      epsil = 0.01 ! tracking precision (cm)
      stmin = 0.01 ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(26,'Aluminum Frame$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
       
c *** NCC/FoCal materials ***
      IFIELD =  1    ! magnetic field
      FIELDM =  5.0  ! max field
      TMAXFD = 45.0  ! maximum angle due to field (one step) in degrees
      DMAXMS =  0.2  ! max disp. due to mulsct. in one step (cm)
      DEEMAX =  0.1  ! max fractional energy loss in one step
      EPSIL  =  0.01 ! tracking precision (cm)
      STMIN  =  0.01 ! min step due to e loss or mulsct. (cm)

c   Tracking media # 27 - G10 for NCC, larger magnetic field
      CALL GSMIXT(27,'G10_NCC$',AG10,ZG10,DG10,5,WG10)
      nmat = 27   ! G10- plate
      if(ivolu_opt(1,26).ne.0) then 
        isvol = 1   ! sensitive
      else
        isvol = 0
      end if
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(27,'NCC G10$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c   Modified cutoffs for FoCal
      call gstpar(27,'CUTELE',focut)      ! electron cut at 100 keV
      call gstpar(27,'CUTGAM',focut)      ! gamma cut at 100 keV
      call gstpar(27,'BCUTE',focut)       ! e brem cut at 100 keV

c   Tracking media # 28 - Tungsten for NCC
c      nmat = 12   ! Standard tungsten in GEANT
      nmat = 28   ! NCC tungsten
      if((ivolu_opt(1,26).ne.0).or.(ivolu_opt(1,28).ne.0)) then 
        isvol = 1   ! sensitive
      else
        isvol = 0
      end if; 
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(28,'NCC Tungsten$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c   Modified cutoffs for FoCal
      call gstpar(28,'CUTELE',focut)      ! electron cut at 120 keV
      call gstpar(28,'CUTGAM',focut)      ! gamma cut at 120 keV
      call gstpar(28,'BCUTE',focut)       ! e brem cut at 120 keV

c   Tracking media # 29 - Lead for NCC - not used in FoCal
      nmat = 29   ! NCC lead in GEANT
      isvol = 0   ! insensitive
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(29,'NCC Lead$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c   Tracking media # 30 - Copper for NCC
      nmat = 30   ! NCC copper
      isvol = 0   ! insensitive
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(30,'NCC Cu$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c   Modified cutoffs for FoCal
      call gstpar(30,'CUTELE',focut)      ! electron cut at 120 keV
      call gstpar(30,'CUTGAM',focut)      ! gamma cut at 120 keV
      call gstpar(30,'BCUTE',focut)       ! e brem cut at 120 keV

c   Tracking media # 31 - FoCal silicon
      nmat = 31   ! NCC silicon 
      isvol = 1   ! sensitive
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(31,'NCC Si$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c   Modified cutoffs for FoCal
c      call gstpar(31,'CUTELE',focut)      ! electron cut at 120 keV
c      call gstpar(31,'CUTGAM',focut)      ! gamma cut at 120 keV
c      call gstpar(31,'BCUTE',focut)       ! e brem cut at 120 keV

c VERY LOW tracking cuts in the Si to avoid unrealistic bumps in the 
c MPC-EX energy spectra
      call gstpar(31,'CUTELE',0.00001)      ! electron cut at 10 keV
      call gstpar(31,'CUTGAM',0.00001)      ! gamma cut at 10 keV
      call gstpar(31,'BCUTE', 0.00001)      ! e brem cut at 10 keV

c   Tracking media # 34 - FoCal Al
      nmat = 34   ! NCC Al
      if(ivolu_opt(1,26).ne.0) then 
        isvol = 1   ! sensitive
      else
        isvol = 0
      end if
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(34,'NCC Al$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c   Modified cutoffs for FoCal
      call gstpar(34,'CUTELE',focut)      ! electron cut at 120 keV
      call gstpar(34,'CUTGAM',focut)      ! gamma cut at 120 keV
      call gstpar(34,'BCUTE',focut)       ! e brem cut at 120 keV


c   Tracking media # 119 - Air + field (high field)
      nmat = 15   ! Air
      if(ivolu_opt(1,26).ne.0) then 
        isvol = 1   ! sensitive
      else
        isvol = 0
      end if
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(119,'MPCEX AIRF$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c   Modified cutoffs for FoCal
      call gstpar(119,'CUTELE',focut)      ! electron cut at 120 keV
      call gstpar(119,'CUTGAM',focut)      ! gamma cut at 120 keV
      call gstpar(119,'BCUTE',focut)       ! e brem cut at 120 keV


c   FoCal end
c====================================================================


c   Tracking media # 32 - Vacuum + magnetic field
      nmat = 16   ! vacuum
      isvol = 0   ! not sensitive
      ifield = 1  ! magnetic field
      fieldm = 5.0  ! max field
      tmaxfd = 5.0    ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2    ! max disp. due to mulsct. in one step (cm)
      deemax =  0.2   ! max fractional energy loss in one step
      epsil =   0.01  ! tracking precision (cm)
      stmin =   5.00  ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(32,'Vac + Field$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)


c   Tracking media # 33 - Beryllium + magnetic field
      nmat = 5 ! Beryllium
      isvol = 0   ! not sensitive
      ifield = 1  ! magnetic field
      fieldm = 5.0  ! max field
      tmaxfd = 0.3    ! maximum angle due to field (one step) in degrees
      dmaxms = 0.5    ! max disp. due to mulsct. in one step (cm)
      deemax =  0.2   ! max fractional energy loss in one step
      epsil =   0.01  ! tracking precision (cm)
      stmin =   0.01  ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(33,'Be + Field$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)


c   Tracking media # 35 - Aluminum + magnetic field (for new beampipe)
      nmat = 9 ! Aluminum
      isvol = 0   ! not sensitive
      ifield = 1  ! magnetic field
      fieldm = 5.0  ! max field
      tmaxfd = 0.3    ! maximum angle due to field (one step) in degrees
      dmaxms = 0.5    ! max disp. due to mulsct. in one step (cm)
      deemax =  0.2   ! max fractional energy loss in one step
      epsil =   0.01  ! tracking precision (cm)
      stmin =   0.01  ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(35,'Al + Field$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)



c   Tracking media # 4 - Lithium + magnetic field
      nmat = 4 ! Lithium (RL = 155 cm)
      isvol = 0   ! not sensitive
      ifield = 1  ! magnetic field
      fieldm = 5.0  ! max field
      tmaxfd = 0.3    ! maximum angle due to field (one step) in degrees
      dmaxms = 0.5    ! max disp. due to mulsct. in one step (cm)
      deemax =  0.2   ! max fractional energy loss in one step
      epsil =   0.01  ! tracking precision (cm)
      stmin =   0.01  ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(4,'Li + Field$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)


c   Tracking media # 40 - Mu Magnet Yoke Magnetic (Iron)
      nmat = 40   ! iron (high threshold)
      isvol = 0   ! not sensitive
      ifield = 1  ! magnetic field
      fieldm = 5.0  ! max field
      tmaxfd = 0.3    ! maximum angle due to field (one step) in degrees
      dmaxms = 2.0   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.2   ! max fractional energy loss in one step
      epsil = .1     ! tracking precision (cm)
      stmin = .1     ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(40,'MU mag-hi$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c     Special Thresholds for MU Medium 40

      call gstpar(40,'CUTELE',0.050)      ! electron cut at 50 MeV
      call gstpar(40,'CUTGAM',0.050)      ! gamma cut at 50 MeV
      call gstpar(40,'BCUTE',0.050)       ! e brem cut at 50 MeV
      call gstpar(40,'CUTMUO',0.01)        ! muon cut at 10 MeV
      call gstpar(40,'BCUTM',0.01)         ! muon brem cut at 10 MeV
      call gstpar(40,'DCUTE',1.e+4)        ! e delta ray cut (Landau ON)
      call gstpar(40,'DCUTM',1.e+4)        ! muon delta ray cut (Landau ON)
      call gstpar(40,'CUTHAD',0.050)      ! charged hadron cut at 50 MeV
      call gstpar(40,'CUTNEU',0.050)      ! neutral hadron cut at 50 MeV

c   Tracking media # 41 - Mu Magnet Yoke (Iron)
      nmat = 41   ! iron (high threshold)
      isvol = 0   ! not sensitive
      ifield = 0  ! no magnetic field
      fieldm = 0.0  ! max field
      tmaxfd = 0.3    ! maximum angle due to field (one step) in degrees
      dmaxms = 2.0   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.2   ! max fractional energy loss in one step
      epsil = .1  ! tracking precision (cm)
      stmin = .1 ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(41,'MU non-mag-hi$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c     Special Thresholds for MU Medium 41

      call gstpar(41,'CUTELE',0.050)      ! electron cut at 50 MeV
      call gstpar(41,'CUTGAM',0.050)      ! gamma cut at 50 MeV
      call gstpar(41,'BCUTE',0.050)       ! e brem cut at 50 MeV
      call gstpar(41,'CUTMUO',0.01)        ! muon cut at 10 MeV
      call gstpar(41,'BCUTM',0.01)         ! muon brem cut at 10 MeV
      call gstpar(41,'DCUTE',1.e+4)        ! e delta ray cut (Landau ON)
      call gstpar(41,'DCUTM',1.e+4)        ! muon delta ray cut (Landau ON)
      call gstpar(41,'CUTHAD',0.050)      ! charged hadron cut at 50 MeV
      call gstpar(41,'CUTNEU',0.050)      ! neutral hadron cut at 50 MeV

c   Tracking media # 45 - Mu Magnet Yoke Magnetic (Iron)
      nmat = 45   ! iron (low threshold)
      isvol = 0   ! not sensitive
      ifield = 1  ! magnetic field
      fieldm = 5.0  ! max field
      tmaxfd = 0.3    ! maximum angle due to field (one step) in degrees
      dmaxms = 2.0   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.2   ! max fractional energy loss in one step
      epsil = .1     ! tracking precision (cm)
      stmin = .1     ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(45,'MU mag-lo$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c     Special Thresholds for MU Medium 45

      call gstpar(45,'CUTELE',0.001)      ! electron cut at 1 MeV
      call gstpar(45,'CUTGAM',0.001)      ! gamma cut at 1 MeV
      call gstpar(45,'BCUTE',0.001)       ! e brem cut at 1 MeV
      call gstpar(45,'CUTMUO',0.01)        ! muon cut at 10 MeV
      call gstpar(45,'BCUTM',0.01)         ! muon brem cut at 10 MeV
      call gstpar(45,'DCUTE',1.e+4)        ! e delta ray cut (Landau ON)
      call gstpar(45,'DCUTM',1.e+4)        ! muon delta ray cut (Landau ON)
      call gstpar(45,'CUTHAD',0.01)      ! charged hadron cut at 10 MeV
      call gstpar(45,'CUTNEU',0.01)      ! neutral hadron cut at 10 MeV

c   Tracking media # 46 - Mu Magnet Yoke (Iron)
      nmat = 46   ! iron (low threshold)
      isvol = 0   ! not sensitive
      ifield = 0  ! no magnetic field
      fieldm = 0.0  ! max field
      tmaxfd = 0.3    ! maximum angle due to field (one step) in degrees
      dmaxms = 2.0   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.2   ! max fractional energy loss in one step
      epsil = .1  ! tracking precision (cm)
      stmin = .1 ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(46,'MU non-mag-low$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c     Special Thresholds for MU Medium 46

      call gstpar(46,'CUTELE',0.001)      ! electron cut at 1 MeV
      call gstpar(46,'CUTGAM',0.001)      ! gamma cut at 1 MeV
      call gstpar(46,'BCUTE',0.001)       ! e brem cut at 1 MeV
      call gstpar(46,'CUTMUO',0.01)        ! muon cut at 10 MeV
      call gstpar(46,'BCUTM',0.01)         ! muon brem cut at 10 MeV
      call gstpar(46,'DCUTE',1.e+4)        ! e delta ray cut (Landau ON)
      call gstpar(46,'DCUTM',1.e+4)        ! muon delta ray cut (Landau ON)
      call gstpar(46,'CUTHAD',0.010)      ! charged hadron cut at 10 MeV
      call gstpar(46,'CUTNEU',0.010)      ! neutral hadron cut at 10 MeV

c************************************************************************

c       First define new mixtures (for CsI and BaF2)

        CALL GSMIXT(861,'CSI$',acsi,zcsi,4.51,-2,wcsi)

        nmat = 861              ! CsI
        isvol = 1               ! Sensitive
        ifield = 0              ! no magnetic field (hopefully...)
        fieldm = 0.0            ! max field
        tmaxfd = 45.0           ! max angle due to field
        dmaxms = 0.5            ! max disp. due to multiple scatt. (cm) / step
        deemax = 0.05           ! max fractional energy loss in a step
        epsil = 0.01            ! tracking precision
        stmin = 0.2             ! min step in eloss or muls
        CALL GSTMED(830,'CSI $',nmat,isvol,ifield,fieldm,tmaxfd,
     1               dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

        CALL GSMIXT(862,'BAF2$',abaf2,zbaf2,4.51,-2,wbaf2)

        nmat = 862              ! BaF2
        isvol = 1               ! Sensitive
        ifield = 0              ! no magnetic field (hopefully...)
        fieldm = 0.0            ! max field
        tmaxfd = 45.0           ! max angle due to field
        dmaxms = 0.5            ! max disp. due to multiple scatt. (cm) / step
        deemax = 0.05           ! max fractional energy loss in a step
        epsil = 0.01            ! tracking precision
        stmin = 0.2             ! min step in eloss or muls
        CALL GSTMED(831,'BAF2 $',nmat,isvol,ifield,fieldm,tmaxfd,
     1               dmaxms,deemax,epsil,stmin,ubuf,nwbuf)




C**************************************************************************
c
c     Ralf Seidl new SS310 Muon absorber materials 
      WRITE ( *,* ) 'before  942'
      ubuf(1) = 0.
      call gsmixt(942, 'SS310', ASS310, ZSS310, DSS310, 5, WSS310)
      call gstmed(942, 'SS310Medium$',942,
     &     0, ! sensitive
     &     0, ! no magnetic field
     &     0, ! maximum filed
     &     0, ! maximum angle due to field
c     &     0.05, ! max displacement in one step
c     &     0.05, ! max fractional energy loss in one step
c     &     0.001, ! tracking percision
     &     0.10, ! max displacement in one step
     &     0.10, ! max fractional energy loss in one step
     &     0.001, ! tracking percision
     &     .02, ! min step due to e loss
     &     ubuf,
     &     0)

      WRITE ( *,* ) ' 942 defined'

C *** DEFINITION OF MATERIALS FOR THE LEAD-GLASS CALORIMETER ***
C    Terry AWES

C --- Define the various materials for GEANT ---
      CALL GSMATE(800,'Fe    $', 55.85,26., 7.87,1.76,17.1,UBUF,0)
      CALL GSMATE(801,'Al    $', 26.98,13., 2.70,8.90,37.2,UBUF,0)
C --- Define the various mixtures for GEANT ---
      CALL GSMIXT(802,'Plastic    $',AP,ZP,DP,-3,WP)
      CALL GSMIXT(803,'S Pb-glass $',AEFF,ZEFF,DS,2,WS)
      CALL GSMIXT(804,'T Pb-glass $',AEFF,ZEFF,DT,2,WT)

C *** DEFINITION OF TRACKING MEDIA FOR THE LEAD-GLASS CALORIMETER ---

      CALL GSTMED(800,'Fe plate$',800,0,0,0.,10.,1.,0.1,0.01,
     +	   0.02,UBUF,0)
      CALL GSTMED(801,'Al plate$',801,0,0,0.,10.,1.,0.1,0.01,
     +	   0.05,UBUF,0)
      CALL GSTMED(802,'Plastic $',802,0,0,0.,10.,1.,0.1,0.01,
     +	   0.05,UBUF,0)
      CALL GSTMED(803,'S block $',803,1,0,0.,10.,1.,0.1,0.01,
     +	   0.30,UBUF,0)
      CALL GSTMED(804,'T block $',804,1,0,0.,10.,1.,0.1,0.01,
     +	   0.30,UBUF,0)

C --- Energy cut-offs in the Lead glass to gain time in tracking ---
C --- without affecting the hit patterns ---
      CALL GSTPAR(803,'DCAY',1.)
      CALL GSTPAR(803,'PAIR',1.)
      CALL GSTPAR(803,'COMP',1.)
      CALL GSTPAR(803,'PHOT',1.)
      CALL GSTPAR(803,'PFIS',1.)
      CALL GSTPAR(803,'DRAY',0.)
      CALL GSTPAR(803,'ANNI',1.)
      CALL GSTPAR(803,'BREM',1.)
      CALL GSTPAR(803,'MUNU',0.)
      CALL GSTPAR(803,'HADR',1.)
      CALL GSTPAR(803,'LOSS',2.)
      CALL GSTPAR(803,'MULS',1.)
      CALL GSTPAR(803,'RAYL',0.)
      CALL GSTPAR(803,'CUTGAM',1.E-3)
      CALL GSTPAR(803,'CUTELE',1.E-3)
      CALL GSTPAR(803,'CUTNEU',1.E-3)
      CALL GSTPAR(803,'CUTHAD',1.E-3)
      CALL GSTPAR(803,'CUTMUO',1.E-3)

      CALL GSTPAR(804,'DCAY',1.)
      CALL GSTPAR(804,'PAIR',1.)
      CALL GSTPAR(804,'COMP',1.)
      CALL GSTPAR(804,'PHOT',1.)
      CALL GSTPAR(804,'PFIS',1.)
      CALL GSTPAR(804,'DRAY',0.)
      CALL GSTPAR(804,'ANNI',1.)
      CALL GSTPAR(804,'BREM',1.)
      CALL GSTPAR(804,'MUNU',0.)
      CALL GSTPAR(804,'HADR',1.)
      CALL GSTPAR(804,'LOSS',2.)
      CALL GSTPAR(804,'MULS',1.)
      CALL GSTPAR(804,'RAYL',0.)
      CALL GSTPAR(804,'CUTGAM',1.E-3)
      CALL GSTPAR(804,'CUTELE',1.E-3)
      CALL GSTPAR(804,'CUTNEU',1.E-3)
      CALL GSTPAR(804,'CUTHAD',1.E-3)
      CALL GSTPAR(804,'CUTMUO',1.E-3)
cgd**********************************************************************
c Include Arne Claussen's lead glass definitions (GD Dec 22, 1993)
C Call the GEANT-routine to create the TF1
 
      CALL GSMIXT(810,'TF1$',APBG,ZPBG,DPBG,2,WPBG)
 
C Call the GEANT-routine to create the Mylar
 
      CALL GSMIXT(820,'MYLAR$',AMY,ZMY,DMY,-3,WMY)
 
C Call the GEANT-routine to create the PVC
 
      CALL GSMIXT(821,'PVC$',APVC,ZPVC,DPVC,-3,WPVC)
 
C Call the GEANT-routine to create the Polyamid
 
      CALL GSMIXT(822,'PAM$',APAM,ZPAM,DPAM,-4,WPAM)

 
C Call the GEANT-routine to create the Epoxid stuff
 
      CALL GSMIXT(824,'EPH$',AEPH,ZEPH,DEPH,-4,WEPH)
 
C Call the GEANT-routine to create the "Neusilber"
 
      CALL GSMIXT(825,'NSI$',ANSI,ZNSI,DNSI,3,WNSI)
 
C Call the GEANT-routine to create the Silicon
 
      CALL GSMIXT(826,'SIL$',ASIL,ZSIL,DSIL,-3,WSIL)
 
C Create the PEEK for the new beam-pipe stand (m.chiu, 23-Mar-2010)
 
      CALL GSMIXT(60,'PEEK$',APEEK,ZPEEK,DPEEK,-3,WPEEK)

C define the tracking media
 
      CALL GSTMED(810,'TF1$',810,1,0,0.,10.,0.01,0.1,0.01,
     +	   5.E-4,UBUF,0)

cMM   use the original definitions
cMM      CALL GSTMED(820,'MYLAR$',820,0,0,0.,1.,0.1,0.1,1.E-4,5.E-4,0,0)
      CALL GSTMED(820,'MYLAR$',820,1,0,0.,1.,0.001,0.1,1.E-4,
     +	   5.E-4,UBUF,0)
cMM      CALL GSTMED(821,'PVC$',821,0,0,0.,1.,0.1,0.1,1.E-4,5.E-4,0,0)
      CALL GSTMED(821,'PVC$',821,0,0,0.,1.,0.01,0.1,1.E-4,
     +	   5.E-4,UBUF,0)
cMM

      CALL GSTMED(822,'PAM$',822,0,0,0.,1.,0.1,0.1,1.E-4,
     +	   5.E-4,UBUF,0)
      CALL GSTMED(824,'EPH$',824,0,0,0.,1.,0.1,0.1,1.E-4,
     +	   5.E-4,UBUF,0)
      CALL GSTMED(825,'NSI$',825,0,0,0.,1.,0.1,0.1,1.E-4,
     +	   5.E-4,UBUF,0)
      CALL GSTMED(826,'SIL$',826,0,0,0.,1.,0.1,0.1,1.E-4,
     +	   5.E-4,UBUF,0)

c  End of lead glass
cgd**********************************************************************

C     Soren P. Sorensen insertions for the MUM (muon magnet trackers)

csps
        call gsmate( 70, 'DRIFT CHAMBER MATERIAL$', 30.68, 14.00,
     1          7.99E-3, 2.965E3, 1.259E4, ubuf, nwbuf )
csps
      nmat      = 70    ! muon tracking station drift chamber material
      isvol     = 1     ! sensitive
      ifield    = 1     ! magnetic field
      fieldm    = 20.0  ! max field
      tmaxfd    = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms    = 0.05  ! max disp. due to mulsct. in one step (cm)
      deemax    = 0.05  ! max fractional energy loss in one step
      epsil     = 0.001 ! tracking precision (cm)
      stmin     = 0.1   ! min step due to e loss or mulsct. (cm)
      ubuf(1)   = 0.    ! tracking stop switch
      call gstmed( 70,'drift chamber material$',nmat,isvol,ifield,
     +           fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)



c     Stephen C. Johnson
c     Defining the materials for the FCAL:

      ubuf(1) = 0
      call gsmixt(870, 'FCLPbSci$', aFcl, zFcl, densityFcl, 3, wFcl)
      call gstmed(871, 'FCLMedium$', 870,
     &     1, ! sensitive
     &     0, ! no magnetic field
     &     0, ! maximum filed
     &     0, ! maximum angle due to field
c     &     0.05, ! max displacement in one step
c     &     0.05, ! max fractional energy loss in one step
c     &     0.001, ! tracking percision
     &     0.10, ! max displacement in one step
     &     0.10, ! max fractional energy loss in one step
     &     0.01, ! tracking percision
     &     .1, ! min step due to e loss
     &     ubuf,
     &     0)

      call gstmed(36, 'PISMEDIUM$', 60,
     &     1, ! sensitive
     &     0, ! no magnetic field
     &     0, ! maximum filed
     &     0, ! maximum angle due to field
c     &     0.05, ! max displacement in one step
c     &     0.05, ! max fractional energy loss in one step
c     &     0.001, ! tracking percision
     &     0.10, ! max displacement in one step
     &     0.10, ! max fractional energy loss in one step
     &     0.01, ! tracking percision
     &     .1, ! min step due to e loss
     &     ubuf,
     &     0)


      write ( lout,* ) ' MATMXMED <I>: exit'
      RETURN
      END
