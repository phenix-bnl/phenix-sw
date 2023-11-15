c
      subroutine emc_trk_media
      implicit none
c
c     GEANT tracking media definitions for EMCal exclusive use
c
c     Original author: Charles F. Maguire  (Template draft for Gabor David)
c     Creation date: September 25, 1997
c
c     Revised History
c         Gabor David    October 17, 1997   True first draft of routine
c
c     Method: Standard GEANT calls formerly in  MAT_MIXT_MED routine
c
c     Calling Map: Called by EMC
c                  Calls GSMATE, GSTMED, GSTPAR
c
c     Revision History
c
c
c     Local variables
c
      integer nmat    ! tracking material number
      integer isvol   ! sensitive or non-sensitive tracking medium
      integer ifield  ! magnetic field on or off (1 or 0) in medium
      real    fieldm  ! maximum magnetic field in medium
      real    tmaxfd  ! maximum deflection angle due to magnetic field
      real    dmaxms  ! maximum displacement due to mult scatt in one step (cm)
      real    deemax  ! maximum fractional energy loss in one step
      real    epsil   ! tracking precision (cm)
      real    stmin   ! minimum step due to elect loss or mult scatt (cm)
      integer nwbuf   ! user buffer words for tracking medium
      parameter (nwbuf = 1)  ! buffer words generally not used in PISA media
      real    ubuf(nwbuf)    ! but they can be used as needed

c
c     define the Polystyrol material (CH)
c
      REAL*4 APST(2), ZPST(2), WPST(2), DPST
      DATA APST /12., 1./
      DATA ZPST / 6., 1./
      DATA WPST / 1., 1./
      DATA DPST / 1.05/ 
c
c     Define scint
c
      REAL*4 asci(2),zsci(2),wsci(2),dsci
      DATA asci/12.0,1.0/
      DATA zsci/6.0,1.0/
      DATA wsci/1.0,1.0/
      DATA dsci/1.032/
c
c     Define WLS
c
      REAL*4 awls(2),zwls(2),wwls(2),dwls
      DATA awls/12.0,1.0/
      DATA zwls/6.0,1.0/
      DATA wwls/1.0,1.0/
      DATA dwls/1.032/
c
c
c     Define plastic 1
c
      REAL*4 apl1(3),zpl1(3),wpl1(3),dpl1
      DATA apl1/12.0,1.0,16.0/
      DATA zpl1/6.0,1.0,8.0/
      DATA wpl1/5.0,8.0,2.0/
      DATA dpl1/1.18/
c
c
c     Define paper
c
      REAL*4 apap(3),zpap(3),wpap(3),dpap
      DATA apap/12.0,1.0,16.0/
      DATA zpap/6.0,1.0,8.0/
      DATA wpap/1.0,5.0,5.0/
      DATA dpap/0.6/
c

*HeB --- The steel side plates (VA, X5 CrNi 18 10) for Pbgl ---
      real*4 asteel(3),zsteel(3),wsteel(3),dsteel
      DATA ASTEEL /55.85,52.,58.7/
      DATA ZSTEEL /26.,24.,28./
      DATA WSTEEL /0.72,0.18,0.1/
      DATA DSTEEL /7.8/

c     begin execution
c
c     First do special EMCal materials definitions with GSMATE calls
c
      ubuf(1) = 0.

c
c     Call the GEANT-routine to create the Polystyrol ("Plastic")
c     Tracking medium #923 formerly in Mat_Mixt_Med routine
c
cgd***      CALL GSMIXT(923,'PST$',APST,ZPST,DPST,-2,WPST)
cgd***      CALL GSTMED(923,'PST$',923,0,0,0.,1.,0.1,0.1,1.E-4,5.E-4,0,0)
c
c     Change tracking media from 9xx to 8xx to comply with PISA convention
c     March 28, 1998
c
      CALL GSMIXT(823,'PST$',APST,ZPST,DPST,-2,WPST)
      CALL GSTMED(823,'PST$',823,0,0,0.,1.,0.1,0.1,1.E-4,5.E-4,0.0,0)
c

c
c     define IRON material (old FOPI definition, was in MAT_MIXT_MED)
c
cgd***      call gsmate(945,'Low Mag Iron$',55.85,28.0,7.87,
cgd***     1              1.76,17.1,ubuf,nwbuf)
      call gsmate(845,'Low Mag Iron$',55.85,28.0,7.87,
     1              1.76,17.1,ubuf,nwbuf)
c
c     define SCINTILLATOR material (old FOPI definition, was in MAT_MIXT_MED)
c
cgd***      call gsmate(980,'SCINTILLATOR$',6.670,3.60,1.180,
cgd***     1              0.344E02,0.708E02,ubuf,nwbuf)
c
c     define LEAD (Pb) material (old FOPI definition, was in MAT_MIXT_MED)
c
c
c     Tracking media # 945 - EMCal iron
c
cgd***      nmat = 945     ! iron 
      nmat = 845     ! iron 
      isvol = 0      ! not sensitive detector medium
      ifield = 0     ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 0.50  ! max disp. due to mulsct. in one step (cm)
      deemax = 0.05  ! max fractional energy loss in one step
      epsil = 0.1    ! tracking precision (cm)
      stmin = 0.1    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(945,'EMCal iron$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c
c     Special Thresholds for Iron Medium 945
c
cgd***      call gstpar(945,'CUTELE',0.001)      ! electron cut at 1 MeV
cgd***      call gstpar(945,'CUTGAM',0.001)      ! gamma cut at 1 MeV
cgd***      call gstpar(945,'BCUTE',0.001)       ! e brem cut at 1 MeV
cgd***      call gstpar(945,'CUTMUO',0.01)       ! muon cut at 10 MeV
cgd***      call gstpar(945,'BCUTM',0.01)        ! muon brem cut at 10 MeV
cgd***      call gstpar(945,'DCUTE',1.e+4)       ! e delta ray cut (Landau ON)
cgd***      call gstpar(945,'DCUTM',1.e+4)       ! muon delta ray cut (Landau ON)
cgd***      call gstpar(945,'CUTHAD',0.01)       ! charged hadron cut at 10 MeV
cgd***      call gstpar(945,'CUTNEU',0.01)       ! neutral hadron cut at 10 MeV
c
c
c
cgd***      call gsmate(999,'Lead_Sub_Conv$',207.19,82.0,11.35,
cgd***     1              0.56,18.5,ubuf,nwbuf)
cgd***      nmat = 999
cgd***      call gstmed(999,'Lead_sub_conv$',nmat,isvol,ifield,fieldm,tmaxfd,
cgd***     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
      call gsmate(899,'Lead_Sub_Conv$',207.19,82.0,11.35,
     1              0.56,18.5,ubuf,nwbuf)
      nmat = 899
      call gstmed(899,'Lead_sub_conv$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c
c ---------------------------------------------------------------
c
c     New definitions, (GD Sep 27, 1997)
c
cgd***      call gsmixt(981,'EMCal scintillator$',asci,zsci,dsci,-2,wsci)
cgd***      call gsmate(981,'EMCal scintillator$',6.670,3.60,1.180,
cgd***     1            34.4,70.8,ubuf,nwbuf)
cgd***      call gsmixt(982,'EMCal Wavelength shifter$',
cgd***     1                 awls,zwls,dwls,-2,wwls)
cgd***      call gsmate(983,'EMCal lead$',207.19,82.0,11.35,
cgd***     1              0.56,18.5,ubuf,nwbuf)
cgd***      call gsmate(984,'EMCal steel$',55.85,28.0,7.87,
cgd***     1              1.76,17.1,ubuf,nwbuf)
cgd***      call gsmixt(985,'EMCal tyvek paper$',apap,zpap,dpap,-3,wpap)
cgd***      call gsmixt(986,'EMCal plastic 1$',apl1,zpl1,dpl1,-3,wpl1)
c
      call gsmate(881,'EMCal scintillator$',6.670,3.60,1.180,
     1            34.4,70.8,ubuf,nwbuf)
      call gsmixt(882,'EMCal Wavelength shifter$',
     1                 awls,zwls,dwls,-2,wwls)
      call gsmate(883,'EMCal lead$',207.19,82.0,11.35,
     1              0.56,18.5,ubuf,nwbuf)
      call gsmate(884,'EMCal steel$',55.85,28.0,7.87,
     1              1.76,17.1,ubuf,nwbuf)
      call gsmixt(885,'EMCal tyvek paper$',apap,zpap,dpap,-3,wpap)
      call gsmixt(886,'EMCal plastic 1$',apl1,zpl1,dpl1,-3,wpl1)
c
c     Matrials 987-989 are reserved for future use (GD Sep 17, 1997)
c
c
c     Now do actual tracking media definitions
c
c
c     Tracking media # 981 - scint
c
cgd***      nmat = 981     ! scintillator
      nmat = 881     ! scintillator
      isvol = 1      ! sensitive detector medium
      ifield = 0     ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 0.50  ! max disp. due to mulsct. in one step (cm)
      deemax = 0.05  ! max fractional energy loss in one step
      epsil = 0.01    ! tracking precision (cm)
      stmin = 0.01    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
cgd***      call gstmed(981,'EMC scint$',nmat,isvol,ifield,fieldm,tmaxfd,
cgd***     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
      call gstmed(881,'EMC scint$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c
c     Special Thresholds for Scintillator
c     These thresholds may or may NOT be redefined in emc.f
c     However, their values will be passed to PISORP/STAF (GD, Sep 27, 1997)
c
cgd***      call gstpar(981,'CUTELE',0.0001)      ! electron cut at 100 keV
cgd***      call gstpar(981,'CUTGAM',0.0001)      ! gamma cut at 100 keV
cgd***      call gstpar(981,'BCUTE',0.0001)       ! e brem cut at 100 keV
cgd***      call gstpar(981,'CUTMUO',0.01)        ! muon cut at 10 MeV
cgd***      call gstpar(981,'BCUTM',0.01)         ! muon brem cut at 10 MeV
cgd***      call gstpar(981,'DCUTE',1.e+4)        ! e delta ray cut (Landau ON)
cgd***      call gstpar(981,'DCUTM',1.e+4)        ! muon delta ray cut (Landau ON)
c
      call gstpar(881,'CUTELE',0.0001)      ! electron cut at 100 keV
      call gstpar(881,'CUTGAM',0.0001)      ! gamma cut at 100 keV
      call gstpar(881,'BCUTE',0.0001)       ! e brem cut at 100 keV
      call gstpar(881,'CUTMUO',0.01)        ! muon cut at 10 MeV
      call gstpar(881,'BCUTM',0.01)         ! muon brem cut at 10 MeV
      call gstpar(881,'DCUTE',1.e+4)        ! e delta ray cut (Landau ON)
      call gstpar(881,'DCUTM',1.e+4)        ! muon delta ray cut (Landau ON)
c
c     Tracking media # 982 - WLS
c
cgd***      nmat = 982     ! scintillator
      nmat = 882     ! scintillator
      isvol = 1      ! sensitive detector medium
      ifield = 0     ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 0.50  ! max disp. due to mulsct. in one step (cm)
      deemax = 0.05  ! max fractional energy loss in one step
      epsil = 0.01    ! tracking precision (cm)
      stmin = 0.01    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
cgd***      call gstmed(982,'EMC WLS$',nmat,isvol,ifield,fieldm,tmaxfd,
cgd***     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
      call gstmed(882,'EMC WLS$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c
c     Special Thresholds for Scintillator
c     These thresholds may or may NOT be redefined in emc.f
c     However, their values will be passed to PISORP/STAF (GD, Sep 27, 1997)
c
cgd***      call gstpar(982,'CUTELE',0.0001)      ! electron cut at 100 keV
cgd***      call gstpar(982,'CUTGAM',0.0001)      ! gamma cut at 100 keV
cgd***      call gstpar(982,'BCUTE',0.0001)       ! e brem cut at 100 keV
cgd***      call gstpar(982,'CUTMUO',0.01)        ! muon cut at 10 MeV
cgd***      call gstpar(982,'BCUTM',0.01)         ! muon brem cut at 10 MeV
cgd***      call gstpar(982,'DCUTE',1.e+4)        ! e delta ray cut (Landau ON)
cgd***      call gstpar(982,'DCUTM',1.e+4)        ! muon delta ray cut (Landau ON)
c
      call gstpar(882,'CUTELE',0.0001)      ! electron cut at 100 keV
      call gstpar(882,'CUTGAM',0.0001)      ! gamma cut at 100 keV
      call gstpar(882,'BCUTE',0.0001)       ! e brem cut at 100 keV
      call gstpar(882,'CUTMUO',0.01)        ! muon cut at 10 MeV
      call gstpar(882,'BCUTM',0.01)         ! muon brem cut at 10 MeV
      call gstpar(882,'DCUTE',1.e+4)        ! e delta ray cut (Landau ON)
      call gstpar(882,'DCUTM',1.e+4)        ! muon delta ray cut (Landau ON)
c
c
c     Tracking media # 983 - EMC lead
c
cgd***      nmat = 983     ! Average lead
      nmat = 883     ! Average lead
      isvol = 0      ! not sensitive detector medium
      ifield = 0     ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 0.50  ! max disp. due to mulsct. in one step (cm)
      deemax = 0.05  ! max fractional energy loss in one step
      epsil = 0.01    ! tracking precision (cm)
      stmin = 0.01    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
cgd***      call gstmed(983,'EMC lead$',nmat,isvol,ifield,fieldm,tmaxfd,
cgd***     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
      call gstmed(883,'EMC lead$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c
c     Special Thresholds for Lead number 983
c
cgd***      call gstpar(983,'CUTELE',0.0001)      ! electron cut at 1 MeV
cgd***      call gstpar(983,'CUTGAM',0.0001)      ! gamma cut at 1 MeV
cgd***      call gstpar(983,'BCUTE',0.0001)       ! e brem cut at 1 MeV
cgd***      call gstpar(983,'CUTMUO',0.01)       ! muon cut at 10 MeV
cgd***      call gstpar(983,'BCUTM',0.01)        ! muon brem cut at 10 MeV
cgd***      call gstpar(983,'DCUTE',1.e+4)       ! e delta ray cut (Landau ON)
cgd***      call gstpar(983,'DCUTM',1.e+4)       ! muon delta ray cut (Landau ON)
c
      call gstpar(883,'CUTELE',0.0001)      ! electron cut at 1 MeV
      call gstpar(883,'CUTGAM',0.0001)      ! gamma cut at 1 MeV
      call gstpar(883,'BCUTE',0.0001)       ! e brem cut at 1 MeV
      call gstpar(883,'CUTMUO',0.01)       ! muon cut at 10 MeV
      call gstpar(883,'BCUTM',0.01)        ! muon brem cut at 10 MeV
      call gstpar(883,'DCUTE',1.e+4)       ! e delta ray cut (Landau ON)
      call gstpar(883,'DCUTM',1.e+4)       ! muon delta ray cut (Landau ON)
c
c     Tracking media # 984 - EMC steel
c
cgd***      nmat = 984     ! Steel
      nmat = 884     ! Steel
      isvol = 0      ! not sensitive detector medium
      ifield = 0     ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 0.50  ! max disp. due to mulsct. in one step (cm)
      deemax = 0.05  ! max fractional energy loss in one step
      epsil = 0.01    ! tracking precision (cm)
      stmin = 0.01    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
cgd***      call gstmed(984,'EMC steel$',nmat,isvol,ifield,fieldm,tmaxfd,
cgd***     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
      call gstmed(884,'EMC steel$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c
c     Special Thresholds for steel number 984
c
cgd***      call gstpar(984,'CUTELE',0.0001)      ! electron cut at 1 MeV
cgd***      call gstpar(984,'CUTGAM',0.0001)      ! gamma cut at 1 MeV
cgd***      call gstpar(984,'BCUTE',0.0001)       ! e brem cut at 1 MeV
cgd***      call gstpar(984,'CUTMUO',0.01)       ! muon cut at 10 MeV
cgd***      call gstpar(984,'BCUTM',0.01)        ! muon brem cut at 10 MeV
cgd***      call gstpar(984,'DCUTE',1.e+4)       ! e delta ray cut (Landau ON)
cgd***      call gstpar(984,'DCUTM',1.e+4)       ! muon delta ray cut (Landau ON)
c
      call gstpar(884,'CUTELE',0.0001)      ! electron cut at 1 MeV
      call gstpar(884,'CUTGAM',0.0001)      ! gamma cut at 1 MeV
      call gstpar(884,'BCUTE',0.0001)       ! e brem cut at 1 MeV
      call gstpar(884,'CUTMUO',0.01)       ! muon cut at 10 MeV
      call gstpar(884,'BCUTM',0.01)        ! muon brem cut at 10 MeV
      call gstpar(884,'DCUTE',1.e+4)       ! e delta ray cut (Landau ON)
      call gstpar(884,'DCUTM',1.e+4)       ! muon delta ray cut (Landau ON)
c
c
c     Tracking media # 985 - EMC tyvek
c
cgd***      nmat = 985     ! Tyvek
      nmat = 885     ! Tyvek
      isvol = 0      ! not sensitive detector medium
      ifield = 0     ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 0.50  ! max disp. due to mulsct. in one step (cm)
      deemax = 0.05  ! max fractional energy loss in one step
      epsil = 0.001    ! tracking precision (cm)
      stmin = 0.01    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
cgd***      call gstmed(985,'EMC tyvek$',nmat,isvol,ifield,fieldm,tmaxfd,
cgd***     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
      call gstmed(885,'EMC tyvek$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c
c     Special Thresholds for tyvek 985
c
cgd***      call gstpar(985,'CUTELE',0.0001)      ! electron cut at 1 MeV
cgd***      call gstpar(985,'CUTGAM',0.0001)      ! gamma cut at 1 MeV
cgd***      call gstpar(985,'BCUTE',0.0001)       ! e brem cut at 1 MeV
cgd***      call gstpar(985,'CUTMUO',0.01)       ! muon cut at 10 MeV
cgd***      call gstpar(985,'BCUTM',0.01)        ! muon brem cut at 10 MeV
cgd***      call gstpar(985,'DCUTE',1.e+4)       ! e delta ray cut (Landau ON)
cgd***      call gstpar(985,'DCUTM',1.e+4)       ! muon delta ray cut (Landau ON)
c
      call gstpar(885,'CUTELE',0.0001)      ! electron cut at 1 MeV
      call gstpar(885,'CUTGAM',0.0001)      ! gamma cut at 1 MeV
      call gstpar(885,'BCUTE',0.0001)       ! e brem cut at 1 MeV
      call gstpar(885,'CUTMUO',0.01)       ! muon cut at 10 MeV
      call gstpar(885,'BCUTM',0.01)        ! muon brem cut at 10 MeV
      call gstpar(885,'DCUTE',1.e+4)       ! e delta ray cut (Landau ON)
      call gstpar(885,'DCUTM',1.e+4)       ! muon delta ray cut (Landau ON)
c
c
c     Tracking media # 986 - EMC plastic 1
c
cgd***      nmat = 986     ! Steel
      nmat = 886     ! Steel
      isvol = 0      ! not sensitive detector medium
      ifield = 0     ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 0.50  ! max disp. due to mulsct. in one step (cm)
      deemax = 0.05  ! max fractional energy loss in one step
      epsil = 0.01    ! tracking precision (cm)
      stmin = 0.01    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
cgd***      call gstmed(986,'EMC steel$',nmat,isvol,ifield,fieldm,tmaxfd,
cgd***     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
      call gstmed(886,'EMC steel$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c
c     Special Thresholds for plastic 1 986
c
cgd***      call gstpar(986,'CUTELE',0.0001)      ! electron cut at 1 MeV
cgd***      call gstpar(986,'CUTGAM',0.0001)      ! gamma cut at 1 MeV
cgd***      call gstpar(986,'BCUTE',0.0001)       ! e brem cut at 1 MeV
cgd***      call gstpar(986,'CUTMUO',0.01)       ! muon cut at 10 MeV
cgd***      call gstpar(986,'BCUTM',0.01)        ! muon brem cut at 10 MeV
cgd***      call gstpar(986,'DCUTE',1.e+4)       ! e delta ray cut (Landau ON)
cgd***      call gstpar(986,'DCUTM',1.e+4)       ! muon delta ray cut (Landau ON)
c
      call gstpar(886,'CUTELE',0.0001)      ! electron cut at 1 MeV
      call gstpar(886,'CUTGAM',0.0001)      ! gamma cut at 1 MeV
      call gstpar(886,'BCUTE',0.0001)       ! e brem cut at 1 MeV
      call gstpar(886,'CUTMUO',0.01)       ! muon cut at 10 MeV
      call gstpar(886,'BCUTM',0.01)        ! muon brem cut at 10 MeV
      call gstpar(886,'DCUTE',1.e+4)       ! e delta ray cut (Landau ON)
      call gstpar(886,'DCUTM',1.e+4)       ! muon delta ray cut (Landau ON)
c
c
c
*HeB Additional materials and media for Pbgl

cgd***      call gsmate(911,'CARBON$',12.01,6.0,2.265,18.8,
cgd***     1              0.56,ubuf,nwbuf)
      call gsmate(811,'CARBON$',12.01,6.0,2.265,18.8,
     1              0.56,ubuf,nwbuf)

cgd***      nmat = 911     ! Carbon
      nmat = 811     ! Carbon
      isvol = 0      ! not sensitive detector medium
      ifield = 0     ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 10.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 1.  ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1  ! max fractional energy loss in one step
      epsil = 0.01    ! tracking precision (cm)
      stmin = 0.01    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
cgd***      call gstmed(911,'CARBON$',nmat,isvol,ifield,fieldm,tmaxfd,
cgd***     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
      call gstmed(811,'CARBON$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c

cgd***      CALL GSMIXT(912,'STEEL$',ASTEEL,ZSTEEL,DSTEEL,3,WSTEEL)
      CALL GSMIXT(812,'STEEL$',ASTEEL,ZSTEEL,DSTEEL,3,WSTEEL)

cgd***      nmat = 912     ! Carbon
      nmat = 812     ! Carbon
      isvol = 0      ! not sensitive detector medium
      ifield = 0     ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 10.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 1.  ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1  ! max fractional energy loss in one step
      epsil = 0.01    ! tracking precision (cm)
      stmin = 0.01    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
cgd***      call gstmed(912,'STEEL$',nmat,isvol,ifield,fieldm,tmaxfd,
cgd***     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
      call gstmed(812,'STEEL$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c   Tracking media # 16 - vacuum (for inside the pipe)
      nmat = 16   ! GEANT vacuum
      isvol = 1   ! not sensitive
      ifield = 0  ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 30.0  ! max disp. due to mulsct. in one step (cm)
      deemax = 1.0   ! max fractional energy loss in one step
      epsil = 1.  ! tracking precision (cm)
      stmin = 20. ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(813,'VACUUM$',nmat,isvol,ifield,fieldm,tmaxfd,
     1            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
      return
      end

