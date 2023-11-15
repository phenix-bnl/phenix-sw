c $Id: tecgeo.f,v 1.7 2009/06/27 15:11:48 pinkenbu Exp $
* ============== PHENIX Time Expansion Chamber geometry ================
*                          ( for PISA )
      SUBROUTINE TECGEO

      implicit none

#include "g77trigdef.inc"

*  ---------------------------------------
*    A.Chikanian     Yale University
*-------------------------------------------
*    Version     Date        comments
*     0.00   (10 Oct.,93) creation for PISA
*     1.00   (20 Oct.,93)
*     2.00   (17 Nov.,93)
*     2.01   (08 Jul.,94) additions for TEC simulation
*     3.00   (05 Sep.,94)
*     3.01   (09 Nov.,94)
*     4.00   (   May  95) Compatibility with PC-2,3
*     5.00   (28 Oct.,95) Plane configuration generalisation
*     6.00   (20 Nov.,95) compatibility with tec_geom_par.f
*     6.01   (10 Feb.,96) because new tec_geom_par.f
*     6.04   (14 Feb.,96) corrections acording new drawing
*     6.05   (25 Feb.,96) 
*     6.06   (08 Mar.,96) remove tec_geom_par, one module (TECGEO) again
*     7.00   (20 Mar.,96) compatibility with TRK,NTRK,XTRK in PISORP
*     7.01   (21 Mar.,96) strip position corrected in wire support structure
*     8.00   (23 Mar.,96) introducing SECTOR volume
*     9.00   (15 Feb.,98) add date to PARA bank, 15th entry
*---------------------------------------------------------------------------
*     Sasha Lebedev (S.L.) --- Iowa State University --- lebedev@iastate.edu
*     ----------------------------------------------------------------------
*           (Dec. 08, 98) TEC geometry placed in phnx.par (trd_par namelist)
*			  Only East Arm activated by default
*---------------------------------------------------------------------------


* --- GEANT commons
#include "gugeom.inc"
* --- PISA commons
#include "sublink.inc"
#include "fptlink.inc"
#include "fstore.inc"


c     Zebra parameter bank variables

      character*10 chform
      integer iod  

*  --------------------------------------------------------------
*  GLOBAL geometrical parameters 
*  ---------------------
      Integer   NtecMax   ! Max# of planes
      Parameter(NtecMax   =   6)
      Integer Narms,Iarmor,Nsect,Ntec,Ltec(NtecMax)
      Real AngS
*-------------- S.L. --- Dec.98 -----------------------
	Integer FirstArm,LastArm,EastArm,WestArm
	Real ZszO(6)
	Integer trdupgrade	! SL 10/02/2002
	NAMELIST /TRD_PAR/
     &    WestArm,
     &    EastArm,
     &    Nsect,
     &    R0,
     &    ThRad,
     &    ThXe,
     &    AngS,
     &    Ltec,
     &    ThickF,
     &    ThickX,
     &    ThickZ,
     &    Ysup, 
     &    ThickS,  
     &    ThickS1,
     &    ThSLU, 
     &    ThStr, 
     &    ThWin, 
     &    ThWS,  
     &    XWS,   
     &    SST,   
     &    ZszO, 
     &    trdupgrade
*--------------------- S.L. --------------------------
 
* ----------------------------------------------------
* =============  PHENIX global parameters  ===========
*  Narms - Number of Arms (usually 2)
*  Iarmor- Arms orientation parameter 0:  45.0 deg. between arms
*                                  or 1:  67.5 deg.
*  AngS  - open angle of sector
* ----------------------------------------------------
* =============  T E C global parameters  ============
*  Nsect - number of sectors/arm (at z>0) (usually 4)
*  Ntec    - number active planes/sector
*  Ltec(6) - List of TEC active planes numbers
*            reasonable combinations:   1,2,3,4,0,0
*                                 or    3,4,5,6,0,0
*                                 or    2,3,4,5,0,0
*                                 or    1,2,5,6,0,0
*                                 or    1,2,3,4,5,6
*----------------------------------------------------------------
      Real R0,ThRad,ThXe
*  R0     - distance from centr to first plane(radiator)
*  ThRad  - radiator thicknes
*  ThXe   - gas thiknes in chamber
*----------------------------------------------------------------
      Real Par(10)
      Real ZsizOut(NtecMax)
      Real ARAD(2),ZRAD(2),WRAD(2)
      Real ACO2(2),ZCO2(2),WCO2(2)
      Real AMYLAR(3),ZMYLAR(3),WMYLAR(3)
      Real AXeHe(2),ZXeHe(2),WXeHe(2)
      Integer MEGAS(NtecMax)
      Character*4 AGAS(NtecMax)
      Character*4 FR(NtecMax),XE(NtecMax),Ra(NtecMax)
     +           ,S1(NtecMax),S2(NtecMax),Xe2(NtecMax),Xe3(NtecMax)
     +           ,PC1(NtecMax),PC2(NtecMax),PC3(NtecMax)
     +           ,W1(NtecMax),W2(NtecMax),W3(NtecMax)
     +           ,S1W(NtecMax),AIR(NtecMax),Co(NtecMax)
c     +           ,S2W(NtecMax)

      Integer maair,mafrm,mag10,maco2,marad,maxea,maxen,mamyl,magraf
      Integer meair,mefrm,meg10,meco2,merad,mexea,mexen,memyl,megraf
      Integer npar,nlmat,mfld,ivol,iph
      Integer nn,i,ise,ll,ir,iz,ix,ixs,ipl
      Integer Ntma
*   zebra variables
c      Integer iod          ! Z
c      character*20 chform  ! Z

      Real dens,scalef,radlen,abslen,conc
      Real dphi,rpos,rpoc,p01,p02,p03,p04,xpos,ypos
      Real aa,zz,dd,x,y,xx,yy
      Real fma,an,dm,de,eps,st ! tracking par.
      Real thickf,thickx,thickz,ysup,thicks,thicks1
      Real ThSLU,ThStr,ThWin,ThWS,XWS,Sst
      Real Th1,Ph1,Th2,Ph2,Th3,Ph3 ! rotation matrix angles
      Real TgA,dSup,ThCham,ThSect,HWS

* ------------------------------------------------------------
      Real Xinch/2.54/   !   Conversion factor from inch to cm
c     Integer itest/1/
c     Integer nnn/0/
* ------------------------------------------------------------
* --- Material and medium numders
      DATA MAAIR,MAFRM,MAG10,MACO2,MARAD,MAXEA,MAXEN,MAMYL,MAGRAF
     +    /  680,  681,  682,  683,  684,  685,  686,  687,  688/
      DATA MEAIR,MEFRM,MEG10,MECO2,MERAD,MEXEA,MEXEN,MEMYL,MEGRAF
     +    /  680,  681,  682,  683,  684,  685,  686,  687,  688/

* ----    Polyethylene (CH2)n  --  Radiator
      DATA ARAD/ 12.01, 1./
      DATA ZRAD/  6.,   1./
      DATA WRAD/  1.,   2./

*-----     ( 50%Xe + 50%He )     Gas  TEC     ---
      DATA AXeHe/ 131.29,4.0/, ZXeHe/54.,2./, WXeHe/ 0.5, 0.5/

* ----   Mylar
      DATA AMYLAR/12.01, 1.01, 15.99/
      DATA ZMYLAR/ 6.,   1.,    8. /
      DATA WMYLAR/ 5.,   4.,    2./

* ----   CO2
      DATA ACO2/ 12.01, 15.99/
      DATA ZCO2/  6.,    8. /
      DATA WCO2/  1.,    2./

* ------ Volume Names -----------------------------------
* plane#         1      2      3      4      5      6
      Data FR /'FRM1','FRM2','FRM3','FRM4','FRM5','FRM6'/
      Data Xe /'XEN1','XEN2','XEN3','XEN4','XEN5','XEN6'/
      Data AIR/'AIR1','AIR2','AIR3','AIR4','AIR5','AIR6'/
      Data RA /'RAD1','RAD2','RAD3','RAD4','RAD5','RAD6'/
      Data S1 /'SP11','SP12','SP13','SP14','SP15','SP16'/
      Data S2 /'SP21','SP22','SP23','SP24','SP25','SP26'/
      Data Xe2/'XE21','XE22','XE23','XE24','XE25','XE26'/
      Data Xe3/'XE31','XE32','XE33','XE34','XE35','XE36'/
      Data Co /'CO 1','CO 2','CO 3','CO 4','CO 5','CO 6'/
      Data PC1/'PC11','PC12','PC13','PC14','PC15','PC16'/
      Data PC2/'PC21','PC22','PC23','PC24','PC25','PC26'/
      Data PC3/'PC31','PC32','PC33','PC34','PC35','PC36'/
      Data W1 /'WI11','WI12','WI13','WI14','WI15','WI16'/
      Data W2 /'WI21','WI22','WI23','WI24','WI25','WI26'/
      Data W3 /'WI31','WI32','WI33','WI34','WI35','WI36'/
      Data S1W/'S1W1','S1W2','S1W3','S1W4','S1W5','S1W6'/
c      Data S2W/'S2W1','S2W2','S2W3','S2W4','S2W5','S2W6'/
       
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun

* -------------------------------------------------------
* -------------------------------------------------------
* ---  PHENIX / T E C  global geometry configuration  ---

C      Narms =2  ! number of arms
*---------------------- S.L. --- Dec. 98 --------------------------------
      Narms =1  ! number of arms (only East Arm present by default)
      WestArm=0
      EastArm=1
      FirstArm=1
      LastArm=1
      trdupgrade=0	! SL 10/02/2002
*------------------------- S.L. -----------------------------------------
      Iarmor=1  ! arms orientation parameter: 0  45.0 deg. between arms.
*                                             1  67.5 deg.(standart for now)
      Nsect =4  ! number of sectors per arm

*   List of active TEC-plane numbers
*                    reasonable  combinations
      Ltec(1) = 1    !  1 or 2 or 3 or 1 or 1
      Ltec(2) = 2    !  2    3    4    2    2
      Ltec(3) = 3    !  3    4    5    5    3
      Ltec(4) = 4    !  4    5    6    6    4
      Ltec(5) = 5    !  0    0    0    0    5
      Ltec(6) = 6    !  0    0    0    0    6

* ------------------  Geometry  Parameters  ----------------------------
c     R0       = 423.448   !  Radial distance to first radiator, Feb.14,96
      R0       = 421.7 +2.5!  Old value to be compatible with TRK,NTRK,XTRK
      AngS     =  11.25    !  Half angle of one sector in phi(deg.)  
      ThRad    =   7.065   !  Thicknes of radiator
      ThXe     =   3.702   !  Thicknes of chamber ( Xe+He )
* ---------------------------------------------------------------------
      NtMa=NtecMax
* ----------------------------- this block for TEST ONLY !!!
c     If(Itest.NE.0) Then    !   test
c       Read(22,*) Itest     !   test
c       If(Itest.NE.0) Then  !   test
c         Read(22,*) R0,Ntma,Ltec,Nsect,Narms,Iarmor       ! Test
c       ! Itest = 1  ! All parameters from file            ! Test
c       ! Itest = 2  ! One sector with canonic orientation ! Test
c       ! Itest = 3  ! As 3 but fixed small size           ! Test
c         If(Itest.ge.2) Then!   Test
c           Ntma  = 1        !   Test
c           Nsect = 1        !   Test
c           NARMS = 1        !   Test
c           Ltec(1) = 1      !   Test
c           do ipl=2,NtecMax !   Test
c             Ltec(ipl)=0    !   Test
c           enddo            !   Test
c         EndIf              !   Test 
c       EndIf                !   Test 
c     EndIf                  ! End of Test
* -----------------------------------------
* --------------- Geometry  Parameters (for GEANT only) ----------------
      ThickF =   1.94      ! Frame Thick.(r-phi)(Rad.section) Graph./Epoxi
      ThickX =   2.5*Xinch ! Frame Thick.( in Z)( Xe section)
      ThickZ =   0.6       ! Frame Thick.( in Z)(Rad section)
      Ysup   =  3.5        ! Width of wires support
      ThickS =  .05        ! Thicknes of wires support and PC board (G10)
      ThickS1=  .076       ! Thicknes of wires support in Xe sect.,vertic.
      ThSLU  =  .3         ! Thicknes of ...
      ThStr  =  .1         ! Thicknes of strip (G10)
      ThWin  =  .0050      ! Thicknes of windowes (mylar)
      ThWS   =  .076       ! Thicknes of window support strip(Graphite/Epoxi)
      XWS    =  Xinch      ! Width of window support horizontal strip
      SST    =  10.*Xinch  ! Distance between vertical window support strips
*-------------- Z-size  ---------------
*    numbers from Ed s Fax on Feb.14,96
      ZsizOut(1) = 310.9453/2. + ThickX
      ZsizOut(2) = 318.6553/2. + ThickX
      ZsizOut(3) = 326.3653/2. + ThickX
      ZsizOut(4) = 334.0753/2. + ThickX
      ZsizOut(5) = 341.7853/2. + ThickX
      ZsizOut(6) = 349.4953/2. + ThickX
*---------------------------------------------------------------
*   # of active planes definition
      Ntec=0
      Do ipl=1,NtMa
        If(Ltec(ipl).gt.0) Ntec=Ntec+1
      EndDo

*------------ S.L. start reading geometry from phnx.par ------------------

*  Inserted by Sasha Lebedev, ISU --- Dec. 08, 1998
*  Read TEC geometry parameters from phnx.par (trd_par namelist)

      write( *,* ) 'tecgeo - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = trd_par, err = 992 )
      WRITE(*,*)
     &  'tecgeo - successfully read TEC geometry',
     &  'from trd_par namelist'

      if(EastArm.eq.0) FirstArm=2
      if(WestArm.gt.0) LastArm=2
      if(WestArm.gt.0.and.EastArm.gt.0) Narms=2

      ZsizOut(1) = ZszO(1)/2. + ThickX
      ZsizOut(2) = ZszO(2)/2. + ThickX
      ZsizOut(3) = ZszO(3)/2. + ThickX
      ZsizOut(4) = ZszO(4)/2. + ThickX
      ZsizOut(5) = ZszO(5)/2. + ThickX
      ZsizOut(6) = ZszO(6)/2. + ThickX

      Ntec=0
      Do ipl=1,NtMa
        If(Ltec(ipl).gt.0) Ntec=Ntec+1
      EndDo

      if(trdupgrade.ne.0) then
        if(trdupgrade.eq.1) then
          WRITE(*,'(a)')'tecgeo - trd upgrade INSTALLED (P10).'
        else
          WRITE(*,'(a)')'tecgeo - trd upgrade INSTALLED (Xe/He).'
        endif
      else
        WRITE(*,'(a)')'tecgeo - trd upgrade NOT installed.'
      endif
      GOTO 993

 992  WRITE(*,*)
     &  'tecgeo - tead error in trd_par namelist of phnx.par.',
     &  'tecgeo - will use default geometry.'

 993  CONTINUE

*------------- S.L. end reading geometry from phnx.par -----------------

      If( Ntec .gt.NtecMax) Ntec=NtecMax
      If( Nsect.gt.4 ) Nsect = 4
      If( Narms.gt.2 ) Narms = 2

      Write(6,*) '===========   T E C global configuration   =========='
      Write(6,'(a11,i3,f10.1,a18)')
     +     ' #Arms    :',Narms,45+2*Iarmor*AngS,' deg. between arms'
      Write(6,*) '#Sect/arm: ',Nsect
      Write(6,*) '# of Planes:',NtMa,'   # of Active Planes:',Ntec
      Write(6,*) 'List of Active Planes:',Ltec
      Write(6,*) 'West Arm:',WestArm
      Write(6,*) 'East Arm:',EastArm
      Write(6,*) 'First, Last Arms:',FirstArm,LastArm
      Write(6,*) 'ThickF,ThickX,ThickZ:',ThickF,ThickX,ThickZ
      Write(6,*) 'Ysup,ThickS,ThickS1:',Ysup,ThickS,ThickS1
      Write(6,*) 'ThSLU,ThStr,ThWin,ThWS:',ThSLU,ThStr,ThWin,ThWS
      Write(6,*) 'XWS,SST:',XWS,SST
      Write(6,*) 'ZsizOut:',ZsizOut
      Write(6,*) '====================================================='

*--------------------------------------------------------------------
* Transformation to TEC Parameter bank in ZEBRA-structure
*     (takes 15 parameters)

       chform = '10I 4F -I'  ! 10 integers, 4 floating  point, rest integer
       call mzform('PARA', CHFORM, iod)  ! book characteristics
       call mzbook(ixdiv_fr,lft_para,lft_para,1,'PARA',0,0,15,iod,0)

       iqf(lft_para + 1) = Narms
       iqf(lft_para + 2) = Nsect
       iqf(lft_para + 3) = Iarmor

       iqf(lft_para + 4) = NTEC
       iqf(lft_para + 5) = Ltec(1)
       iqf(lft_para + 6) = Ltec(2)
       iqf(lft_para + 7) = Ltec(3)
       iqf(lft_para + 8) = Ltec(4)
       iqf(lft_para + 9) = Ltec(5)
       iqf(lft_para +10) = Ltec(6)

        qf(lft_para +11) = R0
        qf(lft_para +12) = AngS
        qf(lft_para +13) = ThRad
        qf(lft_para +14) = ThXe
       iqf(lft_para +15) = 19980215

*--------------------------------------------------------
*   initially all planes are passive
      Do i=1,NtecMax
        MEGAS(i)=MEAIR
        AGAS(i)=AIR(i)
      EndDo

*   Selected planes activation
      Do i=1,Ntec
        ipl=Ltec(i) ! active planes
        MEGAS(ipl)=MEXEA
        AGAS(ipl)=Xe(ipl)
      EndDo

*  ------------------------    MATERIALS   -----------------------
*  --------------------------------------------------  Air  ------
      LL=1
      DENS=.001205
      RADLen=30423.
      ABSLen=67500.
      CALL GSMATE(MaAir,'AIR$',14.61,7.3,dens,RADLen,ABSLen,DD,LL)

*  ---------------------------------- Graphite/Epoxy ( Frame ) ----
      LL=1
      AA = 12.011
      ZZ = 6.
      ScaleF = 1.  !  assumed solid now
      Dens   = 1.68 * ScaleF
      RadLen   = 25./ScaleF
      AbsLen = 99.
      CALL GSMATE(MAFRM,'Graph/Epo$',AA,ZZ,Dens,RadLen,AbsLen,DD,LL)

*  ---------------------------- Graphite/Epoxy  ( Window Support ) ----
      LL=1
      AA = 12.011
      ZZ = 6.
      Dens   = 1.68
      RadLen = 25.
      AbsLen = 99.
      CALL GSMATE(MAGRAF,'Graph/Epoxy$',AA,ZZ,Dens,RadLen,AbsLen,DD,LL)

*  -----------------------------------   G10   (  Wires Support ) ---7--
      LL=1
      AA = 20.
      ZZ = 10.
      DENS   =  1.7
      RadLen = 19.4
      AbsLen = 53.6
      CALL GSMATE(MAG10,'G10$',AA,ZZ,Dens,RadLen,AbsLen,DD,LL)

* -----------------------   Gas in  T E C ---------------------------
* -------------------- ( 90% Ar + 10% CH4 = 91% ) ----  P10 ( day 1 )

      if(trdupgrade.lt.2) then

        Conc = 0.91 ! Argon effective concentration in P10
        LL=1 
        ZZ=18.
        AA=39.95
        DENs    = 1.78e-3 * Conc
        RadLen  = 7262./Conc
        AbsLen  = 8000. ! Dummy
        CALL GSMATE(MAXEA,'ArgonA',ZZ,AA,DENs,RadLen,AbsLen,DD,LL) ! active
        CALL GSMATE(MAXEN,'Argon ',ZZ,AA,DENs,RadLen,AbsLen,DD,LL) ! passive

      else

* ----------------------------------------------  Xe  ( TRD upgrate )
C        Conc = 0.5
C        LL=1 
C        AA=131.29
C        ZZ=54.
C        DENs   = 5.89e-3 * Conc
C        RadLen  = 1436.8 / Conc
C        AbsLen  = 4000. ! Dummy
C        CALL GSMATE(MAXEA,'XenonA$',ZZ,AA,DENs,RadLen,AbsLen,DD,LL)
C        CALL GSMATE(MAXEN,'Xenon $',ZZ,AA,DENs,RadLen,AbsLen,DD,LL)

*--------------------------------   50%Xe + 50%He ( possible upgrate )
        Dens = .001*(5.89+0.178)/2.
        NLMAT = -2
        CALL GSMIXT(MAXEA,'50%Xe+50%He$',AXeHe,ZXeHe,Dens,NLMAT,WXeHe)
        CALL GSMIXT(MAXEN,'50%Xe+50%He$',AXeHe,ZXeHe,Dens,NLMAT,WXeHe)

      endif

* ----------------------------  Polyethylene (CH2)n  ( TRD  radiator )
c SL 10/02/2002
c Take radiator density from Rob's talk: 
c http://www.phenix.bnl.gov/~pisani/TRD/TRDupgrade.htm
c Radiator is 17 micron polypropylene (CH2) fibers.
c We include in radiator Rohacell (polymetacrylimide, C6H8ON) stiffener,
c which has approximately the same density and radiation length 
c as the fibers.   
c Radiator total thickness is 6.35 cm, so, for simplicity we correct the
c density by 6.35/7.0 (radiator volume thickness is 7.0 cm).

      NLMAT= - 2
c      DENs=.03366 !   0.53% r.l. in 7 cm
      DENs=.06*6.35/7.0 !   0.94% r.l. 
      CALL GSMIXT(MARAD,'Polyethylene$',ARAD,ZRAD,DENs,NLMAT,Wrad) 

* -------------------------------    Mylar  (  Windows (50mk) ) ----
      NLMAT= - 3
      Dens = 1.39
      CALL GSMIXT(MAMYL,'MYLAR$',AMYLAR,ZMYLAR,DENS,NLMAT,Wmylar) 

* ----------------------------------------------   CO2  ------------ 
      NLMAT= - 2
      Dens = .001977 ! CO2
C      Dens = ( ACO2(1) + 2.*ACO2(2) ) / 22.4 / 1000.
      CALL GSMIXT(MaCO2,'CO2$',ACO2,ZCO2,Dens,NLMAT,WCO2)


* =====================  MEDIUMS INSTALLATION =====================
* ---------------------  Tracking parameters  ---------------------
      MFLD = 1     ! Runge-Kutta
      FMA  = 10.   ! Max.Magn.Field (KGauss)
      AN   = 1.    ! Dif Ang in one step (Deg.)
      DM   = .5    ! Max.Displacement/step (cm.)
      DE   = .2    ! Max.fract.en.loss/step
      EPS  = .001  ! Tracking precision
      ST   = .8    ! Min. step due en.loss or m.scat. (cm.)

* --------------------      M E D I U M S      ----------------------7--
      LL=1
      CALL GSTMED(MEAIR,'AIR$',MAAIR,0,MFLD,FMA,AN,DM,DE,EPS,ST,DD,LL)
      LL=1
      CALL GSTMED(MEFRM,'FRM$',MAFRM,0,MFLD,FMA,AN,DM,DE,EPS,ST,DD,LL)
      LL=1
      CALL GSTMED(MEG10,'G10$',MAG10,0,MFLD,FMA,AN,DM,DE,EPS,ST,DD,LL)
      LL=1
      CALL GSTMED(MERAD,'RAD$',MARAD,0,MFLD,FMA,AN,DM,DE,EPS,ST,DD,LL)
      LL=1
      CALL GSTMED(MEXEA,'XEA$',MAXEA,1,MFLD,FMA,AN,DM,DE,EPS,ST,DD,LL)
      LL=1
      CALL GSTMED(MEXEN,'XEN$',MAXEN,0,MFLD,FMA,AN,DM,DE,EPS,ST,DD,LL)
      LL=1
      CALL GSTMED(MECO2,'CO2$',MACO2,0,MFLD,FMA,AN,DM,DE,EPS,ST,DD,LL)
      LL=1
      CALL GSTMED(MEMYL,'MYL$',MAMYL,0,MFLD,FMA,AN,DM,DE,EPS,ST,DD,LL)
      LL=1
      CALL GSTMED(MEGRAF,'GRA$',MAGRAF,0,MFLD,FMA,AN,DM,DE,EPS,ST,DD,LL)

* --------------- Geometry  Parameters (for GEANT only) ----------------
      TgA    =  Tand(AngS)
      ThCham =  ThXe + ThRad      ! chamber sizer in r (rad+xe)
      ThSect =  ThCham*NtMa       ! sector size in r
      dSup   =  ThStr + 2.*ThSLU
      HWS    =  ThRad - ThWin  - 2.*ThWS
      RpoS = R0 + ThSect/2.       ! r - centr of sect
      SST = ((R0-ThXe+ThSect)*TgA-ThickF)/4.! dist. between vert. w.s.str.

* ----------------------  V o l u m e s -------------------------
      NPAR=0
      Do ir = 1,NtMa                ! loop over the planes
        CALL GSVOLU(  FR(ir),'TRD1',MEFRM,PAR,NPAR,IVOL)     ! Frame
        CALL GSVOLU(AGAS(ir),'TRD1',MEGAS(ir),PAR,NPAR,IVOL) ! Xenon/P10
c SL 10/02/2002
        if(trdupgrade.eq.0) then
          CALL GSVOLU(  Ra(ir),'TRD1',MEAIR,PAR,NPAR,IVOL)     ! Radiator (Air)
        else
          CALL GSVOLU(  Ra(ir),'TRD1',MERAD,PAR,NPAR,IVOL)     ! Radiator (polypropylene fibers)
        endif
        CALL GSVOLU( S1(ir),'TRD1',MEG10,PAR,NPAR,IVOL)  ! Support 1 (in RAD)
        CALL GSVOLU( Co(ir),'TRD1',MEAIR,PAR,NPAR,IVOL)  ! Sup 1 (Air)
C       CALL GSVOLU( Co(ir),'TRD1',MECO2,PAR,NPAR,IVOL)  ! Sup 1 (CO2)
        CALL GSVOLU( S2(ir),'TRD1',MEG10,PAR,NPAR,IVOL)  ! Sup 2 (in Xe)
        CALL GSVOLU(Xe2(ir),'TRD1',MEXEN,PAR,NPAR,IVOL)  ! Xen2 in Sup 2
        CALL GSVOLU(Xe3(ir),'TRD1',MEXEN,PAR,NPAR,IVOL)  ! Xen3 in Xenon
        CALL GSVOLU(PC3(ir),'BOX ',MEG10,PAR,NPAR,IVOL)  ! PCboard 3
        CALL GSVOLU(PC2(ir),'BOX ',MEG10,PAR,NPAR,IVOL)  ! PCboard 2
        CALL GSVOLU(PC1(ir),'BOX ',MEG10,PAR,NPAR,IVOL)  ! Strip (PCB 1)
        CALL GSVOLU( W3(ir),'BOX ',MEMYL,PAR,NPAR,IVOL)  ! Wind 3, top, in Xe
        CALL GSVOLU( W2(ir),'BOX ',MEMYL,PAR,NPAR,IVOL)  ! Wind 2, mid, in 
        CALL GSVOLU( W1(ir),'BOX ',MEMYL,PAR,NPAR,IVOL)  ! Wind 1,bot, in Rad
        CALL GSVOLU(S1W(ir),'BOX ',MEGRAF,PAR,NPAR,IVOL) ! Wind.Sup.V&H
      EndDo  ! ir

c      Do i=1,Ntec
c        ipl=Ltec(i)
c        Call GSATT(AGAS(ipl),'FILL',1)
c        Call GSATT(AGAS(ipl),'COLO',3)
c      EndDo
* --------------------------------------------------- SECTOR
      NPAR=4
      Par(1) = R0         *Tga        !  dx, finaly dx sect bot
      Par(2) = (R0+ThSect)*TgA        !  dX, finaly dx sect top
      Par(3) = ZsizOut(NtMa)          !  dY, finaly dZ sect
      Par(4) = ThSect / 2.            !  dZ, finaly dy sect

c     If(Itest.ge.2) RpoS = 0.       ! Test
c     If(Itest.ge.3) Then            ! Test
c       Par(3) = 15.                 ! Test
c       Par(2) = 16.                 ! Test
c       Par(1) = Par(2)-ThSect*TgA   ! Test
c     EndIf                          ! Test

      CALL GSVOLU('SECT','TRD1',MEAIR,PAR,NPAR,IVOL)

* ---------------- Planes positionning in SECTOR -------------------

c     nnn=0                       ! instaled volumes counter
      Do 100 ir = 1,NtMa          ! loop over the planes

        RpoC = R0 + ThCham*ir       !  r - top ch & xe   x
        P01 = (RpoC-ThCham)*TgA     !  dX, finaly dx ch bot
        P02 = RpoC * TgA            !  dX, finaly dx ch top
        P03 = ZsizOut(ir)           !  dY, Finaly dZ ch
        P04 = ThCham / 2.           !  dZ, finaly dy ch

c       If(Itest.ge.3) Then         ! Test
c         P03 = 15.                 ! Test
c         P02 = 16.                 ! Test
c         P01 = P02-ThCham*TgA      ! Test
c       EndIf                       ! Test

* ------------------------------------------------------ Frame
      NPAR=4
      PAR(1) = P01  ! dX bot
      PAR(2) = P02  ! dX top
      PAR(3) = P03  ! dZ
      Par(4) = P04  ! dY
      zz = ThCham*ir-(ThCham+ThSect)/2.
      CALL GSPOSP(FR(ir),1,'SECT',0.,0.,zz,0,'ONLY',Par,Npar)
c     nnn=nnn+1
* ------------------------------------------------------ "Xenon"
      PAR(1) =  P02-ThickF-ThXe*TgA  ! dX bot Xe
      PAR(2) =  P02-ThickF           ! dX top Xe
      PAR(3) =  P03-ThickX           ! dZ
      Par(4) =  ThXe / 2.            ! dY
      zz = ThRad / 2.               ! position in ch
      CALL GSPOSP(AGAS(ir),1,FR(ir),.0,.0,zz,0,'ONLY',Par,Npar)
c     nnn=nnn+1
* ------------------------------------------------------ Radiator
      PAR(1) =  P01-ThickF           ! dX bot
      PAR(2) =  P02-ThickF-ThXe*TgA  ! dX top
      PAR(3) =  P03-ThickZ           ! dZ
      PAR(4) =  ThRad / 2.           ! dY
      zz = - ThXe / 2.               ! position in ch
      CALL GSPOSP(Ra(ir),1,FR(ir),.0,.0,zz,0,'ONLY',Par,Npar)
c     nnn=nnn+1
* ------------------------------------------------------ Support 1 (in RAD)
      PAR(3) =  Ysup
      CALL GSPOSP(S1(ir),1,RA(ir),.0,.0,.0,0,'ONLY',Par,Npar)
c     nnn=nnn+1
* ------------------------------------------------------ Gas(CO2/Air) in Supp1
      PAR(1) =  PAR(1) + ThickS*TgA
      PAR(2) =  PAR(2) - ThickS*TgA
      PAR(3) =  Par(3) - ThickS
      PAR(4) =  Par(4) - ThickS
      CALL GSPOSP(CO(ir),1,S1(ir),.0,.0,.0,0,'ONLY',Par,Npar)
c     nnn=nnn+1
* ------------------------------------------------------ Supp 2 (in Xe)
      PAR(1) =  P02 - ThickF - ThXe*TgA
      PAR(2) =  P02 - ThickF - dSup*TgA
      PAR(3) =  Ysup
      PAR(4) =  (ThXe - dSup) / 2.
      zz = - dSup / 2.
      CALL GSPOSP(S2(ir),1,AGAS(ir),.0,.0,zz,0,'ONLY',Par,Npar)
c     nnn=nnn+1
* ------------------------------------------------------ "Xenon2" in Supp2
      PAR(4) =  Par(4) - ThickS / 2.
      PAR(3) =  Par(3) - ThickS1
      PAR(2) =  Par(2) - ThickS*TgA
      zz = - ThickS / 2.
      CALL GSPOSP(XE2(ir),1,S2(ir),.0,.0,zz,0,'ONLY',Par,Npar)
c     nnn=nnn+1
* ------------------------------------------------------ "Xenon3" in Xenon
      PAR(1) =  P02 - ThickF - dSup*TgA
      PAR(2) =  P02 - ThickF
      PAR(3) =  Ysup
      PAR(4) =  dSup / 2.
      zz =  (ThXe-dSup) / 2.
      CALL GSPOSP(XE3(ir),1,AGAS(ir),.0,.0,zz,0,'ONLY',Par,Npar)
c     nnn=nnn+1
* ------------------------------------------------------ PCboard 3
      NPAR=3
      PAR(1) = P02 - ThickF - dSup*TgA
      PAR(2) =   4.45 / 2.
      Par(3) = ThickS / 2.
      zz = dSup / 2. - ThStr - ThSLU -ThickS / 2.
      CALL GSPOSP(PC3(ir),1,Xe3(ir),.0,.0,zz,0,'ONLY',Par,Npar)
c     nnn=nnn+1
* ------------------------------------------------------ PCboard 2
      PAR(1) = Par(1) + ThickS*TgA
      PAR(2) =  2.85 / 2.
      zz = zz  +  ThSLU
      CALL GSPOSP(PC2(ir),1,Xe3(ir),.0,.0,zz,0,'ONLY',Par,Npar)
c     nnn=nnn+1
* ------------------------------------------------------ Strip (PCB 1)
      PAR(1) = Par(1) + ThStr*TgA
      PAR(2) =  1.
      Par(3) = ThStr / 2.
      zz = dSup / 2. - ThStr  / 2.
      CALL GSPOSP(PC1(ir),1,Xe3(ir),.0,.0,zz,0,'ONLY',Par,Npar)
c     nnn=nnn+1
* ------------------------------------------------------------- Windows
* ------------------------------------------------------ 3 top
      PAR(1) =  P02 - ThickF
      PAR(2) =  P03 - ThickX
      PAR(3) =  ThWin / 2.
      zz = (ThXe - ThWin) / 2.
      CALL GSPOSP(W3(ir),1,AGAS(ir),.0,.0,zz,0,'ONLY',Par,Npar)
c     nnn=nnn+1
* ------------------------------------------------------ 2 mid
      PAR(1) =  P02 - ThickF - ThXe*TgA
      CALL GSPOSP(W2(ir),1,AGAS(ir),.0,.0,-zz,0,'ONLY',Par,Npar)
c     nnn=nnn+1
* ------------------------------------------------------ 1 bot
      PAR(1) =  P01 - ThickF
      PAR(2) =  P03 - ThickZ
      zz = - (ThRad - ThWin) / 2.
      CALL GSPOSP(W1(ir),1,Ra(ir),.0,.0,zz,0,'ONLY',Par,Npar)
c     nnn=nnn+1
      nn=0
* ------------------------------------------------------ Window Supports
c      SST = ((R0-ThXe+ThCham*ir)*TgA-ThickF)/4.!var.dist.between vert.w.s.str.
c      If(SST.lt.XWS) SST=XWS          ! (not practical case but ok for now)
c      P01 = (R0 + ThCham*(ir-1))*TgA  !  dX, finaly dx ch bot
      y = ( ZsizOut(ir) - ThickZ + Ysup ) / 2.

      Do ix = 1,4     ! 4 from the midle of chamber to the adge
        x = (ix-1)*Sst
        If( x + Sst/3. .GT. P01-ThickF .and. ix.ne.1 ) Go To 100
        Do ixs = 1,2     ! simmetry in x
          If( ix.ne.1.or.ixs.ne.2 ) Then
            xx = x
            If(ixs.eq.2) xx = -x 
            yy = y
            Do iz = 1,2 ! symmetry in z
              If(iz.eq.2) yy = -y
              PAR(2) = ( P03 - ThickZ - Ysup ) / 2.  !  Length
              PAR(1) = XWS / 2.                      !  Width
              PAR(3) = ThWS / 2.                     !  Height
              zz = ( ThRad - ThWS ) / 2.
              NN = NN + 1
              CALL GSPOSP(S1W(ir),NN,Ra(ir),xx,yy,zz,0,'ONLY',Par,Npar)
c             nnn=nnn+1
              zz = - zz + ThWin
              NN = NN + 1
              CALL GSPOSP(S1W(ir),NN,Ra(ir),xx,yy,zz,0,'ONLY',Par,Npar)
c             nnn=nnn+1
              PAR(1) = ThWS / 2.                     !  Width
              PAR(3) = HWS / 2.                      !  Height
              zz = ThWin / 2.
              NN = NN + 1
              CALL GSPOSP(S1W(ir),NN,Ra(ir),xx,yy,zz,0,'ONLY',Par,Npar)
c             nnn=nnn+1
            EndDo  ! iz
          EndIf
        EndDo      ! ixs
      EndDo        ! ix

  100 Continue     ! ir     end of loop over the arms

c     Write(6,*) '## of installed TEC volumes/sect:', nnn


      If(irot.lt.0.or.irot.gt.1111111111) irot=0 ! rot.matr.init.

* -------------------- Sectors positioning -------------------------

      NPAR=4
      Par(1) = R0         *Tga        !  dx, finaly dx sect bot
      Par(2) = (R0+ThSect)*TgA        !  dX, finaly dx sect top
      Par(3) = ZsizOut(NtMa)          !  dY, finaly dZ sect
      Par(4) = ThSect / 2.            !  dZ, finaly dy sect

c     If(Itest.ge.2) RpoS = 0.       ! Test
c     If(Itest.ge.3) Then            ! Test
c       Par(3) = 15.                 ! Test
c       Par(2) = 16.                 ! Test
c       Par(1) = Par(2)-ThSect*TgA   ! Test

c     EndIf                          ! Test


      Do iph = 1,Nsect  ! ------ Cycle through Phi
        dPhi = ( 2*iph + Iarmor +1 ) * AngS
c       If(Itest.ge.2) dPhi = 0.

C        Do i = 1,Narms  ! ------ Cycle through Arms
        Do i = FirstArm,LastArm  ! ------ Cycle through Arms
          If(i.eq.2) dphi = - dphi
*                      Rotation matrix parameters
          Th1 =  90.
          Ph1 = dPhi
          Th2 = 180.
          Ph2 =   0.
          Th3 =  90.
          Ph3 =  90. + dPhi
          Irot=Irot+1
          Call GSROTM(Irot,TH1,PH1,TH2,PH2,TH3,PH3)

          ise=iph+Nsect*(i-1)
          Xpos = - RpoS*Sind(dPhi) ! ch x-position
          Ypos =   RpoS*Cosd(dPhi) ! ch y-position

          if(FirstArm.eq.0)then
             CALL GSPOS ('SECT',ise,wTEC,Xpos,Ypos, 0.,irot,'ONLY')
          else
             CALL GSPOS ('SECT',ise,eTEC,Xpos,Ypos, 0.,irot,'ONLY')
          endif ! just in case there is ever a West Arm TEC

        EndDo  ! i    arm
      EndDo    ! iph  sector

      end
