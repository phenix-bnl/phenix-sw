c $Id: aer.f,v 1.11 2009/08/20 03:53:03 hpereira Exp $
c       File name : aer.f
c       ---------
c     Creation date : Mar 4, 2004

c     Author : Satoshi Takagi

c     Purpose : Set up of aerogel counter

c     Revisions:

c     Date        Name            Description
c     ----        ----            -----------
c     10/27/2005  M.Konno         Change for Run5 Configuration
c     10/30/2005  M.Konno         Change the center position of ARGL Volume


c structure is
c              EMCL
c               |
c               |
c    --------------------------
c    |     |  .......   |     |
c  A001  A002 ....... A159  A160

      subroutine create_name( index, tag, output )
      
      implicit none
            
      integer index
      character*1 tag
      character*4 output
      character*4 tmp
            
      write( tmp, '(i4)' ) index
      write( output, '(a4)' ) tmp(2:4)//tag
      end

      subroutine aer(full,nh)

        Implicit none

c       Formal Argument Declarations
c       ----------------------------

c       External Functions
c       ------------------

c       Global Declarations
c       -------------------
#include "guphnx.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "gcflag.inc"

c  need to access zebra to write parameters to FZOUT file

#include "fstore.inc"
#include "sublink.inc"
#include "fpalink.inc"

c       Local Declarations
c       ------------------

c       main geometry parameter file (phnx.par) segment


c --> Deifine the parameters for Aerogel Counter
c------------------------------------------------
      integer aer_hori,hori,aer_vert,vert
      integer aer_number1,aer_number2,hole_number,air_number,
     &        frame_number1,frame_number2,pamp_number1

      integer n60,n70,n80,n90,n100

      integer med_air
      integer mat_al,mat_g10,mat_aero,mat_myl,mat_gor,mat_mume, mat_pmt
      integer med_al,med_g10,med_aero,med_myl,med_gor,med_mume, med_pmt
      integer iMylar,iGore,iAir,iMume,iFr4,iFrame1,iFrame2,iFrame3,
     &        iPamp1,iPamp2

c size for the each volume

      real argl_dim_acc(3) /20.0, 100.0, 200.0/
      real box_dim_acc1(3) /6.150, 5.9250, 11.550/   ! Al small box volume
      real box_dim_acc2(3) /4.450, 5.9250, 22.650/   ! Al large box volume
      real mylar_dim_acc1(3) /6.110, 5.8450, 11.470/ ! Mylar sheet in Al small box
      real mylar_dim_acc2(3) /4.310, 5.8450, 11.470/ ! Mylar sheet in Al large box
      real gore_dim_acc1(3) /6.1050, 5.8350, 11.460/ ! Goretex sheet in Al small box
      real gore_dim_acc2(3) /4.3050, 5.8350, 11.460/ ! Goretex sheet in Al large box
      real window_dim_acc1(3) /0.0, 4.050, 0.0050/   ! Air hole at mylar in Al large box
      real window_dim_acc2(3) /0.0, 4.050, 0.0250/   ! Air hole at goretex in Al large box 
      real g10_dim_acc1(3) /0.10, 5.8450, 22.570/    ! G10 Plate
      real aer_dim_acc1(3) /6.080, 5.7850, 11.410/   ! Aerogel block in the counter
      real air_dim_acc1(3) /4.280, 5.7850, 11.410/   ! Air for integration in the Al box
      real air_dim_acc2(3) /4.270, 5.8450, 5.550/    ! Air for PMT in the Al box
      real mume_dim_acc1(3) /3.9750, 4.0, 4.0/       ! Mu_Metal Shield for PMT
      real pmt_dim_acc1(3) /3.60, 3.80, 1.50/        ! PMT Glass (large TUBE shape)
      real pmt_dim_acc2(5) /0.750, 3.60, 3.80, 2.3750, 2.5750/ ! PMT Glass (CONE shape) 
      real pmt_dim_acc3(5) /0.750, 2.3750, 2.5750, 3.60, 3.80/ ! PMT Glass (CONE shape) 
      real pmt_dim_acc4(3) /2.3750, 2.5750, 2.250/   ! PMT Glass (small TUBE shape)
      real bld_dim_acc1(3) /4.0, 3.750, 0.10 /       ! Bleeder Board for PMT
      real inn_dim_acc1(3) /0.50, 5.50, 0.150 /      ! FR4 inner plate 
      real out_dim_acc1(3) /2.550, 5.30, 0.150 /     ! FR4 outer plate 
      real frame_dim_fr4(3) /2.550, 63.550, 0.150/   ! FR4 plate for the frame
      real frame_dim_al1(3) /16.650, 1.9050, 0.150/  ! Al plate for the frame
      real frame_dim_al2(3) /16.650, 0.150, 1.7550/  ! Al plate for the frame
      real frame_dim_al3(3) ! Al plate for the frame
      real frame_dim_al4(3) ! Al plate for the frame
      real frame_dim_al5(3) ! Al plate for the frame
      real frame_dim_al6(3) ! Al plate for the frame
      real frame_dim_al7(3) /4.925, 1.9050, 0.150/  ! Al plate for the frame
      real frame_dim_al8(3) /4.925, 0.150, 1.7550/  ! Al plate for the frame
      real pamp_dim_al1(3) /0.150, 62.240, 5.090/  ! Al plate for the pre amp frame
      real pamp_dim_al2(3) /11.050, 62.240, 0.150/  ! Al plate for the pre amp frame
      real pamp_dim_al3(3) /3.60, 1.150, 4.750/  ! Al box for the preamp box
      real pamp_dim_air(3) /3.55, 1.10, 4.70/    ! Air block for the preamp box

c --> position for the each volume
      real argl_pos_acc(3)    ! position of mother volume for Aerogel Detector
      real box_pos_acc1(3)    ! position of Al small box
      real box_pos_acc2(3)    ! position of Al large box
      real g10_pos_acc1(3)    ! position of G10 plate in Al large box
      real mylar_pos_acc1(3)  ! position of Mylar sheet in Al small box
      real mylar_pos_acc2(3)  ! position of Mylar sheet in Al large box
      real gore_pos_acc1(3)   ! position of Goretex sheet in Al small box
      real gore_pos_acc2(3)   ! position of Goretex sheet in Al large box
      real window_pos_acc1(3) ! position of Air hole at Mylar in Al small box
      real window_pos_acc2(3) ! position of Air hole at Goretex in Al large box
      real aer_pos_acc1(3)    ! position of Aerogel in Al large box
      real air_pos_acc1(3)    ! position of Air for integration in Al large box
      real air_pos_acc2(3)    ! position of Air for PMT in Al large box
      real mume_pos_acc1(3)   ! position of Mu_MEtal Shield
      real pmt_pos_acc1(3)    ! position of PMT Glass
      real pmt_pos_acc2(3)    ! position of PMT Glass
      real pmt_pos_acc3(3)    ! position of PMT Glass
      real bld_pos_acc1(3)    ! position of Bleeder board
      real inn_pos_acc1(3)    ! position of FR4 for conection between frame and ACC
      real out_pos_acc1(3)    ! position of FR4 for conection between frame and ACC
      real frame_pos_fr4(3)   ! position of FR4 plate for the frame 
      real frame_pos_al1(3)    ! position of Al plate for the frame 
      real frame_pos_al2(3)    ! position of Al plate for the frame 
      real frame_pos_al3(3)    ! position of Al plate for the frame 
      real frame_pos_al4(3)    ! position of Al plate for the frame 
      real frame_pos_al5(3)    ! position of Al plate for the frame 
      real frame_pos_al6(3)    ! position of Al plate for the frame 
      real pamp_pos_al1(3)    ! position of Al plate for the frame of pre amp 
      real pamp_pos_al2(3)    ! position of Al plate for the frame of pre amp 
      real pamp_pos_al3(3)    ! position of Al plate for the frame of pre amp 
      real pamp_pos_al4(3)    ! position of Al box for the preamp box 
      real pamp_pos_air(3)    ! position of Air block for the preamp box 

c ---- name for the each volume ----
      character*4  boxNames(2),mylarNames(2),holeNames(2),goreNames(2),
     &             windowNames(2),aerNames(1),airNames(2),mumeNames(1),
     &             pmtNames(3),g10Names(1),bldNames(1),innNames(1),
     &             outNames(1),frm1Names(1),frm2Names(1),frm3Names(1),
     &             frm4Names(1),frm5Names(1),frm6Names(1),frm7Names(1),
     &             amp1Names(1),amp2Names(1),amp3Names(1),amp4Names(1),
     &             amp5Names(1)
      character*50 box_name(2),mylar_name(2),hole_name(2),gore_name(2),
     &             window_name(2),aer_name(1),air_name(2),mume_name(1),
     &             pmt_name(3),g10_name(1),bld_name(1),inn_name(1),
     &             out_name(1),frm1_name(1),frm2_name(1),frm3_name(1),
     &             frm4_name(1),frm5_name(1),frm6_name(1),frm7_name(1),
     &             amp1_name(1),amp2_name(1),amp3_name(1),amp4_name(1),
     &             amp5_name(1)

c ---- Aerogel ----
      real AAero(2) /28.09, 16.0/ ! mass number(Si, O)
      real ZAero(2) /14.0, 8.0/   ! atomic number(Si, O)
      real WAero(2) /1.0, 2.0/    ! SiO2
      real DAero(1) /5.0E-2/      ! Density (g/cm^3)

c ---- mu_metal (permalloy) ----
      real AMume(2) /58.69, 55.85/ ! mass number(Ni, Fe)
      real ZMume(2) /28.0, 26.0/   ! atomic number(Ni, Fe)
      real WMume(2) /0.78, 0.22/   ! Ni(78.0%) + Fe(22.0%) 
      real DMume(1) /8.580/        ! Density (g/cm^3)

c ---- G10 plate ----
      real AG10(4) /28.09, 16.0, 12.01, 1.01/ ! mass number(Si, O, C, H)
      real ZG10(4) /14.0, 8.0, 6.0, 1.0/      ! atomic number(Si, O, C, H)
      real WG10(4) /0.60, 1.60, 0.80, 1.60/   ! SiO2(60.0%) + C2H4O(40.0%)
      real DG10(1) /1.70/                     ! Density (g/cm^3)

c ---- Mylar ----
      real AMylar(3) /12.01, 1.01, 16.00/ ! mass number(C, H, O)
      real ZMylar(3) /6.0, 1.0, 8.0/      ! atomic number(C, H, O)
      real WMylar(3) /5.0, 4.0, 2.0/      ! C5H4O2
      real DMylar(1) /1.39/               ! Density (g/cm^3)

c ---- Borosilicate (PMT_Glass) ----
      real ABoro(4) /16.00, 28.09, 10.81, 22.99/ ! mass number(O, Si, B, Na)
      real ZBoro(4) /8.0, 14.0, 5.0, 11.0/       ! atomic number(O, Si, B, Na)
      real WBoro(4) /2.01, 0.80, 0.24, 0.10/     ! SiO2(80%)+B2O3(12%)+Na2O(5%)
      real DBoro(1) /2.230/                      ! Density (g/cm^3)

c ---- Goretex ----
      real AGoretex(2) /12.01, 19.00/ ! mass number(C, F)
      real ZGoretex(2) /6.0, 9.0/ ! atomic number(C, F)
      real WGoretex(2) /2.0, 4.0/ ! CF2=CF2
      real DGoretex(1) /2.20/ ! Density (g/cm^3)

c     AER layers

      integer aerLayers
      parameter (aerLayers = 160)     ! 80-> 160 (Run5, MK) 

      real aer_rin(aerLayers)         !/430.0, 450.0/
      real aer_rout(aerLayers)        !/434.0, 454.0/
      real aer_zHalfLength(aerLayers) !/125.0, 140.0/
      real aer_phimin(aerLayers)      !/-30.0, -30.0/
      real aer_phimax(aerLayers)      !/45.0, 45.0/
      real aer_zCenter(aerLayers)     !/aerLayers*0.0/
      real dim_aer(5)

      namelist /aer_par/
     $     aer_rin, aer_rout, aer_phimin, aer_phimax,
     $     aer_zCenter, aer_ZHalfLength
     

      character*4     v_m_name,v_i_name,set_id,namesv(3)

      integer nr,npar,nmed,ivolu,inull,nv,idtype,nbitsv(3),
     &     iaxis,nwpa,nwsa,iset,idet,IVAL, IROT, ioa
      CHARACTER*10 CHFORM

      integer*4 nh              !set before call in gugeom
      character*4 full          ! set before call in gugeom
      integer iLayer
      integer iPoint

c       /*--- null rotation ---*/

      real nul_rot(6) /90.0,0.0,90.0,90.0,0.0,0.0/
      real rot1, rot2, rot3, rot4, rot5, rot6, x_ang, y_ang
      real del_x,del_y
      INTEGER NMAT,ISVOL,IFIELD,NWBUF

      REAL FIELDM,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN, AAA(3), ZZZ(3),
     &     WMAT(3),UBUF(12),DDD

c   The following are used in GSDETH
c   Hit parameters will be position(3), energy loss, particle type, 
c   momentum components in the detector, path Length, and TOF

      integer nhAER
      parameter (nhAER = 17)
c       character*4 namesh(nhAER)/'POSX','POSY','POSZ','DELE','P_ID',
c      &   'MOMX', 'MOMY', 'MOMZ', 'LENG', 'TOF', 
c      &   'STEP', 'ETOT', 'CHARGE', 'MOMENTUM', 
c      &   'VERT_X', 'VERT_Y', 'VERT_Z'/

      character*8 namesh(nhAER)/'POSX','POSY','POSZ','DELE','P_ID',
     &   'MOMX', 'MOMY', 'MOMZ', 'LENG', 'TOF', 
     &   'STEP', 'ETOT', 'CHARGE', 'MOMENTUM', 
     &   'VERT_X', 'VERT_Y', 'VERT_Z'/

      integer*4 hitbits(nhAER) /nhAER*32/

c     default setting of offsets and gains

      REAL ORIGIN(nhAER) /1000.0,1000.0,1000.0,2*0.,3*1000.0,4*0.0,
     &                    1000.0,0.0,1000.0,1000.0,1000.0/       !offset
      REAL FACTOR(nhAER) /3*100.,1.E7,1.0,3*100000.0,100.0,
     &                    100000.0,1.E7,1.E7,100.0,1.E7,3*100/   !gain, path Length, and TOF

       
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun

     c

c   The above gains give
c              - 0.1 keV energy deposition resolution
c              - 0.1 mm position resolution
c              - 0.01 MeV/c momentum resolution
c              - 0.1 mm path resolution
c              - 0.1 ps for TOF

c    Executable code
c    ===============

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c BEGIN
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


c     Run-by-Run Geometry Setting Using RHICRUN


      aer_hori = 8 
      if(RHICRUN .ge. 5) then
         aer_hori = 16
      end if

      aer_vert = 10


c     Run4 Geometry

      n60  = 10
      n70  = 10
      n80  = 2
      n90  = 1
      n100 = 20

      frame_dim_al3(1) = 1.270
      frame_dim_al3(2) = 0.150
      frame_dim_al3(3) = 92.25

      frame_dim_al4(1) = 1.270
      frame_dim_al4(2) = 0.150
      frame_dim_al4(3) = 103.80

      frame_dim_al5(1) = 0.150
      frame_dim_al5(2) = 4.780
      frame_dim_al5(3) = 92.25

      frame_dim_al6(1) = 0.150
      frame_dim_al6(2) = 4.780
      frame_dim_al6(3) = 103.80


c     Run5 Geometry

      if(RHICRUN .ge. 5) then
      n60  = 18
      n70  = 20
      n80  = 2
      n90  = 2
      n100 = 40
      end if

      if(RHICRUN .ge. 5) then
      frame_dim_al3(1) = 1.270
      frame_dim_al3(2) = 0.150
      frame_dim_al3(3) = 196.35

      frame_dim_al4(1) = 1.270
      frame_dim_al4(2) = 0.150
      frame_dim_al4(3) = 196.35

      frame_dim_al5(1) = 0.150
      frame_dim_al5(2) = 4.780
      frame_dim_al5(3) = 196.35

      frame_dim_al6(1) = 0.150
      frame_dim_al6(2) = 4.780
      frame_dim_al6(3) = 196.35
      endif


c       Read the geometery file segment

      write( *,* ) 'aer - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = aer_par, err = 999 )

      CHFORM = '1I / 3F'        ! integer count, then all floating
      write(6,*)'CHFORM=',CHFORM
      call mzform('PARA',CHFORM,ioa) ! book characteristic

c     write the parameters to a zebra bank. later they will go to output file

      call mzbook(ixdiv_fr, lfa_PARA, lfa_PARA, 1,
     &            'PARA', 0, 0, 1+aerLayers*3, ioa, 0)
 
c  fill the bank
      iqf(lfa_para + 1) = aerLayers
      iPoint = 1
      do iLayer = 1,aerLayers
         qf(lfa_para + iPoint + 1) = argl_dim_acc(1)
         qf(lfa_para + iPoint + 2) = argl_dim_acc(2)
         qf(lfa_para + iPoint + 3) = argl_dim_acc(3)
         iPoint = iPoint + 3
      enddo 

c --> Define the parameter of material and medium for Aerogel Counter

c ---- Integer number of material ----
      mat_aero = 6000         ! Aerogel
      mat_g10  = mat_aero + 1 ! G10 plate
      mat_al   = mat_aero + 2 ! Aluminume
      mat_myl  = mat_aero + 3 ! Mylar
      mat_gor  = mat_aero + 4 ! Goretex
      mat_mume = mat_aero + 5 ! Mu_Metal
      mat_pmt  = mat_aero + 6 ! Borosilicate

c ---- Integer number of medium ----
      med_air  = 18           ! Air + field (low field in PC3 and beyond)
      med_aero = 7000         ! Aerogel
      med_g10  = med_aero + 1 ! G10 plate
      med_al   = med_aero + 2 ! Aluminume
      med_myl  = med_aero + 3 ! Mylar
      med_gor  = med_aero + 4 ! Goretex
      med_mume = med_aero + 5 ! Mu_Metal
      med_pmt  = med_aero + 6 ! Borosilicate

c ---- Set Parameter for medium ----
      ifield  = 0        ! magnetic field
      fieldm  = 0.0      ! max field
      tmaxfd  = 45.0     ! maximume angle due to field (one step) in degrees
      dmaxms  = 1.00     ! max disp. due to mulsct. in one step (cm)
      deemax  = 0.05     ! max fractional energy loss in one step
      epsil   = 0.01     ! tracking precision (cm)
      stmin   = 0.5      ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.       ! tracking stop switch
      nwbuf   =0      

c ---- Aerogel (SiO2) ----
      isvol   = 1        ! sensitive
      call gsmixt(mat_aero,'SiO2 $',AAero,ZAero,DAero,-2,WAero)
      call gstmed(med_aero,'Aerogel $',mat_aero,isvol,ifield,fieldm,
     &     tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c ---- Mumetal 
      isvol = 0
      call gsmixt(mat_mume,'Permalloy$',AMume,ZMume,DMume,-2,WMume)
      call gstmed(med_mume,'Mu_Metal $',mat_mume,isvol,ifield,fieldm,
     &            tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c ---- Borosilicate (PMT_Glass)
      isvol = 0
      call gsmixt(mat_pmt,'Borosilicate$',ABoro,ZBoro,DBoro,-4,WBoro)
      call gstmed(med_pmt,'PMT_Glass $',mat_pmt,isvol,ifield,fieldm,
     &            tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c ---- Mylar (C5H4O2) ----
      isvol = 0
      call gsmixt(mat_myl,'C2H5O2 $',AMylar,ZMylar,DMylar,-3,WMylar)
      call gstmed(med_myl,'Mylar_sheet $',mat_myl,isvol,ifield,fieldm,
     &            tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c ---- G10 plate (glass fiber) ----
      isvol = 0
      call gsmixt(mat_g10,'G10 $',AG10,ZG10,DG10,-4,WG10)
      call gstmed(med_g10,'G10_plte $',mat_g10,isvol,ifield,fieldm,
     &            tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c ---- Alminium ----
      isvol = 0
      call gsmate(mat_al,'Alminium $',26.98,13.0,2.70,8.90,37.2,UBUF,0)
      call gstmed(med_al,'Al $',mat_al,isvol,ifield,fieldm,
     &     tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c ---- Goretex
      isvol = 0
      call gsmixt(mat_gor,'CF2=CF2 $',AGoretex,ZGoretex,DGoretex,-2,
     &            WGoretex)
      call gstmed(med_gor,'Goretex $',mat_gor,isvol,ifield,fieldm,
     &            tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c  only book volumes if input parameters are OK

      IF(CVOLU_OPT(1,14).EQ.'FULL')then
        v_m_name = 'EMCL'
        set_id = 'AER '        ! put it in a SET
        nv = 3
        namesv(1) = 'HALL'
        namesv(3) = 'AERO'
        nbitsv(1) = 16
        nbitsv(2) = 16
        nbitsv(3) = 16
        NH = 8
        idtype = 2014
        nwpa = 8000             ! for now
        nwsa = 8000             ! for now
        nr = 1                 !copy number
        irot = 1               !rot matrix (NULL ROTATION)
        
c -->   Create & Put Volume 'ARGL' (aerogel mother volume)
        
        npar = 3
        nmed = med_air         ! Air + field (low field in PC3 and beyond)
        v_i_name = 'ARGL'      ! nmae of aer mother volume
        call gsvolu(v_i_name,'BOX ',nmed,argl_dim_acc,npar,ivolu)
        call gsatt(v_i_name,'SEEN',0)
        call gsatt(v_i_name,'WORK',1) ! Make volume sensitive
        
        argl_pos_acc(1) =  449.4140
        argl_pos_acc(2) =  10.6331
        
        argl_pos_acc(3) =  0.8290
              
        if(RHICRUN .ge. 5) then
          argl_pos_acc(3) = -0.4036
        endif
        
        call gspos(v_i_name,nr,v_m_name,argl_pos_acc(1),
     &    argl_pos_acc(2),argl_pos_acc(3),irot,'ONLY')
              
c ---- initial number for ACC volume ----

      aer_number1   = 1000     ! for the small part of counter
      aer_number2   = 1160     ! for the large part of counter
      hole_number   = 1000     ! for the Air hole
      air_number    = 1000     ! for the Air,the mumetal and the PMT_Glass
      frame_number1 = 1000
      
c --> Create the geometry of FR4 long plate for BOX 
      
      do 60 iFr4=1,n60
         frame_number1 = frame_number1 + 1
         nmed = med_g10         ! G10 
         npar = 3
         v_m_name = 'ARGL'
         call create_name( frame_number1, 'N', frm1Names(1) ); 
               
         call gsvolu(frm1Names(1),'BOX ',nmed,
     &      frame_dim_fr4,npar,ivolu)
         call gsatt(frm1Names(1),'SEEN',1)
         call gsatt(frm1Names(1),'COLO',6)
         
         frame_pos_fr4(1) = -14.10*((-1.0)**(real(iFr4)-1.0))
         frame_pos_fr4(2) = 2.390
         frame_pos_fr4(3) = 196.35-(real(iFr4)-1.0)*23.1
         call gspos(frm1Names(1),nr,v_m_name,
     &      frame_pos_fr4(1),frame_pos_fr4(2),frame_pos_fr4(3),
     &      irot,'ONLY')                  
 60    continue

c --> Create the geometry of Al plate like "L" shape  

        do 70 iFrame1=1,n70  ! 1-9(Box),10(PreAmp),11-19(Box),20(Preamp)
                
          frame_number1 = frame_number1 + 1 
          nmed = med_al
          npar = 3
          v_m_name = 'ARGL'
          call create_name( frame_number1, 'N', frm2Names(1) ); 
                
          call gsvolu(frm2Names(1),'BOX ',nmed,
     &      frame_dim_al1,npar,ivolu)
            
          call gsatt(frm2Names(1),'SEEN',1)
          call gsatt(frm2Names(1),'COLO',4)
                
          if(iFrame1.le.9)then
                  
            frame_pos_al1(1) = 0.0
            frame_pos_al1(2) = 64.0350
            frame_pos_al1(3) = 196.050 - (real(iFrame1)-1.0)*23.10
                  
          else if(iFrame1.eq.10)then
                  
            frame_pos_al1(1) = 0.0
            frame_pos_al1(2) = 64.0350
            frame_pos_al1(3) = 185.1150
                  
          else if(iFrame1.ge.11 .and. iFrame1.le.19)then
                  
            frame_pos_al1(1) = 0.0
            frame_pos_al1(2) = 64.0350
            frame_pos_al1(3) = 196.050 - (real(iFrame1)-2.0)*23.10 + 0.6
                  
          else if(iFrame1.eq.20)then
                
            frame_pos_al1(1) = 0.0
            frame_pos_al1(2) = 64.0350
            frame_pos_al1(3) = -185.1150
                  
          end if

          call gspos(frm2Names(1),nr,v_m_name,frame_pos_al1(1),
     &      frame_pos_al1(2),frame_pos_al1(3),irot,'ONLY')                  
            
            
            
          frame_number1 = frame_number1 + 1 
          nmed = med_al
          npar = 3
          v_m_name = 'ARGL'
                
          call create_name( frame_number1, 'N', frm3Names(1) )
          call gsvolu(frm3Names(1),'BOX ',nmed,
     &      frame_dim_al2,npar,ivolu)
                
          call gsatt(frm3Names(1),'SEEN',1)
          call gsatt(frm3Names(1),'COLO',4)
                
          if(iFrame1.le.9)then
                  
            frame_pos_al2(1) = 0.0
            frame_pos_al2(2) = 65.790
            frame_pos_al2(3) = 194.1450 - (real(iFrame1)-1.0)*23.10

          else if(iFrame1.eq.10)then
          
            frame_pos_al2(1) = 0.0
            frame_pos_al2(2) = 65.790
            frame_pos_al2(3) = 183.210
          
          else if(iFrame1.ge.11 .and. iFrame1.le.19)then
          
            frame_pos_al2(1) = 0.0
            frame_pos_al2(2) = 65.790
            frame_pos_al2(3) = 194.1450-(real(iFrame1)-2.0)*23.10+4.41

          else if(iFrame1.eq.20)then
            
            frame_pos_al2(1) = 0.0
            frame_pos_al2(2) = 65.790
            frame_pos_al2(3) = -183.210

          end if

          call gspos(frm3Names(1),nr,v_m_name,frame_pos_al2(1),
     &      frame_pos_al2(2),frame_pos_al2(3),irot,'ONLY')                  
                
 70     continue
          
c -->   Create the Al plate       
          
        do 80 iFrame2=1,n80
          
          frame_number1 = frame_number1 + 1 
          nmed = med_al
          npar = 3
          v_m_name = 'ARGL'
                
          call create_name( frame_number1, 'N', frm4Names(1) )
          call gsvolu(frm4Names(1),'BOX ',nmed,frame_dim_al3,npar,ivolu)
          call gsatt(frm4Names(1),'SEEN',1)
          call gsatt(frm4Names(1),'COLO',4)
          frame_pos_al3(1) = -8.070
          frame_pos_al3(2) = 66.090 + 9.860*(real(iFrame2)-1.0)
          
          frame_pos_al3(3) = 103.950
          
          if(RHICRUN .ge. 5) then
            
            frame_pos_al3(3) = 0.0
          
          endif

          call gspos(frm4Names(1),nr,v_m_name,frame_pos_al3(1),
     &      frame_pos_al3(2),frame_pos_al3(3),irot,'ONLY')
          
          frame_number1 = frame_number1 + 1 
          nmed = med_al
          npar = 3
          v_m_name = 'ARGL'
                
          ! subroutine create_name( index, tag, output )
          call create_name( frame_number1, 'N', frm5Names(1) ); 
          call gsvolu(frm5Names(1),'BOX ',nmed,frame_dim_al4,npar,ivolu)
          call gsatt(frm5Names(1),'SEEN',1)
          call gsatt(frm5Names(1),'COLO',4)
          frame_pos_al4(1) = 8.07
          frame_pos_al4(2) = 66.090 + 9.860*(real(iFrame2)-1.0) 
          
          frame_pos_al4(3) = 92.40
          
          if(RHICRUN .ge. 5) then
            frame_pos_al4(3) = 0.0
          endif

          call gspos(frm5Names(1),nr,v_m_name,
     &      frame_pos_al4(1),frame_pos_al4(2),frame_pos_al4(3),
     &      irot,'ONLY')
 80     continue

        frame_number1 = frame_number1 + 1 
        nmed = med_al
        npar = 3
        v_m_name = 'ARGL'
        call create_name( frame_number1, 'N', frm6Names(1) ); 
        call gsvolu(frm6Names(1),'BOX ',nmed,frame_dim_al5,npar,ivolu)
        call gsatt(frm6Names(1),'SEEN',1)
        call gsatt(frm6Names(1),'COLO',4)
        frame_pos_al5(1) = -6.950
        frame_pos_al5(2) = 71.020
        
        frame_pos_al5(3) = 103.950
        
        if(RHICRUN .ge. 5) then
          frame_pos_al5(3) = 0.0
        endif
        
        call gspos(frm6Names(1),nr,v_m_name,frame_pos_al5(1),
     &    frame_pos_al5(2),frame_pos_al5(3),irot,'ONLY')
          
        frame_number1 = frame_number1 + 1 
        nmed = med_al
        npar = 3
        v_m_name = 'ARGL'
              
        call create_name( frame_number1, 'N', frm7Names(1) )
        call gsvolu(frm7Names(1),'BOX ',nmed,frame_dim_al6,npar,ivolu)
        call gsatt(frm7Names(1),'SEEN',1)
        call gsatt(frm7Names(1),'COLO',4)
        frame_pos_al6(1) = 6.950
        frame_pos_al6(2) = 71.020
        
        frame_pos_al6(3) = 92.40
        
        if(RHICRUN .ge. 5) then
          frame_pos_al6(3) = 0.0
        endif
        
        call gspos(frm7Names(1),nr,v_m_name,frame_pos_al6(1),
     &    frame_pos_al6(2),frame_pos_al6(3),irot,'ONLY')
          
c -->   frame For pre-amp box 
          
        pamp_number1 = 1000

        do 90 iPamp1=1,n90

          pamp_number1 = pamp_number1 + 1 
          nmed = med_al
          npar = 3
          v_m_name = 'ARGL'
                
          call create_name( pamp_number1, 'P', amp1Names(1) )
          call gsvolu(amp1Names(1),'BOX ',nmed,pamp_dim_al1,npar,ivolu)
          call gsatt(amp1Names(1),'SEEN',1)
          call gsatt(amp1Names(1),'COLO',4)
          pamp_pos_al1(1) = 16.50 * ((-1.0)**( real(iPamp1) -1.0))
          pamp_pos_al1(2) = 2.9150
          pamp_pos_al1(3) = 190.3550 * ((-1.0)**( real(iPamp1) -1.0))
          call gspos(amp1Names(1),nr,v_m_name,pamp_pos_al1(1),
     &      pamp_pos_al1(2),pamp_pos_al1(3),irot,'ONLY')
            
          pamp_number1 = pamp_number1 + 1 
          nmed = med_al
          npar = 3
                
          v_m_name = 'ARGL'
          call create_name( pamp_number1, 'P', amp2Names(1) )
          call gsvolu(amp2Names(1),'BOX ',nmed,pamp_dim_al1,npar,ivolu)
          call gsatt(amp2Names(1),'SEEN',1)
          call gsatt(amp2Names(1),'COLO',4)
          pamp_pos_al2(1) = -5.9 * ((-1.0)**( real(iPamp1) -1.0))
          pamp_pos_al2(2) = 2.9150
          pamp_pos_al2(3) = 190.3550 * ((-1.0)**( real(iPamp1) -1.0))
          call gspos(amp2Names(1),nr,v_m_name,pamp_pos_al2(1),
     &      pamp_pos_al2(2),pamp_pos_al2(3),irot,'ONLY')               
            
          pamp_number1 = pamp_number1 + 1 
          nmed = med_al
          npar = 3
          v_m_name = 'ARGL'
          call create_name( pamp_number1, 'P', amp3Names(1) )
          call gsvolu(amp3Names(1),'BOX ',nmed,pamp_dim_al2,npar,ivolu)
          call gsatt(amp3Names(1),'SEEN',1)
          call gsatt(amp3Names(1),'COLO',4)
          pamp_pos_al3(1) = 5.30 * ((-1.0)**( real(iPamp1) -1.0))
          pamp_pos_al3(2) = 2.9150
          pamp_pos_al3(3) = 185.4150 * ((-1.0)**( real(iPamp1) -1.0))        
          call gspos(amp3Names(1),nr,v_m_name,pamp_pos_al3(1),
     &      pamp_pos_al3(2),pamp_pos_al3(3), irot,'ONLY')               
            
 90     continue

c --> pre-amp box

        do 100 iPamp2=1,n100
          pamp_number1 = pamp_number1 + 1 
          nmed = med_al
          npar = 3
          v_m_name = 'ARGL'
                
          call create_name( pamp_number1, 'P', amp4Names(1) )
          call gsvolu(amp4Names(1),'BOX ',nmed,pamp_dim_al3,npar,ivolu)
          call gsatt(amp4Names(1),'SEEN',1)
          call gsatt(amp4Names(1),'COLO',4)
          
          if(iPamp2.le.20)then
                  
            pamp_pos_al4(1) = 5.30
            pamp_pos_al4(2) = 52.480-(5.0*real(iPamp2))
            pamp_pos_al4(3) = 190.3150
                  
          else if(iPamp2.ge.21 .and. iPamp2.le.40)then
                  
            pamp_pos_al4(1) = -5.30
            pamp_pos_al4(2) = 52.480-(5.0* (real(iPamp2)-20.0))
            pamp_pos_al4(3) = -190.3150

          end if

          call gspos(amp4Names(1),nr,v_m_name,pamp_pos_al4(1),
     &      pamp_pos_al4(2),pamp_pos_al4(3),irot,'ONLY')               
            
          pamp_number1 = pamp_number1 + 1 
          nmed = med_air
          npar = 3
                
          v_m_name = amp4Names(1) 
                
          call create_name( pamp_number1, 'P', amp5Names(1) )
          call gsvolu(amp5Names(1),'BOX ',nmed,pamp_dim_air,npar,ivolu)
          call gsatt(amp5Names(1),'SEEN',1)
          call gsatt(amp5Names(1),'COLO',4)
          pamp_pos_air(1) = 0.0
          pamp_pos_air(2) = 0.0
          pamp_pos_air(3) = 0.0
          call gspos(amp5Names(1),nr,v_m_name,pamp_pos_air(1),
     &      pamp_pos_air(2),pamp_pos_air(3),irot,'ONLY')                              
 100    continue

c --> Define and place the volume of Aerogel counter box (Alminium)

      do 10 hori = 1, aer_hori 
         do 20 vert = 1, aer_vert
            aer_number1 = aer_number1 + 1
            aer_number2 = aer_number2 + 1
c ---- Al small box ----
            nmed = med_al       ! Aluminium
            npar = 3
            v_m_name = 'ARGL'
            call create_name( aer_number1, 'B', boxNames(1) )                  
            call gsvolu(boxNames(1),'BOX ',nmed,box_dim_acc1,3,ivolu)
            call gsatt(boxNames(1),'SEEN',1)
            box_pos_acc1(1) = 0.0
            box_pos_acc1(2) = 53.3250 - (real(vert) - 1.0) * 11.850
            box_pos_acc1(3) = 173.250 - (real(hori) - 1.0) * 23.10
            call gspos(boxNames(1),nr,v_m_name,box_pos_acc1(1),
     &                 box_pos_acc1(2),box_pos_acc1(3),irot,'ONLY')

c ---- Al large box ----
            call create_name( aer_number2, 'B', boxNames(2) )                  
            call gsvolu(boxNames(2),'BOX ',nmed,box_dim_acc2,npar,ivolu)
            call gsatt(boxNames(2),'SEEN',1)
            box_pos_acc2(1) = -10.60 * ((-1.0)**(real(hori) - 1.0))
            box_pos_acc2(2) = 53.3250 - (real(vert) - 1.0) * 11.850
            box_pos_acc2(3) = 173.250 - (real(hori) - 1.0) * 23.10
            call gspos(boxNames(2),nr,v_m_name,box_pos_acc2(1),
     &                 box_pos_acc2(2),box_pos_acc2(3),irot,'ONLY')

c --> Define and place the PC board in Al box

            nmed = med_g10      ! PC board
            npar = 3
            v_m_name = boxNames(2)
            call create_name( aer_number1, 'H', g10Names(1) )                  
            call gsvolu(g10Names(1),'BOX ',nmed,g10_dim_acc1,npar,ivolu)
            call gsatt(g10Names(1),'SEEN',1)
            call gsatt(g10Names(1),'COLO',1)
            g10_pos_acc1(1) = -4.270 * ((-1.0)**(real(hori)-1.0))
            g10_pos_acc1(2) = 0.0
            g10_pos_acc1(3) = 0.0
            call gspos(g10Names(1),nr,v_m_name,g10_pos_acc1(1),
     &                 g10_pos_acc1(2),g10_pos_acc1(3),irot,'ONLY')

c --> Define and place the Mylar Sheet in Al box

c ---- Mylar sheet in Al small box ---- 
            nmed = med_myl      ! Mylar
            npar = 3
            v_m_name = boxNames(1)
                  
            call create_name( aer_number1, 'M', mylarNames(1) )                  
            call gsvolu(mylarNames(1),'BOX ',nmed,
     &                  mylar_dim_acc1,npar,ivolu)
            call gsatt(mylarNames(1),'SEEN',1)
            mylar_pos_acc1(1) = -0.140 * ((-1.0)**(real(hori) - 1.0))
            mylar_pos_acc1(2) = 0.0
            mylar_pos_acc1(3) = 0.0
            call gspos(mylarNames(1),nr,v_m_name,mylar_pos_acc1(1),
     &                 mylar_pos_acc1(2),mylar_pos_acc1(3),irot,'ONLY')

c ---- Mylar sheet in Al large box ----
            v_m_name = boxNames(2)
            call create_name( aer_number2, 'M', mylarNames(2) )                  
            call gsvolu(mylarNames(2),'BOX ',nmed,
     &                  mylar_dim_acc2,npar,ivolu)
            call gsatt(mylarNames(2),'SEEN',1)
            mylar_pos_acc2(1) = -0.140 * ((-1.0)**(real(hori)))
            mylar_pos_acc2(2) = 0.0
            mylar_pos_acc2(3) = 0.0
            call gspos(mylarNames(2),nr,v_m_name,mylar_pos_acc2(1),
     &                 mylar_pos_acc2(2),mylar_pos_acc2(3),irot,'ONLY')

c --> Define and Place the Goretex sheet in Al box 

c ---- Goretex sheet in Al small box ----
            nmed = med_gor      ! Goretex
            npar = 3
            v_m_name = mylarNames(1)

            call create_name( aer_number1, 'G', goreNames(1) )                  
            call gsvolu(goreNames(1),'BOX ',nmed,
     &        gore_dim_acc1,npar,ivolu)
            call gsatt(goreNames(1),'SEEN',1)
            call gsatt(goreNames(1),'COLO',6)
            gore_pos_acc1(1) = -0.0050 * ((-1.0)**(real(hori) - 1.0))
            gore_pos_acc1(2) = 0.0
            gore_pos_acc1(3) = 0.0
            call gspos(goreNames(1),nr,v_m_name,gore_pos_acc1(1),
     &        gore_pos_acc1(2),gore_pos_acc1(3),irot,'ONLY')
              
c ----        Goretex sheet in Al large box ----
            nmed = med_gor      ! Goretex
            npar = 3
            v_m_name = mylarNames(2)
            call create_name( aer_number2, 'G', goreNames(2) )                  
            call gsvolu(goreNames(2),'BOX ',nmed,
     &        gore_dim_acc2,npar,ivolu)
            call gsatt(goreNames(2),'SEEN',1)
            call gsatt(goreNames(2),'COLO',6)
            gore_pos_acc2(1) = -0.0050 * ((-1.0)**(real(hori)))
            gore_pos_acc2(2) = 0.0
            gore_pos_acc2(3) = 0.0
            call gspos(goreNames(2),nr,v_m_name,gore_pos_acc2(1),
     &                 gore_pos_acc2(2),gore_pos_acc2(3),irot,'ONLY')
                      
c --> Define and Place the Aerogel block in Al box 

            nmed = med_aero     ! Aerogel
            npar = 3
            v_m_name = goreNames(1)
                                    
            ! for some reason reason A is pre-pended to index here.
            ! so that create_name cannot be used
            write(aer_name(1), '(i4)') aer_number1
            aerNames(1) = 'A'//aer_name(1)(2:4)
            call gsvolu(aerNames(1),'BOX ',nmed,
     &                  aer_dim_acc1,npar,ivolu)
            call gsatt(aerNames(1),'SEEN',1)
            call gsatt(aerNames(1),'COLO',7)
            aer_pos_acc1(1) = -0.0250 * ((-1.0)**(real(hori) - 1.0))
            aer_pos_acc1(2) = 0.0
            aer_pos_acc1(3) = 0.0
            call gspos(aerNames(1),nr,v_m_name,aer_pos_acc1(1),
     &                 aer_pos_acc1(2),aer_pos_acc1(3),irot,'ONLY')

c --> Define and place the Air block for integration in Al box

            nmed = med_air      ! Air
            npar = 3
            v_m_name = goreNames(2)

            call create_name( aer_number1, 'I', airNames(1) )                  
            call gsvolu(airNames(1),'BOX ',nmed,
     &        air_dim_acc1,npar,ivolu)
            call gsatt(airNames(1),'SEEN',1)
            call gsatt(airNames(1),'COLO',7)
            air_pos_acc1(1) = -0.0250 * ((-1.0)**(real(hori)))
            air_pos_acc1(2) = 0.0
            air_pos_acc1(3) = 0.0
            call gspos(airNames(1),nr,v_m_name,air_pos_acc1(1),
     &        air_pos_acc1(2),air_pos_acc1(3),irot,'ONLY')

c --> Define and Place the Air Hole in Mylar and Goretex

c ---- Air hole in Mylar for PMT ----
            do 30 iMylar = 1, 2
              hole_number = hole_number + 1
              nmed = med_air   ! Air
              npar = 3
              v_m_name = mylarNames(2)
              
              call create_name( hole_number, 'W', windowNames(1) )                  
              call gsvolu(windowNames(1),'TUBE',nmed,
     &          window_dim_acc1,npar,ivolu)
              call gsatt(windowNames(1),'SEEN',1)
              call gsatt(windowNames(1),'COLO',1)
              window_pos_acc1(1) = -0.040*((-1.0)**(real(hori)-1.0))  
              window_pos_acc1(2) = 0.0
              window_pos_acc1(3) = -11.465*((-1.0)**(real(iMylar))) 
              call gspos(windowNames(1),nr,v_m_name,window_pos_acc1(1),
     &          window_pos_acc1(2),window_pos_acc1(3),irot,'ONLY')           
 30         continue
                  
c -----------------------------------
c ---- Air hole in Goretex for PMT ----
                  
            do 40 iGore = 1, 2
              hole_number = hole_number + 1
              nmed = med_air   ! Air
              npar = 3
              v_m_name = goreNames(2)
              call create_name( hole_number, 'W', windowNames(2) )                  
              call gsvolu(windowNames(2),'TUBE',nmed,
     &          window_dim_acc2,npar,ivolu)
              call gsatt(windowNames(2),'SEEN',1)
              call gsatt(windowNames(2),'COLO',1)
              window_pos_acc1(1) = -0.045*((-1.0)**(real(hori)-1.0))  
              window_pos_acc1(2) = 0.0
              window_pos_acc1(3) = -11.435 * ((-1.0)**(real(iGore))) 
              call gspos(windowNames(2),nr,v_m_name,window_pos_acc1(1),
     &          window_pos_acc1(2),window_pos_acc1(3),irot,'ONLY')           
 40         continue

c --> Define and place the Air block, the mumetal shield and PMT Glass for PMT

            do 50 iAir = 1, 2
              air_number = air_number + 1
c ----        Air block ----
              nmed = med_air   ! Air
              npar = 3
              v_m_name = boxNames(2)

              call create_name( air_number, 'C', airNames(2) )                  
              call gsvolu(airNames(2),'BOX ',nmed,
     &          air_dim_acc2,npar,ivolu)
                    
              call gsatt(airNames(2),'SEEN',1)
              call gsatt(airNames(2),'COLO',1)
              air_pos_acc2(1) = -0.10*((-1.0)**real(hori))  
              air_pos_acc2(2) = 0.0
              air_pos_acc2(3) = -17.020*((-1.0)**real(iAir))
              call gspos(airNames(2),nr,v_m_name,air_pos_acc2(1),
     &          air_pos_acc2(2),air_pos_acc2(3),irot,'ONLY')           

c ---- Mumetal shield ----
              nmed = med_mume  ! Mu_Metal
              npar = 3
              v_m_name = airNames(2)
              call create_name( air_number, 'S', mumeNames(1) )                  
              call gsvolu(mumeNames(1),'TUBE',nmed,
     &          mume_dim_acc1,npar,ivolu)
              call gsatt(mumeNames(1),'SEEN',1)
              call gsatt(mumeNames(1),'COLO',3)
              mume_pos_acc1(1) = 0.0  
              mume_pos_acc1(2) = 0.0
              mume_pos_acc1(3) = -1.55 * ((-1.0)**(real(iAir)-1.0)) 
              call gspos(mumeNames(1),nr,v_m_name,mume_pos_acc1(1),
     &          mume_pos_acc1(2),mume_pos_acc1(3),irot,'ONLY')           
                
c ----          PMT Glass ----
              nmed = med_pmt   ! Borosilicate (PMT Glass)
              npar = 3
              v_m_name = airNames(2)
              
              call create_name( air_number, 'D', pmtNames(1) )                  
              call gsvolu(pmtNames(1),'TUBE',nmed,
     &          pmt_dim_acc1,npar,ivolu)
              
              call gsatt(pmtNames(1),'SEEN',1)
              call gsatt(pmtNames(1),'COLO',4)
              pmt_pos_acc1(1) = 0.0  
              pmt_pos_acc1(2) = 0.0
              pmt_pos_acc1(3) = -4.05 * ((-1.0)**(real(iAir)-1.0)) 
              call gspos(pmtNames(1),nr,v_m_name,pmt_pos_acc1(1),
     &          pmt_pos_acc1(2),pmt_pos_acc1(3),irot,'ONLY')                     
                
              nmed = med_pmt   ! Borosilicate (PMT Glass)
              npar = 5
              v_m_name = airNames(2)
              call create_name( air_number, 'E', pmtNames(2) )                  
              if(iAir.eq.1)then

                call gsvolu(pmtNames(2),'CONE',nmed,
     &            pmt_dim_acc2,npar,ivolu)
                call gsatt(pmtNames(2),'SEEN',1)
                call gsatt(pmtNames(2),'COLO',4)
                pmt_pos_acc2(1) = 0.0  
                pmt_pos_acc2(2) = 0.0
                pmt_pos_acc2(3) = -1.80
              
              else if(iAir.eq.2)then
              
                call gsvolu(pmtNames(2),'CONE',nmed,
     &            pmt_dim_acc3,npar,ivolu)
                call gsatt(pmtNames(2),'SEEN',1)
                call gsatt(pmtNames(2),'COLO',4)
                pmt_pos_acc2(1) = 0.0  
                pmt_pos_acc2(2) = 0.0
                pmt_pos_acc2(3) = 1.80
              
              end if

              call gspos(pmtNames(2),nr,v_m_name,pmt_pos_acc2(1),
     &          pmt_pos_acc2(2),pmt_pos_acc2(3),irot,'ONLY')         
                
              nmed = med_pmt   ! Borosilicate (PMT Glass)
              npar = 3
              v_m_name = airNames(2)
                    
              call create_name( air_number, 'F', pmtNames(3) )                  
              call gsvolu(pmtNames(3),'TUBE',nmed,
     &          pmt_dim_acc4,npar,ivolu)
              call gsatt(pmtNames(3),'SEEN',1)
              call gsatt(pmtNames(3),'COLO',4)
              pmt_pos_acc3(1) = 0.0  
              pmt_pos_acc3(2) = 0.0
              pmt_pos_acc3(3) = -1.20 * ((-1.0)**(real(iAir))) 
              call gspos(pmtNames(3),nr,v_m_name,pmt_pos_acc3(1),
     &          pmt_pos_acc3(2),pmt_pos_acc3(3),irot,'ONLY')                     
                
c ----          Bleeder Board ----
              nmed = med_g10   ! G10 
              npar = 3
              v_m_name = airNames(2)
        
              call create_name( air_number, 'J', bldNames(1) )                  
              call gsvolu(bldNames(1),'BOX ',nmed,
     &          bld_dim_acc1,npar,ivolu)
              call gsatt(bldNames(1),'SEEN',1)
              call gsatt(bldNames(1),'COLO',4)
              bld_pos_acc1(1) = 0.0  
              bld_pos_acc1(2) = 0.0
              bld_pos_acc1(3) = -4.550 * ((-1.0)**(real(iAir))) 
              call gspos(bldNames(1),nr,v_m_name,bld_pos_acc1(1),
     &          bld_pos_acc1(2),bld_pos_acc1(3),irot,'ONLY')             
                
c ---- FR4 for the conction between frame and counter (inner parts) ----
              nmed = med_g10   ! G10 
              npar = 3
              v_m_name = airNames(2)
              call create_name( air_number, 'K', innNames(1) )                  
              call gsvolu(innNames(1),'BOX ',nmed,
     &          inn_dim_acc1,npar,ivolu)
              call gsatt(innNames(1),'SEEN',1)
              call gsatt(innNames(1),'COLO',4)
              inn_pos_acc1(1) = -3.050*(-1.0*((-1.0)**(real(hori))))  
              inn_pos_acc1(2) = 0.0
              inn_pos_acc1(3) = -5.40*((-1.0)**(real(iAir))) 
              call gspos(innNames(1),nr,v_m_name,inn_pos_acc1(1),
     &          inn_pos_acc1(2),inn_pos_acc1(3),irot,'ONLY')                  
                
c ----          FR4 for the conction between frame and counter(outer parts) ----
              nmed = med_g10   ! G10 
              npar = 3
              v_m_name = 'ARGL'
              
              call create_name( air_number, 'L', outNames(1) )                  
              call gsvolu(outNames(1),'BOX ',nmed,
     &          out_dim_acc1,npar,ivolu)
              call gsatt(outNames(1),'SEEN',1)
              call gsatt(outNames(1),'COLO',4)
              out_pos_acc1(1) = -14.1*((-1.0)**(real(hori)-1.0))
              out_pos_acc1(2) = 53.325-(real(vert)-1.0)*11.85
              out_pos_acc1(3) = 173.25-(real(hori)-1.0)*23.1
     &          - 22.80*((-1.0)**(real(iAir)))
              call gspos(outNames(1),nr,v_m_name,out_pos_acc1(1),
     &          out_pos_acc1(2),out_pos_acc1(3),irot,'ONLY')                  
 50         continue

c --> put volume elements together into a set and Annn in set 'AER '

            v_i_name = aerNames(1)
            namesv(2) = v_i_name
            call gsdet(set_id,v_i_name,nv,namesv,nbitsv,idtype,nwpa,
     &        nwsa,iset,idet)
            call gsdeth(set_id,v_i_name,nhAER,namesh,hitbits,
     &        origin,factor)
 20       continue
 10     continue
c --> end the loop for the individual Aerogel Counter

      endif     ! check on volume character

 9999 continue
      return

 999  continue
      write(6,1000)

 1000 format(/'aer - read error in aer_par segment')
      stop 'aer - namelist mis-match in aer_par segment'
      end
