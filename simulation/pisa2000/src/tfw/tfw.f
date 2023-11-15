c       File name: tfw.f
C       ---------

C     Original author: Charles F. Maguire
C     Creation date: August 17, 2006
C     Modified to match the geometry of TOF-W by Shengli Huang, Ivan Danchev and Julia Velkovska on Oct 9, 2007 
C     Revised comment section inserted on October 19, 2007
C=============================================================================================================================================


C*********************************************************************************************************************************************      

C   The EMCL is the mother volume (in HALL) which contains also PC2 and PC3

C   The TFWB (TOF-W Box) is the Aluminum panel  
C   Two copies of this panel at placed at a distance R (R=477.606, read from phnx.par file ) on the (PHENIX) x-axis facing the origin
C   Two other copies are raised at the same distance at angle phi (22.50 read from phnx.par) again facing origin


C                                 /\
C                                /  \
C                               / T / 
C                              / F / 
C                             / W /
C                            / B /\
C                            \  /  \
C                             \/    \
C                                    \  477.606 
C                     ___             \
C                    |   |             \
C                    | T |  22.50 deg   \  Vertex
C                    | F |_______________O
C                    | W |  477.606
C                    | B | 
C                    |___|



C=======================================================================================================================

c                                 Hierarchical structure of embedded volumes inside a panel

c                                                     TFWB  4

c                                                    /    \
c                                                   /      \
 
c                                                BOXG 1    BOTB 32

c                                              /    |   \
c                                             /     |    \

c                                         INBA 16  INBB 16  CHMB 32                                                                
c                                           |       |       \
c                                           |       |        \
c                                           |       |         \
c                                         AOTB 1  AOTB 1  --------------------------------
c                                                        |                                |
c                                                        | THEX 2  TSTR 8  TPCB 2  TMLA 2 |  
c                                                        |                                |
c                                                        | TELE 2  TOGL 2  TIGL 5  RPCG 6 |
c                                                        |                                |
c                                                        ---------------------------------


C=========================================================================================================================

C     Volume definitions in hierarchal order Mother>Daughter..character tfwPanel, boxgas,rpcgas,insblk,outblk,chamberblk

c      character*4 tfwPanel   /'TFWB'/            !TOF-W Box
c      character*4 boxgas     /'BOXG'/            ! inactive gas inside box

c      character*4 rpcgas(6)  /'RPG1','RPG2','RPG3' 'RPG4','RPG5','RPG6'/ ! the active gas in the gap between float glass plates

c      character*4 insblk     /'INSB'/            ! pedestal block
c      character*4 aoutblk    /'AOTB'/            ! air bulk in the boxgas
c      character*4 boutblk    /'BOTB'/            ! air bulk take place of the tfwbox
c      character*4 chamberblk /'CHMB'/            ! chamber bulk
c      character*4 tinsgla    /'TIGL'/            ! inside glass
c      character*4 toutgla    /'TOGL'/            ! outside glass
c      character*4 telect     /'TELE'/            ! electronode
c      character*4 tmylar     /'TMLA'/            ! mylar layers
c      character*4 tpcb       /'TPCB'/            ! pcb board
c      character*4 tstrip     /'TSTR'/            ! strip
c      character*4 thexcell   /'THEX'/            ! hexcell

C==========================================================================================================================

      
      SUBROUTINE TFW(FULL,NH)

      Implicit none

C     Formal Argument Declarations
C       ----------------------------

C     External Functions
C       ------------------

C     Global Declarations
C       -------------------
#include "guphnx.inc"
#include "gugeom.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "gcflag.inc"

C  need to access zebra to write parameters to FZOUT file

#include "fstore.inc"
#include "sublink.inc"
#include "fpwlink.inc"


C     Local Declarations
C     ------------------

      integer firstCall /1/
      save firstCall            ! redundant use of save since variable was initialized

c     TOF-West panels default values      
      integer nPanels       /4/
      real tfw_Length(4)    /4*192.405/                  ! (75 + 0.76 ) inch = 193.946 cm
      real tfw_Width(4)     /4*92.639/                   ! (35.722+0.75)inch = 93.378 cm
      real tfw_Thick(4)     /4*7.468/                    !  2*1.47 inch        =2*3.763 cm =7.526 cm both Up and Down panels
      real azym(4)          /0.00,0.00,22.75,22.75/      !  azymuthal angle of the bottom and top Al panels
      real R                /481.367/                    !  = (186.564+1.47)inch distance to midpoint of front an back panels
                                                            
c..    Define 'namelist' structure of dimensions of TFW to be read in from 'phnx.par'.                                           

      namelist /tfw_par/nPanels, R, tfw_Length, tfw_Width,
     $                  tfw_Thick, azym


C     Volume definitions in hierarchal order Mother>Daughter..character tfwPanel, boxgas,rpcgas,insblka,insblkb,aoutblk,...
      character*4 tfwPanel   /'TFWB'/              !TOF-W Box
      character*4 boxgas     /'BOXG'/              !inactive gas inside box
      character*4 rpcgas(6)     /'RPG1','RPG2','RPG3',
     & 'RPG4','RPG5','RPG6'/                       !the active gas betweeen gap
      character*4 insblka     /'INBA'/             !pedestal blk bottom
      character*4 insblkb     /'INBB'/             !pedestal blk up
      character*4 aoutblk    /'AOTB'/              !air blk in the pedestal blk
      character*4 boutblk    /'BOTB'/              !air blk take place of the tfwbox's wall to make a hole
      character*4 chamberblk /'CHMB'/              !chamber blk
      character*4 tinsgla     /'TIGL'/             !inside glass
      character*4 toutgla     /'TOGL'/             !outside glass
      character*4 telect      /'TELE'/             !electronode
      character*4 tmylar      /'TMLA'/             !mylar layers
      character*4 tpcb        /'TPCB'/             !pcb board
      character*4 tstrip      /'TSTR'/             !strip
      character*4 thexcell    /'THEX'/             !hexcell

C     dim_ NAME block definition REDUNDANT , one really needs just one 5 dim array and one 3 dim array
      real dim_tfw(3)                  
      real dim_boxgas(3) /6.80,88.824,188.595/
      real dim_aoutblk(3) /2.210,29.820,8.230/
      real dim_boutblk(3) /0.334,29.820,8.230/
      real dim_insblk(3) /2.54,30.48,8.89/
      real dim_chamberblk(3) /3.163,41.000,13.000/
 
      real dim_rpcgas(3) /0.023, 37.00, 11.20/   ! 6
      real dim_tinsgla(3) /0.055, 37.00, 11.20/   ! 5 need the length of ...
      real dim_toutgla(3) /0.110, 37.00, 11.50/   ! 2
      real dim_telect(3)  /0.090, 37.00, 11.20/    ! 2
      real dim_tmylar(3)  /0.025, 37.00, 11.20/    ! 2
      real dim_tpcb(3)    /0.150, 37.00, 13.00/       ! 2
      real dim_tstrip(3)   /0.05, 37.00, 2.81/       ! 2x6
      real dim_thexcell(3)  /0.95, 37.00, 12.00/ ! 2

      real blk_gap /22.375/     ! the distance bwtween two blks(inside/outside/chamber)
      real blk_z /13.665/       !3.63+3.50/2.0(inch) the center of blk to the edge of box
      real blk_y /20.500/       ! the center of blk to the center of box
      real gpos(3)
      real ydisp /-11.669/                       !/-11.669/
      real dim1 /63.429/                ! a characteristic panel dimension cm = (26.972 - 2.00 inch on pdf drawing)  // old 69.048
      real ph1 /32.1 /          ! characteristic panel angle in degrees
      real ph2,ph3,ph4,x1, y1, Rt ! chacteristic top panel angles and lengths,
                                          ! Rt is the (calculated) distance from the beam vertex to the center of top panel

C     for metail definiction 
      INTEGER n_electro
      INTEGER n_mylar
      INTEGER n_glass
      INTEGER n_boxgas
      INTEGER n_rpcgas 
      INTEGER n_hexcell
      INTEGER n_strips
      INTEGER n_mother_board

      REAL A, Z, DENS, RADL, ABSL, UBUF
      INTEGER IMATE, NWBUF
      INTEGER NLMAT
      REAL AGAS(3), ZGAS(3), WGAS(3) 
      REAL AGLASS(4), ZGLASS(4), WGLASS(4)
      REAL AMYLAR(3), ZMYLAR(3),WMYLAR(3)
      INTEGER IFIELD
      REAL FIELDM, TMAXFD, DMAXMS, DEEMAX, EPSIL, STMIN

      character*4 set_id,namesv(3)

      integer nr,npar,ivolu,inull,nv,idtype,nbitsv(3),
     &     iaxis,nwpa,nwsa,iset,idet,IVAL, iod
      CHARACTER*10 CHFORM

      integer*4 nh              ! set before call in gugeom
      character*4 full          ! set before call in gugeom
      integer iPanel
      integer iPanr(4)
      integer iPoint
      character*50 par_file

c.    Miscellaneous variables/constants
      REAL pih  /1.570796327/            ! pi/2
      REAL xPan(4),yPan(4),zPan(4)       ! coordinates of the origin of the Al panel in EMCL, the Master Reference Frame (MRF)  
      REAL T1, T2, T3, F1, F2, F3        ! polar (T) and azimuthal (F) angles defining the orientation of Al panel in the EMCL
      INTEGER L 

      
      integer ndim
      integer iblk
            


c   The following are used in GSDETH
c   Hit parameters will be local (x,y,z) for entrance and exit (1-6)
c                          tof, particle type, energy loss, (7-9)
c                          global (x,y,z) for entrance, and path length (10-12)



c     detector stored hit information

      character*4 nameshTFW(13) /'X1  ','Y1  ','Z1  ',
     &                        'X2  ','Y2  ','Z2  ', 
     &                        'TOF ', 'PTID', 'DELE', 
     &                        'X1GL', 'Y1GL', 'Z1GL', 'PTHL'/
      integer*4 nbitshTFW(13) /13*32/


c     default setting of offsets and gains for stored hits

      real origTFW(13) /6*1000., 3*0., 3*1000., 0./
      real factTFW(13) /7*1000., 1., 5*1000./
       
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun


C    Executable code
C    ===============

c.. - - - - - - - - MEDIA DECLARATIONS- - - - - - - - - - - - - - - - - - - - - - - - 

         
        n_electro=784            !CARBON material ,        ITMED=17, NMAT=6 in PISA 
        n_strips=785             !READ OUR strips made of Copper; ITMED=15, NMAT=11 in PISA see below
        n_mylar=786             !MYLAR plates ,           NOT SENSITIVE  
	n_mother_board = 787	!Electronics (Mother board+Chips+Connectors) G10 for now  NOT SENSITIVE 
	n_glass=788             !Float Glass              NOT SENSITIVE 
	n_hexcell = 789         !Noneycomb  HEXCELL       NOT SENSITIVE 
	n_boxgas=790	        !Box Gas freon/R134a      NOT SENSITIVE 
        n_rpcgas=791	        !RPC Gas freon/R134a      !!!!!!!!!!!!!!!!  SENSITIVE 



C.. ------------------- DEFINE USER MATERIALS ----------------------

C.. HEXCELL - the core material for both the chevron/pixel and ground
*             cathode panels. Its density = 0.024 g/cm*3 and radiation
*             length = 81.7 m are deduced from the data used by GEM with the 

*             assumption that the radiation is emitted from a point source 
*             into a cone of 6 degrees with respect to its central axis, 
*             incident upon the hexcell (facing the holes) of infinite extent.
C.. Mother Board + chips + connectors
*   Take G10 PLATE for now 
      IMATE = n_mother_board
      A = 18.14
      Z = 9.065
      DENS = 1.7
      RADL = 19.4
      ABSL = 56.7
      NWBUF = 1
      CALL GSMATE(IMATE,'MOTHER_BOARD$',A,Z,DENS,RADL,ABSL,UBUF,NWBUF)    

C.. hexcell
      IMATE = n_hexcell
      A = 12.01 
      Z = 6.
      DENS = 0.024
      RADL = 8170.
      ABSL = 99999.
      NWBUF = 1
      CALL GSMATE(IMATE,'HEXCELL$',A,Z,DENS,RADL,ABSL,UBUF,NWBUF)    

C.. MYLAR (POLYETHYLENE TEREPHTHALATE) (NOT USED, SEE NOTE in GSTMED assigments further on)
*http://en.wikipedia.org/wiki/Polyethylene_terephthalate
* http://physics.nist.gov/cgi-bin/Star/compos.pl?matno=171

C NOTE MYLAR is already defined in PISA: NMAT=920
      IMATE = n_mylar
*   Atomic weights of Hydroden(H), Oxygen(O), Carbon(C):
      AMYLAR(1) = 1.008 
      AMYLAR(2) = 12.011           
      AMYLAR(3) = 15.999                       
*   Mass numbers of Hydroden(H), Oxygen(O), Carbon(C):
      ZMYLAR(1) = 1.0           
      ZMYLAR(2) = 6.0             
      ZMYLAR(3) = 8.0             

c   The 6th input parameter in GSMIXT is NLMAT = 3 ! which is > 0, hence " WMAT contains the proportion by weights of each material in the mixture;"
*   Proportions are taken from the above website; CHECK !!! verify by calculation !!
      WMYLAR(1) = 0.041959          
      WMYLAR(2) = 0.625017
      WMYLAR(3) = 0.333025

      DENS = 1.4     ! quoted in website  
      CALL GSMIXT(IMATE,'MYLAR$',AMYLAR,ZMYLAR,DENS,3,WMYLAR)


C.. GLASS 
*   Take G10 PLATE for now / Try also Plate glass dens=2.4
* Since exact composition of Float Glass was unknown from the manufacturer
* we have assumed that it is not much different from the chemical
* composition of ordinary Glass Plate found here:
* http://physics.nist.gov/cgi-bin/Star/compos.pl?matno=171

      IMATE = n_glass
*   Atomic weights of Oxygen(O), Sodium(Na), Silicon(Si), Calcium(Ca):
      AGLASS(1) = 15.999           
      AGLASS(2) = 22.011           
      AGLASS(3) = 28.085           
      AGLASS(4) = 40.078
*   Mass numbers of Oxygen(O), Sodium(Na), Silicon(Si), Calcium(Ca):
      ZGLASS(1) = 8.0           
      ZGLASS(2) = 11.0             
      ZGLASS(3) = 14.0             
      ZGLASS(4) = 20.0

c      NLMAT = 4 ! which is > 0, hence " WMAT contains the proportion by weights of each material in the mixture;"
*   Proportions: 0.4598:0.0964:0.3365:0.1072 taken from website; CHECK !!! verify by calculation !!
      WGLASS(1) = 0.4598           
      WGLASS(2) = 0.0964
      WGLASS(3) = 0.3365
      WGLASS(4) = 0.1072

      DENS = 2.4     ! quoted in website  
      CALL GSMIXT(IMATE,'FLOAT_GlASS$',AGLASS,ZGLASS,DENS,4,WGLASS)
 

C.. box gas (% by volume) : 95% tetrafluoroethane, HFC-134a, CH2 FCF3 and 5% isobutene, i.e. C4H8

      IMATE=n_boxgas
*   Atomic weights of H, C, and F:
      AGAS(1) = 1.008           
      AGAS(2) = 12.011           
      AGAS(3) = 18.9984           

*   Mass numbers of H, C, and F:
      ZGAS(1) = 1.0            
      ZGAS(2) = 6.0             
      ZGAS(3) = 9.0             


c      NLMAT = 3 ! which is > 0, hence " WMAT contains the proportion by weights of each material in the mixture;"
      NLMAT =-1  ! which is <0, hence  " WMAT contains the proportion by number of atoms of each kind, 
c                ! the content of  WMAT in output is changed to contain the relative weights;
*   Proportions: 95% 2:2:4 + 5% 8:4:?? (no F)
C   (.95*2+.05*8):(.95*2+0.05*4):(1.0*4)= 2.3:2.1:4
      WGAS(1) = 0.2738                                        !  old  (i.e. with R134a only) is 0.250            
      WGAS(2) = 0.250                                         !  old  0.250
      WGAS(3) = 0.4762                                        !  old  0.500

      DENS =(0.95*102.03+0.05*56.108)/22400.0       ! = 99.7339/22400.0 compared to 102.03/22400.0 with  R134a only
      CALL GSMIXT(IMATE,'CHAMBER_GAS$',AGAS,ZGAS,DENS, NLMAT,WGAS)

C.. rpc gas (% by volume) : 95% tetrafluoroethane, HFC-134a, CH2 FCF3 and 5% isobutene, i.e. C4H8

      IMATE=n_rpcgas
*   Atomic weights of H, C, and F:
      AGAS(1) = 1.008           
      AGAS(2) = 12.011           
      AGAS(3) = 18.9984           

*   Mass numbers of H, C, and F:
      ZGAS(1) = 1.0            
      ZGAS(2) = 6.0             
      ZGAS(3) = 9.0             


c      NLMAT = 3 ! which is > 0, hence " WMAT contains the proportion by weights of each material in the mixture;"
      NLMAT =-1  ! which is <0, hence  " WMAT contains the proportion by number of atoms of each kind, 
c                ! the content of  WMAT in output is changed to contain the relative weights;
*   Proportions: 95% 2:2:4 + 5% 8:4:?? (no F)
C   (.95*2+.05*8):(.95*2+0.05*4):(1.0*4)= 2.3:2.1:4
      WGAS(1) = 0.2738                                        !  old  (i.e. with R134a only) is 0.250            
      WGAS(2) = 0.250                                         !  old  0.250
      WGAS(3) = 0.4762                                        !  old  0.500

      DENS =(0.95*102.03+0.05*56.108)/22400.0       ! = 99.7339/22400.0 compared to 102.03/22400.0 with  R134a only
      CALL GSMIXT(IMATE,'CHAMBER_GAS$',AGAS,ZGAS,DENS, NLMAT,WGAS)

C.. ----------------- DEFINE USER TRACKING MEDIA -------------------

      IFIELD = 0     ! magnetic field; tracking performed with GRKUTA;
      FIELDM = 0.    ! max field value (in Kilogauss);
      TMAXFD = 0.0   ! maximum angle due to field in one step (in degrees);
      DMAXMS = 0.5   ! max disp. due to mult. scatt. in one step (in cm);
      DEEMAX = 0.2   ! max fractional energy loss in one step;
      EPSIL = 0.01   ! tracking precision (in cm);
      STMIN = 0.01   ! min step due to energy loss or mult. scatt. (in cm);
 

C   -----------------784 785 786//associate the track to the material
      CALL GSTMED(n_electro,'ELECTRO $', 6,  0,  IFIELD,
     &                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(n_strips,'STRIPS   $', 11,  0,  IFIELD,
     &                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(n_mylar,'MYLAR     $', 786,  0,  IFIELD,
     &                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(n_mother_board,'MOTHER_BOARD $', 787,  0,  IFIELD,
     &                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(n_glass,'GLASS     $', 788,  0,  IFIELD,
     &                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(n_hexcell,'HEXCELL $', 789,  0,  IFIELD,
     &                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(n_boxgas,'BOX_GAS  $', 790,  0,  IFIELD,
     &                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(n_rpcgas,'RPC_GAS  $', 791,  1,  IFIELD,
     &                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0) ! ATTN: active patches of gas volume that 
c      CALL GSTMED(n_tfwbox,'TFW_BOX  $', 9,   0,  IFIELD,
c     &                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)

C     Special Thresholds for TOFw-Rpc Gas
      call gstpar(791,'CUTELE',0.0001)    ! electron cut at 100 keV
      call gstpar(791,'CUTGAM',0.0001)    ! gamma cut at 100 keV
      call gstpar(791,'BCUTE',0.0001)     ! e brem cut at 100 keV
      call gstpar(791,'CUTMUO',0.01)        ! muon cut at 10 MeV
      call gstpar(791,'BCUTM',0.01)         ! muon brem cut at 10 MeV
      call gstpar(791,'DCUTE',1.e+4)        ! e delta ray cut (Landau ON)
      call gstpar(791,'DCUTM',1.e+4)        ! muon delta ray cut (Landau ON)


c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c BEGIN
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

      if(firstCall.eq.1)then
         firstCall = 0
         write(6,1)
 1       format(//,
     &   ' Call to tfw geometry, version October 19, 2007 at 13:13',//)
      endif


c     Read the geometry file segment

      write( *,* ) 'tfw - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = tfw_par, err = 999 )

      CHFORM = '1I / 3F'        ! integer count, then all floating
      call MZFORM('PARA',CHFORM,iod) ! book characteristic


c     write the parameters to a zebra bank. later they will go to output file

      call MZBOOK(ixdiv_fr, lfw_PARA, lfw_PARA, 1,
     &            'PARA', 0, 0, 4, iod, 0)


C  fill the bank

      iqf(lfw_para + 1) = nPanels
      qf(lfw_para + 2) = tfw_Length(1)
      qf(lfw_para + 3) = tfw_Width(1)
      qf(lfw_para + 4) = tfw_Thick(1)

C.. -------------------- DEFINE USER VOLUMES -----------------------
      set_id = 'TFW '           ! put it in a SET

      NH = 13
      idtype = 2012
      nwpa = 200                ! for now
      nwsa = 200                ! for now
      nr = 1                    !copy number
      npar = 3

      dim_tfw(1) = tfw_Thick(1)/2.0 
      dim_tfw(2) = tfw_Width(1)/2.0 
      dim_tfw(3) = tfw_Length(1)/2.0 

      DO ndim=1,3
         dim_boxgas(ndim) = 0.5*dim_boxgas(ndim)
         dim_chamberblk(ndim) = 0.5*dim_chamberblk(ndim)
         dim_insblk(ndim) = 0.5*dim_insblk(ndim)
         dim_aoutblk(ndim) = 0.5*dim_aoutblk(ndim)
         dim_boutblk(ndim) = 0.5*dim_boutblk(ndim)
         dim_rpcgas(ndim)  = 0.5*dim_rpcgas(ndim)
         dim_tinsgla(ndim) = 0.5*dim_tinsgla(ndim)
         dim_toutgla(ndim) = 0.5*dim_toutgla(ndim)
         dim_telect(ndim)  = 0.5*dim_telect(ndim)
         dim_tmylar(ndim)  = 0.5*dim_tmylar(ndim)
         dim_tpcb(ndim)    = 0.5*dim_tpcb(ndim)  
         dim_tstrip(ndim)  = 0.5*dim_tstrip(ndim)
         dim_thexcell(ndim)= 0.5*dim_thexcell(ndim)
      ENDDO

      CALL GSVOLU(tfwPanel,'BOX ',26,dim_tfw,3,ivolu) ! aluminum frame , not sensitive // defined in mat_mix_med.f
      CALL GSATT(tfwPanel,'SEEN',1)
c      CALL GSATT(tfwPanel,'WORK',1) ! volume active for tracking  (but not for hits)
      CALL GSATT(tfwPanel,'COLO',1) ! color the Al panel
C Colors : 1- black, 2- red, 3-green,4-blue,5-yellow,6-violet,7-light blue

C---> Place Al frames in its EMCL mother volume

c insert the inasctive gas inside the aluminum box
      
      CALL GSVOLU(boxgas,'BOX ',790,dim_boxgas,3,ivolu) ! gas inside box , not sensitive
      CALL GSATT(boxgas,'SEEN',1)
      CALL GSATT(boxgas,'COLO',1)

      CALL GSVOLU(insblka,'BOX ',26,dim_insblk,3,ivolu) ! aluminum inside blk , not sensitive, bottom
      CALL GSATT(insblka,'SEEN',1)
      CALL GSATT(insblka,'COLO',4)

      CALL GSVOLU(insblkb,'BOX ',26,dim_insblk,3,ivolu) ! aluminum inside blk , not sensitive,up
      CALL GSATT(insblkb,'SEEN',1)
      CALL GSATT(insblkb,'COLO',4)

      CALL GSVOLU(aoutblk,'BOX ',18,dim_aoutblk,3,ivolu) ! air outside blk a, not sensitive  // defined in mat_mix_med.f
      CALL GSATT(aoutblk,'SEEN',1)
      CALL GSATT(aoutblk,'COLO',4) 

      CALL GSVOLU(boutblk,'BOX ',18,dim_boutblk,3,ivolu) ! air outside blk b, not sensitive  // defined in mat_mix_med.f  
      CALL GSATT(boutblk,'SEEN',0)
      CALL GSATT(boutblk,'COLO',1) 
      
      CALL GSPOS(aoutblk,1,insblka, 0.165,0.,0., 0, 'ONLY') !build up the insblka
      CALL GSPOS(aoutblk,1,insblkb,-0.165,0.,0., 0, 'ONLY')!build up the insblkb

      CALL GSVOLU(chamberblk,'BOX ',790,dim_chamberblk,3,ivolu) ! chamber blk , It is the detector
      CALL GSATT(chamberblk,'SEEN',1)
      CALL GSATT(chamberblk,'COLO',5)

c------------------------------------------------------------------------------------------------
      CALL GSVOLU(rpcgas(1),'BOX ',791,dim_rpcgas,3,ivolu) !  rpc gap, sensitive
      CALL GSATT(rpcgas(1),'SEEN',1)
      CALL GSATT(rpcgas(1),'COLO',6)
      
      CALL GSVOLU(rpcgas(2),'BOX ',791,dim_rpcgas,3,ivolu) !  rpc gap, sensitive
      CALL GSATT(rpcgas(2),'SEEN',1)
      CALL GSATT(rpcgas(2),'COLO',6)
      
      CALL GSVOLU(rpcgas(3),'BOX ',791,dim_rpcgas,3,ivolu) !  rpc gap, sensitive
      CALL GSATT(rpcgas(3),'SEEN',1)
      CALL GSATT(rpcgas(2),'COLO',6)

      CALL GSVOLU(rpcgas(4),'BOX ',791,dim_rpcgas,3,ivolu) !  rpc gap, sensitive
      CALL GSATT(rpcgas(4),'SEEN',1)
      CALL GSATT(rpcgas(4),'COLO',6)

      CALL GSVOLU(rpcgas(5),'BOX ',791,dim_rpcgas,3,ivolu) !  rpc gap, sensitive
      CALL GSATT(rpcgas(5),'SEEN',1)
      CALL GSATT(rpcgas(5),'COLO',6)

      CALL GSVOLU(rpcgas(6),'BOX ',791,dim_rpcgas,3,ivolu) !  rpc gap, sensitive
      CALL GSATT(rpcgas(6),'SEEN',1)
      CALL GSATT(rpcgas(6),'COLO',6)
c-------------------------------------------------------------------------------------------------
      CALL GSVOLU(tinsgla,'BOX ',n_glass,dim_tinsgla,3,ivolu) !  inside glass, not sensitive
      CALL GSATT(tinsgla,'SEEN',1)
      CALL GSATT(tinsgla,'COLO',1)
      
      CALL GSVOLU(toutgla,'BOX ',n_glass,dim_toutgla,3,ivolu) !  outside glass,  not sensitive
      CALL GSATT(toutgla,'SEEN',1)
      CALL GSATT(toutgla,'COLO',1)

      CALL GSVOLU(telect,'BOX ',n_electro,dim_telect,3,ivolu) !  outside elect, not sensitive 
      CALL GSATT(telect,'SEEN',1)
      CALL GSATT(telect,'COLO',1)

      CALL GSVOLU(tmylar,'BOX ',n_mylar,dim_tmylar,3,ivolu) !  outside mylar,  not sensitive
      CALL GSATT(tmylar,'SEEN',1)
      CALL GSATT(tmylar,'COLO',1)

      CALL GSVOLU(tpcb,'BOX ',n_mother_board,dim_tpcb,3,ivolu) !  outside pcb, not sensitive
      CALL GSATT(tpcb,'SEEN',1)
      CALL GSATT(tpcb,'COLO',1)

      CALL GSVOLU(tstrip,'BOX ',n_strips,dim_tstrip,3,ivolu) !  outside strip, not sensitive  
      CALL GSATT(tstrip,'SEEN',1)
      CALL GSATT(tstrip,'COLO',1)

      CALL GSVOLU(thexcell,'BOX ',n_hexcell,dim_thexcell,3,ivolu) !  outside hexcell, not sensitive
      CALL GSATT(thexcell,'SEEN',1)
      CALL GSATT(thexcell,'COLO',1)


C     ========================================================================================
C     -----------Build up the RPC chamber
      gpos(1)= -1.1065                                              !hecell
      CALL GSPOS(thexcell,1,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= -0.6065
      gpos(3)= -2.0*0.3-1.5*2.81                                   !1 strips
      CALL GSPOS(tstrip,1,chamberblk,gpos(1), 0., gpos(3), 0, 'ONLY')

      gpos(1)= -0.6065
      gpos(3)= -0.3-0.5*2.81                                       !2 strips
      CALL GSPOS(tstrip,2,chamberblk,gpos(1), 0., gpos(3), 0, 'ONLY')

      gpos(1)= -0.6065 
      gpos(3)= 0.3+0.5*2.81                                        !3 strips
      CALL GSPOS(tstrip,3,chamberblk,gpos(1), 0., gpos(3), 0, 'ONLY')

      gpos(1)= -0.6065
      gpos(3)= 2.0*0.3+1.5*2.81                                    !4 strips
      CALL GSPOS(tstrip,4,chamberblk,gpos(1), 0., gpos(3), 0, 'ONLY')

      gpos(1)= -0.5065                                             !pcb
      CALL GSPOS(tpcb,1,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

c     --  2.5*(0.055+0.023)+0.5*0.023+0.110+0.09+0.5*0.025 = 0.419
      gpos(1)= -0.419                                              !mylar
      CALL GSPOS(tmylar,1,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= -0.3615                                             !electro
      CALL GSPOS(telect,1,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= -2.5*(0.055+0.023) - 0.5*(0.023+0.110)              !outside glass
      CALL GSPOS(toutgla,1,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= -2.5*(0.055+0.023)                                  !1 gas
      CALL GSPOS(rpcgas(1),1,chamberblk,gpos(1), 0., 0., 0, 'ONLY')
 
      gpos(1)= -2.0*(0.055+0.023)                                  ! 1 inside glass
      CALL GSPOS(tinsgla,1,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= -1.5*(0.055+0.023)                                  ! 2 gas
      CALL GSPOS(rpcgas(2),2,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= -1.0*(0.055+0.023)                                  ! 2 inside glass
      CALL GSPOS(tinsgla,2,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= -0.5*(0.055+0.023)                                  ! 3 gas
      CALL GSPOS(rpcgas(3),3,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= 0                                                   ! 3 inside glass
      CALL GSPOS(tinsgla,3,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= 0.5*(0.055+0.023)                                   ! 4 gas
      CALL GSPOS(rpcgas(4),4,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= 1.0*(0.055+0.023)                                   ! 4 inside glass
      CALL GSPOS(tinsgla,4,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= 1.5*(0.055+0.023)                                   ! 5 gas
      CALL GSPOS(rpcgas(5),5,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= 2.0*(0.055+0.023)                                   ! 5 inside glass
      CALL GSPOS(tinsgla,5,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= 2.5*(0.055+0.023)                                   ! 6 gas
      CALL GSPOS(rpcgas(6),6,chamberblk,gpos(1), 0., 0., 0, 'ONLY')
      
      gpos(1)= 2.5*(0.055+0.023) + 0.5*(0.023+0.110)               !outside glass
      CALL GSPOS(toutgla,2,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= 2.5*(0.055+0.023)+0.5*0.023+0.110+0.5*0.09          !electro
      CALL GSPOS(telect,2,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= 0.419                                               !mylar
      CALL GSPOS(tmylar,2,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= 0.5065                                              !pcb
      CALL GSPOS(tpcb,2,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

      gpos(1)= 0.6065
      gpos(3)= -2.0*0.3-1.5*2.81                                   !1 strips
      CALL GSPOS(tstrip,5,chamberblk,gpos(1), 0., gpos(3), 0, 'ONLY')

      gpos(1)= 0.6065
      gpos(3)= -0.3-0.5*2.81                                       !2 strips
      CALL GSPOS(tstrip,6,chamberblk,gpos(1), 0., gpos(3), 0, 'ONLY')

      gpos(1)= 0.6065 
      gpos(3)= 0.3+0.5*2.81                                        !3 strips
      CALL GSPOS(tstrip,7,chamberblk,gpos(1), 0., gpos(3), 0, 'ONLY')

      gpos(1)= 0.6065
      gpos(3)= 2.0*0.3+1.5*2.81                                    !4 strips
      CALL GSPOS(tstrip,8,chamberblk,gpos(1), 0., gpos(3), 0, 'ONLY')

      gpos(1)= 1.1065                                              !hecell
      CALL GSPOS(thexcell,2,chamberblk,gpos(1), 0., 0., 0, 'ONLY')

C     -----------Build up the box with (down->up; left-> right)
C     ------------------insert the gas box first
      CALL GSPOS(boxgas,1,tfwPanel,0., 0., 0., 0, 'ONLY')

c     ------------------insert the blk one by one
      DO iblk = 1, 8 
C        ------------------ put the blk inside
         gpos(1) = 2.130
         gpos(2) = -1.0*blk_y
         gpos(3) = blk_z + (iblk-1.0)*blk_gap - dim_tfw(3)
         CALL GSPOS(insblka,iblk,boxgas,gpos(1), gpos(2), 
     &  gpos(3),0,'ONLY')

         gpos(1) = -2.130
         gpos(2) = -1.0*blk_y
         gpos(3) = blk_z + (iblk-0.5)*blk_gap - dim_tfw(3)
         CALL GSPOS(insblkb,iblk,boxgas,gpos(1),gpos(2),
     & gpos(3),0,'ONLY')
         
         gpos(1) = -2.130
         gpos(2) = 1.0*blk_y
         gpos(3) = blk_z + (iblk-1.0)*blk_gap - dim_tfw(3)
         CALL GSPOS(insblkb,iblk+8,boxgas,gpos(1),gpos(2), 
     & gpos(3), 0, 'ONLY')

         gpos(1) = 2.130
         gpos(2) = 1.0*blk_y
         gpos(3) = blk_z + (iblk-0.5)*blk_gap - dim_tfw(3)
         CALL GSPOS(insblka,iblk+8,boxgas,gpos(1),gpos(2), 
     & gpos(3), 0, 'ONLY')

C        ------------------ make the hole in the TFWB wall
c         gpos(1) = 2.295
         gpos(2) = -1.0*blk_y
         gpos(3) = blk_z + (iblk-1.0)*blk_gap - dim_tfw(3)
c         CALL GSPOS(aoutblk,iblk,boxgas,gpos(1), gpos(2), 
c     &  gpos(3),0,'ONLY')
         gpos(1) = 3.567
         CALL GSPOS(boutblk,iblk,tfwPanel,gpos(1),gpos(2), 
     &  gpos(3),0,'ONLY')

c         gpos(1) = -2.295
         gpos(2) = -1.0*blk_y
         gpos(3) = blk_z + (iblk-0.5)*blk_gap - dim_tfw(3)
c         CALL GSPOS(aoutblk,iblk+8,boxgas,gpos(1),gpos(2),
c     & gpos(3),0,'ONLY')
         gpos(1) = -3.567
         CALL GSPOS(boutblk,iblk+8,tfwPanel,gpos(1),gpos(2),
     & gpos(3),0,'ONLY')

c         gpos(1) = -2.295
         gpos(2) = 1.0*blk_y
         gpos(3) = blk_z + (iblk-1.0)*blk_gap - dim_tfw(3)
c         CALL GSPOS(aoutblk,iblk+16,boxgas,gpos(1),gpos(2), 
c     & gpos(3), 0, 'ONLY')
         gpos(1) = -3.567
         CALL GSPOS(boutblk,iblk+16,tfwPanel,gpos(1),gpos(2), 
     & gpos(3), 0, 'ONLY')

c         gpos(1) = 2.295
         gpos(2) = 1.0*blk_y
         gpos(3) = blk_z + (iblk-0.5)*blk_gap - dim_tfw(3)
c         CALL GSPOS(aoutblk,iblk+24,boxgas,gpos(1),gpos(2), 
c     & gpos(3), 0, 'ONLY')
         gpos(1) = 3.569
         CALL GSPOS(boutblk,iblk+24,tfwPanel,gpos(1),gpos(2), 
     & gpos(3), 0, 'ONLY')

C        -----------------put the chamber blk
         gpos(1) = -1.604
         gpos(2) = -1.0*blk_y
         gpos(3) = blk_z + (iblk-1.0)*blk_gap - dim_tfw(3)
         CALL GSPOS(chamberblk,iblk,boxgas,gpos(1),gpos(2), 
     &  gpos(3),0,'ONLY')

         gpos(1) = 1.604
         gpos(2) = -1.0*blk_y
         gpos(3) = blk_z + (iblk-0.5)*blk_gap - dim_tfw(3)
         CALL GSPOS(chamberblk,iblk+8,boxgas,gpos(1),gpos(2),
     & gpos(3),0,'ONLY')

         gpos(1) = 1.604
         gpos(2) = 1.0*blk_y
         gpos(3) = blk_z + (iblk-1.0)*blk_gap - dim_tfw(3)
        CALL GSPOS(chamberblk,iblk+16,boxgas,gpos(1),gpos(2), 
     & gpos(3), 0, 'ONLY')

         gpos(1) = -1.604
         gpos(2) = 1.0*blk_y
         gpos(3) = blk_z + (iblk-0.5)*blk_gap - dim_tfw(3)
         CALL GSPOS(chamberblk,iblk+24,boxgas,gpos(1),gpos(2), 
     & gpos(3), 0, 'ONLY')
      ENDDO

c..........set GSDET
      
C     Describe the position of the panels in EMCAL ; typical geometry

      T1 = 90.                  ! polar angle for axis 1
      T2 = 90.                  ! polar angle for axis 2 
      F3 = 0.
         
      do iPanel = 1, nPanels    
         F2 = azym(iPanel) + 90.          ! azimuthal angle for axis 2

         if((iPanel.eq.1).or.(iPanel.eq.3)) then
           T3 = 0.
           F1 = azym(iPanel)
         end if

         if((iPanel.eq.2).or.(iPanel.eq.4)) then
           T3 = 180.
           F1 = azym(iPanel) + 180.
         end if

         if (iPanel.le.2) then
          xPan(iPanel)=R             ! bottom panels x coordinate of center
          yPan(iPanel)=ydisp         ! bottom panels y coordinate of center
          zPan(iPanel)= (-1)**iPanel*tfw_Length(1)/2.0
         end if                              

         if (iPanel.ge.3) then
           ph2=ph1-azym(iPanel)
           x1=R*tan(ph2*pih/90.)
           y1=x1-dim1/2.0
           ph3=90.*atan(y1/R)/pih
           Rt=sqrt(y1*y1+R*R)
           ph4=azym(iPanel)+ph3
                         
           xPan(iPanel)=Rt*cos(ph4*pih/90.) ! top panels x coordinate of center
           yPan(iPanel)=Rt*sin(ph4*pih/90.) ! top panels y coordinate of center
           zPan(iPanel)=(-1)**iPanel*tfw_Length(1)/2.0  ! z- coordinate
         end if

         irot = irot +1      !irot will keep all the subsystem rotation matrix id
         iPanr(iPanel) = irot
         CALL GSROTM ( iPanr(iPanel), T1, F1, T2, F2, T3, F3 )

         CALL GSPOS(tfwPanel,iPanel,
     &    'EMCL',xPan(iPanel),yPan(iPanel),
     &        zPan(iPanel), iPanr(iPanel),'ONLY')
         
      enddo
      
      call GSDETV(set_id,rpcgas(1),idtype,nwpa,nwsa,iset,idet)
      call GSDETH(set_id,rpcgas(1),13,nameshTFW,
     $     nbitshTFW,origTFW,factTFW)
      
      call GSDETV(set_id,rpcgas(2),idtype,nwpa,nwsa,iset,idet)
      call GSDETH(set_id,rpcgas(2),13,nameshTFW,
     $     nbitshTFW,origTFW,factTFW)

      call GSDETV(set_id,rpcgas(3),idtype,nwpa,nwsa,iset,idet)
      call GSDETH(set_id,rpcgas(3),13,nameshTFW,
     $     nbitshTFW,origTFW,factTFW)

      call GSDETV(set_id,rpcgas(4),idtype,nwpa,nwsa,iset,idet)
      call GSDETH(set_id,rpcgas(4),13,nameshTFW,
     $     nbitshTFW,origTFW,factTFW)

      call GSDETV(set_id,rpcgas(5),idtype,nwpa,nwsa,iset,idet)
      call GSDETH(set_id,rpcgas(5),13,nameshTFW,
     $     nbitshTFW,origTFW,factTFW)

      call GSDETV(set_id,rpcgas(6),idtype,nwpa,nwsa,iset,idet)
      call GSDETH(set_id,rpcgas(6),13,nameshTFW,
     $     nbitshTFW,origTFW,factTFW)
      
      return
      write(6,998)
 998  format(/,3x,'Unable to open phnx.par')
      stop '  Cannot find main geometry file'
 999  continue
      write(6,1000)
 1000 format(/,3x,'Read error in tfw_par segment of phnx.par')
      stop 'tfw - namelist mis-match in tfw_par segment?'


      end
C**************************************************************************************
