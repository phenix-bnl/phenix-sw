c $Id: rxn.f,v 1.8 2008/05/21 08:22:18 hpereira Exp $
C     File name: rxn.f
C     ---------

C     Original author: Charles F. Maguire
C     Creation date: August 25, 2006

C     Purpose: Begin Reaction Plane simulation software in PHENIX

C     Revision History:



c structure is
c     HALL
c     |
c     |
c     -------------
c     |           |
c     RXNN        RXNS

c   The RXNN and the RXNS represent the North and South prototypes of the RXN
c   The initial sensitive detector material is silicon, to be replaced by scintillator


      SUBROUTINE RXN(FULL,NH)

      implicit none

C     Formal Argument Declarations
C     ----------------------------

C     External Functions
C     ------------------

C     Global Declarations
C     -------------------
#include "guphnx.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "gcflag.inc"

C  need to access zebra to write parameters to FZOUT file

#include "fstore.inc"
#include "sublink.inc"
#include "fpslink.inc"
      

C     Local Declarations
C     ------------------
      
      integer firstCall /1/
      save firstCall            ! redundant use of save since variable was initialized

c     main geometry parameter file (phnx.par) segment

      
c     Reaction Plane Detector arm default values      
      integer rxnArms          /48/
      real rxn_rin(48)         /48*6.0/
      real rxn_rout(48)        /48*34.0/
      real rxn_zHalfLength(48) /48*1.0/
      real rxn_zCenter(48)     /48*38.0/
      real rxn_phimin(48)      /48*0.0/
      real rxn_phimax(48)      /48*360.0/
      real dim_rxn(5),dim_rxc(3),zps
      integer id,i1,i2
      
      namelist /rxn_par/rxnArms,
     $rxn_rin, rxn_rout, rxn_phimin, rxn_phimax,
     $rxn_zCenter, rxn_ZHalfLength
      
      character*4 v_m_name,v_i_name,set_id,namesv(3)
      
      character*4 rxnNames(48) 
     &/'RX00','RX01','RX02','RX03','RX04','RX05',
     &'RX06','RX07','RX08','RX09','RX0A','RX0B',
     &'RX10','RX11','RX12','RX13','RX14','RX15',
     &'RX16','RX17','RX18','RX19','RX1A','RX1B',
     &'RX20','RX21','RX22','RX23','RX24','RX25',
     &'RX26','RX27','RX28','RX29','RX2A','RX2B',
     &'RX30','RX31','RX32','RX33','RX34','RX35',
     &'RX36','RX37','RX38','RX39','RX3A','RX3B' /
      character*4 rxnConvs(2) /'RXCN', 'RXCS'/
      
      integer nr,npar,nmed,ivolu,inull,nv,idtype,nbitsv(3),
     +iaxis,nwpa,nwsa,iset,idet,IVAL, IROT, iod
      
      integer nmat,isvol,ifield,nwbuf
      
      real fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf(10)
      
      real AConv(2)
      real ZConv(2)
      real DConv
      real WConv(2)
      integer  cmat, cmed
      
      character*10 CHFORM
      
      integer*4 nh              ! set before call in gugeom
      character*4 full          ! set before call in gugeom
      integer iArm
      integer iPoint

c     /*--- null rotation ---*/

      real nul_rot(6) /90.0,0.0,90.0,90.0,0.0,0.0/
         

c   The following are used in GSDETH
c   Hit parameters will be local (x,y,z) for entrance and exit (1-6)
c     tof, particle type, energy loss, (7-9)
c     global (x,y,z) for entrance, and path length (10-12)

      

c     detector stored hit information

      character*4 namesh(16) /'X1  ','Y1  ','Z1  ',
     +'X2  ','Y2  ','Z2  ', 
     +'TOF ', 'PTID', 'DELE', 
     +'X1GL', 'Y1GL', 'Z1GL', 'PTHL',
     +'PX', 'PY', 'PZ'/
      integer*4 nbitsh(16) /16*32/
      

c     default setting of offsets and gains for stored hits

      real orig(16) /6*1000., 3*0., 3*1000., 0.0, 3*1000./
      real fact(16) /7*1000., 1., 8*1000./
            
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun
      

C    Executable code
C    ===============

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c BEGIN
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      

c     Read the geometery file segment

      do iArm=1,48
        id=int((iArm-1)/12)
        if (id.eq.0 .or. id.eq.2) then
          rxn_rin(iArm)=6.0
          rxn_rout(iArm)=18.0
        else
          rxn_rin(iArm)=18.0
          rxn_rout(iArm)=34.0
        endif
        rxn_zHalfLength(iArm)=0.9
        if (id.eq.0 .or. id.eq.1) then
          rxn_zCenter(iArm)=38.0
        else
          rxn_zCenter(iArm)=-38.0
        endif
        id=mod(iArm-1,12)
        rxn_phimin(iArm)=id*30.0
        rxn_phimax(iArm)=(id+1)*30.0
      enddo
      
c ---------------------------------------------
c- read geometry
c     write( *,* ) 'rxn - reading parameter from common interface'
c     rewind(itf_lun)
c     read( itf_lun, nml = rxn_par, err = 999 )
            
      if(firstCall.eq.1)then
        firstCall = 0
        write(6,1)
 1      format(//, '  Call to rxn geometry routine',//)
        write(6,*) 'arms   : ',rxnArms
        do id=1,4
          i1=(id-1)*12+1
          i2=id*12
          write(6,*) 'seg    : ',i1,' - ',i2 
          write(6,*) 'rin    : ',(rxn_rin(iArm),iArm=i1,i2)
          write(6,*) 'rout   : ',(rxn_rout(iArm),iArm=i1,i2)
          write(6,*) 'phimin : ',(rxn_phimin(iArm),iArm=i1,i2)
          write(6,*) 'phimax : ',(rxn_phimax(iArm),iArm=i1,i2)
          write(6,*) 'zHalf  : ',(10*rxn_zHalfLength(iArm),iArm=i1,i2)
          write(6,*) 'zCent  : ',(rxn_zCenter(iArm),iArm=i1,i2)
        enddo
      endif
      
      CHFORM = '1I / 6F'        ! integer count, then all floating
      call mzform('PARA',CHFORM,iod) ! book characteristic
      

c     write the parameters to a zebra bank. later they will go to output file

      call mzbook(ixdiv_fr, lfs_PARA, lfs_PARA, 1,
     &'PARA', 0, 0, 1+rxnArms*6, iod, 0)
      
      
C  fill the bank
      
      iqf(lfs_para + 1) = rxnArms
      iPoint = 1
      do iArm = 1,rxnArms
        
        qf(lfs_para + iPoint + 1) = rxn_phimin(iArm)
        qf(lfs_para + iPoint + 2) = rxn_phimax(iArm)
        qf(lfs_para + iPoint + 3) = rxn_rin(iArm)
        qf(lfs_para + iPoint + 4) = rxn_rout(iArm)
        qf(lfs_para + iPoint + 5) = rxn_zCenter(iArm)
        qf(lfs_para + iPoint + 6) = rxn_zHalfLength(iArm)
        
        iPoint = iPoint + 6
        
      enddo
      
      v_m_name = 'HALL'
      set_id = 'RXN '           ! put it in a SET
      nv = 3
      namesv(1) = 'HALL'
      namesv(3) = 'RXN'
      nbitsv(1) = 8
      nbitsv(2) = 8
      nbitsv(3) = 8
      
      NH = 16                   ! number of hit parameters stored by rxn_gustep
      idtype = 2018
      nwpa = 200                ! for now
      nwsa = 200                ! for now
      nr = 1                    !copy number
      irot = 1                  !rot matrix (NULL ROTATION)
      
      !nmed = 711                ! TOF-Scintillator BC404
      nmat = 1910               ! scintillator BC404 material (copied from TOF code)
      nmed = 1910               ! scintillator BC404 medium
      
      cmat = 1920               ! lead converter material
      cmed = 1920               ! lead converter medium
      npar = 5
      
      isvol = 1      ! sensitive
      ifield = 1     ! magnetic field
      fieldm = 5.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 0.50  ! max disp. due to mulsct. in one step (cm)
      deemax = 0.05  ! max fractional energy loss in one step
      epsil = 0.01   ! tracking precision (cm)
      stmin = 0.1    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      nwbuf=0
      
      AConv(1)=207.2
      AConv(2)=121.8
      ZConv(1)=82.0
      ZConv(2)=51.0
      DConv=11.35
      WConv(1)=0.98
      WConv(2)=0.02
      ! 710 = tof_mate
      
      ! scintillator material/medium (copied from TOF code)
      call gsmate(nmat,'RXN BC404 $',6.221,3.373,1.032,
     1  0.424E02,0.820E02,ubuf,nwbuf)

      call gstmed(nmed,'RXN SCINT $',nmat,isvol,ifield,
     &  fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf) 
        
      ! lead convertor material/medium
      call gsmixt(cmat,'RXN CONV $',AConv,ZConv,DConv,2,WConv)
      call gstmed(cmed,'RXN converter $', cmat, 0, ifield,
     &fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
      
      do iArm = 1,2

C-->    Create converter Volume 'RXCn'

        v_i_name = rxnConvs(iArm)
        id = (iArm-1)*24+1
        dim_rxc(1) = rxn_rin(id) ! Rmin
        dim_rxc(2) = rxn_rout(id+12) ! Rmax
        dim_rxc(3) = rxn_zHalfLength(id) ! Z(half)
        zps=rxn_zCenter(id)
        if (zps.lt.0) zps=zps+2.0*rxn_zHalfLength(id)+0.2
        if (zps.gt.0) zps=zps-2.0*rxn_zHalfLength(id)-0.2
        write(6,*) iArm,zps,' ',v_i_name 
        call gsvolu(v_i_name, 'TUBE', cmed, dim_rxc, 3, ivolu)
        call gsatt (v_i_name, 'SEEN', 1)
        call gsatt (v_i_name, 'COLO', 6)
        call gspos(v_i_name,nr,v_m_name,0.0,0.0,zps,irot,'ONLY')
      enddo
      
      do iArm = 1,RxnArms

C-->    Create Volume 'RXNn'

        v_i_name = rxnNames(iArm)
        dim_rxn(1) = rxn_rin(iArm) ! Rmin
        dim_rxn(2) = rxn_rout(iArm) ! Rmax
        dim_rxn(3) = rxn_zHalfLength(iArm) ! Z(half)
        dim_rxn(4) = rxn_phimin(iArm) ! phi min angle
        dim_rxn(5) = rxn_phimax(iArm) ! phi max angle
        
        call gsvolu(v_i_name,'TUBS',nmed,dim_rxn,npar,ivolu)
        CALL GSATT(v_i_name,'SEEN',1)
        CALL GSATT(v_i_name,'COLO',7)
        CALL GSATT(v_i_name,'WORK',1) ! Make volume sensitive
        

C---    > Place RXNn in HALL mother volume

        call gspos(v_i_name,nr,v_m_name,0.0,0.0,
     &  rxn_zCenter(iArm),irot,'ONLY')
        

C       put volume elements together into a set

        

c       put RXNn in set 'RXN '

        namesv(2) = v_i_name
        call gsdet(set_id,v_i_name,nv,namesv,nbitsv,idtype,nwpa,
     +  nwsa,iset,idet)
        call gsdeth(set_id,v_i_name,nh,namesh,nbitsh,orig,fact)
        
        
C       write(6,*) iArm,rxn_zCenter(iArm),' ',v_i_name
      enddo                     ! loop over arms
      
      return
      
c ------------------------------------------------------------------------------
c 999 continue
c     write(6,1000)
c 1000format(/,3x,'rxn - read error in rxn_par segment')
c     stop 'rxn - namelist mis-match in rxn_par segment ?'
      
      end
      
