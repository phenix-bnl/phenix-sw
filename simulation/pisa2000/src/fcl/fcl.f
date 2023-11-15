c $Id: fcl.f,v 1.9 2008/05/21 08:21:56 hpereira Exp $
C       ---------

C     Original author: Charles Maguire
C     Creation date: January 25, 2003

C     Purpose: Set up PISA infrastructure for FCL forward calorimeter volume
c              mock detector looks like Silicon layers

C     Revision History:



c structure is
c              HALL
c               |
c               |
c         -------------
C         |     |     |
c       FC01  FC02   as many volumes as requested (up to 20)


      SUBROUTINE FCL(FULL,NH)

        Implicit none

C       Formal Argument Declarations
C       ----------------------------

C       External Functions

C       ------------------

C       Global Declarations
C       -------------------
#include "guphnx.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "gcflag.inc"
#include "gugeom.inc"
#include "fcl.inc"

C  need to access zebra to write parameters to FZOUT file

#include "fstore.inc"
#include "sublink.inc"
#include "fpylink.inc"

C       Local Declarations
C       ------------------

c       main geometry parameter file (phnx.par) segment



      real dim_fcl(3)


      real XHalfWidth ! The half widths of the detector definition
      real YHalfWidth 
      real ZHalfWidth 

      real XBegin
      real YBegin
      real ZBegin

      real tempXPos, tempYPos, tempZPos
      real XZero, YZero, ZZero
      integer copyNumber

      namelist /fcl_par/ Sides, Rows, Columns,
     &     ZFront, XFront, YFront, Angle, ModuleSpacing,
     &     SideWallWidth, BottomWallWidth, TopWallWidth,
     &     FrontWallWidth

      character*4     v_m_name,v_i_name,set_id,namesv(3)

      real HallXPos, HallYPos, HallZPos
      integer iRow, iCol, Number, irotFCL

      integer npar,ivolu,inull,nv,idtype,nbitsv(3),
     &     nwpa,nwsa,iset,idet, iod
      CHARACTER*10 CHFORM

      integer*4 nh              !set before call in gugeom
      character*4 full          ! set before call in gugeom
      integer iLayer
      integer iPoint


c   The following are used in GSDETH
c   Hit parameters will be position(3), energy loss, prticle type, and
c   magnitude of momentum  in the detector

      character*4 fclNamesh(8) /'POSX','POSY','POSZ','DELE','P_ID',
     &     'MOMX', 'MOMY', 'MOMZ'/
      integer*4 fclNbitsh(8) /8*32/

cscj      character*4 fclNamesh(1) /'DELE'/
cscj      integer*4 fclNbitsh(1) /1*32/

c     default setting of offsets and gains

      REAL FCLORIG(8) /1000.0,1000.0,1000.0,2*0.,3*1000.0/ !offset
      REAL FCLFACT(8) /3*100.,1.E7,1.0,3*100000./ !gain
cscj      REAL FCLORIG(1) /0.0/ !offset
cscj      REAL FCLFACT(1) /1.E7/ !gain


c     Materials

      integer IronMedium /800/
      integer AlMedium /801/
      integer ModuleMedium /871/
c      integer IronMedium /99/
c      integer AlMedium /99/
c      integer ModuleMedium /10/


c   The above gains give
c              - 0.1 keV energy deposition resolution
c              - 0.1 mm position resolution
c              - 0.01 MeV/c momentum resolution

c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun
            

C    Executable code
C    ===============

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c BEGIN
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

      



c       Read the geometery file segment


      write( *,* ) 'fcl - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = fcl_par, err = 999 )
      
      CHFORM= '1I / 6F'       ! integer count, then all floating
      call mzform('PARA',CHFORM,iod) ! book characteristic

c     write the parameters to a zebra bank. later they will go to output file

      call mzbook(ixdiv_fr, lFY_PARA, lFY_PARA, 1,
     &            'PARA', 0, 0, 90*6+1, iod, 0)



c Now some of the detector definitions from these constants

c The 1/2 widths of the detector definition:
      XHalfWidth = (Columns*ModuleSpacing + 2*SideWallWidth)/2.0
      YHalfWidth = (Rows*ModuleSpacing + 
     &     TopWallWidth + BottomWallWidth)/2.0
      ZHalfWidth = 100  !Constant till I find a better way...

c The zero point around which we place modules:
      XZero = 0.0
      YZero = (BottomWallWidth-TopWallWidth)/2.0
      ZZero = 0.0

c The beginning point where we start placing modules:
      XBegin = XZero - (Columns/2.0)*moduleSpacing
      YBegin = YZero - (Rows/2.0)*moduleSpacing
      ZBegin = ZZero+ZHalfWidth - FrontWallWidth - 2.0*ModuleZHalfWidth



C  only book volumes if input parameters are OK

      IF(CVOLU_OPT(1,19).EQ.'FULL'.OR.CVOLU_OPT(1,19).EQ.'VOLS')THEN

         v_m_name = 'HALL'
         set_id = 'FCL '        ! put it in a SET
         nv = 3
         namesv(1) = 'HALL'
         namesv(2) = 'FCLN'
         nbitsv(1) = 8
         nbitsv(2) = 8
         nbitsv(3) = 8
         NH = 8
         idtype = 2019
         nwpa = 200             ! for now
         nwsa = 200             ! for now
         irot = irot+1
         call gsrotm(irot,
     &        90.0-Angle,0.0,
     &        90.0,90.,
     &        Angle,0.0)
         irotFCL = irot
c         call gprotm(0) ! to print out all the rotation matrices


c        nmed = 10              !sensitive silicon
c         nmed = 9              !Pb low
c         nmed = 99              !Pb high
         npar = 3


c     Here is the outline of the box that holds the FCAL
         dim_fcl(1) = XHalfWidth
         dim_fcl(2) = YHalfWidth
         dim_fcl(3) = ZHalfWidth
         HallXPos = XFront-XHalfWidth
     &        -sin(Angle*3.14159/180.0)*ZHalfWidth
         HallYPos = YFront
         HallZPos = ZFront+ZHalfWidth
c     material 16 is vacuum
         call gsvolu('FCLN','BOX ',16,dim_fcl,npar,ivolu)
         call gsatt('FCLN','SEEN',1)
         call gsatt('FCLN','WORK',1)
         call gspos('FCLN',1,'HALL',
     &        HallXPos,
     &        HallYPos,
     &        HallZPos,irotFCL,'ONLY')


c     Then, insert the outer plates:

c     The front plate:
         dim_fcl(1) = XHalfWidth
         dim_fcl(2) = YHalfWidth
         dim_fcl(3) = FrontWallWidth/2.0
         call gsvolu('FCFR','BOX ',AlMedium,dim_fcl,npar,ivolu)
         call gsatt('FCFR','SEEN',0)
         call gspos('FCFR',1,'FCLN',
     &        XZero,
     &        YZero,
     &        -(ZZero+ZHalfWidth-(FrontWallWidth/2.0)),
     &        0,'ONLY')

c     The side plates:
         dim_fcl(1) = SideWallWidth/2.0
         dim_fcl(2) = YHalfWidth-(TopWallWidth/2.0)
     &        -(BottomWallWidth/2.0)
         dim_fcl(3) = ZHalfWidth-(FrontWallWidth/2.0)
         call gsvolu('FCS0','BOX ',AlMedium,dim_fcl,npar,ivolu)
         call gsvolu('FCS1','BOX ',AlMedium,dim_fcl,npar,ivolu)
         call gsatt('FCS0','SEEN',0)
         call gsatt('FCS1','SEEN',0)

         call gspos('FCS0',1,'FCLN',
     &        XZero+XHalfWidth-(SideWallWidth/2.0),
     &        YZero-(TopWallWidth-BottomWallWidth),
     &        ZZero+(FrontWallWidth),
     &        0,'ONLY')
         call gspos('FCS1',1,'FCLN',
     &        -(XZero+XHalfWidth-(SideWallWidth/2.0)),
     &        YZero,
     &        ZZero,
     &        0,'ONLY')

c     The top plate:
         dim_fcl(1) = XHalfWidth
         dim_fcl(2) = TopWallWidth/2.0
         dim_fcl(3) = ZHalfWidth
         call gsvolu('FCTP','BOX ',AlMedium,dim_fcl,npar,ivolu)
         call gsatt('FCTP','SEEN',0)
         call gspos('FCTP',1,'FCLN',
     &        XZero,
     &        YZero+YHalfWidth-(TopWallWidth/2.0),
     &        ZZero,
     &        0,'ONLY')

c     The bottom plate:
         dim_fcl(1) = XHalfWidth
         dim_fcl(2) = BottomWallWidth/2.0
         dim_fcl(3) = ZHalfWidth
         call gsvolu('FCBT','BOX ',IronMedium,dim_fcl,npar,ivolu)
         call gsatt('FCBT','SEEN',0)
         call gspos('FCBT',1,'FCLN',
     &        XZero,
     &        -(YZero+YHalfWidth-(BottomWallWidth/2.0)),
     &        ZZero,
     &        0,'ONLY')


         dim_fcl(1) = ModuleXYHalfWidth
         dim_fcl(2) = ModuleXYHalfWidth
         dim_fcl(3) = ModuleZHalfWidth

c     And here we insert the 10x9 array of Pb-Sci modules
         do iRow=1, Rows
           do iCol = 1, Columns
             copyNumber = (iRow-1)*(Columns) + iCol
             call gsvolu(NAME_SUBVOLUMES(copyNumber),
     &            'BOX ',ModuleMedium, dim_fcl,npar,ivolu)
             call gsatt(NAME_SUBVOLUMES(copyNumber),'SEEN',1)
             call gsatt(NAME_SUBVOLUMES(copyNumber),'WORK',1)
             tempXPos = XBegin + (iCol-0.5) * ModuleSpacing
             tempYPos = YBegin + (iRow-0.5) * ModuleSpacing
             write(*,*) iCol, iRow, tempXPos, tempYPos
             tempZPos = -(ZBegin + ModuleZHalfWidth)
             call gspos(NAME_SUBVOLUMES(copyNumber),1,'FCLN',
     &            tempXPos, tempYPos, tempZPos,
     &            0, 'ONLY')
             namesv(3) = NAME_SUBVOLUMES(copyNumber)
             call gsdet(set_id,NAME_SUBVOLUMES(copyNumber),
     &            nv,namesv,nbitsv,idtype,nwpa,
     &            nwsa, iset,idet)
             call gsdeth(set_id,NAME_SUBVOLUMES(copyNumber),
     &            nh,fclNamesh,fclNbitsh,
     &            fclOrig,fclFact)
             
           enddo
         enddo


       ENDIF                    ! check on volume character
       
 9999  continue
       return
 999   continue
       write(6,1000)
 1000  format(/' fck- read error in fcl_par segment')
       stop 'fcl - namelist mis-match in fcl_par segment ?'
       end
      
