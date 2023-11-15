      subroutine tecdet
*     =================

*****************************************************************
*                                                               *
*    THE ROUTINE TO DESCRIBE TEC  HIT PARAMETERS                *
*       CALLED FROM TRD                                         *
*                                                               *
*****************************************************************

c    Version for 1996 TEC geometry includes 6 possible planes

c    Revision History
c  Name             Date           Comment
c  C.F. Maguire     Feb. 15, 1998  Add global coordinates to output

      implicit none

c    Global variables

#include "gugeom.inc"
#include "sublink.inc"
#include "fptlink.inc"
#include "fstore.inc"

c    This subroutine is based on the DCHDET subroutine


      character*4 namehd(13),mtec(4)
      character*4 namech
      character*4 fr(6) /'FRM1','FRM2','FRM3','FRM4','FRM5','FRM6'/
      character*4 xe(6) /'XEN1','XEN2','XEN3','XEN4','XEN5','XEN6'/
 
      integer*4 nbitsd(3),nbithd(13),ipl,is,id
      real*4 factd(13),origd(13)
      data mtec/'HALL', 'EMCL',  'SECT', '    ' /
 
      data nbitsd/4,4,4/
      data namehd/'X1  ','Y1  ','Z1  ','X2  ','Y2  ','Z2  ','TOF1',
     +            'PTID', 'TOF2', 'DELE',
     +            'XG1', 'YG1', 'ZG1'/
      data nbithd/ 13*32 /
      data origd/ 6*1000., 4*0., 3*1000./
      data factd/ 7*1000., 1., 2*1000., 3*1000./


C                  DETECTOR HIT DESCRIPTION
C     Uses the geometry parameter bank to determine active planes
C     Ltec(6) array was originally "hardwired" in TECGEO routine


      do ipl = 1,6   ! Maximum of 6 planes possible
         if (iqf(lft_para+4+ipl).gt.0) then   ! this is the ltec(ipl) value
c           mtec(1) = 'EMCL'
            mtec(1) = eTEC  ! assume only East Arm
            mtec(2) = 'SECT'
            mtec(3) = fr(ipl)
            mtec(4) = xe(ipl)
            namech = xe(ipl)
            call gsdet('TRD ',namech,4,mtec,nbitsd,4,990,1990,is,id)
            call gsdeth('TRD ',namech,13,namehd,nbithd,origd,factd)
         endif                  ! check on plane being active
      enddo   ! ipl=1,6

      return
      end
