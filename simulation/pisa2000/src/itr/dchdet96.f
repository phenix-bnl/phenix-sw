      subroutine dchdet96(nmchmb)

c ****************************************************************
c                                                                *
c     THE ROUTINE TO DISCRIBE DR. CH.  HIT PARAMETERS            *
c        CALLED FROM itgeom                                      *
c                                                                *
c ****************************************************************

      implicit none


c    New version from Mitch on December 15, 1995 for LUXOR
c    PISA version adds the exit TOF2 to go with the entrance TOF1

c     Revision History
c    Name           Date            Comment
c    C.F. Maguire   Feb. 15, 1998   Add global coordinates and path length



#include "gugeom.inc"


c GEANT variable definitions

      character*4 namehd(13), mdrc(3)
      integer*4 nbitsd(3)
      integer nbithd(13),j,l,nmchmb,is,id
      real*4 factd(13),origd(13)
      data mdrc /'INTR','    ','    '/
      data nbitsd /8,8,8/

      integer ikey,igroup,iplane,iwire,icell
      integer nkey,ngroup,nplane,nwire,ncell
      integer jplane,itest

c Define volume names here

      character*4 arm1name,arm2name
      character*2 parm1name,parm2name
      character*2 name40(40)
      character*4 namec,namep
      character*1 name20(20),namee,namew,name8(8)

      data arm1name /'DCAW'/
      data arm2name /'DCAE'/
      data namee /'F'/
      data namew /'D'/
      data parm1name /'DP'/
      data parm2name /'FP'/
      data name40 /'01','02','03','04','05','06','07','08','09','10',
     +             '11','12','13','14','15','16','17','18','19','20',
     +             '21','22','23','24','25','26','27','28','29','30',
     +             '31','32','33','34','35','36','37','38','39','40'/
      data name8 /'1','2','3','4','5','6','7','8'/
      data name20 /'1','2','3','4','5','6','7','8','9','A','B','C',
     +     'D','E','F','G','H','I','J','K'/

c set GEANT variables here

      data namehd /'X1  ','Y1  ','Z1  ','X2  ','Y2  ','Z2  ','TOF1',
     +             'PTID', 'TOF2' , 'XG1', 'YG1', 'ZG1', 'PL' /
      data nbithd /32,32,32,32,32,32,32,32,32,4*32/
      data origd /1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,
     +            0.0,0.0,0.0, 3*1000., 0.0/
      data factd /1000.0,1000.0,1000.0,1000.0,1000.0,1000.0,
     +            1000.0,1.0,1000.0,4*1000./

c EXECUTABLE STATEMENTS

      write(6,*) 'DCHDET-I: dchdet execution started.'

      ncell = 80
      nplane = 20
      ngroup = 2
      nkey = 20

c work on arm 1 (west) first
      do igroup = 1,ngroup      ! loop over wire groups in a cell
         do iplane = 1,nplane   ! loop over planes in a group
            jplane = iplane
            if (igroup.eq.2) jplane = iplane + 20
            mdrc(1) = wDCH
            mdrc(2) = 'DCAW'
            namep = parm1name//name40(jplane)
            mdrc(3) = namep
            call gsdet('ITR ',namep,3,mdrc,nbitsd,4,990,1990,is,id)
            call gsdeth('ITR ',namep,13,namehd,nbithd,origd,factd)
         enddo                  ! iplane=1,nplane
      enddo                     ! igroup = 1,ngroup

c work on arm 2 (east)
      do igroup = 1,ngroup      ! loop over wire groups in a cell
         do iplane = 1,nplane   ! loop over planes in a group
            jplane = iplane
            if (igroup.eq.2) jplane = iplane + 20
            mdrc(1) = eDCH
            mdrc(2) = 'DCAE'
            namep = parm2name//name40(jplane)
            mdrc(3) = namep
            call gsdet('ITR ',namep,3,mdrc,nbitsd,4,990,1990,is,id)
            call gsdeth('ITR ',namep,13,namehd,nbithd,origd,factd)
         enddo                  ! iplane=1,nplane
      enddo                     ! igroup = 1,ngroup

      write(6,*) 'DCHDET-I: dchdet execution completed successfully.'

      return
      end
