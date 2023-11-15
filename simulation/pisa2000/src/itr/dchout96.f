      subroutine dchout96

c **********************************************************
c *  Output routine called in PISA from ITR_DIGI           *
c *        at the end of each sub-event                    *
c **********************************************************

c     Revision History
c    Name           Date            Comment
c    C.F. Maguire   Feb. 15, 1998   Add global coordinates and path length
c                                   Fix CHFORM to be consistent with output

c    C.H. Pinkenburg Feb. 09, 2001  check on # of hits returned by gfhits
c                                   indicating a too small user hit buffer 
c                                   (and set nhits to size of user buffer,
c                                   gfhits: GEANT manual page156)



      IMPLICIT NONE

#include "guphnx.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpilink.inc"
#include "subevt.inc"
#include "secubuf.inc"
 
      integer nvdim,nhmax,nhddr
      parameter (nvdim=3,nhmax=1999,nhddr=13)
      integer numvs(nvdim), numvv(nvdim,nhmax), itrah(nhmax)
      real hitd(nhddr,nhmax)
 
c     detector parameters as stored in DCHDET subroutine

c     DATA NAMEHD/'X1  ','Y1  ','Z1  ','X2  ','Y2  ','Z2  ','TOF1',
c    +    'PTID', 'TOF2' , 'XG1', 'YG1', 'ZG1', 'PL' /


c ********************* Define volume names *****************************

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
 
      real xd1(3),xd2(3),tof1,tof2
      integer ljfst,ic,nhits,ih,ioidcw,ioidce,ksub,nnhits
      integer ndmtob,ipoint,jpoint,jj
      integer numhits(40),mhits

      character*20 chform

      data ljfst /0/
      save ljfst,ioidcw,ioidce,ksub

      integer ikey,igroup,iplane,jplane,icell
      integer itest


c EXECUTABLE STATEMENTS


c initialization phase

      if (ljfst.eq.0) then
         ljfst = 1
         ksub = 1
         CHFORM = '1I / 7F 4I 4F'
         namep = 'DCAW'
         call mzform(namep,chform,ioidcw)
         namep = 'DCAE'
         call mzform(namep,chform,ioidce)
      endif
      call vzero(numvs,nvdim)

c pre-book the ZEBRA bank even if some of these will have no hits
c "runaway" writing possible if not pre-booked


c form name of the wire subvolume and count total number of hits

c     *********** west arm *******************

      nnhits = 0
      do jplane=1,40
         numhits(jplane) = 0
      enddo

      do igroup = 1,2           ! loop over wire groups
         do iplane = 1,20       ! loop over planes
            jplane = iplane
            if (igroup.eq.2) jplane = iplane + 20
            namec = arm1name
            namep = parm1name//name40(jplane)
            call gfhits('ITR ',namep,nvdim,nhddr,nhmax,0,
     +           numvs,itrah,numvv,hitd,nhits)

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
            if (nhits .gt. nhmax) then
             write(6,*) '<W> ITR (dchout96.f): number of hits exceeds',
     #       nhmax,' nhits truncated to ',nhmax,' for ',namep
             nhits = nhmax
            end if

            nnhits = nnhits + nhits
            numhits(jplane) = nhits
         enddo                  ! iplane=1,20
      enddo                     ! igroup=1,8


c set maximum dimension of DIGI bank

      if (nnhits.le.1) nnhits = 1
      ndmtob = nnhits*mfi_dch
      namep = 'DCAW'
      if (cvolu_opt(4,4).eq.'TRKS') then
         lfi_dcaw(ksub) = 0     ! this will be returned non-zero by MZBOOK
         call mzbook(ixdiv_fe,
     +        lfi_dcaw(ksub),   ! return bank address
     +        lfi_dcaw(ksub),   ! supporting link
     +        1,                ! JBIAS=1 ---> top level bank
     +        namep,            ! bank name
     +        0,                ! # of links
     +        0,                ! # of down links
     +        ndmtob+1,         ! # of data words
     +        ioidcw,           ! I/O characteristics
     +        -1)               ! do not clear the memory contents
      endif
      ipoint = lfi_dcaw(ksub)+1
      jpoint = ipoint
      iqf(jpoint) = 0

c Now store the west arm hit information

      do igroup = 1,2   ! loop over wire groups
         do iplane = 1,20   ! loop over planes in group
            jplane = iplane
            if (igroup.eq.2) jplane = iplane + 20
            namec = arm1name
            namep = parm1name//name40(jplane)
            call gfhits('ITR ',namep,nvdim,nhddr,nhmax,0,
     +           numvs,itrah,numvv,hitd,nhits)

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
            if (nhits .gt. nhmax) then
             write(6,*) '<W> ITR (dchout96.f): number of hits exceeds',
     #       nhmax,' nhits truncated to ',nhmax,' for ',namep
             nhits = nhmax
            end if

            if (nhits.gt.0) then
               do ih=1,nhits    ! loop over hits on the plane
                  call trkstack(itrah(ih)) ! put track # in PISA stack
                  do jj=1,3     ! convert to local coordinates
                     xd1(jj) = hitd(jj,ih)
                     xd2(jj) = hitd(jj+3,ih)
                  enddo
                  tof1 = hitd(7,ih)*0.001 ! units = microseconds
                  tof2 = hitd(9,ih)*0.001
                  qf(jpoint+ofidc_x1) = xd1(1)
                  qf(jpoint+ofidc_y1) = xd1(2)
                  qf(jpoint+ofidc_z1) = xd1(3)
                  qf(jpoint+ofidc_f1) = tof1
                  qf(jpoint+ofidc_x2) = xd2(1)
                  qf(jpoint+ofidc_y2) = xd2(2)
                  qf(jpoint+ofidc_z2) = xd2(3)
                  iqf(jpoint+ofidc_f2) = jplane
                  iqf(jpoint+ofidc_tr) = itrah(ih)
                  iqf(jpoint+ofidc_nw) = numvv(3,ih)
                  iqf(jpoint+ofidc_id) = hitd(8,ih)
                  qf(jpoint+ofidc_xg1) = hitd(10,ih)
                  qf(jpoint+ofidc_yg1) = hitd(11,ih)
                  qf(jpoint+ofidc_zg1) = hitd(12,ih)
                  qf(jpoint+ofidc_pl) = hitd(13,ih)
                  iqf(ipoint) = iqf(ipoint) + 1   ! number of entries
                  jpoint = jpoint+mfi_dch
               enddo            ! ih=1,nhits
            endif               ! nhits > 0
         enddo                  ! iplane=1,20
      enddo                     ! igroup=1,2




c     *************** east arm ****************

      nnhits = 0
      do jplane=1,40
         numhits(jplane) = 0
      enddo

      do igroup = 1,2           ! loop over wire groups
         do iplane = 1,20       ! loop over planes
            jplane = iplane
            if (igroup.eq.2) jplane = iplane + 20
            namec = arm2name
            namep = parm2name//name40(jplane)
            call gfhits('ITR ',namep,nvdim,nhddr,nhmax,0,
     +           numvs,itrah,numvv,hitd,nhits)

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
            if (nhits .gt. nhmax) then
             write(6,*) '<W> ITR (dchout96.f): number of hits exceeds',
     #       nhmax,' nhits truncated to ',nhmax,' for ',namep
             nhits = nhmax
            end if

            nnhits = nnhits + nhits
            numhits(jplane) = nhits
         enddo                  ! iplane=1,20
      enddo                     ! igroup=1,8


c set maximum dimension of DIGI bank

      if (nnhits.le.1) nnhits = 1
      ndmtob = nnhits*mfi_dch
      namep = 'DCAE'
      if (cvolu_opt(4,4).eq.'TRKS') then
         lfi_dcae(ksub) = 0     ! this will be returned non-zero by MZBOOK
         call mzbook(ixdiv_fe,
     +        lfi_dcae(ksub),   ! return bank address
     +        lfi_dcae(ksub),   ! supporting link
     +        1,                ! JBIAS=1 ---> top level bank
     +        namep,            ! bank name
     +        0,                ! # of links
     +        0,                ! # of down links
     +        ndmtob+1,         ! # of data words
     +        ioidce,           ! I/O characteristics
     +        -1)               ! do not clear the memory contents
      endif
      ipoint = lfi_dcae(ksub)+1
      jpoint = ipoint
      iqf(jpoint) = 0


c Now store the east arm hit information

      do igroup = 1,2   ! loop over wire groups
         do iplane = 1,20   ! loop over planes in group
            jplane = iplane
            if (igroup.eq.2) jplane = iplane + 20
            namec = arm2name
            namep = parm2name//name40(jplane)
            call gfhits('ITR ',namep,nvdim,nhddr,nhmax,0,
     +           numvs,itrah,numvv,hitd,nhits)

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
            if (nhits .gt. nhmax) then
             write(6,*) '<W> ITR (dchout96.f): number of hits exceeds',
     #       nhmax,' nhits truncated to ',nhmax,' for ',namep
             nhits = nhmax
            end if

            if (nhits.gt.0) then
               do ih=1,nhits    ! loop over hits on the plane
                  call trkstack(itrah(ih)) ! put track # in PISA stack
                  do jj=1,3     ! convert to local coordinates
                     xd1(jj) = hitd(jj,ih)
                     xd2(jj) = hitd(jj+3,ih)
                  enddo
                  tof1 = hitd(7,ih)*0.001 ! units = microseconds
                  tof2 = hitd(9,ih)*0.001
                  qf(jpoint+ofidc_x1) = xd1(1)
                  qf(jpoint+ofidc_y1) = xd1(2)
                  qf(jpoint+ofidc_z1) = xd1(3)
                  qf(jpoint+ofidc_f1) = tof1
                  qf(jpoint+ofidc_x2) = xd2(1)
                  qf(jpoint+ofidc_y2) = xd2(2)
                  qf(jpoint+ofidc_z2) = xd2(3)
                  iqf(jpoint+ofidc_f2) = jplane
                  iqf(jpoint+ofidc_tr) = itrah(ih)
                  iqf(jpoint+ofidc_nw) = numvv(3,ih)
                  iqf(jpoint+ofidc_id) = hitd(8,ih)
                  qf(jpoint+ofidc_xg1) = hitd(10,ih)
                  qf(jpoint+ofidc_yg1) = hitd(11,ih)
                  qf(jpoint+ofidc_zg1) = hitd(12,ih)
                  qf(jpoint+ofidc_pl) = hitd(13,ih)
                  iqf(ipoint) = iqf(ipoint) + 1   ! number of entries
                  jpoint = jpoint+mfi_dch
               enddo            ! ih=1,nhits
            endif               ! nhits > 0
         enddo                  ! iplane=1,20
      enddo                     ! igroup=1,2


      return
      end
