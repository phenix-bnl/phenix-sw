c     $Id: fz_put_dst.f,v 1.5 2009/02/24 17:34:18 hpereira Exp $

      subroutine fz_put_dst

c     author: charles f. maguire (vanderbilt university)     
c     creation date: february 11, 2000
c     purpose: retrieve zdc hits data and transfer to root encoder routine

c     calling map:
c        called by e_put_dst at the end of each subevent
c        calls gfhits, encodeeventrootzdc

c     revision history

c    c.h. pinkenburg feb. 09, 2001  check on # of hits returned by gfhits
c                                   indicating a too small user hit buffer 
c                                   (and set nhits to size of user buffer,
c                                   gfhits: geant manual page156)


      implicit none


c     global variables

#include "subevt.inc"


c     local variables

      integer nhitsmax
      parameter (nhitsmax=10000)

      integer nhdim
      parameter (nhdim=11)
      integer numvs(5), itr(nhitsmax), numbv(5,nhitsmax), nhits
      real    hits(nhdim, nhitsmax)


      integer ihit

      integer dcode /13/   ! key for encoding into root

c     begin execution
c     retrieve ZDC hits for this subevent

      call gfhits(
     +  'ZDC ','MAI1',5,NHDIM,NHITSMAX,
     +  0,numvs,itr,numbv,hits,nhits)

C /chp/ if user array hits exceeded, nhits is returned as nhitsmax+1
      if (nhits .gt. nhitsmax) then
        write(6,*) '<W> ZDC (fz_put_dst.f): number of hits exceeds',
     +  nhitsmax,' nhits truncated to ',nhitsmax,' for MAI1'
        nhits = nhitsmax
      end if

      if(nhits.gt.0)then
        call dstrootout(dcode, 0, nhits, itr(1), hits(1,1)) 
      endif

      call gfhits(
     +  'ZDC ','MAI2',5,NHDIM,NHITSMAX,
     +  0,numvs,itr,numbv,hits,nhits)

C /chp/ if user array hits exceeded, nhits is returned as nhitsmax+1
      if (nhits .gt. nhitsmax) then
        write(6,*) '<W> ZDC (fz_put_dst.f): number of hits exceeds',
     #  nhitsmax,' nhits truncated to ',nhitsmax,' for MAI2'
        nhits = nhitsmax
      end if

      if(nhits.gt.0) then
         call dstrootout(dcode, 0, nhits, itr(1), hits(1,1)) 
      endif

      return
      end
