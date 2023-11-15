c     $Id: fmpc_put_dst.f,v 1.6 2009/02/24 17:34:15 hpereira Exp $

      subroutine fmpc_put_dst
      implicit none

c     MPC Zebra data structure output

c     Original author: Vasily Dzhordzhadze

#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fmpclink.inc"

C      integer isector
      integer dcode /25/        ! WBS order, after KIN and PRI


C  ...and names.. (these should be declared only in one place, not declared
C   locally in n places)

      integer ic
      integer lk
      character*4 bnam       ! bank name?
      data bnam /'MPCB'/
 
C      CHARACTER*4  SECN1, SECS1
C      DATA SECN1 /'MPCN1'/
C      DATA SECS1 /'MPCS1'/

c  at beginning of run write out parameters associated with North

      ic = 1   ! 1 = north arm????
      lk = lfmpc(1,1)
      call u_put_ds(ixdiv_fe,lk,'PISA','MPC ',bnam,' ')

      if (
     +  root_output.eq.1 .and. 
     +  lk.gt.0 .and. 
     +  iqf(lk+1).gt.0 ) then
                
        call dstrootout(dcode, IC, iqf(lk+1),iqf(lk+2), qf(lk+2))
      endif ! check if sector has hits
 
c      bnam = secs1
c      ic = 2   ! 2 = south arm
c      lk = lfmpc(2,1)
C      call u_put_ds(ixdiv_fe,LK,'PISA','MPC ',BNAM,' ')
C      if (root_output.eq.1) then
C        if (iqf(lk+1).gt.0) then
C          call dstrootout(dcode, IC, iqf(lk+1),iqf(lk+2), qf(lk+2))
C        endif ! check if sector has hits
C      endif  ! check if root output requested

C      print*, 'end fmpc_put_dst '

      return
      end
