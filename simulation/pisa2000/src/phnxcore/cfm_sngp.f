*CMZ :  2.04/00 06/06/93  18.12.56  by  Charles F. Maguire
*CMZ :          01/06/93  12.03.29  by  Charles F. Maguire
      SUBROUTINE CFM_SNGP(IEVSTAT)
      IMPLICIT NONE
C.
C.    ******************************************************************
C.    *                                                                *
C.    *        "Private Event Generator Code from C.F. Maguire         *
C.    *              Fills PMC2 array for simple events.               *
C.    *                                                                *
C.    ******************************************************************
C.
C.    ------------------------------------------------------------------


c     Called by GUKINE
c     Calls MCRANCO

      integer ievstat

c     Subroutine call variable: IEVSTAT
c                               = -1 for normal return
c                               =  0 for error

#include "gckine.inc"
#include "gcflag.inc"
#include "guevgen.inc"
#include "subevt.inc"

c     local specifications

      integer i
      logical logopn /.false./

      integer icall /0/

      save logopn, icall  ! actually don't need to use save on initialized variables

c     begin execution

      numevt = numevt + 1
      ievstat = 0                 ! default as error return

c     Currently (6/1/93) the end_evtflg is not reset in case of an error
c                        similarly for the IEOTRI and IEORUN flags

      end_evtflg = .true.        ! in case of previous error
      ieotri = 0                 ! in case of previous error
      ieorun = 0                 ! in case of previous error
      if(ikine.ne.-9)then
         logopn=.false.
         chevt_name = 'CFM single particle'
      else
         chevt_name = 'CFM multi-particle'
         if(pkine(6).eq.1.0)then ! forced reset of open status
           logopn = .false.
           pkine(6) = 0.0        ! clear the flag
         endif
      endif
      if(.not.logopn)then
         OPEN(UNIT=17,FILE='cfmgmc.input',STATUS='OLD',ERR=8199)
      endif
      read(17,*,err=8197,end=8195)(pmc2(i),i=1,10)
      if(ikine.eq.9)then
         close(unit=17)
      else
         logopn = .true.
      endif
      if(ievent.lt.25.or.(ievent-250*(ievent/250).eq.0))then
         write(6,8101)(pmc2(i),i=1,10),IEVENT
8101     format(1x,'MC: ',F5.0,F6.1,7F7.1,F7.1,' EVENT ',I7)
      endif
      IF(iran_phi.eq.1)THEN
         call mc_ranphi
         if(icall.eq.0)then
            icall = 1
            write(6,*) 'USER CALLS PARTICLES UNIFORMLY IN PHI'
         endif  ! one time output
      ELSE
         call mc_ranco
         if(icall.eq.0)then
            icall = 1
            write(6,*) 'USER CALLS PARTICLES IN A CONE'
         endif  ! one time output
      ENDIF

      ipopsub = mxtot
      ievstat = -1             ! signify good return
      go to 9000
8195  continue
      write(6,*)'   ***EOF WHILE READING cfmgmc.input FILE***'
      return
8197  continue
      WRITE(6,*)'   ***ERROR WHILE READING cfmgmc.input FILE***'
      return
8199  continue
      WRITE(6,*)'   ***UNABLE TO OPEN cfmgmc.input FILE***'
      return
9000  continue

c     return to the GO TO 9000 location in GUEVGEN

      return
      END
