       subroutine mpc_gustep

*      Author: Mickey Chiu         03.06.2007
*              Vasily Dzhordzhadze 06.25.2005

*        steps in Muon Piston Calorimeter

*-----------------------------------------------------------

C     *** global variables ***
                                                                               
#include "gconst.inc"
#include "gctmed.inc"
#include "gckine.inc"
#include "gctrak.inc"
#include "gcsets.inc"
#include "gcvolu.inc"

C  KEEP,GCKING.
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN),
     +              TOFD(MXGKIN),IFLGK(MXGKIN)
      INTEGER       KCASE,NGKINE ,IFLGK
      REAL          GKIN,TOFD

C  Temporary array used to pass values to GSCHIT, to be stored
C  in JHITS bank
      REAL mpc_hit
      COMMON/mpchit/mpc_hit(11)
      INTEGER ihit

      REAL r_vert    ! r of track's origin vertex

      if ( numed.ne.2506 ) print*, 'mpc error, numed = ', numed

      MAXNST = 20000

c      print*, 'inwvol = ', inwvol, itra, vert(1), vert(2), vert(3),
c     +        destep

cc         call gpcxyz                 ! print out xyz

C *
C * Store information on the track that is entering MPC
C *
c      if (numed.eq.2506.and.inwvol.eq.1
c     +    .and.abs(VECT(3)).ge.220.0.and.abs(VECT(3)).lt.238.0
c     +    .and.abs(VERT(3)).lt.40.) then
      if (inwvol.eq.1) then

        r_vert = sqrt(vert(1)*vert(1)+vert(2)*vert(2))

C * Store info for tracks which come from outside MPC
        if ( .not. (abs(vert(3)).gt.220.0 .and. abs(vert(3)).lt.238.0
     +        .and. r_vert.lt.22.0 .and. r_vert.gt.4.0) ) then

          mpc_hit(5) = VECT(1)         ! X value of track entering MPC
          mpc_hit(6) = VECT(2)         ! Y value of track entering MPC
          mpc_hit(7) = VECT(7)         ! Momentum of track entering MPC
          mpc_hit(8) = FLOAT(IPART)    ! PID of track entering MPC
          mpc_hit(9) = FLOAT(ITRA)     ! Number of track entering MPC

c          print*, 'vect = ', vect(1), vect(2), vect(3), vect(4),
c     +      vect(5), vect(6), vect(7)

        endif

      endif

C * Store hit if there is an energy deposit
C * Hits are generated in em_step if their energy (DESTEP) in an active
C * volume, i.e., crystal, exceeds 0.
C * A hit consists of nine elements: x, y, z, crystal index, time,
C * weighted energy, and energy.
C * Hits are stored with GSCHIT in such a way that the last element,
C * energy loss, is accumulated; for the other
C * elements what gets stored is the information from the _first_ hit
C * associated with a track.

      if (numed.eq.2506.and.destep.gt.0.) then
        mpc_hit(1) = VECT(1)         ! X of track in MPC
        mpc_hit(2) = VECT(2)         ! Y of track in MPC
        mpc_hit(3) = VECT(3)         ! Z of track in MPC
        mpc_hit(4) = TOFG*1e9        ! TOF of step (ns)

        mpc_hit(5) = VECT(1)         ! X value of track entering MPC
        mpc_hit(6) = VECT(2)         ! Y value of track entering MPC
        mpc_hit(7) = VECT(7)         ! Momentum of track entering MPC
        mpc_hit(8) = FLOAT(IPART)    ! PID of track entering MPC
        mpc_hit(9) = FLOAT(ITRA)     ! Number of track entering MPC

        mpc_hit(10) = NUMBV(1)       ! Simulation Tower Number
        mpc_hit(11) = destep*1.e+6   ! DEDX of track in MPC

c        print*, 'mpcgustep: ', iset, idet, itra, numbv(1), numbv(2), numbv(3)
c        print*, mpc_hit(3), mpc_hit(9), mpc_hit(10), mpc_hit(11)

c        call gsahit(iset,idet,ITRA,NUMBV,mpc_hit,ihit)   
c * we should call gschit instead to integrate energy
        call gschit(iset,idet,ITRA,NUMBV,mpc_hit,1,ihit)
      endif


      end
