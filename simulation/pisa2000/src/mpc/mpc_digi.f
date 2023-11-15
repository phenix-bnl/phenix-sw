
C...  User Routine called at the end of each event
C...  
C...  Author: Vasily Dzhordzhadze 06.25.2005
C...  
      
      subroutine mpc_digi
      
      IMPLICIT NONE

#include "gconst.inc"
#include "gcflag.inc"
#include "gcvolu.inc"
#include "gcnum.inc"
#include "guphnx.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fmpclink.inc"
#include "mapfest.inc"
 
      integer nvdim
      integer nhmax
      integer nhddr
C      PARAMETER ( NVDIM=2,NHMAX=1999,NHDDR= 9)
      PARAMETER ( NVDIM=2,NHMAX=150000,NHDDR= 11)

      real    svrtx(3), pvrtx(4)
      integer spart,snvert,ncycle,ittest

      integer ITRAH(NHMAX), NUMVS(NVDIM), NUMVV(NVDIM, NHMAX)
      real    hitd(NHDDR,NHMAX)

      integer ientry
      integer p_track
      integer isumhit, sumhits
      integer newflag                  ! whether there is a new track,tower combo
      real    sum_hitd(NHDDR,NHMAX)    ! sum of all hits from a track in a tower

      integer ioipd1
      integer ioipd2
      integer ic,ijk
      integer ipoint
      integer jpoint
      integer ihit
      integer nhits
      integer ljfst
      integer ksub
      integer jj
 
      CHARACTER*20 CHFORM

      DATA LJFST /0/
      SAVE LJFST, IOIPD1, IOIPD2, KSUB
   
      character*4 MPCAL/'PBO4'/

C      call gphits('MPC ',MPCAL) ! dump all hits info

c     begin execution
      CALL VZERO (NUMVS, NVDIM)
      if ( ljfst .eq. 0 ) then
         ljfst = 1
         KSUB = 1
C         CHFORM = '1I / 1I 7F 1I 5F'
C         CHFORM = '1I / 2I 5F 3I 2F'
         CHFORM = '1I / 1I 11F'
C         CHFORM = '1I / 1I 6F 3I 2F'
         CALL MZFORM( MPCAL, CHFORM, IOIPD1 )
      endif                     !  end initialization

      

C --- 

      CALL GFHITS ( 'MPC ', MPCAL, NVDIM, NHDDR, NHMAX, 
     *               0, NUMVS, ITRAH, NUMVV, HITD, NHITS )

C      print*,'mpc_digi.f: <I> MPC nhits = ', nhits

      if (nhits .gt. nhmax) then
         print*, 'mpc_digi.f: <E> MPC nhits goes beyond limit ', nhits
         nhits = nhmax
      end if

      sumhits = 0                              ! number of unique tower,track entries

      if (nhits .ge. 1) then

         IF (
     *    ((ivolu_opt(1,23).ne.0).and.(CVOLU_OPT(2,23).EQ.'P_ID'))
     *    .or.
     *    ((ivolu_opt(1,24).ne.0).and.(CVOLU_OPT(2,24).EQ.'P_ID')) )
     *   THEN

C *
C *   Count the number of different entering towers*tracks
C *

C           write(*,*) 'nhits ',nhits
C           write(*,*) ' call gpkine(0)'
C           call gpkine(0)

           DO ihit = 1, nhits

             call mpc_track(itrah(ihit), svrtx, pvrtx, spart, snvert,
     +                      ittest, ncycle)

C *   Look for an existing track,tower combination
             newflag = 0;
             do isumhit = sumhits, 1, -1
               if ( sum_hitd(9,isumhit).eq.ittest .and.
     +              sum_hitd(10,isumhit).eq.hitd(10,ihit) )
     +         then
                 newflag = isumhit
               endif
             enddo

             if ( newflag.gt.0 ) then

               if ( hitd(4,newflag)<sum_hitd(4,ihit) ) then
                 sum_hitd(1,newflag) =  hitd(1,ihit)
                 sum_hitd(2,newflag) =  hitd(2,ihit)
                 sum_hitd(3,newflag) =  hitd(3,ihit)
                 sum_hitd(4,newflag) =  hitd(4,ihit)
                 sum_hitd(5,newflag) =  hitd(5,ihit)
                 sum_hitd(6,newflag) =  hitd(6,ihit)
               endif

               sum_hitd(11,newflag) = sum_hitd(11,newflag)
     +                                     + hitd(11,ihit)
                 
             else                ! track,tower combo not found

               sumhits = sumhits + 1
               do ientry=1,MFI_MPC-1
                 sum_hitd(ientry,sumhits) = hitd(ientry,ihit)
               enddo
               
               sum_hitd(7,sumhits) = pvrtx(4)
               sum_hitd(8,sumhits) = spart
               sum_hitd(9,sumhits) = ittest

             endif

           ENDDO                 !  loop on number of hits

C           print *, 'sumhits ', sumhits

C           DO ihit = 1, sumhits
C             print *,ihit,sum_hitd(1,ihit),sum_hitd(2,ihit),
C     +          sum_hitd(3,ihit),sum_hitd(4,ihit),sum_hitd(5,ihit),
C     +          sum_hitd(6,ihit),sum_hitd(7,ihit),sum_hitd(8,ihit),
C     +          sum_hitd(9,ihit),sum_hitd(10,ihit),sum_hitd(11,ihit)
C           ENDDO

           call MZBOOK(ixdiv_fe,
     $        LFMPC(1,KSUB),     !return bank address
     $        LFMPC(1,KSUB),     !supporting link
     $        1,                 !JBIAS=1 ---> top level bank
     $        MPCAL,             !bank name
     $        0,                 !# of links
     $        0,                 !# of down links
     $        MFI_MPC*sumhits + 1, !# of data words
     $        IOIPD1,            !I/O characteristics
     $        -1)                !do not clear memory contents
           IPOINT = LFMPC(1,KSUB) + 1
           JPOINT = IPOINT
C          Put in the total number of summed up hits
           IQF(JPOINT) = sumhits

           DO ihit = 1, sumhits

             p_track = sum_hitd(9,ihit)          ! incoming track id
             IQF( IPOINT + OFIMPC_TR ) = sum_hitd(9,ihit)

             call trkstack(p_track)

             QF(ipoint + OFIMPC_X  ) = sum_hitd(1,ihit)   ! x
             QF(ipoint + OFIMPC_Y  ) = sum_hitd(2,ihit)   ! y
             QF(ipoint + OFIMPC_Z  ) = sum_hitd(3,ihit)   ! z
             QF(ipoint + OFIMPC_TOFG) = sum_hitd(4,ihit)  ! tofg
             QF(ipoint + OFIMPC_EX) = sum_hitd(5,ihit)    ! entry point x
             QF(ipoint + OFIMPC_EY) = sum_hitd(6,ihit)    ! entry point y
             QF(ipoint + OFIMPC_EMOM) = sum_hitd(7,ihit)  ! entry momentum
             QF(ipoint + OFIMPC_EPID) = sum_hitd(8,ihit)  ! entry pid
             QF(ipoint + OFIMPC_ENUM) = sum_hitd(9,ihit)  ! entry track num
             QF(ipoint + OFIMPC_TOWR) = sum_hitd(10,ihit) ! sim tower number
             QF(ipoint + OFIMPC_DEDX) = sum_hitd(11,ihit) ! dedx

             IPOINT = IPOINT + MFI_MPC

C             print*, 'X ', ihit, itrah(ihit), hitd(1,ihit), 
C     $        hitd(2,ihit),
C     $        hitd(3,ihit),hitd(4,ihit),hitd(5,ihit),hitd(6,ihit),
C     $        hitd(7,ihit),hitd(8,ihit),hitd(9,ihit),hitd(10,ihit),
C     $        hitd(11,ihit)


c     Track ancestry call

C             call mpc_track(itrah(ihit), svrtx, pvrtx, spart, snvert,
C     +                      ittest, ncycle)
C             print*, 'Y ', ihit, itrah(ihit), svrtx(1), svrtx(2),
C     +            svrtx(3), pvrtx(4), spart, snvert, ittest, ncycle

           ENDDO                  !  loop on number of hits

         ELSE               ! FULL HITS INFO if CVOLU_OPT(2,23)!=P_ID

           call MZBOOK(ixdiv_fe,
     $        LFMPC(1,KSUB),     !return bank address
     $        LFMPC(1,KSUB),     !supporting link
     $        1,                 !JBIAS=1 ---> top level bank
     $        MPCAL,             !bank name
     $        0,                 !# of links
     $        0,                 !# of down links
     $        MFI_MPC*NHITS + 1, !# of data words
     $        IOIPD1,            !I/O characteristics
     $        -1)                !do not clear memory contents
           IPOINT = LFMPC(1,KSUB) + 1
           JPOINT = IPOINT
           IQF(JPOINT) = NHITS

           DO ihit = 1, nhits

             IQF( IPOINT + OFIMPC_TR ) = itrah(ihit)

             call trkstack(itrah(ihit))
             
             QF(ipoint + OFIMPC_X  ) = hitd(1,ihit)   ! x
             QF(ipoint + OFIMPC_Y  ) = hitd(2,ihit)   ! y
             QF(ipoint + OFIMPC_Z  ) = hitd(3,ihit)   ! z
             QF(ipoint + OFIMPC_TOFG) = hitd(4,ihit)  ! tofg
             QF(ipoint + OFIMPC_EX) = hitd(5,ihit)    ! entry point x
             QF(ipoint + OFIMPC_EY) = hitd(6,ihit)    ! entry point y
             QF(ipoint + OFIMPC_EMOM) = hitd(7,ihit)  ! entry momentum
             QF(ipoint + OFIMPC_EPID) = hitd(8,ihit)  ! entry pid
             QF(ipoint + OFIMPC_ENUM) = hitd(9,ihit)  ! entry track num
             QF(ipoint + OFIMPC_TOWR) = hitd(10,ihit) ! sim tower number
             QF(ipoint + OFIMPC_DEDX) = hitd(11,ihit) ! dedx

             IPOINT = IPOINT + MFI_MPC
           ENDDO                  !  loop on number of hits

         END IF

      else       ! There were no hits!

         call MZBOOK(ixdiv_fe,
     $        LFMPC(1,KSUB),    !return bank address
     $        LFMPC(1,KSUB),    !supporting link
     $        1,                !JBIAS=1 ---> top level bank
     $        MPCAL,            !bank name
     $        0,                !# of links
     $        0,                !# of down links
     $        MFI_MPC + 1,      !# of data words
     $        IOIPD1,           !I/O characteristics
     $        -1)               !do not clear memory contents

         IPOINT = LFMPC(1,KSUB) + 1
         JPOINT = IPOINT
         IQF(JPOINT) = 0
      end if

      RETURN 
      END
