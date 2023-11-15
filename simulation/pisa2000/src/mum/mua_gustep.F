*CMZ :  2.04/00 23/08/94  06.55.14  by  Mike Leitch
*CMZ :  2.03/00 28/07/93  09.03.06  by  Charles F. Maguire
*-- Author :    Surender Saini   08/06/93
 
      subroutine mua_gustep

c    *************************************************************
c    *                                                           *
c    *  MUA_GUSTEP (vsn 1.00) gustep for muon_arm                *
c    *                                                           *
c    *  Called by ==> :: < gustep >                              *
c    *  IN   :: none                                             *
c    *  OUT  :: none                                             *
c    *                                                           *
c    *  written  by ::  Surender Saini, 08/06/93 10.49.44        *
c    *  modified by ::  C.F. Maguire, 3/18/98, block neutrinos   *
c    *  modified by ::                                           *
c    *                                                           *
c    *************************************************************

C ISVOL =1 For sensitive volumes
C INWVOL=0 Same volume
c        1 Entering new volume
c        2 Exiting a volume
c        3 Exiting Mother
c ISTOP =0 Not stopped
c        1 Particle lost identity
c        2 Energy of particle below cut

*KEEP,GCKING.
#include "geant321/gcking.inc"
*KEEP,GCKINE.
#include "gckine.inc"
*KEEP,GCBANK.
#include "gcbank.inc"
*KEEP,GCTRAK.
#include "gctrak.inc"
*KEEP,GCFLAG.
#include "gcflag.inc"
*KEEP,GCTMED.
#include "gctmed.inc"
*KEEP,GCSETS.
#include "gcsets.inc"
*KEEP,GCVOLU.
#include "gcvolu.inc"
*KEEP,NAMLVL.
#include "namlvl.inc"
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEEP,SECUBUF.
#include "secubuf.inc"
*KEND.
 
C   local variables
         REAL HIT_ALL(11)
         INTEGER ITEMP1, ITEMP6, IFIRST
         SAVE ITEMP1, ITEMP6
         CHARACTER*4 CTEMP1, CTEMP6
         DATA CTEMP1 /'MUF1'/
         DATA CTEMP6 /'MFU6'/
         DATA IFIRST /0/

c     begin execution

      IF(IFIRST.EQ.0)THEN

c        fix-up for IBM not liking integers compared to characters

         IFIRST = 1
         CALL UCTOH(CTEMP1,ITEMP1,4,4)
         CALL UCTOH(CTEMP6,ITEMP6,4,4)
      ENDIF

c     Don't ever consider neutrinos (led to GSAHIT saturation)

      if(ipart.eq.4)then
         return
      endif

c     check for neutral particle storage

      if(charge.eq.0.0.and.
     +   (cvolu_opt(9,10).eq.'NNEU'.or.
     +   cvolu_opt(9,11).eq.'NNEU'.or.
     +   cvolu_opt(9,12).eq.'NNEU'))then
         return  ! skip the storage
      endif  ! check for neutral particle storage

 

c Store hits for muon arm
c (a) FAKE_VOLUMES ( All particles )
 
      if(isvol .eq. 1) then        !sensitive_volume, so store hits
        if(idtype .eq. 7)then
 
c  Accumulate energy deposition
 
         if(inwvol .eq. 0 .and. istop .eq. 0)then
            hit_all(4) = hit_all(4) + destep
         else if(inwvol .eq. 0 .and. istop .eq. 2)then
            hit_all(4) = hit_all(4) + destep + 0.001
            hit_all(10) = sleng - hit_all(10)
            hit_all(11) = getot
            call gsahit(iset,idet,itra,numbv,hit_all,ihit)
C  Particle leaving volume or has been destroyed
 
         else if(inwvol .eq. 2 .or. (inwvol .eq. 0 .and.
     &      istop .eq. 1))then
            hit_all(1)= (hit_all(1) + vect(1))/2.0
            hit_all(2)= (hit_all(2) + vect(2))/2.0
            hit_all(3)= (hit_all(3) + vect(3))/2.0

            hit_all(4) = hit_all(4) + destep
            hit_all(10) = sleng - hit_all(10)
            hit_all(11) = getot
            call gsahit(iset,idet,itra,numbv,hit_all,ihit)
 
C  Entering the volume. Init and store position & momentum at entry point.
 
         else if(inwvol .eq. 1 .and. istop .eq. 0)then
 
            hit_all(1)=vect(1)
            hit_all(2)=vect(2)
            hit_all(3)=vect(3)
            hit_all(4) = 0.0
            hit_all(5) = tofg*1.E9     ! INTEGRATED time of flight
            hit_all(6) = ipart
            hit_all(7)=vect(4)*vect(7)
            hit_all(8)=vect(5)*vect(7)
            hit_all(9)=vect(6)*vect(7)
            hit_all(10) = sleng
            hit_all(11) = getot
         end if
        endif              ! idtype check
      end if               !isvol = 1
 
        if (idebug.ne.0)then
         if(inwvol .eq. 1)then

c    comparison not legal on IBM RS6000
c         if(names(nlevel) .eq. 'MUF6')then

          if(names(nlevel) .eq. itemp6)then
           write(6,'(2x,''Entered MUF6'',3i4)')itra,ipart,ipaold

c         else if(names(nlevel) .eq. 'MUF1')then

          else if(names(nlevel) .eq. itemp1)then
           write(6,'(2x,''Entered MUF1'',3i4)')itra,ipart,ipaold
          end if
         end if
        end if
 
      return
      end
