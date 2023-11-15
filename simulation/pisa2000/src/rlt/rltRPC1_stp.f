C...  Routine called at the end of each tracking step from gustep.
C...  -->>INWVOL = 1 at front face and 2 at back face and 0 if track 
C...               is increasing
C...  -->>ISTOP = 0 if track has stopped.
C...  
C...  Author: M.C. McCain and L. A. Linden Levy 17.02.2004
C...
C...  ============================================================
C...  CHANGELOG:
C...
C...  ============================================================
    
      SUBROUTINE RLTRPC1_stp
      
      IMPLICIT NONE

#include "gckine.inc"
#include "gcsets.inc"
#include "gctrak.inc"
#include "gctmed.inc"
#include "guphnx.inc"

C...  Local Variables
      REAL XM(3), XD(3), HITSD(13), XMS(3), XD1(3), XD2(3), DELE
      INTEGER INOUTL, NUMDD, IT, K

      SAVE XMS, XD1, XD2, INOUTL, DELE, HITSD
      DATA INOUTL/0/

C...  Track Medium ISVOL must be defined
      if ( ISVOL .gt. 0 .and. CHARGE .ne. 0.0 ) then
         if ( INWVOL .eq. 1 ) then ! Front Face of volume
            INOUTL=1
            DELE=0.0
            do k=1,3
               xm(k) = vect(k)
               xms(k) = vect(k)
            enddo
            CALL GMEDIA(XM, NUMDD) !may not need to know the volume here
            CALL GMTOD(XM, XD1, 1 )
            
C...  Global coords at front face
            HITSD(10) = XM(1)   !Global coords as translated by GMTOD
            HITSD(11) = XM(2)
            HITSD(12) = XM(3)

c	   print*, ' XM(1) = ', XM(1), 
c     +             ' XM(2) = ', XM(2),
c     +             ' XM(3) = ', XM(3)
         endif   
         
C...  Calculate the energy loss
         DELE = DELE + DESTEP*1.e+6 !GeV to keV conversion
         
         if ((INWVOL.eq.2.or.ISTOP.gt.0).and.INOUTL.gt.0) then !Second interface
            do k = 1,3
               XM(k) = vect(k)
            enddo
            INOUTL = 0
            CALL GMEDIA ( XMS,NUMDD)
            CALL GMTOD ( XM, XD2, 1 )
            
            HITSD( 1) = XD1( 1) ! front interface position
            HITSD( 2) = XD1( 2)
            HITSD( 3) = XD1( 3)
            HITSD( 4) = XD2( 1) ! back interface position
            HITSD( 5) = XD2( 2)
            HITSD( 6) = XD2( 3)
            HITSD( 7) = TOFG * 1.E9 !TOF info
            HITSD( 8) = FLOAT(IPART) !Particle type
            HITSD( 9) = DELE    !Energy Loss
            HITSD(13) = SLENG   !Need to look this up
            
c	   print*, ' XD1(1) = ', XD1(1), 
c     +             ' XD1(2) = ', XD1(2),
c     +             ' XD1(3) = ', XD1(3)
            CALL GSAHIT ( ISET, IDET, ITRA, NUMBV, HITSD, IT)
            if(it.eq.0)then
               print*,'rltRPC1_stp: <E> Hit not stored'
            endif
         endif
         
      endif
      
      return 
      end
