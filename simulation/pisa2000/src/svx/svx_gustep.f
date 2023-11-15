C     File name: svx_gustep.f
C       ---------
      SUBROUTINE SVX_GUSTEP
C     ====================================================================
c     Revision History:
c     """"""""""""""""
c     V. L. Rykov   03-Sep-2003:
c              1) Local entry/exit positions in the sensitive volume added.
c              2) Global position in Si is average of entry/exit positions
c     V. L. Rykov   19-Sep-2003: Zero energy deposit hits are not stored

c     C.F. Maguire  08-Mar-2004: Change INR name to SVX name
C     ====================================================================
      Implicit none

#include "gctrak.inc"
#include "gckine.inc"
#include "gcsets.inc"
#include "guphnx.inc"

      integer ifirst/0/

      Logical dbg_inr /.FALSE./
      INTEGER K
      INTEGER ITEST
      REAL HIT_ALL(21)  ! 15 refers to the MAXIMUM number of tracking
c                       ! parameters to store for each hit.
                        ! upped to 21 July 2006 Hubert van Hecke

      IF (INWVOL .EQ. 0 .AND. ISTOP .EQ. 0) THEN
C  accumulate energy loss
         HIT_ALL(4) = HIT_ALL(4) + DESTEP

         If(dbg_inr)                           Then
           write(*,*) 'HITS =', HIT_ALL
         Endif

      ELSE IF( INWVOL .EQ. 1 ) THEN
C  initialise energy loss, time-of-flight & entry momentum
         HIT_ALL(4) = 0.0
         HIT_ALL(5) = TOFG
         HIT_ALL(6) = FLOAT(IPART)
         Do k = 1, 3
           HIT_ALL(6+k) = VECT(7)*VECT(3+k)
         Enddo
C  record entry global and local positions.
         call gmtod(vect,HIT_ALL(10),1)
         DO K = 1,3
            HIT_ALL(K) = VECT(K)
         END DO

         DO K = 1,3                  ! Also keep global in
            HIT_ALL(K+15) = VECT(K)  ! Jul 2006
         END DO                      ! Hubert van Hecke

         If(dbg_inr)                           Then
           write(*,*) ' '
           write(*,*) 'HITS =', HIT_ALL
         Endif

      ELSE IF (INWVOL .EQ. 2 .OR. ISTOP .NE. 0) THEN
c  accumulate energy loss & record time of flight & momenta
         HIT_ALL(4) = HIT_ALL(4) + DESTEP
         HIT_ALL(5) = 0.5*(HIT_ALL(5) + TOFG)
C  take average of entry and exit global and local positions and widths.
         call gmtod(vect,HIT_ALL(13),1)
         DO K = 1,3
            HIT_ALL(K)   = 0.5*(HIT_ALL(K) + VECT(K))
         END DO

         DO K = 1,3                  ! Also keep global out
            HIT_ALL(K+18) = VECT(K)  ! Jul 2006
         END DO                      ! Hubert van Hecke


c     Store hits for charged particles, or if NEUT set in pisa.kumac file

         IF(HIT_ALL(4).GT.0 .AND.
     &        (CVOLU_OPT(5,3).EQ.'NEUT'.OR.CHARGE.NE.0.0))THEN
            CALL GSAHIT ( ISET, IDET, ITRA, NUMBV, HIT_ALL, ITEST )
            IF(ITEST.EQ.0)THEN
               WRITE(6,999)ITEST
               STOP  ' PISA is stopping in SVX_GUSTEP'
            ENDIF               !  check for successful store
         ENDIF

         If(dbg_inr)                           Then
           write(*,*) 'HITS =', HIT_ALL
         Endif

      END IF
      RETURN

 999  FORMAT(/,' SVX_GUSTEP <E>: failure in call to GSAHIT ',
     +            /,' return value was ITEST = ', i10,/)
 1001 format(a,20(1x,a4))

      END
