      SUBROUTINE RV_CHI2(PTOT,P1,PD1,PD2)
      IMPLICIT NONE
c
c     Original Author: Charles F. Maguire (based on RV_PHI and RV_JPSI)
c     Creation Date: June 15, 1999
c
c     Revision History
C
C     DECAYS THE CHI MASS INTO J/Psi + GAMMA; RETURNS DAUGHTER MOMENTA PD1,PD2
C
c     Subroutine arguments:
c          PTOT: total momentum of parent (input)
c          P1:   momentum vector components of parent (input)
c          PD1:  momentum vector component of J/Psi   (output)
c          PD2:  momentum vector component of gamma  (output)
c
c     Map:
c         Called by RV_CHI
c         Calls GDECA2, GLOREN, GDROT
c
c
c     local specifications
c
      INTEGER IXYZ
      integer icall/0/
      REAL PTOT
      REAL P1(3)
      REAL PD1(3)
      REAL PD2(3)
      REAL AMASS
      PARAMETER (AMASS = 3.51)
      REAL BETA(4)
      REAL PCM(4,3)
      REAL GKIN(3,2)
      REAL DMASS
      REAL SINTH, COSTH, COSPH, SINPH
      REAL E0
      save icall
c
c     begin execution
c
      DMASS = 3.097  ! J/PSI mass
      CALL GDECA2(AMASS,DMASS,0.0,PCM)
      E0 = SQRT(PTOT*PTOT + AMASS*AMASS)
      BETA(1) = 0.0
      BETA(2) = 0.0
      BETA(3) = -PTOT/E0
      BETA(4) = E0/AMASS
      COSTH = P1(3)/PTOT
      SINTH = SQRT(ABS(1.0 - COSTH*COSTH))
      IF(SINTH.NE.0.0)THEN
         COSPH = P1(1)/SQRT(P1(1)*P1(1) + P1(2)*P1(2))
         SINPH = P1(2)/SQRT(P1(1)*P1(1) + P1(2)*P1(2))
      ELSE
         SINPH = 0.0
         COSPH = 1.0
      ENDIF
      DO IXYZ = 1,3
         GKIN(IXYZ,1) = PCM(IXYZ,1)
         GKIN(IXYZ,2) = PCM(IXYZ,2)
      ENDDO
      CALL GLOREN(BETA,PCM(1,1),GKIN(1,1))
      CALL GDROT(GKIN(1,1),COSTH,SINTH,COSPH,SINPH)
      CALL GLOREN(BETA,PCM(1,2),GKIN(1,2))
      CALL GDROT(GKIN(1,2),COSTH,SINTH,COSPH,SINPH)
      DO IXYZ = 1,3
         PD1(IXYZ) = GKIN(IXYZ,1)
         PD2(IXYZ) = GKIN(IXYZ,2)
      ENDDO
      if(icall.lt.0)then       ! diagnostic printout disabled
         icall = icall + 1
         write(6,1)ptot,p1,pd1,pd2,beta,(gkin(ixyz,1),ixyz=1,3),
     1             (gkin(ixyz,2),ixyz=1,3),(pcm(ixyz,1),ixyz=1,3),
     2             (pcm(ixyz,2),ixyz=1,3)
1     format(/,2x,'ptot,p1',4e14.5,
     1       /,2x,'pd1 ',3e14.5,
     2       /,2x,'pd2 ',3e14.5,
     3       /,2x,'beta ',4e14.5,
     4       /,2x,'gkin1 ',3e14.5,
     5       /,2x,'gkin2 ',3e14.5,
     6       /,2x,'pcm1 ',3e14.5,
     7       /,2x,'pcm2 ',3e14.5)
      endif
      RETURN
      END
