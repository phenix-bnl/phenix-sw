      SUBROUTINE RV_PHI2(PTOT,IEOM,BWMASS,P1,PD1,PD2)
      IMPLICIT NONE
C
C     DECAYS THE PHI MASS INTO TWO ELECTRONS, RETURNS DAUGHTER MOMENTA PD1,PD2
C
c
c     Update June 5, 1993 to handle decay into two electrons
c     Update July 8, 1997 to handle decay into two kaons
c     Update October 31, 2002 to allow for BW width in Phi mass, and change mass to 1.019456 from 1.020 GeV
c
c     Subroutine arguments:
c          PTOT: total momentum of parent (input)
c          IEOM: switch for muon (=0), electron (=1), or kaon (=2)  (input)
c          BWMASS: switch for 0 MeV width (BWMASS=0) or 4.26 MeV Breit-Wigner width (BWMASS=1)
c          P1:   momentum vector components of parent (output)
c          PD1:  momentum vector component of first daughter   (output)
c          PD2:  momentum vector component of second daughter  (output)
c          
c
c     Map:
c         Called by RV_PHI
c         Calls GDECA2, GLOREN, GDROT
c
c    Author: Charles F. Maguire
c
c     local specifications
c
      INTEGER IXYZ
      integer icall/0/
      INTEGER IEOM
      INTEGER BWMASS
      REAL RNDM
      REAL PTOT
      REAL P1(3)
      REAL PD1(3)
      REAL PD2(3)
      REAL AMASS
      PARAMETER (AMASS = 1.019456)
      REAL BWGAMMA
      PARAMETER (BWGAMMA = 0.00426)
      REAL BMASS
      REAL BETA(4)
      REAL PCM(4,3)
      REAL GKIN(4,2)
      REAL ELEMAS
      PARAMETER (ELEMAS = 0.000511)
      REAL RMUMAS
      PARAMETER (RMUMAS = 0.105659)
      REAL RKMASS
      PARAMETER (RKMASS = 0.493677)
      REAL DMASS
      REAL SINTH, COSTH, COSPH, SINPH
      REAL E0
      save icall
c
c     begin execution
c
      IF(IEOM.EQ.0)THEN
         DMASS = RMUMAS
      ENDIF
      IF(IEOM.EQ.1)THEN
         DMASS = ELEMAS
      ENDIF
      IF(IEOM.EQ.2)THEN
         DMASS = RKMASS
      ENDIF
      BMASS = AMASS
      IF(BWMASS.EQ.1)THEN
c
c     Code based on what is in gdecay.f for introducing a Breit-Wigner Gamma width (non-relativistic)
c
         CALL GRNDM(RNDM,1)
         BMASS = AMASS + 0.5*BWGAMMA*TAN(3.1415927*(RNDM - 0.5))
      ENDIF
      CALL GDECA2(BMASS,DMASS,DMASS,PCM)
      E0 = SQRT(PTOT*PTOT + BMASS*BMASS)
      BETA(1) = 0.0
      BETA(2) = 0.0
      BETA(3) = -PTOT/E0
      BETA(4) = E0/BMASS
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
     2             (pcm(ixyz,2),ixyz=1,3),ieom
1     format(/,2x,'ptot,p1',4e14.5,
     1       /,2x,'pd1 ',3e14.5,
     2       /,2x,'pd2 ',3e14.5,
     3       /,2x,'beta ',4e14.5,
     4       /,2x,'gkin1 ',3e14.5,
     5       /,2x,'gkin2 ',3e14.5,
     6       /,2x,'pcm1 ',3e14.5,
     7       /,2x,'pcm2 ',3e14.5,
     7       /,2x,'ieom ',i2)
      endif
      RETURN
      END
