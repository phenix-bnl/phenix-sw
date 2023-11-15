      SUBROUTINE GSNGTR(X,P,IACT,SNEXT,SNXT,SAFE,INSIDE)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *    Routine to determine the shortest distance from the point   *
C.    *    X(1-3) to the boundary of the shape of type GTRA defined    *
C.    *    by the parameters P along the vector X(4-6). If INSIDE is   *
C.    *    1 then the point is inside the shape and the distance is    *
C.    *    returned as SNEXT. If INSIDE is 0 then the point is         *
C.    *    outside the shape and if the line hits the shape then       *
C.    *    if the new distance is less than the                        *
C.    *    old value of SNEXT the new distance is returned as SNEXT.   *
C.    *                                                                *
C.    *          Called by : GNEXT, GTNEXT                             *
C.    *          A.C.McPherson   22nd April 1985.                      *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.

c     PHENIX version of gsngtr

c     Author: C.F. Maguire
c     Creation date: December 8, 1996 

c     Purpose: Prevent the routine from going into an infinite loop.
c              The problem is specific to the use of the GTRA volume shape.
c              New version (IT96B, 10/96) of DC geometry caused problems
c              with infinite looping (see section below).  Version here
c              monitors how many times a reverse GO TO loop is executed, and
c              breaks out of infinite loop when necessary.  The passage in
c              the routine can be monitored via the NTUPLE 88 which is
c              engaged whenever the SWIT(8) option is set to 3 or 4.
c              The reverse GO TO count is monitored by the LOOP_110 variable
c              which is checked to see that it does not execeed 200.  Studies
c              with the diagnostic NTUPLE showed that this upper limit should
c              be valid for all well-behaved tracks.

#include "gconsp.inc"
#include "gcunit.inc"
#include "gckine.inc"
#include "gcflag.inc"
#include "subevt.inc"


      integer icall /0/
      integer iwrite /0/

      integer loop_110

      integer itra_old
      integer nsub_old
      integer ntru_old

      integer kloop_110
      integer kcall
      integer kwarn

      save kcall, kloop_110
      save itra_old, nsub_old, ntru_old


c     NTUPLE diagnosis

      integer np88
      parameter (np88=6)
      character*8 ch88(np88) /'ITRACK', 'KCALL', 'KLOOP110', 'KWARN',
     1                        'SUBEVENT', 'EVENT'/
      real evt88(np88)


      DOUBLE PRECISION X0,Y0,DXDZ,DYDZ,A,B,C,DISC,X1,X2,X3,SN,CP,SMALL
      PARAMETER (SMALL=1E-10)

      DIMENSION X(6),P(30),SN(2,5),IOUT(5),X0(4),Y0(4),DXDZ(4),DYDZ(4)
C.
C.                ---------------------------------------------

      if(icall.eq.0)then
         if(iswit(8).eq.1)then
            iwrite = 1
         else
            iwrite = 0
         endif

c     NTUPLE booking, if requested

         if(iswit(8).eq.3.or.iswit(8).eq.4)then
           call hbookn(88,'GSNGTR Diagnosis',np88,'GEANHIST',
     1                 25000,ch88)
           write(6,*)'  NTUPLE 88 is booked'
         endif
         if(iwrite.eq.1)write(lout,1)
 1       format(' Call to GSNGTR')
         icall = 1
         kcall = 0
         kloop_110 = 0
         kwarn = 0
         itra_old = itra
         ntru_old = ntru_evt
         nsub_old = nsub_evt
      endif  ! end initialization

c     check if new track  (PHENIX insertion)

      if(itra.ne.itra_old.or.ntru_old.ne.ntru_evt.or.
     +   nsub_old.ne.nsub_evt)then
         evt88(np88) = ntru_evt
         evt88(np88-1) = nsub_evt
         evt88(1) = itra
         evt88(2) = kcall
         evt88(3) = kloop_110
         evt88(4) = kwarn
         if(iswit(8).eq.3.or.iswit(8).eq.4)then
            call hfn(88,evt88)
         endif
         kcall = 0
         kloop_110 = 0
         kwarn = 0
         itra_old = itra
         ntru_old = ntru_evt
         nsub_old = nsub_evt
      else
         kcall = kcall + 1
      endif  ! new track check

C               Compute Safety distance

      IF(IACT.LT.3) CALL GSAGTR(X,P,SAFE,INSIDE)
      SNXT=BIG
      IF (IACT .EQ. 0) GO TO 999
      IF (IACT .EQ. 1) THEN
        IF (SNEXT .LT. SAFE) GO TO 999
      ENDIF

C               First compute the distance along the line to the
C               boundaries.

C               The distance to the planes defined by z=+/-P(1).

      IF(X(6).EQ.0.0) THEN
          SN(1,1)=BIG
          SN(2,1)=BIG
          GOTO 10
      ENDIF
      SN(1,1)=(-P(1)-X(3))/X(6)
      SN(2,1)=(P(1)-X(3))/X(6)
      IF(X(6).GT.0.0) GO TO 10
      ST=SN(2,1)
      SN(2,1)=SN(1,1)
      SN(1,1)=ST
   10 CONTINUE

C               The distance to the remaining four surfaces.

      DO 20 I=1,4
      X0(I)=P(I*4+11)
      Y0(I)=P(I*4+12)
      DXDZ(I)=P(I*4+13)
      DYDZ(I)=P(I*4+14)
   20 CONTINUE

      DO 65 I=1,4
      J=I+1
      IF(J.EQ.5) J=1

      A=(X(4)-DXDZ(I)*X(6))*(DYDZ(J)-DYDZ(I))*X(6) -
     +(X(5)-DYDZ(I)*X(6))*(DXDZ(J)-DXDZ(I))*X(6)

      B=(X(4)-DXDZ(I)*X(6))*(Y0(J)-Y0(I)+(DYDZ(J)-DYDZ(I))*X(3)) +
     +(X(1)-X0(I)-DXDZ(I)*X(3))*(DYDZ(J)-DYDZ(I))*X(6) -
     +(X(5)-DYDZ(I)*X(6))*(X0(J)-X0(I)+(DXDZ(J)-DXDZ(I))*X(3)) -
     +(X(2)-Y0(I)-DYDZ(I)*X(3))*(DXDZ(J)-DXDZ(I))*X(6)

      C=(X(1)-X0(I)-DXDZ(I)*X(3))*(Y0(J)-Y0(I)+(DYDZ(J)-DYDZ(I))*X(3))
     + - (X(2)-Y0(I)-DYDZ(I)*X(3))*(X0(J)-X0(I)+(DXDZ(J)-DXDZ(I))*X(3))

      IOUT(I+1)=0
      IF(C.GT.0.0) IOUT(I+1)=1

C             The solutions are in the normal form:
C             s = (-B+/-SQRT(B*B-4.0*A*C))*0.5/A

      SN(1,I+1)=BIG
      SN(2,I+1)=BIG
      IF(ABS(A).GT.1.0E-10) GO TO 30

C             A = 0 only one solution.

      IF(ABS(B).LT.1.0E-10) GO TO 60

      SN(1,I+1)=-C/B
      GO TO 60

   30 CONTINUE
      IF(ABS(C).GT.1.0E-10) GO TO 40
      SN(1,I+1)=0.0
      SN(2,I+1)=0.0
      IF(ABS(B).LT.1.0E-10) GO TO 60
      SN(1,I+1)=-C/B
      IF(C.EQ.0.0) SN(1,I+1)=SIGN(SMALL,B)
      SN(2,I+1)=-B/A
      GO TO 50

   40 CONTINUE
      DISC=B*B-A*C*4.0
      IF(DISC.LT.0.0) GO TO 60
      IF(DISC.GT.0.0) DISC=SQRT(DISC)
      SN(1,I+1)=(-B-DISC)*0.5/A
      SN(2,I+1)=(-B+DISC)*0.5/A

   50 CONTINUE
      IF(SN(2,I+1).GT.SN(1,I+1)) GO TO 60
      ST=SN(2,I+1)
      SN(2,I+1)=SN(1,I+1)
      SN(1,I+1)=ST

   60 CONTINUE

      DO 65 K=1,2
      IF(ABS(SN(K,I+1)).GT.1.0E+05.OR.ABS(SN(K,I+1)).LT.1.0E-05)
     +GO TO 65

      X1=X(1)+SN(K,I+1)*X(4)
      X2=X(2)+SN(K,I+1)*X(5)
      X3=X(3)+SN(K,I+1)*X(6)
      CP=(X1-X0(I)-DXDZ(I)*X3)*(Y0(J)-Y0(I)+(DYDZ(J)-DYDZ(I))*X3)
     + - (X2-Y0(I)-DYDZ(I)*X3)*(X0(J)-X0(I)+(DXDZ(J)-DXDZ(I))*X3)
      CP=CP/SQRT((X0(J)-X0(I)+(DXDZ(J)-DXDZ(I))*X3)**2+
     +   (Y0(J)-Y0(I)+(DYDZ(J)-DYDZ(I))*X3)**2)

      IF(ABS(CP).LT.0.0001) GO TO 65
      IF(ABS(CP/SN(K,I+1)).LT.1.0E-06) GO TO 65
      WRITE(CHMAIL,1020) I,K,SN(K,I+1)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,1021) X
      CALL GMAIL(0,0)
      WRITE(CHMAIL,1022) X1,X2,X3,CP
      CALL GMAIL(0,0)
      WRITE(CHMAIL,1023) A,B,C,DISC
      CALL GMAIL(0,0)
      WRITE(CHMAIL,1024) INSIDE
      CALL GMAIL(0,0)
      WRITE(CHMAIL,1025) X0
      CALL GMAIL(0,0)
      WRITE(CHMAIL,1026) Y0
      CALL GMAIL(0,0)
      WRITE(CHMAIL,1027) DXDZ
      CALL GMAIL(0,0)
      WRITE(CHMAIL,1028) DYDZ
      CALL GMAIL(0,0)
 1020 FORMAT('0 GSNGTR ERROR - I,K =',2I2,' SN =',E13.5)
 1021 FORMAT(' X =',6F15.6)
 1022 FORMAT(' X1,X2,X3 =',3F15.6,' CP =',E15.6)
 1023 FORMAT(' A =',E15.6,' B =',E15.6,' C =',E15.6,' DISC =',E15.6)
 1024 FORMAT(' INSIDE =',I3)
 1025 FORMAT('   X0 =',4E15.6)
 1026 FORMAT('   Y0 =',4E15.6)
 1027 FORMAT(' DXDZ =',4E15.6)
 1028 FORMAT(' DYDZ =',4E15.6)

   65 CONTINUE


C             Have computed the two distances for the z planes and
C             the four surfaces. Combine them accordingly as to
C             whether the point is inside or outside the shape.

      IF(INSIDE.EQ.0) GO TO 80

C             Point is inside shape.

      DO 70 I=1,5
      DO 70 J=1,2
      IF(SN(J,I).GT.0.0.AND.SN(J,I).LT.SNXT) SNXT=SN(J,I)
   70 CONTINUE
      GO TO 999

   80 CONTINUE

C             Point is outside shape.

      IOUT(1)=0
      IF(ABS(X(3)).GT.P(1)) IOUT(1)=1

C             For each of five sets of SN and IOUT, IOUT(I) equal to 1
C             indicates that the point is outside the shape by the Ith
C             test, SN(1,I) is the distance to the first change in the
C             test and SN(2,I) is the distance to the second change.
C             The remaining logic just attempts to find a distance when
C             the line is inside by all five tests, bearing in mind that
C             for some tests the line can start inside, leave and return
C             inside.

      SL=-1.0
      SM=BIG
      SM1=BIG
      DO 100 I=1,5
      IF(IOUT(I).EQ.0) GO TO 90
      IF(SN(2,I).LT.0.0) GO TO 999
      IF(SN(1,I).LT.0.0.AND.SN(2,I).GT.SL) SL=SN(2,I)
      IF(SN(1,I).GT.SL) SL=SN(1,I)
      IF(SN(1,I).GE.0.0.AND.SN(2,I).LT.SM) SM=SN(2,I)
      GO TO 100
   90 CONTINUE
      IF(SN(1,I).LT.0.0.AND.SN(2,I).GE.0.0.AND.SN(2,I).LT.SM) SM=SN(2,I)
      IF(SN(1,I).LT.0.0.OR.SN(1,I).GT.SM1) GO TO 100
      IF(SN(1,I).GE.SN(2,I)) GO TO 100
      SM1=SN(1,I)
      SL1=SN(2,I)
  100 CONTINUE

C             SL is the largest of the five distances to the first
C             time the line is inside. SM is the smallest to the
C             last time the point is inside. SM1 is the smallest
C             distance to when the line is temporarily outside
C             one of the tests.

      IF(SM.LE.SL) GO TO 999
      IF(SM1.GT.SL) GO TO 130


c     This 110 reverse GO TO loop is the source of the infinite looping

      loop_110 = 0
  110 CONTINUE
      kloop_110 = kloop_110 + 1
      loop_110 = loop_110 + 1

C             In this loop SL is updated by the return after SM1
C             if SM1 is less than SL.

      SL=SL1
      IF(SM.LE.SL) GO TO 999
      SM1=SM

      DO 120 I=1,5
      IF(IOUT(I).EQ.1) GO TO 120
      IF(SN(2,I).LE.SL.OR.SN(1,I).GT.SM1) GO TO 120
      IF(SN(1,I).GE.SN(2,I)) GO TO 120
      SM1=SN(1,I)
      SL1=SN(2,I)
  120 CONTINUE

      IF(SM1.GT.SL) GO TO 130


c   CFM: Infinite loop problem here ?
c      GO TO 110

      if(loop_110.lt.200)then
         go to 110
      else
         kwarn = kwarn + 1
         go to 130
      endif
  130 CONTINUE

      IF(SL.LT.SNXT) SNXT=SL

  999 CONTINUE

      return
      END
