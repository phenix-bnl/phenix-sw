      SUBROUTINE DOUT_PUT_DST

C  to write out the zebra banks belonging to Geant to a Zebra FZ file every
C   event
C  SRTonse 24-JUL-1992

      IMPLICIT NONE
#include "gcbank.inc"
#include "gcnum.inc"
#include "guphnx.inc"
#include "subevt.inc"

      INTEGER I
      logical logwrn /.true./
      logical logerr /.false./

      if(zebra_output.eq.0)then
         return
      endif


C  loop through out keys and write out

      DO I = 1,NOUT_KEYS
        IF(COUT_KEYS(I) .EQ. 'KINE' .AND. NTRACK .GT. 0)THEN
          call u_put_ds(ixdiv,JKINE,'PISA','KINE','    ','L')
        ELSE IF(COUT_KEYS(I) .EQ. 'VERT' .AND. NVERTX .GT. 0)THEN
          call u_put_ds(ixdiv,JVERTX,'PISA','VERT','    ','L')
        ELSE IF(COUT_KEYS(I) .EQ. 'HITS' .AND. NTRACK .GT. 0)THEN
          call u_put_ds(ixdiv,JHITS,'PISA','HITS','    ','L')
        ELSE IF(COUT_KEYS(I) .EQ. 'JXYZ' .AND. NTRACK .GT. 0)THEN
          call u_put_ds(ixdiv,JXYZ,'PISA','JXYZ','    ','L')
        ELSE

c    cfm: Need to have at least one variable on the DOUT command
c         DIGI is just a way to get the file opened in GUINIT

          if(cout_keys(i).ne.'DIGI')then
             logerr = .true.
             if(logwrn)then
                write(6,49)i,nout_keys
49     format(/,3x,'DOUT_PUT_DST <E>: below is a one-time warning ',
     1       'message with I =',i3,' and NOUT_KEYS = ',i3)
                WRITE(6,50)' DOUT_PUT_DST: Undefined bank name ',
     &          COUT_KEYS(I)
50              FORMAT(A,A4)
             endif   ! warning check
          endif  ! bypass if DIGI was used in DOUT line
        END IF
      END DO
      if(logerr)logwrn=.false.

C  end of event empty bank ('Z')

      IF(END_EVTFLG)THEN
        call u_put_ds(ixdiv,0,'PISA','END2','    ','Z')
      ELSE
        call u_put_ds(ixdiv,0,'PISA','END1','    ','Z')
      END IF
      RETURN
      END
