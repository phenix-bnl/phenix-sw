*+PATCH,//BBCFORTR/BBCFORTR
*+DECK,bbc_user.
**CMZ :          31/01/95  16.56.13  by  Charles F. Maguire
**CMZU:  2.03/00 12/06/93  11.54.37  by  Toru Sugitate
**-- Author :    Toru Sugitate   07/06/93
C=====================================================================
        SUBROUTINE BBC_USER
C=====================================================================

C    DESCRIPTION: This is a user code for the Beam-Beam Counter.

C    ARGUMENTS: (none)

C    MAP:
C           1) CALLED BY; G_USER2
C           2) CALLS;     (none)

C    AUTHOR: Toru Sugitate [Hiroshima University]     07 June 1993

C    REVISIONS:  Date     Name               Modification
C               ------   ------  ----------------------------------------------
C               09/21/93 Toru    Minor changes to suppress messages.

C=====================================================================
C     GLOBAL SPECIFICATIONS:
      IMPLICIT      NONE
C    GUPHNX contains "variables of general interest", data-card defs, flags.
C           in particular, CVOLU_OPT is used in this subroutine.
C           Unfortunately, this also defines the local variables HIT_ALL,
C           RMAG1_FSLOPE, and RMAG2_FSLOPE which are not used here and
C           therefore result in undefined variable warning messages
C           when this is compiled.
#include "guphnx.inc"

C    FSTORE has the common where the detector specific data are stored.
#include "fstore.inc"

C    FBLINK contains the zebra links for the beam-beam detector. In particular
C           it has the pointers into the common inside FSTORE where the beam-
C           beam detector event data and geometry parameters are stored.
C           It also contains various offset parameters related the the
C           structure of the beam-beam detector data banks.
#include "sublink.inc"
#include "fpblink.inc"

C    SUBEVT contains data related to the sub-event structure, for example,
C           the sub-event number and the true event number.
#include "subevt.inc"

C=====================================================================
C     EXTERNAL SPECIFICATIONS:
C     (none)
C=====================================================================
C     INTERNAL SPECIFICATIONS:

c---------------------------------------------------------------------
c Local variables.

      INTEGER      ICALL/    0/ !
      SAVE         ICALL
      INTEGER      IP           ! Loop pointer
      INTEGER      IMUL         ! Loop pointer on multiplicity
      INTEGER      BMUL         ! multiplicity
      INTEGER      LUNBBC       ! Logical unit for logging.
      INTEGER      BBHITPMT     ! Encoded PMT address
      INTEGER      BBHITPID     ! Particle ID
      INTEGER      LF_B         ! offset into mother bank.
 
      REAL         BBHITPOS(3)  ! Hit positon in a Master Refernce Sys.
      REAL         BBHITMOM(3)  ! Momentum of hit particle
      REAL         BBHITDEL     ! Energy loss
      REAL         BBHITTOF     ! TOF value
      REAL         BBHITLEN     ! Path length

c---------------------------------------------------------------------
c Initialize default values for Namelist.

      PARAMETER   (LUNBBC=6)
C=====================================================================
C               EXECUTABLE STATEMENTS FOLLOW BELOW
c---------------------------------------------------------------------

      IF(ICALL.EQ.0) THEN
        WRITE(6,*) ' <I> BBC: call to BBC_USER. '
      ENDIF
c---------------------------------------------------------------------
c extract from the Zebra bank for this event.

      IF(CVOLU_OPT(4,2).EQ.'BMAP') THEN
        BMUL=IQF(LFB_MAP(1)+1)
      ELSEIF(CVOLU_OPT(4,2).EQ.'BCAL') THEN
        BMUL=IQF(LFB_CAL(1)+1)
      ENDIF ! map or cal
      IF(BMUL.GT.0) THEN
CTS     WRITE(6,*) ' <I> BBC: Hits with multiplicity ',BMUL
        DO IMUL=1,BMUL
          IF(CVOLU_OPT(4,2).EQ.'BMAP') THEN
            LF_B=LFB_MAP(1)+(IMUL-1)*MFB_MAP+2 ! offset into mother bank.
            BBHITPMT    = IQF(LF_B+oFBM_PMT)
            BBHITPOS(1) =  QF(LF_B+oFBM_X  )
            BBHITPOS(2) =  QF(LF_B+oFBM_Y  )
            BBHITPOS(3) =  QF(LF_B+oFBM_Z  )
            BBHITDEL    =  QF(LF_B+oFBM_DEL)
            BBHITTOF    =  QF(LF_B+oFBM_TOF)
            BBHITPID    = IQF(LF_B+oFBM_PID)
            BBHITMOM(1) =  QF(LF_B+oFBM_PX )
            BBHITMOM(2) =  QF(LF_B+oFBM_PY )
            BBHITMOM(3) =  QF(LF_B+oFBM_PZ )
            BBHITLEN    =  QF(LF_B+oFBM_LEN)
          ELSEIF(CVOLU_OPT(4,2).EQ.'BCAL') THEN
            LF_B=LFB_CAL(1)+(IMUL-1)*MFB_CAL+2 ! offset into mother bank.
            BBHITPMT    = IQF(LF_B+oFBC_PMT)
            BBHITPOS(1) =  QF(LF_B+oFBC_X  )
            BBHITPOS(2) =  QF(LF_B+oFBC_Y  )
            BBHITPOS(3) =  QF(LF_B+oFBC_Z  )
            BBHITDEL    =  QF(LF_B+oFBC_DEL)
            BBHITTOF    =  QF(LF_B+oFBC_TOF)
            BBHITPID    = IQF(LF_B+oFBC_PID)
            BBHITMOM(1) =  QF(LF_B+oFBC_PX )
            BBHITMOM(2) =  QF(LF_B+oFBC_PY )
            BBHITMOM(3) =  QF(LF_B+oFBC_PZ )
            BBHITLEN    =  QF(LF_B+oFBC_LEN)
          ENDIF ! map or cal
          IF(ICALL.EQ.0) THEN
            WRITE(LUNBBC,100) IMUL,BMUL
            WRITE(LUNBBC,110) BBHITPMT,BBHITPID,BBHITDEL,BBHITTOF,
     &                        BBHITLEN,(BBHITPOS(IP),IP=1,3),
     &                        (BBHITMOM(IP),IP=1,3)
          ENDIF ! icall
        ENDDO ! loop on multiplicity
        ICALL=1
      ELSE ! bmul=0
        WRITE(LUNBBC,*) ' <I> BBC: No hits in this event.'
      ENDIF ! bmul>0 ?
      RETURN
100   FORMAT(/,'(IMUL/BMUL) ',2I10)
110   FORMAT(' PMT = ',I5,' PID = ',I5,' DEL = ',E15.5,
     &       ' TOF = ',E15.5,' LEN = ',E15.5,/,
     &       ' POS = ',3(E15.5,3X),/,
     &       ' MON = ',3(E15.5,3X))
      END ! bbc_user
