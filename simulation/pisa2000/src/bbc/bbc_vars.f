**CMZ :          31/01/95  16.54.47  by  Charles F. Maguire
**CMZU:  2.03/00 11/06/93  13.46.31  by  Toru Sugitate
**CMZ :  2.02/00 05/06/93  19.11.12  by  Charles F. Maguire
*-- Author :    Toru Sugitate   31/05/93

C=====================================================================
      SUBROUTINE BBC_VARS(BBCCOLOR,BBCSEENN,BBMEDABS,BBMEDATT,
     &                    BBMEDBAC,BBMEDCOV,BBMEDFRO,BBMEDMOT,
     &                    BBMEDPMT,BBMEDQUA,BBMEDSTR,BBABSORB,
     &                    BBATTACH,BBBACKBD,BBCOVERT,BBFRONTB,
     &                    BBPMTSIZ,BBQUARTZ,BBSPACIN,BBSTRUCT,
     &                    BBZPOSIT)
C=====================================================================

C     DESCRIPTION: This subroutine saves the geometry parameters for the
C                  Beam-Beam Counter in a Zebra bank.

C     ARGUMENTS: All the arguments are of the variables which define the
C                Beam-Beam Counter geometry.

C     MAP:
C           1) CALLED BY; VER
C           2) CALLS;     MZFORM,MZBOOK

C     AUTHOR: Toru Sugitate [Hiroshima University] 31 June 1993

C     REVISIONS:  Date     Name               Modification
C               ------   ------  ----------------------------------------------
C               6/5/93   Maguire  Change to "official" BBC designation
C=====================================================================
C     GLOBAL SPECIFICATIONS:
      IMPLICIT      NONE
#include "fstore.inc"
#include "sublink.inc"
#include "fpblink.inc"
 
C=====================================================================
C     EXTERNAL SPECIFICATIONS:

      INTEGER       BBCCOLOR     ! Color (visible/invisible)
      INTEGER       BBCSEENN     ! visible or invisible
      INTEGER       BBMEDABS     ! Electron absorber material #
      INTEGER       BBMEDATT     ! Attachment material #
      INTEGER       BBMEDBAC     ! BackBoad material #
      INTEGER       BBMEDCOV     ! Barrel cover material #
      INTEGER       BBMEDFRO     ! FrontBoad material #
      INTEGER       BBMEDMOT     ! MotherVomlume material #
      INTEGER       BBMEDPMT     ! PMT material #
      INTEGER       BBMEDQUA     ! Quartz material #
      INTEGER       BBMEDSTR     ! StructureTube material #
 
      REAL          BBABSORB(3)  ! Electron absorber with Rmax,Rmin,thick
      REAL          BBATTACH(10) ! Attachment with Rmin,Rmax,thick
      REAL          BBBACKBD(3)  ! BackBoad with Rmin,Rmax,thick
      REAL          BBCOVERT     ! BarrelCover thickness
      REAL          BBFRONTB(3)  ! FrontBoad with Rmin,Rmax,thick
      REAL          BBPMTSIZ(3)  ! PMT size with Rmin,Rmax,thick
      REAL          BBQUARTZ(10) ! Quartz size with Rmin,Rmax,thick
      REAL          BBSPACIN     ! Space btwn straight edges of adj. element.
      REAL          BBSTRUCT(3)  ! StructureTube with Rmin,Rmax,thick
      REAL          BBZPOSIT(2)  ! BBC Z-position, (1) for +Z
 
C=====================================================================
C     INTERNAL SPECIFICATIONS:

      INTEGER       IOPARA
      INTEGER       IP           ! loop pointer
 
C=====================================================================
C               EXECUTABLE STATEMENTS FOLLOW BELOW
c---------------------------------------------------------------------

      WRITE(6,*) ' <I> BBC: Call to BBC_PARS.'
 
c---------------------------------------------------------------------
c Specify the bank format of 'PPRA'.

      CALL MZFORM('PARA','-F',IOPARA)
 
c---------------------------------------------------------------------
c Create the bank 'PPRA' to save the geometry parameters.

c       IXDIV_FR:      defined in FSTORE.
c       LFB_PARA:      return address of the created bank.
c       LFB_PARA:      return assress of the supporting bank.
c       JBIAS=1:       create top-level bank.
c       'PARA':        bank name.
c       NL=0:          no links.
c       NS=0:          no structual links.
c       BBC_PARA_ND:   # of data words, defined in FPLINK.
c       IOPARA:        IP format word, passed from MZBOOK.
c       NZERO=0:       the whole bank is cleared.
 
      CALL MZBOOK(IXDIV_FR,LFB_PARA,LFB_PARA,1,'PARA',0,0,
     &            BBC_PARA_ND,IOPARA,0)
 
c---------------------------------------------------------------------
c Copy the geometry paramters in 'PPRA' bank.


      IQF(LFB_PARA + OFBA_COL)   = BBCCOLOR
      IQF(LFB_PARA + OFBA_SEE)   = BBCSEENN
      IQF(LFB_PARA + OFBA_MAB)   = BBMEDABS
      IQF(LFB_PARA + OFBA_MAT)   = BBMEDATT
      IQF(LFB_PARA + OFBA_MBA)   = BBMEDBAC
      IQF(LFB_PARA + OFBA_MCO)   = BBMEDCOV
      IQF(LFB_PARA + OFBA_MFR)   = BBMEDFRO
      IQF(LFB_PARA + OFBA_MMO)   = BBMEDMOT
      IQF(LFB_PARA + OFBA_MPM)   = BBMEDPMT
      IQF(LFB_PARA + OFBA_MQU)   = BBMEDQUA
      IQF(LFB_PARA + OFBA_MST)   = BBMEDSTR
 
      QF(LFB_PARA + OFBA_ABS)    = BBABSORB(1)
      QF(LFB_PARA + OFBA_ABS +1) = BBABSORB(2)
      QF(LFB_PARA + OFBA_ABS +2) = BBABSORB(3)
      DO IP=1,10
        QF(LFB_PARA+OFBA_ATT+IP-1) = BBATTACH(IP)
      ENDDO
      QF(LFB_PARA + OFBA_BAC)    = BBBACKBD(1)
      QF(LFB_PARA + OFBA_BAC +1) = BBBACKBD(2)
      QF(LFB_PARA + OFBA_BAC +2) = BBBACKBD(3)
      QF(LFB_PARA + OFBA_COV)    = BBCOVERT
      QF(LFB_PARA + OFBA_FRO)    = BBFRONTB(1)
      QF(LFB_PARA + OFBA_FRO +1) = BBFRONTB(2)
      QF(LFB_PARA + OFBA_FRO +2) = BBFRONTB(3)
      QF(LFB_PARA + OFBA_PMT)    = BBPMTSIZ(1)
      QF(LFB_PARA + OFBA_PMT +1) = BBPMTSIZ(2)
      QF(LFB_PARA + OFBA_PMT +2) = BBPMTSIZ(3)
      DO IP=1,10
        QF(LFB_PARA+OFBA_QUA+IP-1) = BBQUARTZ(IP)
      ENDDO
      QF(LFB_PARA + OFBA_SPA)    = BBSPACIN
      QF(LFB_PARA + OFBA_STR)    = BBSTRUCT(1)
      QF(LFB_PARA + OFBA_STR +1) = BBSTRUCT(2)
      QF(LFB_PARA + OFBA_STR +2) = BBSTRUCT(3)
      QF(LFB_PARA + OFBA_ZPO)    = BBZPOSIT(1)
      QF(LFB_PARA + OFBA_ZPO +1) = BBZPOSIT(2)

      RETURN
      END
