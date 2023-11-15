*CMZ :  2.04/00 18/03/93  12.28.35  by  S.R.Tonse
*-- Author :
*-- Author :
**
c       ============================================================
        Subroutine UFPATH (IUSET, IUDET, NUMBV, NLEV, LNAME, LNUMB)
c       ============================================================

c       Description:-
c       =============

c         Routine to augment GFPATH. Called with detector and set
c           names not indices. GFPATH returns the full path through
c           to volume tree for sensitive detectors defined using
c           GSDETV. NUMBV should contain only indeterminate
c           volume numbers (i.e. for levels at which there are
c           multiple copies of a volume). LNAME and LNUMB contain
c           the full path through the volume tree on return, and
c           NLEV contains the number of levels in the full path.


c       Author: BAC
c       ===========

c       Creation Date: 22-SEP-1992
c       ==========================

c       Modification history
c       ====================

c       Implicit inputs, outputs, side effects:-
c       ========================================
        Implicit None

c       Global Specifications:-
c       =======================
*KEEP,GCBANK.
#include "gcbank.inc"
*KEND.

c       Sub-program argument specifications:-
c       =====================================
        Character*4     IUSET           ! Set name
        Character*4     IUDET           ! Detector name
        Integer         NUMBV (*)       ! Detector volume numbers
        Integer         NLEV            ! Number of volume tree levels for det.
        Integer         LNAME (*)       ! Volume tree names for det.
        Integer         LNUMB (*)       ! Volume tree numbers for det.

c       External Specifications:-
c       =========================

c       Local Specifications:-
c       ======================
        Integer         JS      ,       ! Link into detector banks for set
     &                  NDET    ,       ! Number of defined hit sets
     &                  NSET    ,       ! Number of detectors in set
     &                  IDET    ,       ! Detector index
     &                  ISET            ! Set index

c       Executable Statements:-
c       =======================

c       Find Set index from list of GEANT set names
c       ===========================================
        NSET = IQ (JSET - 1)
        CALL GLOOK (IUSET, IQ (JSET + 1), NSET, ISET)
        IF (ISET .LE. 0) Then
          NLEV = 0
          Go to 999
        End if

c       Find link to set bank
c       =====================
        JS = LQ (JSET - ISET)
        IF (JS .LE. 0) Then
          NLEV = 0
          Go to 999
        End if

c       Find detector index from list of GEANT det. names
c       =================================================
        NDET = IQ (JS - 1)
        CALL GLOOK (IUDET, IQ (JS + 1), NDET, IDET)
        IF (IDET .LE. 0) Then
          NLEV = 0
          Go to 999
        End if

c       Now call GFPATH using found set and det. indices
c       ================================================
        Call GFPATH (ISET, IDET, NUMBV, NLEV, LNAME, LNUMB)
999     Continue
        Return
        End
