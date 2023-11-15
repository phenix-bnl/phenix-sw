*CMZ :  2.04/00 01/06/93  15.34.37  by  Charles F. Maguire
*-- Author :    Brian Cole :: Nevis   11/11/92
 
c       ============================================================
        Integer Function EGZEVT ( )
c       ============================================================

c       Description:-
c       =============


c       Author:         Brian Cole
c       Creation Date:  12-Nov-1992
c       Modified:

c       Implicit inputs, outputs, side effects:-
c       ========================================
 
        Implicit None
 

c       Global Specifications:-
c       =======================
 
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEEP,GUEVGEN.
#include "guevgen.inc"
*KEND.
 

c       Set the size of the Store used for EGZ I/O
c       ==========================================
        Integer         EGZSIZ
        Parameter       (EGZSIZ = 200000)
 
        Integer         IEGZSTR (EGZSIZ)
        Common /EGZC/ IEGZSTR
 

c       External Specifications:-
c       =========================
 
        Integer         EGZOPEN           ! Routine to open EGZ file
        Integer         EGZRUN            ! Routine to read EGZ run header
        Integer         EGZIN             ! Routine to read EGZ event
 

c       Local Specifications:-
c       ======================
 

c       EGZ-related variables
c       =====================
        Integer         MAX_RHDR          ! Size of array for run header
        Integer         MAX_EHDR          ! Size of array for event header
 
        Parameter       (MAX_RHDR = 1)    ! (not interested in EG-specific hdr)
        Parameter       (MAX_EHDR = 1)    ! (not interested in EG-specific hdr)
 
        Real            RUNHDR (MAX_RHDR) ! generator-specific run parametes
        Real            EVTHDR (MAX_EHDR) ! generator-specific event header
 
        Integer         NAPROJ, NATARG    ! Mass number of projectile and target
        Integer         NZPROJ, NZTARG    ! Charge of projectile and target
        Integer         NRUNHDR, NEVTHDR  ! Number of words in Run, Event hdrs
        Integer         NEVPRT            ! Number of particles in event
 
        Real            BMIN, BMAX        ! Min. max. impact parameters
        Real            ROOTS             ! Center-of-mass energy in collisions
        Real            VERSION           ! Version of event generator
 
        Character*8     EGCODE            ! Event generator name
 
        Integer         EGTYPE (MAX_MXTOT) ! EGZ particle types
        Integer         MCTYPE (MAX_MXTOT) ! PDG monte-carlo particle types
        Integer         PPARNT (MAX_MXTOT) ! Particle parent info (not used)
 

c       Local working variables
c       =======================
        Logical         FIRST             ! Logical indicating 1st event or not
        Save            FIRST
 

c       NOTE: Asterisked variables were flagged by HP compiler as not used
c             These are commented out pending further consideration.

*       Integer         IDXPRT            ! Pointer to particle in event
        Integer         ISUBP             ! Index over particles in sub-event
        Integer         OPN_STATUS        ! Status value returned from EGZOPEN
*       Integer         NREMAIN           ! # of particles remaining in event
*       Integer         NSTORE            ! # of part. to be stored in subevent
*       Integer         NUSER_PAR         ! # of run header words
        Integer         RD_STATUS         ! Status value returned from EGZIN
 
        Data            FIRST /.TRUE./
 

c       Executable Statements:-
c       =======================
 
        EGZEVT = -1
 
        If (FIRST) Then
          FIRST = .FALSE.

c         Open input file
c         ===============
          Call EGZINIT (EGZSIZ)
          OPN_STATUS = EGZOPEN (LUN_DMCI, CDMCI_FILE, 'I')
          If (OPN_STATUS .NE. -1) Then
            Write (*, 901)
            EGZEVT = OPN_STATUS
            Go to 9999
          End if

c         Read run header
c         ===============
          RD_STATUS = EGZRUN (EGCODE, VERSION, NZPROJ, NAPROJ,
     &                        NZTARG, NATARG, ROOTS, BMIN, BMAX,
     &                        NRUNHDR, RUNHDR, MAX_RHDR)
        End if
 

c       Read complete event from EGZ file
c       =================================
        RD_STATUS = EGZIN (NUMEVT, NEVTHDR, EVTHDR, MAX_EHDR, NEVPRT,
     &                     MAX_MXTOT, PPTOT, EGTYPE, PPARNT)
        If (RD_STATUS .NE. -1) Then
          If (RD_STATUS .EQ. 2) Then
            Write (*, 902)              ! End of file
          Else
            Write (*, 902)              ! Read error
          End if
 
          EGZEVT = RD_STATUS
          Go to 9999
        End if
 

c       Set number of particles in PISA's internal complete-event list
c       ==============================================================
        MXTOT = MIN (NEVPRT, MAX_MXTOT)
 

c       Translate generator-specific ID's to PDG standard id's
c       ======================================================
        Call EGZTRAN (MXTOT, EGTYPE, MCTYPE)
 
        Do 100 ISUBP = 1, MXTOT

c         Call GEANT (??) (FCA-supplied) routine to translate
c           Particle-data-group ID's to GEANT ID's
c         ===================================================
          Call SGPDGE (MCTYPE (ISUBP), IDTOT (ISUBP))
100     Continue
 
        EGZEVT = -1
 
901     Format (1H , 'EGZEVT - Error opening requested input file')
902     Format (1H , 'EGZEVT - FZ read error on input file')
 
9999    Continue
 
        Return
        End
