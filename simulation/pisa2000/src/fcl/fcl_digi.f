      subroutine fcl_digi

      Implicit none

C     Formal Argument Declarations
C     ----------------------------

C     External Functions
C     ------------------

C     Global Declarations
C     -------------------
#include "guphnx.inc"
#include "fstore.inc"
#include "pisascr.inc"
#include "sublink.inc"
#include "fpylink.inc"
#include "fcl.inc"

C       Local Declarations
C       ------------------
      character*4 cudet
      integer*4   iudet
      equivalence (iudet, cudet)

      INTEGER   NHMAX
      PARAMETER (NHMAX=500000)
      REAL      HITSH(8,NHMAX)
      INTEGER   ITRAH(NHMAX)
      INTEGER   NUVS(3) /3*0/
      INTEGER   NUBV(3,NHMAX)
C  use scratch area. Choose offsets conservatively
      EQUIVALENCE(HITSH(1,1),TEMP(1)),(ITRAH(1),ITEMP(80000)),
     &     (NUBV(1,1),ITEMP(120000))
      INTEGER   NHITS, IPOINT
      real      xnor
      real      dele, pos_x, pos_y, pos_z, partl, px, py, pz
      integer   icall /0/
      integer   blen, i, j, iodcal
      character*20 chform
      INTEGER ISUB


c     Variables for track ancestry of shower particles

      integer  spart,snvert,ncycle,ittest,last_trk
      real     svrtx(3), pvrtx(4)

      integer numTracks /10/

      save iodcal

      save numTracks


C    Executable code
C    ===============

C     Initialize

      do i=1,90
        fclOut(i) = 0
      enddo


      IF(ICALL.EQ.0) THEN       ! Initialize
c        print 999
c 999     format(/,3x,'Call to FCL_DIGI'//)
         ICALL = 1

C     CVOLU_OPT(I,NSET)       ith volume option for set number NSET
C     NSET: VER, PAD, FCL, ITR, CRK, TRD, TOF,
C     EMC, PHO, MUO  ( 1 ---> 10 )
C     same for RVOLU_OPT and IVOLU_OPT

         if(cvolu_opt(2,19).ne.'P_ID') then
            write(6,*)
     &           ' <E> FCL_DIGI: inconsistent HIT structure - STOP!'
            stop
         endif
         if(cvolu_opt(4,19).NE.'ELEM') then
            write(6,*)
     &      ' <W> FCL_DIGI: You will have a hard time getting output '
            write(6,*)
     &      '               valid DIGI options are: ELEM '
         endif

         CHFORM = '1I / 5F 3I 2F'
         call mzform(NAME_SUBVOLUMES(1),CHFORM,iodcal) ! book characteristic
         
        endif

c       book IO characteristic for event banks



        do i=1,90
          
          CUDET = NAME_SUBVOLUMES(I) ! namesv variable in FCL subroutine
          
          CALL GFHITS('FCL '    ! set identifier
     &         ,CUDET           ! detector identifier
     &         ,1               ! dimension of path identification
     &         ,8               ! dimension of hit array
     &         ,NHMAX           ! maximum number of returned hits
     &         ,0               ! take all tracks
     &         ,NUVS            ! volume descriptor
     &         ,ITRAH           ! array of hit producing tracks
     &         ,NUBV            ! volume descriptor numbers on output
     &         ,HITSH           ! hit values
     &         ,NHITS)          ! number of hits in this detector


          IF(CVOLU_OPT(4,19).EQ.'ELEM') THEN ! elementary bank
            blen = mFY_FCL*NHITS + 10

            lfy_fcl(i,1) = 0
            call mzbook(ixdiv_fe,
     &           lFY_FCL(I,1),
     &           lFY_FCL(I,1), 1,
     &           NAME_SUBVOLUMES(I),
     &           0,
     &           0,
     &           blen,
     &           iodcal,
     &           -1)
            
            DO j = 1,NHITS      ! loop on stored hits /module
              fclOut(i) = hitsh(4,j) + fclOut(i)
            enddo
            iqf(lFY_FCL(i,1) + 1) = 1 ! put length into mother bank
            
            IPOINT = lFY_FCL(i,1) + 2
            
            qf(IPOINT + OFYM_x) = 0
            qf(IPOINT + OFYM_y) = 0
            qf(IPOINT + OFYM_z) = 0
            qf(IPOINT + OFYM_del) = fclOut(i)
            qf(IPOINT + OFYM_px) = 0
            qf(IPOINT + OFYM_py) = 0
            qf(IPOINT + OFYM_pz) = 0
            iqf(IPOINT + OFYM_partl) = i
            iqf(IPOINT + OFYM_itra)   = i
            iqf(IPOINT + OFYM_layer) = i
            call trkstack(i)    ! put track # in PISA track stacker
            
            
          endif
        enddo
        
        
        return
        end
      
      
