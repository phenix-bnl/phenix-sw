C     File name: INR_DIGI.FOR
C       ---------

C     Original Author: Tom Hemmick
C     Release Date: April 28, 1992

C     Converted to PHENIX upgrades use by C.F. Maguire (June 28, 2001)

c     V. L. Rykov   03-Sep-2003: Adjusted to the extented hit set.

      subroutine inr_digi

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
#include "fpdlink.inc"

C       Local Declarations
C       ------------------
      character*4 cudet
      integer*4   iudet
      equivalence (iudet, cudet)

      INTEGER   NHMAX, NHCOMP, NVDIM
      PARAMETER (NHMAX=5000)       ! Maximum number of hits
      PARAMETER (NHCOMP = 15)      ! Number of hit components
      PARAMETER (NVDIM  = 6)       ! Length of the volume tree
      REAL      HITSH(NHCOMP,NHMAX)
      INTEGER   ITRAH(NHMAX)
      INTEGER   NUVS(NVDIM) /NVDIM*0/
      INTEGER   NUBV(NVDIM,NHMAX)
C  use scratch area. Choose offsets conservatively
      EQUIVALENCE(HITSH(1,1),TEMP(1)),(ITRAH(1),ITEMP(80000)),
     &     (NUBV(1,1),ITEMP(120000))
      INTEGER   NHITS, IPOINT
      real      xnor
      real      pos_x, pos_y, pos_z, dele, partl, px, py, pz
     &        , tofl, xilc, yilc, zilc, xolc, yolc, zolc
      integer   icall /0/
      integer   blen, lf_d, lf_dbase, dmul, imod, ihalf,
     &     iodcal,incnd, isext
      character*20 chform
      INTEGER N_SUBVOLUMES,ISUB
      PARAMETER (N_SUBVOLUMES=20)
      CHARACTER*4 NAME_SUBVOLUMES(N_SUBVOLUMES)
     &                          /'SI01', 'SI02', 'SI03', 'SI04',
     &                           'SI05', 'SI06', 'SI07', 'SI08',
     &                           'SI09', 'SI10', 'SI11', 'SI12',
     &                           'SI13', 'SI14', 'SI15', 'SI16',
     &                           'SI17', 'SI18', 'SI19', 'SI20'/


c---
c---  Data from 'PARA' bank
c---
      INTEGER    N_INR_GEOPAR  ! Position of the last parameters of interest
c                              ! from 'PARA' bank
      PARAMETER (N_INR_GEOPAR = 10)
      Integer zinr_int(N_INR_GEOPAR)
      Real    zinr_real(N_INR_GEOPAR)
      Integer sili_br_nlayers    ! Number of barrel layers
      Integer sili_sideLayers    ! Number of endcap layers
      Integer n_active_layers    ! Total number of installed Si layers
      Integer nhh                ! Number of hit components
      Integer nbrv               ! Number of barrel volume descriptors
      Integer necv               ! Number of endcap volume descriptors
      Integer nv                 ! max(nbrv,necv)
      Equivalence (zinr_int(1), zinr_real(1))
      Equivalence (zinr_int(1), sili_br_nlayers)
      Equivalence (zinr_int(2), sili_sideLayers)
      Equivalence (zinr_int(3), nhh)
      Equivalence (zinr_int(4), nbrv)
      Equivalence (zinr_int(5), necv)

c---
c---  Work variables
c---
      Logical inr_dbg /.FALSE./
      Integer i, j, k
c---
c---
      SAVE inr_dbg, iodcal, icall, zinr_int, n_active_layers

C    Executable code
C    ===============

C     Initialize

      IF(ICALL.EQ.0) THEN       ! Initialize
        print 999
 999    format(/,3x,'Call to INR_DIGI'//)

        if(cvolu_opt(5,3).eq.'NEUT')then
           write(6,998)
 998       format(/,'  INR_DIGI<I>: Neutrals are stored',/)
        else
           write(6,997)
 997       format(/,'  INR_DIGI<I>: Neutrals are not stored',/)    
        endif

        if(cvolu_opt(6,3).eq.'WDBG')inr_dbg = .true.

        Do iPoint = 1, N_INR_GEOPAR
          zinr_int(iPoint) = iqf(lfd_para + iPoint)
          If(inr_dbg)                                      THEN
            write(*,*) zinr_int(iPoint), zinr_real(iPoint)
          Endif
        Enddo
        n_active_layers = sili_br_nlayers + sili_sideLayers
        nv = max(nbrv, necv)
        IF(nv .NE. NVDIM)
     *       write(*,*) 'INR_DIGI -- WARNING: nv NE. NVDIM'
        IF(nhh .NE. NHCOMP)
     *       write(*,*) 'INR_DIGI -- WARNING: nhh NE. NHCOMP'
        ICALL = 1

C     CVOLU_OPT(I,NSET)       ith volume option for set number NSET
C     NSET: VER, PAD, INR, ITR, CRK, TRD, TOF,
C     EMC, PHO, MUO  ( 1 ---> 10 )
C     same for RVOLU_OPT and IVOLU_OPT

         if(cvolu_opt(2,3).ne.'P_ID') then
            write(6,*)
     &           ' <E> INR_DIGI: inconsistent HIT structure - STOP!'
            stop
         endif
         if(cvolu_opt(4,3).NE.'ELEM') then
            write(6,*)
     &      ' <W> INR_DIGI: You will have a hard time getting output '
            write(6,*)
     &      '               valid DIGI options are: ELEM '
         endif

Corrected by V. L. Rykov 09/03/2003
*         CHFORM = '1I / 5F 3I 2F'
         CHFORM = '1I / 5F 3I 9F 6I'
         call mzform(NAME_SUBVOLUMES(1),CHFORM,iodcal) ! book characteristic
         
      ENDIF

c       book IO characteristic for event banks


c###############################################################################
Corrected by V. L. Rykov 09/03/2003
        DO ISUB = 1, n_active_layers
*        DO ISUB=1,N_SUBVOLUMES
           CUDET = NAME_SUBVOLUMES(ISUB) ! namesv variable in INR subroutine

c     extract hit information from Zebra/Geant Hit Banks

           CALL GFHITS('INR '      ! set identifier
     &     ,CUDET                  ! detector identifier
     &     ,nv                     ! dimension of path identification
     &     ,nhh                    ! dimension of hit array
     &     ,NHMAX                  ! maximum number of returned hits
     &     ,0                      ! take all tracks
     &     ,NUVS                   ! volume descriptor
     &     ,ITRAH                  ! array of hit producing tracks
     &     ,NUBV                   ! volume descriptor numbers on output
     &     ,HITSH                  ! hit values
     &     ,NHITS)                 ! number of hits in this detector


C /chp/ if user array hith exceeded, nhits is returned as nhmax+1
           if (nhits .gt. nhmax) then
              write(6,*) '<W> INR (inr_digi.f): number of hits exceeds',
     #             nhmax,' nhits truncated to ',nhmax,' for ',cudet
              nhits = nhmax
           end if

           If(inr_dbg)                   THEN
             write(*,*) NAME_SUBVOLUMES(ISUB)
             write(*,*) 'NHITS =',nhits
             Do i = 1, nhits
               write(*,*) 'Track number =', ITRAH(i)
               write(*,*) 'VOLUMES =',(NUBV(j,i), j = 1, nv)
               write(*,*) 'HITS =', (HITSH(j,i), j = 1, nhh)
             Enddo
           Endif

           If(NHITS.GT.0) THEN


C     TONSE:  go ahead & book banks since we have something to put inside
C  book Zebra bank

              IF(CVOLU_OPT(4,3).EQ.'ELEM') THEN ! elementary bank
                 blen = mFD_SIL*NHITS + 10

                 lfd_sil(isub,1) = 0
                 call mzbook(ixdiv_fe,
     &                       lFD_SIL(ISUB,1),
     &                       lFD_SIL(ISUB,1), 1,
     &                       NAME_SUBVOLUMES(ISUB),
     &                       0,
     &                       0,
     &                       blen,
     &                       iodcal,
     &                       -1)

                 iqf(lFD_SIL(ISUB,1) + 1) = NHITS ! put length into mother bank
                 IPOINT = lFD_SIL(ISUB,1) + 2
                 DO I = 1, NHITS ! loop on stored hits /module
                   call trkstack(itrah(i)) ! put track # in PISA track stacker
                   qf(IPOINT + OFDM_x)      = HITSH(1,I)
                   qf(IPOINT + OFDM_y)      = HITSH(2,I)
                   qf(IPOINT + OFDM_z)      = HITSH(3,I)
                   qf(IPOINT + OFDM_del)    = HITSH(4,I)
                   qf(IPOINT + OFDM_px)     = HITSH(7,I)
                   iqf(IPOINT + OFDM_partl) = HITSH(6,I)
                   iqf(IPOINT + OFDM_itra)  = ITRAH(I)
                   iqf(IPOINT + OFDM_layer) = isub
                   qf(IPOINT + OFDM_py)     = HITSH(8,I)
                   qf(IPOINT + OFDM_pz)     = HITSH(9,I)

Corrected by V. L. Rykov 09/03/2003
                   qf(IPOINT + OFDM_tf)     = HITSH(5,I)
                   qf(IPOINT + OFDM_xi)     = HITSH(10,I)
                   qf(IPOINT + OFDM_yi)     = HITSH(11,I)
                   qf(IPOINT + OFDM_zi)     = HITSH(12,I)
                   qf(IPOINT + OFDM_xo)     = HITSH(13,I)
                   qf(IPOINT + OFDM_yo)     = HITSH(14,I)
                   qf(IPOINT + OFDM_zo)     = HITSH(15,I)
                   Do j = 1, nv
                     iqf(IPOINT + OFDM_vol + j - 1) = NUBV(j,I)
                   Enddo
                   IPOINT = IPOINT + mfd_sil
                 END DO  ! loop on hits
              endif  ! check on bank name
           endif  ! check on NHITS > 0
        ENDDO ! loop on sub-volumes

      return
      end
