c $Id: hbd_digi.f,v 1.9 2008/05/21 08:21:56 hpereira Exp $
      subroutine hbd_digi
      Implicit none
C===============================================================================
C    Global Declarations
C===============================================================================
#include "guphnx.inc"
#include "fstore.inc"
#include "pisascr.inc"
#include "sublink.inc"
#include "fphlink.inc"
C===============================================================================
C    Local Declarations
C===============================================================================
      character*4 cudet
      INTEGER   NHMAX
      PARAMETER (NHMAX=5000)
C===============================================================================
C     HITSH->Xin,Yin,Zin,Px,Py,Pz,TOF,P_ID,Xout,Yout,Zout,DELE,DSTP
C     All 13 of these are meaningful for TPC padrows, 
C     only first 11 of them for HBD radiator gas volume,
C     only first 8 of them for HBD CsI pad detector.
C===============================================================================
      REAL      HITSH(13,NHMAX)
      INTEGER   TRCK(NHMAX)
C===============================================================================
C     TPC hits are in the 8th nested sensitive volume; HBD gas hits and HBD CsI hits 
C     are in the 7th nested sensitive volume.  Take dimension of NUVS and first 
C     dimension of NUBV to be the larger of these nested depths.
C===============================================================================
      INTEGER   NUVS(8) /8*0/
      INTEGER   NUBV(8,NHMAX)
C===============================================================================
C     Use scratch area. Choose offsets conservatively
C===============================================================================
      equivalence(hitsh(1,1),temp(1)),(trck(1),itemp(80000)),
     &  (nubv(1,1),itemp(120000))
      INTEGER   NHITS, IPOINT
      real x1, y1, z1, px, py, pz, tof, p_id, x2, y2, z2, dele, dstp
C===============================================================================
C     Declare p_id as real because the array of hits values is an array of reals.
C===============================================================================
      integer   icall /0/
      integer   blen, ii, iohcal, isub ! isub used to index all sensitive subvolumes
      character*20 chform
C===============================================================================
C     Some geometry variables and data statements from hbd.f
C===============================================================================
      integer ndet, nsect, npr, sect, i,j,k
      character*4 Hgas, Hcsi, Hdet
      character*1 nmsect(8)
      character*1 sectnum(6)
      character*1 armnum(2)
      character*2 detset(4)
      integer dettype(4)
      Data dettype /2,3,2,3/
      Data detset /'PN','QN','PS','QS'/
      Data nmsect /'1','2','3','4','5','6','7','8'/
      Data sectnum /'0','1','2','3','4','5'/ !6 sectors - final detector
      Data armnum /'0','1'/     !EAST - 0; WEST - 1
C===============================================================================
C     End geometry from hbd.f
C===============================================================================
      save iohcal
C===============================================================================
C     Executable code
C===============================================================================
C     Initialize
C===============================================================================
      if(icall.eq.0) then       ! initialize
        write( *,* ) 'hbd_digi - initialization'
        icall = 1
C===============================================================================
C     CVOLU_OPT(I,NSET)       ith volume option for set number NSET
C     NSET: VER, PAD, HBD, ITR, CRK, TRD, TOF,
C     EMC, PHO, MUO  ( 1 ---> 10 )
C     same for RVOLU_OPT and IVOLU_OPT
C===============================================================================
        if(cvolu_opt(2,15).ne.'P_ID') then
          write(6,*)  'hbd_digi - inconsistent HIT structure'
          stop
        endif
        
C===============================================================================
C     Xin,Yin,Zin,Px,Py,Pz,TOF->7 reals; p_id,trck->2 integers; Xout,Yout,Zout,dele,dstp->
C     5 reals; ndet,nsect,npr,detflag->4 integers
C===============================================================================
        CHFORM = '1I / 7F 2I 5F 4I'
C===============================================================================
C     Book IO characteristic for event banks
C===============================================================================
        call mzform('HBD',CHFORM,iohcal)
      endif
C===============================================================================
C     Initialize isub, the index number of the sensitive volume with its
C     data being put into zebra bank lfh_hbd
C===============================================================================
      isub = 1
      
      do i = 0, 1 !arm
        do j = 0, 5 !apanel
          do k = 0, 3 !det
            sect=6*i+j+1
            Hdet = detset(k+1)//armnum(i+1)//sectnum(j+1)
            cudet = Hdet
            
C===============================================================================
C    Extract hit information from Zebra/Geant Hit Banks
C===============================================================================
            CALL GFHITS('HBD '     ! set identifier
     &        ,CUDET                ! detector identifier
     &        ,1                          ! dimension of path identification
     &        ,13                        ! dimension of hit array
     &        ,NHMAX               ! maximum number of returned hits
     &        ,0                          ! take all tracks
     &        ,NUVS                  ! volume descriptor
     &        ,TRCK                  ! array of hit producing tracks
     &        ,NUBV                  ! volume descriptor numbers on output
     &        ,HITSH                 ! hit values
     &        ,NHITS)                ! number of hits in this detector
C===============================================================================
C    /chp/ if user array hith exceeded, nhits is returned as nhmax+1
C===============================================================================
            if (nhits .gt. nhmax) then
             write(6,*) 
     +          'hbd_digi - number of hits exceeds',
     +          nhmax,' nhits truncated to ',
     +          nhmax,' for ',cudet
              nhits = nhmax
            end if
            If(NHITS.GT.0) THEN
C===============================================================================
C    TONSE:  go ahead & book banks since we have something to put inside
C    book Zebra bank
C===============================================================================
              IF(CVOLU_OPT(4,15).EQ.'ELEM') THEN ! elementary bank
                blen = mfh_hbd*NHITS + 13
                lfh_hbd(isub,1) = 0
C===============================================================================
C    lfh_hbd(isub,1) marks the location of the word preceding the first data
C    word in the bank.  mzbook returns the actual location.
C===============================================================================
                call mzbook(ixdiv_fe,
     &            lfh_hbd(ISUB,1),
     &            lfh_hbd(ISUB,1), 1,
     &            cudet,
     &            0,
     &            0,
     &            blen,
     &            iohcal,
     &            -1)
                iqf(lfh_hbd(ISUB,1) + 1) = NHITS ! put length into mother bank
                IPOINT = lfh_hbd(ISUB,1) + 2
                DO II = 1,NHITS                            ! loop on stored hits /module
                  qf(IPOINT + OFHM_Xin) = HITSH(1,II)
                  qf(IPOINT + OFHM_Yin) = HITSH(2,II)
                  qf(IPOINT + OFHM_Zin) = HITSH(3,II)
                  qf(IPOINT + OFHM_PX) = HITSH(4,II)
                  qf(IPOINT + OFHM_PY) = HITSH(5,II)
                  qf(IPOINT + OFHM_PZ) = HITSH(6,II)
                  qf(IPOINT + OFHM_TOF) = HITSH(7,II)
                  iqf(IPOINT + OFHM_P_ID) = HITSH(8,II)
                  iqf(IPOINT + OFHM_TRCK) = TRCK(II)
                  call trkstack(TRCK(II))                 ! put track # in PISA track stacker
                  qf(IPOINT + OFHM_Xout) = HITSH(9,II)
                  qf(IPOINT + OFHM_Yout) = HITSH(10,II)
                  qf(IPOINT + OFHM_Zout) = HITSH(11,II)
                  qf(IPOINT + OFHM_DELE) = -0.999 ! Will be multiplied by 1000 (GeV->MeV)
                  qf(IPOINT + OFHM_DSTP) = -999
                  iqf(IPOINT + OFHM_NDET) = -999
                  iqf(IPOINT + OFHM_NSECT) = sect
                  iqf(IPOINT + OFHM_NPR) = -999
                  iqf(IPOINT + OFHM_DETFLAG) = dettype(k+1) ! HBD gas = 2
                  IPOINT = IPOINT + mfh_hbd
                END DO           ! loop on hits
              endif               ! check on bank name
            endif                  ! check on NHITS > 0
C===============================================================================
C    isub should be a different number for each sensitive volume
C    HGAS having isub = 1-8, and HCSI having 9-16.
C===============================================================================  
            isub = isub + 1
          enddo                     ! arm loop for Hgas
        enddo                     ! sector loop for Hgas
      enddo
      return
      end
      
      
