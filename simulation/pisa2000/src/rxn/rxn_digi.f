c $Id: rxn_digi.f,v 1.7 2008/05/21 08:22:18 hpereira Exp $

C     Original author: Charles F. Maguire
C     Creation date: August 25, 2006

      subroutine rxn_digi

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
#include "fpslink.inc"

C     Local Declarations
      character*4 cudet
      integer*4   iudet
      equivalence (iudet, cudet)

      integer   nhmax
      parameter (nhmax=5000)
      real      hitsh(16,nhmax)
      integer   itrah(nhmax)
      integer   nuvs(3) /3*0/
      integer   nubv(3,nhmax)
c  use scratch area. choose offsets conservatively
      equivalence(hitsh(1,1),temp(1)),(itrah(1),itemp(80000)),
     &     (nubv(1,1),itemp(120000))
      integer   nhits, ipoint
      real      xnor
      real      dele, pos_x, pos_y, pos_z, partl, px, py, pz
      integer   icall /0/
      integer   blen, lf_d, lf_dbase, dmul, i, j, imod, ihalf,
     &     iowcal,incnd, isext
      character*20 chform
      integer n_subvolumes,isub
      parameter (n_subvolumes=48)
      character*4 name_subvolumes(n_subvolumes)
     & /'RX00','RX01','RX02','RX03','RX04','RX05',
     &  'RX06','RX07','RX08','RX09','RX0A','RX0B',
     &  'RX10','RX11','RX12','RX13','RX14','RX15',
     &  'RX16','RX17','RX18','RX19','RX1A','RX1B',
     &  'RX20','RX21','RX22','RX23','RX24','RX25',
     &  'RX26','RX27','RX28','RX29','RX2A','RX2B',
     &  'RX30','RX31','RX32','RX33','RX34','RX35',
     &  'RX36','RX37','RX38','RX39','RX3A','RX3B' /

      save iowcal

      if(icall.eq.0) then       ! initialize
        write( *,* ) 'rxn_digi - initialization'
        icall = 1

C     CVOLU_OPT(I,NSET)       ith volume option for set number NSET
C     NSET: VER, PAD, RXN, ITR, CRK, TRD, TOF,
C     EMC, PHO, MUO  ( 1 ---> 10 )
C     same for RVOLU_OPT and IVOLU_OPT

         if(cvolu_opt(2,18).ne.'P_ID') then
            write(6,*) 'rxn_digi - inconsistent HIT structure'
            stop
         endif
         if(cvolu_opt(4,18).NE.'ELEM') then
          write(6,*) 'rxn_digi - elem is not selected. slow output'
          write(6,*) 'rxn_digi - valid DIGI options are: ELEM '
         endif
        
         chform = '1I / 2I 16F'
         call mzform(name_subvolumes(1),chform,iowcal) ! book characteristic
         
        endif
        
c       book IO characteristic for event banks
        do isub=1,n_subvolumes
          
          cudet = name_subvolumes(isub) ! namesv variable in rxn subroutine

c     extract hit information from Zebra/Geant Hit Banks

          call gfhits('RXN '   ! set identifier
     &      ,cudet               ! detector identifier
     &      ,1                      ! dimension of path identification
     &      ,16                      ! dimension of hit array
     &      ,nhmax                  ! maximum number of returned hits
     &      ,0                      ! take all tracks
     &      ,nuvs                   ! volume descriptor
     &      ,itrah                  ! array of hit producing tracks
     &      ,nubv                   ! volume descriptor numbers on output
     &      ,hitsh                  ! hit values
     &      ,nhits)                 ! number of hits in this detector

          if (nhits .gt. nhmax) then
            write(6,*) 
     +        'rxn_digi - number of hits exceeds',
     +        nhmax,' nhits truncated to ',nhmax,' for ',cudet
            nhits = nhmax
          end if

          if(nhits.gt.0) then
            
            if(cvolu_opt(4,18).eq.'ELEM') then ! elementary bank

c   mfd_rxn = number of data elements per hit; the 10 is a safetybuffer

            blen = mfd_rxn*nhits + 10
            
            lfs_rxn(isub) = 0
            call mzbook(ixdiv_fe,
     &        lfs_rxn(isub),
     &        lfs_rxn(isub), 1,
     &        name_subvolumes(isub),
     &        0,
     &        0,
     &        blen,
     &        iowcal,
     &        -1)
              
            iqf(lfs_rxn(isub) + 1) = nhits ! put length into mother bank
            ipoint = lfs_rxn(isub) + 2
            do i = 1,nhits ! loop on stored hits /module
              qf(ipoint + ofsc_xlocal1) = hitsh(1,i)
              qf(ipoint + ofsc_ylocal1) = hitsh(2,i)
              qf(ipoint + ofsc_zlocal1) = hitsh(3,i)
              qf(ipoint + ofsc_xlocal2) = hitsh(4,i)
              qf(ipoint + ofsc_ylocal2) = hitsh(5,i)
              qf(ipoint + ofsc_zlocal2) = hitsh(6,i)
              qf(ipoint + ofsc_tof) = hitsh(7,i)
              qf(ipoint + ofsc_pid) = hitsh(8,i)
              qf(ipoint + ofsc_dele) = hitsh(9,i)
              qf(ipoint + ofsc_xglobal) = hitsh(10,i)
              qf(ipoint + ofsc_yglobal) = hitsh(11,i)
              qf(ipoint + ofsc_zglobal) = hitsh(12,i)
              qf(ipoint + ofsc_pathlength) = hitsh(13,i)
              qf(ipoint + ofsc_pmomx) = hitsh(14,i)
              qf(ipoint + ofsc_pmomy) = hitsh(15,i)
              qf(ipoint + ofsc_pmomz) = hitsh(16,i)
              iqf(ipoint + ofsc_track)   = itrah(i)
              iqf(ipoint + ofsc_arm) = isub ! 1-24 = north, 25-48 = south
              call trkstack(itrah(i)) ! put track # in pisa track stacker
              ipoint = ipoint + mfd_rxn
            enddo         ! loop on hits
          endif  ! check on bank name
        endif  ! check on nhits > 0
      enddo ! loop on sub-volumes
      
      return
      end
      
      
