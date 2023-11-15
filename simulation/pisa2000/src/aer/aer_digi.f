c $Id: aer_digi.f,v 1.8 2009/08/11 15:47:34 hpereira Exp $

      subroutine aer_digi
      implicit none

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
#include "fpalink.inc"

C     Local Declarations
C     ------------------
      character*4 cudet
      integer*4   iudet
      equivalence (iudet, cudet)

      integer   nhmax
      parameter (nhmax=10000)
      real      hitsh(17,nhmax)
      integer   itrah(nhmax)
      integer   nuvs(3) /3*0/   !deepest nested level for sensitive volumes is 3
      integer   nubv(3,nhmax)
      
      ! usescratch area. Choose offsets conservatively
      EQUIVALENCE(HITSH(1,1),TEMP(1)),(ITRAH(1),ITEMP(80000)),
     &  (NUBV(1,1),ITEMP(120000))
        
      integer nhits, ipoint
      real xnor
      real dele, pos_x, pos_y, pos_z, partl, px, py, pz
      real vert_x,vert_y,vert_z
      integer icall /0/
      integer blen, lf_q, lf_qbase, dmul, i, j, imod, ihalf,
     &  ioacal,incnd, isext
      character*20 chform
      integer n_subvolumes,isub
      parameter (n_subvolumes=160)
      character*4 name_subvolumes(n_subvolumes)

      save ioacal
      
      integer volume_n
      integer n_volume
      character*50 name_volu

      do  n_volume = 1, n_subvolumes
         volume_n = 1000 + n_volume
         write(name_volu,*)volume_n
         name_subvolumes(n_volume) = 'A'//name_volu(3:5)
      enddo




C    Executable code
C    ===============

C     Initialize


      ! initialize
      if(icall.eq.0) then      
        write(*,*) 'aer_digi - initialization'
        icall = 1

C     CVOLU_OPT(I,NSET)       ith volume option for set number NSET
C     NSET: VER, PAD, AER, ITR, CRK, TRD, TOF,
C     EMC, PHO, MUO  ( 1 ---> 10 )
C     same for RVOLU_OPT and IVOLU_OPT

        if(cvolu_opt(2,14).ne.'P_ID') then
          write(6,*) 'aer_digi - inconsistent HIT structure'
          stop
        endif
        
        if(cvolu_opt(4,14).NE.'ELEM') then
          write(6,*) 'aer_digi - ELEM is not selected. slow output'
          write(6,*) 'aer_digi - valid DIGI options are: ELEM '
        endif

        ! book characteristic
        chform = '1I / 5F 3I 4F'
        call mzform(name_subvolumes(1),chform,ioacal) 
              
      endif

c       book IO characteristic for event banks


c###############################################################################
      do isub=1,n_subvolumes
          
        cudet = name_subvolumes(isub) ! namesv variable in aer subroutine
        
c       extract hit information from Zebra/Geant Hit Banks
        call gfhits('AER '   ! set identifier
     &    ,cudet               ! detector identifier
     &    ,1                      ! dimension of path identification
     &    ,17                     ! dimension of hit array
     &    ,nhmax                  ! maximum number of returned hits
     &    ,0                      ! take all tracks
     &    ,nuvs                   ! volume descriptor
     &    ,itrah                  ! array of hit producing tracks
     &    ,nubv                   ! volume descriptor numbers on output
     &    ,hitsh                  ! hit values
     &    ,nhits)                 ! number of hits in this detector
              
        if (nhits .gt. nhmax) then
          write(6,*) 
     +      'aer_digi - number of hits exceeds',
     +      nhmax,' nhits truncated to ',nhmax,' for ',cudet
          nhits = nhmax
        end if
        
        if(nhits.gt.0) then
          
C         TONSE:  go ahead & book banks since we have something to put inside
C         book Zebra bank

          ! elementary bank
          if(cvolu_opt(4,14).eq.'ELEM') then 
            blen = mfa_aer*nhits + 17
                  
            lfa_aer(isub,1) = 0
            call mzbook(ixdiv_fe,
     &        lfa_aer(isub,1),
     &        lfa_aer(isub,1), 1,
     &        name_subvolumes(isub),
     &        0,
     &        0,
     &        blen,
     &        ioacal,
     &        -1)
                  
            ! put length into mother bank
            iqf(lfa_aer(isub,1) + 1) = nhits 
            ipoint = lfa_aer(isub,1) + 2
                  
            ! loop on stored hits /module
            do i = 1,nhits 
              
              qf(ipoint + ofam_x) = hitsh(1,i)
              qf(ipoint + ofam_y) = hitsh(2,i)
              qf(ipoint + ofam_z) = hitsh(3,i)
              qf(ipoint + ofam_del) = hitsh(4,i)
              qf(ipoint + ofam_px) = hitsh(6,i)
              iqf(ipoint + ofam_partl) = hitsh(5,i)
              iqf(ipoint + ofam_itra)   = itrah(i)
              
              ! put track # in pisa track stacker
              call trkstack(itrah(i)) 
                    
              iqf(ipoint + ofam_layer) = isub
              qf(ipoint + ofam_py) = hitsh(7,i)
              qf(ipoint + ofam_pz) = hitsh(8,i)
              qf(ipoint + ofam_leng) = hitsh(9,i)
              qf(ipoint + ofam_tof) = hitsh(10,i)
              qf(ipoint + ofam_step) = hitsh(11,i)
              qf(ipoint + ofam_etot) = hitsh(12,i)
              qf(ipoint + ofam_charge) = hitsh(13,i)
              qf(ipoint + ofam_momentum) = hitsh(14,i)
              qf(ipoint + ofam_vertx) = hitsh(15,i)
              qf(ipoint + ofam_verty) = hitsh(16,i)
              qf(ipoint + ofam_vertz) = hitsh(17,i)
                    
              ipoint = ipoint + mfa_aer
                    
            end do  ! loop on hits
          endif  ! check on bank name
        endif  ! check on nhits > 0

      enddo ! loop on sub-volumes

      return
      end
