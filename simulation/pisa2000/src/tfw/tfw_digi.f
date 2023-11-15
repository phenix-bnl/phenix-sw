c $Id: tfw_digi.f,v 1.17 2009/08/20 04:31:19 pinkenbu Exp $
C     Original author: Charles F. Maguire
C     Creation date: August 17, 2006

      subroutine tfw_digi

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
#include "fpwlink.inc"
      
C     Local Declarations
C     ------------------
      
      character*4 cudet
      integer*4   iudet
      equivalence (iudet, cudet)

      integer   nhmax
      parameter (nhmax=5000)
      real      hitsh(13,nhmax)
      integer   itrah(nhmax)
      integer   nuvs(3) /3*0/
      integer   nubv(3,nhmax)
      
c     use scratch area. choose offsets conservatively
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
      parameter (n_subvolumes=6)


      character*4 name_subvolumes(n_subvolumes) /'RPG1','RPG2',
     + 'RPG3','RPG4','RPG5','RPG6'/

      character*4 namesh(13) /'X1  ','Y1  ','Z1  ',
     +  'X2  ', 'Y2  ', 'Z2  ', 
     +  'TOF ', 'PTID', 'DELE', 
     +  'X1GL', 'Y1GL', 'Z1GL', 'PTHL'/ ! ADDED by ID

      save iowcal


C     Executable code
C     ===============
      !Initialize

      if(icall.eq.0) then
        write (*,*) 
     +    'tfw_digi - initialization ',
     +    'ver. october 19, 2007'
        icall = 1

C     CVOLU_OPT(I,NSET)       ith volume option for set number NSET
C     NSET: VER, PAD, TFW, ITR, CRK, TRD, TOF,
C     EMC, PHO, MUO  ( 1 ---> 10 )
C     same for RVOLU_OPT and IVOLU_OPT
        if(cvolu_opt(2,12).ne.'P_ID') then
          write(6,*) 'tfw_digi - inconsistent HIT structure'
          stop
        endif
        
        if(cvolu_opt(4,12).NE.'ELEM') then
          write(6,*) 'tfw_digi - ELEM is not selected. slow output'
          write(6,*) 'tfw_digi - valid DIGI options are: ELEM '
        endif

        chform = '1I / 2I 13F'
        call mzform(name_subvolumes(1),chform,iowcal) ! book characteristic
              
      endif

c     book IO characteristic for event banks
      do isub=1,n_subvolumes
        cudet = name_subvolumes(isub) ! namesv variable in tfw subroutine

c     extract hit information from Zebra/Geant Hit Banks
        call gfhits('TFW '      ! set identifier
     &    ,cudet                  ! detector identifier
     &    ,1                      ! dimension of path identification
     &    ,13                     ! dimension of hit array
     &    ,nhmax                  ! maximum number of returned hits
     &    ,0                      ! take all tracks
     &    ,nuvs                   ! volume descriptor
     &    ,itrah                  ! array of hit producing tracks
     &    ,nubv                   ! volume descriptor numbers on output
     &    ,hitsh                  ! hit values
     &    ,nhits)                 ! number of hits in this detector


        if (nhits .gt. nhmax) then
          write(6,*) 
     +      'tfw_digi - number of hits exceeds',
     +      nhmax,' nhits truncated to ',nhmax,' for ',cudet
          nhits = nhmax
        end if

        lfw_tfw(isub) = 0
        if(nhits.gt.0) then
          
          if(cvolu_opt(4,12).eq.'ELEM') then 
            blen = mfd_tfw*NHITS + 10
            
            call mzbook(ixdiv_fe,
     &        lfw_tfw(isub),
     &        lfw_tfw(isub), 1,
     &        name_subvolumes(isub),
     &        0,
     &        0,
     &        blen,
     &        iowcal,
     &        -1)
              
              
            if(lfw_tfw(isub).le.0)then
              write(6,9)cudet,isub,nhits,lfw_tfw(isub)
 9            format( ' tfw_digi fatal error: det = ',a,
     +          ', isub = ',i2, ', nhits = ',i4,
     +          ', bad ZEBRA link = ',i10)
              stop 
            endif

            iqf(lfw_tfw(isub) + 1) = nhits ! put length into mother bank
            ipoint = lfw_tfw(isub) + 2  ! pointer to storage area for hits
            do i = 1,nhits ! loop on stored hits /module
              qf(ipoint + ofwc_xlocal1) = hitsh(1,i)
              qf(ipoint + ofwc_ylocal1) = hitsh(2,i)
              qf(ipoint + ofwc_zlocal1) = hitsh(3,i)
              qf(ipoint + ofwc_xlocal2) = hitsh(4,i)
              qf(ipoint + ofwc_ylocal2) = hitsh(5,i)
              qf(ipoint + ofwc_zlocal2) = hitsh(6,i)
              qf(ipoint + ofwc_tof) = hitsh(7,i)
              qf(ipoint + ofwc_pid) = hitsh(8,i)
              qf(ipoint + ofwc_dele) = hitsh(9,i)
              qf(ipoint + ofwc_xglobal) = hitsh(10,i)
              qf(ipoint + ofwc_yglobal) = hitsh(11,i)
              qf(ipoint + ofwc_zglobal) = hitsh(12,i)
              qf(ipoint + ofwc_pathlength) = hitsh(13,i)
              iqf(ipoint + ofwc_track)   = itrah(i)
              iqf(ipoint + ofwc_panel) = isub
              call trkstack(itrah(i)) ! put track # in pisa track stacker
              ipoint = ipoint + mfd_tfw
            enddo         ! loop on hits
          endif  ! check on bank name
        endif  ! check on nhits > 0
      enddo ! loop on sub-volumes
      
      return
      end
      
