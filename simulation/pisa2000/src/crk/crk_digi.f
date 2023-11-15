c $Id: crk_digi.f,v 1.4 2008/05/21 08:21:54 hpereira Exp $

      subroutine crk_digi
*********************
*  FILE crk_digi.f
*********************

c    C.H. Pinkenburg Feb. 09, 2001  check on # of hits returned by gfhits
c                                   indicating a too small user hit buffer 
c                                   (and set nhits to size of user buffer,
c                                   gfhits: GEANT manual page156)


      implicit none
      real acosd

C     CFM   CERENKOV digitization template   April 7, 1992

C        two-parameter store of pos_z and ID for testing, 4/10

c Description of CCAL, CHIT banks

C       YA      Modified. 4/25/92
C               CPHO HITS are digitized, and then written to CCAL bank
C               CCAL bank is supproted by links defined fclink.inc, which
C               is also modified from CFM template version.
C               It is assumed that the CPHO detector consists of 4 sectors,
C               two for each of two arms. Each sector is divided int matrix
C               of photon detector cells. There are NCZ divisions in z, and
C               NCPHI divisions in phi. The value of NCZ, NCPHI are defined
C               by parameter statement in fclink.inc
C               CCAL bank contains 3 words for each hit cell.
C                   integer CELL...CELL index
C                   integer NPE....# of photo-electrons in the cell
C                   real    TOF....TOF value of the hit. If NPE > 1, the
C                                  earlies time of TOF is recorded

c       YA      Modified 5/3/92
c               Structure of CCAL bank is re-organized.
c               The data part of CCAL bank is unchanged
c               Add down links in CCAL. For each CELL hit, there is one down
c               link that point to a CHIT bank, which store the data of
c               original (un-digitized) hit.

c The structure of CCAL bank is

c                     lFC_CAL
c                      |
c                      V
c   -MUL  ...  -2  -1  0 MUL, (CELL, NPE, TOF), ...,
c     |         |   |    number of hit CERENKOV detector
c     |         |   V
c     |         V   CHIT bank for the first hit cell
c     V         CHIT bank for the second hit cell
c     CHIT bank for the MUL-th hit cell

c The structure of CHIT bank is

c    lFC_HIT
c     |
c     V
c     0 MUL, (x, y, z, tof, PID, ITRA, IPARENT), ...,
c                where IPARENT is 0 if PID is not GEANTINO
c                      IPARENT is ITRA of parent e+ or e- if PID is GEANTINO

c       YA      Modified 5/4/92
c               add itra of parent in CHIT bank.

c       YA      Modified 5/15/92
c               add (px,py,pz) in CHIT bank. See fclink for detail

c       KS      Modified 9/04/96
c               add (nbaf,bi1,bi2,bp1,bp2) in CHIT bank
c               light baffling will be handled in PISORP
c                using this information
c               see also fpclink.inc

c The structure of CTRA banks is

c     lFC_TRA
c       |
c       V     next            next
c     CTRA(1) ------> CTRA(2) ------> CTRA(3)
c     (CMIR)          (CTR1)          (CTR2)

c       L (lfc_tra, lq(lfc_tra), or lq(lq(lfc_tra))
c       |
c       V
c       0  MUL, (x,y,z,PID,ITRA,PVx,PVy,PVz,Vx,Vy,Vz),....

C       YA      Modified 4/28/92
C               Add virtual tracker bank CTRA
C               Two virtual tracker is added to CERK to aid Cerenkov analysis
C               and his in the virtual trackers CTR1 and CTR2 are stored in
C               bank CTRA
C               CTRA is supported by link lFC_Tra, defined in fclink.inc
C               Two CTRA banks are created, and supported by lFC_Tra. The
C               first bank is for CTR1, and the second for CTR2.
C               It has 5 words for each hits;(x,y,z,pid,itra)

c       YA      Modified 5/12/92
c               add (px,py,pz,vx,vy,vz) in CTRA bank

#include "guphnx.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpclink.inc"
#include "pisa_parts.inc"

      character*4 cudet
      integer*4   iudet
      equivalence (iudet, cudet)

      integer nvdim
      parameter (nvdim = 10)
      integer nhdim
      parameter (nhdim = 9)

      integer   nhmax
      parameter (nhmax=5000)
      real      hitsh(nhdim,nhmax)
      integer   itrah(nhmax)
      integer   nuvs(nvdim) /nvdim*0/ ! cfm added 2 extra data values
                                ! ks changed to use parameter nvdim
      integer   nubv(nvdim,nhmax)
      integer   nhits
      real      xnor
      real      dele, pos_z
      integer   icall /0/
      integer   blen, lf_c, lf_cbase, cmul, i, j, k, imod, ihalf,
     1    incnd, isext
      integer ioccal            !i/o characteristics of ccal bank
      integer ioctra            !i/o characteristics of ctra bank
      integer iochit            !i/o characteristics of chit
      save ioccal
      save ioctra
      save iochit               ! c.f. maguire  august 18, 1999
      character*10 chform

c vars to get data from GFKINE

      real vert(3)
      real pvert(4)
      integer ipart
      integer nvert
      real ubuf(10)
      integer nbuf

c--> common to communicate with CRK, the geometry initialization routine

      real mirr_cent(3)         !x,y,z
      common/crk_common/mirr_cent
      

c working variables to sort out the hits in cerenkov cell
c Here I assume that (1) no more than MAXCELLS PMT cells will be hit in one
c event, and that each PMT cell will be hit by no more than MAXHITS photo-
c electrons. I chose parameter MAXCELLS and MAXHITS large enough for Au+Au
c collision.

      integer maxcells,maxhits
      parameter (maxcells = 1000) !maximum number of cells hit
      parameter (maxhits  = 30) ! maximum # of photo-electrons in each cell
      integer cere_icell(maxcells) !cell id
      integer cere_npe(maxcells) ! # of p.e.'s in the cell
      integer cere_hpntr(maxcells,maxhits) !pointer to original hits record
      real    cere_tof(maxcells)! earlies tof of the cell
      logical found             !flag
      integer ifound
      integer itemp             !temp loop variable
      integer lfc_hit           !link to chit
      integer lf_h              !temporary link var for chit
c vars for digitizing hit position of photons in photon detector
      integer isect             !photon detector panel id
      integer izcell,iphi       !z and phi index of the photon cell hit
      real z_offset             !z position offset of photon detector panel
      real z_max                !max. z value in photon detector panel
      real dzcell               !z-size of the photon detector cell
      real dphi_cell            !size in phi of the photon detector cell
      real xhit, yhit, zhit, rhit !x,y,z, and r of photon hit position
      real phi,tof              !phi and tof value of the photon hit
      integer icell             !sequential number of photon cell.
      integer jhit              !

c vars for baffle study

      real    glopos(3), locpos(3)
      character*4 vname(15)
      integer nv, lnam(15), lnum(15), ier

      integer max_bafhit
      parameter (max_bafhit = 1000)
      integer n_bafhit
      integer baf_tra(max_bafhit)
      integer baf_id(max_bafhit)
      real    baf_pos(max_bafhit)
      integer nbaf

c detector sub-divisions

      integer ncsect, ncspm, ncpmt
      integer ncell_sect        ! number of cells in one sector
      integer ncell_tot         ! total number of cells
      ncsect     = qf(lfc_para + oc_n_sect)
      ncspm      = qf(lfc_para + oc_n_spm)
      ncpmt      = qf(lfc_para + oc_n_pmt)
      ncell_sect = ncspm * ncpmt
      ncell_tot  = ncell_sect * ncsect

      ! initialize
      if(icall.eq.0) then      
        write(*,*) 'crk_digi - initialization'
        icall = 1

        
C     CVOLU_OPT(I,NSET) ith volume option for set number NSET
C              NSET: VER, PAD, INR, ITR, CRK, TRD, TOF,
C                    EMC, PHO, MUO  ( 1 ---> 10 )
C              same for RVOLU_OPT and IVOLU_OPT

        
        ! simulate cal ADC/TDC data
        if(cvolu_opt(4,5).ne.'CCAL') then 
          write(6,*) 'crk_digi - CCAL is not selected. slow output'
          write(6,*) 'crk_digi - valid DIGI options are: CCAL '
        endif

c       book IO characteristic for event banks
        ! book characteristic of ccal
        chform = '1I / 2I 1F'
        call mzform('CCAL',chform,ioccal) 

        ! book characteristic of CTRA
        chform  = '1I / 3F 2I 6F'
        call mzform('CTRA',chform,ioctra) 
        
        ! bank characteristic of CHIT
        chform = '1I / 7F 6I 2F'
        call mzform('CHIT',chform,iochit) 
        
      endif

c#############################################################################

C     Reset Event Variables

      if ( cvolu_opt(1,5) .eq. 'CBAF' ) then
        
        cudet='CBAF'            ! namesv variable in CRK subroutine

c       extract hit information from Zebra/Geant Hit Banks
        call gfhits('CRK '      ! set identifier
     $    ,cudet             ! detector identifier
     $    ,nvdim             ! dimension of path identification
     $    ,nhdim             ! dimension of hit array
     $    ,nhmax             ! maximum number of returned hits
     $    ,0                 ! take all tracks
     $    ,nuvs              ! volume descriptor
     $    ,itrah             ! array of hit producing tracks
     $    ,nubv              ! volume descriptor numbers on output
     $    ,hitsh             ! hit values
     $    ,n_bafhit)         ! number of hits in this detector

c       /chp/ if user array hitsh exceeded, n_bafhit is returned as nhmax+1
        if (n_bafhit .gt. nhmax) then
          write(6,*) 
     +      'crk_digi - number of hits exceeds',
     +      nhmax,' n_bafhit truncated to ',nhmax,' for ',cudet
          n_bafhit = nhmax
        end if

        do i = 1, n_bafhit
          nv = 7
          vname(1) = 'HALL'
          vname(2) = 'CARM'
          if ((nubv(2,i).gt.0).and.(nubv(3,i).eq.0)) then ! CSHA
            vname(3) = 'CSHA'
            vname(4) = 'CRDA'
          elseif ((nubv(2,i).eq.0).and.(nubv(3,i).gt.0)) then ! CSHB
            vname(3) = 'CSHB'
            vname(4) = 'CRDB'
          else
            write(*,*) 
     +        'crk_digi - CBAF cannot be assigned to CSHA or CSHB'
            stop
          endif
          vname(5) = 'CPHO'
          vname(6) = 'CSPM'
          vname(7) = 'CBAF'
          do j = 1, nv
            call uctoh(vname(j), lnam(j), 4, 4)
          enddo
          lnum(1) = 1
          lnum(2) = nubv(1,i)
          lnum(3) = 1
          lnum(4) = 1
          lnum(5) = 1
          lnum(6) = nubv(4,i)
          lnum(7) = nubv(5,i)
          call glvolu (nv, lnam, lnum, ier)
          
          glopos(1) = hitsh(1,i)
          glopos(2) = hitsh(2,i)
          glopos(3) = hitsh(3,i)
          call gmtod (glopos, locpos, 1)
          
          baf_tra(i) = itrah(i)
          baf_id (i) = nubv(5,i)
          baf_pos(i) = locpos(3)
        enddo
      else
        n_bafhit = 0
      endif                    

      ! namesv variable in CRK subroutine
      CUDET='CPME'             

c     extract hit information from Zebra/Geant Hit Banks
      call gfhits('CRK '        ! set identifier
     $  ,cudet               ! detector identifier
     $  ,nvdim               ! dimension of path identification
     $  ,nhdim               ! dimension of hit array
     $  ,nhmax               ! maximum number of returned hits
     $  ,0                   ! take all tracks
     $  ,nuvs                ! volume descriptor
     $  ,itrah               ! array of hit producing tracks
     $  ,nubv                ! volume descriptor numbers on output
     $  ,hitsh               ! hit values
     $  ,nhits)              ! number of hits in this detector

C /chp/ if user array hitsh exceeded, nhits is returned as nhmax+1
      if (nhits .gt. nhmax) then
        write(6,*) 
     +    'crk_digi - number of hits exceeds',
     +    nhmax,' nhits truncated to ',nhmax,' for ',cudet
        nhits = nhmax
      end if


      if(nhits.gt.0) then
 
c New digitization code (Y.A. Feb 2, 1996)
c PMTs are implemented in new geometry code. Therefore, we can get
c icell from the volume descriptor of the hit PMT.

        cmul = 0                !reset cmul counter
        do i = 1,nhits

c obtain isect,iphi,iz from NUBV()
c  NUBV(1,i) index of ARM  (1 = WEST, 2 = EAST)
c  NUBV(2,i) index of SIDE (1 = CSHA = NW or SE, 0 = CSHB = SW or NE)
c  NUBV(3,i) index of SIDE (0 = CSHA = NW or SE, 1 = CSHB = SW or NE)
c  NUBV(4,i) index of SUPERMODUE [1,40]
c  NUBV(5,i) index of PMT in SUPERMODUE [1,32]

          if ((nubv(2,i).gt.0).and.(nubv(3,i).eq.0)) then 
            
            if (nubv(1,i).eq.1) then  ! NW = section 1
              isect = 1
            else                      ! SE = section 2
              isect = 2
            endif
            
            if (nubv(5,i).le.16) then ! forward row (left side)
              iphi = 2*nubv(4,i) - 1
            else                      ! backward row (right side)
              iphi = 2*nubv(4,i) - 2
            endif
            
          elseif ((nubv(2,i).eq.0).and.(nubv(3,i).gt.0)) then ! CSHB
          
            if (nubv(1,i).eq.1) then  ! SW = section 0
              isect = 0
            else                      ! NE = section 3
              isect = 3
            endif
            
            if (nubv(5,i).le.16) then ! forward row (left side)
              iphi = (NCSPM - nubv(4,i)) * 2
            else                      ! backward row (right side)
              iphi = (NCSPM - nubv(4,i)) * 2 + 1
            endif
            
          else
            
            write(*,*)
     &        'crk_digi - PMT cannot be assigned to CSHA or CSHB'
            stop
          endif

          izcell = mod(nubv(5,i)-1,16)
          
          ! sort out the hit.
          icell = isect * NCELL_SECT + iphi * NCPMT/2 + izcell
          found = .false.
          do itemp = 1,cmul
            if(cere_icell(itemp) .eq. icell) then
              found = .true.
              ifound = itemp
              goto 111          !break
            endif
          enddo
 111      continue
          
           ! this cell has been hit previously
          if(found) then       
            
            cere_npe(ifound) = cere_npe(ifound) + 1 !increment # of p.e.
            if(cere_npe(ifound) .le. maxhits) then
              cere_hpntr(ifound,cere_npe(ifound)) = i
            else
              write( 6, *) ' crk_digi - too many p.e. hits in one cell'
              write( 6, *)  
     +          ' crk_digi - ',
     +          'original hit will not be recorded'
              cere_npe(ifound) = maxhits
              
            endif
            
            if(hitsh(5,i) .le. cere_tof(ifound) ) then
              cere_tof(ifound) = hitsh(5,i)
            endif
            
          else
            
            cmul = cmul + 1
            
            !check if the array overflowed?
            if(cmul .le. maxcells) then 
              cere_icell(cmul) = icell
              cere_npe(cmul) = 1
              cere_hpntr(cmul,1) = i
              cere_tof(cmul) = hitsh(5,i)
            else
              write( 6, *) ' crk_digi - too many CELLs are hit'
              write( 6, *) ' crk_digi - this hit will not be recorded'
            endif
          endif
        enddo 
      else
        cmul = 0
      endif 

c     feed into Zebra output bank, options are:
c     CCAL
      
      ! simulate mapped calibrated data
      if(cvolu_opt(4,5).eq.'CCAL') then 

c       book CCAL bank
c       CCAL bank is supported by a link, lFC_Cal, which is defined in fclink.inc

        blen = mFC_CAL*cmul + 1 ! # of data words in CCAL
        call mzbook(ixdiv_fe,
     $    lfc_cal(1),        !return bank address
     $    lfc_cal(1),        !supporting link
     $    1,                 !JBIAS=1 ---> top level bank
     $    'CCAL',            !bank name
     $    cmul,              !# of links.......We have cmul down links
     $    cmul,              !# of down links..link point to chit bank
     $    blen,              !# of data words
     $    ioccal,            !i/o characteristics
     $    -1)                !do not clear the memory contents


        iqf(lFC_Cal(1)+1) = cmul   ! preset
        lf_c = lFC_Cal(1) + 2
        do i = 1, cmul
          iqf(lf_c + ofcc_cell) = cere_icell(i)
          iqf(lf_c + ofcc_npe)  = cere_npe(i)
          qf( lf_c + ofcc_tof)  = cere_tof(i)
          lf_c = lf_c + mfc_cal

c         create bank CHIT, and record (xhit,yhit,zhit,tof,pid,itra) of the original
c         hits. CHIT is supported by down links in CPHO

          blen = mfc_hit*cere_npe(i) + 1
          call mzbook(ixdiv_fe,
     $      lfc_hit,         !return bank address
     $      lfc_cal(1),      !supporting bank
     $      -i,              !jbias<0 ... link bias in supporting bank
     $      'CHIT',          !bank name
     $      0,               !# of links...no links in this bank
     $      0,               !# of down links...no down links in this bank
     $      blen,            !# of data words
     $      iochit,          !i/o characteristics
     $         -1)              !do not clear the memory contents
          iqf(lfc_hit + 1) = cere_npe(i)
          lf_h = lfc_hit + 2
          do j = 1, cere_npe(i)
            jhit = cere_hpntr(i,j)
            qf(lf_h + ofch_x)   = hitsh(1,jhit)
            qf(lf_h + ofch_y)   = hitsh(2,jhit)
            qf(lf_h + ofch_z)   = hitsh(3,jhit)
            qf(lf_h + ofch_tof) = hitsh(5,jhit)
            qf(lf_h + ofch_px)  = hitsh(7,jhit)
            qf(lf_h + ofch_py)  = hitsh(8,jhit)
            qf(lf_h + ofch_pz)  = hitsh(9,jhit)
            iqf(lf_h + ofch_pid)= hitsh(6,jhit)
            iqf(lf_h + ofch_tra)= itrah(jhit)
            
            if(hitsh(6,jhit).eq. pp_crk_phot) then
c             find the itra of the parent...
              call gfkine(itrah(jhit),vert,pvert,ipart,nvert,ubuf,nbuf)
              iqf(lf_h + oFCh_par) = ubuf(1)
              if (ubuf(1).gt.0) then
                call trkstack(int(ubuf(1))) ! stack the parent of C photon
              else
                write(*,*) 'crk_digi - ',
     +            'no parent recorded for a Cherenkov photon'
              endif
            else
              iqf(lf_h + oFCh_par) = 0
            endif

c           check if baffles are hit by the track
            iqf(lf_h + oFCh_nbf) = 0
            iqf(lf_h + oFCh_bi1) = 0
            iqf(lf_h + oFCh_bi2) = 0
            qf(lf_h + oFCh_bp1) = 0.
            qf(lf_h + oFCh_bp2) = 0.

            if ( cvolu_opt(1,5) .eq. 'CBAF' ) then
              nbaf = 0
              k = 1
              do while ((nbaf .le. 2) .and. (k .le. n_bafhit))
                if (itrah(jhit) .eq. baf_tra(k)) then
                  
                  nbaf = nbaf + 1
                  if (nbaf .eq. 1) then
                    
                    iqf(lf_h + oFCh_bi1) = baf_id(k)
                    qf(lf_h + oFCh_bp1)  = baf_pos(k)
                    
                  else if (nbaf .eq. 2) then
                    
                    iqf(lf_h + oFCh_bi2) = baf_id(k)
                    qf(lf_h + oFCh_bp2)  = baf_pos(k)
                    
                  endif
                end if
                k = k + 1
              end do
              iqf(lf_h + oFCh_nbf) = nbaf
            endif      

            lf_h = lf_h + mFC_HIT
          enddo
        enddo

c---> CTRA hits bank #1 (CMIR)

        cudet = 'CMIR'
        call gfhits('CRK '      ! set identifier
     $    ,cudet               ! detector identifier
     $    ,nvdim               ! dimension of path identification
     $    ,nhdim               ! dimension of hit array
     $    ,nhmax               ! maximum number of returned hits
     $    ,0                   ! take all tracks
     $    ,nuvs                ! volume descriptor
     $    ,itrah               ! array of hit producing tracks
     $    ,nubv                ! volume descriptor numbers on output
     $    ,hitsh               ! hit values
     $    ,nhits)              ! number of hits in this detector
          
C /chp/ if user array hitsh exceeded, nhits is returned as nhmax+1
        if (nhits .gt. nhmax) then
          write(6,*) 
     +      'crk_digi.f - number of hits exceeds',
     +      nhmax,' nhits truncated to ',nhmax,' for ',cudet
          nhits = nhmax
        end if

c       book CTRA bank #1, and store the hits in CMIR in it
        blen = mFC_Tra*Nhits + 1 !# of data words in CTRA

        call mzbook(ixdiv_fe,
     $    lfc_tra(1),           !return bank address
     $    lfc_tra(1),           !supporting link
     $    1,                   !jbias=1 ---> top level bank
     $    'CTRA',              !bank name
     $    0,                   !# of links...no links in this bank
     $    0,                   !# of down links...no links in this bank
     $    blen,                !# of data words
     $    ioctra,              !i/o characteristics
     $    -1)                  !do not clear the memory contents
          
        lf_c = lFC_Tra(1)
        iqf(lf_c + 1) = Nhits
        lf_c = lFC_Tra(1) + 2
        do i=1,Nhits
          qf(lf_c + oFCt_x) = hitsh(1,i)
          qf(lf_c + oFCt_y) = hitsh(2,i)
          qf(lf_c + oFCt_z) = hitsh(3,i)
          iqf(lf_c + oFCt_pid) = hitsh(6,i)
          iqf(lf_c + oFCt_itra)= itrah(i)
          call GFKINE(itrah(i),vert,pvert,ipart,nvert,ubuf,nbuf)
          qf(lf_c + oFCt_pvx) = pvert(1)
          qf(lf_c + oFCt_pvy) = pvert(2)
          qf(lf_c + oFCt_pvz) = pvert(3)
          qf(lf_c + oFCt_vx) = vert(1)
          qf(lf_c + oFCt_vy) = vert(2)
          qf(lf_c + oFCt_vz) = vert(3)
          lf_c = lf_c + mFC_Tra
        enddo

c       CTRA hits bank #2 (CTR1)
        cudet = 'CTR1'
        call gfhits('CRK '      ! set identifier
     $    ,cudet               ! detector identifier
     $    ,nvdim               ! dimension of path identification
     $    ,nhdim               ! dimension of hit array
     $    ,nhmax               ! maximum number of returned hits
     $    ,0                   ! take all tracks
     $    ,nuvs                ! volume descriptor
     $    ,itrah               ! array of hit producing tracks
     $    ,nubv                ! volume descriptor numbers on output
     $    ,hitsh               ! hit values
     $    ,nhits)              ! number of hits in this detector

C /chp/ if user array hitsh exceeded, nhits is returned as nhmax+1
        if (nhits .gt. nhmax) then
          write(6,*) 
     +      'crk_digi - number of hits exceeds',
     +      nhmax,' nhits truncated to ',nhmax,' for ',cudet
          nhits = nhmax
        end if

c       book CTRA bank #2, store his in CTR1, and chain it in the CTR bank chain
        blen = mFC_Tra*Nhits + 1
        call mzbook(ixdiv_fe,
     $    lf_c,                !return bank address
     $    lfc_tra(1),          !supporting previous bank
     $    0,                   !jbias=0 ---> chain after the prevous bank
     $    'CTRA',              !bank name
     $    0,                   !# of links...no links in this bank
     $    0,                   !# of down links...no links in this bank
     $    blen,                !# of data words
     $    ioctra,              !i/o characteristics
     $    -1)                  !do not clear the memory contents

c       now lf_c points to the new bank CTRA #2. It is chained to CTRA #1
        iqf(lf_c + 1) = nhits
        lf_c = lf_c + 2
        do i=1,nhits
          qf(lf_c + ofct_x) = hitsh(1,i)
          qf(lf_c + ofct_y) = hitsh(2,i)
          qf(lf_c + ofct_z) = hitsh(3,i)
          iqf(lf_c + ofct_pid) = hitsh(6,i)
          iqf(lf_c + ofct_itra)= itrah(i)
          call gfkine(itrah(i),vert,pvert,ipart,nvert,ubuf,nbuf)
          qf(lf_c + ofct_pvx) = pvert(1)
          qf(lf_c + ofct_pvy) = pvert(2)
          qf(lf_c + ofct_pvz) = pvert(3)
          qf(lf_c + ofct_vx) = vert(1)
          qf(lf_c + ofct_vy) = vert(2)
          qf(lf_c + ofct_vz) = vert(3)
          lf_c = lf_c + mfc_tra
ccc          write(*,*) 'trkstack for ctr1  ', itrah(i)
          call trkstack(itrah(i)) ! stack the particle passing through ctr1
        enddo

c---> CTRA hits bank #3 (CTR2)
        cudet = 'CTR2'
        call gfhits('CRK '      ! set identifier
     $    ,cudet               ! detector identifier
     $    ,nvdim               ! dimension of path identification
     $    ,nhdim               ! dimension of hit array
     $    ,nhmax               ! maximum number of returned hits
     $    ,0                   ! take all tracks
     $    ,nuvs                ! volume descriptor
     $    ,itrah               ! array of hit producing tracks
     $    ,nubv                ! volume descriptor numbers on output
     $    ,hitsh               ! hit values
     $    ,nhits)              ! number of hits in this detector

C /chp/ if user array hitsh exceeded, nhits is returned as nhmax+1
        if (nhits .gt. nhmax) then
          write(6,*) 
     +      'crk_digi - number of hits exceeds',
     +      nhmax,' nhits truncated to ',nhmax,' for ',cudet
          nhits = nhmax
        end if
        
        blen = mFC_Tra*Nhits + 1

c       note that lFC_Tra      points to CTRA bank #1
c       lqf(lFC_Tra) points to CTRA bank #2
c       the new bank created (CTRA #3) will be chained after CTRA #2
        blen = mFC_Tra*Nhits + 1
        call mzbook(ixdiv_fe,
     $    lf_c,                !return bank address
     $    lqf(lfc_tra(1)),     !supporting previous bank...2nd ctra bank
     $    0,                   !jbias=0 ---> chain after the prevous bank
     $    'CTRA',              !bank name
     $    0,                   !# of links...no links in this bank
     $    0,                   !# of down links...no links in this bank
     $    blen,                !# of data words
     $    ioctra,              !i/o characteristics
     $    -1)                  !do not clear the memory contents
        iqf(lf_c + 1) = nhits
        lf_c = lf_c + 2
        do i=1,nhits
          qf(lf_c + ofct_x) = hitsh(1,i)
          qf(lf_c + ofct_y) = hitsh(2,i)
          qf(lf_c + ofct_z) = hitsh(3,i)
          iqf(lf_c + ofct_pid) = hitsh(6,i)
          iqf(lf_c + ofct_itra)= itrah(i)
          call gfkine(itrah(i),vert,pvert,ipart,nvert,ubuf,nbuf)
          qf(lf_c + ofct_pvx) = pvert(1)
          qf(lf_c + ofct_pvy) = pvert(2)
          qf(lf_c + ofct_pvz) = pvert(3)
          qf(lf_c + ofct_vx) = vert(1)
          qf(lf_c + ofct_vy) = vert(2)
          qf(lf_c + ofct_vz) = vert(3)
          lf_c = lf_c + mfc_tra
          
          ! stack the particle passing through CTR2
          call trkstack(itrah(i)) 
        enddo
      endif
      
 9999 return
      end
