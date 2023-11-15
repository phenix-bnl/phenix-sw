c $Id: emc_hits.f,v 1.7 2009/08/20 03:54:34 hpereira Exp $

      subroutine emc_hits
      implicit none

C     CFM   EM-Cal digitization template  April 7, 1992
c        re-work of Lead Gas calorimeter program (with F. Obenshain)

c     Rewritten 04/15/93 - 05/06/93 G. David

c     Updated 12/27/93  G. David

c     Update: Oct. 1996, G. David
c     This is a completely new version replacing the earlier
c     emc_digi.f  In compliance with the rest of PHENIX all
c     digitization functions are moved into PISORP (PISORP_1)
c     To avoid confusion, all previous comments, explanations
c     are deleted; if you want to work with an earlier
c     version, refer to the archives and the routine emc_digi.f




#include "guphnx.inc"
#include "gcflag.inc"
#include "secubuf.inc"
#include "gckine.inc"
#include "guevgen.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpelink.inc"
#include "gccuts.inc"
#include "subevt.inc"

      integer i_detmax/8/

      character*4 cudet
      real     svrtx(3), pvrtx(4)
      integer    mxcycle, mxexpr
      parameter  (mxcycle=50)
      parameter  (mxexpr=50)

      integer     spart,snvert,ncycle,ittest,last_trk, ihit_bitp
      integer     mfe_cal_specp /4/
      integer     iword(4)   ! bit packed values

      integer   nhmax
      parameter (nhmax=480000)
      real      hitsh(6,nhmax)
      integer   itrah(nhmax)
      integer   nuvs(2) /2*0/
      integer   nubv(2,nhmax)
      integer   nuvs_emc(3) /3*0/
      common /tmpdigi/ hitsh, nubv
      integer   nhits
      real      dele, tof, pos_x, pos_y, pos_z
      real    x_m(3)
      equivalence (x_m(1), pos_x), (x_m(2), pos_y), (x_m(3), pos_z)
      integer   icall /0/
      integer   blen, lf_e, emul,
     1    ioecal,incnd, iwall, itype, numed
      character*10 chform

      integer iz,iy,iz_off,iy_off,iz_rel,iy_rel
      integer i_smody,i_smodz,i_mody,i_modz

      integer emc_debug
      logical l_pbgl/.false./

      logical logpack /.true./
      integer full_length             ! length of uncompressed data
      parameter (full_length = 14)

      character*4  c_det(8)
     1  /'EC11','EC21','EC31','EC41','EC51','EC61',
     1  'EC72','EC82'/

      integer i,i1
      integer i_index(3)

      integer nt_hits
      integer*4	      i_counter/0/

c     This array is for response corrections for different impact
c     positions
      save

c     NTUPLE for vertex positions  (begin)
c     Filled only if CVOLU_OPT(1,8) = PHOT or VERT

      integer np8001
      parameter (np8001=10)
      character*8 ch8001 (np8001)/
     +  'ITRACK', 'IDPART', 'X_VERTEX', 'Y_VERTEX',
     +  'Z_VERTEX', 'R_VERTEX', 'P_VERTEX', 'IWALL',
     +  'SUBEVENT', 'EVENT'/
      real evt8001(np8001)

c    NTUPLE for GEANT hits
c     Filled only if CVOLU_OPT(3,8) = GHIT and
c     CVOLU_OPT(4,8) = BITP

      integer np8011
      parameter (np8011=16)
      character*8 ch8011(np8011) / 'I1', 'IWALL', 'ITYPE', 'DELE',
     +  'POS_X', 'POS_Y', 'POS_Z', 'TOF', 'I_INDEX1', 'I_INDEX2',
     +  'ITTEST', 'SPART', 'NCYCLE', 'NUMED', 
     +  'SUBEVENT', 'EVENT'/
      real evt8011(np8011)

      integer lastitp /-1/    ! initialized variables need not be saved
      integer lastevt /-1/    ! initialized variables need not be saved
      integer lastsub /-1/    ! last subevent
      integer ist		!status return variable
      integer nmtrmx          ! maximum number of tracks in list
      parameter(nmtrmx = 2000)
      integer listtr(nmtrmx)  ! list of tracks
      integer listwa(nmtrmx)  ! list of wall numbers for the tracks

c     NTUPLE for vertex positions  (end)

      real r_sumdep
      integer naccepted  ! count how many sngl_neutral photons are accepted
      integer itaccept   ! track number of accepted photon

c*********************************************************
 
C     Initialize
      i_counter = i_counter + 1

      if(ikine.eq.32.and.ikine2(2).eq.4)then
        if(icall.eq.0)then
          write(6,*) 'emc_hits - forced acceptance SNGL_NEUTRAL'
        endif
        logaccepted = .false.
        naccepted = 0
        itaccept = 0
      endif

      if(icall.eq.0) then
      
        r_sumdep = 0.0
            
        call hbook1(87101,'Sum PISA e deposit',1000,0.0,5.0,0.0)
        call hbook1(87102,'Sum PISA e deposit',1000,0.0,25.0,0.0)
        call hbook1(87103,'Sum PISA e deposit',1000,0.0,100.0,0.0)
        call hbook2(87201,'PISA sector 1',
     1    72,0.5,72.5,36,0.5,36.5,0.0)
        call hbook2(87202,'PISA sector 2',
     1    72,0.5,72.5,36,0.5,36.5,0.0)
        call hbook2(87203,'PISA sector 3',
     1    72,0.5,72.5,36,0.5,36.5,0.0)
        call hbook2(87204,'PISA sector 4',
     1    72,0.5,72.5,36,0.5,36.5,0.0)
        call hbook2(87205,'PISA sector 5',
     1    72,0.5,72.5,36,0.5,36.5,0.0)
        call hbook2(87206,'PISA sector 6',
     1    72,0.5,72.5,36,0.5,36.5,0.0)
        call hbook2(87207,'PISA sector 7',
     1    96,0.5,96.5,48,0.5,48.5,0.0)
        call hbook2(87208,'PISA sector 8',
     1    96,0.5,96.5,48,0.5,48.5,0.0)
        
        icall = 1
              
        if(cvolu_opt(4,8).eq.'SPEC'
     1    .or.cvolu_opt(4,8).eq.'FOLD') then
          write(6,*) 'emc_hits - SPEC or FOLD are not valid options'
          return
        endif

        if(cvolu_opt(2,8).eq.'FULL') then
          write(6,*) 'emc_hits - Hits output will NOT be bit-packed'
          logpack = .false.
        else
          write(6,*) 'emc_hits - Hits output WILL be bit-packed'
          logpack = .true.
        endif
              
c       book parameter banks
c       book IO characteristic for event banks
        if(logpack)then
          chform = '1I / 4I'
        else
          chform = '1I / 3I 5F 6I'
        endif
        call mzform('ECAL',chform,ioecal) ! book characteristic
        
c       Booking NTUPLE 8001 (begin)

	       call hbookn(8001,'Particle Vertex',np8001,
     +    'GEANHIST',10000,ch8001)
	       call hbookn(8011,'GEANT Hits',np8011,
     +    'GEANHIST',10000,ch8011)
        endif   ! initialization at first entry

c       ###############################################################################
c       Initialization for a new event

C       Reset Event Variables
C       First call to GFHITS to establish maximum size of output ZEBRA bank

      emul = 0
      nt_hits = 0
      do i1 = 1,i_detmax
        cudet = c_det(i1)           ! next active em subdetector

c       reset search volume numbers (they have to be zero to get all
c       hits, but may have been changed to retrieve hits in a
c       specific volume)

        call vzero(nuvs , 2)        ! reset; get hits from all volumes

        call gfhits('EMC ',cudet
     1    ,2,6                   ! Dim path id., dim. hit array
     1    ,nhmax,0               ! Max. # of hits, all hits
     1    ,nuvs,itrah            !
     1    ,nubv,hitsh,nhits)

C /chp/ if user array hitsh exceeded, nhits is returned as nhmax+1
        if (nhits .gt. nhmax) then
          write(6,*)
     +      'emc_hits - number of hits exceeds',
     +      ' nhmax, nhit truncated to nhmax for ',cudet
          nhits = nhmax
        end if

        nt_hits = nt_hits + nhits
      enddo 

c     Book the ZEBRA bank (with space for at least one hit)
      if(cvolu_opt(4,8).ne.'FOLD'.and.cvolu_opt(4,8).ne.'SPEC')then
         if(logpack)then
            blen = mfe_cal_specp*(nt_hits + 1)
         else
            blen = full_length*(nt_hits + 1)
         endif
      else
        ! book bank with full size
        blen = mfe_cal*mfe_alldets  
      endif
      
      lfe_cal(1) = 0  ! normal bank
      call mzbook(ixdiv_fe,lFE_Cal(1),lFE_Cal(1),1,'ECAL',0,0,
     1  blen, 9, -1)       ! mother bank

c     Now fill the bank
c     initialize track list (begin)
      call vzero(listtr,nmtrmx)
      call vzero(listwa,nmtrmx)

c     initialize track list (end)
c     loop over active subdetectors (the ones that had GFDETU entries,
c     therefore they show up in the array C_DET)
      if(emc_debug.gt.3) then
         call gprint('HITS','*')
      endif

c     CFM variables
      ihit_bitp = 0                  ! number of hits output with bit packing
      last_trk = 0                   ! last track number
      lf_e = lfe_cal(1) + 2          ! base address
      evt8011(np8011) = ntru_evt
      evt8011(np8011-1) = nsub_evt
      DO i1 = 1,i_detmax
        cudet = c_det(i1)           ! Next active EM subdetector

c       Reset search volume numbers (they have to be zero to get all
c       hits, but may have been changed to retrieve hits in a
c       specific volume)

        call vzero(nuvs , 2)        ! reset; get hits from all volumes
        call gfhits('EMC ',cudet
     1    ,2,6                   ! Dim path id., dim. hit array
     1    ,nhmax,0               ! Max. # of hits, all hits
     1    ,nuvs,itrah            !
     1    ,nubv,hitsh,nhits)
C       if user array hitsh too small, hitsh array is filled and
C       nhits is returned as nhmax+1
          
        if (nhits .gt. nhmax) then
          write(6,*)
     +      'emc_hits - number of hits exceeds',
     +      ' nhmax, nhit truncated to nhmax for ',cudet
          nhits = nhmax
        end if

         if(nhits.le.0) goto 1998    ! nothing to do, jump to end of loop

c       Get info necessary to decide in which array should the energy
c       be deposited
        
        iwall = i1
        if(i1.le.6) then
          itype = 1                ! lead scintillator
        else
          itype = 2                ! lead glass
        endif
        if(emc_debug.gt.0) then
          write(6,*)
     +      'emc_hits - EMcal subdetector ',cudet,' hits ',nhits
          endif

c           Loop over all hits in this subdetector (wall, type)
        do i = 1,nhits
          dele = hitsh(4,i)
          pos_x = hitsh(1,i)        ! position in master,
          pos_y = hitsh(2,i)        ! equivalenced to x_m(1), x_m(2),
          pos_z = hitsh(3,i)        ! x_m(3)...
          tof = hitsh(5,i)
          r_sumdep = r_sumdep + dele
c         Forget the hit if it is outside the readout gate
c         or if DELE = 0  (cfm)
c         Inserts for bit-packed output (begin)

          if(cvolu_opt(4,8).ne.'SPEC'.and.
     1      cvolu_opt(4,8).ne.'FOLD')then
            i_index(1) = nubv(1,i)    ! supermodule index pointer
            i_index(2) = nubv(2,i)    ! cell within a supermodule pointer
       
c           Put in 2D histoes for Sean's studies
            if(i1.le.6) then
              i_smody = 3
              i_smodz = 6
              i_mody = 12
              i_modz = 12
            else
              i_smody = 12
              i_smodz = 16
              i_mody = 4
              i_modz = 6
            endif
            
            iz_off = ((i_index(1)-1) / i_smody) * i_modz
            iy_off = MOD(i_index(1)-1,i_smody) * i_mody
            iz_rel = 1 + (i_index(2) - 1) / i_mody
            iy_rel = i_index(2) - (iz_rel - 1) * i_mody
            iz = iz_rel + iz_off
            iy = iy_rel + iy_off
            
            if (i1.ge.7) then
              iz_off = ((i_index(1)-1)/i_smody)*i_modz
              iy_off = mod(i_index(1)-1,i_smody)*i_mody
              iz_rel = 1+mod(i_index(2)-1,i_modz)
              iy_rel = 1+((i_index(2)-1)/i_modz)
              iz = iz_rel + iz_off
              iy = iy_rel + iy_off
            endif
            call hf2(87200+i1,float(iz),float(iy),dele)
            
            numed = 0                 ! reset before gmedia call
            call gmedia(x_m,numed)    ! x_m from pos variables
            if(itrah(i).ne.last_trk)then
              last_trk = itrah(i)

c             Track ancestry call

              call emc_track(itrah(i), svrtx, pvrtx, spart, snvert,
     +          ittest, ncycle)
              if(ittest.ne.-9999999)then
                call trkstack(iabs(ittest)) ! put track on stack
                
                if(ikine.eq.32.and.ikine2(2).eq.4)then
                  if(iabs(ittest).ne.itaccept.and.spart.eq.1.and.
     +              naccepted.le.1)then
                    naccepted = naccepted + 1
                    itaccept = iabs(ittest)
                    if(naccepted.eq.2)then
                      logaccepted = .true.
                      nrvacc_evt = nrvacc_evt + 1  ! used by ptrig
                    endif ! check if two photons have been accepted
                  endif ! check for valid photon track not equal to previously accepted
                endif  ! check for force acceptance condition on SNGL_NEUTRAL events
                
              else

c               Special case of "orphan track" from GEN_EVT generator
                call trkstack(itrah(i))
                      
                if(ikine.eq.32.and.ikine2(2).eq.4)then
                  if(itrah(i).ne.itaccept.and.spart.eq.1.and.
     +              naccepted.le.1)then
                    naccepted = naccepted + 1
                    itaccept = itrah(i)
                    if(naccepted.eq.2)then
                      logaccepted = .true.
                      nrvacc_evt = nrvacc_evt + 1  ! used by ptrig
                    endif ! check if two photons have been accepted
                  endif ! check for valid photon track not equal to previously accepted
                endif  ! check for force acceptance condition on SNGL_NEUTRAL events
                
              endif
            endif ! check if track number same as for last hit
            
            if(logpack)then
              call emc_bitp(i1, iwall, itype, dele, pos_x,
     +          pos_y, pos_z, tof, i_index(1),
     +          i_index(2), numed, spart, ncycle,
     +          iabs(ittest), iword)
              iqf(lf_e + 1) =  iword(1)
              iqf(lf_e + 2) =  iword(2)
              iqf(lf_e + 3) =  iword(3)
              iqf(lf_e + 4) =  iword(4)
            else
              iqf(lf_e + 1) = i1
              iqf(lf_e + 2) = iwall
              iqf(lf_e + 3) = itype
              qf(lf_e + 4) = dele
              qf(lf_e + 5) = pos_x
              qf(lf_e + 6) = pos_y
              qf(lf_e + 7) = pos_z
              qf(lf_e + 8) = tof
              iqf(lf_e + 9) = i_index(1)
              iqf(lf_e + 10) = i_index(2)
              iqf(lf_e + 11) = numed
              iqf(lf_e + 12) = spart
              iqf(lf_e + 13) = ncycle
              iqf(lf_e + 14) = iabs(ittest)
            endif
            evt8011(1) = i1
            evt8011(2) = iwall
            evt8011(3) = itype
            evt8011(4) = dele
            evt8011(5) = pos_x
            evt8011(6) = pos_y
            evt8011(7) = pos_z
            evt8011(8) = tof
            evt8011(9) = i_index(1)
            evt8011(10) = i_index(2)
            evt8011(11) = ittest
            evt8011(12) = spart
            evt8011(13) = ncycle
            evt8011(14) = numed
            if(cvolu_opt(3,8).eq.'GHIT')then
              call hfn(8011,evt8011)
            endif ! check on NTUPLE store
            if(logpack)then
              lf_e = lf_e + mfe_cal_specp
            else
              lf_e = lf_e + full_length
            endif
            ihit_bitp = ihit_bitp + 1
            go to 1997
          endif  ! check on use of BITP option 

c         Inserts for bit-packed output (end)
c         Storing the NTUPLE 8001 (begin)
c         NTUPLE filled only if PHOT or VERT option in cvolu_opt(1,8)
c         Don't want to keep storing the same particle
c         Also check if photons are to be stored

                if((cvolu_opt(1,8).eq.'PHOT'.or.
     +            (spart.ne.1.and.cvolu_opt(1,8).eq.'VERT')).and.
     +            (lastevt.ne.ntru_evt.or.lastsub.ne.nsub_evt.or.
     +            lastitp.ne.ittest))then
                    
                  lastevt = ntru_evt
                  lastsub = nsub_evt
                  lastitp =  ittest
                  evt8001(1) = ittest  ! parent track number
                  evt8001(2) = spart   ! parent ID number
                  evt8001(3) = svrtx(1)
                  evt8001(4) = svrtx(2)
                  evt8001(5) = svrtx(3)
                  evt8001(6)=  sqrt(svrtx(1)**2+svrtx(2)**2)
                  evt8001(7)=  sqrt(pvrtx(1)**2+pvrtx(2)**2+
     +              pvrtx(3)**2)
                  evt8001(8) = iwall
                  evt8001(np8001-1) = nsub_evt
                  evt8001(np8001) = ntru_evt

c                Extra logic so as not to store duplicate tracks

                  ist = 0
                  call xemctrlist(ittest,iwall,listtr,listwa,nmtrmx,ist)
                  if(ist.gt.0)then
   	                call hfn(8001,evt8001)
                  endif
                endif  ! check if same track, subevent, and event numbers
 1997           continue
              enddo                   ! loop over all hits

 1998   continue
      enddo    ! loop over i1, all the emcal subdetectors that are defined
      
      if(end_evtflg) then
        call hf1(87101,r_sumdep,1.0)
        call hf1(87102,r_sumdep,1.0)
        call hf1(87103,r_sumdep,1.0)
        r_sumdep = 0.0
      endif
      if(cvolu_opt(4,8).ne.'FOLD'.and.cvolu_opt(4,8).ne.'SPEC')then

c       Pass multiplicity (number of consecutive entries in bank)

           if(logpack)then
            iqf(lfe_cal(1) + 1) = -ihit_bitp  ! negative PISORP/STAF key for unpacking
          else
            iqf(lfe_cal(1) + 1) = +ihit_bitp  ! positive for ROOT output (unpacked)
          endif
          
c         Reduce bank size (CFM, 1999: need to check this)
          if(ihit_bitp+1.lt.nt_hits)then
            incnd = (ihit_bitp+1 - nt_hits) * mfe_cal_specp
            call mzpush(ixdiv_fe,lfe_cal(1),0,incnd,'I')
          endif
         return   ! skip rest of signal processing
      endif


 1999 CONTINUE


9999  CONTINUE

      RETURN
      end

c*****************************************************************************
