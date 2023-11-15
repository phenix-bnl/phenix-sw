c $Id: tof_digi.f,v 1.5 2008/05/21 08:22:20 hpereira Exp $

c     Description :
c     =============

c     This subroutine :



c     CFM       TOF digitization template       May 7, 1992

c     Author:-   Tapan Nayak
c     (Based on the original setup by Charlie Maguire.)

c     Creation Date: 16-Sep-1992
c     ==========================

c================================
      subroutine tof_digi
c================================
      implicit      none
#include "guphnx.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpflink.inc"
#include "gcvolu.inc"

      integer blen
      integer fmul
      integer itrs
      integer icall /0/
      integer ih,iv
      integer imod
      integer ihalf
      integer incnd
      integer isext
      integer isegment
      integer iof
      integer ier
      integer lf_f
      integer lf_fbase
      integer*4    mFF_alldets          !  Total # of detectors (PMTs)
      integer maxlev               !  Max. level for full volume tree
      parameter (maxlev = 7)  
      integer lnam(maxlev)         !  Volume tree names
      integer lnum(maxlev)         !  Volume tree numbers
      integer nhmax
      parameter (nhmax=5000)
      integer itra(nhmax)
      integer volevel
      parameter (volevel=7)
      integer numbv(volevel,nhmax)
      integer numvs(volevel) /volevel*0/

c     volume levels needed to uniquely specify the slats 
c     (values which are automatically calculated via GSDETV )
      integer volevl
      parameter (volevl=3)
      integer numbvl(volevl,nhmax)
      integer numvsl(volevl) /volevl*0/
      integer volevs
      parameter (volevs=2)
      integer numbvs(volevs,nhmax)
      integer numvss(volevs) /volevs*0/

      integer nlev                 !  # of volume tree levels
      integer nvdim
      integer nhdim
      integer nhits
      integer n_subvolume
      integer nslats_panel
      integer n_panel
      integer n_column
      integer n_pslat
      integer nslat_seq
      integer partl

      real dele
      real enorm
      integer hits_dim
      parameter (hits_dim=11)
      real hits(hits_dim,nhmax)
      real pos_hit_slat
      real pos_m(3)
      real pos_d(3)
      real r_pos_slat
      real p_m(3)
      real sleng
      real tof

      character*4   cudet
      character*15  chform

      save iof   !  C.F. Maguire,  August 18, 1999

      integer nv
      character*4 chnmsv(10)
      integer nbitsv(10)
      integer nwhi,nwdi,iset,idet,idtyp

      ! do immediate return if no TOF_DIGI is requested
      if(cvolu_opt(4,7).eq.'NONE')then
         return
      endif 
            
      if (icall.eq.0) then
        write(*,*) 'tof_digi - initialization'
        icall = 1
        
c     CVOLU_OPT(I,NSET) : ith volume option for set number NSET
c     NSET: VER, PAD, INR, ITR, CRK, TRD, TOF,EMC, PHO, MUO, CSI (1 ---> 11)
c     same for RVOLU_OPT and IVOLU_OPT

        if (cvolu_opt(2,7).ne.'P_ID') then
          write(6,*) 'tof_digi - inconsistent HIT structure'
          stop
        endif
         if (cvolu_opt(4,7).ne.'FCAL' ) then
          write(6,*) 'tof_digi - FCAL is not selected. slow output'
          write(6,*) 'tof_digi - valid DIGI options are: FCAL '
         endif

c     book parameter banks
c     book IO characteristic for event banks
         chform = '1I / 6I 9F 1I'
         call mzform('FCAL',chform,iof)
      endif

      mff_alldets = iqf( lff_paru + 1 + ofea_mff_alldets )

c     simulate mapped Calibrated data for cvolu_opt(4,7).eq.'FCAL'
      fmul = 0
      if(cvolu_opt(4,7).eq.'FCAL') then
        
         blen = mff_cal*mff_alldets + 1
         call mzbook(ixdiv_fe, lFF_Cal(1), lFF_Cal(1)
     +    , 1, 'FCAL', 0, 0, blen, iof, -1) ! mother bank
         iqf(lff_cal(1)+1) = fmul ! preset
        
      endif

c     Extract hit information from Zebra/Geant Hit Banks
c     Loop over SCTS and SCTL panels

      nhdim    = 11
      itrs     = 0
      nslats_panel = 192
      do n_subvolume = 1,2
        if (n_subvolume.eq.1) then
          cudet = 'SCTS'
          sleng = qf( lff_paru + 1 + ofea_TFSS_length )
          nvdim = 2
          call  gfhits(
     $      'TOF '            ! set identifier
     $      ,cudet            ! detector identifier
     $      ,nvdim            ! dimension of path identification
     $      ,nhdim            ! dimension of hit array
     $      ,nhmax            ! maximum number of returned hits
     $      ,itrs             ! take all tracks for 0
     $      ,numvss           ! volume descriptor, 0 for all
     $      ,itra             ! array of hit producing tracks
     $      ,numbvs           ! volume descriptor numbers on output
     $      ,hits             ! hit values
     $      ,nhits)           ! number of hits in this detector

c         if user array hits exceeded, nhits is returned as nhmax+1
          if (nhits .gt. nhmax) then
            write(6,*) 
     +        'tof_digi - number of hits exceeds',
     +        nhmax,' nhits truncated to ',nhmax,' for ',cudet
            nhits = nhmax
          end if

        else
          cudet = 'SCTL'
          sleng = qf( lff_paru + 1 + ofea_tfls_length )
          nvdim = 3
          call  gfhits(
     $      'TOF '            ! set identifier
     $      ,cudet            ! detector identifier
     $      ,nvdim            ! dimension of path identification
     $      ,nhdim            ! dimension of hit array
     $      ,nhmax            ! maximum number of returned hits
     $      ,itrs             ! take all tracks for 0
     $      ,numvsl           ! volume descriptor, 0 for all
     $      ,itra             ! array of hit producing tracks
     $      ,numbvl           ! volume descriptor numbers on output
     $      ,hits             ! hit values
     $      ,nhits)           ! number of hits in this detector
            
C         if user array hits exceeded, nhits is returned as nhmax+1
          if (nhits .gt. nhmax) then
            write(6,*) 
     +        'tof_digi - number of hits exceeds',
     +        nhmax,' nhits truncated to ',nhmax,' for ',cudet
            nhits = nhmax
          end if
        endif
        
c       Write out the parameters
        if (nhits.le.0) goto 995
        call gfdet('TOF ',cudet,nv,chnmsv,nbitsv,idtyp,nwhi,nwdi,
     1    iset,idet)
              
c       Loop on stored hits/module
        do ih = 1,nhits
          if(n_subvolume.eq.1) then
            n_panel = numbvs(1,ih)
            n_column= numbvs(2,ih)
            n_pslat = 0
            nslat_seq = n_pslat + (n_column-1)*3+
     1        (n_panel-1)*nslats_panel
          else
            n_panel = numbvl(1,ih)
            n_column= numbvl(2,ih)
            n_pslat = numbvl(3,ih)
            nslat_seq = n_pslat + (n_column-1)*3+
     1        (n_panel-1)*nslats_panel
          endif
          
          pos_m(1) =  hits(1,ih)
          pos_m(2) =  hits(2,ih)
          pos_m(3) =  hits(3,ih)
          r_pos_slat = sqrt(pos_m(1)**2.+pos_m(2)**2.+pos_m(3)**2.)
          dele  =  hits(4,ih)
          tof   =  hits(5,ih)
          partl =  hits(6,ih)
          p_m(1) =  hits(7,ih)
          p_m(2) =  hits(8,ih)
          p_m(3) =  hits(9,ih)
                
c         Get the positions in slat coordinate system
c         First fetch detector tree information from GEANT
          
          if(n_subvolume.eq.1) then
            call ufpath('TOF ', cudet, numbvs(1,ih),
     $        nlev, lnam, lnum)
          else
            call ufpath('TOF ', cudet, numbvl(1,ih),
     $        nlev, lnam, lnum)
          endif
          call glvolu(nlev, lnam, lnum, ier)

          call gmtod(pos_m, pos_d, 1)
          pos_hit_slat = pos_d(3)      ! hit pos along the scinti. length(z)
          
c         following items are all deleted on 12/4/96 by K.K.
c         Pulse Height  given by 'dele' from GEANT
c         Time information given by 'tof' from GEANT
c         We have to get time zero from the Beam-Beam counter. 
c         tof_BBC will be handled in PISORP before handed to PEP
c         12/3/96
          
c         Feed into Zebra output bank, options are:
c         FCAL:    (Later on, we'll do something fancy here).

          fmul = fmul + 1
          if (cvolu_opt(4,7).eq.'FCAL')then
                  
            lf_f = lff_cal(1)+(fmul-1)*mff_cal+2 ! offset into mother bank
            iqf(lf_f + offc_slatype )  = n_subvolume     !  short or long
            iqf(lf_f + offc_ipanel  )  = n_panel
            iqf(lf_f + offc_icolumn )  = n_column
            iqf(lf_f + offc_ipslat  )  = n_pslat
            iqf(lf_f + offc_islat   )  = nslat_seq
            iqf(lf_f + offc_partid  )  = partl
            qf (lf_f  + offc_xhall  )  = pos_m(1)
            qf (lf_f  + offc_yhall  )  = pos_m(2)
            qf (lf_f  + offc_zhall  )  = pos_m(3)
            qf (lf_f  + offc_hitpos )  = pos_hit_slat
            qf (lf_f  + offc_pxhall )  = p_m(1)
            qf (lf_f  + offc_pyhall )  = p_m(2)
            qf (lf_f  + offc_pzhall )  = p_m(3)
            qf (lf_f  + offc_tof    )  = tof
            qf (lf_f  + offc_dedx   )  = dele
            iqf(lf_f  + offc_itrack )  = itra(ih)
            call trkstack(itra(ih))
          endif
        enddo
 995    continue
      enddo
      iqf(lFF_CAL(1)+1 )  = fmul ! put length into mother bank
            
c     reduce size of output bank
c     write(*,*)' fmul = ',fmul,' mFF_ALLDETS = ',mFF_ALLDETS
      if(cvolu_opt(4,7).eq.'FCAL') then
        incnd = (fmul - mFF_ALLDETS)*mFF_Cal
        call MZPUSH(ixdiv_fe,lFF_Cal,0,incnd,'I')
      endif

 9999 return
      end
      
      
      
      
      
