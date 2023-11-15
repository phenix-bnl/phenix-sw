c $Id: bbc_hits.f,v 1.5 2008/05/21 08:21:54 hpereira Exp $

*-- Author :    Toru Sugitate   02/06/93

C=====================================================================
      subroutine bbc_hits(name,bmul)
C=====================================================================

C     DESCRIPTION: This routine digitizes hits, and put the data into
C                  BMAP and BCAL banks. The PMT # is encoded as,
C                  NBBCOUNT(=1 for muon side, =2 for the other side)*
C                  1000 + NBBDETEC(Detector # in each counter).

C     ARGUMENTS: NAME -  Geant detector identifier.
C                BMUL -  Current number of hits, updated numer on output.

C     MAP:
C           1) CALLED BY; BBC_DIGI
C           2) CALLS;     GFHITS,MZPUSH

C     AUTHOR: Toru Sugitate [Hiroshima University] 02 June 1993

C=====================================================================
C     GLOBAL SPECIFICATIONS:
      implicit none
C    GUPHNX contains "variables of general interest", data-card defs, flags.
C           in particular, CVOLU_OPT is used in this subroutine.
C           Unfortunately, this also defines the local variable HIT_ALL,
C           which is not used here and therefore results in an undefined
C           variable warning message when this is compiled.
#include "guphnx.inc"

C    FSTORE has the common where the detector specific data are stored.
#include "fstore.inc"

C    FPBLINK contains the zebra links for the beam detector. In particular
C           it has the pointers into the common inside FSTORE where the beam
C           detector event data and geometry parameters are stored.
C           It also contains various offset parameters related the the
C           structure of the beam detector data banks.
#include "sublink.inc"
#include "fpblink.inc"

C -------------------------------------------------------------------------
C --- structure of a mapped raw data bank                     bank ID: BMAP

c     lFB_Map
c     |
c     V
c     0 MUL, (PMT#,X,Y,Z,DELE,TOF,PID,PX,PY,PZ,PLEN),...
c                   PMT number in each detector.
c                   X,Y,Z position.
c                   Energy loss
c                   TOF
c                   Particle ID of hit
c                   Momentum of particle
c                   Path length
c                   Track number

c     PARAMETER( mFB_Map   = 11)
c     PARAMETER( oFBM_PMT  =  0)        ! Offset PMT #
c     PARAMETER( oFBM_X    =  1)        ! Offset X position
c     PARAMETER( oFBM_Y    =  2)        ! Offset Y position
c     PARAMETER( oFBM_Z    =  3)        ! Offset Z Position
c     PARAMETER( oFBM_DEL  =  4)        ! Offset energy loss
c     PARAMETER( oFBM_TOF  =  5)        ! Offset TOF value
c     PARAMETER( oFBM_PID  =  6)        ! Offset particle ID of hit
c     PARAMETER( oFBM_PX   =  7)        ! Offset Px of particle
c     PARAMETER( oFBM_PY   =  8)        ! Offset Py of particle
c     PARAMETER( oFBM_PZ   =  9)        ! Offset Pz of particle
c     PARAMETER( oFBM_LEN  = 10)        ! Offset path length of hit
c     PARAMETER( oFBM_TRAK = 11)        ! Offset for track number of hit
C ---------------------------------------------------------------------------
C --- structure of a calibrated data bank                       bank ID: BCAL
C     Temporalily same as the MCAL bank for now. Toru

c     lFB_Cal
c     |
c     V
c     0 MUL, (PMT#,X,Y,Z,DELE,TOF,PID,PX,PY,PZ,PLEN),...
c                   PMT number in each detector.
c                   X,Y,Z position.
c                   Energy loss
c                   TOF
c                   Particle ID of hit
c                   Momentum of particle
c                   Path length
c                   Track number

c     PARAMETER( mFB_Cal   = 11)
c     PARAMETER( oFBC_PMT  =  0)        ! Offset PMT #
c     PARAMETER( oFBC_X    =  1)        ! Offset X position
c     PARAMETER( oFBC_Y    =  2)        ! Offset Y position
c     PARAMETER( oFBC_Z    =  3)        ! Offset Z Position
c     PARAMETER( oFBC_DEL  =  4)        ! Offset energy loss
c     PARAMETER( oFBC_TOF  =  5)        ! Offset TOF value
c     PARAMETER( oFBC_PID  =  6)        ! Offset particle ID of hit
c     PARAMETER( oFBC_PX   =  7)        ! Offset Px of particle
c     PARAMETER( oFBC_PY   =  8)        ! Offset Py of particle
c     PARAMETER( oFBC_PZ   =  9)        ! Offset Pz of particle
c     PARAMETER( oFBC_LEN  = 10)        ! Offset path length of hit
c     PARAMETER( oFBC_TRAK = 11)        ! Offset for track number of hit

C=====================================================================
C     EXTERNAL SPECIFICATIONS:

      CHARACTER*4   NAME
      INTEGER       BMUL
 
C=====================================================================
C     INTERNAL SPECIFICATIONS:
c---------------------------------------------------------------------
c variables to extract information from the Geant HIT banks.

      integer*4    iudet
      character*4  cudet        ! detector identifier
      equivalence (iudet,cudet)
      integer      nvdim        ! dimension of path identifier
      parameter   (nvdim=3)
      integer      nhdim        ! dimension of hit array
      parameter   (nhdim=11)
      integer      nhmax        ! maximum number of returned hits
      parameter   (nhmax=1000)
      integer      itrs         ! take all tracks for 0
      parameter   (itrs=0)
      integer      numvs(nvdim) /3*0/ ! volume descriptor, 0 for all
      integer      itra(nhmax)  ! array of hit producing tracks
      integer      numbv(nvdim,nhmax) ! volume descriptor numbers on output
      integer      nhits        ! number of hits in this detector
      real         hits(nhdim,nhmax) ! hit values
c---------------------------------------------------------------------
c local variables.

      integer      ip           ! loop pointer
      integer      ih           ! loop pointer for stored hits
      integer      lunbbc       ! logical unit for logging.
      integer      nbbcount     ! number of counter. (1=upstream)
      integer      nbbdetec     ! detector number in each counter
      integer      bbhitpmt     ! encoded pmt address
      integer      bbhitpid     ! particle id
      integer      lf_b         ! offset into mother bank.
      integer      incnd        !
      integer      icall/0/
      save         icall
 
      real         bbhitpos(3)  ! hit positon in a master refernce sys.
      real         bbhitmom(3)  ! momentum of hit particle
      real         bbhitdel     ! energy loss
      real         bbhittof     ! tof value
      real         bbhitlen     ! path length
 
      parameter   (lunbbc=6)

C=====================================================================
C               EXECUTABLE STATEMENTS FOLLOW BELOW
c---------------------------------------------------------------------

      if(icall.eq.0) then
        write(*,*) 'bbc_hits - first call'
        icall=icall+1
      endif
      
c---------------------------------------------------------------------
c Extract the hit information from Zebra/Geant HIT bank.

      cudet = name
      call gfhits('BBC ',
     &  cudet,nvdim,nhdim,nhmax,itrs,
     &  numvs,itra,numbv,hits,nhits)
        
c     if user array hits exceeded, nhits is returned as nhmax+1
      if (nhits .gt. nhmax) then
        write(6,*) 'bbc_hits - number of hits exceeds',
     &    nhmax,' nhit truncated to ',nhmax,' for ',cudet
        nhits = nhmax
      end if

c---------------------------------------------------------------------
c Decode the HIT array.

      if(nhits.gt.0) then

c-----  ----------------------------------------------------------------
c Extract from HITS array for digitization. (for future)

        ! loop on stored hits /module.
        do ih=1,nhits 

c         Add call to TRKSTACK routine to fill FKIN bank
c         This has no effect on the BBC software

          ! check if stack option requested
          if(cvolu_opt(5,2).eq.'STCK')then
             call trkstack(itra(ih))
          endif 
           
          nbbcount=numbv(1,ih)
          nbbdetec=numbv(2,ih)
          bbhitpmt=nbbcount*1000+nbbdetec
          bbhitpid=hits(6,ih)
          bbhitdel=hits(4,ih)
          bbhittof=hits(5,ih)
          bbhitlen=hits(10,ih)
          
          do ip=1,3
            bbhitpos(ip)=hits(ip,ih)
            bbhitmom(ip)=hits(ip+6,ih)
          enddo
c---------------------------------------------------------------------
c feed into Zebra output bank.

          bmul=bmul+1
          if(cvolu_opt(4,2).eq.'BMAP') then
                  
            ! offset into mother bank.
            lf_b=lfb_map(1)+(bmul-1)*mfb_map+2 
            iqf(lf_b+ofbm_pmt) = bbhitpmt
            qf (lf_b+ofbm_x  ) = bbhitpos(1)
            qf (lf_b+ofbm_y  ) = bbhitpos(2)
            qf (lf_b+ofbm_z  ) = bbhitpos(3)
            qf (lf_b+ofbm_del) = bbhitdel
            qf (lf_b+ofbm_tof) = bbhittof
            iqf(lf_b+ofbm_pid) = bbhitpid
            qf (lf_b+ofbm_px ) = bbhitmom(1)
            qf (lf_b+ofbm_py ) = bbhitmom(2)
            qf (lf_b+ofbm_pz ) = bbhitmom(3)
            qf (lf_b+ofbm_len) = bbhitlen
            iqf (lf_b+ofbm_trak)= itra(ih)  ! track-in-subevent number
            iqf(lfb_map(1)+1)     = bmul ! put length into mother bank
            
          elseif(cvolu_opt(4,2).eq.'BCAL') then ! temporarily same as bmap.
          
            lf_b=lfb_cal(1)+(bmul-1)*mfb_cal+2 ! offset into mother bank.
            iqf(lf_b+ofbc_pmt) = bbhitpmt
            qf (lf_b+ofbc_x  ) = bbhitpos(1)
            qf (lf_b+ofbc_y  ) = bbhitpos(2)
            qf (lf_b+ofbc_z  ) = bbhitpos(3)
            qf (lf_b+ofbc_del) = bbhitdel
            qf (lf_b+ofbc_tof) = bbhittof
            iqf(lf_b+ofbc_pid) = bbhitpid
            qf (lf_b+ofbc_px ) = bbhitmom(1)
            qf (lf_b+ofbc_py ) = bbhitmom(2)
            qf (lf_b+ofbc_pz ) = bbhitmom(3)
            qf (lf_b+ofbc_len) = bbhitlen
            iqf (lf_b+ofbc_trak)= itra(ih)  ! track-in-subevent number
            iqf(lfb_cal(1)+1)     = bmul ! put length into mother bank
          endif
        enddo ! loop of stored hits /modele
c---------------------------------------------------------------------
c reduce the Zebra output bank.
        
        if(cvolu_opt(4,2).eq.'BMAP') then
          
          incnd=(bmul-mfb_alldets)*mfb_map
          call mzpush(ixdiv_fe,lfb_map(1),0,incnd,'I')
          
        elseif(cvolu_opt(4,2).eq.'BCAL') then
          
          incnd=(bmul-mfb_alldets)*mfb_cal
          call mzpush(ixdiv_fe,lfb_cal(1),0,incnd,'I')
          
        endif 
      endif ! nhits>0 ?
 
      return
      end  
