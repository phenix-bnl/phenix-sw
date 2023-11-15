*CMZ :  2.04/00 23/11/94  13.27.24  by  Charles F. Maguire
*CMZ :  2.03/00 11/10/92  02.49.55  by  Charles F. Maguire
*-- Author :
*-- Author :
      subroutine CRK_USER()
*********************
*  FILE crk_user.f
*********************
      implicit none

c a template of CERK digi output user program

c pull out the data from CCAL bank, and print-out the data in it.

c  YA  4/25/92

*KEEP,GCFLAG.
#include "gcflag.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,SUBLINK.
#include "sublink.inc"
*KEEP,FPCLINK.
#include "fpclink.inc"
*KEND.
c var
      integer lf_c,lf_h         !pointer used in ZEBERA
      integer i,j               !loop variable
      integer cmul              !multiplicity in the bank
      integer nhit
      logical first /.true./
      save first
      integer LUN
      parameter (LUN=79)
c contents of CCAL bank
      integer icell             !PMT cell id
      integer npe               !# of photo-electrons
      real tof                  !tof of the hits
      integer izcell, iphi, isect !index in CPHO...z-index, phi-index, sector
c contents of CTRA and CHIT bank
      real xpos,ypos,zpos       !(x,y,z) coordinate of the hit
      real px,py,pz             !(px,py,pz)...3 momentum of the hit
      integer pid,itra          !particle id(GEANT code), track # (in KINE)
      real tofh
      integer ipar              !track # of parent
      real pvx,pvy,pvz          !(px,py,pz) at vertex
      real vx,vy,vz             !(x,y,z) of vertex
c control flags
      logical skip/.false./
      integer Ianswer
      save skip
c detector sub-divisions
      integer NCSECT, NCSPM, NCPMT
      integer NCELL_SECT        ! number of cells in one sector
      integer NCELL_TOT         ! total number of cells
      NCSECT     = qf(lFC_PARA + oC_N_SECT)
      NCSPM      = qf(lFC_PARA + oC_N_SPM)
      NCPMT      = qf(lFC_PARA + oC_N_PMT)
      NCELL_SECT = NCSPM * NCPMT
      NCELL_TOT  = NCELL_SECT * NCSECT
c begin
      if(first) then
        first = .false.

c     CFM: REMOVED temporarily (problem with batch test)

c       type *,'CRK....print out HITS? enter 1 if YES'
c        read(*,*) Ianswer

      if(idebug.ne.0)then
         ianswer = 1
      else
         ianswer = 0
      endif
        if(Ianswer.eq.1) then
          skip = .false.
        else
          skip = .true.
        endif
      endif
      if(skip) return
      cmul = iqf(lFC_CAL(1)+1)   ! retrieve stored number of hits
      if(cmul.gt.0)then
        write(6,100) cmul
 100    format(2x,'CRK_USER: cmul ',I5)
        do i = 1,cmul
          lf_c = lFC_CAL(1) + (i-1)*mFC_CAL + 2 ! offset into mother bank

          icell = iqf(lf_c + oFCc_CELL)
          npe = iqf(lf_c + oFCc_NPE)
          tof =  qf(lf_c + oFCc_TOF)
          isect  =  icell / NCELL_SECT
          iphi   = (icell - isect * NCELL_SECT) / (NCPMT/2)
          izcell =  icell - isect * NCELL_SECT - iphi * NCPMT/2
          write(6,101) i,icell,isect,izcell,iphi,npe,tof
 101      format(I5,': ICELL= ',I6,' POSITION = (',i1,',',I3,',',I3,')',
     $         ' NPE= ',I3,' TOF = ',e12.4)
c now retrieved data from CHIT banks pointed by this CELL
          lf_h = lqf(lFC_Cal(1) - i)
          nhit = iqf(lf_h + 1)
          lf_h = lf_h + 2
          do j = 1, nhit
            xpos = qf(lf_h + oFCh_x)
            ypos = qf(lf_h + oFCh_y)
            zpos = qf(lf_h + oFCh_z)
            tofh = qf(lf_h + oFCh_tof)
            px   = qf(lf_h + oFCh_px)
            py   = qf(lf_h + oFCh_px)
            pz   = qf(lf_h + oFCh_px)
            pid  =iqf(lf_h + oFCh_pid)
            itra =iqf(lf_h + oFCh_tra)
            ipar =iqf(lf_h + oFCh_par)
            lf_h = lf_h + mFC_hit
            write(6,102) xpos,ypos,zpos,px,py,pz,tofh,pid,itra,ipar
 102        format(2x,7F8.3,3I7)
          enddo
        enddo
      else
        write(6,103)
 103    format(2x,'CRK_USER: no hits in CCAL bank for CERENKOV')
      endif

c retrieve hits from CTRA banks
c lFC_Ctra points to a linear structure, a list of CTRA banks.
c The first  bank stores the data from hits in CMIR volume
c The second bank stores the data from hits in CTR1 volume
c The third  bank stores the data from hits in CTR2 volume

c Note that
c     lFC_Tra......................address of the firs t CTRA bank
c     lqf(lFC_Tra).................address of the second CTRA bank
c     lqf(lqf(lFC_Tra))............address of the third  CTRA bank

c 1) retrieve data from the first CTRA bank

      lf_c = lFC_Tra(1)
      cmul = iqf(lf_c + 1) ! retrieve stored number of hits
      if(cmul.gt.0)then
        write(6,200) cmul
 200    format(2x,'CRK_USER: CTRA(CMIR) # of hits = ',I5)
        lf_c = lf_c + 2
        do i = 1,cmul
          xpos = qf(lf_c + oFCt_x)
          ypos = qf(lf_c + oFCt_y)
          zpos = qf(lf_c + oFCt_z)
          pid = iqf(lf_c + oFCt_pid)
          itra = iqf(lf_c + oFCt_itra)
          pvx = qf(lf_c + oFCt_pvx)
          pvy = qf(lf_c + oFCt_pvy)
          pvz = qf(lf_c + oFCt_pvz)
          pvx = qf(lf_c + oFCt_pvx)
          vx  = qf(lf_c + oFCt_vx)
          vy  = qf(lf_c + oFCt_vy)
          vz  = qf(lf_c + oFCt_vz)
          lf_c = lf_c + mFC_Tra
          write(6,201) i,xpos,ypos,zpos,pid,itra
          write(6,203)pvx,pvz,pvz,vx,vy,vz
 201      format(I5, 3F10.3,2I10)
 203      format(2('  (',F10.3,',',F10.3,',',F10.3,')'))
        enddo
      else
        write(6,202)
 202    format(2x,'CRK_USER: no hits in CTRA bank #1 (CMIR)')
      endif

c 2) retrieve data from second CTRA bank

      lf_c = lqf(lFC_Tra(1))
      cmul = iqf(lf_c + 1) ! retrieve stored number of hits
      if(cmul.gt.0)then
        write(6,210) cmul
 210    format(2x,'CRK_USER: CTRA(CTR1) # of hits = ',I5)
        lf_c = lf_c + 2
        do i = 1,cmul
          xpos = qf(lf_c + oFCt_x)
          ypos = qf(lf_c + oFCt_y)
          zpos = qf(lf_c + oFCt_z)
          pid = iqf(lf_c + oFCt_pid)
          itra = iqf(lf_c + oFCt_itra)
          pvx = qf(lf_c + oFCt_pvx)
          pvy = qf(lf_c + oFCt_pvy)
          pvz = qf(lf_c + oFCt_pvz)
          pvx = qf(lf_c + oFCt_pvx)
          vx  = qf(lf_c + oFCt_vx)
          vy  = qf(lf_c + oFCt_vy)
          vz  = qf(lf_c + oFCt_vz)
          lf_c = lf_c + mFC_Tra
          write(6,211) i,xpos,ypos,zpos,pid,itra
          write(6,203)pvx,pvz,pvz,vx,vy,vz
 211      format(I5, 3F10.3,2I10)
        enddo
      else
        write(6,212)
 212    format(2x,'CRK_USER: no hits in CTRA bank #2 (CTR1)')
      endif

c 3) retrieve the data from the third CTRA bank

      lf_c = lqf(lqf(lFC_Tra(1)))
      cmul = iqf(lf_c + 1) ! retrieve stored number of hits
      if(cmul.gt.0)then
        write(6,220) cmul
 220    format(2x,'CRK_USER: CTRA(CTR2) # of hits = ',I5)
        lf_c = lf_c + 2
        do i = 1,cmul
          xpos = qf(lf_c + oFCt_x)
          ypos = qf(lf_c + oFCt_y)
          zpos = qf(lf_c + oFCt_z)
          pid = iqf(lf_c + oFCt_pid)
          itra = iqf(lf_c + oFCt_itra)
          pvx = qf(lf_c + oFCt_pvx)
          pvy = qf(lf_c + oFCt_pvy)
          pvz = qf(lf_c + oFCt_pvz)
          pvx = qf(lf_c + oFCt_pvx)
          vx  = qf(lf_c + oFCt_vx)
          vy  = qf(lf_c + oFCt_vy)
          vz  = qf(lf_c + oFCt_vz)
          lf_c = lf_c + mFC_Tra
          write(6,221) i,xpos,ypos,zpos,pid,itra
 221      format(I5, 3F10.3,2I10)
          write(6,203)pvx,pvz,pvz,vx,vy,vz
        enddo
      else
        write(6,222)
 222    format(2x,'CRK_USER: no hits in CTRA bank #3 (CTR2)')
      endif

c$$$      call dzsurv('CCAL structure',ixdiv_fe,lFC_Cal(1))
c$$$      call dzsurv('CTRA structure',ixdiv_fe,lFC_Tra(1))

      return
      end
