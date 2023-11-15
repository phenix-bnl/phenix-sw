c $Id: muident.f,v 1.15 2012/10/19 06:46:45 bbannier Exp $
      subroutine muident

c      subroutine muident(full,nh)      ! removed CFM, 02/21/97
c                                       ! muident is called with nor params

c    *************************************************************
c    *                                                           *
c    *  MUIDENT (vsn 1.00) muon_arm muon_identifier geometry     *
c    *                                                           *
c    *  Called by = => :: < MUN >                                 *
c    *  IN   :: full, nh                                         *
c    *  OUT  :: none                                             *
c    *                                                           *
c    *  written  by ::  Surender Saini, 12/04/93 04.57.06        *
c    *  modified by ::  C.F. Maguire,   02/21/97 fix first line  *
c    *  modified by ::  L. Villatte     03/29/02                 *
c    *                  1) fix placement MuID sensitive volumes  *
c    *                     whose thickness has been increased to *
c    *                     improve simulation of response of     *
c    *                     particles entering the MuID from the  *
c    *                     square hole                           *
c    *                  2) South Arm : extend south square hole  *
c    *                     to accomodate DX magnet               *
c    *                  3) South Arm : add MuID shielding of run2*
c    *                     (installed 10/10/2001)                *
c    *                  4) South Arm : add DX magnet             *
c    *                                                           *
c    *  modufied by ::  V. Dzhirdzhadze 09/22/02                 *
c    *                  1) Modifyed to simulate Run2 and Run3    *
c    *                     shieldings.                           *
c    *                  2) Introduced in phnx.par an integer     *
c    *                     constant: muid_shield which = 0 for   *
c    *                     Run2 and = 1 for Run3 and after       *
c    *                  3) Installed muon shielding for North    *
c    *                     and South arms (Run3).                *
c    *                  4) Extend North arm to put DX magnet     *
c    *                     inside (Run3).                        *
c    *                  5) Removed all hole volumes from the     *
c    *                     shielding volumes (Run3).             *
c    *                  6) Run2 enviroment same as in previous   ********************
c                          release.                              
c    May 2010 Hubert van Hecke: define volume EDXM as a polycone instead of a sphere.
c                                                            
c************************************************************************************

      implicit none

c = ==========================================================

#include "pisalun.inc"

#include "guphnx.inc"

#include "gclist.inc"
#include "gconst.inc"
#include "gugeom.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpnlink.inc"
#include "fpmlink.inc"
#include "gcunit.inc"

c = ==========================================================

      real*4 pai, cdtr,crtd
      common/uconst/pai,cdtr,crtd

      real*4 pist_ang,plug_ang,shad_ang,shneuz2,pbcurz2,znose1,
     + zncref,thncref
      common /ugeom_muon2/ pist_ang,plug_ang,shad_ang,shneuz2,
     + pbcurz2,znose1,zncref,thncref

      real*4  zdont2(2),zyoke2(2)
      common /ugeom_muon3/zdont2,zyoke2


      character*4 v_m_name,v_i_name,v_i_shape,v_hi_name,
     + v_ab_name,v_cr_name

      integer iopara,i,ii,ia,iaa
      integer  npar,nmed,ivolu
      integer  ipl,nstrdp,nstrdn

      real     xv,yv,zv,zvhole,zvshld
      real     gap_size,total_muid_thick,str_thick
      real     zabspl2,zmuend,dzgap

      integer*4 nh             !set before call in gugeom
      character*4 full         ! set before call in gugeom

      ! null rotation
      real nul_rot(6) /90.0,0.0,90.0,90.0,0.0,0.0/

      real strhpar(3),stpspar(3),stcspar(3),stgspar(3),strdpar(3),
     + yokepar(5),dontpar(5),abspar(3),labspar(3),habspar(3),
     + holepar(3),flrpar(3),dxmvpar(3),dxmgpar(3), dxmepar(6),
     & dxnepar(21)
      real zmuid_pl(max_muid_planes)
      real dtorad, d1,d2,r1,r2,delphi,RR
      parameter (dtorad = 0.01745)    ! degrees to radians

      integer muid_channels/200/
      integer muid_alldets
      common /muid_hitcom/ muid_alldets

      integer  mun_para_geom
      integer*4 ncr,nc,iodd
      real*4    crack_abs_gap, crapar(3),dshift

      integer  idtype,nbitsv(2),nv,nwpa,nwsa,iset,idet,kk
      character*4 namesv(2),set_id,det_id

      character*50 par_file

      integer lpzi,izpl, nzdx

      ! number of detectors
      integer nr_muid_planes, ndet_per_p, nr_muon_arms, ndet_per_pl

      real pgon_par(3*max_muhp + 4)

      ! Constant values
      data zmax_muarm/930./
      data set_id/'MUN '/, det_id/'MUGS'/
      data nv/2/,idtype/7/
      data namesv/'MUSD','MUGS'/
      data nbitsv/6,2/

      ! Default values
      data muid_flg/1/,nr_muon_arms/2/,mu_floor_flg/1/
      data nr_muid_planes/6/,ndet_per_pl/1/
      data muid_zmin/710./, muid_zmax/890.0/, beam_height/520./
      data mu_top_height/1039.4/, mu_yoke_thick/2*15./
      data muid_ang/37./
      data mu_gas_thick/1.0/, mu_plas_thick/0.15/
      data mu_donut_thick/10.,0.,5.,10./,mu_floor_thick/3.81/

      data mu_hi_abs/0.0,2*2.0,3*2.5/
      data mu_abs_thick/0.0,2*5.0,3*10.0/
      data mu_hi_med/40,5*41/
      data mu_lo_med/45,5*447/,nmed_yoke/5/,nmed_mu_ps/902/

      data nmed_mu_gas/443/,nmed_mu_cs/449/,nmed_mu_sh/6/
      data nmed_mu_sd/6/,nmed_mudn/5/,nmed_muhl/6/,nmed_mufl/41/
      data mu_hi_color/4/,mu_lo_color/5/,color_muid/4/
      data color_hole/6/,color_dont/2/,color_floor/7/,color_strd/3/
      data color_yoke/2/,zyoke/2*630.0/
      data rykmin1/2*11.0/,rykmin2/2*11.0/,rmin_donut/2*11.0/
      data rmax1_donut/111.086,0.,160.,108.404/
      data rmax2_donut/111.086,0.,150.,88.404/
      data zgap_yoke_abs/50./,zgap_labs_ldet/3.0/,muid_delx/0./
      data str_xstag/0.2/
      data muhl_shld_flag/0/,nmed_muhl_shld/447/,color_muhl_shld/4/
      data z_muhl_shld/690.2,895./,thick_muhl_shld/5./
      data muhl_config_flag/2/,muabs_config_flag/2/,npl_muhl/6/
      data z_muhl/690.2,750.,790.,830.,890.,895./
      data rmax_muhl/80.0,84.0,88.5,93.0,99.7,100.2/
      data rmin_muhl/6*0./
      data muid_shield/1/      ! is 1 except for run 2

c Initialization of local variables

      data strhpar/3*0./,stpspar/3*0./,stcspar/3*0./,stgspar/3*0./
      data strdpar/3*0./,yokepar/5*0./,dontpar/5*0./,abspar/3*0./
      data labspar/3*0./,habspar/3*0./,holepar/3*0./,flrpar/3*0./

c---------------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun

c ---------------------------------------------------------------------

c Readthe geometery file segment


      write( *,* ) 'muident - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = mun_par, err = 999 )

      rewind(itf_lun)
      read( itf_lun, nml = mag_par, err = 998 )


c     check the muid_shield against the RHICRUN number in guphnx common block


      if(RHICRUN.eq.2.and.MUID_SHIELD.eq.1)then
          write(6,*) ' Inconsistent RHIC Run and MUID_SHIELD'
          write(6,*) ' RHICRUN (from SETRHIC command) is set at 2'
          write(6,*) ' MUID_SHIELD (from geometry) is set at 1'
          write(6,*) ' You must change one or the other value'
          stop ' PISA stopping in MUIDENT'
      endif

      if(RHICRUN.ge.3.and.MUID_SHIELD.eq.0)then
          write(6,*) ' Inconsistent RHIC Run and MUID_SHIELD'
          write(6,*) ' RHICRUN (from SETRHIC command) is not set at 2'
          write(6,*) ' MUID_SHIELD (from geometry) is set at 0'
          write(6,*) ' You must change one or the other value'
         stop ' PISA stopping in MUIDENT'
      endif

      mun_para_geom = 54 + 2*6 + 4.*max_muid_planes + 3*max_muhp

c Geometry parameters are read. Now store it in 'NPRA' bank

c BOOK 'NPRA' bank

        call mzform('PARA', '-F',iopara)
        call mzbook(
     &       ixdiv_fr,          !division
     &       lfn_para,          !return address of the bank
     &       lfn_para,          !supporting link
     &       1,                 !JB=1 means create top level bank
     &       'PARA',            !bank name
     &       0,                 !NL=0. No links
     &       0,                 !NS=0. No supporting (down) links
     &       mun_para_geom,     !# of data words
     &       iopara,            !IO format word
     &       0)                 !NZERO=0 means that whole bank is cleared

c copy raw geometry parameters into 'NPRA' bank

      ii = 0
      ii = ii + 1
      qf(lfn_para + ii) = real(nr_muon_arms)
      ii = ii + 1
      qf(lfn_para + ii) = real(nr_muid_planes)
      ii = ii + 1
      qf(lfn_para + ii) = real(ndet_per_pl)
      ii = ii + 1
      qf(lfn_para + ii) = real(mu_floor_flg)
      ii = ii + 1
      qf(lfn_para + ii) = muid_zmin
      ii = ii + 1
      qf(lfn_para + ii) = muid_zmax
      ii = ii + 1
      qf(lfn_para + ii) = muid_ang
      ii = ii + 1
      qf(lfn_para + ii) = beam_height
      ii = ii + 1
      qf(lfn_para + ii) = mu_top_height
      ii = ii + 1
      qf(lfn_para + ii) = mu_floor_height
      ii = ii + 1
      qf(lfn_para + ii) = mu_strp_thick
      ii = ii + 1
      qf(lfn_para + ii) = mu_gas_thick
      ii = ii + 1
      qf(lfn_para + ii) = mu_plas_thick

      do i = 1,nr_muon_arms
        ii = ii + 1
        qf(lfn_para + ii) = mu_yoke_thick(i)
      enddo

      do i = 1,nr_muon_arms
        ii = ii + 1
        qf(lfn_para + ii) = mu_donut_thick(1,i)
        ii = ii + 1
        qf(lfn_para + ii) = mu_donut_thick(2,i)
      enddo

      do i = 1,nr_muon_arms
        ii = ii + 1
        qf(lfn_para + ii) = rmax1_donut(1,i)
        ii = ii + 1
        qf(lfn_para + ii) = rmax1_donut(2,i)
        ii = ii + 1
        qf(lfn_para + ii) = rmax2_donut(1,i)
        ii = ii + 1
        qf(lfn_para + ii) = rmax2_donut(2,i)
      enddo

      ii = ii + 1
      qf(lfn_para + ii) = mu_floor_thick

      do i=1,max_muid_planes
          ii = ii + 1
          qf(lfn_para + ii) = mu_abs_thick(i)
      end do
      do i=1,max_muid_planes
          ii = ii + 1
          qf(lfn_para + ii) = mu_hi_abs(i)
      end do
      do i=1,max_muid_planes
          ii = ii + 1
          qf(lfn_para + ii) = mu_hi_med(i)
      end do
      do i=1,max_muid_planes
          ii = ii + 1
          qf(lfn_para + ii) = mu_lo_med(i)
      end do

      ii = ii + 1
      qf(lfn_para + ii) = real(nmed_mu_ps)
      ii = ii + 1
      qf(lfn_para + ii) = real(nmed_mu_gas)
      ii = ii + 1
      qf(lfn_para + ii) = real(nmed_mu_cs)
      ii = ii + 1
      qf(lfn_para + ii) = real(nmed_mu_sh)
      ii = ii + 1
      qf(lfn_para + ii) = real(nmed_mu_sd)
      ii = ii + 1
      qf(lfn_para + ii) = real(nmed_mudn)
      ii = ii + 1
      qf(lfn_para + ii) = real(nmed_muhl)
      ii = ii + 1
      qf(lfn_para + ii) = real(nmed_mufl)
      ii = ii + 1
      qf(lfn_para + ii) = real(nmed_yoke)
      ii = ii + 1
      qf(lfn_para + ii) = real(mu_hi_color)
      ii = ii + 1
      qf(lfn_para + ii) = real(mu_lo_color)
      ii = ii + 1
      qf(lfn_para + ii) = real(color_muid)
      ii = ii + 1
      qf(lfn_para + ii) = real(color_hole)
      ii = ii + 1
      qf(lfn_para + ii) = real(color_dont)
      ii = ii + 1
      qf(lfn_para + ii) = real(color_floor)
      ii = ii + 1
      qf(lfn_para + ii) = real(color_strd)
      ii = ii + 1
      qf(lfn_para + ii) = real(color_yoke)

      do i = 1,nr_muon_arms
          ii = ii + 1
          qf(lfn_para + ii) = rykmin1(i)
      enddo

      do i = 1,nr_muon_arms
          ii = ii + 1
          qf(lfn_para + ii) = rykmin2(i)
      enddo

      do i = 1,nr_muon_arms
        ii = ii + 1
        qf(lfn_para + ii) = rmin_donut(i)
      enddo

      do i = 1,nr_muon_arms
        ii = ii + 1
        qf(lfn_para + ii) = zyoke(i)
      enddo

      ii = ii + 1
      qf(lfn_para + ii) = zgap_yoke_abs
      ii = ii + 1
      qf(lfn_para + ii) = zgap_labs_ldet
      ii = ii + 1
      qf(lfn_para + ii) = muid_delx
      ii = ii + 1
      qf(lfn_para + ii) = str_xstag
      ii = ii + 1
      qf(lfn_para + ii) = muhl_shld_flag
      ii = ii + 1
      qf(lfn_para + ii) = color_muhl_shld
      ii = ii + 1
      qf(lfn_para + ii) = nmed_muhl_shld
      ii = ii + 1
      qf(lfn_para + ii) = z_muhl_shld(1)
      ii = ii + 1
      qf(lfn_para + ii) = z_muhl_shld(2)
      ii = ii + 1
      qf(lfn_para + ii) = thick_muhl_shld

c New parameters added ( 17-NOV-1993) / S. Saini

      ii = ii + 1
      qf(lfn_para + ii) = muhl_config_flag
      ii = ii + 1
      qf(lfn_para + ii) = muabs_config_flag
      ii = ii + 1
      qf(lfn_para + ii) = npl_muhl

      do i=1,max_muhp
          ii = ii + 1
          qf(lfn_para + ii) = z_muhl(i)
      end do
      do i=1,max_muhp
          ii = ii + 1
          qf(lfn_para + ii) = rmin_muhl(i)
      end do
      do i=1,max_muhp
          ii = ii + 1
          qf(lfn_para + ii) = rmax_muhl(i)
      end do

c     end of NPRA bank filling

      v_m_name = 'HALL'

c Install the Muon_magnet_yoke, donut behind the yoke

      do ia = 1,nr_muon_arms

          iaa = 2 * mod(ia,2) - 1
          v_i_shape= 'CONE'
          nmed = nmed_yoke
          npar = 5

          zyoke2(ia) = zyoke(ia) + 2.*iaa*mu_yoke_thick(ia)
          yokepar(1) = mu_yoke_thick(ia)
          if(ia.eq.1) then
              v_i_name = 'MUYK'
              yokepar(2) = rykmin1(ia)
              yokepar(3) = abs(zyoke(ia))*tan(shad_ang*cdtr)
     1            + mega_thick
              yokepar(4) = rykmin2(ia)
              yokepar(5) = yokepar(3)
          else
              v_i_name = 'YKMU'
              yokepar(4) = rykmin1(ia)
              yokepar(5) = abs(zyoke(ia))*tan(shad_ang*cdtr)
     1            + mega_thick
              yokepar(2) = rykmin2(ia)
              yokepar(3) = yokepar(5)
          endif

          call gsvolu(v_i_name,v_i_shape,nmed,yokepar,npar,ivolu)
          call uvolattr(v_i_name,color_yoke)

          v_m_name = 'HALL'
          xv = 0.
          yv = 0.
          zv = zyoke(ia) + iaa*yokepar(1) + iaa*z_roll(ia)
          call gspos(v_i_name,1,v_m_name,xv,yv,zv,1,'only')

          if(ia.eq.1) then
              v_i_name = 'MUDN'
          else
              v_i_name = 'DNMU'
          endif

          v_i_shape= 'CONE'
          nmed = nmed_mudn
          npar = 5

          dontpar(1) = mu_donut_thick(1,ia)
          dontpar(2) = rmin_donut(ia)
          dontpar(3) = rmax1_donut(1,ia)
          dontpar(4) = rmin_donut(ia)
          dontpar(5) = rmax2_donut(1,ia)
          call gsvolu(v_i_name,v_i_shape,nmed,dontpar,npar,ivolu)
          call uvolattr(v_i_name,color_dont)

          zv = zyoke2(ia) + iaa*dontpar(1) + iaa*z_roll(ia)
          zdont2(ia) = zyoke2(ia) + 2.*iaa*dontpar(1)
          call gspos(v_i_name,ia,v_m_name,xv,yv,zv,1,'only')

          if(rmax1_donut(2,ia).ne.0.) then

              if(ia.eq.1) then
                  v_i_name= 'MUDO'
              else
                  v_i_name= 'DOMU'
              endif

              dontpar(1) = mu_donut_thick(2,ia)
              dontpar(3) = rmax1_donut(2,ia)
              dontpar(5) = rmax2_donut(2,ia)
              call gsvolu(v_i_name,v_i_shape,nmed,dontpar,npar,ivolu)
              call uvolattr(v_i_name,color_dont)
              zv = zdont2(ia) + iaa*dontpar(1) + iaa*z_roll(ia)
              zdont2(ia) = zdont2(ia) + 2.*iaa*dontpar(1)
              call gspos(v_i_name,ia,v_m_name,xv,yv,zv,1,'only')

          endif

      enddo

      write(lout,'('' muident - Muon_Yoke placed at Z='',2f10.4)')zyoke
      write(lout,'('' muident - Muon_Donut placed at Z='',2f10.4)')zyoke2
      write(lout,'('' muident - Back of donut at Z='',2f10.4)')zdont2
      write(lout,'('' muident - Muon Yoke & Donut installed'')')

c Construct and Install the Muon_Identfier

      if(muid_flg .eq. 1) then

          if(nr_muid_planes .gt. max_muid_planes ) goto 9992

          muid_alldets =
     +        nr_muon_arms*nr_muid_planes
     +        *ndet_per_pl*muid_channels

c Make  streamer_tube_plane components

          v_i_shape = 'BOX '
          npar = 3

c (a)     Make streamer_tube housing

          v_i_name = 'MUSH'
          nmed = nmed_mu_sh

          str_thick =
     &           2.*mu_gas_thick + 4.*mu_plas_thick + 4.*mu_strp_thick
          if (muabs_config_flag .eq. 1) then
              strhpar(1)=
     &          mu_abs_z(nr_muid_planes)*tan(muid_ang*cdtr)+muid_delx
          else if (muabs_config_flag .eq. 2) then
              strhpar(1) = mu_abs_x
          end if

          strhpar(2) = (mu_top_height - mu_floor_height)/2.
          strhpar(3) = str_thick/2.

          call gsvolu(v_i_name,v_i_shape,nmed,strhpar,npar,ivolu)

c (b)     Make streamer tube plastic sides

          v_i_name = 'MUPS'
          nmed = nmed_mu_ps

          stpspar(1) = strhpar(1)
          stpspar(2) = strhpar(2)
          stpspar(3) = mu_plas_thick

          call gsvolu(v_i_name,v_i_shape,nmed,stpspar,npar,ivolu)

c  (c)    Make Streamer tube Copper strips

          v_i_name = 'MUCS'
          nmed = nmed_mu_cs

          stcspar(1) = strhpar(1)
          stcspar(2) = strhpar(2)
          stcspar(3) = mu_strp_thick

          call gsvolu(v_i_name,v_i_shape,nmed,stcspar,npar,ivolu)

c  (c)    Make Streamer tube gas_volume

          v_i_name = 'MUGS'
          nmed = nmed_mu_gas

          stgspar(1) = strhpar(1)
          stgspar(2) = strhpar(2)
          stgspar(3) = mu_gas_thick

          call gsvolu(v_i_name,v_i_shape,nmed,stgspar,npar,ivolu)

c Now     assemble the streamer tube plane using the above components

c Posi    tion  Copper_strips inside the streamer_tube_housing

          v_m_name = 'MUSH'
          v_i_name = 'MUCS'
          xv = 0.
          yv = 0.
          zv = -strhpar(3)+ stcspar(3)
          call gspos(v_i_name,1,v_m_name,xv,yv,zv,1,'only')
          zv = strhpar(3)- stcspar(3)
          call gspos(v_i_name,2,v_m_name,xv,yv,zv,1,'only')

c Posi    tion plastic siding inside the streamer_tube_housing

          v_i_name = 'MUPS'
          zv = -strhpar(3)+ 2.*stcspar(3) + stpspar(3)
          call gspos(v_i_name,1,v_m_name,xv,yv,zv,1,'only')
          zv = strhpar(3)- 2.*stcspar(3) - stpspar(3)
          call gspos(v_i_name,2,v_m_name,xv,yv,zv,1,'only')

c Posi    tion streamer_tube_gas_volume inside the streamer_tube_housing

          v_i_name = 'MUGS'
          zv = 0.
          call gspos(v_i_name,1,v_m_name,xv,yv,zv,1,'only')

c Now     make the streamer_tube_detector plane

          v_i_name = 'MUSD'
          nmed = nmed_mu_sd

          strdpar(1) = strhpar(1)
          strdpar(2) = strhpar(2)

          if(ndet_per_pl .eq. 1)then
              strdpar(3) = strhpar(3)
          else if(ndet_per_pl .eq. 2)then
              strdpar(3) = 2.*strhpar(3) + 0.05
          end if

          call gsvolu(v_i_name,v_i_shape,nmed,strdpar,npar,ivolu)
          call uvolattr(v_i_name,color_strd)

          v_m_name = 'MUSD'
          v_i_name = 'MUSH'
          xv = 0.
          yv = 0.
          zv = -strdpar(3)+ strhpar(3)
          call gspos(v_i_name,1,v_m_name,xv,yv,zv,1,'only')

          if(ndet_per_pl .eq. 2)then
              xv = str_xstag
              zv = strdpar(3)- strhpar(3)
              call gspos(v_i_name,2,v_m_name,xv,yv,zv,1,'only')
          end if

c Install the Muon_identifier now

          total_muid_thick=mu_abs_z(nr_muid_planes) +
     +        mu_abs_thick(nr_muid_planes)+2.*zgap_labs_ldet
          gap_size=(zmax_muarm -  total_muid_thick)

          if(gap_size .lt. 0.0)then
              write(6,'(/2x,''MUID size exceeds HALL dimensions'')')
              stop
          endif

          v_m_name = 'HALL'
          v_i_shape= 'BOX '
          npar = 3
          if(muabs_config_flag .eq. 1)then
              abspar(1) =
     &           mu_abs_z(nr_muid_planes)*tan(muid_ang*cdtr)+muid_delx
          end if
          abspar(2) = (mu_top_height - mu_floor_height)/2.
          nstrdp = 0
          nstrdn = nr_muid_planes

          do ipl = 1, nr_muid_planes

              if(muabs_config_flag .ge. 2)then
                  abspar(1) = mu_abs_x
              end if

              if(muabs_config_flag .eq. 3)then
                  abspar(2) = mu_abs_y
              end if

              yv = abspar(2) - (beam_height - mu_floor_height)


c For first streamer_tube_detector plane , there is no absorber

              if(ipl .eq. 1) then

                  zv = mu_abs_z(2) - mu_abs_thick(2) - strdpar(3)
                  v_i_name = 'MUSD'
                  nstrdp = nstrdp + 1
                  call gspos(v_i_name,nstrdp,v_m_name,xv,yv,zv,1,'only')
                  zmuid_pl(ipl) = zv
                  if(nr_muon_arms .eq. 2) then
                      nstrdn = nstrdn + 1
C.....                .  -zv -4 to fit square hall...
                      call gspos(v_i_name,nstrdn,v_m_name,
     &                           xv,yv,-zv-4.,1,'only')
                  end if

              else

                  zv = mu_abs_z(ipl)
                  abspar(3) = mu_abs_thick(ipl)
                  labspar(1) = abspar(1)
                  if (mu_flag_cracks.eq.3 .and. num_cracks.eq.2) then
                      if( ipl .eq. 3) then
                          labspar(1) = abspar(1) - crack_shift
                      endif
                  endif
                  labspar(2)= abspar(2)
                  labspar(3)= abspar(3)
                  habspar(1)= labspar(1) - mu_hi_abs(ipl)
                  habspar(2)= labspar(2) - mu_hi_abs(ipl)
                  habspar(3)= labspar(3) - mu_hi_abs(ipl)

                  if(ipl .lt. 10)then
                      write (v_ab_name,'(a3,i1)') 'ML0',ipl
                      write (v_hi_name,'(a3,i1)') 'MH0',ipl
                  else
                      write (v_ab_name,'(a2,i2)') 'ML',ipl
                      write (v_hi_name,'(a2,i2)') 'MH',ipl
                  end if

                  nmed = mu_lo_med(ipl)
                  v_i_name = v_ab_name
                  call gsvolu(v_i_name, v_i_shape, nmed,
     &                        labspar, npar, ivolu)
                  call uvolattr(v_i_name,mu_lo_color)

                  nmed = mu_hi_med(ipl)
                  v_i_name = v_hi_name
                  call gsvolu(v_i_name, v_i_shape, nmed,
     &                        habspar, npar, ivolu)
                  call uvolattr(v_i_name,mu_hi_color)

c Put             hi_cutoff_absorber inside the low_cutoff_absorber

                  v_m_name = v_ab_name
                  v_i_name = v_hi_name
                  call gspos(v_i_name,1,v_m_name,0.,0.,0.,1,'only')

c z_po            sition of the absorber layer
                  zv = zv + abspar(3)


c Make            cracks in the iron

               if (mu_flag_cracks .ge. 1) then
                 crack_abs_gap = (2.*abspar(1) - num_cracks*crack_size)/
     +                           (num_cracks + 1)

c Now                 construct a crack of size crack_size in the vertical direction

                      crapar(1) = crack_size/2.
                      crapar(2) = abspar(2)
                      crapar(3) = abspar(3)
                      ncr = 0

                      npar = 3
                      nmed = nmed_cracks
                      write (v_cr_name,'(a3,i1)') 'MCR',ipl
                      v_i_name = v_cr_name
                      v_m_name = v_ab_name
                      v_i_shape= 'BOX'
                      call gsvolu(v_i_name, v_i_shape, nmed,
     +                            crapar, npar, ivolu)
                      call uvolattr(v_i_name,crack_color)

                      xv = -abspar(1)
                      do nc = 1, num_cracks
                          if( mu_flag_cracks .eq. 1) then
                              if(nc .eq. 2 .or. nc .eq. 4 )then
                                  dshift =
     &                              crack_abs_gap + (ipl-2)*crack_shift
                              else if(nc .eq. 3 )then
                                  dshift =
     &                              crack_abs_gap-2.*(ipl-2)*crack_shift
                              else
                                  dshift = crack_abs_gap
                              end if
                          else if( mu_flag_cracks .eq. 2) then
                              dshift = crack_abs_gap
                              if( nc .eq. 1)then
                                  if(ipl .gt. 2)then
                                      iodd = 2.*(ipl/2) - ipl
                                      if(iodd .eq. 0)iodd = 1
                                      dshift = dshift + iodd*crack_shift
                                  end if
                              end if
                          else if (mu_flag_cracks.eq.3 .and.
     &                             num_cracks.eq.2) then
                              dshift = crack_abs_gap
                              if( ipl .eq. 3)then
                                  iodd = 1 - 3*(nc/2)
                                  dshift = dshift + iodd*crack_shift
                              end if
                          end if
                          xv = xv + dshift + crapar(1)
c                         write(lout,'(5x,''xv,dsh='',2f10.4)')xv,dshift
                          ncr = ncr + 1
                     call gspos(v_i_name,ncr,v_m_name,xv,0.,0.,1,'only')
                          xv = xv + crapar(1)
                      end do
                  end if

c Now position the absorber plane in the mother volume

                  v_m_name = 'HALL'
                  v_i_name = v_ab_name
                  xv = 0.
c write(lout,'(5x,''zv,abspar3 = '',2f10.4)')zv,abspar(3)
                  call gspos(v_i_name,1,v_m_name,xv,yv,zv,1,'only')

                  if(nr_muon_arms .eq. 2)then
                      call gspos(v_i_name,2,v_m_name,xv,yv,-zv,1,'only')
                  end if


c Position the streamer_tube_detector plane in the mother volume

                  v_i_name = 'MUSD'

                  ! the last muid plane does not exist
                  ! however the last absorber (defined above) does
                  ! which is why one has to loop from 1 to 6, create the 6th plane (and absorber)
                  ! but place the absorber _only_ in the HALL frame.
                  if (ipl .lt. nr_muid_planes) then

                      dzgap=
     +                    0.5*(mu_abs_z(ipl+1) - mu_abs_thick(ipl+1) -
     +                    mu_abs_z(ipl)   - mu_abs_thick(ipl))
                      zv = zv + abspar(3) + dzgap
                      nstrdp = nstrdp + 1

                 call gspos(v_i_name,nstrdp,v_m_name,xv,yv,zv,1,'only')
                      zmuid_pl(ipl) = zv
                  end if




c                 write(lout,'(5x,''zv,dzgap = '',2f10.4)')zv,dzgap

                  if(nr_muon_arms .eq. 2)then
                      nstrdn = nstrdn + 1
                 call gspos(v_i_name,nstrdn,v_m_name,xv,yv,-zv,1,'only')
                  end if
              end if
          end do

c Now drill a cylindrical hole through the MUID, having the dimensions
c equal to the radius of the donut

          v_i_name = 'MUHL'
          nmed = nmed_muhl
          zmuend = muid_zmax + zgap_labs_ldet+ 4.*strdpar(3)
          dzhole = (zmuend - (zdont2(1) + 0.2))/2.
          zvhole = zmuend
          zvshld = 0.

          if(muhl_config_flag .eq. 1)then

              v_i_shape= 'TUBE'
              npar = 3

              holepar(1) = 0.0
              holepar(2) = dontpar(2)
              holepar(3) = dzhole
              call gsvolu(v_i_name,v_i_shape,nmed,holepar,npar,ivolu)
              call uvolattr(v_i_name,color_hole)

          else if(muhl_config_flag .eq. 2)then

              v_i_shape= 'PGON'
              zvhole = 0.
              npar = 3*npl_muhl + 4
              pgon_par(1) = 45.0          ! lower phi limit
              pgon_par(2) = 360.0         ! range in phi
              pgon_par(3) = 4             ! number of sides
              pgon_par(4) = npl_muhl      ! # of z_points
              rmax_1 = rmax_muhl(1)
              if(z_muhl(1) .lt. 0.0)z_muhl(1) = zdont2(1) + 0.2

              do kk = 1,npl_muhl
                  pgon_par(3*kk+2) = z_muhl(kk)
                  if(kk.eq.2.and.muid_shield.eq.1)pgon_par(3*kk+2)=1100.
                  pgon_par(3*kk+3) = rmin_muhl(kk)
                  pgon_par(3*kk+4) = rmax_muhl(kk)
              end do

              call gsvolu(v_i_name,v_i_shape,nmed,pgon_par,npar,ivolu)
              call uvolattr(v_i_name,color_hole)

              if(nr_muon_arms .eq. 2)then

                  v_i_name = 'LHUM'
                  lpzi = 1
                  do izpl = npl_muhl,1,-1
                      pgon_par(3*lpzi+2) = -z_muhl(izpl)

                      ! Extend south square hole to accomodate DX
                      if (lpzi .eq. 1) then
                          pgon_par(3*lpzi+2) = -1100.
                      endif
                      pgon_par(3*lpzi+3) = rmin_muhl(izpl)
                      pgon_par(3*lpzi+4) = rmax_muhl(izpl)
                      lpzi = lpzi + 1
                  end do

                  call gsvolu(v_i_name, v_i_shape, nmed,
     &                        pgon_par, npar, ivolu)
                  call uvolattr(v_i_name, color_hole)



C Shielding Run 2 (installed 10/10/2001): create floor, walls

C........floor

                  v_i_name = 'SFLO'
                  v_i_shape= 'BOX '
                  npar = 3
                  nmed = 46 !26 Iron not  Aluminum

                  if(muid_shield.eq.0)then
                      flrpar(1) = 60.
                      flrpar(2) = 7.5
                      flrpar(3) = 80.
                  endif

                  if(muid_shield.eq.1)then
                      flrpar(1) = 80.58
                      flrpar(2) = 7.62
                      flrpar(3) = 102.235
                  endif

                  call gsvolu(v_i_name,v_i_shape,nmed,flrpar,npar,ivolu)
                  call uvolattr(v_i_name,color_hole+1)

C....... Ceiling

                  if(muid_shield.eq.1)then
                      v_i_name = 'SCIE'
                      call gsvolu(v_i_name, v_i_shape, nmed,
     &                            flrpar, npar, ivolu)
                      call uvolattr(v_i_name, color_hole+1)
                  endif

C........ EAST wall

                  v_i_name = 'SEAS'
                  npar = 3

                  if(muid_shield.eq.0)then
                      flrpar(1) = 7.5
                      flrpar(2) = 60.
                      flrpar(3) = 80.
                  endif

                  if(muid_shield.eq.1)then
                      flrpar(1) = 7.62
                      flrpar(2) = 61.455
                      flrpar(3) = 102.235
                  endif

                  call gsvolu(v_i_name,v_i_shape,nmed,flrpar,npar,ivolu)
                  call uvolattr(v_i_name,color_hole+1)

C........West wall

                  v_i_name = 'SWEA'

                  call gsvolu(v_i_name,v_i_shape,nmed,flrpar,npar,ivolu)
                  call uvolattr(v_i_name,color_hole+1)

C....Holes for shielding (floor)


                  v_i_name = 'FLHO'
                  nmed = 6

                  flrpar(1) = 30.
                  flrpar(2) = 7.5
                  flrpar(3) = 30.
                  call gsvolu(v_i_name,v_i_shape,nmed,flrpar,npar,ivolu)
                  call uvolattr(v_i_name,color_hole)

C....Holes for shielding (west wall)

                  v_i_name = 'WEHO'

                  flrpar(1) = 7.5
                  flrpar(2) = 15.
                  flrpar(3) = 15.
                  call gsvolu(v_i_name,v_i_shape,nmed,flrpar,npar,ivolu)
                  call uvolattr(v_i_name,color_hole)

C...... Aluminum in hole (floor)

                  v_i_name = 'FLHA'
                  nmed = 26

                  flrpar(1) = 30.
                  flrpar(2) = 1.25
                  flrpar(3) = 30.
                  call gsvolu(v_i_name,v_i_shape,nmed,flrpar,npar,ivolu)
                  call uvolattr(v_i_name,color_hole-1)

C ..... Position of the holes

                  if(muid_shield.eq.0)then
                      v_i_name = 'FLHA'
                      v_m_name = 'FLHO'
                      xv = 0.
                      yv = -6.25
                      zv = 0.

                      call gspos(v_i_name,1,v_m_name,xv,yv,zv,1,'ONLY')

                      v_i_name = 'FLHO'
                      v_m_name = 'SFLO'
                      xv = 0.
                      yv = 0.
                      zv = 50.

                      call gspos(v_i_name,1,v_m_name,xv,yv,zv,1,'ONLY')

                      v_i_name = 'WEHO'
                      v_m_name = 'SWEA'
                      xv = 0.
                      yv = 45.
                      zv = 65.

                      call gspos(v_i_name,1,v_m_name,xv,yv,-zv,1,'ONLY')
                  endif

C...... Create DXMH volume for DX Magnet

                  nmed = 6
                  v_i_name = 'DXMH'
                  v_i_shape= 'TUBE'
                  dxmvpar(1) = 0.
                  dxmvpar(2) = 75.5
                  dxmvpar(3) = 124.5
                  call gsvolu(v_i_name, v_i_shape, nmed,
     &                        dxmvpar, npar, ivolu)
                  call uvolattr(v_i_name, color_hole)

              end if
          end if

          v_i_name = 'MUHL'
          v_m_name = 'HALL'
          xv = 0.
          yv = 0.
          zv = zvhole

          call gspos(v_i_name,1,v_m_name,xv,yv,zv,1,'ONLY')
          if(nr_muon_arms .eq. 2)then

              v_i_name = 'LHUM'
              call gspos(v_i_name,2,v_m_name,xv,yv,-zv,1,'ONLY')

          end if

C Posi    tion of floor and walls in LHUM volume

          v_m_name = 'LHUM'
          v_i_name = 'SFLO'

          if(muid_shield.eq.0)then
              xv = 0.
              yv = -74.7
              zv = 801.
          endif

          if(muid_shield.eq.1)then
              xv = 0.
              yv = -74.6
              zv = 785.3
          endif

          call gspos(v_i_name,1,v_m_name,xv,yv, -zv,1,'ONLY')

C Posi    tion of floor in MUHL volume NORTH

          if(muid_shield.eq.1)then
              v_m_name = 'MUHL'
              zv = 785.3
              call gspos(v_i_name,1,v_m_name,xv,yv, zv,1,'ONLY')

C Posi        tion of ceiling in LHUM volume SOUTH

              v_m_name = 'LHUM'
              v_i_name = 'SCIE'
              xv = 0.
              yv = 63.57
              zv = 785.3
              call gspos(v_i_name,1,v_m_name,xv,yv, -zv,1,'ONLY')

C Posi        tion of ceiling  in MUHL volume NORTH

              v_m_name = 'MUHL'
              call gspos(v_i_name,1,v_m_name,xv,yv, zv,1,'ONLY')
          endif

C Posi    tion of East wall in LHUM volume SOUTH

          v_m_name = 'LHUM'
          v_i_name = 'SEAS'

          if(muid_shield.eq.0)then
              xv = -52.5
              yv = -7.2
              zv = 801.
          endif

          if(muid_shield.eq.1)then
              v_m_name = 'LHUM'
              xv = -72.96
              yv = -5.525
              zv = 785.3
          endif

          call gspos(v_i_name,1,v_m_name,xv,yv, -zv,1,'ONLY')

C Posi    tion of East wall in MUHL volume NORTH

          if(muid_shield.eq.1)then
              v_m_name = 'MUHL'
              zv = 785.3
              call gspos(v_i_name,1,v_m_name,xv,yv, zv,1,'ONLY')
          endif

C Posi    tion of West wall in LHUM volume SOUTH

          v_i_name = 'SWEA'
          v_m_name = 'LHUM'

          if(muid_shield.eq.0)then
              xv = 52.5
              yv = -7.2
              zv = 801.
          endif

          if(muid_shield.eq.1)then
              v_m_name = 'LHUM'
              xv = 72.96
              yv = -5.525
              zv = 785.3
          endif

          call gspos(v_i_name,1,v_m_name,xv,yv, -zv,1,'ONLY')

C Posi    tion of West wall in MUHL volume NORTH

          if(muid_shield.eq.1)then
              v_m_name = 'MUHL'
              zv = 785.3
              call gspos(v_i_name,1,v_m_name,xv,yv, zv,1,'ONLY')
          endif

C  Pos    ition of DXMH volume for DX Magnet in LHUM volume

          if(muid_shield.eq.0)then
              v_m_name = 'LHUM'
              v_i_name = 'DXMH'
              xv = 0.
              yv = 0.
              zv = -975.5
              call gspos(v_i_name,1,v_m_name,xv,yv, zv,1,'MANY')
          endif

C Crea    te and install DX Magnet components

          if(muid_shield.eq.0) v_m_name = 'DXMH'
          if(muid_shield.eq.1) v_m_name = 'LHUM'
          nmed = 46

C ....    .... create tube

          v_i_name = 'SDXM'
          v_i_shape= 'TUBE'

          npar = 3
          dxmgpar(1) = 63.5 - 0.64
          dxmgpar(2) = 63.5
          dxmgpar(3) = 95.
          call gsvolu(v_i_name,v_i_shape,nmed,dxmgpar,npar,ivolu)
          call uvolattr(v_i_name,4)

C  ...    ..   position tube

          xv = 0.
          yv = -9.
          if(muid_shield.eq.0) zv = 0.
          if(muid_shield.eq.1) zv = -975.5
          call gspos(v_i_name,1,v_m_name,xv,yv, zv,1,'ONLY')

          if(muid_shield.eq.1) then
              v_m_name = 'MUHL'
              zv = 975.5
              call gspos(v_i_name,1,v_m_name,xv,yv, zv,1,'ONLY')
          endif

C.....    .. create endcap EDXM, changed from SPHE to PCON may 2010 HvH

          if(muid_shield.eq.0) v_m_name = 'DXMH'
          if(muid_shield.eq.1) v_m_name = 'LHUM'   ! lhum is in the south

          d1 = 0.25*2.56           ! 1.4" thick
          d2 = d1/cos(6*dtorad)    ! 

          r1 = 2*dxmgpar(2)        ! outer 'radius'
          r2 = 2*dxmgpar(2) - d2   ! inner 'radius'
          delphi = 22.             ! The endcap is split into 2 angular parts
          nzdx = 6
          dxnepar( 1) = 180.-delphi       ! phi start of upper part
          dxnepar( 2) = 180+2*delphi      ! dphi of upper endcap part
          dxnepar( 3) = nzdx

          dxnepar( 4) = r1*cos(30*dtorad)       ! z1
          dxnepar( 6) = r1*sin(30*dtorad)
          dxnepar( 5) = dxnepar(6)-d1/sin(24*dtorad)

          dxnepar( 7) = r2*cos(18*dtorad)       ! z2
          dxnepar( 8) = r2*sin(18*dtorad)
          dxnepar( 9) = dxnepar(8) + d1/sin(24*dtorad)

          dxnepar(10) = r1*cos(18*dtorad)       ! z3
          dxnepar(12) = r1*sin(18*dtorad)
          dxnepar(11) = dxnepar(12) - d1/sin(12*dtorad)

          dxnepar(13) = r2*cos(6*dtorad)        ! z4
          dxnepar(14) = r2*sin(6*dtorad) 
          dxnepar(15) = dxnepar(14) + d1/sin(12*dtorad) 

          dxnepar(16) = r2*cos(6*dtorad)        ! z5
          yv = -9.                              ! this part of the endcap goes
          dxnepar(17) = 0.0                     ! all the way down to r=0.0
          dxnepar(18) = dxnepar(15)

          dxnepar(19) = r1*cos(6*dtorad)        ! z6
          dxnepar(20) = dxnepar(17)
          dxnepar(21) = r1*sin(6*dtorad)

          v_i_name = 'EDXM'
          call gsvolu('EDXM','PCON',nmed,dxnepar,3+3*nzdx,ivolu)

cC  ...    ..   position endcap bottom half EDXM

          xv = 0.      ! at this point, v_m_name = LHUM, the south ~DX volume
          yv = -9.
          if(muid_shield.eq.0) zv = dxmvpar(3)-dxmepar(2)-12.5
          if(muid_shield.eq.1) zv = dxmvpar(3)-dxmepar(2)-12.5 - 975.5
          call gspos('EDXM',1,v_m_name,xv,yv, zv,1,'ONLY')

          if(muid_shield.eq.1) then
            v_m_name = 'MUHL'
            irot = irot+1
            call gsrotm(irot,90.,180.,90.,90.,180.,0.)
            call gspos('EDXM',2,v_m_name,xv,yv, -zv,irot,'ONLY')
          endif

          nzdx = 5                      ! Now the top half of the endcap
          RR = rmin_muhl(1) + abs(yv) +1.3  ! clear the central hole
          dxnepar( 1) = delphi          ! phi start
          dxnepar( 2) = 180-2*delphi    ! dphi
          dxnepar( 3) = nzdx
                                        ! (4) ... (12) are the same, but we
                                        ! go only down to r-RR:
          dxnepar(13) = dxnepar(7)+(dxnepar(8)-RR)*tan(12*dtorad)
          dxnepar(14) = RR
          dxnepar(15) = RR+d2/tan(12*dtorad)

          dxnepar(16) = dxnepar(10)+(dxnepar(12)-RR)*tan(12*dtorad)
          dxnepar(17) = RR
          dxnepar(18) = RR

          v_i_name = 'EDXN'
          call gsvolu('EDXN','PCON',nmed,dxnepar,3+3*nzdx,ivolu)
          call gsatt('EDXN','COLO',2)

cC  ...    ..   position endcap top half EDXN

          xv = 0.      ! at this point, v_m_name = LHUM, the south ~DX volume
          yv = -9.
          if(muid_shield.eq.0) zv = dxmvpar(3)-dxmepar(2)-12.5
          if(muid_shield.eq.1) zv = dxmvpar(3)-dxmepar(2)-12.5 - 975.5
          call gspos('EDXN',1,'LHUM',xv,yv, zv,1,'ONLY')

          if(muid_shield.eq.1) then
            v_m_name = 'MUHL'
            irot = irot+1
            call gsrotm(irot,90.,180.,90.,90.,180.,0.)
            call gspos('EDXN',2,'MUHL',xv,yv, -zv,irot,'ONLY')
          endif



C ....    .... create tube (support)

          if(muid_shield.eq.0) v_m_name = 'DXMH'
          if(muid_shield.eq.1) v_m_name = 'LHUM'
          v_i_name = 'SSDX'
          v_i_shape= 'TUBE'

          npar = 3
          dxmgpar(1) = 63.5
          dxmgpar(2) = 65.
          dxmgpar(3) = 2.
          call gsvolu(v_i_name,v_i_shape,nmed,dxmgpar,npar,ivolu)
          call uvolattr(v_i_name,4)


C  ...    ..   position tube (support)

          xv = 0.
          yv = -9.
          if(muid_shield.eq.0) zv = 90.0
          if(muid_shield.eq.1) zv = 90.0 - 975.5
          call gspos(v_i_name,1,v_m_name,xv,yv, zv,1,'ONLY')

          if(muid_shield.eq.1) then
              v_m_name = 'MUHL'
              call gspos(v_i_name,1,v_m_name,xv,yv, -zv,1,'ONLY')
          endif


C ....    .... create tube (Forward DX volume)

          if(muid_shield.eq.0) v_m_name = 'DXMH'
          if(muid_shield.eq.1) v_m_name = 'LHUM'
          v_i_name = 'DXTF'
          v_i_shape= 'TUBE'

          npar = 3
          dxmgpar(1) = 7.
          dxmgpar(2) = 13.
          dxmgpar(3) = 1.
          call gsvolu(v_i_name,v_i_shape,nmed,dxmgpar,npar,ivolu)
          call uvolattr(v_i_name,7)


C  ...    ..   position tube (Forward DX volume)

          xv = 0.
          yv = 0.
          if(muid_shield.eq.0) zv = 120.0
          if(muid_shield.eq.1) zv = 120.0 - 975.5
          call gspos(v_i_name,1,v_m_name,xv,yv, zv,1,'ONLY')

          if(muid_shield.eq.1) then
              v_m_name = 'MUHL'
              call gspos(v_i_name,1,v_m_name,xv,yv, -zv,1,'ONLY')
          endif

C ....    ....create Forward-side DX volume

          if(muid_shield.eq.0) v_m_name = 'DXMH'
          if(muid_shield.eq.1) v_m_name = 'LHUM'
          v_i_name = 'DXSF'
          v_i_shape= 'TUBE'

          npar = 3
          dxmgpar(1) = 10.5
          dxmgpar(2) = 11.
          dxmgpar(3) = 4.
          call gsvolu(v_i_name,v_i_shape,nmed,dxmgpar,npar,ivolu)
          call uvolattr(v_i_name,7)

C  ...    ..   position tube (Forward-side DX volume)

          xv = 0.
          yv = 0.
          if(muid_shield.eq.0) zv = 115.0
          if(muid_shield.eq.1) zv = 115.0 - 975.5
          call gspos(v_i_name,1,v_m_name,xv,yv, zv,1,'ONLY')

          if(muid_shield.eq.1) then
              v_m_name = 'MUHL'
              call gspos(v_i_name,1,v_m_name,xv,yv, -zv,1,'ONLY')
          endif

C ....    ....create  DX Magnet skin

          if(muid_shield.eq.0) v_m_name = 'DXMH'
          if(muid_shield.eq.1) v_m_name = 'LHUM'
          v_i_name = 'DXSK'
          v_i_shape= 'TUBE'

          npar = 3
          dxmgpar(1) = 34.8
          dxmgpar(2) = 35.
          dxmgpar(3) = 85.
          call gsvolu(v_i_name,v_i_shape,nmed,dxmgpar,npar,ivolu)
          call uvolattr(v_i_name,2)


C  ...    ..   position magnet skin

          xv = 0.
          yv = 0.
          if(muid_shield.eq.0) zv = 0.
          if(muid_shield.eq.1) zv = -975.5
          call gspos(v_i_name,1,v_m_name,xv,yv, zv,1,'ONLY')

          if(muid_shield.eq.1) then
              v_m_name = 'MUHL'
              call gspos(v_i_name,1,v_m_name,xv,yv, -zv,1,'ONLY')
          endif

C ....    ....create  DX Magnet mylar

          if(muid_shield.eq.0) v_m_name = 'DXMH'
          if(muid_shield.eq.1) v_m_name = 'LHUM'
          v_i_name = 'DXMM'
          v_i_shape= 'TUBE'
          nmed = 25
          npar = 3
          dxmgpar(1) = 34.
          dxmgpar(2) = 34.8
          dxmgpar(3) = 85.
          call gsvolu(v_i_name,v_i_shape,nmed,dxmgpar,npar,ivolu)
          call uvolattr(v_i_name,3)


C  ...    ..   position magnet mylar skin

          xv = 0.
          yv = 0.
          if(muid_shield.eq.0) zv = 0.
          if(muid_shield.eq.1) zv = -975.5
          call gspos(v_i_name,1,v_m_name,xv,yv, zv,1,'ONLY')

          if(muid_shield.eq.1) then
              v_m_name = 'MUHL'
              call gspos(v_i_name,1,v_m_name,xv,yv, -zv,1,'ONLY')
          endif


C ....    ....create  DX Magnet yoke Lo

          if(muid_shield.eq.0) v_m_name = 'DXMH'
          if(muid_shield.eq.1) v_m_name = 'LHUM'
          v_i_name = 'DXYK'
          v_i_shape= 'TUBE'
          nmed = 46
          npar = 3
          dxmgpar(1) = 9.
          dxmgpar(2) = 31.5
          dxmgpar(3) = 85.
          call gsvolu(v_i_name,v_i_shape,nmed,dxmgpar,npar,ivolu)
          call uvolattr(v_i_name,3)


C ....    ....create  DX Magnet yoke Hi

          if(muid_shield.eq.0) v_m_name = 'DXMH'
          if(muid_shield.eq.1) v_m_name = 'LHUM'
          v_m_name = 'DXYK'
          v_i_name = 'DXYL'
          v_i_shape= 'TUBE'
          nmed = 41
          npar = 3
          dxmgpar(1) = 9.
          dxmgpar(2) = 29.
          dxmgpar(3) = 82.5
          call gsvolu(v_i_name,v_i_shape,nmed,dxmgpar,npar,ivolu)
          call uvolattr(v_i_name,2)

C  ...    ..   position magnet yoke Hi

          xv = 0.
          yv = 0.
          zv = 0.
          call gspos(v_i_name,1,v_m_name,xv,yv, zv,1,'ONLY')


C  ...    ..   position magnet yoke Lo

          if(muid_shield.eq.0) v_m_name = 'DXMH'
          if(muid_shield.eq.1) v_m_name = 'LHUM'
          v_i_name = 'DXYK'
          xv = 0.
          yv = 0.
          if(muid_shield.eq.0) zv = 0.
          if(muid_shield.eq.1) zv = -975.5
          call gspos(v_i_name,1,v_m_name,xv,yv, zv,1,'ONLY')

          if(muid_shield.eq.1) then
              v_m_name = 'MUHL'
              call gspos(v_i_name,1,v_m_name,xv,yv, -zv,1,'ONLY')
          endif

          write(lout,
     &          '('' muident - Muon hole installed at z= '',f10.3 )')zv

          write(lout,
     &          '('' muident - Muid planes placed at following Z'')')
          write(lout,
     &          '(1x,5(2x,f9.3))')(zmuid_pl(i),i=1,nr_muid_planes - 1)
          write(lout,'('' muident - Muon_Identifier installed'')')


c Now     install a floor underneath the MUID

          if(mu_floor_flg .eq. 1)then

              v_i_name = 'MUFL'
              v_i_shape= 'BOX '
              npar = 3
              nmed = nmed_mufl

              flrpar(1) = abspar(1)
              flrpar(2) = mu_floor_thick
              flrpar(3) = dzhole + 5.0
              call gsvolu(v_i_name,v_i_shape,nmed,flrpar,npar,ivolu)
              call uvolattr(v_i_name,color_floor)

              v_m_name = 'HALL'
              xv = 0.
              yv = mu_floor_height - beam_height - flrpar(2)
              zv = zdont2(1) + flrpar(3)
              call gspos(v_i_name,1,v_m_name,xv,yv,zv,1,'only')
              if(nr_muon_arms .eq. 2)then
                  call gspos(v_i_name,2,v_m_name,xv,yv,-zv,1,'only')
              end if

              write(lout,'('' muident - Muid_floor installed'')')

          end if
cc
          if( cvolu_opt(1,11) .eq. 'FULL')then
              nwpa = 200           ! for now
              nwsa = 200           ! for now

              call gsdet(set_id,det_id,nv,namesv,nbitsv,idtype,
     +                   nwpa,nwsa, iset,idet)

*****************************************************************************
c C.F. Maguire 02/21/97  (first noticed by Anita Trivedi)
c Fix call to GSDETH which had undefined local variable NH
c This produced a ZFATAL error on the Pentium Pro/Solaris system
c Using default NH = 11, with default names, orig, and fact
c MUID programmers can change these as necessary
*****************************************************************************

              call gsdeth(set_id,det_id,11,namesh,nbitsh,
     +                    orig,fact)
          end if
          write(lunplog, nml=mun_par)
      end if

      return


c---------------------------------------------------------------------
 999  write(6,*) 'muident - Read error in mun_par segment of phnx.par.',
     + ' The PHNX.PAR file will be re-read to pinpoint the erroneous',
     + ' line'
      rewind( itf_lun )
      read( itf_lun, nml = mun_par )
      stop 'muident - PISA stop ... PHNX.PAR file error.'
 998  continue
      write(6, nml=mag_par)
      stop 'muident - PISA stop, read error in namelist $mag_par.'
 9992 write(6,'(''MUID planes exceeded maximum allowed'')')
      stop
      end
