      subroutine svx_massold
*-----------------------------------------------------------------
* Mass in the forward direction lifted out of svx.f
* SISP, SJSP, SISQ, SICC, SJCC, SKCC
* August 2016 HvH
*-----------------------------------------------------------------
      implicit none

#include "gclist.inc"
#include "gconst.inc"  
#include "gcflag.inc"
#include "gugeom.inc"
#include "gcvolu.inc"
C
C     ================================================================
c---  VTX Envelope/Cage parameters: Volumes SIEN(outer)/SICG(inner)
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      Real    sili_cg_rmn    /2.2/    ! Inner cage radius, cm
      Real    sili_cg_thck   /0.5/    ! Cage wall thickness, cm
      Real    sili_cg_inthck /0.2/    ! Thickness of the beam pipe ins., cm
      Real    sili_cg_tempc  /0.0/    ! Temperature inside the Cage, deg. C
      Integer sili_cg_npcon  /6/      ! Number of corners for SIEN's PCON
      Real    sili_cg_z(6)    /6*0.0/ ! z-pos. of the Cage corners
      Real    sili_cg_rmx(10)/10*0.0/ ! Outer SIEN radii at the corners, cm
      Real    sili_cg_xdisp  /0.0/    ! x-displacement of SIEN in HALL, cm
      Real    sili_cg_ydisp  /0.0/    ! y-displacement of SIEN in HALL, cm
      Real    sili_cg_zdisp  /0.0/    ! z-displacement of SIEN in HALL, cm
      real    sili_cg_rinner          ! part of fvtx cage definition
      real    sili_cg_swedge_len      !    ''      note thet these are copied from
      real    sili_cg_bwedge_len      !    ''      the endcap namelist.
      real    sili_cg_support_thk     !    ''      
      Integer sili_endcap_config      ! fvtx (1), ifvtx (2) or none (0)
      real    fcg_z1, fcg_z2, fcg_z3, fcg_r1, fcg_r2, fcg_t, fcg_alpha, 
     &        fcg_sina, fcg_cosa, fcg_tana, fcg_z   ! forward vertex cage variables

      namelist /sili_cg_par/ sili_cg_npcon,sili_cg_z
     &     ,sili_cg_rmn,sili_cg_rmx,sili_cg_thck,sili_cg_inthck
     &     ,sili_cg_xdisp,sili_cg_ydisp,sili_cg_zdisp
     &     ,sili_cg_tempc, sili_cg_rinner, sili_cg_swedge_len,
     &     sili_cg_bwedge_len, sili_cg_support_thk,
     &     fcg_z1, fcg_z2, fcg_z3, fcg_r1, fcg_r2, fcg_t, fcg_z,
     &     sili_endcap_config

c---  VTX Barrel parameters
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      Integer sili_br_nlayers /8/                    ! Number of barrel layers

      Real    sili_br_snhalfx(20) /.696,19*1.71555/  ! Si sensor width, cm
      Real    sili_br_snhalfy(20) /0.01,19*0.02/     ! Si sensor thickness, cm
      Real    sili_br_snhalfz(20) /2.836,19*3.2291/  ! Si sensor length, cm
      Real    sili_br_bp_thk      /0.0017/           ! ROC3 bias plane thickness cm
      Real    sili_br_psv_thk     /0.12/             ! ROC3 insulator thickness cm
      Real    sili_br_k3_thk(2)   /0.09,0.33/        ! K3 thickness cm
      Real    sili_br_tb_thk(2)   /0.0381, 0.028/    ! Cooling u-tube thickness for stripixel cm
      Real    sili_br_stv_thk(3)  /0.05, 0.0410, 0.0408/   ! ROC3 stave thickness cm
      Real    sili_br_bus_thk(2)  /0.0178, 0.0054/   ! BUS thickness for stripixel cm
      Real    sili_br_rcc_thk(2)  /0.1484, 0.0027/   ! RCC thickness for stripixel cm
      Real    sili_br_tap_thk(2)  /0.0127, 0.0027/   ! TAPER thickness for stripixel cm
      Real    sili_br_rsn_thk     /0.01/             ! CFC resin thickness cm
      Real    sili_br_x0add(20)   /20*0.01/          ! Passive material thickness
c                                              ! in the ladders added on top
c                                              ! of Si sensor, RadLength X0
      Real    sili_br_snzgap(20)  /20*0./      ! Sensor z-gap (if >=0) or
c                                              ! sensor z-overlap (if <0), cm
      Real    sili_br_tilt(20)    /7.8,7.0,0.0,0.0,16*0./ !  Ladder tilts
      Integer sili_br_nsn(20)     /4,19*5/       ! Number of Si sensors/ladder
      Real    sili_br_r(20)   /2.5,5.,10.,14.,0.3,15*0./ ! Radial positions ...
      Real    sili_br_z(20)      /20*0./ ! Z-pos. of the ladder centers in SICG
      Real    sili_br_dphi(20)   /29.40,29.40  ! Azim. spacing of ladders, deg.
     *                           ,22.87,18.71
     *                           ,16*0./
      Real    sili_br_stdphi(5,4) /10*0.
     *                            ,28.20, 18.04, 17.36, 16.06, 30.84
     *                            ,24.86, 12.40, 11.60, 12.06, 22.96/
      Integer sili_br_nsec(20)   /20*2/  ! Number of cont. phi-sections/layer
      Real    sili_br_phic(5,20) /0.,180.,3*0. ! Sect. center azimuth pos.
     *                           ,0.,180.,3*0.
     *                           ,0.,180.,3*0.
     *                           ,0.,180.,3*0.
     *                           ,80*0./
      Integer sili_br_nlad(5,20) /5*5    ! Number of ladders/phi-section
     *                           ,5*5
     *                           ,5*7
     *                           ,5*9
     *                           ,80*1/

      real sili_br_misalignment(8) /8*0./ ! misalignment in X-Y plane (cm), east arm first
      real sili_br_shiftstag

      logical lbarrel /.true./           ! allows the barrel to be switched on/off
      integer sili_wheel_yesno /1/       ! build readout wheels or not

c     ======================================================================
c     The parameters are for support rings at the end of the ladders
      Integer sili_br_nspring     /2/          ! Number of rings
      Real    sili_br_spz(20) /-17.,17.,18*0./ ! Z-positions, cm
      Real    sili_br_sprin(20)  /20*2.4/      ! Inner radii, cm
      Real    sili_br_sprout(20) /20*10./      ! Outer radii, cm
      Real    sili_br_spthck(20) /20*0.25/     ! Half-hickness, cm
      Real    sili_br_manz(20) /-17.,17.,18*0./ ! Z-positions cooling manifold
      Real    sili_br_manr(20)  /20*2.4/       ! mean radius cooling manifold
      Real    sili_br_mandia(20) /20*10./      ! diameter cooling manifold
      real    pixel_hdi_thk/.01/
      real    pixel_hdi_phimin/110./
      real    pixel_hdi_phimax/250./ 
      real siwh_ppcb_half(3)  ! pixel pcb half-thickness = 1/32" 
      real siwh_spcb_half(3)  ! strip pcb half-thickness = 1/32" 
      real siwh_fpcb_thick    ! fvtx pcb half-thickness = 1/32" 
      real siwh_pcb_z(10)     ! pcb center positions     
      real siwh_pcb_rmax      ! pcb max radius        
      real siwh_pcb_connz     ! pcb connector half-z
      real siwh_pcb_suppz(10) ! pcb support/cooling half-z
      real siwh_roha1_thick   ! inner rohacell cover half-thickness
      real sili_br_ct_radius  ! cooling tube radius
      real sili_endcap_z(8)   ! to place barrel cables, endcap roha cage
      character*40 vtx_alignment_file1, vtx_alignment_file2

c     ======================================================================
      namelist /sili_br_par/ sili_br_nlayers,sili_br_r,sili_br_z
     $     ,sili_br_nsn,sili_br_snhalfx,sili_br_snhalfy,sili_br_snhalfz
     $     ,sili_br_bp_thk,sili_br_psv_thk,sili_br_k3_thk,sili_br_tb_thk
     $     ,sili_br_stv_thk,sili_br_bus_thk,sili_br_rcc_thk
     $     ,sili_br_tap_thk,sili_br_rsn_thk,sili_br_x0add,sili_br_snzgap
     $     ,sili_br_nsec,sili_br_nlad,sili_br_phic,sili_br_dphi
     $     ,sili_br_stdphi,sili_br_tilt
     $     ,sili_br_nspring,sili_br_spz,sili_br_sprin
     $     ,sili_br_sprout,sili_br_spthck
     &     ,sili_br_manz,sili_br_manr ,sili_br_mandia
     $     ,sili_br_misalignment,
     &     sili_wheel_yesno,siwh_ppcb_half,siwh_spcb_half,
     &     siwh_fpcb_thick, siwh_pcb_z, siwh_pcb_rmax,
     &     siwh_pcb_connz, siwh_pcb_suppz, siwh_roha1_thick,
     &     sili_br_ct_radius, sili_endcap_z,
     &     vtx_shiftx, vtx_shifty, vtx_shiftz,
     &     vtx_alignment_file1, vtx_alignment_file2

      real vtx_shiftx, vtx_shifty, vtx_shiftz
C     ================================================================
C---  Local definitions
C     ================================================================
      Integer sili_med_passive /26/     ! Ladder passive nmed    (nmat=09,Al)
      Integer sili_med_gfrp    /122/    ! GFRP for the fake support
      integer sili_med_coolant /124/    ! C5F12 liquid coolant
      integer sili_med_honeycomb /125/  ! 1/4" honeycomb, .5mm c-c skin, Al core     

C     ================================================================
c---  Local work variables (counters, etc)
C     ================================================================
      character*4  v_m_name,v_i_name

      integer 
     &     nr, npar, nmed, ivolu, ivol1,
     &   icopy, irottop,  irotbot, irot1, irot2, irot3, i

      real dim_sili(30), par(21), posx, posy, posz

      integer itf_lun       !   geometry description logical unit   
      common /interface/itf_lun

      integer skip_sector, skip_ladder
      integer sec_flag, lad_flag

C======================================================================!
C*************************** EXECUTABLE CODE***************************!
C======================================================================!

      rewind( unit = itf_lun )
      read( itf_lun, nml = sili_cg_par )
      rewind( unit = itf_lun )
      read( itf_lun, nml = sili_br_par )

c---  Set parameters to old values ----------------------------------
      sili_br_sprin(1) = 3.1        ! from old phnx.par 30 april 2015
      sili_br_sprin(2) = 5.6        ! for SISP support rings
      sili_br_sprin(3) = 10.9
      sili_br_sprin(4) = 15.3
      sili_br_sprin(5) = sili_br_sprin(1)
      sili_br_sprin(6) = sili_br_sprin(2)
      sili_br_sprin(7) = sili_br_sprin(3)
      sili_br_sprin(8) = sili_br_sprin(4)

      sili_br_sprout(1) = 4.0
      sili_br_sprout(2) = 6.5
      sili_br_sprout(3) = 11.9
      sili_br_sprout(4) = 16.3
      sili_br_sprout(5) = sili_br_sprout(1)
      sili_br_sprout(6) = sili_br_sprout(2)
      sili_br_sprout(7) = sili_br_sprout(3)
      sili_br_sprout(8) = sili_br_sprout(4)

      do i=1,8
        sili_br_spthck(i) = 0.62
      enddo

      sili_br_spz(3) = -16.8
      sili_br_spz(4) = -20.0
      sili_br_spz(7) = -sili_br_spz(3)
      sili_br_spz(8) = -sili_br_spz(4)


      sili_br_manr(1) = 4.4      ! for SJSP, cooling arc
      sili_br_manr(2) = 6.8
      sili_br_manr(3) = 12.3
      sili_br_manr(4) = 16.7
      sili_br_manr(5) = sili_br_manr(1)
      sili_br_manr(6) = sili_br_manr(2)
      sili_br_manr(7) = sili_br_manr(3)
      sili_br_manr(8) = sili_br_manr(4)

      do i=1,8
        sili_br_mandia(i) = 0.35
      enddo

      sili_br_manz(1) = -13.4
      sili_br_manz(2) = -13.4
      sili_br_manz(3) = -17.6
      sili_br_manz(4) = -20.8
      sili_br_manz(5) = -sili_br_manz(1)
      sili_br_manz(6) = -sili_br_manz(2)
      sili_br_manz(7) = -sili_br_manz(3)
      sili_br_manz(8) = -sili_br_manz(4)

      sili_br_r(1) = 2.63      ! for SICC, SJCC, SKCC (VTX cable sheet)
      sili_br_r(2) = 5.13
      pixel_hdi_phimin = 100
      pixel_hdi_phimax = 260
c---  ... end set parameters to old values ----------------------------------


C...  Create Support rings. // Should be changed.
	nmed   = sili_med_gfrp           ! G-10 describing the clip blocks
        v_m_name = 'SICG'
        v_i_name = 'SISP'                ! SISP are support rings
        call gsvolu(v_i_name,'TUBE',nmed,dim_sili,0,ivolu)
        call gsatt(v_i_name,'SEEN',1)    ! 
        nmed   = sili_med_honeycomb      ! SISQ are posts that connect rings to outside frame
        call gsvolu('SISQ','TRD1',nmed,dim_sili,0,ivolu)
        call gsatt(v_i_name,'SEEN',1)

        nmed = sili_med_coolant
        call gsvolu('SJSP','TUBE',nmed,dim_sili,0,ivolu)   ! cooling tube circle
        call gsatt(v_i_name,'SEEN',1)
        call gsvolu('SJSQ','TUBE',nmed,dim_sili,0,ivolu)   ! cooling tube feeds
        call gsatt(v_i_name,'SEEN',1)

        npar = 3
        irot = irot+1                    ! for top SISQ posts
        call gsrotm(irot,90.,0.,180.,0.,90.,90.)
        irottop = irot
        irot = irot+1                    ! for bottom SISQ posts
        call gsrotm(irot,90.,0.,0.,0.,90.,-90.)
        irotbot = irot

        Do nr = 1, sili_br_nspring       ! loop over 8 rings 
          dim_sili(1) = sili_br_sprin(nr)        ! support rings
          dim_sili(2) = sili_br_sprout(nr)
          dim_sili(3) = sili_br_spthck(nr)
          if (lbarrel) then
            call gsposp(v_i_name,nr,v_m_name,0.,0.,
     &         sili_br_spz(nr),
     &         irotnull,'ONLY',dim_sili,npar)    !
          endif

          dim_sili(1) = 0.14* sili_br_sprout(nr) ! ring support posts/wedges
          dim_sili(2) = 2.56                     ! =  sili_cg_rmx(1)*tan(8 degrees)
          dim_sili(3) = sili_br_spthck(nr)
          dim_sili(4) = (sili_cg_rmx(3) -0.5 -sili_br_sprout(nr))/2-0.12
          if (nr.eq.1.or.nr.eq.5) then           ! special for innermost rings
            dim_sili(2) = 0.14*sili_br_sprin(2)  ! shared support 
            dim_sili(4) = (sili_br_sprin(2) - sili_br_sprout(1))/2-0.05
          endif 
          posy = sili_br_sprout(nr) + dim_sili(4)
          if (lbarrel) then
            call gsposp('SISQ',nr,v_m_name,0.,posy,
     &         sili_br_spz(nr),
     &         irottop,'ONLY',dim_sili,4)    !
            call gsposp('SISQ',nr+8,v_m_name,0.,-posy,
     &         sili_br_spz(nr),
     &         irotbot,'ONLY',dim_sili,4)    !
          endif

          dim_sili(1) = sili_br_manr(nr) - 0.443*sili_br_mandia(nr)    ! cooling manifolds - rings
          dim_sili(2) = sili_br_manr(nr) + 0.443*sili_br_mandia(nr)
          dim_sili(3) = 0.443*sili_br_mandia(nr)
          if (lbarrel) then
            call gsposp('SJSP',nr,v_m_name,0.,0.,
     &         sili_br_manz(nr),
     &         irotnull,'ONLY',dim_sili,npar)    !
          endif

          dim_sili(1) = 0.0                               ! cooling manifolds - supply pipes
          dim_sili(2) = 0.5*sili_br_mandia(nr)
          dim_sili(3) = (sili_cg_rmx(3) - sili_br_manr(nr) -
     &                  sili_cg_thck - 0.443*sili_br_mandia(nr))/2.0-0.1
          posx = 1.0
          posy = sili_br_manr(nr) +0.443*sili_br_mandia(nr)+ dim_sili(3)
          if (lbarrel.and.(nr.ne.1).and.(nr.ne.5)) then
            call gsposp('SJSQ',nr   ,v_m_name, posx, posy,
     &         sili_br_manz(nr),
     &         irottop,'ONLY',dim_sili,4)    !
            call gsposp('SJSQ',nr+8 ,v_m_name, posx,-posy,
     &         sili_br_manz(nr),
     &         irotbot,'ONLY',dim_sili,4)    !
            call gsposp('SJSQ',nr+16,v_m_name,-posx, posy,
     &         sili_br_manz(nr),
     &         irottop,'ONLY',dim_sili,4)    !
            call gsposp('SJSQ',nr+24,v_m_name,-posx,-posy,
     &         sili_br_manz(nr),
     &         irotbot,'ONLY',dim_sili,4)    !
          endif

        Enddo     ! loop over 8 rings


c
c       barrel VTX kapton cables pixel layers - a tube section
c
        irot=irot+1
        irot1 = irot
        CALL GSROTM(irot1,90.,  0.,90.,270.,180.,0.) ! rotate about x
        irot=irot+1
        irot2 = irot
        CALL GSROTM(irot2,90.,180.,90., 90.,180.,0.) ! rotate about y
        irot=irot+1
        irot3 = irot
        CALL GSROTM(irot3,90.,180.,90.,270.,  0.,0.) ! rotate about z       
        par(1) = sili_br_r(1)
        par(2) = sili_br_r(2)
        par(3) = pixel_hdi_thk
        par(4) = pixel_hdi_phimin
        par(5) = pixel_hdi_phimax
        nmed = sili_med_passive
        Call GSVOLU('SICC','TUBS',nmed,PAR,5,IVOL1)
        call GSPOS('SICC',1,'SICG',0.,0.,sili_br_manz(1)-.5,
     c   irotnull,'ONLY')
        call GSPOS('SICC',2,'SICG',0.,0.,sili_br_manz(5)+.5,
     c   irotnull,'ONLY')
        call GSPOS('SICC',3,'SICG',0.,0.,sili_br_manz(1)-.5,
     c   irot2,'ONLY')
        call GSPOS('SICC',4,'SICG',0.,0.,sili_br_manz(5)+.5,
     c   irot2,'ONLY')     
        Call GSATT('SICC','SEEN', 1)
        Call GSATT('SICC','COLO', 4)
c
c       barrel VTX kapton cables strip layers - a cone section:
c
        par(1) = (sili_endcap_z(6)-2.8-sili_br_manz(5))/2.
        par(2) = sili_br_r(2) + pixel_hdi_thk
        par(3) = sili_br_r(2) + pixel_hdi_thk*2. 
        par(4) = sili_cg_rmx(4) - 1.2
        par(5) = par(4) + pixel_hdi_thk*4.
        par(6) = pixel_hdi_phimin
       par(7) = pixel_hdi_phimax
       Call GSVOLU('SJCC','CONS',nmed,PAR,7,IVOL1)
        call GSPOS('SJCC',1,'SICG',0.,0.,sili_br_manz(5)+par(1)+1.0,
     c   irotnull,'ONLY')
        call GSPOS('SJCC',2,'SICG',0.,0.,-sili_br_manz(5)-par(1)-1.0,
     c   irot1,'ONLY')
        call GSPOS('SJCC',3,'SICG',0.,0.,sili_br_manz(5)+par(1)+1.0,
     c   irot3,'ONLY')
        call GSPOS('SJCC',4,'SICG',0.,0.,-sili_br_manz(5)-par(1)-1.0,
     c   irot2,'ONLY')     
        Call GSATT('SJCC','SEEN',1)
        Call GSATT('SJCC','COLO', 4)            
c
c       VTX Barrel kapton layers after cone - a tube section:
c
        par(1) = sili_cg_rmx(4) - 1.0
        par(2) = par(1) + pixel_hdi_thk*4.
        par(3) = (sili_endcap_z(8)-sili_endcap_z(6))/2.
        par(4) = pixel_hdi_phimin
        par(5) = pixel_hdi_phimax
        nmed = sili_med_passive
        Call GSVOLU('SKCC','TUBS',nmed,PAR,5,IVOL1)
        call GSPOS('SKCC',1,'SICG',0.,0.,sili_endcap_z(8)-par(3),
     c   irotnull,'ONLY')
        call GSPOS('SKCC',2,'SICG',0.,0.,-sili_endcap_z(8) +par(3),
     c   irotnull,'ONLY')
        call GSPOS('SKCC',3,'SICG',0.,0.,sili_endcap_z(8) -par(3),
     c   irot3,'ONLY')
        call GSPOS('SKCC',4,'SICG',0.,0.,-sili_endcap_z(8) +par(3),
     c   irot3,'ONLY')
        call gsatt('SKCC','SEEN',1)
        Call GSATT('SKCC','COLO', 4)                       



        return 
        end
