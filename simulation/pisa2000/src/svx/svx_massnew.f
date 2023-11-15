      subroutine svx_massnew

      implicit none

#include "guphnx.inc"    ! contains RHICRUN
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

c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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
      real vtx_shiftx, vtx_shifty, vtx_shiftz
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

C     ================================================================
      character*4 siliEnvelope/'SIEN'/  ! Envelope (outer cage surface)
      character*4 siliCage    /'SICG'/  ! Cage inner surface
      character*4 siliSupport /'SISP'/  ! Barrel support rings
      character*4 siliClips   /'SISO'/  ! Support clips

      Integer irot_cage        ! Cage (SIEN) rotation matrix #

C     ================================================================
C---  Local definitions of materials/media
C     ================================================================
      Integer sili_med_gfrp    /122/    ! GFRP for the fake support
      integer sili_med_coolant /124/    ! C5F12 liquid coolant
      integer sili_med_honeycomb /125/  ! 1/4" honeycomb, .5mm c-c skin, Al core
      integer sili_med_resin   /127/    ! CFC resin
      integer sili_med_kapton  /129/    ! kapton
      integer sili_med_k3      /130/    ! Allcomp K3
      integer sili_med_peek    /60/     ! PEEK

C     ================================================================
c---  Local work variables (counters, etc)
C     ================================================================
      character*4  v_m_name,v_i_name

      integer 
     & iLayer, 
cx iPoint, icnt, 
     & nr,  npar, nmed, ivolu,  nladd, iladd, isec, nsensors, ivol1, ierr, 
     &   icopy, irottop,  irotbot, irot1, irot2, irot3, i,
     &   njtube, nktube, idouble, isign, imanp, imant, 
     &   nsjsq, nsisq

      real dim_sili(30), delphi, par(21), posx, posy, posz, rholder(5), 
     & phi, phispread, theta, phideg, 
     & stub_len, stub_r(4), dz, ss_fraction, ss_vol_stub, ss_vol_stubs,
     & ss_vol_arc, ss_vol_arcs, cables_thk,zg10, yg10, angg10, dg10,tg10

      data rholder/2.3, 4.0, 9.0, 14.7, 19.7/ 
      data stub_r / 4.2, 6.7, 12.9, 18.4 /  ! was 13.6

      integer itf_lun       !   geometry description logical unit   
      common /interface/itf_lun

      integer skip_sector, skip_ladder
      integer sec_flag, lad_flag

C======================================================================!
C*************************** EXECUTABLE CODE***************************!
C======================================================================!
      rewind( unit = itf_lun )
      read  ( itf_lun, nml = sili_br_par)
      rewind(itf_lun)
      read  ( itf_lun, nml = sili_cg_par)

C...   Create SISP, the barrel support half-rings.
        nmed   = sili_med_k3             ! changed material 6 Apr 2016c
cxx	nmed   = sili_med_honeycomb      !
        v_m_name = 'SICG'
        v_i_name = 'SISP'                ! SISP are support rings
        call gsvolu(v_i_name,'TUBS',nmed,dim_sili,0,ivolu)
        call gsatt(v_i_name,'SEEN',1)    ! 

C...    Create SISQ, the posts that connect rings to outside frame
        nmed   = sili_med_honeycomb      !
        call gsvolu('SISQ','TRD1',nmed,dim_sili,0,ivolu)
        call gsatt(v_i_name,'SEEN',1)

C...   Create plastic clips on the Support rings. Added apr 2016. Logbook pg 122, 6 Apr 2016.
	nmed   = sili_med_peek           ! PEEK, describing the clip blocks
        v_m_name = 'SICG'
        v_i_name = siliClips             ! SISO are support ring clips
        call gsvolu(v_i_name,'TUBS',nmed,dim_sili,0,ivolu)
        call gsatt(v_i_name,'SEEN',1)    ! 
        call gsatt(v_i_name,'COLO',2)    ! 

        nmed = sili_med_coolant
        call gsvolu('SJSP','TUBS',nmed,dim_sili,0,ivolu)   ! cooling tube partial circle, square cross section
        call gsatt(v_i_name,'SEEN',1)
        call gsatt(v_i_name,'COLO',2)

        call gsvolu('SJSQ','TUBE',nmed,dim_sili,0,ivolu)   ! cooling tube feeds, straight tubes
        call gsatt(v_i_name,'SEEN',1)

        stub_len = 1.5
        dim_sili(1) = 0
        dim_sili(2) =  0.5*sili_br_mandia(2)
        dim_sili(3) = stub_len
        call gsvolu('SJSR','TUBE',nmed,dim_sili,3,ivolu)   ! short tube+stainless stubs to ladders
        call gsatt('SJSR','SEEN',1)
        call gsatt('SJSR','COLO',2)
        call gsvolu('SKSR','TUBE',nmed,dim_sili,3,ivolu)   ! short tube stubs to ladders, no stainless
        call gsatt('SKSR','SEEN',1)
        call gsatt('SKSR','COLO',2)

        nmed = 2000                   ! nmed=2000 (stainless) seen in newabsorber.f
        dim_sili(2) =  0.5*sili_br_mandia(2)*0.89          ! Stainless Steel tube wall
        dim_sili(1) = dim_sili(2) - 0.107                   ! 1mm SS wall thickness adjusted to make
        dim_sili(3) = 0.28 * stub_len                       ! the stub weight come out ~143g total
        call gsvolu('SJSS','TUBE',nmed,dim_sili,3,ivolu)   ! SS tee part
        call gsatt('SJSS','SEEN',1)
        call gsatt('SJSS','COLO',4)
        call gspos('SJSS',1,'SJSR',0.,0.,-stub_len+dim_sili(3),
     &              irotnull,'ONLY')
        ss_vol_stub = 2.*3.14*dim_sili(3)*(dim_sili(2)**2-dim_sili(1)**2)
        write (6,*) 'ss_vol_stub= ',ss_vol_stub
                                                           ! location  matters: nmed set to 2000
        call gsvolu('SJST','TUBS',nmed,dim_sili,0,ivolu)   ! cooling tube Stainless steel content
        call gsatt ('SJST','SEEN',1)
        call gsatt ('SJST','COLO',4)

        npar = 3
        irot = irot+1                    ! for top SISQ posts
        call gsrotm(irot,90.,0.,180.,0.,90.,90.)
        irottop = irot
        irot = irot+1                    ! for bottom SISQ posts
        call gsrotm(irot,90.,0.,0.,0.,90.,-90.)
        irotbot = irot

*       Barrel support structures and cooling lines -----------------------------------------
        njtube = 0                               ! number of hose+stainless stubs
        nktube = 0                               ! number of hose stubs
        imanp = 0                                 ! number of cooling manifold arcs
        imant = 0                                 ! number of ss parts of the arcs

c       4  3  2  1    5  6  7  8    nr
c       |                      |
c    W  |  |                |  |
c       |  |  |  |    |  |  |  |
c       |  |  |  |    |  |  |  |
c                   
c       |  |  |  |    |  |  |  |
c    E  |  |  |  |    |  |  |  |
c       |  |                |  |
c       |                      |

        Do nr = 1, sili_br_nspring               ! loop over 8 rings 
          dim_sili(1) = sili_br_sprin(nr)        ! support rings
          dim_sili(2) = sili_br_sprout(nr)
          dim_sili(3) = sili_br_spthck(nr)
          if (lbarrel) then                     
            if (rhicrun.ne.13) then
              dim_sili(4) = 90.                  ! phimin for East halves
              dim_sili(5) = 270.                 ! phimax
              call gsposp('SISP',nr,'SICG',0.,0.,
     &           sili_br_spz(nr),
     &           irotnull,'ONLY',dim_sili,5)    !
            endif
            dim_sili(4) = 270.                  ! phimin for West halves
            dim_sili(5) = 90.                   ! phimax
            call gsposp('SISP',sili_br_nspring+nr,'SICG',0.,0.,
     &         sili_br_spz(nr),
     &         irotnull,'ONLY',dim_sili,5)    !

            if (abs(sili_br_spz(nr)).lt.15.0) then ! barrel 1,2 only, plastic clips
              dim_sili(1) = dim_sili(1) - 1.19    ! fixed dr, scaled with vol and density
              dim_sili(2) = dim_sili(1) + 1.19    ! logbook 9 April 2016 pg. 124
              dim_sili(3) = 0.59                 ! such that 1.19 x 0.59 is 70.4 mm^2
              dim_sili(4) = 110.    ! phimin
              dim_sili(5) = 250.    ! phimax
              if (rhicrun.ne.13) then     ! remove east vtx in '13
                call gsposp('SISO',nr,'SICG',0.,0.,
     &                      sili_br_spz(nr) -
     &      2*(abs(sili_br_spz(nr))/sili_br_spz(nr))*sili_br_spthck(nr),
     &          irotnull,'ONLY',dim_sili,5)    !
              endif
              dim_sili(4) = 290.    ! phimin
              dim_sili(5) = 070.    ! phimax
              call gsposp('SISO',nr+8,'SICG',0.,0.,
     &                     sili_br_spz(nr) -
     &      2*(abs(sili_br_spz(nr))/sili_br_spz(nr))*sili_br_spthck(nr),
     &          irotnull,'ONLY',dim_sili,5)    !
            endif
          endif

          dim_sili(1) = 0.14* sili_br_sprout(nr) ! ring support posts/wedges
          dim_sili(2) = 2.56                     ! =  sili_cg_rmx(1)*tan(8 degrees)
          dim_sili(3) = sili_br_spthck(nr)       ! 1/4" thick
          dim_sili(4) = (sili_cg_rmx(3) -0.5 -sili_br_sprout(nr))/2-0.12
          if (nr.eq.1.or.nr.eq.5) then           ! special for innermost rings
            dim_sili(2) = 0.14*sili_br_sprin(2)  ! shared support 
            dim_sili(4) = (sili_br_sprin(2) - sili_br_sprout(1))/2-0.05
          endif 
          posy = sili_br_sprout(nr) + dim_sili(4)
          if (lbarrel .and. mod(nr,4).ne.0) then ! support for barrel 4 no longer needed.
            nsisq = nsisq+1
            call gsposp('SISQ',nsisq,'SICG',0.,posy,
     &         sili_br_spz(nr),
     &         irottop,'ONLY',dim_sili,4)    !
            nsisq = nsisq+1
            call gsposp('SISQ',nsisq,'SICG',0.,-posy,
     &         sili_br_spz(nr),
     &         irotbot,'ONLY',dim_sili,4)    !
          endif

*         Start cooling manifold arcs --------------------------------------!---------------------------
          ilayer = mod(nr-1,4)+1                  ! 12345678 -> 12341234
          isign = ((nr-1)/4 )*2 -1                ! 12341234 -> -1-1-1-1 1 1 1 1
          dz =  0.5*0.886*sili_br_mandia(nr)                                ! Volume is a TUBS
          ss_fraction = 0.63                                                ! vol fraction of stainless in arcs
          dim_sili(1) = stub_r(ilayer) +0.5*0.886*sili_br_mandia(nr)+1.1    ! cooling manifolds - partial rings
          dim_sili(2) = stub_r(ilayer) +1.5*0.886*sili_br_mandia(nr)+1.1    ! placed at stub ends
          dim_sili(3) = dz                                                  ! Vol is a TUBS:
          dim_sili(4) = pixel_hdi_phimin - 180.                             ! rmin rmax dz phi1 phi2
          dim_sili(5) = -dim_sili(4)                                        ! 0.886 converts from round to square area.
          posz = sili_br_spz(nr) + isign*(sili_br_spthck(nr) + 
     &    sqrt(2.)*stub_len )
                                                                 ! nr=3 is barrel3 S, nr=8 is barrel4 N
          if (lbarrel.and.nr.ne.3.and.nr.ne.8) then              ! do 1,2,4, 5,6,7
            if (nr.eq.4.or.nr.eq.7) then                         ! nr=4 is B4 S, nr=7 is B3 N
              if (rhicrun.ge.14) then                            ! run select
                dim_sili(3) = dz*(1-ss_fraction)                 ! Vol is a TUBS:
                posz = posz -dz*ss_fraction                      ! here we split the volume 
                if (.not.(rhicrun.eq.16.and.nr.eq.7)) then       ! in run16, layer3 W (nr=7) was not in
                  imanp = imanp+1                                ! between plastic and Stainless
                  call gsposp('SJSP',imanp,v_m_name,0.,0.,posz,
     &               irotnull,'ONLY',dim_sili,5)
                  dim_sili(3) = dz*ss_fraction
                  posz = posz + dz
                  imant = imant+1
                  call gsposp('SJST',imant,v_m_name,0.,0.,posz,
     &               irotnull,'ONLY',dim_sili,5)    !
                endif
                ss_vol_arc = 3.14*dim_sili(3)*                  ! calculate the volume
     &                       (dim_sili(2)**2-dim_sili(1)**2)*
     &                       (dim_sili(5)-dim_sili(4))/360.
                ss_vol_arcs = ss_vol_arcs + abs(ss_vol_arc)
              else                                              ! run select <14, no Stainless
                dim_sili(3) = dz                                ! Vol is a TUBS:
                imanp = imanp+1
                call gsposp('SJSP',imanp,v_m_name,0.,0.,posz,
     &             irotnull,'ONLY',dim_sili,5)
              endif                                             ! end run select
            else                                                ! else do nr=1,2,5,6 (is B1 and B2 N&S)
              if (rhicrun.ge.15) then                           ! run select
                dim_sili(3) = dz*(1-ss_fraction)                ! Vol is a TUBS:
                posz = posz -dz*ss_fraction
                imanp = imanp+1
                call gsposp('SJSP',imanp,v_m_name,0.,0.,posz,
     &             irotnull,'ONLY',dim_sili,5)
                dim_sili(3) = dz*ss_fraction
                posz = posz + dz
                imant = imant+1
                call gsposp('SJST',imant,v_m_name,0.,0.,posz,
     &               irotnull,'ONLY',dim_sili,5)    !
                ss_vol_arc = 3.14*dim_sili(3)*
     &                       (dim_sili(2)**2-dim_sili(1)**2)*
     &                       (dim_sili(5)-dim_sili(4))/360.
                ss_vol_arcs = ss_vol_arcs + abs(ss_vol_arc)
              else                                              ! run select <=15
                dim_sili(3) = dz                                ! Vol is a TUBS:
                imanp = imanp+1
                call gsposp('SJSP',imanp,v_m_name,0.,0.,posz,
     &             irotnull,'ONLY',dim_sili,5)
              endif                                             ! end run select
            endif                                               ! end special for barrel 3,4
          elseif (rhicrun.ne.13) then                           ! nr=3,8 is B3 S and B4 N
            dim_sili(4) = pixel_hdi_phimin
            dim_sili(5) = pixel_hdi_phimax
            if (rhicrun.ge.14) then                             ! run select
              dim_sili(3) = dz*(1-ss_fraction)                  ! Vol is a TUBS:
              posz = posz - dz*ss_fraction
              imanp = imanp+1
              call gsposp('SJSP',imanp,v_m_name,0.,0.,posz,
     &           irotnull,'ONLY',dim_sili,5)    !
              posz = posz + dz
              dim_sili(3) = dz*ss_fraction
              imant = imant+1
              call gsposp('SJST',imant,v_m_name,0.,0.,posz,
     &           irotnull,'ONLY',dim_sili,5)    !
              ss_vol_arc = 3.14*dim_sili(3)*
     &                     (dim_sili(2)**2-dim_sili(1)**2)*
     &                     (dim_sili(5)-dim_sili(4))/360.
              ss_vol_arcs = ss_vol_arcs + abs(ss_vol_arc)
            else                                                ! run 12,(13)
              dim_sili(3) = dz                                  ! Vol is a TUBS:
              imanp = imanp+1
              call gsposp('SJSP',imanp,v_m_name,0.,0.,posz,
     &           irotnull,'ONLY',dim_sili,5)    !
            endif                                               ! end run select
          endif
          if (lbarrel.and.                                      ! barrels 1 and 2
     &      (nr.eq.1.or.nr.eq.2.or.nr.eq.5.or.nr.eq.6) ) then
            dim_sili(4) = pixel_hdi_phimin
            dim_sili(5) = pixel_hdi_phimax
            if (rhicrun.ge.15) then                             ! run select
              dim_sili(3) = dz*(1-ss_fraction)                  ! Vol is a TUBS:
              posz = posz - dz*ss_fraction
              imanp = imanp+1
              call gsposp('SJSP',imanp,v_m_name,0.,0.,posz,
     &           irotnull,'ONLY',dim_sili,5)    !
              posz = posz + dz
              dim_sili(3) = dz*ss_fraction
              imant = imant+1
              call gsposp('SJST',imant,v_m_name,0.,0.,posz,
     &           irotnull,'ONLY',dim_sili,5)    !
              ss_vol_arc = 3.14*dim_sili(3)*
     &                     (dim_sili(2)**2-dim_sili(1)**2)*
     &                     (dim_sili(5)-dim_sili(4))/360.
            elseif (rhicrun.ne.13) then                         ! run 12,(13)
              dim_sili(3) = dz                                  ! Vol is a TUBS:
              imanp = imanp+1
              call gsposp('SJSP',imanp,v_m_name,0.,0.,posz,
     &           irotnull,'ONLY',dim_sili,5)    !
            endif                                               ! end run select
          endif                                                 ! barrel 1,2
*         end cooling manifold arcs ----------------------------!

          dim_sili(1) = 0.0                               ! cooling manifolds - supply pipes
          dim_sili(2) = 0.5*sili_br_mandia(nr)
          dim_sili(3) = (sili_cg_rmx(3) - sili_br_manr(nr) -
     &                  sili_cg_thck - 0.443*sili_br_mandia(nr))/2.0-0.1
          posx = 1.0
          posy = sili_br_manr(nr) +0.443*sili_br_mandia(nr)+ dim_sili(3)
          if (lbarrel.and.(nr.ne.1).and.(nr.ne.5)) then
            nsjsq = nsjsq+1 
            call gsposp('SJSQ',nsjsq,v_m_name, posx, posy,
     &         sili_br_manz(nr),
     &         irottop,'ONLY',dim_sili,3)    !
            nsjsq = nsjsq+1 
            call gsposp('SJSQ',nsjsq,v_m_name, posx,-posy,
     &         sili_br_manz(nr),
     &         irotbot,'ONLY',dim_sili,3)    !
            if (rhicrun.ne.13) then          ! no east vtx in '13
              nsjsq = nsjsq+1 
              call gsposp('SJSQ',nsjsq,v_m_name,-posx, posy,
     &           sili_br_manz(nr),
     &           irottop,'ONLY',dim_sili,3)    !
              nsjsq = nsjsq+1 
              call gsposp('SJSQ',nsjsq,v_m_name,-posx,-posy,
     &           sili_br_manz(nr),
     &           irotbot,'ONLY',dim_sili,3)    !
            endif
          endif

*         Make the little hose stubs at the end of the ladders ------------------------
          idouble = (ilayer+1)/2                  ! 1234 -> 1122 (layers1,2: one tube, 3,4 a pair
          nladd   = sili_br_nlad(1,ilayer)        ! ladders per sector
          phispread = (pixel_hdi_phimax-pixel_hdi_phimin)*pi/180
          delphi = phispread/(nladd*idouble)
          theta = -45.
c          if (nr.gt.4) theta = -theta
           if (nr.gt.4) theta = theta+270.
          do i=1,nladd*idouble                    ! xxx
            phi = (pi-phispread)/2 + (i-0.5)*delphi
            posx = sin(phi) * stub_r(ilayer) 
            posy = cos(phi) * stub_r(ilayer) 
            if (nr.ne.3 .and. nr.ne.8) then        ! West half
              phideg = -phi*180/pi +90.            ! some shifting and flipping
              irot = irot+1
              call gsrotm(irot,90.+theta,phideg,
     &                    90.,90.+phideg,
     &                    theta,phideg)
              posz = sili_br_spz(nr) + isign*(sili_br_spthck(nr) + 
     &        sqrt(0.5)*(0.5*sili_br_mandia(2)+stub_len) )
              if (nr.eq.3.or.nr.eq.4.or.nr.eq.7.or.nr.eq.8) then  ! barrel 3,4, hose+stainless
                if (rhicrun.ge.14) then              ! ss in stub
                  if (.not.(rhicrun.eq.16.and.nr.eq.7)) then  ! in run16, nr=7 (layer 3 West) was not in
                    njtube = njtube+1 
                    call gspos('SJSR',njtube,v_m_name,posx,posy,
     &                   posz, irot,'ONLY')
                  endif
                else                                 ! noss in stub
                  nktube = nktube+1
                  call gspos('SKSR',nktube,v_m_name,posx,posy,
     &                 posz, irot,'ONLY')
                endif
              else                                   ! barrel 1,2 hose only
                if (rhicrun.lt.15) then              ! no ss
                  nktube = nktube+1
                  call gspos('SKSR',nktube,v_m_name,posx,posy,
     &                 posz, irot,'ONLY')
                else                              ! ss
                  njtube = njtube+1
                  call gspos('SJSR',njtube,v_m_name,posx,posy,
     &                 posz, irot,'ONLY')
                endif
              endif
            endif
            if (nr.ne.4 .and. nr.ne.7.and.rhicrun.ne.13) then        ! East half
              phideg = -(phi*180/pi + 90.)         ! some shifting and flipping
              irot = irot+1
              call gsrotm(irot,90.+theta,phideg,
     &                    90.,90.+phideg,
     &                    theta,phideg)
              posz = sili_br_spz(nr) + isign*(sili_br_spthck(nr) + 
     &        sqrt(0.5)*(0.5*sili_br_mandia(2)+stub_len) )
              if (nr.eq.3.or.nr.eq.4.or.nr.eq.7.or.nr.eq.8) then  ! barrel 3,4, hose+stainless
                if (rhicrun.ge.14) then
                  njtube = njtube+1
                  call gspos('SJSR',njtube,v_m_name,-posx,-posy,
     &                 posz, irot,'ONLY')
                else
                 nktube = nktube+1
                  call gspos('SKSR',nktube,v_m_name,-posx,-posy,
     &                 posz, irot,'ONLY')
                endif
              else                                                  ! barrel 1,2, hose only
                if (rhicrun.lt.15) then         ! no ss in stub
                  nktube = nktube+1
                  call gspos('SKSR',nktube,v_m_name,-posx,-posy,
     &                 posz, irot,'ONLY')
                else                              ! ss in stub
                  njtube = njtube+1
                  call gspos('SJSR',njtube,v_m_name,-posx,-posy,
     &                 posz, irot,'ONLY')
                endif
              endif
            endif
          enddo
*         End making little stubs -----------------------------------------------------
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

*       Make a single PCON for the cable sheet:
        cables_thk = 1.0*pixel_hdi_thk      ! scale factor
        par(1) =  pixel_hdi_phimin
        par(2) =  pixel_hdi_phimax - pixel_hdi_phimin 
        par(3) = 6                          ! nz
        par(4) = sili_endcap_z(1)           ! z1  
        par(5) =  sili_cg_rmx(4) - 1.6      ! 
        par(6) =  par(5) + 2*cables_thk     ! See logbook 18 feb 2015 pg 174 for a picture
        par(7) = sili_endcap_z(3) + 2.0     ! z2   Also logbook 29 Mar 2016 pg 120
        par(8) = par(5)                     ! 
        par(9) = par(6)                     ! 

        par(10) = par(7) + 2*cables_thk     ! z3
        par(11) = par(8) - 2.1*cables_thk   ! 
        par(12) = par(9)                    ! 

        par(13) = sili_br_manz(1) - 1.5     ! z4
        par(14) = sili_br_r(2) + 3.5        ! 
        par(15) = par(14) + 4.*cables_thk   ! 
        par(16) = par(13)+0.0               ! z5
        par(17) = sili_br_r(1)              ! 
        par(18) = par(15)                   ! 
        par(19) = par(16) + 2*cables_thk    ! z6
        par(20) = par(17)                   ! 
        par(21) = par(14)                   ! 

        nmed = sili_med_kapton
        call GSVOLU('SICC','PCON',nmed,par,21,ivolu)
        call GSPOS ('SICC',1,'SICG',0.,0.,0., irotnull,'ONLY')
        call GSPOS ('SICC',2,'SICG',0.,0.,0., irot1   ,'ONLY')
        call GSPOS ('SICC',3,'SICG',0.,0.,0., irot2,   'ONLY')
        call GSPOS ('SICC',4,'SICG',0.,0.,0., irot3,   'ONLY')
        call gsatt ('SICC','SEEN',1)
        call GSATT ('SICC','COLO', 4)                       

*       Make a PCON for the G10 connectors at the end of the Barrel1,2 hdi's:
        zg10 = par(19) + 0.01
        yg10 = par(21) + 0.01
        dg10 = 4.4
        tg10 = 0.2
        angg10 = atan2(abs(par(21)-par(12)) , abs(par(19)-par(10)) )
        par(01) =  pixel_hdi_phimin
        par(02) =  pixel_hdi_phimax - pixel_hdi_phimin 
        par(03) = 4                          ! nz

        par(04) = zg10 - dg10*cos(angg10)               ! z1
        par(05) = yg10 + dg10*sin(angg10)             ! 
        par(06) = par(5) + tg10/cos(angg10)                ! 

        par(07) = zg10                      ! z2
        par(08) = yg10        ! 
        par(09) = par(08) + tg10/cos(angg10)
 
        par(10) = par(07)   ! z3
        par(11) = par(08) - 0.2             ! 
        par(12) = par(09) 
                  ! 
        par(13) = par(10) + tg10  ! z4
        par(14) = par(11)                   ! 
        par(15) = par(12) - tg10/tan(angg10/2)                   ! 

        nmed = sili_med_gfrp     ! should be g10 xxx
        call GSVOLU('SJCC','PCON',nmed,par,15,ivolu)
        call GSPOS ('SJCC',1,'SICG',0.,0.,0., irot2,   'ONLY')
        call GSPOS ('SJCC',2,'SICG',0.,0.,0., irot3,   'ONLY')
        if (rhicrun.ne.13) then             ! no east vtx in '13
          call GSPOS ('SJCC',3,'SICG',0.,0.,0., irotnull,'ONLY')
          call GSPOS ('SJCC',4,'SICG',0.,0.,0., irot1   ,'ONLY')
        endif
        call gsatt ('SJCC','SEEN',1)
        call GSATT ('SJCC','COLO', 2)                       

        ss_vol_stubs = ss_vol_stub * njtube
        write (6,*) 'ss_vol_stubs = ', njtube, ss_vol_stubs, 
     &                         ss_vol_stubs*7.9,'g'
        write (6,*) 'ss_vol_arcs  = ', imant, ss_vol_arcs,
     &                         ss_vol_arcs*7.9,'g'
        write (6,*) 'total weight of SS couplings for run', 
     &   rhicrun,': ',(ss_vol_arcs+ss_vol_stubs)*7.9,'g'

        return 
        end
