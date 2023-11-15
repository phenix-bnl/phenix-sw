c $Id: svx_fvtx_old.f,v 1.8 2009/08/20 04:31:47 pinkenbu Exp $
C     File name: svx_fvtx_old.f      ( was previously part of svx.f)
C     ------xxx----------
C
C     Original author: Hubert van Hecke
C     Creation date: March, 2008
C
C     Purpose: Set up the Silicon Vertex Detector (FVTX)
C
C     Revision History: code lifted out of svx.f
C
C     15 Oct 08 HvH: fixed namelist for new phnx.par,
C                    and fixed rotation matrices for stations
*=====================================================================================

        SUBROUTINE svx_fvtx_old
        implicit none 

#include "gugeom.inc"
#include "gconst.inc"

        character*4  sil_name
        character*4  set_id      /'SVX '/           ! Detector/hit set ID
        Integer      nbitsv(7) /7*8/                ! Bits to pack vol. copy #
        Integer      idtype    /2001/               ! User def. detector type
        Integer      nwpa      /500/                ! Init. size of HITS banks
        Integer      nwsa      /500/                ! Init. size of DIGI banks
        integer      iset, idet
        INTEGER  LNAM(6)
        integer  LNUM(6) /1,1,1,1,1,1/
        Character*4 namesw(7) /'HALL','SIEN','SICG',
     &                         'SIxx','SIPx','SISI','SSSx'/
        integer sili_med_silicon /10/     ! Sensitive silicon nmed (nmat=50,Si)
        Integer sili_med_coldair /121/    ! Gas inside the SICG (cold air)
        integer sili_med_carbon  /123/    ! carbon-carbon composite
        Integer sili_med_passive /26/     ! Ladder passive nmed    (nmat=09,Al)
        integer sili_med_honeycomb /125/  ! 1/4" honeycomb, .5mm c-c skin, Al core

c       Hit component names
        character*4 inrNMSH(21) /'POSX','POSY','POSZ'  ! Global positions
     &     ,'DELE','TOFL'                              ! Energy loss & TOF
     &     ,'P_ID','MOMX', 'MOMY', 'MOMZ'              ! Particle ID & Entry mom.
     &     ,'XILC','YILC','ZILC','XOLC','YOLC','ZOLC'  ! Local entry & exit
     &     ,'XIGL','YIGL','ZIGL','XOGL','YOGL','ZOGL'/ ! global entry & exit

        Integer     nhh         /21/                   ! Number of hit components
        integer*4 inrNBITSH(21) /21*32/                ! Bits for packing the hits

c       Default setting of offsets and gains
        REAL inrORIG(21) /3*1000.,3*0.,3*1000.,6*1000.,6*1000./       ! offsets
        REAL inrFACT(21) /3*100000.,1.E7,1.e12,1.0,3*100000.
     &                   ,6*100000.,6*100000./         ! These gains give:
c              - 0.1 keV energy deposition resolution
c              - 0.0001 mm position resolution
c              - 0.01 MeV/c momentum resolution

        integer ivol1, ii, i, j, k, l, irx(48), irsh(12), istation, 
     &       irstation, icopy, irot1, irot2, wedges, nmed, idisk, ivolu

        character*4 wedge_name, support_name
        integer sili_endcap_layers    /8/    ! 4 south, 4 north
        real stagger                /0.0/    ! turn on endcap staggering in phi
        real stag_ang(12)        /12*0.0/    ! small rotations of endcap planes
        real sili_endcap_z(8), panthk, rinner, routerb, routers, 
     &       support_thk, sens_off, chipwid, chiplen, chipthk, cstep, 
     &       dd, z_disk, bangle, aangle, stationzthick, deg, rad, 
     &       pangle, wedge_thk, par(15), par_sisi_b(4), par_sisi_s(4), 
     &       par_s1(4), parb(10), pars(10), bwedge_lowx, bwedge_highx, 
     &       bwedge_len, swedge_lowx, swedge_highx, swedge_len, 
     &       bsil_lowx, bsil_highx, bsil_len, ssil_lowx, ssil_highx, 
     &       ssil_len, back_planthk, hdithk, silthk,
     &       bsic_lowx, bsic_highx, bsic_len, bsic_posx, bsic_posz,
     &       ssic_lowx, ssic_highx, ssic_len, ssic_posx, ssic_posz,
     &       sili_endcap_strip_on

        integer           itf_lun                    ! phnx(Sili).par logical unit
        common /interface/itf_lun                    ! in pisa core; namelist is read

      Real    sili_cg_rmn    /2.2/    ! Inner cage radius, cm
      Real    sili_cg_thck   /0.5/    ! Cage wall thickness, cm
      Real    sili_cg_inthck /0.2/    ! Thickness of the beam pipe ins., cm
      Real    sili_cg_tempc  /0.0/    ! Temperature inside the Cage, deg. C
      Integer sili_cg_npcon  /6/      ! Number of corners for SIEN's PCON
      Real    sili_cg_z(6)   /6*0.0/  ! z-pos. of the Cage corners
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


      logical ltest, lbarrel
      character*4 panel_name
      integer panels, sili_endcap_type, ishade, nparb, npars,
     &     irshade, ir1, ir2,
     &     ipanel, isen, nlevel, ierr, nsensors
      real plength, plength2, plength3,  plength4, plen3, plen4, 
     &     sbx1_thk,  strip3, sbx2_thk, strip6, gap1, strip_z5, 
     &     strip_z6, strip_z3, sithk, silen, silen2, silen3, silen4,
     &     silen5, silen6, silen7, silen8, silen9, zangle, zsep, 
     &     zdist, zdist2,  sangle2, d, d3, halfz, halfz3,
     &     sangle, stripz, stripz3, stripix, plen8, 
     &     stripz5, l1, stripoxb, stripoxy, stripx4, 
     &     stripy4, stripz4, par_s2(10), par_s4(10), shade_z, strip7,
     &     stripoy, stripozb, sili_endcap_zflat(8), rmax, delz, dely,
     &     shade_end, z_shade, 
     &     srvc_z, zangle_srvc, d4,
     &     parc(20), sili_srvc_rmn, cc_thick,
     &     srvc_zmid, sili_srvc_pos, bchipoff, schipoff,
     &     back_lowx, back_highx

      integer shade_start

        namelist /sili_endcap_par/                   ! from there. 
     &    sili_endcap_layers, sili_endcap_z, panthk, stagger, wedges,
     &    bwedge_lowx, bwedge_highx, bwedge_len,
     &    swedge_lowx, swedge_highx, swedge_len, 
     &    bsil_lowx,   bsil_highx,   bsil_len, 
     &    ssil_lowx,   ssil_highx,   ssil_len, back_lowx, back_highx, 
     &    back_planthk, hdithk, silthk, chiplen, chipwid, chipthk,
     &    bchipoff, schipoff, rinner, support_thk,
     &    bsic_lowx, bsic_highx, bsic_len, bsic_posx, bsic_posz,
     &    ssic_lowx, ssic_highx, ssic_len, ssic_posx, ssic_posz,
     &    sili_endcap_strip_on

      namelist /sili_endcap_old_par/
     &     sili_endcap_zflat,
     &     sili_srvc_pos, sili_srvc_rmn,
     &     cc_thick, panthk

*--------.---------.---------.---------.---------.---------.---------.--
*=========================================================================================================================
      rewind( unit = itf_lun )
      read( itf_lun, nml = sili_cg_par, err = 997 )
      read( itf_lun, nml = sili_endcap_par, err = 996 )
      read( itf_lun, nml = sili_endcap_old_par, err = 996 )
      write (6,*)' SVX_FVTX_OLD.F:: installing OLD endcaps'
    


C     ================================================================

C    Executable code
C    ===============
c======================================================================c
c=============================      Endcaps code    ===================c
c======================================================================c

      panels=24       ! total # of carbon panels per "cone" section
*     if (ltest) panels = 1
      plength =14.40  ! overall length of each large carbon panel
      plength2=10.55  ! overall length of each medium carbon panel
      plength3= 6.80  ! overall length of each small carbon panel
      plength4=14.0   !  ''                          service panel
*     plen3=.94       ! length along x of small end on large carbon panel
      plen3=.92       ! shrunk by .2mm so there is no overlap in the 'flat' mode
      plen4=4.45      ! length along x of large end on large carbon panel
*     panthk=.3       ! now in namelist: total thickness of a carbon panel (along z)
      sbx1_thk=.10    ! thickness (y) of inner carbon strip
      strip3=.38      ! width (x) of one carbon strip (same for all 3)
      sbx2_thk=.025   ! thickness (y) of outer carbon strip (changed from 0.05)
      strip6=6.41     ! distance of outer carbon strips from sm end of panel
      strip7=3.9      ! length (z) of outer radius strip on medium panel
      gap1=.11        ! tiny gap between carbon strip and edge of panel
      chiplen=1.3     ! total length of one chip (along z)
      chipwid=.38     ! total length of one chip (along x)
      chipthk=.030    ! total thickness of one chip (along y) Dec 2006 HvH
      strip_z5=5.0*chiplen    ! length (z) of inner-radius carbon strips (big) 
      strip_z6=6.0*chiplen    ! length (z) of outer-radius carbon strips 
      strip_z3=3.0*chiplen    !       ...     outer-radius ... (medium)
      sithk=.0300     ! total thickness of silicon panel, doe detectors
      silen=6.6       ! overall length of INNER silicon panel
      silen2=1.6      ! length along x of large end of INNER Si panel
      silen3=.54      ! length along x of small end of INNER Si panel
      silen4=7.9      ! overall length of the larger, OUTER silicon panel
      silen5=2.7      ! length along x of large end of larger OUTER Si panel
      silen6=1.6      ! length along x of small end of larger OUTER Si panel
      silen7=4.0      ! overall length of the smaller, OUTER silicon panel
      silen8=2.16     ! length along x of large end of smaller OUTER Si panel
      silen9=1.6      ! length along x of small end of smaller OUTER Si panel
      rinner=3.5      ! inner "radius" of each "cone" dist from z-axis
      zangle = 90.0  ! flat planes
      zsep=6.         ! distance between "cone" sections
      zdist=19.86+18  ! z dist of outermost edge on outermost "cone"
                      ! Note: zdist depends on zsep.
      zdist2=19.86    ! z dist of outermost edge on small "cones"
      sangle2=0.0     ! just a place holder for now
     
C ----These values (below) are calculated from those entered above-----

C   d      = distance from z-axis to the midpoint of each large carbon panel
C   d3     = dist from z-axis to midpoint of each small carbon panel
C   halfz  = the half length along z for each large carbon panel

      deg      = 2*PI/360           ! To convert degrees into radians

      d        = (plength /2)*SIN(zangle*DEG)+rinner
      d3       = (plength3/2)*SIN(zangle*DEG)+rinner

      halfz    = (plength/2)*COS(zangle*DEG)
      halfz3   = (plength3/2)*COS(zangle*DEG)

      rad      = 360/(2*PI)         ! To convert radians into degrees
      pangle   = (360/panels)       ! The total angle of each carbon panel
      pangle   = (360/24.0)         ! The total angle of each carbon panel
      sangle   = 3.47433            ! angle of carbon strips

      stripz   = -plength /2 + (gap1+strip_z5/2)*COS(sangle*deg) ! z-position of strips
      stripz3  = -plength3/2 + (gap1+strip_z5/2)*COS(sangle*deg)

      stripix   = plen3/4+(gap1+strip_z5/2)*SIN(sangle*deg)   ! x-position of inner SI, SBX1
      plen8    = 2*plength3*TAN(2*sangle*deg)+plen3
 
      stripz5  = -plength3/2 + (gap1+strip_z5/2)*COS(sangle*deg)
      l1       = rinner/SIN(zangle*deg)
 
      stripoxb  = plen3/4  +(strip6+strip_z6/2)*SIN(sangle*deg)
      stripoy   = (panthk+sbx2_thk)/2
      stripozb  = -plength /2 + (strip6+strip_z6/2)*COS(sangle*deg)

      stripx4  = plen3/4  +(strip6+strip7/2)*SIN(sangle*deg)
      stripy4  = (panthk+sbx2_thk)/2
      stripz4  = -plength2/2 + (strip6+strip7/2)*COS(sangle*deg)

      do i=1,8
        sili_endcap_z(i) = sili_endcap_zflat(i)
      enddo 

c=============     Now define the endcap volumes:   =============================c

C---  Some items common to DOE and LDRD are needed first:
      PAR_s1(1) = silen3/2         ! This is one inner silicon panel
      PAR_s1(2) = silen2/2         ! found in SIPB and SIPS
      PAR_s1(3) = sithk/2
      PAR_s1(4) = silen/2
      CALL GSVOLU( 'SISI', 'TRD1 ', sili_med_silicon, PAR_s1, 0, IVOL1) ! 0 parameters
*     write (15,'(''small si trapezoid: '',4f6.3)') (par_s1(i),i=1,4)

      if (stagger.gt.0) then
        stag_ang( 5) = 0.                       ! staggering angle in +phi direction
        stag_ang( 6) = 0.9375                   ! 1x 360 / 96 / 4
        stag_ang( 7) = 1.8750                   ! 2x
        stag_ang( 8) = 2.8125                   ! 3x   stagger >0 PATTERN: 
        stag_ang( 9) = 0.                       ! 0x   (1 2 3 4 - 1 2 3 4)
        stag_ang(10) = 0.9735                   ! 1x
        stag_ang(11) = 1.8750                   ! 2x
        stag_ang(12) = 2.8125                   ! 3x
      elseif(stagger.lt.0) then
        stag_ang( 8) = 0.                       ! staggering angle in +phi direction
        stag_ang( 6) = 0.9375                   ! 1x 360 / 96 / 4
        stag_ang( 5) = 1.8750                   ! 2x
        stag_ang( 7) = 2.8125                   ! 3x   stagger <0 PATTERN:
        stag_ang( 9) = 0.                       ! 0x   (3 2 4 1 - 1 4 2 3)
        stag_ang(11) = 0.9735                   ! 1x
        stag_ang(12) = 1.8750                   ! 2x
        stag_ang(10) = 2.8125                   ! 3x
      endif

      do ishade=5,12                       ! make a rot matrix for each shade
        irot = irot + 1                    ! such that all face in the same direction, 
        irsh(ishade) = irot                ! and phi=0 starts at y=0, and goes positive. 
          CALL GSROTM( irsh(ishade), 
     &                 90.,  0.  +stag_ang(ishade)*abs(stagger),
     &                 90., 90.  +stag_ang(ishade)*abs(stagger), 
     &                  0.,  0. )

      enddo

C (1) This is one large mother panel. The extras in x (.15 and .3 are so
C  that the overhanging silicon does not protrude from this mother volume.
      PAR(1) = plen3/2 + .15       ! half length along x at -z
      PAR(2) = plen4/2 + .3        ! half length along x at +z
      PAR(3) = sithk+chipthk+sbx1_thk+panthk/2  ! half thickness (y)
      PAR(4) = plength/2      ! half length along z
      CALL GSVOLU( 'SIPB', 'TRD1 ', sili_med_coldair, PAR, 4, IVOL1)

      PAR(1) = plen3/2             ! This is one large carbon panel inside SIPB
      PAR(2) = plen4/2
      PAR(3) = panthk/2
      PAR(4) = plength/2
      CALL GSVOLU( 'SICB', 'TRD1 ', sili_med_carbon, PAR, 4, IVOL1)

      PAR_s2(1) = silen6/2         ! This is one outer silicon panel in SIPB
      PAR_s2(2) = silen5/2
      PAR_s2(3) = sithk/2
      PAR_s2(4) = silen4/2         ! no GSVOLU call, done with GSPOSP

      PAR_s4(1) = silen9/2         ! This is one outer silicon panel in SIPM
      PAR_s4(2) = silen8/2
      PAR_s4(3) = sithk/2
      PAR_s4(4) = silen7/2         ! no GSVOLU call, done with GSPOSP
                                   
      PAR(1) = chipwid/2           ! This is one computer chip inside SIPB,S
      PAR(2) = chipthk/2      
      PAR(3) = chiplen/2
      CALL GSVOLU( 'SCHP', 'BOX ', sili_med_passive, PAR, 3, IVOL1)  ! med is Al

C (3) This is one small mother panel.  The extras in x (.15 and .3) are so  
C that the overhanging silicon does not protrude from this mother volume.
      PAR(1) = plen3/2 + 0.15                   ! half length along x at -z
      PAR(2) = plen8/2 + 0.30                   ! half length along x at +z
      PAR(3) = sithk+chipthk+sbx1_thk+panthk/2    ! half length along y
      PAR(4) = plength3/2                       ! half length along z
      CALL GSVOLU( 'SIPS', 'TRD1 ', sili_med_coldair, PAR, 4, IVOL1)

      PAR(1) = plen3/2             ! This is one small carbon panel inside SIPS
      PAR(2) = plen8/2
      PAR(3) = panthk/2
      PAR(4) = plength3/2
      CALL GSVOLU( 'SICS', 'TRD1 ', sili_med_carbon, PAR, 4, IVOL1)

c---- PCON mother volume to hold lampshades: --------------------------!---------
      shade_z = 1.0                   ! lampshade thickness (down from 1.0)

      parb( 1) = 0                  ! start angle
      parb( 2) = 360                ! full 360 cone
      parb( 3) = 2                  ! 2 z's will be given
      parb( 4) = -shade_z/2.0       ! z1
      parb( 5) = sili_cg_rmn 
      parb( 6) = sili_cg_rmx(3) - 3.6
      parb( 7) = -parb(4)           ! z2
      parb( 8) =  parb(5)           ! at inner radius
      parb( 9) =  parb(6)           ! at inner radius
      nparb = 9
                                        ! Next for the small shade mother volume:
      rmax = sili_cg_rmx(3) - sili_cg_thck - (plength-plength3) +0.6-0.5
      delz = (rmax - sili_cg_rmn)/tan(zangle*deg)
      dely = shade_z * tan(zangle*deg)

      pars( 1) = 0                    ! start angle
      pars( 2) = 360                  ! full 360 cone
      pars( 3) = 2                    ! 2 z's will be given
      pars(4) = -shade_z/2.0          ! z1
      pars(5) = sili_cg_rmn           ! inner radius
      pars(6) = 10.5                   ! outer radius (number by hand)
      pars(7) = shade_z/2.0           ! z2
      pars(8) = sili_cg_rmn 
      pars(9) = pars(6)
      npars = 9

      shade_start = 5

      do ishade=shade_start,12                             ! define 4, 8 copies SI05 - SI12
        write (sil_name, '(''SI'',I2.2)') ishade
        if (ishade.eq.8 .or. ishade.eq.9) then             ! small lampshades
          CALL GSVOLU(sil_name,'PCON',sili_med_coldair,pars,npars,ivolu)
        else                                               ! big lampshades
          CALL GSVOLU(sil_name,'PCON',sili_med_coldair,parb,nparb,ivolu) 
        endif
      enddo

C=================   Position the detectors ======================c

      do i = 0, panels-1                 ! make 24 matrices, these
        irot = irot+1                    ! will be used several times
        irx(i+1) = irot
        CALL GSROTM( irx(i+1),
     &               90.       , (    -360*(I+0.5)/panels) ,
     &               90+zangle , (90. -360*(I+0.5)/panels) ,
     &                  zangle , (90  -360*(I+0.5)/panels))
      enddo

      do ishade = shade_start,12                       ! Place shades: loop north to south
        write (sil_name, '(''SI'',I2.2)') ishade       ! SI05, SI06 ... SI12
        z_shade = sili_endcap_z(ishade-4)              ! z from par file
        irshade = irsh(ishade)                         ! rotation of this shade
        CALL GSPOS (sil_name, 1, 'SICG',               ! Place this shade
     &         0., 0., z_shade , irshade, 'ONLY')      ! z = -zdist+(ishade-5)*zsep+halfz
                                                       ! Now fill them with panels:
        if (ishade.eq.8 .or. ishade.eq.9) then         ! small panel parameters:
          panel_name = 'SIPS'                          ! name
          dd = d3                                      ! center distance
        else                                           ! big panel parameters: 
          panel_name = 'SIPB'                          ! big panel name          
          dd = d                                       ! center 
        endif
        do i = 0, panels-1                             ! place panels in lampshades, 'MANY' !!!
          CALL GSPOS (panel_name , i+1, sil_name ,
     &              dd*SIN(2*PI*(i+0.5)/panels),
     &              dd*COS(2*PI*(i+0.5)/panels), 0.0 , irx(i+1), 'MANY')
        enddo                                          ! end placing 24 panels in the shade
      enddo                                            ! loop over all lampshades

c------------------------------------------------------!---------------!-------------
      
*     Place the inner(radius) carbon strips on the big carbon panels
      irot = irot+1
      ir1 = irot
      CALL GSROTM( ir1, 90-sangle, 0. , 90. , 90. , sangle, 180.)
      irot = irot+1                            ! and another copy on the other side
      ir2 = irot
      CALL GSROTM( ir2, 90+sangle, 0. , 90. , 90. , -sangle, 180.)

*     Place the inner-radius silicon inside SIPB
      CALL GSPOSP( 'SISI' , 1, 'SIPB' , -stripix , 
     &      panthk/2+sbx1_thk+chipthk+sithk/2 , stripz ,ir1,'ONLY',
     &      par_s1, 4)
      CALL GSPOSP ( 'SISI' , 2, 'SIPB' ,  stripix , 
     &    -(panthk/2)-sbx1_thk-chipthk-sithk/2 , stripz ,ir2,'ONLY',
     &      par_s1, 4)
                           ! Place the outer-radius silicon in SIPB
      CALL GSPOSP ( 'SISI' , 3, 'SIPB' , stripoxb , panthk/2+
     &      sithk/2+sbx2_thk +chipthk,  stripozb, ir2,'ONLY',
     &      par_s2,4)
      CALL GSPOSP ( 'SISI' , 4, 'SIPB' , -stripoxb, -panthk/2-
     &     (chipthk+sithk)/2-sbx2_thk -chipthk/2,  stripozb, ir1,'ONLY',
     &      par_s2,4)
                           ! Place the readout chips in SIPB,
      do i=-5,5,2          !  6 under one outer si chip: copies 1-6
        CALL GSPOS( 'SCHP', (i+7)/2, 'SIPB' ,  plen3/4 + 
     &    (strip6 + strip_z6/2 + i*chiplen/2) * sin(sangle*deg), 
     &    (panthk/2) + sbx2_thk + chipthk/2,  -plength/2 + 
     &    (strip6 + strip_z6/2 + i*chiplen/2)*cos(sangle*deg), 
     &    ir2,'ONLY')
      enddo
                          ! and 6 under the other outer si chip:
      do i=-5,5,2         ! copies 7-12
c--------1---------1---------1---------1---------1---------1---------1--
        CALL GSPOS( 'SCHP', (i+19)/2, 'SIPB' ,  -plen3/4 - 
     &    (strip6 + strip_z6/2 + i*chiplen/2) * sin(sangle*deg), 
     &   -(panthk/2) - sbx2_thk - chipthk/2,  -plength/2 + 
     &      (strip6 + strip_z6/2 + i*chiplen/2)*cos(sangle*deg), 
     &    ir1,'ONLY')
      enddo
                          ! chips under the inner-radius silicon
      do i=-2,2           ! copies 13-17
        CALL GSPOS( 'SCHP' , 15+i, 'SIPB' ,
     &      -stripix - i*chiplen*sin(sangle*deg) , 
     &      (panthk/2) + sbx1_thk + chipthk/2 ,
     &      stripz + i*chiplen*cos(sangle*deg),
     &      ir1,'ONLY')
      enddo

      do i=-2,2           ! copies 18-22, inner-radius, other side
        CALL GSPOS( 'SCHP' , 20+i, 'SIPB' ,
     &      stripix + i*chiplen*sin(sangle*deg) , 
     &      -(panthk/2) - sbx1_thk - chipthk/2 ,
     &      stripz + i*chiplen*cos(sangle*deg),
     &      ir2,'ONLY')
      enddo

C     Place the Large carbon panels in the mother panel volume
      CALL GSPOS( 'SICB' , 1, 'SIPB', 0. , 0. , 0. ,0, 'ONLY')


c---- now fill the small panel mother volume: --------------------------------- 

*     Place carbon panel in small lampshade panel mother volume
      CALL GSPOS( 'SICS' , 1, 'SIPS', 0. , 0. , 0. ,0,'ONLY')

*     Place the inner(radius) silicon in small panel volume SIPS:
c--------1---------1---------1---------1---------1---------1---------1-- 
      CALL GSPOSP ( 'SISI' , 1, 'SIPS' , -stripix , 
     &     panthk/2 + sbx1_thk + chipthk + sithk/2 , stripz5,ir1,'ONLY',
     &     par_s1,4)
      CALL GSPOSP ( 'SISI' , 2, 'SIPS' ,  stripix , 
     &    -panthk/2 - sbx1_thk - chipthk - sithk/2 , stripz5,ir2,'ONLY',
     &     par_s1,4)

                          ! Readout chips under the inner-radius silicon
      do i=-2,2           ! copies 39-43
        CALL GSPOS( 'SCHP' , 41+i, 'SIPS' ,
     &      -stripix - i*chiplen*sin(sangle*deg) , 
     &      (panthk/2) + sbx1_thk + chipthk/2 ,
     &      stripz5 + i*chiplen*cos(sangle*deg),
     &      ir1,'ONLY')
      enddo
      do i=-2,2           ! inner, on the other (x) side, copies 44-48
        CALL GSPOS( 'SCHP' , 46+i, 'SIPS' ,
     &      stripix + i*chiplen*sin(sangle*deg) , 
     &      -(panthk/2) - sbx1_thk - chipthk/2 ,
     &      stripz5 + i*chiplen*cos(sangle*deg),
     &      ir2,'ONLY')
      enddo

c================= declare the sensitive volumes   ====================c

*     namesw    = HALL, SIEN, SICG, SIxx, SIPy, SISI, where xx=05-12, y = B, M or S
      do ishade = shade_start,12
        write (namesw(4),'(''SI'',I2.2)')  ishade
                                           namesw(5) = 'SIPB'
        if (ishade.eq.8 .or. ishade.eq. 9) namesw(5) = 'SIPS'
*       write (6,*)' gsdet 1:'
        call gsdet (set_id, namesw(4), 6, namesw, nbitsv, idtype, 
     &                                  nwpa, nwsa, iset, idet)
        call gsdeth(set_id, namesw(4), nhh,inrNMSH,inrNBITSH,
     &            inrORIG,inrFACT)
      enddo

c----  Hide some of the volumes, and set colors ----------------------!---------------------------
c     -2 = only that volume seen, no descendants
c     -1 = none visible
c      0 = volume is not visible but descendants are
c      1 = both volume and descendants are visible

      do ishade = shade_start,12                 ! SI05, SI06 ... SI12
        write (sil_name, '(''SI'',I2.2)') ishade
        call gsatt(sil_name, 'SEEN', 1)  !
        call gsatt(sil_name, 'COLO', 7)  !
      enddo

      CALL GSATT( 'SIPB', 'SEEN ', 0)    ! big panels in big lampshade
      CALL GSATT( 'SIPS', 'SEEN ', 0)    ! small  ''     small   ''
      CALL GSATT( 'SICB', 'SEEN ', 1)    ! Carbon panels big
      CALL GSATT( 'SICS', 'SEEN ', 1)    !               small

      CALL GSATT( 'SCHP ', 'SEEN ', 1)   ! readout chips
                                         ! Add color to individual pieces
      CALL GSATT( 'SICB', 'COLO', 1)     ! Carbon       1=black
      CALL GSATT( 'SICS', 'COLO', 1)     !     2=red    5=yellow    8=white

*------ end of big cone/flat endcaps ------------------------------------------------------

C (4) This is one service panel.
      PAR(1) = 2.0*pi*sili_srvc_rmn/48.0       ! half length along x at -z
      PAR(2) = 2.0*pi*(sili_cg_rmx(3) - sili_cg_thck -0.5)/48.0 -0.1 ! half length along x at +z
      srvc_z = 0.7                    ! width of cone base (down from 1.5
      par(3) = 0.15                   ! half-thickness (down from 0.5)
      zangle_srvc = 50.0                        ! angle of service shade with z-axis      
      plength4 = (sili_cg_rmx(3) - sili_cg_thck
     &                - sili_srvc_rmn)/sin(zangle_srvc*deg)
     &                - PAR(3)/cos(zangle_srvc*DEG) -0.1 -0.5
      PAR(4) = plength4/2.0                         ! half length along z
      d4 = (plength4/2.0 + 0.5*PAR(3)/cos(zangle_srvc*DEG))
     &                       *SIN(zangle_srvc*DEG)+sili_srvc_rmn
      CALL GSVOLU( 'SIPT', 'TRD1 ', sili_med_coldair, PAR, 4, IVOL1)
      CALL GSVOLU( 'SIQT', 'TRD1 ', sili_med_coldair, PAR, 4, IVOL1)

C (4b) cooling tube (moved further up, since copies are used in the barrel)
c     call gsvolu( 'SICT', 'TUBE', sili_med_coolant, par, 0, ivol1)

C (5) This is one small service panel.
      PAR(1) = 2.0*pi*sili_cg_rmn  /48.0 ! half length along x at +z
      PAR(2) = 2.0*pi*sili_srvc_rmn/48.0       ! half length along x at -z
      par(3) = 0.15   ! half thickness =  y doen from 0.5
      PAR(4) = 0.5*(sili_srvc_rmn-sili_cg_rmn)              ! half length along z
      CALL GSVOLU( 'SJPT', 'TRD1 ', sili_med_coldair, PAR, 4, IVOL1)
      CALL GSVOLU( 'SJQT', 'TRD1 ', sili_med_coldair, PAR, 4, IVOL1)

      if (ltest) then
        PAR(1) = 9.0                    ! test box to show +x, +y and +z directions:
        PAR(2) = 9.0
        PAR(3) = (sili_cg_z(6)-sili_cg_thck)/2.
        CALL GSVOLU( 'TEST', 'BOX ', sili_med_coldair , PAR, 3, IVOL1)
        call gspos  ('TEST',1,'SICG', par(1), par(2), par(3), 
     &                      0, 'ONLY')  ! test box showing +x,+y,+z

      endif

c(x)  this is the barrel support structure cone -----------------------!----------------------
      rmax = sili_cg_rmx(3) - sili_cg_thck + 0.15 - 0.7
      delz = (rmax - sili_srvc_rmn)/tan(zangle_srvc*deg)

      dely = srvc_z * tan(zangle_srvc*deg)
      srvc_zmid = (srvc_z + delz) / 2.0

      parc( 1) = 0                    ! start angle
      parc( 2) = 360                  ! full 360 cone
      parc( 3) = 5                    ! 5 z's will be givenc

      parc( 4) = -srvc_zmid           ! z1
      parc( 5) = sili_cg_rmn
      parc( 6) = sili_srvc_rmn

      parc( 7) = parc(4) + srvc_z     ! z2
      parc( 8) = sili_cg_rmn          ! at inner radius
      parc( 9) = sili_srvc_rmn + dely

      parc(10) = parc(4) + srvc_z     ! z3
      parc(11) = sili_srvc_rmn        ! at inner radius
      parc(12) = sili_srvc_rmn + dely

      parc(13) = parc(7) + delz - srvc_z         ! z4
      parc(14) = rmax -dely
      parc(15) = rmax                            ! outer radius

      parc(16) = parc(4) + delz + srvc_z         ! z5
      parc(17) = rmax                            ! at outer radius
      parc(18) = rmax

      CALL GSVOLU('SISR','PCON', sili_med_coldair,parc,18,ivolu) 

      do i = 0, panels-1                             ! place support panels in cone
*xxx    if (i.ne.0.and.i.ne.11.and.i.ne.12.and.i.ne.23) then swap dec 2006 hvh
        if (i.eq.0. or.i.eq.11. or.i.eq.12. or.i.eq.23) then 
          irot = irot+1                    ! will be used several times
          CALL GSROTM(irot, 
     &             90.            , (     -360*(I+0.5)/panels), 
     &             90.+zangle_srvc, (90.  -360*(I+0.5)/panels),
     &                 zangle_srvc, (90. - 360*(I+0.5)/panels)    )
          CALL GSPOS ('SIPT' , i+1, 'SISR' ,         !   volume
     &                 d4*SIN(2*PI*(i+0.5)/panels),
     &                 d4*COS(2*PI*(i+0.5)/panels), 0.00 , irot, 'ONLY')

          irot = irot+1                    ! will be used several times
          CALL GSROTM(irot, 90.0, (   -360.*(I+0.5)/panels), 
     &                      0.0,   0.0,
     &                      90.0, (90.-360.*(I+0.5)/panels) )
          CALL GSPOS ('SJPT' , i+1, 'SISR' ,         !   volume
     &         0.5*(sili_srvc_rmn+sili_cg_rmn)*SIN(2*PI*(i+0.5)/panels),
     &         0.5*(sili_srvc_rmn+sili_cg_rmn)*COS(2*PI*(i+0.5)/panels), 
     &         -srvc_zmid+srvc_z/2.0 , irot, 'ONLY')
        else                                         ! 4 panels without tubes

          irot = irot+1                    ! will be used several times
          CALL GSROTM(irot, 90.           , (    -360*(I+0.5)/panels), 
     &                      90+zangle_srvc, (90. -360*(I+0.5)/panels),
     &                         zangle_srvc, (90. -360*(I+0.5)/panels) )
          CALL GSPOS ('SIQT' , i+1, 'SISR' ,
     &                  d4*SIN(2*PI*(i+0.5)/panels),
     &                  d4*COS(2*PI*(i+0.5)/panels), 0.0 , irot, 'ONLY')

          irot = irot+1                    ! will be used several times
          CALL GSROTM(irot, 90.0, (   -360.*I/panels), 
     &                      0.0,   0.0,
     &                      90.0, (90.-360.*I/panels) )
          CALL GSPOS ('SJQT' , i+1, 'SISR' ,
     &         0.5*(sili_srvc_rmn+sili_cg_rmn)*SIN(2*PI*i/panels),
     &         0.5*(sili_srvc_rmn+sili_cg_rmn)*COS(2*PI*i/panels), 
     &         -srvc_zmid+srvc_z/2.0 , irot, 'ONLY')
        endif                                          ! tubes / no tubes
      enddo                                            ! end placing 24 panels in the shade

      if (lbarrel) then
        CALL GSPOS ('SISR', 1, 'SICG',                 ! Place the services - north cone
     &         0., 0., sili_srvc_pos , irotnull, 'ONLY')  
        irot = irot+1                                  ! rotate 180 around y
        CALL GSROTM(irot, 90.,180.,90.,90.,180.,0.)
        CALL GSPOS ('SISR', 2, 'SICG',                 ! Place the services - south cone
     &         0., 0., -sili_srvc_pos , irot, 'ONLY') 
      endif
c---  end build the common support structure cones ----------------------!-----------------

c----  Hide some of the volumes, and set colors ----------------------!---------------------------
c     -2 = only that volume seen, no descendants
c     -1 = none visible
c      0 = volume is not visible but descendants are
c      1 = both volume and descendants are visible

      CALL GSATT( 'SISI', 'SEEN ', 1)    ! silicon
      CALL GSATT( 'SISI', 'COLO', 6)     ! silicon

      CALL GSATT( 'SIPT', 'COLO', 7)     ! service panel
      CALL GSATT( 'SIQT', 'COLO', 7)     ! service panel
      CALL GSATT( 'SJPT', 'COLO', 7)     ! service panel
      CALL GSATT( 'SJQT', 'COLO', 7)     ! service panel

      CALL GSATT( 'SISI','WORK',1)       ! Make volumes sensitive

*-----------------------------------------------------------------------------------

      call uctoh('HALL', LNAM(1), 4, 4)            ! copy ascii to hollerith
      call uctoh('SIEN', LNAM(2), 4, 4)            !
      call uctoh('SICG', LNAM(3), 4, 4)            !

      do ishade = shade_start,12                   ! loop over 4 or 8 lampshades
        write (sil_name, '(''SI'',I2.2)') ishade   !
        call uctoh(sil_name, LNAM(4), 4, 4)        !
        lnum(4) = 1
        do ipanel = 1, panels                      ! loop over 6 or 24 panels per shade
           lnum(5) = ipanel                        ! 
           sil_name = 'SIPB'                       ! big, small panels
           if (ishade.eq.8 .or. ishade.eq. 9) sil_name = 'SIPS'
           call uctoh(sil_name, LNAM(5), 4, 4)     !
                                   nsensors = 4    !
           if (sil_name.eq.'SIPS') nsensors = 2    ! No outer silicon
           do isen = 1, nsensors                   ! front, back, and inner, outer radius
             lnum(6) = isen                        ! of the carbon panel
             call uctoh('SISI', LNAM(6),4,4)
             nlevel=0                            ! for next call to glvolu:
             call glvolu(6, LNAM, LNUM, ierr)    ! fills /GCVOLU/
            enddo      ! 4 si detectors per panel
        enddo         !  6, 24 panels per lampshade
      enddo           ! loop over 4, 8 lampshades
      nlevel = 0

      return      ! from subroutine svx_fvtx_old

 996  stop 'svx_fvtx_old - read error in sili_endcap_par segment.'
 997  stop 'svx_fvtx_old - read error in sili_cg_par segment.'
      end      ! end of subroutine svx

c=============================================================================c
