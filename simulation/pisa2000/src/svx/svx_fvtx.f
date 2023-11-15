C$Id: svx_fvtx.f,v 1.23 2018/02/17 01:08:58 shlim Exp $
C     File name: svx_fvtx.f      ( was previously part of svx.f)
C     --------------------
C
C     Original author: Hubert van Hecke, Dave Lee
C     Creation date: March, 2008
C
C     Purpose: Set up the Silicon Vertex Detector (FVTX)
C
C     Revision History:  code was lifted out of svx.f
C
C     16 Oct 08 HvH: fixed rotation matrices for placing si05-si12 disks.
C     06 Nov 08 HvH: small move of SNCC after moving barrel supports
C     04 Jan 11 HvH: Split disks into East and West halves. This is a major 
C                    reorganization
C     April 12 HvH: read disk survey numbers from the SQL database
C     Dec 2014 HvH: reorg for the G3->G4 work. Removed holder volumes SCM1-4
C     Mar 2015 HvH: small mods, code cleanup using ~pinkenbu/ftnchek
C 
C=====================================================================================

      SUBROUTINE svx_fvtx
      implicit none

#include "gugeom.inc"
#include "gconst.inc"
      character*4  sil_name
      character*4  cag_name
      character*4  set_id      /'SVX '/           ! Detector/hit set ID
      integer      nbitsv(7) /7*8/                ! Bits to pack vol. copy #
      integer      idtype    /2001/               ! User def. detector type
      integer      nwpa      /500/                ! Init. size of HITS banks
      integer      nwsa      /500/                ! Init. size of DIGI banks
      integer      iset, idet
      
      Character*4 namesw(9) /'HALL','SIEN','SICG',
     &              'SCMx','DUMM','SIzz','SIPq','SISq','SISI'/
c      Character*4 namesw(8) /'HALL','SIEN','SICG',
c     &              'SCMx','SIzz','SIPq','SISq','SISI'/
      integer sili_med_silicon /10/     ! Sensitive silicon nmed (nmat=50,Si)
      integer sili_med_silipass/11/     ! Non-sensitive silicon nmed (nmat=50,Si)
      integer sili_med_coldair /121/    ! Gas inside the SICG (cold air)
      integer sili_med_carbon  /123/    ! carbon-carbon composite
      integer sili_med_passive /26/     ! Ladder passive nmed    (nmat=09,Al)
      integer sili_med_honeycomb /125/  ! 1/4" honeycomb, .5mm c-c skin, Al core

c     Hit component names
      character*4 inrNMSH(21) /'POSX','POSY','POSZ'  ! Global positions
     &   ,'DELE','TOFL'                              ! Energy loss & TOF
     &   ,'P_ID','MOMX', 'MOMY', 'MOMZ'              ! Particle ID & Entry mom.
     &   ,'XILC','YILC','ZILC','XOLC','YOLC','ZOLC'  ! Local entry & exit
     &   ,'XIGL','YIGL','ZIGL','XOGL','YOGL','ZOGL'/ ! global entry & exit

      integer     nhh         /21/                   ! Number of hit components
      integer*4 inrNBITSH(21) /21*32/                ! Bits for packing the hits

c     Default setting of offsets and gains
      real inrORIG(21) /3*1000.,3*0.,3*1000.,6*1000.,6*1000./       ! offsets
      real inrFACT(21) /3*100000.,1.E7,1.e12,1.0,3*100000.
     &                 ,6*100000.,6*100000./         ! These gains give:
c            - 0.1 keV energy deposition resolution
c            - 0.0001 mm position resolution
c            - 0.01 MeV/c momentum resolution

      integer ivol1, i, istz, jstz, 
     &     irot1, irot2, wedges, nmed, idisk, 
     &     ivolu, irot3, irot4, ibgsm, half_cage, jdisk,
     &     sili_endcap_strip_on, sili_use_survey
      character*4 wedge_name, support_name
      character*40 sili_deltas_file 
      character*100 line
      character*4 cage_name /' '/
 
      integer sili_endcap_layers    /8/    ! 4 south, 4 north
      real stagger                /0.0/    ! turn on endcap staggering in phi
      real stag_ang(20)        /20*0.0/    ! small rotations of endcap planes
      real sili_endcap_z(8), panthk, rinner, routerb, routers, 
     &     support_thk, sens_off, chipwid, chiplen, chipthk, cstep, 
     &     dd, z_disk, stationzthick, deg, rad, alpha, beta, gamma,
     &     pangle, wedge_thk, par(21), par_sisi_b(4), par_sisi_s(4), 
     &     par_s1(4), parb(9), pars(9), bwedge_lowx, bwedge_highx, 
     &     bwedge_len, swedge_lowx, swedge_highx, swedge_len, 
     &     bsil_lowx, bsil_highx, bsil_len, ssil_lowx, ssil_highx, 
     &     ssil_len, back_planthk, hdithk, silthk, bchipoff, schipoff,
     &     bsic_lowx, bsic_highx, bsic_len, bsic_posx, bsic_posz,
     &     ssic_lowx, ssic_highx, ssic_len, ssic_posx, ssic_posz,
     &     back_lowx, back_highx, sback_lowx, sback_highx,
     &     xwedge, ywedge, zwedge, sens_off_small,
     &     sens_off_big, dtip, dsi

      integer           itf_lun                    ! phnx(Sili).par logical unit
      common /interface/itf_lun                    ! in pisa core; namelist is read
      namelist /sili_endcap_par/                   ! from there. 
     &  sili_endcap_layers, sili_endcap_z, panthk, stagger, wedges,
     &  bwedge_lowx, bwedge_highx, bwedge_len,
     &  swedge_lowx, swedge_highx, swedge_len, 
     &  bsil_lowx,   bsil_highx,   bsil_len, 
     &  ssil_lowx,ssil_highx,ssil_len,back_lowx,back_highx,sback_lowx,
     &  sback_highx, 
     &  back_planthk, hdithk, silthk, chiplen, chipwid, chipthk,
     &  bchipoff, schipoff, rinner, support_thk,
     &  bsic_lowx, bsic_highx, bsic_len, bsic_posx, bsic_posz,
     &  ssic_lowx, ssic_highx, ssic_len, ssic_posx, ssic_posz,
     &  sili_endcap_strip_on, sili_use_survey, sili_deltas_file

*---- NOTE: namelist sili_cg_par appears in svx_fvtx.f AND svx.f ----- make sure it is the same
c---  VTX Envelope/Cage parameters: Volumes SIEN(outer)/SICG(inner)
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      Integer sili_cg_npcon  /6/      ! Number of corners for SIEN's PCON
      Real    sili_cg_z(6)   /6*0.0/  ! z-pos. of the Cage corners
      Real    sili_cg_rmn    /2.2/    ! Inner cage radius, cm
      Real    sili_cg_rmx(6) /6*0.0/  ! Outer SIEN radii at the corners, cm
      Real    sili_cg_thck   /0.5/    ! Cage wall thickness, cm
      Real    sili_cg_inthck /0.2/    ! Thickness of the beam pipe ins., cm
      Real    sili_cg_xdisp  /0.0/    ! x-displacement of SIEN in HALL, cm
      Real    sili_cg_ydisp  /0.0/    ! y-displacement of SIEN in HALL, cm
      Real    sili_cg_zdisp  /0.0/    ! z-displacement of SIEN in HALL, cm
      Real    sili_cg_tempc  /0.0/    ! Temperature inside the Cage, deg. C
      real    sili_cg_rinner          ! part of fvtx cage definition
      real    sili_cg_swedge_len      !    ''      note thet these are copied from
      real    sili_cg_bwedge_len      !    ''      the endcap namelist.
      real    sili_cg_support_thk     !    ''      
      real    fcg_z1, fcg_z2, fcg_z3, fcg_r1, fcg_r2, fcg_t, 
     &        fcg_z                   ! forward vertex cage variables
      Integer sili_endcap_config      ! fvtx (1), ifvtx (2) or none (0)

      namelist /sili_cg_par/ sili_cg_npcon,sili_cg_z
     &     ,sili_cg_rmn,sili_cg_rmx,sili_cg_thck,sili_cg_inthck
     &     ,sili_cg_xdisp,sili_cg_ydisp,sili_cg_zdisp
     &     ,sili_cg_tempc, sili_cg_rinner, sili_cg_swedge_len,
     &     sili_cg_bwedge_len, sili_cg_support_thk,
     &     fcg_z1, fcg_z2, fcg_z3, fcg_r1, fcg_r2, fcg_t, fcg_z,
     &     sili_endcap_config
*---  END namelist sili_cg_par -------------------------------------------------------------

      real fcg_sina, fcg_cosa, fcg_tana, fcg_alpha

      real rotoff, rshift
      character*3  disk_name(16), disk
      character*11 disk_delta
      data disk_name /'SW3','SE3','SW2','SE2','SW1','SE1',
     &'SW0','SE0','NW0','NE0','NW1','NE1','NW2','NE2','NW3','NE3'/
      real xmid(24), ymid(24), wphi(24), w_ang
      common /wedgesurvey/ xmid, ymid, wphi     ! This common block maps onto C struct

      character*8 object, astation              ! these vars have to do with the 
      integer arm, cage, station, sector, icage,   ! survey and alignments
     &        wphi_map(16), jarm, jcage, jstation, jwedge
      real xx, yy, delx(24), dely(24), xprime, yprime, xdisk, ydisk, 
     &     dtorad, dtr, disk_delx(16), disk_dely(16), disk_delphi(16),
     &     xdel,ydel
      logical lfirst, lfound, lexist
      character*10 adummy
      data delx, dely, lfirst, dtorad /24*0.0, 24*0.0, .false., 
     &                                 0.017453293/
      data wphi_map/4,8,3,7,2,6,1,5, 9,13,10,14,11,15,12,16/

*=========================================================================================================

      rewind( unit = itf_lun )
      read( itf_lun, nml = sili_endcap_par, err = 996 )
      rewind( unit = itf_lun )
      read( itf_lun, nml = sili_cg_par, err = 997 )
      write (6,*)' SVX_FVTX.F:: installing split-disk endcaps'
      
      deg      = 2*PI/360           ! To convert degrees into radians
      rad      = 360/(2*PI)         ! To convert radians into degrees
      pangle   = (360./wedges)      ! The total angle of each carbon panel

      rotoff   = 2.366              ! common rotation of support disk

      if (stagger.gt.0.0) then
        stag_ang(1) = 0.                      ! staggering angle in +phi direction
        stag_ang(2) = 0.9375                  ! 1x 360 / 96 / 4
        stag_ang(3) = 1.8750                  ! 2x
        stag_ang(4) = 2.8125                  ! 3x   stagger >0 PATTERN: 
      elseif(stagger.lt.0.0) then             ! Order: in SW cage, S to N (st.4 to st.1)
        stag_ang(1) = 0.9375 + rotoff         ! staggering angle in +phi direction, 
        stag_ang(2) = 1.8750 + rotoff         ! changed to match midplane crossing page
        stag_ang(3) = 0.0000 + rotoff         ! rotoff = 2.366 degrees           
        stag_ang(4) = 2.8125 + rotoff +0.6515 ! Note that the small disk is rotated a little further. 
      endif

c     ===========================================================
c     Defining FVTX half-cage mother volumes.
c     ===========================================================

c     SCMS/SCMN:  FVTX cage mother: a PCON.  Origin is that of SICG, shape is designed 
c     to have a ~0.1 cm buffer outside SLCC and to accomodate the small radius FVTX disks 
c     closest to the interaction region. -- A. Barron 7/14/2010

      nmed = sili_med_coldair
      !rinner = 1.1 ! xxx pick up real value hvh

      par(1) = 0.         ! This is the volume holding all of the South FVTX. The real
      par(2) = 360.       ! function of this volume is to stop geant searches of the
      par(3) = 3          ! volume tree higher up, caused by the use of 'MANY' in SInn below.

      par(4) = +sili_cg_z(1)  + sili_cg_thck
      par(5) = sili_cg_rinner - 0.1
      par(6) = fcg_r2 + 0.2

      par(7) = -(fcg_z2+fcg_z) + 0.1     ! link to physical cage
      par(8) = par(5)
      par(9) = par(6)

      par(10) = -(fcg_z1 + fcg_z) + 1.25   ! z1
      par(11) = par(5)
                routers = rinner + swedge_len 
      par(12) = routers + 1.5
      call gsvolu('SCMS','PCON', nmed, par, 12, ivol1)  ! SCMS:
      call GSPOS('SCMS',1,'SICG', 0.0, 0.0, 0.0, irotnull,'ONLY')

      irot = irot+1                                 ! SCMN
      call gsrotm(irot,90., 0., 90.,270., 180.,0.)  ! from scms: 180 about z, 180 around y
      call gsvolu('SCMN','PCON', nmed, par, 12, ivol1)  ! SCMN:
      call GSPOS('SCMN',1,'SICG', 0.0, 0.0, 0.0, irot,'ONLY')

      par(1) = 45.                         ! take a bite out of the 'unused' side
      par(2) = 270.                        ! so we can see the orientation
      par(3) = 3
      par(4) = -(fcg_z3 + 1.5 + fcg_z)     ! z3, 1.5 cm outside SLCC z to catch station 4
      par(5) = sili_cg_rinner - 0.1
      par(6) = fcg_r2 + 0.1 
      par(7) = -(fcg_z2 - 0.05 + fcg_z)    ! z2
      par(8) = sili_cg_rinner - 0.1
      par(9) = fcg_r2 + 0.1                ! 0.1 cm larger than outer SLCC r
      par(10) = -(fcg_z1 - 1.65 + fcg_z)   ! z1, accomodating Si disk width outside SLCC
      par(11) = sili_cg_rinner - 0.1       ! 0.1 cm smaller than Si disk mothers
      par(12) = routers +0.5

c     ===========================================================
c     Defining FVTX  carbon cages
c     ===========================================================

c     FVTX  cage: a PCON. What is given is the z,r of the 'outer 3 corners' 
c     of the volume, plus the thickness. Everything else is derived. HvH may 08
c
      fcg_alpha = atan2( fcg_r2-fcg_r1, fcg_z2-fcg_z1)
      fcg_sina = sin(fcg_alpha)
      fcg_cosa = cos(fcg_alpha)
      fcg_tana = tan(fcg_alpha)  

c     place cages in each cage mother - SLC1
      par(1) = 90.
      par(2) = 180.
      par(3) = 6
      par(4) = -fcg_z1                   ! z1 (small side)
      par(5) = fcg_r1
      par(6) = fcg_r1+fcg_t
      par(7) = par(4) - 0.35             ! z2
      par(8) = par(5)
      par(9) = par(6)
      par(10) = par(7) - 0.5*fcg_t       ! z3
      par(11) = par(8)
      par(12) = par(11) + 2.0*fcg_t
      par(13) = -fcg_z2                  ! z4
      par(14) = fcg_r2-2.0*fcg_t
      par(15) = fcg_r2
      par(16) = par(13)-0.5*fcg_t        ! z5
      par(17) = fcg_r2 - fcg_t
      par(18) = fcg_r2
      par(19) = -fcg_z3                  ! z6
      par(20) = par(17)
      par(21) = par(18)

      nmed = sili_med_carbon
      call gsvolu('SLC1','PCON',nmed,par,21,ivol1)  ! 180 rot about z
      irot = irot+1
      call gsrotm(irot,90.,180.,90.,270.,0.,0.)
      call GSPOS('SLC1',1,'SCMS', 0.0, 0.0, -fcg_z,
     & irotnull,'MANY')
      call GSPOS('SLC1',2,'SCMS', 0.0, 0.0, -fcg_z,
     & irot,'MANY')
      call GSPOS('SLC1',3,'SCMN', 0.0, 0.0, -fcg_z,
     & irotnull,'MANY')
      call GSPOS('SLC1',4,'SCMN', 0.0, 0.0, -fcg_z,
     & irot,'MANY')

      call gsatt('SLC1','SEEN',1)
      Call GSATT('SLC1','COLO', 6)

*---------------------- fvtx 'cables sheets' SNCC, SOCC into the N,S cages: -----------------------*

      par(1) = (sili_endcap_z(6)-sili_endcap_z(5)-1.0)/2.   ! between station 1 and station 2 
      par(2) = rinner + swedge_len+0.8                      ! a half-cone
      par(3) = par(2) + hdithk
      par(4) = rinner + bwedge_len+0.8 !+0.6
      par(5) = par(4) + hdithk
      par(6) = 100.0
      par(7) = 260.0
      nmed = sili_med_passive
      Call GSVOLU('SNCC','CONS',nmed,PAR,7,IVOL1)
                                                             !  Now a tapered cone for FVTX cables - 
      parb(1) = (sili_endcap_z(8)-sili_endcap_z(6)+.9)/2.    !  between station 2 and station 4 
      parb(2) = rinner + bwedge_len+0.7
      parb(3) = parb(2) + hdithk*2.
      parb(4) = rinner + bwedge_len+0.7
      parb(5) = parb(4) + hdithk*4.
      parb(6) = 100.0
      parb(7) = 260.0
      nmed = sili_med_passive
      Call GSVOLU('SOCC','CONS',nmed,parb,7,IVOL1)

      irot=irot+1
      irot1 = irot
      CALL GSROTM(irot1,90.,180.,90.,90.,180.,0.)

      irot=irot+1
      irot2 = irot
      CALL GSROTM(irot2,90.,0.,90.,270.,180.,0.)

      do i=1,4     ! N-S, E-W
        cag_name =             'SCMS'
        if (i.gt.2) cag_name = 'SCMN'
        if (mod(i,2).eq.1) then         ! East
          call GSPOS('SNCC', i, cag_name, 0., 0.,
     &      -sili_endcap_z(5)-par(1)+0.01, irot1, 'ONLY')
          call GSPOS('SOCC', i, cag_name, 0., 0.,
     &      -sili_endcap_z(6)-parb(1)+.9,  irot1, 'ONLY')
        else                            ! West
          call GSPOS('SNCC', i, cag_name, 0., 0.,
     &      -sili_endcap_z(5)-par(1)+0.01, irot2, 'ONLY')
          call GSPOS('SOCC', i, cag_name, 0., 0.,
     &      -sili_endcap_z(6)-parb(1)+.9,  irot2, 'ONLY')
        endif
      enddo

      call gsatt('SNCC','SEEN',1)     
      call GSATT('SNCC','COLO',7)    !  7 is light blue                 
      call gsatt('SOCC','SEEN',1)     
      call GSATT('SOCC','COLO',7)      
c     
C (1) This is the big wedge mother panel. Wedge mother volume contains the carbon back plane,
c     the HDI, the silicon sensor, and the 13 x 2 chips in mother volume SCHM
 
      wedge_thk=silthk+back_planthk+hdithk
      PAR(1) = bwedge_lowx/2        ! half length along x at -z
      PAR(2) = bwedge_highx/2       ! half length along x at +z
      PAR(3) = wedge_thk/2          ! half thickness (y)
      PAR(4) = bwedge_len/2         ! half length along z
      CALL GSVOLU( 'SIPB', 'TRD1 ', sili_med_coldair, PAR, 4, IVOL1)
c
c     Wedge Back Plane
c
      PAR(1) = back_lowx/2             ! This is wedge carbon back plane inside SIPB
      PAR(2) = back_highx/2
      PAR(3) = back_planthk/2
      PAR(4) = bwedge_len/2
      CALL GSVOLU( 'SICB', 'TRD1 ', sili_med_carbon, PAR, 4, IVOL1)
c
c      The HDI
c
      PAR(1) = bwedge_lowx/2             ! This is wedge HDI     inside SIPB
      PAR(2) = bwedge_highx/2
      PAR(3) = hdithk/2
      PAR(4) = bwedge_len/2
      CALL GSVOLU( 'HDIB', 'TRD1 ', sili_med_carbon, PAR, 4, IVOL1)      
      call gsatt ( 'HDIB', 'COLO', 3)    ! HDI is green
c     
c     Silicon Sensor from 6" wafer
c     
      PAR(1) = bsil_lowx/2               ! This is a silicon sensor in SIPB, including dead area
      PAR(2) = bsil_highx/2              ! 
      PAR(3) = silthk/2
      PAR(4) = bsil_len/2

      CALL GSVOLU( 'SISB', 'TRD1 ', sili_med_silipass, PAR, 4, IVOL1)
      call gsatt ( 'SISB','COLO',6)      ! silicon is magenta
c
c     Silicon sensor active volume
c     
      PAR_sisi_b(1) = bsic_lowx/2        ! This is a silicon sensor column in SISB
      PAR_sisi_b(2) = bsic_highx/2       !
      PAR_sisi_b(3) = silthk/2
      PAR_sisi_b(4) = bsic_len/2
      CALL GSVOLU( 'SISI', 'TRD1 ', sili_med_silicon,   ! big sensitive silicon
     &            PAR_sisi_b, 0, IVOL1)               ! 0 parameters - POSP later
      CALL GSATT( 'SISI', 'WORK', 1)     ! make volume sensitive
      call gsatt( 'SISI','COLO',6)       ! silicon is magenta

      if (sili_endcap_strip_on.eq.1) then
        CALL GSDVT( 'STRP', 'SISI', 0.0075, 3, sili_med_silicon, 255) ! divide a sensitive silicon into 75micron strips
      endif
      
c     Small Disk wedges   
C (1) This is the small wedge mother panel. Wedge mother volume contains the carbon back plane,
c     the HDI, the silicon sensor, and the 5 x 2 chips in mother volume SCHM
 
      PAR(1) = swedge_lowx/2             ! half length along x at -z
      PAR(2) = swedge_highx/2            ! half length along x at +z
      PAR(3) = wedge_thk/2               ! half thickness (y)
      PAR(4) = swedge_len/2              ! half length along z
      CALL GSVOLU( 'SIPS', 'TRD1 ', sili_med_coldair, PAR, 4, IVOL1)
c
c     Wedge Back Plane
c
      PAR(1) = sback_lowx/2             ! This is wedge carbon back plane inside
      PAR(2) = sback_highx/2            ! SIPS
      PAR(3) = back_planthk/2
      PAR(4) = swedge_len/2
      CALL GSVOLU( 'SICS', 'TRD1 ', sili_med_carbon, PAR, 4, IVOL1)
c
c     The HDI
c
      PAR(1) = swedge_lowx/2             ! This is wedge HDI     inside SIPS
      PAR(2) = swedge_highx/2
      PAR(3) = hdithk/2
      PAR(4) = swedge_len/2
      CALL GSVOLU( 'HDIS', 'TRD1 ', sili_med_carbon, PAR, 4, IVOL1)      
c     
c     Silicon Sensor from 6" wafer
c     
      PAR_s1(1) = ssil_lowx/2            ! This is a silicon sensor SIPS
      PAR_s1(2) = ssil_highx/2           ! 
      PAR_s1(3) = silthk/2
      PAR_s1(4) = ssil_len/2
      CALL GSVOLU('SISS','TRD1', sili_med_silipass, PAR_s1, 4, IVOL1) ! 0 parameters

c
c     ilicon sensor active volume
c 
      PAR_sisi_s(1) = ssic_lowx/2       ! This is a silicon sensor in SISS
      PAR_sisi_s(2) = ssic_highx/2       !
      PAR_sisi_s(3) = silthk/2
      PAR_sisi_s(4) = ssic_len/2
c
c     FPHX chips
c     FPHX big Mother volume first, contains 13 FPHX chips
c
      PAR(1) = chipwid/2                 ! This is one row of readout chips inside SIPB
      PAR(2) = silthk/2      
      PAR(3) = .96*13./2
      CALL GSVOLU( 'CHMR', 'BOX ', sili_med_silipass, PAR, 3, IVOL1)  ! 
c
c     FPHX small Mother volume first, contains 5 FPHX chips
c
      PAR(1) = chipwid/2                 ! This is one row of readout chips inside SIPS
      PAR(2) = silthk/2      
      PAR(3) = .96*5./2
      CALL GSVOLU( 'CHMS', 'BOX ', sili_med_silipass, PAR, 3, IVOL1)  !       
c           
c     Now we need four support plates for disks
c     Big half-disk support plate
      routerb = rinner + bwedge_len
      parb( 1) = -90 !- 2.68  ! 2.68 = rotation from the support disk to HDI @ midplane
      parb( 2) = 180
      parb( 3) = 2
      parb( 4) = -support_thk/2.
      parb( 5) = rinner             ! inner radius
      parb( 6) = routerb            ! outer radius for station 2,3,4
      parb( 7) = support_thk/2.
      parb( 8) = rinner
      parb( 9) = routerb
      CALL GSVOLU('SUPB','PCON',sili_med_honeycomb,parb, 9,ivolu)               

c     small support plate
      pars( 1) = -90 !- 2.029  ! 2.029 = rotation from the support disk to HDI @ midplane
      pars( 2) = 180
      pars( 3) = 2
      pars( 4) = -support_thk/2.
      pars( 5) = rinner             ! inner radius
      pars( 6) = routers            ! outer radius for station 1
      pars( 7) = support_thk/2.
      pars( 8) = rinner
      pars( 9) = routers
      CALL GSVOLU('SUPS','PCON',sili_med_honeycomb,pars, 9,ivolu)
c     
c     Disk mother volume to hold Wedges and support plate
c
      stationzthick=support_thk+wedge_thk*2.+hdithk*2.+silthk*2.+1.

      parb(1) = 253                   ! leave a 158-degree bite out of the 'unused' side
      parb(2) = 210                   ! (1)= starting angle, (2) dphi
      parb(3) = 2
      parb(4) = -stationzthick/2.     ! 
      parb(5) = rinner                ! inner radius
      parb(6) = routerb +.5           ! outer radius for station 2,3,4
      parb(7) =  stationzthick/2.     ! 
      parb(8) = rinner                ! inner radius
      parb(9) = routerb +.5           ! outer radius for station 2,3,4
      pars(1) = parb(1)               ! leave 110 out
      pars(2) = parb(2)
      pars(3) = parb(3)
      pars(4) = parb(4)               ! 
      pars(5) = parb(5)               ! inner radius
      pars(6) = routers +.5           ! outer radius for station 1
      pars(7) = parb(7)               ! 
      pars(8) = parb(8)               ! inner radius
      pars(9) = routers +.5           ! outer radius for station 1

c                         West         
c          +-------------\    /------------+
c          | si05 07 09   \  /   15 17 19  |
c          |            11    13           |
c  South   +---------------  --------------+   North
c  SCMS    |            12    14           |   SCMN
c          |   06 08 10   /  \   16 18 20  |
c          +-------------/    \------------+
c                         East          

      do idisk=5,20                      ! define 16 copies SI05 - SI20
        write (sil_name, '(''SI'',I2.2)') idisk
        if (idisk.ge.11 .and. idisk.le.14) then       ! small disks
          CALL GSVOLU(sil_name,'PCON',sili_med_coldair,pars, 9,ivolu)    ! small half-disks
        else                                               ! big disks
          CALL GSVOLU(sil_name,'PCON',sili_med_coldair,parb, 9,ivolu)    !large half-disks
        endif
      enddo

*=================   now build the big wedge  =======================
*     position backplane,HDI,sensor and chip mother volume in wedge
*
      irot=irot+1
      irot1 = irot
      irot=irot+1
      irot2 = irot
      sens_off = (bwedge_len-bsil_len)/2.

      dtip = 0.8998   ! from tip of the HDI to bottom of Silicon (from Hitec)
      dsi  = 4.4      ! radius of inner edge of the Silicon      (from Hitec)
      sens_off_small = ssil_len/2 + dtip - swedge_len/2  ! the Si starts at 0.8998 from the tip
      sens_off_big   = bsil_len/2 + dtip - bwedge_len/2  ! for both small and big
*     write (6,*)'Si offset small, big: ',sens_off_small,sens_off_big

      CALL GSROTM(irot1,93.75,0.,90.,90.,3.75,0.)
      CALL GSROTM(irot2,90.-3.75,0.,90.,90.,-3.75,0.)
      panthk=back_planthk
      CALL GSPOS('SICB',1,'SIPB',0.,                   ! carbon support
     &-wedge_thk/2.+panthk/2.,0.,irotnull,'ONLY')
      CALL GSPOS('HDIB',1,'SIPB',0.,                   ! HDI
     &-wedge_thk/2.+panthk+hdithk/2.,0.,irotnull,'ONLY')
      CALL GSPOS('SISB',1,'SIPB',0., -wedge_thk/2. +   ! big silicon
     &          panthk+hdithk+silthk/2.,sens_off_big,irotnull,'ONLY')
      irot=irot+1
      irot3 = irot
      irot=irot+1
      irot4 = irot
      CALL GSROTM(irot3,88.125,0.,90.,90.,1.875,180.)
      CALL GSROTM(irot4,91.875,0.,90.,90.,1.875,0.)
      CALL GSPOSP('SISI',1,'SISB',-bsic_posx,0.,bsic_posz,
     &            irot3,'ONLY',par_sisi_b,4)
      CALL GSPOSP('SISI',2,'SISB',bsic_posx,0.,bsic_posz,
     &            irot4,'ONLY',par_sisi_b,4)
      CALL GSPOS('CHMR',1,'SIPB', bchipoff,            ! readout chips
     &-wedge_thk/2.+panthk+hdithk+silthk/2.,sens_off,irot1,'ONLY')
             CALL GSPOS('CHMR',2,'SIPB', -bchipoff,           ! readout chips
     &-wedge_thk/2.+panthk+hdithk+silthk/2.,sens_off,irot2,'ONLY')

*=================   now build the small wedge  =======================
*     position backplane,HDI,sensor and chip mother volume in wedge
*
      CALL GSPOS('SICS',1,'SIPS',0.,
     &-wedge_thk/2.+panthk/2.,0.,irotnull,'ONLY')
      CALL GSPOS('HDIS',1,'SIPS',0.,
     &-wedge_thk/2.+panthk+hdithk/2.,0.,irotnull,'ONLY')

      CALL GSPOS('SISS',1,'SIPS',0.,
     &-wedge_thk/2.+panthk+hdithk+silthk/2.,sens_off_small,
     & irotnull,'ONLY')

      CALL GSPOSP('SISI',3,'SISS',-ssic_posx,0.,ssic_posz,
     &            irot3,'ONLY',par_sisi_s,4)
      CALL GSPOSP('SISI',4,'SISS',ssic_posx,0.,ssic_posz,
     &            irot4,'ONLY',par_sisi_s,4)

      CALL GSPOS('CHMS',1,'SIPS', schipoff,
     &-wedge_thk/2.+panthk+hdithk+silthk/2.,sens_off,irot1,'ONLY')
             CALL GSPOS('CHMS',2,'SIPS', -schipoff,
     &-wedge_thk/2.+panthk+hdithk+silthk/2.,sens_off,irot2,'ONLY')        
         
*=================   Position the detectors ======================c

      if (.not.lfirst) then
        lfirst = .true.
        write (6,10)  sili_use_survey
 10     format (/,' Use FVTX survey and alignment? sili_use_survey =',
     &     i3,/,
     &'    0 means ideal geometry', /,
     &'    nonzero n means read database records version |n|',/,
     &'    negative n means ALSO apply small corrections ',
     &     'from Milipede.',/)
        if (sili_use_survey.lt.0) then               ! get the deltas from Jin's ascii file
          write (6,13) sili_deltas_file
 13       format (' Using FVTX Millepede deltas file ',A40)
          inquire (file=sili_deltas_file, exist=lexist)
          if (lexist) then
            open(unit=42,file=sili_deltas_file,err=995)
            read(42,'(a100)') line
            write (6,'('' Alignment delta file comment: '',a100)') line
 14         close (unit=42)
          else
          write(6,*) 'FVTX: alignment file sili_deltas_file not found, 
     &    check phnx.par'
          endif
          open(unit=21,file='wedge_pos_out.csv')
        endif
      endif

      if (sili_use_survey.ne.0) then
        disk_delta = "delta"                        ! First fetch disk x,y offsets and phi rotations
        call fvtx_getsurvey(abs(sili_use_survey),disk_delta,5)    
        write (6,*) 'Disk shifts and rotations 1-16:'
        write (6,*)'xmid(...)=',xmid(1),xmid(3),'...',xmid(16)
        write (6,*)'ymid(...)=',ymid(1),ymid(3),'...',ymid(16)
        write (6,*)'wphi(...)=',wphi(1),wphi(3),'...',wphi(16)
        do i=1,8
          disk_delx(i)   = xmid(wphi_map(i))
          disk_dely(i)   = ymid(wphi_map(i))
          disk_delphi(i) = wphi(wphi_map(i))
        enddo
        do i=9,16
          disk_delx(i)   = -xmid(wphi_map(i))
          disk_dely(i)   = -ymid(wphi_map(i))
          disk_delphi(i) = -wphi(wphi_map(i))
        enddo
      endif


      do idisk = 5,20   ! 5-20                       ! Place (half-)disks: loop south to north, west - east
        write (sil_name, '(''SI'',I2.2)') idisk      ! Half-disk names SI05, SI06 ... SI20
        istz = idisk                                 ! south  5-12, north 13-20
        if (idisk.gt.12) istz = 25 - idisk           ! map north  13-20 into 5-12
        istz =  1 + (istz-5)/2                       ! transform 5-12 into 4-1. S:1122334444332211:N
        z_disk = sili_endcap_z(istz)                 ! z position from par file xxx NOTE that because of istz,
                                                     ! only the first 4 are used, and all cages are built the same.
                                                     ! In principle, there should have be 16 values. xxx 
        jstz = 5-istz                                ! istz is backwards (because of z order in phnx.par)
                                                     ! jstz = s:4433221111223344:N
        xdisk = 0                                    ! Default starting position
        ydisk = 0                                    !
        
        if (sili_use_survey.lt.0.and.lexist) then    ! get the delta's from Jin's ascii file
          open(unit=43,file=sili_deltas_file)
          lfound = .false.
          do while (.not.lfound)                        ! search for station deltas for this disk
            read(43,'(a40)',end=12,err=12) line
            read (line,*) object
            if (object.eq."station") then
              read(line,*,end=12,err=12) 
     &        object, arm, cage, station, xdisk, ydisk
              if ( (jstz-1.eq.station)  .and.          ! station match
     &             ((idisk-4)/9.eq.arm)       .and.    ! N-S arm match
     &             ((1-mod(idisk,2)).eq.cage) ) then   ! W-E cage match
                lfound = .true.                        ! break out of the search loop
              endif                                ! matching record in ascii file
            endif                                  ! 'station' record
          enddo                                    ! end find matching records
 12       close (unit=43)
cxx          write (21,23) cage_name,station,xdisk,ydisk          ! Write disk positions to the
cxx 23       format('* ', a4, '_',i1, ', ', f10.5, ', ', f10.5)   ! data base output file
        endif                                      ! if pick up station delta's

cxx        write (6,*) 'disk angle: ',sil_name, disk_delphi(idisk-4)

        cag_name =                  'SCMS'
        if (idisk.gt.12) cag_name = 'SCMN'

        if (mod(idisk,2).eq.1) then                ! odd disks go on the West side
          irot = irot + 1                          ! Disk into cage placement angles
          call gsrotm(irot,
     &               90.,  0.-stag_ang(istz)-disk_delphi(idisk-4),
     &               90., 90.-stag_ang(istz)-disk_delphi(idisk-4),0.,0.) ! 180deg + delta about z
        else
          irot = irot + 1                              ! Disk into cage placement angles
          call gsrotm(irot,
     &               90.,180.-stag_ang(istz)-disk_delphi(idisk-4),
     &               90.,270.-stag_ang(istz)-disk_delphi(idisk-4),0.,0.) ! 180deg + delta about z
        endif
        CALL GSPOS (sil_name, 1, cag_name,           ! Place this disk
     &   xdisk, ydisk, z_disk , irot, 'MANY')        ! z = -zdist+(idisk-5)*zsep+halfzc

                                                     ! Set some large/small switches:
        if (idisk.ge.11 .and. idisk.le.14) then      ! 11,12,13,14 = small wedge parameters:
           wedge_name = 'SIPS'                       ! Small wedge name
           support_name = 'SUPS'                     ! Small support panel
           beta = 5.42                               ! Angle from panel edge to small Si center
           dd = dsi - dtip + swedge_len/2            ! of the first wedge
           rshift = 0.37505
           ibgsm = -1       ! small wedges           ! Unfortunately, station 1 module staggering
        else                                         ! is different from stations 2,3,4, and this
           wedge_name = 'SIPB'                       ! affects the offsets and the rotations.
           support_name = 'SUPB'                     ! I use ibgsm = +-1 to do the switching. 
           beta = 6.072                              ! Panel edge to large Si center line
           dd = dsi - dtip + bwedge_len/2 
           rshift = 0.43905
           ibgsm = +1      ! small wedges            ! is different from stations 2,3,4, and this
        endif

        CALL GSPOS (support_name, idisk-4, sil_name, ! Place this support disk
     &          0., 0., 0., irotnull, 'MANY')   

c       Now fill them with wedges:
           
        disk = disk_name(idisk-4)                    ! copy from array member to single variable
                                                     ! names are like SW0, NE3 etc.
*--------+---------+---------+---------+---------+---------+---------+-+
*        1         2         3         4         5         6         7 7      
*--------0---------0---------0---------0---------0---------0---------0-2

        if (sili_use_survey.ne.0) then
          call fvtx_getsurvey(sili_use_survey,disk,3)    ! fetch wedge x,y, phi's for this disk (into xmid, ymid,wphi)
          if (idisk.eq.5) then
            write (6,
     &      '(''Wedge shifts and rotations (Hexagon) for disk:'',i3)') 
     $      idisk
            write (6,*)'xmid(...)=',xmid(1),xmid(3),'...',xmid(16)
            write (6,*)'ymid(...)=',ymid(1),ymid(3),'...',ymid(16)
            write (6,*)'wphi(...)=',wphi(1),wphi(3),'...',wphi(16)
          endif

          if (sili_use_survey.lt.0.and.lexist) then    ! get the delta's from Jin's ascii file
            open(unit=44,file=sili_deltas_file)

            do while (.TRUE.)                          ! search for deltas for this disk
              read(44,'(a40)',end=11,err=11) line
              read (line,*) object
              if (object.eq."wedge") then
                read(line,*,end=11,err=11) 
     &          object, arm, cage, station, sector, xx, yy
                if ( object.eq."wedge"  .and. 
     &             (jstz-1.eq.station)  .and.          ! station match
     &             ((idisk-4)/9.eq.arm)       .and.    ! N-S arm match
     &             ((1-mod(idisk,2)).eq.cage) ) then   ! W-E cage match

                  xx = xx * (-2*cage+1)
                  yy = yy * (-2*abs(arm-cage)+1)

                  delx(sector+1) = xx                  ! fill arrays with corrections
                  dely(sector+1) = yy                  ! 0-23 -> 1-24
                if (xx.gt.2.0) write (6,*) 'BIG xx seen 1',
     &          arm,cage,station,sector

                endif                                  ! matching record
              endif                                  ! 'wedge' record
            enddo                                    ! end find matching records
 11         close (unit=44)
            read(cage_name,'(3x,i1)') icage
                            astation(1:2) = '??'
            if (icage.eq.1) astation(1:2) = 'SW'                         ! 
            if (icage.eq.2) astation(1:2) = 'SE'                         ! 
            if (icage.eq.3) astation(1:2) = 'NW'                         ! 
            if (icage.eq.4) astation(1:2) = 'NE'
            if (idisk.le.12) write (astation(3:3),'(i1)') 3-(idisk-5 )/2                         ! 
            if (idisk.gt.12) write (astation(3:3),'(i1)')   (idisk-13)/2                         ! 
            write (21,'(''* '',a3,'' location,x,y,angle'')') astation
          endif                                      ! if pick up delta's
        endif                                        ! if use survey

        do i = 0, 23                                 ! Loop over wedges-1  (0-23)
          if      (mod(i+1,4).eq.1)then              ! make the z-offset 0.09 or 0.49 cm,
             cstep = 0.29 - ibgsm*0.20               ! depending on big or small module.
          else if (mod(i+1,4).eq.2)then              ! (ibigsm = +-1)
             cstep = 0.29 + ibgsm*0.20
          else if (mod(i+1,4).eq.3)then
             cstep = 0.29 + ibgsm*0.20
          else if (mod(i+1,4).eq.0)then
             cstep = 0.29 - ibgsm*0.20
          endif

          alpha = 360.*i/wedges     ! beta=0 means the centerline of Si #1 is on the support edge.
          gamma = alpha+beta

          if (sili_use_survey.ne.0) then
            gamma = wphi(i)         ! now use the survey angles
cxxx            write (6,*) cage_name, idisk,i,gamma
          endif
          if( mod(i,2).eq.1 ) then  ! odd wedge
            irot = irot+1
            call gsrotm(irot, 90.0, 0.0-gamma,
     &                 180.0, 0.0,
     &                90.0, 90.0-gamma)   
          else                      ! even wedge
            irot = irot+1
            call gsrotm(irot, 90.0, 180.0-gamma,
     &                 0.0, 0.0,
     &                90.0, 90.0-gamma)
          endif

          dtr = dtorad                                     ! N<->S rotation direction is opposite
          if (cage_name.eq.'SCM3'.or.cage_name.eq.'SCM4') dtr = -dtorad 
                                                           ! Rotate (delx,dely) by the staggering angle.
          xprime = delx(i+1)*cos(dtr*stag_ang(istz)) -     ! xxx NOTE i runs 0-23, NOT 1-24
     &             dely(i+1)*sin(dtr*stag_ang(istz))       !
          yprime = delx(i+1)*sin(dtr*stag_ang(istz)) +     ! xxx should this be inverted:
     &             dely(i+1)*cos(dtr*stag_ang(istz))       ! istz ->jstz   

cxx5          if      (cage_name .eq. 'SCM1') then    ! Transform absolute delta x,y
c            xprime =  xprime                      ! 
c            yprime =  yprime                      ! 
c          else if (cage_name .eq. 'SCM2') then    ! Cages start out identical, 
c            xprime = -xprime                      ! and then get rotated into 
c            yprime = -yprime                      ! position.
c          else if (cage_name .eq. 'SCM3') then    ! 
c            xprime =  xprime                      !  
c            yprime = -yprime                     
c          else if (cage_name .eq. 'SCM4') then
c            xprime = -xprime
c            yprime =  yprime                     
cxx5          endif

          if (sili_use_survey.ne.0) then
            w_ang  = wphi(i) * dtorad             ! xmid, ymid, wphi from the data base              
            xwedge = xmid(i) - rshift*sin(w_ang)  ! Correct for offset between SI center
            ywedge = ymid(i) - rshift*cos(w_ang)  ! and SIPS center.
            xwedge = xwedge + xprime              ! apply the corrections from the delta file
            ywedge = ywedge + yprime              ! 
          else
            xwedge = dd*sin ( gamma*deg)
            ywedge = dd*cos ( gamma*deg)
          endif
          zwedge = (support_thk/2. +wedge_thk/2.+cstep)*(-1)**i


          if (sili_use_survey.lt.0) then
            write (21,21) i,xwedge,ywedge,gamma
 21         format(i2,','f11.7,',',f11.7,', ',f10.5)
          endif

          CALL GSPOS (wedge_name , i+1, sil_name ,
     &         xwedge, ywedge, zwedge, irot, 'ONLY')       ! was MANY

        enddo                                              ! loop over 24 modules
      enddo                                                ! loop over all half-disks

*================= make disks part of set SVX    ====================*
*
*    namesw    = HALL, SIEN, SICG, SCMx, DUMM, SIzz, SIPu, SISv, SISI, 
*                where x=N-S, yy=1-4, zz=05-12, u = B-S, v=B-S
c*    namesw    = HALL, SIEN, SICG, SCMx, SIzz, SIPu, SISv, SISI, 
c*                where x=N-S, yy=1-4, zz=05-12, u = B-S, v=B-S
*
c                         West          
c          +-------------\    /------------+
c          | si05 07 09   \  /   15 17 19  |
c          |            11    13           |
c  South   +---------------  --------------+   North
c  SCMS    |            12    14           |   SCMN
c          |   06 08 10   /  \   16 18 20  |
c          +-------------/    \------------+
c                         East          

      do half_cage=1,4    !
        namesw(4) =                       'SCMN'
        if ( half_cage.le.2 ) namesw(4) = 'SCMS'
        do jdisk = 0,6,2  ! 0 2 4 6
          idisk = ((half_cage-1)/2)*8+5 + 
     &            mod(half_cage-1,2) + jdisk

          write (namesw(5),'(''DUMM'')')            ! SCM1-4 replaced by dummy placeholder
          write (namesw(6),'(''SI'',I2.2)')  idisk  ! so the offline code can remain 
          namesw(7) = 'SIPB'                        ! the same
          namesw(8) = 'SISB'
          if (idisk.ge.11 .and. idisk.le. 14) then
            namesw(7) = 'SIPS'
            namesw(8) = 'SISS'
          endif
          namesw(9) = 'SISI'
cxx         write (6,*)"*** FVTX NAMESW *** ",(namesw(i),' ',i=1,9)
         call gsdet (set_id, namesw(6), 9, namesw, nbitsv, idtype, 
     &                nwpa, nwsa, iset, idet)
         call gsdeth(set_id, namesw(6), nhh,inrNMSH,inrNBITSH,
     &                inrORIG,inrFACT)
       enddo
      enddo

*----  Hide some of the volumes, and set colors ----------------------*

      do idisk = 5,20          !  SI05, SI06 ... SI20
        write (sil_name, '(''SI'',I2.2)') idisk
        call gsatt(sil_name, 'SEEN', 0) !
        call gsatt(sil_name, 'COLO', 1) !
      enddo

      CALL GSATT( 'SIPB', 'SEEN ', 0)    ! big wedges in big stations      
      CALL GSATT( 'SIPS', 'SEEN ', 0)    ! small  ''     small   ''
      CALL GSATT( 'SICB', 'SEEN ', 1)    ! big carbon backplate
      CALL GSATT( 'HDIB', 'SEEN ', 1)    ! big HDI
      CALL GSATT( 'SUPS', 'SEEN ', 1)    ! small support plate
      CALL GSATT( 'SUPB', 'SEEN ', 1)    ! BIG support plate      
      CALL GSATT( 'SISB', 'SEEN ', 1)    ! Big silicon
      CALL GSATT( 'SICS', 'SEEN ', 1)    ! small carbon backplate
      CALL GSATT( 'HDIS', 'SEEN ', 1)    ! small HDI
      CALL GSATT( 'SISS', 'SEEN ', 1)    ! small silicon 
      CALL GSATT( 'CHMR', 'SEEN ', 1)    ! big readout chips
      CALL GSATT( 'CHMS', 'SEEN ', 1)    ! small readout chips 
                                         ! Add color to individual pieces
      CALL GSATT( 'SICB', 'COLO', 1)     !     1=black 2=red    5=yellow    8=white
      CALL GSATT( 'SICS', 'COLO', 1)     !     6=magenta     
      CALL GSATT( 'SUPB', 'COLO', 2)     !     7=lightblue
      CALL GSATT( 'SUPS', 'COLO', 2)     !     8=white
      call GSATT( 'HDIS', 'COLO', 3)     ! HDI is green
      call GSATT( 'SISS', 'COLO', 6)     ! silicon is magenta
      call GSATT( 'SISI', 'COLO', 6)     ! silicon is magenta
      call GSATT( 'CHMR', 'COLO', 4)     ! readout chips are blue
      call GSATT( 'CHMS', 'COLO', 4)     ! readout chips are blue
      if (sili_endcap_strip_on.eq.1) then 
        CALL GSATT( 'STRP', 'SEEN ', 0)    ! strips
        call GSATT( 'STRP', 'COLO', 7)     ! readout chips are cyan
      endif

      write (6,'(''...FVTX finished'',/)')
      return            ! from subroutine svx_fvtx

 995  stop 'FVTX - cannot find (Millepede) deltas file.'
 996  stop 'FVTX - read error in sili_fvtx_par segment.'
 997  stop 'FVTX - read error in sili_cg_par segment.'
      end               ! end of subroutine svx_fvtx
 
*=============================================================================c
