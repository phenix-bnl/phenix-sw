c $Id: inr.f,v 1.8 2008/05/21 08:21:57 hpereira Exp $
C     File name: inr.f
C     ----------------

C     Original author: Shaheen Tonse (LLNL)
C     Creation date: March 18, 1993

C     Purpose: Set up the Silicon Vertex Detector (VTX)

C     Revision History:
c     """"""""""""""""
C     C.F. Maguire       June 28, 2001 Convert to use for PHENIX upgrades group

c     V. L. Rykov        26-Aug-2003:
c              1) The vertex detector encloser ("envelope", cooling bag)
c                 is introduced.
c              2) For the barrel VTX, "plain" Si cylinders are replaced with
c                 something more realistic (ladders and sensors).
c              3) Set of hit components extended with the "wish list", but,
c                 for the time being, only 8 the first components are stored.
c              4) The gains for position resolution set for resolution 0.1 mkm.
c     V. L. Rykov        03-Sep-2003:
c              Using the full (extended) set for hit components.

c     V. L. Rykov        19-Feb-2004:
c              inrORIG & inrFACT adjusted for geting rid of the bad diagnostic
c              due to endcap, which still is not splitted into sensors.


c The tree structure is:
c                                      HALL
c                                       |
c                                      SIEN (VTX Envelope/Cage)
c                                       |
c                                      SICG (VTX cage inner surface)
c                                       |
c   ------------------------------------------------------------------------
C   |       |          |       |                           |    |    |    |
c SI01(1) SI01(2)... SI02(1) SI02(2) ... (B-ladders)      SInn ... (EC-layers)
C   |       |          |
c  ------ ------    ------------------
c  | |... | |...    |     |                  
c                SISN(1) SISN(2) ... (B-sensors)

      SUBROUTINE INR(FULL,NH)

        Implicit none

C---  Formal Argument Declarations
C     ----------------------------
c...  Input (?):
      character*4 full            ! set before call in gugeom
C...  Output: number of components of a hit
      integer*4   nh              ! set before call in gugeom

C---  External Functions
C     ------------------

C     ================================================================
C---  Global Declarations
C     ================================================================
#include "guphnx.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "gcflag.inc"
#include "gugeom.inc"

C  need to access zebra to write parameters to FZOUT file

#include "fstore.inc"
#include "sublink.inc"
#include "fpdlink.inc"

C     ================================================================
C---  Local declarations of the input data from phnx.par file
C     ================================================================
c---  VTX Envelope/Cage parameters: Volumes SIEN(outer)/SICG(inner)
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      Real    sili_cg_rmn    /2.2/    ! Inner cage radius, cm
      Real    sili_cg_thck   /0.5/    ! Cage wall thickness, cm
      Real    sili_cg_inthck /0.2/    ! Thickness of the beam pipe ins., cm
      Real    sili_cg_tempc  /0.0/    ! Temperature inside the Cage, deg. C
      Integer sili_cg_npcon  /6/      ! Number of corners for SIEN's PCON
      Real    sili_cg_z(10)  /-40.0,-25.6,-13.0 ! z-pos. of the Cage corners:
     *     ,13.0,25.6,40.0,4*0./                ! MUST: z(1)<z(2)<z(3)<...
      Real    sili_cg_rmx(10)/ 3.0, 35.0, 18.0
     *     ,18.0,35.0, 3.0,4*0./      ! Outer SIEN radii at the corners, cm
      Real    sili_cg_xdisp  /0.0/    ! x-displacement of SIEN in HALL, cm
      Real    sili_cg_ydisp  /0.0/    ! y-displacement of SIEN in HALL, cm
      Real    sili_cg_zdisp  /0.0/    ! z-displacement of SIEN in HALL, cm

      namelist /sili_cg_par/ sili_cg_npcon,sili_cg_z
     $     ,sili_cg_rmn,sili_cg_rmx,sili_cg_thck,sili_cg_inthck
     $     ,sili_cg_xdisp,sili_cg_ydisp,sili_cg_zdisp
     $     ,sili_cg_tempc
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

c---  VTX Barrel parameters
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      Integer sili_br_nlayers /4/                    ! Number of barrel layers

      Real    sili_br_snhalfx(20) /.696,19*1.71555/  ! Si sensor width, cm
      Real    sili_br_snhalfy(20) /0.01,19*0.02/     ! Si sensor thickness, cm
      Real    sili_br_snhalfz(20) /2.836,19*3.2291/  ! Si sensor length, cm
      Real    sili_br_x0add(20)   /20*0.01/    ! Passive material thickness
c                                              ! in the ladders added on top
c                                              ! of Si sensor, RadLength X0
      Real    sili_br_snzgap(20)  /20*0./      ! Sensor z-gap (if >=0) or
c                                              ! sensor z-overlap (if <0), cm
      Real    sili_br_tilt(20)    /10.0,6.0    ! Ladder tilt in layers, deg.
     *                           ,5.5,5.0
     *                           ,16*0./
      Integer sili_br_nsn(20)     /4,19*5/       ! Number of Si sensors/ladder
      Real    sili_br_r(20)   /2.5,6.,8.,10.,16*20./ ! Radial positions ...
c                                                    ! (read carefully!!!)
c                                                    ! of the center line,
c                                                    ! x=y=0 (loca Si sensor
c                                                    ! coordinates), of the Si
c                                                    ! sensors in the layers
      Real    sili_br_z(20)      /20*0./ ! Z-pos. of the ladder centers in SICG
      Real    sili_br_dphi(20)   /29.40,29.40  ! Azim. spacing of ladders, deg.
     *                           ,22.87,18.71
     *                           ,16*0./
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
c     ======================================================================
c     The parameters between === are for the temporary fake support emulation
c     with "plain" discs introduced for accomodating the request of
c     some people. You are welcome to spread this brand new "artificial
c     butter" over your piece of bread. Just, please, do not eat it! - VR
      Integer sili_br_nspring     /2/          ! Number of rings
      Real    sili_br_spz(20) /-17.,17.,18*0./ ! Z-positions, cm
      Real    sili_br_sprin(20)  /20*2.4/      ! Inner radii, cm
      Real    sili_br_sprout(20) /20*10./      ! Outer radii, cm
      Real    sili_br_spthck(20) /20*0.25/     ! Half-hickness, cm
c     ======================================================================
      namelist /sili_br_par/ sili_br_nlayers,sili_br_r,sili_br_z
     $     ,sili_br_nsn,sili_br_snhalfx,sili_br_snhalfy
     $     ,sili_br_snhalfz,sili_br_x0add,sili_br_snzgap
     $     ,sili_br_nsec,sili_br_nlad,sili_br_phic,sili_br_dphi
     $     ,sili_br_tilt
     $     ,sili_br_nspring,sili_br_spz,sili_br_sprin
     $     ,sili_br_sprout,sili_br_spthck
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c&&&&&&&&&&&&&&&&&& To take care for the Endcap programer &&&&&&&&&&&&&&
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

c---  VTX Endcap parameters
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      integer sili_sideLayers       /20/
     
      real sili_zCenter_side(20)         /20*3/
      real sili_phi1_side(20)         /20*3/
      real sili_dph_side(20)         /20*3/
      integer sili_npdv_side(20)         /20*3/
      integer sili_nz_side(20)         /20*3/
      real sili_z1_side(20)         /20*3/
      real sili_rmin1_side(20)         /20*3/
      real sili_rmax1_side(20)         /20*3/
      real sili_z2_side(20)         /20*3/
      real sili_rmin2_side(20)         /20*3/
      real sili_rmax2_side(20)         /20*3/

      namelist /siliside_par/
     $     sili_sideLayers,
     $     sili_zCenter_side,
     $     sili_phi1_side, sili_dph_side,
     $     sili_npdv_side, sili_nz_side,
     $     sili_z1_side, sili_rmin1_side, sili_rmax1_side,
     $     sili_z2_side, sili_rmin2_side, sili_rmax2_side
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

C     ================================================================
C---  Local definitions
C     ================================================================
c     Input filename
      character*4  set_id  /'INR '/        ! Detector/hit set ID
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
c     VTX cage volume names
      character*4 siliEnvelope/'SIEN'/  ! Envelope (outer cage surface)
      character*4 siliCage    /'SICG'/  ! Cage inner surface
      character*4 siliInCage  /'SIIC'/  ! Thermal insulation around beam-pipe
      character*4 siliSupport /'SISP'/  ! Fake barrel support


c     VTX layer names (for barrel, names of ladders in a layer)
      character*4 siliNames(20) /'SI01', 'SI02', 'SI03', 'SI04',
     &                           'SI05', 'SI06', 'SI07', 'SI08',
     &                           'SI09', 'SI10', 'SI11', 'SI12',
     &                           'SI13', 'SI14', 'SI15', 'SI16',
     &                           'SI17', 'SI18', 'SI19', 'SI20'/

      character*4 siliBrSensor  /'SISN'/  ! Barrel sensor name
      character*4 siliBrPassive /'SIPV'/  ! Barrel passive material name
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

c   The following are used in GSDET: namesv(4) is to be replaced with
c   'SI01', 'SI02', ... at the running time

      Integer     nwpa      /500/                    ! Init. size of HITS banks
      Integer     nwsa      /500/                    ! Init. size of DIGI banks
      Integer     idtype    /2001/                   ! User def. detector type

      Integer     nbrv      /5/                      ! Num. of br. vol. desc.
      Integer     necv      /6/                      ! Num. of ec  vol. desc.
      Integer     nv        /6/                      ! max(nbrv,necv)
      Integer     nbitsv(6) /6*8/                    ! Bits to pack vol. copy #
      Character*4 namesv(6) /'HALL','SIEN','SICG'
     *     ,'SIxx',2*'SISN'/                         ! Volume names
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

c   The following are used in GSDETH
c   """"""""""""""""""""""""""""""""
c   Hit parameters will be global position(3), energy loss, time of flight,
c   particle type and 
c   entry momentum(3), local in & out positions(6),

      Integer     nhh         /15/                  ! Number of hit components
      integer*4 inrNBITSH(15) /15*32/               ! Bits for packing the hits
c   Hit component names
      character*4 inrNMSH(15) /'POSX','POSY','POSZ' ! Global positions
     &     ,'DELE','TOFL'                           ! Energy loss & TOF
     &     ,'P_ID','MOMX', 'MOMY', 'MOMZ'           ! Particle ID & Entry mom.
     &     ,'XILC','YILC','ZILC','XOLC','YOLC','ZOLC'/ ! Local entry & exit


c     Default setting of offsets and gains
      REAL inrORIG(15) /3*1000.,3*0.,3*1000.,6*1000./       ! offsets
      REAL inrFACT(15) /3*100000.,1.E7,1.e12,1.0,3*100000.
     &     ,6*100000./                                      ! gains

c     The above gains give
c              - 0.1 keV energy deposition resolution
c              - 0.0001 mm position resolution
c              - 0.01 MeV/c momentum resolution
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

c     Rotation matrices
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      Integer irot_cage        ! Cage (SIEN) rotation matrix #

c     /*--- null rotation ---*/
*      real nul_rot(6) /90.0,0.0,90.0,90.0,0.0,0.0/
*      real rot1, rot2, rot3, rot4, rot5, rot6, x_ang, y_ang
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

C     ================================================================
C---  Local definitions of materials/media
C     ================================================================
      Integer sili_med_silicon /10/     ! Sensitive silicon nmed
      Integer sili_med_passive /26/     ! Ladder passive nmed (Al)
      Integer sili_med_cg      /1102/   ! Cage (SIEN-SICG) nmat/nmed (Rohacell)
      Integer sili_med_coldair /1019/   ! Gas inside the SICG (cold air)
      Integer sili_med_gfrp    /1490/   ! GFRP for the fake support

c     Material for the cage, volumes SIEN-SICG
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
C     Copied from pisa200/src/ver/vermatdef.f, Rev 1.2
C     Rohacell(H11-C8-N1-O12) Mixture Parameters for the vertex detector:
      REAL AROHA(4)/  1.008 , 12.01  , 14.008 , 16.  / ! A for Rohacell
      REAL ZROHA(4)/  1.    ,  6.    ,  7.    ,  8.  / ! Z for Rohacell
      REAL WROHA(4)/ 11.    ,  8.    ,  1.    ,  2.  / ! Weights for Rohacell
c     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

C     ================================================================
c---  Local work variables (counters, etc)
C     ================================================================
      CHARACTER*20 NAMTMED
      INTEGER NMAT,ISVOL,IFIELD,NWBUF
      REAL FIELDM,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN
     &     ,WMAT(3),UBUF(10),ANUMB,ZNUMB,DENS,RADL,ABSL
      CHARACTER*80 CHFORM
      character*4  v_m_name,v_i_name

      Integer iLayer,iPoint,icnt,nr,npar,nmed,ivolu
     &     ,iset,idet,iod
      Integer nladd, iladd, isec
      Real    dim_sili(20),philadd,dphid,dphir,dphit,sgn
     &     ,phi1,phi2,dthck,phirotladd,rladd,xladd,yladd
     &     ,ladd_halfx,ladd_halfthck,ladd_halfz,pasv_halfy
     &     ,sen_y(2)
          
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun          
          
C     ================================================================
C     ================================================================

C    Executable code
C    ===============

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c BEGIN
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c       Read the geometery file segments


      
      write( *,* ) 'inr - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = sili_cg_par, err = 998 )
      read( itf_lun, nml = sili_br_par, err = 997 )
      read( itf_lun, nml = siliside_par, err = 996 )


C  only book volumes if input parameters are OK

      IF(CVOLU_OPT(1,3).EQ.'FULL'.OR.CVOLU_OPT(1,3).EQ.'VOLS')THEN

        NH = nhh             ! Number of hit components to NH output parameter
        nv = max(nbrv, necv) ! Number of volume descriptors

C...  Create outer surface of the cage, SIEN
        dim_sili(1)  =   0.
        dim_sili(2)  = 360.
        dim_sili(3)  =   sili_cg_npcon
        npar = 3
        Do icnt = 1, sili_cg_npcon
          npar = npar + 1
          dim_sili(npar)  = sili_cg_z(icnt)
          npar = npar + 1
          dim_sili(npar)  = sili_cg_rmn
          npar = npar + 1
          dim_sili(npar)  = sili_cg_rmx(icnt)
        Enddo
        nwbuf   = 1
C**************************************************************************
C     Define mixture ROHACELL for the SIEN
C     Copied from pisa200/src/ver/vermatdef.f, Rev 1.2

        NMAT    = sili_med_cg   ! Rohacell
        CALL GSMIXT (NMAT,' ROHACELL$',AROHA,ZROHA,0.075,-4,WROHA)

C     Tracking media # sili_cg_wall - Rohacell

        NMED    = sili_med_cg   ! Rohacell
        ISVOL   = 0             ! Not  sensitive
        IFIELD  = 1             ! Magnetic field
        FIELDM  = 10.0          ! max field
        TMAXFD  = 45.0          ! maximum angle due to field (one step) in deg
        DMAXMS  = 0.2           ! max disp. due to mulsct. in one step (cm)
        DEEMAX  = 0.1           ! max fractional energy loss in one step
        EPSIL   = .001          ! tracking precision (cm)
        STMIN   = 0.5           ! min step due to e loss or mulsct. (cm)
        UBUF(1) = 0.            ! tracking stop switch
        CALL GSTMED(NMED,'Rohacell$',NMAT,ISVOL,IFIELD
     1       ,FIELDM,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN,UBUF,NWBUF)
C**************************************************************************
        v_m_name = 'HALL'
        v_i_name = siliEnvelope
        nr   = 1
        call gsvolu(v_i_name,'PCON',nmed,dim_sili,npar,ivolu)
        call gsatt(v_i_name,'SEEN',1)
        call gspos(v_i_name,nr,v_m_name
     *       ,sili_cg_xdisp,sili_cg_ydisp,sili_cg_zdisp
     *       ,irotnull,'ONLY')
        
C...  Create inner volume of the cage SICG
        npar = 3
        sgn  = 1.
        If(sili_cg_z(1).GT.sili_cg_z(sili_cg_npcon)) sgn = -1.
        phi1 = 1.570796327
        Do icnt = 1, sili_cg_npcon
          npar = npar + 3
          If(icnt .LT. sili_cg_npcon)        THEN
            phi2 = ATAN2(sili_cg_rmx(icnt+1)-sili_cg_rmx(icnt),
     *            sili_cg_z(icnt+1)-sili_cg_z(icnt))
          Else
            phi2 = -1.570796327
          Endif
          dphir = 0.5*(phi1 + phi2)
          dthck = sili_cg_thck/COS(0.5*(phi1-phi2))
          dim_sili(npar-2) = sili_cg_z(icnt)   + dthck*SIN(dphir)
          dim_sili(npar-1)  = sili_cg_rmn ! Inner radius
          dim_sili(npar)   = sili_cg_rmx(icnt) - dthck*COS(dphir)
          If(dim_sili(npar) .LT. sili_cg_rmn)          THEN
            If(icnt .EQ. 1)                            THEN
              dphir = phi2
            Elseif(icnt .EQ. sili_cg_npcon)            THEN
              dphir = phi1
            Endif
            dim_sili(npar)   = sili_cg_rmn
            dim_sili(npar-2) = (sili_cg_rmx(icnt)-sili_cg_rmn)/
     *           TAN(dphir)
            dim_sili(npar-2) = sili_cg_z(icnt) +
     *           sgn*(sili_cg_thck/SIN(dphir) - dim_sili(npar-2))
          Endif
          phi1 = phi2
        Enddo
C**************************************************************************
c     Define material 'AIRCOLD' at 0C
        ubuf(1) = sili_cg_tempc
        nmat = sili_med_coldair
        anumb = 14.61
        znumb = 7.3
        dens  = 1.293e-3*273./(sili_cg_tempc+273.)
        radl  = 0.283e5*(sili_cg_tempc+273.)/273
        absl  = 0.696e5*(sili_cg_tempc+273.)/273.
        CALL GSMATE(nmat,'AIRCOLD',anumb,znumb,dens
     1       ,radl,absl,ubuf,nwbuf)
        nmed = sili_med_coldair ! Air at temperature sili_cg_tempc C
        isvol = 0               ! Not sensitive
        ifield = 1              ! magnetic field
        fieldm = 10.            ! max field, kGs
        tmaxfd = 0.2            ! max angle due to field (one step) in degrees
        dmaxms = 0.1            ! max disp. due to mulsct. in one step, cm
        deemax = 0.01           ! max fractional energy loss in one step
        epsil  = 0.001          ! tracking precision, cm
        stmin  = 0.1            ! min step due to e-loss or multsct., cm
        ubuf(1) = 0.            ! tracking stop switch
        CALL GSTMED(nmed,'AIRCOLD$',nmat,isvol,ifield,fieldm
     *       ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
C**************************************************************************
        v_i_name = siliCage
        v_m_name = siliEnvelope
        nr   = 1
        call gsvolu(v_i_name,'PCON',nmed,dim_sili,npar,ivolu)
        call gsatt(v_i_name,'SEEN',1)
        call gspos(v_i_name,nr,v_m_name,0.,0.,0.,irotnull,'ONLY')

c     Create and position thermal insulation of the beam-pipe made of Rohacell
        v_i_name = siliInCage
        v_m_name = siliEnvelope
        nmed     = sili_med_cg   ! Rohacell
        nr       = 1
        npar     = 3
        dim_sili(1) = sili_cg_rmn - sili_cg_inthck
        dim_sili(2) = sili_cg_rmn
        dim_sili(3) = 0.5*(sili_cg_z(sili_cg_npcon)-sili_cg_z(1))
        dim_sili(4) = 0.5*(sili_cg_z(sili_cg_npcon)+sili_cg_z(1))
        call gsvolu(v_i_name,'TUBE',nmed,dim_sili,npar,ivolu)
        call gsatt(v_i_name,'SEEN',1)
        call gspos(v_i_name,nr,v_m_name,0.,0.,dim_sili(4)
     *       ,irotnull,'ONLY')

c     ======================================================================
c     The parameters between === are for the temporary fake support emulation
c     with "plain" discs introduced for accomodating the request of
c     some people. You are welcome to spread this brand new "artificial
c     butter" over your piece of bread. Just, please, do not eat it! - VR

c     Define material/media 'GFRP' (slightly modified G10 from ../itr/pc1gem.f)
        nmat  = sili_med_gfrp
        anumb = 18.14
        znumb = 9.065
        dens  = 1.68
        radl  = 25.
        absl  = 56.7
        CALL GSMATE(nmat,'GFRP',anumb,znumb,dens
     1       ,radl,absl,ubuf,nwbuf)
        nmed   = sili_med_gfrp
        isvol  = 0              ! Not sensitive
        ifield = 1              ! magnetic field
        fieldm = 10.            ! max field, kGs
        tmaxfd = 0.2            ! max angle due to field (one step) in degrees
        dmaxms = 0.1            ! max disp. due to mulsct. in one step, cm
        deemax = 0.01           ! max fractional energy loss in one step
        epsil  = 0.001          ! tracking precision, cm
        stmin  = 0.1            ! min step due to e-loss or multsct., cm
        ubuf(1) = 0.            ! tracking stop switch
        CALL GSTMED(nmed,'GFRP$',nmat,isvol,ifield,fieldm
     *       ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

        v_i_name = siliSupport
        v_m_name = siliCage
        call gsvolu(v_i_name,'TUBE',nmed,dim_sili,0,ivolu)
        call gsatt(v_i_name,'SEEN',1)
        npar = 3
        Do nr = 1, sili_br_nspring
          dim_sili(1) = sili_br_sprin(nr)
          dim_sili(2) = sili_br_sprout(nr)
          dim_sili(3) = sili_br_spthck(nr)
          call gsposp(v_i_name,nr,v_m_name,0.,0.,sili_br_spz(nr)
     *         ,irotnull,'ONLY',dim_sili,npar)
        Enddo
c     ======================================================================

c...  Build barrel VTX
c     Get passive material radiation length
        nmed = sili_med_passive
        CALL GFTMED(nmed,NAMTMED,nmat,isvol,ifield,fieldm
     *       ,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
        CALL GFMATE(nmat,NAMTMED,anumb,znumb,dens,radl,absl
     *                   ,ubuf,nwbuf)

c     Create a layer of passive material behind the sensor
        pasv_halfy = 0.5*sili_br_x0add(iLayer)*radl      ! Al Thick/2
        nmed = sili_med_passive
        npar = 0
        call gsvolu(siliBrPassive,'BOX ',nmed,dim_sili,npar,ivolu)
        call GSATT(siliBrPassive,'SEEN',1)
        call GSATT(siliBrPassive,'COLO',3)

c     Create Si sensors SISN
        nmed = sili_med_silicon
        npar = 0
        call gsvolu(siliBrSensor,'BOX ',nmed,dim_sili,npar,ivolu)
        call GSATT(siliBrSensor,'SEEN',1)
        call GSATT(siliBrSensor,'COLO',2)
        CALL GSATT(siliBrSensor,'WORK',1) ! Make volume sensitive

c---  Cycle over the layers
        Do iLayer = 1, sili_br_nlayers

c...  Define ladder volume, SInn
          v_m_name = siliNames(iLayer)
c     Define ladder dimensions
          ladd_halfx    = sili_br_snhalfx(iLayer)                     ! Width/2
          dim_sili(1)   = ladd_halfx
          ladd_halfthck = pasv_halfy + sili_br_snhalfy(iLayer)  ! Thick/2
          If(sili_br_snzgap(iLayer) .LT. 0.)          Then
            ladd_halfthck = 2.*ladd_halfthck
          Endif
          dim_sili(2)   = ladd_halfthck
          ladd_halfz    = sili_br_nsn(iLayer)*sili_br_snhalfz(iLayer)
     *     + 0.5*sili_br_snzgap(iLayer)*(sili_br_nsn(iLayer)-1) ! Length/2
          dim_sili(3)   = ladd_halfz

c     Create a ladder volume SInn made of cold air
          npar = 3
          nmed = sili_med_coldair
          call gsvolu(v_m_name,'BOX ',nmed,dim_sili,npar,ivolu)
          call GSATT(v_m_name,'SEEN',0)
          call GSATT(v_m_name,'COLO',4)
         
c     Position sensors SISN and passive layers in the ladder SInn
          dim_sili(2) = sili_br_snhalfz(iLayer) - ladd_halfz
          npar = 3
          dim_sili(3) = sili_br_snhalfx(iLayer)
          dim_sili(5) = sili_br_snhalfz(iLayer)
          sen_y(1)   = sili_br_snhalfy(iLayer) - ladd_halfthck
          If(sili_br_snzgap(iLayer) .GE. 0.)          Then
            sen_y(2) = sen_y(1)
          Else
            sen_y(2) = sen_y(1) +
     *            2.*(sili_br_snhalfy(iLayer) + pasv_halfy) 
          Endif
          Do nr = 1, sili_br_nsn(iLayer)
c     Sensor
            v_i_name = siliBrSensor
            dim_sili(1) = sen_y(mod(nr-1,2)+1)
            dim_sili(4) = sili_br_snhalfy(iLayer)
            call gsposp(v_i_name,nr,v_m_name
     *            ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *            ,dim_sili(3),npar)
c     Passive layer
            v_i_name = siliBrPassive
            dim_sili(1) = dim_sili(1) + dim_sili(4) + pasv_halfy
            dim_sili(4) = pasv_halfy
            call gsposp(v_i_name,nr,v_m_name
     *            ,0.,dim_sili(1),dim_sili(2),irotnull,'ONLY'
     *            ,dim_sili(3),npar)
            dim_sili(2) = dim_sili(2) + 2.*sili_br_snhalfz(iLayer)
     *           + sili_br_snzgap(iLayer)
          Enddo

c---  Place ladders SInn in the cage SICG
          rladd = sili_br_r(iLayer)
          dphit = DEGRAD*sili_br_tilt(iLayer)
          xladd = ladd_halfthck - sili_br_snhalfy(iLayer)
          yladd = rladd + xladd*COS(dphit)
          xladd = xladd*SIN(dphit)
          rladd = sqrt(xladd*xladd + yladd*yladd)
          dphit = RADDEG*ATAN(xladd/rladd)

          dphid = sili_br_dphi(iLayer)
          dphir = DEGRAD*dphid

          v_i_name = v_m_name
          v_m_name = siliCage
          nr       = 1
          Do isec = 1, sili_br_nsec(iLayer)
            nladd   = sili_br_nlad(isec,iLayer)
            philadd = sili_br_phic(isec,iLayer) + dphit
     *           - (0.5*dphid)*(nladd-1)
            phirotladd = -90.+sili_br_tilt(iLayer) + philadd
            philadd    = DEGRAD*philadd
            DO iladd = 1, nladd
              xladd    = rladd*cos(philadd)
              yladd    = rladd*sin(philadd)
              irot = irot + 1
              CALL GSROTM(irot,90.,phirotladd,90.
     *             ,phirotladd+90.,0.,0.)
              call gspos(v_i_name,nr,v_m_name
     *             ,xladd,yladd,sili_br_z(iLayer),irot,'ONLY')
              philadd    = philadd + dphir
              phirotladd = phirotladd + dphid
              nr         = nr + 1
            Enddo
          Enddo


c     put SInn in set 'INR'

          namesv(4) = v_i_name
          call gsdet(set_id,v_i_name,nv,namesv,nbitsv,idtype,nwpa
     &         ,nwsa,iset,idet)
          call gsdeth(set_id,v_i_name,nhh,inrNMSH,inrNBITSH
     &         ,inrORIG,inrFACT)

        Enddo                    ! loop over layers


c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c&&&&&&&&&&&&&&&&&& To take care for the Endcap programer &&&&&&&&&&&&&&
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        v_m_name = siliCage
        npar = 10
        do iLayer = 1,Sili_sideLayers

C--> Create Volume 'SInn'

          v_i_name = siliNames(iLayer + sili_br_nlayers) ! So as to not override previous cylinders
          dim_sili(1)  = sili_phi1_side(iLayer) ! 
          dim_sili(2)  = sili_dph_side(iLayer) ! 
          dim_sili(3)  = sili_npdv_side(iLayer) ! 
          dim_sili(4)  = sili_nz_side(iLayer) ! 
          dim_sili(5)  = sili_z1_side(iLayer) ! 
          dim_sili(6)  = sili_rmin1_side(iLayer) ! 
          dim_sili(7)  = sili_rmax1_side(iLayer) ! 
          dim_sili(8)  = sili_z2_side(iLayer) !
          dim_sili(9)  = sili_rmin2_side(iLayer) !
          dim_sili(10) = sili_rmax2_side(iLayer) !

          nmed = sili_med_silicon
          call gsvolu(v_i_name,'PGON',nmed,dim_sili,npar,ivolu)
          CALL GSATT(v_i_name,'SEEN',1)
          CALL GSATT(v_i_name,'WORK',1) ! Make volume sensitive



C---> Place SIL1 in HALL

          nr   = 1
          call gspos(v_i_name,nr,v_m_name,0.0,0.0,
     &                 sili_zCenter_side(iLayer),irotnull,'ONLY')


C     put volume elements together into a set



c     put SInn in set 'INR '

          namesv(4) = v_i_name
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c&&&& For the Endcap programmer &&&&&&
c     """""""""""""""""""""""""
c     For the time beeing, I would not
c     recommend   changing anything in
c     these two calls. - VR 08/21/2003
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
          call gsdet(set_id,v_i_name,nv,namesv,nbitsv,idtype,nwpa
     &         ,nwsa,iset,idet)
          call gsdeth(set_id,v_i_name,nhh,inrNMSH,inrNBITSH
     &         ,inrORIG,inrFACT)
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

        enddo                    ! loop over layers
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


      ENDIF     ! check on volume character
c---
c---  Fill 'PARA' zebra-bank
c---
      CHFORM = '5I -F'               ! 4 integer counts, then use all float
      call mzform('PARA',CHFORM,iod) ! book characteristic

c     write the parameters to a zebra bank. later they will go to output file
c---  Counting number of parameters

      npar = 1                       ! Number of hit components

c     Contribution from SIEN/SICG/SIIC
      npar = npar + 8 + 2*sili_cg_npcon

c     Contribution from Barrel
      npar = npar + 2 + 11*sili_br_nlayers
      Do iLayer = 1, sili_br_nlayers
        npar = npar + 2*sili_br_nsec(iLayer)
      Enddo

c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c&&&&&&&&&&&&&&&&&& To take care for the Endcap programer &&&&&&&&&&&&&&
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c     Contribution from Endcap
      npar = npar + 2 + 11*sili_sideLayers
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      call mzbook(ixdiv_fr, lFD_PARA, lFD_PARA, 1,
     &     'PARA', 0, 0, npar, iod, 0)

C  fill the bank

c     Two first integers: numbers of layers in Barrel & Endcap
      iPoint = 1
      iqf(lfd_para + iPoint) = sili_br_nlayers
      iPoint = iPoint + 1
      iqf(lfd_para + iPoint) = sili_sideLayers
c     Number of hit components
      iPoint = iPoint + 1
      iqf(lfd_para + iPoint) = nhh
c     Number of barrel & endcap volume descriptors
      iPoint = iPoint + 1
      iqf(lfd_para + iPoint) = nbrv
      iPoint = iPoint + 1
      iqf(lfd_para + iPoint) = necv

c     Envelope/Cage parameters

      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = sili_cg_rmn
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = sili_cg_thck
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = sili_cg_inthck
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = sili_cg_tempc
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = FLOAT(sili_cg_npcon)
      Do icnt = 1, sili_cg_npcon
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_cg_z(icnt)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_cg_rmx(icnt)
      Enddo
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = sili_cg_xdisp
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = sili_cg_ydisp
      iPoint = iPoint + 1
      qf(lfd_para + iPoint) = sili_cg_zdisp

C     Barrel parameters

      Do iLayer = 1, sili_br_nlayers
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_snhalfx(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_snhalfy(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_snhalfz(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_x0add(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_snzgap(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_tilt(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = FLOAT(sili_br_nsn(iLayer))
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_r(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_z(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_br_dphi(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = FLOAT(sili_br_nsec(iLayer))
        Do isec = 1, sili_br_nsec(iLayer)
          iPoint = iPoint + 1
          qf(lfd_para + iPoint) = sili_br_phic(isec,iLayer)
          iPoint = iPoint + 1
          qf(lfd_para + iPoint) = FLOAT(sili_br_nlad(isec,iLayer))
        Enddo
      Enddo

c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c&&&&&&&&&&&&&&&&&& To take care for the Endcap programer &&&&&&&&&&&&&&
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

C     Endcap parameters

      do iLayer = 1,sili_sideLayers
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_phi1_side(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_dph_side(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_z1_side(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_rmin1_side(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_rmax1_side(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_z2_side(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_rmin2_side(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_rmax2_side(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_npdv_side(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_nz_side(iLayer)
        iPoint = iPoint + 1
        qf(lfd_para + iPoint) = sili_zCenter_side(iLayer)
      enddo
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      return

 996  stop '   Read error in siliside_par' 
 997  stop '   Read error in sili_br_par' 
 998  stop '   Read error in sili_cg_par' 

      end
