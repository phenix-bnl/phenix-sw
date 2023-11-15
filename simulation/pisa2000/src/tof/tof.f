c $Id: tof.f,v 1.10 2008/05/21 08:22:20 hpereira Exp $
c     ======================
c     File Name :    tof.f
c     ======================


c     Description :
c     =============

c     This subroutine defines the geometry of PHENIX - TOF wall.


c     Author:-   Tapan Nayak
c     (with help from Brian Cole)

c     Creation Date: 16-Sep-1992
c     ==========================

c	Structure of TOF;
c	=================
c                                      TFWG
c                                       |
c                         +-------------+-----+-----+-----+-------+-----+
c                         |             |     |     |     |       |     |
c                        TFPN          TFR1  TFR2  TFR3  TFR4    TFRU  TFRL
c                         | [Panel]     |   [Side Rack]            [Rail]
c                         |
c    +-----+----+----+----+----+-----+------------+
c    |     |    |    |    |    |     |            |
c  HCMB  TFA1 TFA2 TFA3 TFAC TFCB   CLMN        TFBP
c    |     |                         |            |
c  CABN  TFAH          +-------------+       +----+----+
c                      |             |       |    |    |
c                    SLTS          SLTL    TUNI TUA1 TUA2
c                      |             |
c          +-----+-----+-----+       +-----+-----+
c          |     |     |     |       |     |     |
c        PMAS  SCTS  LGPR  LGST    PMAS  SCTL  LGPR
c          |                         |
c                    +-------+-------+-------+
c                    |       |       |       |
c                   PMT    MMTL    BASE    SCKT


c                          TFR1 (TFR2,TFR3,TFR4)
c		  	    | [Side Rack]
c		  	    |
c         +-----+-----+-----+-----+-----+-----+-----+-----+
c         |     |     |     |     |     |     |     |     |
c       SRBT  HVRC  SRTP  SRSD  HVMV  RACL  RACS  SRCB  CFMV     
c                     |           |                       |
c                +----+      +----+                    +--+
c                |    |      |    |                    |  
c              STPM STPS   SCUP HVDB                 CFBX
c                                 |                    |
c                     +-----+-----+-----+         +----+
c                     |     |     |     |         |    |
c                   HPCN  HVSS  HVLS  HVBT      CFSS CFLS


c     The following parameters are used in the PHNX.par routine.
c     $tof_par
c     $end
c================================================================

c============================
      subroutine TOF(FULL,NH)
c============================
      implicit none
#include "gcflag.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "gugeom.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "fpflink.inc"
#include "fstore.inc"

      real         piby8
      real         rpos
      real         dphi
      real         phi
      real         phi_low

c  input parameter from phnx.par
      integer      TOFL_arms
      real         TOFL_rpos
      real         TFPN_dimen(3)
      real         TFPN_dr(10)
      real         TFPN_dphi(10)
      real         TFPN_dz(10)
      real         TFPN_panel(3,3,10) ! (point[3], xyz[3],panel[10])
      real         TFPN_panel0(3,3) ! Panel_A (p0(3), p1(3),p2(3))
      real         TFPN_panel1(3,3) ! Panel_B (p0(3), p1(3),p2(3))
      real         TFPN_panel2(3,3) ! Panel_C (p0(3), p1(3),p2(3))
      real         TFPN_panel3(3,3) ! Panel_D (p0(3), p1(3),p2(3))
      real         TFPN_panel4(3,3) ! Panel_F (p0(3), p1(3),p2(3))
      real         TFPN_panel5(3,3) ! Panel_G (p0(3), p1(3),p2(3))
      real         TFPN_panel6(3,3) ! Panel_H (p0(3), p1(3),p2(3))
      real         TFPN_panel7(3,3) ! Panel_I (p0(3), p1(3),p2(3))
      real         TFPN_panel8(3,3) ! Panel_E (p0(3), p1(3),p2(3))
      real         TFPN_panel9(3,3) ! Panel_J (p0(3), p1(3),p2(3))
      real         TFPN_p0x(10)
      real         TFPN_p0y(10)
      real         TFPN_p0z(10)
      real         TFPN_p1x(10)
      real         TFPN_p1y(10)
      real         TFPN_p1z(10)
      real         TFPN_p2x(10)
      real         TFPN_p2y(10)
      real         TFPN_p2z(10)
      real         p1x, p1y, p1z
      real         norm, scalar
      integer      TFPN_nslat
      integer      npnl_sector0
      integer      npnl_sector1
      integer      npnl_sector2
      integer      color_tof
      integer      med_tof

      real         TFSP_phi_1
      real         TFSP_phi_2
      real         TFSP_dimen(3)
      real         TFLP_phi_1
      real         TFLP_phi_2
      real         TFLP_phi_3
      real         TFLP_phi_4
      real         TFLP_dimen(3)
      integer      TFSP_nslat
      integer      TFSP_isegm
      integer      TFLP_nslat
      integer      TFLP_isegm

c  output parameter
      real         TFPN_rpos(10)/10*0./
      real         TFPN_phi(10) /10*0./
      real         TFPN_zpos(10)/10*0./
      real         TFSS_dimen(3)
      real         TFSS_length
      real         TFLS_dimen(3)
      real         TFLS_length
      integer*4    mFF_HADdets
      integer*4    mFF_ELEdets
      integer*4    mFF_ALLdets

      real         z_panel
      integer      nw
      integer      nuHADpar
      parameter    (nuHADpar = 13)
      integer      nuELEpar
      parameter    (nuELEpar = 14)
      real         uHADpar(nuHADpar)
      real         uELEpar(nuELEpar)
      integer      idtype
      character*4  chnmsv(2)/'TFPN','CLMN'/
      integer      nbitsv(2)/8,8/
      integer      nvd
      parameter    (nvd=3)
      character*4  chnmlv(nvd)/'TFPN','CLMN','SLTL'/
      integer      nbitlv(nvd)/nvd*8/
      integer      iofA, IOFU, IPOINT
      integer      iset
      integer      idet
      integer      isegment
      integer      ival
      integer      ivolu
      integer      iphi
      integer      iaxis
      integer      irot_tof
      integer      irot_rail
      integer      itheta
      integer      ip
      integer      nr
      integer*4    nh           ! set before call in gugeom
      integer      ndiv
      integer      nv
      integer      npar
      integer      nmed
      integer      nwpa
      integer      nwsa
      integer      tof_para_nd
      integer      tof_paru_nd
      character*20 chform
      character*4  full         ! set before call in gugeom
      character*4  set_id


c       Large-scale modification of tof_par and fpflink.inc 
c         29-Oct-1997 Akio Kiyomichi
c         22-Nov-1997 Akio Kiyomichi  tof_par and fpflink.inc are not changed

c     tof_par and fpflink.inc are re-chainged
c     adopt the TYPE-B 29-May-2000 Akio Kiyomichi

      data  TOFL_arms      /   0  / ! TOF position  East-->0, West-->1

      data  TOFL_rpos      / 503. / ! front surface position now
      data  TFPN_dimen     /   7.87, 98.5, 24.42464  /
      data  TFPN_dr        /10*0. / ! offset of   R-position
      data  TFPN_dphi      /10*0. / ! offset of phi-position
      data  TFPN_dz        /10*0. / ! offset of   Z-position
      data  TFPN_panel0    /
     &     -503.00,   -94.93,  -195.102,
     &     -503.00,   -94.93,  -146.802,
     &     -503.00,    94.93,  -195.102 / ! Panel_A  p0(x,y,z)-p2(x,y,z)
      data  TFPN_panel1    /
     &     -503.00,   -94.93,  -146.253,
     &     -503.00,   -94.93,   -97.993,
     &     -503.00,    94.93,  -146.253 / ! Panel_B  p0(x,y,z)-p2(x,y,z)
      data  TFPN_panel2    /
     &     -503.00,   -94.93,   -97.404,
     &     -503.00,   -94.93,   -49.144,
     &     -503.00,    94.93,   -97.404 / ! Panel_C  p0(x,y,z)-p2(x,y,z)
      data  TFPN_panel3    /
     &     -503.00,   -94.93,  -48.5546,
     &     -503.00,   -94.93,   -0.2946,
     &     -503.00,    94.93,  -48.5546 / ! Panel_D  p0(x,y,z)-p2(x,y,z)
      data  TFPN_panel4    /
     &     -503.00,   -94.93,    0.2946,
     &     -503.00,   -94.93,   48.5546,
     &     -503.00,    94.93,    0.2946 / ! Panel_F  p0(x,y,z)-p2(x,y,z)
      data  TFPN_panel5    /
     &     -503.00,   -94.93,    49.143,
     &     -503.00,   -94.93,    97.404,
     &     -503.00,    94.93,    49.143 / ! Panel_G  p0(x,y,z)-p2(x,y,z)
      data  TFPN_panel6    /
     &     -503.00,   -94.93,    97.993,
     &     -503.00,   -94.93,   146.253,
     &     -503.00,    94.93,    97.993 / ! Panel_H  p0(x,y,z)-p2(x,y,z)
      data  TFPN_panel7    /
     &     -503.00,   -94.93,   146.802,
     &     -503.00,   -94.93,   195.102,
     &     -503.00,    94.93,   146.802 / ! Panel_I  p0(x,y,z)-p2(x,y,z)
      data  TFPN_panel8    /
     &     -428.38,  -280.19,  -48.5546,
     &     -428.38,  -280.19,   -0.2946,
     &     -501.04,  -104.79,  -48.5546 / ! Panel_E  p0(x,y,z)-p2(x,y,z)
      data  TFPN_panel9    /
     &     -428.38,  -280.19,    0.2946,
     &     -428.38,  -280.19,   48.5546,
     &     -501.04,  -104.79,    0.2946 / ! Panel_J  p0(x,y,z)-p2(x,y,z)
      data  TFPN_nslat     /  96  /
      data  npnl_sector0   /   2  /
      data  npnl_sector1   /   8  /
      data  npnl_sector2   /   0  /
      data  color_tof      /   4  /
      data  med_tof        / 710  / ! TOF-Scintillator BC404


c Second (New) type of tof-par (TYPE-B) 22-Nov-1997 Akio Kiyomichi

c     tof_par and fpflink.inc are re-chainged
c     adopt the TYPE-B                  29-May-2000 Akio Kiyomichi

      namelist     /tof_par/
     $             TOFL_rpos,
     $             TFPN_dimen,
     $             TFPN_panel0,
     $             TFPN_panel1,
     $             TFPN_panel2,
     $             TFPN_panel3,
     $             TFPN_panel4,
     $             TFPN_panel5,
     $             TFPN_panel6,
     $             TFPN_panel7,
     $             TFPN_panel8,
     $             TFPN_panel9,
     $             TFPN_nslat,
     $             npnl_sector0,
     $             npnl_sector1,
     $             npnl_sector2,
     $             color_tof,
     $             med_tof

c working parameters
      character*4  v_m_name
      character*4  v_i_name
	real gpos(3)
        real pos_panel(3)       ! panel center position on HCMB surface
        real rot_theta(3)       ! rotation parameters (theta) 
        real rot_phi(3)         ! rotation parameters (phi)
        real xdiff(4)           ! X -> X*  (x, y, z, r=sqrt(x**2+y**2))
        real ydiff(4)           ! Y -> Y*  (x, y, z, r=sqrt(x**2+y**2))
        real zdiff(4)           ! Z -> Z*  (x, y, z, r=sqrt(x**2+y**2))
        real diffs
        real scintz(3) ! z position of scintillators in CLMN. Short one is 1
        real send_gap/2.654/,lend_gap/5.486/,btwn_gap/4.445/
        real pos_rack(3)
        real pos_rail(3)

c	vectors starting with p were meant for pseudo volume


cak     real    pTFWG_dim(10)/146.25,45.,2.,2.,-200.,503.,523.697,
cak  &          200.,503.,523.697/
ckn    real    pTFWG_dim(10)/168.75,45.,2.,2.,-200.,503.,523.697,
ckn  &          200.,503.,523.697/
        real    pTFWG_dim(10)/168.75,45.,2.,2.,-270.,503.,523.697,
     &          270.,503.,523.697/
cak	real	pTFPN_dim(3)/7.87,96.9,24.5/
cak     real	pTFPN_dim(3)/7.87,98.5,24.5/
        real	pTFPN_dim(3)/7.87,96.6,24.5/
ckn   
        real    pTFR1_dim(3)/7.8,96.6,32.5/
        real    pTFR2_dim(3)/7.8,96.6,32.5/
        real    pTFR3_dim(3)/7.8,96.6,32.5/
        real    pTFR4_dim(3)/7.8,96.6,32.5/
cak     real 	pHCMB_dim(3)/1.27,96.3,24.3/
cak     real	CRBN_dim(3) /0.15875,93.98,24.13/
        real 	pHCMB_dim(3)/1.27,94.93,24.3/
        real	CRBN_dim(3) /0.15875,94.93,24.13/
        real	pSLTS_dim(3)/1.5,5.5,24.0/
        real	pSLTL_dim(3)/1.5,5.5,34.0/
        real	pCLMN_dim(3)/1.5,5.5,93.98/
        real 	SCTL_dim(3) /0.7505,0.7505,31.885/
        real	SCTS_dim(3) /0.7505,0.7505,21.695/
        real	LGPR_dim(4) /0.,2.064,0.7505,1.032/
        real	LGST_dim(3) /0.7505,1.407,0.7505/
        real	LGS1_dim(3) /0.7505,0.6565,0.7505/
        real	LGS2_dim(4) /0.,1.0615,0.7505,0.5307/
        real	pPMAS_dim(3)/1.5,2.2225,3.7/
        real 	PMT_dim(3)  /0.8,0.95,3.2/
        real	MMTL_dim(3) /1.04,1.09,2.1/
        real 	BASE_dim(3) /1.27,2.2225,0.1524/
        real	SCKT_dim(3) /0.4,0.9,0.3/
c        real	TFA1_dim(3) /1.0,88.,1.0/
c        real	pTFAH_dim(3)/0.8,86.,0.8/
        real	TFA1_dim(3) /1.5,84.85,1.0/
        real	pTFAH_dim(3)/1.3,81.35,0.8/
c        real	TFA2_dim(3) /3.1,0.8,23.5/
c        real	TFA2_dim(3) /7.15,0.35,23.5/
        real	TFA2_dim(3) /2.1,0.65,24.28/
        real	TFA3_dim(3) /5.5,0.8,2.4/
        real	TFAC_dim(3) /0.1,90.,24.5/
        real	TFCB_dim(3) /0.38,90.,24.5/
cak Nov-30-98
        real	pTFBP_dim(3)/4.0,10.0,24.31/ ! Tof Fiber Bundle Plate
        real	TUNI_dim(3) /0.2,10.0,24.31/ ! Tof UNIlate 
        real	TUA1_dim(3) /0.025,7.50,21.5/ ! Tof Unilate Alumi. cover 1
        real	TUA2_dim(3) /3.75,0.025,21.5/ ! Tof Unilate Alumi. cover 2

        real	TFRU_dim(3) /2.1,0.8,130.25/ ! ToF Rail Upper
        real	TFRL_dim(3) /7.5,0.5,130.25/ ! ToF Rail Lower
ckn
c        real    HVRC_dim(3) /0.5,96.45,5./
        real    HVRC_dim(3) /0.5,95.8,5./
        real    SRBT_dim(3) /0.05,96.55,32.5/
        real    pSRTP_dim(3)/0.05,96.55,32.5/
        real    STPS_dim(3) /0.05,16.275,32.5/
        real    STPM_dim(3) /0.05,64.,22.5/
        real    SRSD_dim(3) /7.7,0.05,32.5/
        real    pHVMV_dim(3)/4.4,66.45,10./
        real    pHVM2_dim(3)/4.4,66.45,10./
        real    pHVDB_dim(3)/4.4,16.5,10./
        real    HVLS_dim(3) /4.2,16.3,0.1/
        real    HVBT_dim(3) /0.1,16.5,10./
        real    HVSS_dim(3) /4.2,0.1,10./
        real    HPCN_dim(3) /1.16,1.4,3.295/
        real    RACL_dim(3) /0.4,61.65,8./
        real    RACS_dim(3) /0.4,33.25,8./
        real    SRCB_dim(3) /0.38,96.35,22.3/
        real    SCUP_dim(3) /4.0,8.0,10./
        real    SCLW_dim(3) /2.0,8.0,10./
        real    CFLS_dim(3) /5.95,0.1,1.25/
        real    CFSS_dim(3) /0.1,5.75,1.25/
        real    pCFBX_dim(3)/5.95,5.95,1.25/
c        real    pCFMV_dim(3)/5.95,96.5,1.25/
        real    pCFMV_dim(3)/5.95,95.8,1.25/
c        real    TFA4_dim(3)  /7.15,0.35,32.5/
        real    TFA4_dim(3)  /7.15,0.4,32.5/
ckn   
        real	RAIL_dim
        real	WRSP_dim
        real	PLBR_dim
        real    Slat_halfwidth/0.76327/
c        real    Panel_halfwidth/24.42464/ ! same as TFPN_dimen(3)
        integer glass_med,lucite_med,mumetal_med,tof_med
        integer cable_med,uni_med,carbon_med
        integer iclmn
	integer ipnl,npnl,npnl_opt
        integer iral,nral
        integer n_box,n_ls,n_ss,n_bt,n_cn,n_rc,n_bx,n_l,n_s,n_r
        integer n_sd, n_sc, npoint, ixyz, ipanel

      INTEGER NMAT,ISVOL,IFIELD,NWBUF

      REAL FIELDM,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN,
     1  UBUF(10)

c --- Plastic (lucite) for cover plates (C5H8O2) ---
      real AP(3) /12.,1.,16./     ! C,H,O
      real ZP(3) /6.,1.,8./
      real WP(3) /5.,8.,2./
      real DP /1.18/
c --- Borosilicate glass (PMT glass) [Pyrex]
      real AG(4)/16.0,28.086,10.81,22.99/ ! O,Si,B,Na
      real ZG(4)/8.,14.,5.,11./
      real WG(4)/2.01,0.8,.24,.1/  ! 80% SiO2 12% B2O3 5% Na2O
      real DG/2.23/

c --- Cable
      real AC(4)/63.54,12.01,1.01,35.45/  ! Cu,C,H,Cl
      real ZC(4)/29.00,6.00,1.00,17.00/
      real WC(4)/0.393,1.214,2.117,0.311/ ! 39.3% Cu 31.1% C2H3Cl 29.6% C2H4
      real DC/1.936/

c --- UNILATE plastic   {CO-C6H4-COO-(CH2)2-O-}n
      real AU(3) /12.,1.,16./     ! C,H,O
      real ZU(3) /6.,1.,8./
      real WU(3) /10.,8.,4./
      real DU /1.63/

      integer tof_mate,lucite_mate,glass_mate,mumetal_mate,cable_mate
      integer uni_mate
       
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun
      
      piby8=pi/8.

c     Read geometry file segment
      write( *,* ) 'tof - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = tof_par, err = 999 )

c     Zebra Bank :
      chform = '94F 6I'      ! TYPE-B
      call MZFORM('PARA',chform,iofA)         ! book characteristics

c     Write parameters to a zebra bank.
c     Later they will go to output file.
c     tof_para_nd is the # of data words.
c     iof is the IO format word.

c     tof_par and fpflink.inc are re-chainged
c     adopt the TYPE-B                  29-May-2000 Akio Kiyomichi

c      tof_para_nd = 19          ! TYPE-A
      tof_para_nd = 100          ! TYPE-B
      call MZBOOK(
     $     ixdiv_fr,
     $     lFF_PARA,
     $     lFF_PARA,
     $     1,
     $     'PARA',
     $     0,
     $     0,
     $     TOF_PARA_ND,
     $     iofA,
     $     0)

c     Copy raw geometry parameters into 'EPRA' bank.
c     Fill the bank

c TYPE-A
c      qf( IPOINT  + ofea_TOFL_rpos   )  =   TOFL_rpos
c      qf( IPOINT  + ofea_TFSP_phi_1  )  =   TFSP_phi_1
c      qf( IPOINT  + ofea_TFSP_phi_2  )  =   TFSP_phi_2
c      qf( IPOINT  + ofea_TFSP_dimen_1)  =   TFSP_dimen(1)
c      qf( IPOINT  + ofea_TFSP_dimen_2)  =   TFSP_dimen(2)
c      qf( IPOINT  + ofea_TFSP_dimen_3)  =   TFSP_dimen(3)
c      iqf( IPOINT  + ofea_TFSP_nslat  )  =   TFSP_nslat
c      iqf( IPOINT  + ofea_TFSP_isegm  )  =   TFSP_isegm
c      qf( IPOINT  + ofea_TFLP_phi_1  )  =   TFLP_phi_1
c      qf( IPOINT  + ofea_TFLP_phi_2  )  =   TFLP_phi_2
c      qf( IPOINT  + ofea_TFLP_phi_3  )  =   TFLP_phi_3
c      qf( IPOINT  + ofea_TFLP_phi_4  )  =   TFLP_phi_4
c      qf( IPOINT  + ofea_TFLP_dimen_1)  =   TFLP_dimen(1)
c      qf( IPOINT  + ofea_TFLP_dimen_2)  =   TFLP_dimen(2)
c      qf( IPOINT  + ofea_TFLP_dimen_3)  =   TFLP_dimen(3)
c      iqf( IPOINT  + ofea_TFLP_nslat  )  =   TFLP_nslat
c      iqf( IPOINT  + ofea_TFLP_isegm  )  =   TFLP_isegm
c      iqf( IPOINT  + ofea_color_tof   )  =   color_tof
c      iqf( IPOINT  + ofea_med_tof     )  =   med_tof

c     tof_par and fpflink.inc are re-chainged
c     adopt the TYPE-B                  29-May-2000 Akio Kiyomichi

c TYPE-B
      IPOINT = LFF_PARA + 1
      qf( IPOINT  + ofea_TOFL_rpos     )  =   TOFL_rpos
      qf( IPOINT  + ofea_TFPN_dimen_x  )  =   TFPN_dimen(1)
      qf( IPOINT  + ofea_TFPN_dimen_y  )  =   TFPN_dimen(2)
      qf( IPOINT  + ofea_TFPN_dimen_z  )  =   TFPN_dimen(3)
      qf( IPOINT  + ofea_TFPN_panel0_0x)  =   TFPN_panel0(1,1)
      qf( IPOINT  + ofea_TFPN_panel0_0y)  =   TFPN_panel0(2,1)
      qf( IPOINT  + ofea_TFPN_panel0_0z)  =   TFPN_panel0(3,1)
      qf( IPOINT  + ofea_TFPN_panel0_1x)  =   TFPN_panel0(1,2)
      qf( IPOINT  + ofea_TFPN_panel0_1y)  =   TFPN_panel0(2,2)
      qf( IPOINT  + ofea_TFPN_panel0_1z)  =   TFPN_panel0(3,2)
      qf( IPOINT  + ofea_TFPN_panel0_2x)  =   TFPN_panel0(1,3)
      qf( IPOINT  + ofea_TFPN_panel0_2y)  =   TFPN_panel0(2,3)
      qf( IPOINT  + ofea_TFPN_panel0_2z)  =   TFPN_panel0(3,3)
      qf( IPOINT  + ofea_TFPN_panel1_0x)  =   TFPN_panel1(1,1)
      qf( IPOINT  + ofea_TFPN_panel1_0y)  =   TFPN_panel1(2,1)
      qf( IPOINT  + ofea_TFPN_panel1_0z)  =   TFPN_panel1(3,1)
      qf( IPOINT  + ofea_TFPN_panel1_1x)  =   TFPN_panel1(1,2)
      qf( IPOINT  + ofea_TFPN_panel1_1y)  =   TFPN_panel1(2,2)
      qf( IPOINT  + ofea_TFPN_panel1_1z)  =   TFPN_panel1(3,2)
      qf( IPOINT  + ofea_TFPN_panel1_2x)  =   TFPN_panel1(1,3)
      qf( IPOINT  + ofea_TFPN_panel1_2y)  =   TFPN_panel1(2,3)
      qf( IPOINT  + ofea_TFPN_panel1_2z)  =   TFPN_panel1(3,3)
      qf( IPOINT  + ofea_TFPN_panel2_0x)  =   TFPN_panel2(1,1)
      qf( IPOINT  + ofea_TFPN_panel2_0y)  =   TFPN_panel2(2,1)
      qf( IPOINT  + ofea_TFPN_panel2_0z)  =   TFPN_panel2(3,1)
      qf( IPOINT  + ofea_TFPN_panel2_1x)  =   TFPN_panel2(1,2)
      qf( IPOINT  + ofea_TFPN_panel2_1y)  =   TFPN_panel2(2,2)
      qf( IPOINT  + ofea_TFPN_panel2_1z)  =   TFPN_panel2(3,2)
      qf( IPOINT  + ofea_TFPN_panel2_2x)  =   TFPN_panel2(1,3)
      qf( IPOINT  + ofea_TFPN_panel2_2y)  =   TFPN_panel2(2,3)
      qf( IPOINT  + ofea_TFPN_panel2_2z)  =   TFPN_panel2(3,3)
      qf( IPOINT  + ofea_TFPN_panel3_0x)  =   TFPN_panel3(1,1)
      qf( IPOINT  + ofea_TFPN_panel3_0y)  =   TFPN_panel3(2,1)
      qf( IPOINT  + ofea_TFPN_panel3_0z)  =   TFPN_panel3(3,1)
      qf( IPOINT  + ofea_TFPN_panel3_1x)  =   TFPN_panel3(1,2)
      qf( IPOINT  + ofea_TFPN_panel3_1y)  =   TFPN_panel3(2,2)
      qf( IPOINT  + ofea_TFPN_panel3_1z)  =   TFPN_panel3(3,2)
      qf( IPOINT  + ofea_TFPN_panel3_2x)  =   TFPN_panel3(1,3)
      qf( IPOINT  + ofea_TFPN_panel3_2y)  =   TFPN_panel3(2,3)
      qf( IPOINT  + ofea_TFPN_panel3_2z)  =   TFPN_panel3(3,3)
      qf( IPOINT  + ofea_TFPN_panel4_0x)  =   TFPN_panel4(1,1)
      qf( IPOINT  + ofea_TFPN_panel4_0y)  =   TFPN_panel4(2,1)
      qf( IPOINT  + ofea_TFPN_panel4_0z)  =   TFPN_panel4(3,1)
      qf( IPOINT  + ofea_TFPN_panel4_1x)  =   TFPN_panel4(1,2)
      qf( IPOINT  + ofea_TFPN_panel4_1y)  =   TFPN_panel4(2,2)
      qf( IPOINT  + ofea_TFPN_panel4_1z)  =   TFPN_panel4(3,2)
      qf( IPOINT  + ofea_TFPN_panel4_2x)  =   TFPN_panel4(1,3)
      qf( IPOINT  + ofea_TFPN_panel4_2y)  =   TFPN_panel4(2,3)
      qf( IPOINT  + ofea_TFPN_panel4_2z)  =   TFPN_panel4(3,3)
      qf( IPOINT  + ofea_TFPN_panel5_0x)  =   TFPN_panel5(1,1)
      qf( IPOINT  + ofea_TFPN_panel5_0y)  =   TFPN_panel5(2,1)
      qf( IPOINT  + ofea_TFPN_panel5_0z)  =   TFPN_panel5(3,1)
      qf( IPOINT  + ofea_TFPN_panel5_1x)  =   TFPN_panel5(1,2)
      qf( IPOINT  + ofea_TFPN_panel5_1y)  =   TFPN_panel5(2,2)
      qf( IPOINT  + ofea_TFPN_panel5_1z)  =   TFPN_panel5(3,2)
      qf( IPOINT  + ofea_TFPN_panel5_2x)  =   TFPN_panel5(1,3)
      qf( IPOINT  + ofea_TFPN_panel5_2y)  =   TFPN_panel5(2,3)
      qf( IPOINT  + ofea_TFPN_panel5_2z)  =   TFPN_panel5(3,3)
      qf( IPOINT  + ofea_TFPN_panel6_0x)  =   TFPN_panel6(1,1)
      qf( IPOINT  + ofea_TFPN_panel6_0y)  =   TFPN_panel6(2,1)
      qf( IPOINT  + ofea_TFPN_panel6_0z)  =   TFPN_panel6(3,1)
      qf( IPOINT  + ofea_TFPN_panel6_1x)  =   TFPN_panel6(1,2)
      qf( IPOINT  + ofea_TFPN_panel6_1y)  =   TFPN_panel6(2,2)
      qf( IPOINT  + ofea_TFPN_panel6_1z)  =   TFPN_panel6(3,2)
      qf( IPOINT  + ofea_TFPN_panel6_2x)  =   TFPN_panel6(1,3)
      qf( IPOINT  + ofea_TFPN_panel6_2y)  =   TFPN_panel6(2,3)
      qf( IPOINT  + ofea_TFPN_panel6_2z)  =   TFPN_panel6(3,3)
      qf( IPOINT  + ofea_TFPN_panel7_0x)  =   TFPN_panel7(1,1)
      qf( IPOINT  + ofea_TFPN_panel7_0y)  =   TFPN_panel7(2,1)
      qf( IPOINT  + ofea_TFPN_panel7_0z)  =   TFPN_panel7(3,1)
      qf( IPOINT  + ofea_TFPN_panel7_1x)  =   TFPN_panel7(1,2)
      qf( IPOINT  + ofea_TFPN_panel7_1y)  =   TFPN_panel7(2,2)
      qf( IPOINT  + ofea_TFPN_panel7_1z)  =   TFPN_panel7(3,2)
      qf( IPOINT  + ofea_TFPN_panel7_2x)  =   TFPN_panel7(1,3)
      qf( IPOINT  + ofea_TFPN_panel7_2y)  =   TFPN_panel7(2,3)
      qf( IPOINT  + ofea_TFPN_panel7_2z)  =   TFPN_panel7(3,3)
      qf( IPOINT  + ofea_TFPN_panel8_0x)  =   TFPN_panel8(1,1)
      qf( IPOINT  + ofea_TFPN_panel8_0y)  =   TFPN_panel8(2,1)
      qf( IPOINT  + ofea_TFPN_panel8_0z)  =   TFPN_panel8(3,1)
      qf( IPOINT  + ofea_TFPN_panel8_1x)  =   TFPN_panel8(1,2)
      qf( IPOINT  + ofea_TFPN_panel8_1y)  =   TFPN_panel8(2,2)
      qf( IPOINT  + ofea_TFPN_panel8_1z)  =   TFPN_panel8(3,2)
      qf( IPOINT  + ofea_TFPN_panel8_2x)  =   TFPN_panel8(1,3)
      qf( IPOINT  + ofea_TFPN_panel8_2y)  =   TFPN_panel8(2,3)
      qf( IPOINT  + ofea_TFPN_panel8_2z)  =   TFPN_panel8(3,3)
      qf( IPOINT  + ofea_TFPN_panel9_0x)  =   TFPN_panel9(1,1)
      qf( IPOINT  + ofea_TFPN_panel9_0y)  =   TFPN_panel9(2,1)
      qf( IPOINT  + ofea_TFPN_panel9_0z)  =   TFPN_panel9(3,1)
      qf( IPOINT  + ofea_TFPN_panel9_1x)  =   TFPN_panel9(1,2)
      qf( IPOINT  + ofea_TFPN_panel9_1y)  =   TFPN_panel9(2,2)
      qf( IPOINT  + ofea_TFPN_panel9_1z)  =   TFPN_panel9(3,2)
      qf( IPOINT  + ofea_TFPN_panel9_2x)  =   TFPN_panel9(1,3)
      qf( IPOINT  + ofea_TFPN_panel9_2y)  =   TFPN_panel9(2,3)
      qf( IPOINT  + ofea_TFPN_panel9_2z)  =   TFPN_panel9(3,3)
      iqf(IPOINT  + ofea_TFPN_nslat    )  =   TFPN_nslat
      iqf(IPOINT  + ofea_npnl_sector0  )  =   npnl_sector0
      iqf(IPOINT  + ofea_npnl_sector1  )  =   npnl_sector1
      iqf(IPOINT  + ofea_npnl_sector2  )  =   npnl_sector2
      iqf(IPOINT  + ofea_color_tof     )  =   color_tof
      iqf(IPOINT  + ofea_med_tof       )  =   med_tof

c     End of EPRA bank filling.

c     Only book volumes if input parameters request it
      if (CVOLU_OPT(1,7) .ne. 'FULL' .and.
     $       CVOLU_OPT(1,7) .ne. 'VOLS')then
         write(*,'(1x,a)')' TOF : No volumes defined '
         goto 9999
      endif
ckk
ckk
ckk

c     re-setting the palnel geometry for useful in this code
c                                    May 31, 2000 Akio Kiyomichi
      do npoint = 1,3
         do ixyz = 1,3
            TFPN_panel(ixyz, npoint, 1) = TFPN_panel0(ixyz, npoint)
            TFPN_panel(ixyz, npoint, 2) = TFPN_panel1(ixyz, npoint)
            TFPN_panel(ixyz, npoint, 3) = TFPN_panel2(ixyz, npoint)
            TFPN_panel(ixyz, npoint, 4) = TFPN_panel3(ixyz, npoint)
            TFPN_panel(ixyz, npoint, 5) = TFPN_panel4(ixyz, npoint)
            TFPN_panel(ixyz, npoint, 6) = TFPN_panel5(ixyz, npoint)
            TFPN_panel(ixyz, npoint, 7) = TFPN_panel6(ixyz, npoint)
            TFPN_panel(ixyz, npoint, 8) = TFPN_panel7(ixyz, npoint)
            TFPN_panel(ixyz, npoint, 9) = TFPN_panel8(ixyz, npoint)
            TFPN_panel(ixyz, npoint, 10)= TFPN_panel9(ixyz, npoint)
         enddo
      enddo
      do ipanel = 1,10
         TFPN_p0x(ipanel) = TFPN_panel(1, 1, ipanel)
         TFPN_p0y(ipanel) = TFPN_panel(2, 1, ipanel)
         TFPN_p0z(ipanel) = TFPN_panel(3, 1, ipanel)
         TFPN_p1x(ipanel) = TFPN_panel(1, 2, ipanel)
         TFPN_p1y(ipanel) = TFPN_panel(2, 2, ipanel)
         TFPN_p1z(ipanel) = TFPN_panel(3, 2, ipanel)
         TFPN_p2x(ipanel) = TFPN_panel(1, 3, ipanel)
         TFPN_p2y(ipanel) = TFPN_panel(2, 3, ipanel)
         TFPN_p2z(ipanel) = TFPN_panel(3, 3, ipanel)
      enddo

c ---> define material for TOF
c     CFM: add in 710 and 720, which have different tracking thresholds

      tof_mate=700

c     CFM: August 18, 1999   g77 sees that NWBUF is undefined?

      nwbuf = 0
      call gsmate(tof_mate,'BC404 $',6.221,3.373,1.032,
     1              0.424E02,0.820E02,ubuf,nwbuf)
      call gsmate(710,'BC404_710 $',6.221,3.373,1.032,
     1              0.424E02,0.820E02,ubuf,nwbuf)
      call gsmate(720,'BC404_720 $',6.221,3.373,1.032,
     1              0.424E02,0.820E02,ubuf,nwbuf)

      lucite_mate=tof_mate+1
      CALL GSMIXT(lucite_mate,'Lucite    $',AP,ZP,DP,-3,WP)

      glass_mate=tof_mate+2
      CALL GSMIXT(glass_mate,'Borosilicate$',AG,ZG,DG,-4,WG)

      mumetal_mate=tof_mate+3

c     C Maguire, August 15 1999, change for g77 compatibility (ubuf,nwbuf)

      CALL GSMATE(mumetal_mate,'Fe    $', 55.85,26., 7.87,1.76,17.1,
     +            ubuf,nwbuf)
cak[
      cable_mate=tof_mate+4
      CALL GSMIXT(cable_mate,'Cable$',AC,ZC,DC,-4,WC)

      uni_mate=tof_mate+5
      CALL GSMIXT(uni_mate,'UNILATE$',AU,ZU,DU,-3,WU)
cak]

c ---> define tracking media for TOF

c	glass_med,lucite_med,mumetal_med,tof_med
      isvol = 1      ! sensitive
      ifield = 0     ! no magnetic field
      fieldm = 0.0   ! max field
      tmaxfd = 45.0  ! maximum angle due to field (one step) in degrees
      dmaxms = 0.50  ! max disp. due to mulsct. in one step (cm)
      deemax = 0.05  ! max fractional energy loss in one step
      epsil = 0.01   ! tracking precision (cm)
      stmin = 0.1    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      nwbuf=0

      call gstmed(700,'TOF SCINT $',tof_mate,isvol,ifield,
     1	  fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c   CFM: using different materials for tracking media with different thresholds

      call gstmed(710,'TOF SCINT $',710,isvol,ifield,
     1	  fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
      call gstmed(720,'TOF SCINT $',720,isvol,ifield,
     1	  fieldm,tmaxfd, 0.10 ,deemax,epsil,stmin,ubuf,nwbuf)

c     Special Thresholds for TOF-Scintillator

      call gstpar(710,'CUTELE',0.0001)      ! electron cut at 100 keV
      call gstpar(710,'CUTGAM',0.0001)      ! gamma cut at 100 keV
      call gstpar(710,'BCUTE',0.0001)       ! e brem cut at 100 keV
      call gstpar(710,'CUTMUO',0.01)        ! muon cut at 10 MeV
      call gstpar(710,'BCUTM',0.01)         ! muon brem cut at 10 MeV
      call gstpar(710,'DCUTE',1.e+4)        ! e delta ray cut (Landau ON)
      call gstpar(710,'DCUTM',1.e+4)        ! muon delta ray cut (Landau ON)

      call gstpar(720,'CUTELE',0.0001)      ! electron cut at 100 keV
      call gstpar(720,'CUTGAM',0.0001)      ! gamma cut at 100 keV
      call gstpar(720,'BCUTE',0.0001)       ! e brem cut at 100 keV
      call gstpar(720,'CUTMUO',0.01)        ! muon cut at 10 MeV
      call gstpar(720,'BCUTM',0.01)         ! muon brem cut at 10 MeV
      call gstpar(720,'DCUTE',1.e+4)        ! e delta ray cut (Landau ON)
      call gstpar(720,'DCUTM',1.e+4)        ! muon delta ray cut (Landau ON)

      isvol=0
      lucite_med=700+1
      call gstmed(Lucite_med,'TOF Light Guide$',lucite_mate,isvol,
     1	ifield,fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

      glass_med=700+2
      call gstmed(glass_med,'TOF PMT glass$',glass_mate,isvol,ifield,
     1	fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

      mumetal_med=700+3
      call gstmed(mumetal_med,'TOF Mu metal$',mumetal_mate,isvol,
     1	ifield,fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
cak[
      cable_med=700+4
      CALL GSTMED(cable_med,'TOF Cable $',cable_mate,isvol,ifield,
     1     fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

      uni_med=700+5
      CALL GSTMED(uni_med,'TOF UNILATE $',uni_mate,isvol,ifield,
     1     fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

      carbon_med=700+6
      CALL GSTMED(carbon_med,'TOF Carbon $',6,isvol,ifield,
     1     fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
cak]

c     TOF-Scintilator mediate. 'med_tof' is input parameter from phnx.par
c    (10/29/97 Akio Kiyomichi)
c              700 : no set thresholds
c              710 : set thresholds (ELE,GAM = 100keV, MUO = 10MeV) [Default]
c              720 : set thresholds and maximum step is 0.1cm

      if(med_tof .eq. 700)then
         tof_med = 700
      elseif(med_tof .eq. 710)then
         tof_med = 710
      elseif(med_tof .eq. 720)then
         tof_med = 720
      else
         tof_med = 710
      endif


c---> Create PHENIX-TOF pseudo volums (May 95)
c	(fill them with air)

c     new definition of TOF pseudo volume 
c     two sides of hexadecagon (wedge shape) K.K. 12/6/96 

      v_i_name = 'TFWG'
      npar     = 10
      call GSVOLU(v_i_name,'PGON',6,pTFWG_dim,npar,ivolu)
      call GSATT(v_i_name,'SEEN',0)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'TFPN'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pTFPN_dim,npar,ivolu)
      call GSATT(v_i_name,'SEEN',0)

      v_i_name = 'HCMB'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pHCMB_dim,npar,ivolu)
      call GSATT(v_i_name,'SEEN',0)
	
      v_i_name = 'PMAS'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pPMAS_dim,npar,ivolu)
      call GSATT(v_i_name,'SEEN',0)

      v_i_name = 'CLMN'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pCLMN_dim,npar,ivolu)
      call GSATT(v_i_name,'SEEN',0)

      v_i_name = 'SLTS'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pSLTS_dim,npar,ivolu)
      call GSATT(v_i_name,'SEEN',0)

      v_i_name = 'SLTL'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pSLTL_dim,npar,ivolu)
      call GSATT(v_i_name,'SEEN',0)

c---> Create real volumes ( carbon= med# 706, aluminium= med# 26)

      v_i_name = 'CRBN'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',carbon_med,CRBN_dim,npar,ivolu) ! carbon
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)

      v_i_name = 'PMT '
      npar     = 3
      call GSVOLU(v_i_name,'TUBE',glass_med,PMT_dim,npar,ivolu) ! glass
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)

      v_i_name = 'MMTL'
      npar     = 3
      call GSVOLU(v_i_name,'TUBE',mumetal_med,MMTL_dim,npar,ivolu) ! mu-metal
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)

      v_i_name = 'BASE'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',24,BASE_dim,npar,ivolu) ! G10 plate
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)

      v_i_name = 'SCKT'
      npar     = 3
      call GSVOLU(v_i_name,'TUBE',Lucite_med,SCKT_dim,npar,ivolu) ! lucite
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)

      v_i_name = 'SCTS'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',tof_med,SCTS_dim,npar,ivolu) ! scinti.
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'SCTL'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',tof_med,SCTL_dim,npar,ivolu) ! scinti.
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'LGPR'
      npar     = 4
      call GSVOLU(v_i_name,'TRD1',lucite_med,LGPR_dim,npar,ivolu) ! scinti.
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)

      v_i_name = 'LGST'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',lucite_med,LGST_dim,npar,ivolu) ! scinti.
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)


      v_i_name = 'LGS2'
      npar     = 4
      call GSVOLU(v_i_name,'TRD1',lucite_med,LGS2_dim,npar,ivolu) ! scinti.
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)

      v_i_name = 'LGS1'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',lucite_med,LGS1_dim,npar,ivolu) ! scinti.
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)
cak[
      v_i_name = 'TFAH'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pTFAH_dim,npar,ivolu)
      call GSATT(v_i_name,'SEEN',0)

      v_i_name = 'TFA1'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,TFA1_dim,npar,ivolu) ! alumi.frame
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)

      v_i_name = 'TFA2'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,TFA2_dim,npar,ivolu) ! alumi.frame
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)
cak
      v_i_name = 'TFA3'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,TFA3_dim,npar,ivolu) ! alumi.frame
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)
ckn
      v_i_name = 'TFA4'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,TFA4_dim,npar,ivolu) ! alumi.frame for side rack
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1) 

      v_i_name = 'TFAC'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,TFAC_dim,npar,ivolu) ! alumi. cover
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1) 

      v_i_name = 'TFCB'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',cable_med,TFCB_dim,npar,ivolu) ! cable
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)

cak Nov-30-98
      v_i_name = 'TFBP'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pTFBP_dim,npar,ivolu) ! Tof Fiber Bundle Plate
      call GSATT(v_i_name,'SEEN',0)
      call GSATT(v_i_name,'COLO',0)
      
      v_i_name = 'TUNI'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,TUNI_dim,npar,ivolu) ! Tof UNIlate 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)
      
      v_i_name = 'TUA1'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,TUA1_dim,npar,ivolu) ! Tof Unilate Al cover 1
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)
      
      v_i_name = 'TUA2'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,TUA2_dim,npar,ivolu) ! Tof Unilate Al cover 2
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)
      
      v_i_name = 'TFRU'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,TFRU_dim,npar,ivolu) ! ToF Rail Upper
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)
      
      v_i_name = 'TFRL'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,TFRL_dim,npar,ivolu) ! ToF Rail Lower
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',1)
cak
ckn   new definition of pseudo volume(TFR1,2,3,4) for placing side rack 
ckn      for HV distr. box etc,,,     (Kunio Koseki,,,5/14/98)     
      v_i_name = 'TFR1'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pTFR1_dim,npar,ivolu)
      call GSATT(v_i_name,'SEEN',0)
      call GSATT(v_i_name,'COLO',1)

      v_i_name = 'TFR2'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pTFR2_dim,npar,ivolu)
      call GSATT(v_i_name,'SEEN',0)
      call GSATT(v_i_name,'COLO',1)

      v_i_name = 'TFR3'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pTFR3_dim,npar,ivolu)
      call GSATT(v_i_name,'SEEN',0)
      call GSATT(v_i_name,'COLO',1)

      v_i_name = 'TFR4'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pTFR4_dim,npar,ivolu)
      call GSATT(v_i_name,'SEEN',0)
      call GSATT(v_i_name,'COLO',1)

      v_i_name = 'HVRC'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,HVRC_dim,npar,ivolu)
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'SRTP'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pSRTP_dim,npar,ivolu) !top of rack
      call GSATT(v_i_name,'SEEN',0)
      call GSATT(v_i_name,'COLO',3)

      v_i_name = 'SRBT'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,SRBT_dim,npar,ivolu) !botom of rack
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'STPS'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,STPS_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'STPM'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,STPM_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'SRSD'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,SRSD_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'HVMV'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pHVMV_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',0)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'HVM2'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pHVM2_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',0)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'HVDB'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pHVDB_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',0)
      call GSATT(v_i_name,'COLO',4)  

      v_i_name = 'HVLS'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,HVLS_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'HVSS'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,HVSS_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'HVBT'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,HVBT_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'HPCN'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',24,HPCN_dim,npar,ivolu) !G10 box
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4) 

      v_i_name = 'RACL'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,RACL_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'RACS'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,RACS_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'SRCB'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',cable_med,SRCB_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'SCUP'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',cable_med,SCUP_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4) 

      v_i_name = 'SCLW'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',cable_med,SCLW_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4) 
     
      v_i_name = 'CFLS'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,CFLS_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4) 

      v_i_name = 'CFSS'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',26,CFSS_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4) 

      v_i_name = 'CFBX'
      npar     = 4
      call GSVOLU(v_i_name,'BOX ',6,pCFBX_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      v_i_name = 'CFMV'
      npar     = 3
      call GSVOLU(v_i_name,'BOX ',6,pCFMV_dim,npar,ivolu) 
      call GSATT(v_i_name,'SEEN',1)
      call GSATT(v_i_name,'COLO',4)

      ! Definition of rotation matrix
      irot_tof  = irot+1                  ! start of rotation matrix index
      irot=irot+30                        ! reserve 30 rotation matrix
      call GSROTM(irot_tof,90.,0.,90.,90.,0.,0.) ! no rotation
      call GSROTM(irot_tof+1,90.,90.,90.,180.,0.,0.) ! +90 deg around z-axis
      call GSROTM(irot_tof+2,90.,270.,135.,0.,45.,0.) ! -90z x -45x
      call GSROTM(irot_tof+3,90.,90.,90.,0.,180.,0.) ! +90z x +180x
      call GSROTM(irot_tof+4,0.,0.,90.,0.,90.,-90.) ! x->z y->x z->-y
      call GSROTM(irot_tof+5,0.,0.,90.,0.,90.,90.) ! x->z y->x z->y
      call GSROTM(irot_tof+6,90.,180.,90.,270.,0.,0.) ! +180z
      call GSROTM(irot_tof+7,90.,0.,180.,0.,90.,90.) ! +90x for 90deg PMT
      call GSROTM(irot_tof+8,90.,180.,90.,90.,180.,0.) ! +180y
      call GSROTM(irot_tof+9,90.,0.,90.,90.,180.,0.) ! z -> -z
      call GSROTM(irot_tof+10,90.,0.+22.5, 90.,90.+22.5, 0.,0.) ! +22.5z
      call GSROTM(irot_tof+11,90.,180.+22.5, 90.,90.+22.5, 180.,0.) ! +180y & +22.5z

cc  irot_tof+9 -- +19 is defined with just before GSPOS(TFPN,,TFWG,....)
cc (10/29/97 Akio Kiyomichi)

c---> Positioning media into mother volumes

      v_m_name  =  'TFA1'	! mother volum
c     ========================================
c TFAH: hollow of alumi.frame
      v_i_name  =  'TFAH'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=0.0
      gpos(3)=0.0
      call GSPOS('TFAH',nr,'TFA1',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'ONLY')

      v_m_name  =  'TFPN'       ! mother volum
c     ========================================
c TFA1: alumi.frame side
      v_i_name  =  'TFA1'       ! daughter volume
      
      nr    =  1            ! Copy number
      gpos(1)=pTFPN_dim(1)-TFAC_dim(1)*2-TFCB_dim(1)*2-TFA1_dim(1)
      gpos(2)=0.0
      gpos(3)=TFA1_dim(3)-TFA2_dim(3)        ! left side of pseudo volume
      call GSPOS('TFA1',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'MANY')
        
      nr        =  2            ! Copy number
      gpos(3)=-1.*(TFA1_dim(3)-TFA2_dim(3))  ! right side of pseudo volume
      call GSPOS('TFA1',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'MANY')
c TFA2: alumi.frame 
c              Alumi. frame size is modified (10/31/97 Akio Kiyomichi)
c              Alumi. frame size is modified (11/30/98 Akio Kiyomichi)
      v_i_name  =  'TFA2'       ! daughter volume
      nr        =  1            ! Copy number
      
      gpos(1)=TFA2_dim(1)-pTFPN_dim(1)       ! on the wall of pseudo volume
      gpos(2)=TFA2_dim(2)+pHCMB_dim(2)       ! top of the pseudo volume
      gpos(3)=0.0
      call GSPOS('TFA2',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')
                  
      nr        =  2            ! Copy number
      gpos(2)=-1.*(TFA2_dim(2)+pHCMB_dim(2)) ! bottom of the pseudo volume
      call GSPOS('TFA2',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')
c TFA3: alumi.frame 

      v_i_name  =  'TFA3'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=TFA3_dim(1)+TFA2_dim(1)*2.-pTFPN_dim(1)
      gpos(2)=TFA3_dim(2)+pHCMB_dim(2)
      gpos(3)=-21.1
      call GSPOS('TFA3',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')
      nr        =  2            ! Copy number
      gpos(3)=19.6
      call GSPOS('TFA3',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')
      nr        =  3            ! Copy number
      gpos(2)=-1.*(TFA3_dim(2)+pHCMB_dim(2))
      gpos(3)=21.1
      call GSPOS('TFA3',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')
      nr        =  4            ! Copy number
      gpos(3)=-19.6
      call GSPOS('TFA3',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')

c TFAC: Al. cover
      v_i_name  =  'TFAC'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=pTFPN_dim(1)-TFAC_dim(1)
      gpos(2)=0.0
      gpos(3)=0.0
      call GSPOS('TFAC',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')
                  
c TFCB: cable
c             Cable size is modified (10/31/97 Akio Kiyomichi)
      v_i_name  =  'TFCB'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=pTFPN_dim(1)-TFAC_dim(1)*2.-TFCB_dim(1)
      gpos(2)=0.0
      gpos(3)=0.0
      call GSPOS('TFCB',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')

cak Nov-30-98
      v_m_name  =  'TFBP'       ! mother volum
c     ========================================
c TUNI: fiber plate
      v_i_name  =  'TUNI'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=TUNI_dim(1)-pTFBP_dim(1)
      gpos(2)=0.0
      gpos(3)=0.0
      call GSPOS('TUNI',nr,'TFBP',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'MANY')
                  
c TUA1: fiber cover 1
      v_i_name  =  'TUA1'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=TUA1_dim(1) + 2*TUA2_dim(1) + 2*TUNI_dim(1) - pTFBP_dim(1)
      gpos(2)=0.0
      gpos(3)=0.0
      call GSPOS('TUA1',nr,'TFBP',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'MANY')
                  
c TUA2: fiber cover 2
      v_i_name  =  'TUA2'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=TUA2_dim(1) + 2*TUNI_dim(1) - pTFBP_dim(1)
      gpos(2)=TUA1_dim(2)
      gpos(3)=0.0
      call GSPOS('TUA2',nr,'TFBP',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'MANY')
      nr        =  2            ! Copy number
      gpos(2)=-TUA1_dim(2)
      call GSPOS('TUA2',nr,'TFBP',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'MANY')

      v_m_name  =  'PMAS'       ! mother volum
c     ========================================
c MMTL: Mu-metal shield
      v_i_name  =  'MMTL'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=MMTL_dim(2)-pPMAS_dim(2) ! bottom of the pseudo volume
      gpos(3)=MMTL_dim(3)-pPMAS_dim(3) ! on the wall of pseudo volume
      call GSPOS('MMTL',nr,'PMAS',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'ONLY')
                
c PMT: PMT itself
      v_i_name  =  'PMT '       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=MMTL_dim(2)-pPMAS_dim(2) !center of MMTL
      gpos(3)=PMT_dim(3)-pPMAS_dim(3) ! on the wall of pseudo volume
      call GSPOS('PMT ',nr,'PMAS',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'ONLY')
        
c SCKT: PMT socket
      v_i_name  =  'SCKT'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=MMTL_dim(2)-pPMAS_dim(2)  !center of MMTL
      gpos(3)=SCKT_dim(3)+2.*PMT_dim(3)-pPMAS_dim(3) ! on the PMT end
      call GSPOS('SCKT',nr,'PMAS',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'ONLY')
        
c BASE: PMT base G10 board
      v_i_name  =  'BASE'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=BASE_dim(2)-pPMAS_dim(2) !bottom of pseudo volume
      gpos(3)=BASE_dim(3)+2*SCKT_dim(3)+2.*PMT_dim(3)-pPMAS_dim(3)! PMT end
      call GSPOS('BASE',nr,'PMAS',gpos(1),gpos(2),gpos(3),
     &          irot_tof,'ONLY')


      v_m_name  =  'SLTS'       ! mother volum
c     ========================================
c SCTS: scinti. short
      v_i_name  =  'SCTS'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=SCTS_dim(2)-pSLTS_dim(2) ! bottom of the pseudo volume
      gpos(3)=0.0
      call GSPOS('SCTS',nr,'SLTS',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'MANY')
                
c LGPR: prism shape light guide 	
      v_i_name  =  'LGPR'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=LGPR_dim(2)-pSLTS_dim(2) ! bottom of the pseudo volume
      gpos(3)=-1.*(SCTS_dim(3)+LGPR_dim(4))
      call GSPOS('LGPR',nr,'SLTS',gpos(1),gpos(2),gpos(3),
     &  irot_tof+1,'MANY')
                  
c LGST: 90deg light guide (substitute at this moment)
      v_i_name  =  'LGST'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=LGST_dim(2)-pSLTS_dim(2) ! bottom of psudo volume
      gpos(3)=SCTS_dim(3)+LGST_dim(3)
      call GSPOS('LGST',nr,'SLTS',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'MANY')
        
c PMAS: left side PMT
      v_i_name  =  'PMAS'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=pPMAS_dim(2)+2.*SCTS_dim(2)-pSLTS_dim(2) ! top of scintillator
      gpos(3)=pPMAS_dim(3)-SCTS_dim(3)                 ! on light guide face
      call GSPOS('PMAS',nr,'SLTS',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'MANY')
        
c PMAS: right side 90 degree PMT
      v_i_name  =  'PMAS'       ! daughter volume
      nr        =  2            ! Copy number
      gpos(1)=0.0
      gpos(2)=pPMAS_dim(3)+2.*LGST_dim(2)-pSLTS_dim(2) ! top of scintillator
      gpos(3)=SCTS_dim(3)+LGST_dim(3)-pPMAS_dim(2)+MMTL_dim(2)!on LGST face
      call GSPOS('PMAS',nr,'SLTS',gpos(1),gpos(2),gpos(3),
     &  irot_tof+7,'MANY')
        
c LGS1: 90deg light guide
c      v_i_name  =  'LGS1'       ! daughter volume
c      nr        =  1            ! Copy number
c      gpos(1)=0.0
c      gpos(2)=LGS1_dim(2)+SCTS_dim(2)*2.-pSLTS_dim(2) ! right above SCTS
c      gpos(3)=SCTS_dim(3)+LGS1_dim(3)
c      call GSPOS('LGS1',nr,'SLTS',gpos(1),gpos(2),gpos(3),
c     &            irot_tof,'MANY')
        
c LGS2: 90deg light guide
c      v_i_name  =  'LGS2'       ! daughter volume
c      nr        =  1            ! Copy number
c      gpos(1)=0.0
c      gpos(2)=LGS2_dim(2)*sqrt(2.)-LGS2_dim(4)/sqrt(2.)
c     &     -pSLTS_dim(2)                               ! right below LGS1
c      gpos(3)=SCTS_dim(3)+LGS2_dim(4)/sqrt(2.)
c      call GSPOS('LGS2',nr,'SLTS',gpos(1),gpos(2),gpos(3),
c     &            irot_tof+2,'MANY')


      v_m_name  =  'SLTL'       ! mother volum
c     ========================================
c SCTL: scinti. long
      v_i_name  =  'SCTL'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=SCTL_dim(2)-pSLTL_dim(2) ! bottom of the pseudo volume
      gpos(3)=0.0
      call GSPOS('SCTL',nr,'SLTL',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'MANY')
                
c LGPR: prism shape light guide 	
      v_i_name  =  'LGPR'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=LGPR_dim(2)-pSLTL_dim(2) ! bottom of the pseudo volume
      gpos(3)=-1.*(SCTL_dim(3)+LGPR_dim(4))
      call GSPOS('LGPR',nr,'SLTL',gpos(1),gpos(2),gpos(3),
     &  irot_tof+1,'MANY')
                  
c LGPR: prism shape light guide the other side	
      v_i_name  =  'LGPR'       ! daughter volume
      nr        =  2            ! Copy number
      gpos(1)=0.0
      gpos(2)=LGPR_dim(2)-pSLTL_dim(2) ! bottom of the pseudo volume
      gpos(3)=SCTL_dim(3)+LGPR_dim(4)
      call GSPOS('LGPR',nr,'SLTL',gpos(1),gpos(2),gpos(3),
     &  irot_tof+3,'MANY')

c PMAS: left side PMT
      v_i_name  =  'PMAS'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=pPMAS_dim(2)+2.*SCTL_dim(2)-pSLTL_dim(2) ! top of scintillator
      gpos(3)=pPMAS_dim(3)-SCTL_dim(3)                 ! on light guide face
      call GSPOS('PMAS',nr,'SLTL',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'MANY')
        
c PMAS: right side PMT
      v_i_name  =  'PMAS'       ! daughter volume
      nr        =  2            ! Copy number
      gpos(1)=0.0
      gpos(2)=pPMAS_dim(2)+2.*SCTL_dim(2)-pSLTL_dim(2) ! top of scintillator
      gpos(3)=-1.*(pPMAS_dim(3)-SCTL_dim(3))             ! on light guide face
      call GSPOS('PMAS',nr,'SLTL',gpos(1),gpos(2),gpos(3),
     &  irot_tof+8,'MANY')
                  
      v_m_name  =  'CLMN'       ! mother volum
c     ========================================

      scintz(1)=pCLMN_dim(3)-(send_gap+SCTS_dim(3)) ! short from positive z
      scintz(2)=scintz(1)-(SCTS_dim(3)+btwn_gap+SCTL_dim(3)) ! long
      scintz(3)=scintz(2)-(SCTL_dim(3)+btwn_gap+SCTL_dim(3)) ! long

      gpos(1)=0.0
      gpos(2)=0.0               ! mother and daughter have same height
      gpos(3)=scintz(2)
      v_i_name  =  'SLTL'       ! daughter volume
      nr        =  1            ! Copy number
      call GSPOS('SLTL',nr,'CLMN',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'MANY')
        
      gpos(1)=0.0
      gpos(2)=0.0               ! mother and daughter have same height
      gpos(3)=scintz(3)
      nr        =  2            ! Copy number
      call GSPOS('SLTL',nr,'CLMN',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'MANY')

      v_i_name  =  'SLTS'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=0.0               ! mother and daughter have same height
      gpos(3)=scintz(1)
      call GSPOS('SLTS',nr,'CLMN',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'MANY')

      v_m_name  =  'HCMB'       ! mother volum
c     ========================================
c  CRBN-> HCMB: prism shape light guide 	
      v_i_name  =  'CRBN'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=pHCMB_dim(1)-CRBN_dim(1)
      gpos(2)=0.0
      gpos(3)=0.0
      call GSPOS('CRBN',nr,'HCMB',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')
      nr        =  2            ! Copy number
      gpos(1)=-1.*gpos(1)
      call GSPOS('CRBN',nr,'HCMB',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')


      v_m_name  =  'TFPN'       ! mother volum
c     ========================================
c  HCMB-> TFPN: Honey Comb to the panel 	
      v_i_name  =  'HCMB'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=pHCMB_dim(1)-pTFPN_dim(1)
      gpos(2)=0.0
      gpos(3)=0.0
      call GSPOS('HCMB',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')


      v_m_name  =  'TFPN'       ! mother volum
c     ========================================
c  CLMN-> TFPN: slat column to the panel
      v_i_name  =  'CLMN'       ! daughter volume
      nr        =  1            ! Copy number
      do iclmn=1,32
        gpos(1)= pHCMB_dim(1)*2.+pCLMN_dim(2)-pTFPN_dim(1)
        gpos(2)=0.0
        gpos(3)=(float(iclmn-16)*2-1)*slat_halfwidth
        if(mod(iclmn,2).eq.1)then
          call GSPOS('CLMN',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &            irot_tof+4,'MANY')
        else
          call GSPOS('CLMN',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &            irot_tof+5,'MANY')
        endif
        nr=nr+1
      enddo
cak Nov-30-98
c  TFBP-> TFPN: fiber plate to the panel 
      v_i_name  =  'TFBP'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1) = pHCMB_dim(1)*2.+SCTL_dim(2)*2.+pTFBP_dim(1)-pTFPN_dim(1)
      gpos(2) = -((scintz(1)-SCTS_dim(3))-(scintz(3)-SCTL_dim(3)))/2.
      gpos(3) = 0.0
      call GSPOS('TFBP',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'MANY')
        
      nr        =  2            ! Copy number
      gpos(2) = 0.0
      call GSPOS('TFBP',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'MANY')

      nr        =  3            ! Copy number
      gpos(2) = ((scintz(1)-SCTS_dim(3))-(scintz(3)-SCTL_dim(3)))/2.
      call GSPOS('TFBP',nr,'TFPN',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'MANY')

ckn
      v_m_name  =  'TFR1'       ! mother volum
c     ========================================
c  SRBT-> TFR1: botom of rack to the TFR1 	
      v_i_name  =  'SRBT'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=SRBT_dim(1)-pTFR1_dim(1)
      gpos(2)=0.0
      gpos(3)=0.0 
      call GSPOS('SRBT',nr,'TFR1',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')

      v_m_name  =  'TFR2'       ! mother volum
c     ========================================
c  SRBT-> TFR2: botom of rack to the TFR2 	
      v_i_name  =  'SRBT'       ! daughter volume
      nr        =  2            ! Copy number
      gpos(1)=SRBT_dim(1)-pTFR2_dim(1)
      gpos(2)=0.0
      gpos(3)=0. 
      call GSPOS('SRBT',nr,'TFR2',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')

       v_m_name  =  'TFR3'       ! mother volum
c     ========================================
c  SRBT-> TFR1: botom of rack to the TFR1 	
      v_i_name  =  'SRBT'       ! daughter volume
      nr        =  3            ! Copy number
      gpos(1)=SRBT_dim(1)-pTFR3_dim(1)
      gpos(2)=0.0
      gpos(3)=0.0 
      call GSPOS('SRBT',nr,'TFR3',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')

      v_m_name  =  'TFR4'       ! mother volum
c     ========================================
c  SRBT-> TFR2: botom of rack to the TFR2 	
      v_i_name  =  'SRBT'       ! daughter volume
      nr        =  4            ! Copy number
      gpos(1)=SRBT_dim(1)-pTFR4_dim(1)
      gpos(2)=0.0
      gpos(3)=0. 
      call GSPOS('SRBT',nr,'TFR4',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')

      v_m_name  =  'TFR1'       ! mother volum
c     ========================================
c  HVRC-> TFR1: rack boad for HV Distr box 
      v_i_name  =  'HVRC'       ! daughter volume
      nr        =  1            ! Copy number
      do n_rc = 1,3
         if(n_rc.eq.1)then
            gpos(1)=4.9
            gpos(2)=0.
            gpos(3)=27.5 + (1 - n_rc)*55.
         elseif(n_rc.eq.2)then
            gpos(1)=-4.9
            gpos(2)=0.0
            gpos(3)=27.5 + (1 - n_rc)*55.
         else
            gpos(1)=-4.9-2.2
         endif
         call GSPOS('HVRC',nr,'TFR1',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
         nr = nr + 1
      enddo

         v_m_name  =  'TFR2'    ! mother volum
c     ========================================
c     HVRC-> TFR2: rack boad for HV Distr box 
         v_i_name  =  'HVRC'    ! daughter volume
         nr        =  4         ! Copy number
         do n_rc = 1,3
            if(n_rc.eq.1)then
               gpos(1)=-4.9
               gpos(2)=0.
               gpos(3)=27.5 + (1 - n_rc)*55.
            elseif(n_rc.eq.2)then
               gpos(1)=-4.9-2.2
               gpos(2)=0.
               gpos(3)=27.5 + (2 - n_rc)*55.
            else
               gpos(1)=4.9
               gpos(2)=0.0
               gpos(3)=27.5 + (2 - n_rc)*55.
            endif
            call GSPOS('HVRC',nr,'TFR2',gpos(1),gpos(2),gpos(3),
     &           irot_tof,'ONLY')
            nr = nr + 1
         enddo

         v_m_name  =  'TFR3'       ! mother volum
c     ========================================
c  HVRC-> TFR1: rack boad for HV Distr box 
      v_i_name  =  'HVRC'       ! daughter volume
      nr        =  7            ! Copy number
      do n_rc = 1,3
         if(n_rc.eq.1)then
            gpos(1)=4.9
            gpos(2)=0.
            gpos(3)=27.5 + (1 - n_rc)*55.
         elseif(n_rc.eq.2)then
            gpos(1)=-4.9
            gpos(2)=0.0
            gpos(3)=27.5 + (1 - n_rc)*55.
         else
            gpos(1)=-4.9-2.2
         endif
         call GSPOS('HVRC',nr,'TFR3',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
         nr = nr + 1
      enddo

         v_m_name  =  'TFR4'    ! mother volum
c     ========================================
c     HVRC-> TFR2: rack boad for HV Distr box 
         v_i_name  =  'HVRC'    ! daughter volume
         nr        =  10         ! Copy number
         do n_rc = 1,3
            if(n_rc.eq.1)then
               gpos(1)=-4.9
               gpos(2)=0.
               gpos(3)=27.5 + (1 - n_rc)*55.
            elseif(n_rc.eq.2)then
               gpos(1)=-4.9-2.2
               gpos(2)=0.
               gpos(3)=27.5 + (2 - n_rc)*55.
            else
               gpos(1)=4.9
               gpos(2)=0.0
               gpos(3)=27.5 + (2 - n_rc)*55.
            endif
            call GSPOS('HVRC',nr,'TFR4',gpos(1),gpos(2),gpos(3),
     &           irot_tof,'ONLY')
            nr = nr + 1
         enddo
     
         v_m_name  =  'TFR1'    ! mother volum
c     ========================================
c  SRSD-> TFR1: side of rack to the TFR1 	
      v_i_name  =  'SRSD'       ! daughter volume
      nr        = 1             ! Copy number
      do n_sd = 1, 2
         gpos(1)=0.0
         gpos(2)=-96.5 + ( n_sd -1 )*193.
         gpos(3)=0.0
         nr = nr + 1
         call GSPOS('SRSD',nr,'TFR1',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
      enddo

      v_m_name  =  'TFR3'       ! mother volum
c     ========================================
c  SRSD-> TFR1: side of rack to the TFR1 	
      v_i_name  =  'SRSD'       ! daughter volume
      nr        = 3            ! Copy number
      do n_sd = 1, 2 
         gpos(1)=0.0
         gpos(2)=-96.5 + ( n_sd - 1)*193.
         gpos(3)=0.0
         call GSPOS('SRSD',nr,'TFR3',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
         nr = nr + 1
      enddo

      v_m_name  =  'TFR2'       ! mother volum
c     ========================================
c  SRSD-> TFR2: side of rack to the TFR2 	
      v_i_name  =  'SRSD'       ! daughter volume
      nr        = 5            ! Copy number
      do n_sd = 1,2
         gpos(1)=0.0
         gpos(2)=-96.5 + (n_sd-1)*193.
         gpos(3)=0.0
         call GSPOS('SRSD',nr,'TFR2',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
         nr = nr + 1
      enddo

      v_m_name  =  'TFR4'       ! mother volum
c     ========================================
c  SRSD-> TFR2: side of rack to the TFR2	
      v_i_name  =  'SRSD'       ! daughter volume
      nr        = 7            ! Copy number
      do n_sd = 1,2
         gpos(1)=0.0
         gpos(2)=-96.5 + (n_sd-1)*193.
         gpos(3)=0.0
         call GSPOS('SRSD',nr,'TFR4',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
         nr = nr + 1 
      enddo

      v_m_name  =  'TFR2'       ! mother volum
c     ========================================
c  SRTP-> TFR2: botom of rack to the TFR2 	
      v_i_name  =  'SRTP'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=7.75
      gpos(2)=0.0
      gpos(3)=0.0
      call GSPOS('SRTP',nr,'TFR2',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')

      v_m_name  =  'TFR4'       ! mother volum
c     ========================================
c  SRTP-> TFR4: botom of rack to the TFR2 	
      v_i_name  =  'SRTP'       ! daughter volume
      nr        =  2            ! Copy number
      gpos(1)=7.75
      gpos(2)=0.0
      gpos(3)=0.0
      call GSPOS('SRTP',nr,'TFR4',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')

      v_m_name  =  'TFR1'       ! mother volum
c     ========================================
c  SRTP-> TFR1: botom of rack to the TFR1 	
      v_i_name  =  'SRTP'       ! daughter volume
      nr        =  3            ! Copy number
      gpos(1)=7.75
      gpos(2)=0.0
      gpos(3)=0.0
c      call GSROTM(1,90.,0.,90.,90.,180.,0.)
      call GSPOS('SRTP',nr,'TFR1',gpos(1),gpos(2),gpos(3),
     &            irot_tof+9,'ONLY')

      v_m_name  =  'TFR3'       ! mother volum
c     ========================================
c  SRTP-> TFR3: botom of rack to the TFR2 	
      v_i_name  =  'SRTP'       ! daughter volume
      nr        =  4            ! Copy number
      gpos(1)=7.75
      gpos(2)=0.0
      gpos(3)=0.0
c      call GSROTM(1,90.,0.,90.,90.,180.,0.)
      call GSPOS('SRTP',nr,'TFR3',gpos(1),gpos(2),gpos(3),
     &            irot_tof+9,'ONLY')

      v_m_name  =  'SRTP'       ! mother volum
c     ========================================
c  STPM-> SRTP: midle of top to the SRTP 	
      v_i_name  =  'STPM'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=0.0
      gpos(3)=10.
      call GSPOS('STPM',nr,'SRTP',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY') 

      v_m_name  =  'SRTP'       ! mother volum
c     ========================================
c  STPS-> SRTP: side of top to the SRTP 	
      v_i_name  =  'STPS'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1)=0.0
      gpos(2)=80.275
      gpos(3)=0.0
      call GSPOS('STPS',nr,'SRTP',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY') 

      v_m_name  =  'SRTP'       ! mother volum
c     ========================================
c  STPS-> SRTP: side of top to the SRTP 	
      v_i_name  =  'STPS'       ! daughter volume
      nr        =  2            ! Copy number
      gpos(1)=0.0
      gpos(2)=-80.275
      gpos(3)=0.0
      call GSPOS('STPS',nr,'SRTP',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')

      v_m_name  =  'TFR1'       ! mother volum
c     ========================================
c  HVMV-> TFR1: mother volume for HV box
      v_i_name  =  'HVMV'       ! daughter volume
      nr        = 1            ! Copy number
      gpos(1)=0.0
      gpos(2)=0.0
      gpos(3)=22.5
      call GSPOS('HVMV',nr,'TFR1',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')

      v_m_name  =  'TFR3'       ! mother volum
c     ========================================
c  HVMV-> TFR3: mother volume for HV box
      v_i_name  =  'HVM2'       ! daughter volume
      nr        = 3            ! Copy number
      gpos(1)=0.0
      gpos(2)=0.0
      gpos(3)=22.5
      call GSPOS('HVM2',nr,'TFR3',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')

      v_m_name  =  'HVMV'       ! mother volum
c     ========================================
c     signal cable for upper sector
      v_i_name  =  'SCUP'       ! daughter volume
      nr        = 1             ! Copy number
      do n_sc = 1 , 2
         gpos(1)=0.0
         gpos(2)=-80.0 + ( n_sc -1) * 160.
         gpos(3)=0.
         call GSPOS('SCUP',nr,'HVMV',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
         nr = nr + 1
      enddo

      v_m_name  =  'HVM2'       ! mother volum
c     ========================================
c     signal cable for upper sector
      v_i_name  =  'SCLW'       ! daughter volume
      nr        = 1             ! Copy number
         gpos(1)=0.0
         gpos(2)=21.05         !-40.0
         gpos(3)=0.
         call GSPOS('SCLW',nr,'HVM2',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')

      v_m_name  =  'HVMV'       ! mother volum
c     ========================================
c  HVDB-> HVMV: 
      v_i_name  =  'HVDB'       ! daughter volume
      nr        = 1            ! Copy number
      do n_box = 1 , 4
         gpos(1)=0.
         gpos(2)=49.95 - (n_box - 1)*33.3
         gpos(3)=0.
         call GSPOS('HVDB',nr,'HVMV',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
         nr  = nr + 1
      enddo

      v_m_name  =  'HVM2'       ! mother volum
c     ========================================
c  HVDB-> HVMV: 
      v_i_name  =  'HVDB'       ! daughter volume
      nr        = 5            ! Copy number
         gpos(1)=0.
         gpos(2)=70.            !-49.95 
         gpos(3)=0.
         call GSPOS('HVDB',nr,'HVM2',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')

      v_m_name  =  'HVDB'       ! mother volum
c     ========================================
c     HVBT-> HVDB: 
      v_i_name  =  'HVBT'       ! daughter volume
      nr        = 1             ! Copy number
      do n_bt = 1,2
         gpos(1)=4.3 - (n_bt-1)*8.6
         gpos(2)=0.
         gpos(3)=0.
         call GSPOS('HVBT',nr,'HVDB',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')    
         nr = nr + 1
      enddo

      v_m_name  =  'HVDB'       ! mother volum
c     ========================================
c     HVLS-> HVDB: 
      v_i_name  =  'HVLS'       ! daughter volume
      nr        = 1             ! Copy number
      do n_ls = 1,2
         gpos(1)=0.
         gpos(2)=0.
         gpos(3)=9.9 - (n_ls-1)*19.8
         call GSPOS('HVLS',nr,'HVDB',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
         nr = nr + 1
      enddo

      v_m_name  =  'HVDB'       ! mother volum
c     ========================================
c     HVSS-> HVDB: 
      v_i_name  =  'HVSS'       ! daughter volume
      nr        = 1             ! Copy number
      do n_ss = 1,2
         gpos(1)=0.
         gpos(2)=16.4 - (n_ss - 1)*32.8
         gpos(3)=0.
         call GSPOS('HVSS',nr,'HVDB',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
         nr = nr + 1
      enddo  

      v_m_name  =  'TFR2'       ! mother volum
c     ========================================
c  HVMV-> TFR2: mother volume for HV box
      v_i_name  =  'HVMV'       ! daughter volume
      nr        = 2            ! Copy number
      gpos(1)=0.               
      gpos(2)=0.
      gpos(3)=-22.5
      call GSPOS('HVMV',nr,'TFR2',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')

      v_m_name  =  'TFR4'       ! mother volum
c     ========================================
c  HVMV-> TFR2: mother volume for HV box
      v_i_name  =  'HVM2'       ! daughter volume
      nr        = 4            ! Copy number
      gpos(1)=0.               
      gpos(2)=0.
      gpos(3)=-22.5
      call GSPOS('HVM2',nr,'TFR4',gpos(1),gpos(2),gpos(3),
     &            irot_tof,'ONLY')

      v_m_name  =  'HVDB'       ! mother volum
c     ========================================
c     HPCN-> HVDB: connecter for housing into HVDB  
      v_i_name  =  'HPCN'       ! daughter volume
      nr        = 1             ! Copy number
      do n_cn = 1,16
         gpos(1)=2.55
         if(n_cn.le.8)then
            gpos(2)=13.3 - (n_cn-1)*3.8
            gpos(3)=5.0
         else
            gpos(2)=13.3 - (n_cn-9)*3.8
            gpos(3)=-5.0
         endif
         call GSPOS('HPCN',nr,'HVDB',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
         nr = nr + 1
      enddo

      v_m_name  =  'TFR1'       ! mother volum
c     ========================================
c  RACL-> TFR1: LONG PART of RACK FOR CABLE
      v_i_name  =  'RACL'       ! daughter volume
      nr        = 1            ! Copy number
      gpos(1)=2.7
      gpos(2)=34.025            !-34.8 + 0.775
      gpos(3)=0.
      call GSPOS('RACL',nr,'TFR1',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY') 

      v_m_name  =  'TFR3'       ! mother volum
c     ========================================
c  RACL-> TFR1: LONG PART of RACK FOR CABLE
      v_i_name  =  'RACL'       ! daughter volume
      nr        = 3            ! Copy number
      gpos(1)=2.7
      gpos(2)=34.025            !-34.8 + 0.775
      gpos(3)=0.
      call GSPOS('RACL',nr,'TFR3',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY')

      v_m_name  =  'TFR1'       ! mother volum
c     ========================================
c     RACS-> TFR1: SHORT RACK FOR CABLE
      v_i_name  =  'RACS'       ! daughter volume
      nr        = 1             ! Copy number
      gpos(1)=2.7
      gpos(2)=-62.425           !63.2 - 0.775
      gpos(3)=0.
      call GSPOS('RACS',nr,'TFR1',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY') 

      v_m_name  =  'TFR3'       ! mother volum
c     ========================================
c     RACS-> TFR1: SHORT RACK FOR CABLE
      v_i_name  =  'RACS'       ! daughter volume
      nr        = 3             ! Copy number
      gpos(1)=2.7
      gpos(2)=-62.425           !63.2 - 0.775
      gpos(3)=0.
      call GSPOS('RACS',nr,'TFR3',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY')

      v_m_name  =  'TFR2'       ! mother volum
c     ========================================
c     RACL-> TFR1: LONG PART RACK FOR CABLE
      v_i_name  =  'RACL'       ! daughter volume
      nr        = 2             ! Copy number
      gpos(1)=2.7
      gpos(2)=34.025            !-34.8 + 0.775
      gpos(3)=0.
      call GSPOS('RACL',nr,'TFR2',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY') 

      v_m_name  =  'TFR4'       ! mother volum
c     ========================================
c     RACL-> TFR1: LONG PART RACK FOR CABLE
      v_i_name  =  'RACL'       ! daughter volume
      nr        = 4             ! Copy number
      gpos(1)=2.7
      gpos(2)=34.025            !-34.8 + 0.775
      gpos(3)=0.
      call GSPOS('RACL',nr,'TFR4',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY')

      v_m_name  =  'TFR2'       ! mother volum
c     ========================================
c     RACS-> TFR1: SHORT RACK FOR CABLE
      v_i_name  =  'RACS'       ! daughter volume
      nr        = 2             ! Copy number
      gpos(1)=2.7
      gpos(2)=-62.425           !63.2 - 0.775
      gpos(3)=0.
      call GSPOS('RACS',nr,'TFR2',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY')

      v_m_name  =  'TFR4'       ! mother volum
c     ========================================
c     RACS-> TFR1: SHORT RACK FOR CABLE
      v_i_name  =  'RACS'       ! daughter volume
      nr        = 4             ! Copy number
      gpos(1)=2.7
      gpos(2)=-62.425           !63.2 - 0.775
      gpos(3)=0.
      call GSPOS('RACS',nr,'TFR4',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY')

      v_m_name  =  'TFR1'       ! mother volum
c     ========================================
c     SRCB-> TFR1: HV/SIG CABLE... 
      v_i_name  =  'SRCB'       ! daughter volume
      nr        = 1             ! Copy number
      gpos(1)=-6.12
      gpos(2)=0.
      gpos(3)=-9.9
      call GSPOS('SRCB',nr,'TFR1',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY')

      v_m_name  =  'TFR3'       ! mother volum
c     ========================================
c     SRCB-> TFR1: HV/SIG CABLE... 
      v_i_name  =  'SRCB'       ! daughter volume
      nr        = 3             ! Copy number
      gpos(1)=-6.12
      gpos(2)=0.
      gpos(3)=-9.9
      call GSPOS('SRCB',nr,'TFR3',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY')

      v_m_name = 'TFR2'         !mother volum
c     ===========================================
c     SRCB-> TFR2:HV/SIG CABLE...
      v_i_name = 'SRCB'         !daughter volume
      nr       = 2              !Copy number
      gpos(1)  =-6.12
      gpos(2)  =0.
      gpos(3)  =9.9
      call GSPOS('SRCB',nr,'TFR2',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY')

      v_m_name = 'TFR4'         !mother volum
c     ===========================================
c     SRCB-> TFR2:HV/SIG CABLE...
      v_i_name = 'SRCB'         !daughter volume
      nr       = 4              !Copy number
      gpos(1)  =-6.12
      gpos(2)  =0.
      gpos(3)  =9.9
      call GSPOS('SRCB',nr,'TFR4',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY')

      v_m_name = 'TFR1'         !mother volum
c     ===========================================
c     CFMV--->TFR1:mother volume for cooling fan onto  TFR1
      v_i_name = 'CFMV'         !daughter volume
      nr       = 1              !Copy number
      gpos(1)  =1.65
      gpos(2)  =0.
      gpos(3)  =-30.
      call GSPOS('CFMV',nr,'TFR1',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY')

      v_m_name = 'TFR3'         !mother volum
c     ===========================================
c     CFMV--->TFR1:mother volume for cooling fan onto  TFR1
      v_i_name = 'CFMV'         !daughter volume
      nr       = 3              !Copy number
      gpos(1)  =1.65
      gpos(2)  =0.
      gpos(3)  =-30.
      call GSPOS('CFMV',nr,'TFR3',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'ONLY')

      v_m_name = 'TFR2'         !mother volum
c     ===========================================
c     CFMV--->TFR1:mother volume for cooling fan onto  TFR1
      v_i_name = 'CFMV'         !daughter volume
      nr       = 2              !Copy number
      gpos(1)  =1.65
      gpos(2)  =0.
      gpos(3)  =30.
      call GSPOS('CFMV',nr,'TFR2',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'ONLY')

      v_m_name = 'TFR2'         !mother volum
c     ===========================================
c     CFMV--->TFR1:mother volume for cooling fan onto  TFR1
      v_i_name = 'CFMV'         !daughter volume
      nr       = 4              !Copy number
      gpos(1)  =1.65
      gpos(2)  =0.
      gpos(3)  =30.
      call GSPOS('CFMV',nr,'TFR4',gpos(1),gpos(2),gpos(3),
     &  irot_tof,'ONLY')

      v_m_name = 'CFMV'         !mother volum
c     ===========================================
c     CFBX--->CFMV ;real volume for cooling fan onto CFMV
      v_i_name = 'CFBX'         !daughter volume
      nr       = 1              !Copy number
      do n_bx = 1 , 14
        gpos(1)  = 0.
        gpos(2)  = 4. + 5.95 + ( n_bx - 1 )*12.
        gpos(3)  = 0.
        if( (n_bx .ge. 6 ) .and. (n_bx.le.7) )then
          gpos(2) = 77.85 + (n_bx -6)*12
        elseif( (n_bx .ge.8) .and. (n_bx.le.12) )then
          gpos(2)  = -4. - 5.95 - ( n_bx - 8 )*12.
        elseif(n_bx .ge.13)then
          gpos(2) = -(77.85 + (n_bx -13)*12)
        endif
        call GSPOS('CFBX',nr,'CFMV',gpos(1),gpos(2),gpos(3),
     &    irot_tof,'ONLY') 
        nr = nr + 1
      enddo
      
      v_m_name = 'CFBX'         !mother volum
c     ===========================================
c     CFLS--->CFBX:REAL volume for cooling fan onto CFBX
      v_i_name = 'CFLS'         !daughter volume
      nr       = 1              !Copy number
      do n_l = 1,2
         gpos(1)  =0.
         gpos(2)  =-5.85 + (n_l - 1) * 11.7
         gpos(3)  =0.
         call GSPOS('CFLS',nr,'CFBX',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
         nr = nr + 1
      enddo

      v_m_name = 'CFBX'         !mother volum
c     ===========================================
c     CFSS--->CFBX:REAL volume for cooling fan onto CFBX
      v_i_name = 'CFSS'         !daughter volume
      nr       = 1              !Copy number
      do n_s = 1 , 2
         gpos(1)  =-5.85 + (n_s - 1) * 11.7
         gpos(2)  =0.
         gpos(3)  =0.
         call GSPOS('CFSS',nr,'CFBX',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
         nr = nr + 1
      enddo

      v_m_name = 'TFR1'         !mother volum
c     ===========================================
c     TFA4---> TFR1: rail for rack to mother
      v_i_name = 'TFA4'         !daughter volume
      nr       = 1              !Copy number
      gpos(1)  = 0.
      gpos(2)  = -(pTFR1_dim(2)-TFA4_dim(2))
      gpos(3)  = 0.
      call GSPOS('TFA4',nr,'TFR1',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
      nr       = 2              !Copy number
      gpos(2)  = (pTFR1_dim(2)-TFA4_dim(2))
      call GSPOS('TFA4',nr,'TFR1',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')

      v_m_name = 'TFR2'         !mother volum
c     ===========================================
c     TFA4---> TFR2: rail for rack to mother
      v_i_name = 'TFA4'         !daughter volume
      nr       = 3              !Copy number
      gpos(1)  = 0.
      gpos(2)  = -(pTFR2_dim(2)-TFA4_dim(2))
      gpos(3)  = 0.
      call GSPOS('TFA4',nr,'TFR2',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
      nr       = 4              !Copy number
      gpos(2)  = (pTFR2_dim(2)-TFA4_dim(2))
      call GSPOS('TFA4',nr,'TFR2',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')

      v_m_name = 'TFR3'         !mother volum
c     ===========================================
c     TFA4---> TFR3: rail for rack to mother
      v_i_name = 'TFA4'         !daughter volume
      nr       = 5              !Copy number
      gpos(1)  = 0.
      gpos(2)  = -(pTFR3_dim(2)-TFA4_dim(2))
      gpos(3)  = 0.
      call GSPOS('TFA4',nr,'TFR3',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
      nr       = 6              !Copy number
      gpos(2)  = (pTFR3_dim(2)-TFA4_dim(2))
      call GSPOS('TFA4',nr,'TFR3',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')

      v_m_name = 'TFR4'         !mother volum
c     ===========================================
c     TFA4---> TFR4: rail for rack to mother
      v_i_name = 'TFA4'         !daughter volume
      nr       = 7              !Copy number
      gpos(1)  = 0.
      gpos(2)  = -(pTFR4_dim(2)-TFA4_dim(2))
      gpos(3)  = 0.
      call GSPOS('TFA4',nr,'TFR3',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
      nr       = 8              !Copy number
      gpos(2)  = (pTFR4_dim(2)-TFA4_dim(2))
      call GSPOS('TFA4',nr,'TFR3',gpos(1),gpos(2),gpos(3),
     &        irot_tof,'ONLY')
    

c ===> placing TOF panels into EMCL

c     First of all, TOF pseudo volume TFWG is splaced in EMCL
c     In PISA2000,  TOF pseudo volume TFWG is splaced in EAST(eTOF)
cc
c     Moving TOF panel from West to East (Akio Kiyomichi, 10/29/97)
c     input parameters from 'phnx.par'
c        TOFL_arms  : TOF position  East-->0, West-->1
c        TOFL_rpos  : TOF R-position
c        TFPN_dimen : (3) is panel halfwidth (Z direction)
c        TFPN_p0x(panel), TFPN_p0y(panel), TFPN_p0z(panel)
c        TFPN_p1x(panel), TFPN_p1y(panel), TFPN_p1z(panel)
c        TFPN_p2x(panel), TFPN_p2y(panel), TFPN_p2z(panel)

c     output [Center point on HCMB surface]
c        TFPN_rpos  :   R-position. calculate from TFPN_p0x - TFPN_p2z
c        TFPN_phi   : phi-position. calculate from TFPN_p0x - TFPN_p2z
c        TFPN_zpos  :   Z-position. calculate from TFPN_p0x - TFPN_p2z

c       +---+---+---+---+---+---+---+---+    :  -+ +------p2 +------p2
c       |   |   |   |   |   |   |   |   |    :   | |       | |       |
c panel | 3 | 2 | 1 | 0 | 0 | 1 | 2 | 3 |    :   | |       | |       |
c       |   |   |   |   |   |   |   |   | 1  :   | |       | |       |
c       | I | H | G | F | D | C | B | A |    :   | |       | |       |
c       |   |   |   |   |   |   |   |   |    :   | |       | |       |
c       +---+---+---+---+---+---+---+---+    :   | |   +   | |   +   |
c                   +---+---+                :   | (R,phi,Z) |       |
c                   |   |   |                :   | |       | |       |
c                   | 0 | 0 |                :   | |       | |       |
c                   |   |   |             0  :   | |       | |       |
c                   | J | E |                :   | |       | |       |
c                   |   |   |                :  -+ p1-----p0 p1-----p0
c                   +---+---+                :                    (x,y,z)
c    - - - - - - - - - - - - - - - - - -     :   North <---
c     side      1               0            : 
c            (North)         (South)         : 

c  panel_seq  = 1:2:3:4:5:6:7:8:9:10 [in PISA (Fortran style)]
c  panel_seq  = 0:1:2:3:4:5:6:7:8:9  [in PHOOL (C style)]
c  panel_char = A:B:C:D:F:G:H:I:E:J
c     sector  = 1:1:1:1:1:1:1:1:0:0
c     side    = 0:0:0:0:1:1:1:1:0:1
c     panel   = 3:2:1:0:0:1:2:3:0:0


      v_m_name  =  eTOF       ! mother volum TFWG-->EMCL 
c     ========================================
      v_i_name  =  'TFWG'       ! daughter volume
      nr        =  1            ! Copy number
      gpos(1) = 0.0
      gpos(2) = 0.0
      gpos(3) = 0.0
      if(TOFL_arms .eq. 1)then
         call GSPOS(v_i_name,nr,v_m_name,gpos(1),gpos(2),gpos(3),
     &              irot_tof+8,'ONLY') ! West
      else
         call GSPOS(v_i_name,nr,v_m_name,gpos(1),gpos(2),gpos(3),
     &              irot_tof,'ONLY')   ! East
      endif

      v_m_name  =  'TFWG'       ! mother volum TFPN-->TFWG 12/6/96
c     ========================================
      v_i_name  =  'TFPN'       ! daughter volume
      nr        =  1            ! Copy number
      npnl = npnl_sector1
      npnl_opt = npnl_sector0
      do ipnl = 1, npnl+npnl_opt
        
        pos_panel(1) = (TFPN_p1x(ipnl) + TFPN_p2x(ipnl))/2.
        pos_panel(2) = (TFPN_p1y(ipnl) + TFPN_p2y(ipnl))/2.
        pos_panel(3) = (TFPN_p1z(ipnl) + TFPN_p2z(ipnl))/2.
              
        TFPN_rpos(ipnl) = sqrt(pos_panel(1)**2 + pos_panel(2)**2)
        if(pos_panel(2).gt.0.)phi = atan(pos_panel(2)/pos_panel(1))
        if(pos_panel(2).le.0.)phi = pi+atan(pos_panel(2)/pos_panel(1))
        TFPN_phi(ipnl)  = phi*180/pi
        TFPN_zpos(ipnl) = pos_panel(3)
        
        ! rotation matrix Y p0 - p2
        if(TOFL_arms .eq. 1)then 
          ! West    
          ydiff(1) = TFPN_p2x(ipnl) - TFPN_p0x(ipnl) ! West p2-p0
          ydiff(2) = TFPN_p2y(ipnl) - TFPN_p0y(ipnl)
          ydiff(3) = TFPN_p2z(ipnl) - TFPN_p0z(ipnl)
          ydiff(4) = sqrt(ydiff(1)**2 + ydiff(2)**2)
        else                  
          ! East
          ydiff(1) = TFPN_p0x(ipnl) - TFPN_p2x(ipnl) ! East p0-p2
          ydiff(2) = TFPN_p0y(ipnl) - TFPN_p2y(ipnl)
          ydiff(3) = TFPN_p0z(ipnl) - TFPN_p2z(ipnl)
          ydiff(4) = sqrt(ydiff(1)**2 + ydiff(2)**2)
        endif

        ! compute theta and phi angles using atan2
        rot_theta(2) = atan2(ydiff(4),ydiff(3))*180./pi
        
        if(ydiff(1) .eq. 0. .and. ydiff(2) .eq. 0 ) then 
          rot_phi(2) = 0.
        else
          rot_phi(2) = atan2( ydiff(2), ydiff(1) )*180./pi
        endif
        
        !rotation matrix Z (p1 - p0)
        zdiff(1) = TFPN_p1x(ipnl) - TFPN_p0x(ipnl)
        zdiff(2) = TFPN_p1y(ipnl) - TFPN_p0y(ipnl)
        zdiff(3) = TFPN_p1z(ipnl) - TFPN_p0z(ipnl)
        
        ! scalar product of p1,p0 and p2,p0
        ! it should be 0 if you don't want GEANT to complain about
        ! re-defining rotation matrices
        scalar = 
     +    ydiff(1)*zdiff(1) +
     +    ydiff(2)*zdiff(2) +
     +    ydiff(3)*zdiff(3)
        
        ! norm of p0,p2 vector
        norm = 
     +    ydiff(1)**2 +    
     +    ydiff(2)**2 +    
     +    ydiff(3)**2
          
        ! modify zdiff so that (p0,p2) and (p0,p1) are orthogonals
        zdiff(1) = zdiff(1) - scalar*ydiff(1)/norm 
        zdiff(2) = zdiff(2) - scalar*ydiff(2)/norm 
        zdiff(3) = zdiff(3) - scalar*ydiff(3)/norm         
        zdiff(4) = sqrt(zdiff(1)**2 + zdiff(2)**2)
 
        ! compute theta and phi angles using atan2
        rot_theta(3) = atan2(zdiff(4),zdiff(3))*180./pi
        
        if(zdiff(1) .eq. 0. .and. zdiff(2) .eq. 0 ) then 
          rot_phi(3) = 0.
        else
          rot_phi(3) = atan2( zdiff(2), zdiff(1) )*180./pi
        endif      
      
        ! rotation matrix X [vector product by Y, Z]
        xdiff(1) = ydiff(2)*zdiff(3) - ydiff(3)*zdiff(2)
        xdiff(2) = ydiff(3)*zdiff(1) - ydiff(1)*zdiff(3)
        xdiff(3) = ydiff(1)*zdiff(2) - ydiff(2)*zdiff(1)
        xdiff(4) = sqrt(xdiff(1)**2 + xdiff(2)**2)
              
        ! compute theta and phi angles using atan2
        rot_theta(1) = atan2(xdiff(4),xdiff(3))*180./pi
        
        if(xdiff(1) .eq. 0. .and. xdiff(2) .eq. 0 ) then 
          rot_phi(1) = 0.
        else
          rot_phi(1) = atan2( xdiff(2),xdiff(1) )*180./pi
        endif      
        
        !panel position
        do ixyz = 1,3
          gpos(ixyz) = pos_panel(ixyz) + 
     &    pTFPN_dim(1)*xdiff(ixyz)/sqrt(xdiff(3)**2+xdiff(4)**2)
        enddo

        ! rotation
        ! make sure phi angles are positive
        if( rot_phi(1) .lt. 0 ) rot_phi(1) = rot_phi(1)+360.  
        if( rot_phi(2) .lt. 0 ) rot_phi(2) = rot_phi(2)+360.  
        if( rot_phi(3) .lt. 0 ) rot_phi(3) = rot_phi(3)+360.  
        
        call GSROTM(irot_tof+11+ipnl,rot_theta(1),rot_phi(1),
     &    rot_theta(2),rot_phi(2),rot_theta(3),rot_phi(3))
        call GSPOS(v_i_name,nr,v_m_name,gpos(1),gpos(2),gpos(3),
     &    irot_tof+11+ipnl,'ONLY') 

c         ! write-out matrix parameters, for debugging
c         write(*,100) irot_tof+11+ipnl,
c      &    rot_theta(1), rot_phi(1),
c      &    rot_theta(2), rot_phi(2),
c      &    rot_theta(3), rot_phi(3)

c  100    format( i5,3x,6(3x,f10.3) )
          
        nr = nr + 1
      enddo

c implementing second sector TOF panels( could vary according to our
c budget) For maintainer,  This change is realized by changing the 
c initialization value for npnl_opt at the declaration.

c      npnl_opt = npnl_sector0
c      do ipnl = 1,npnl_opt
c         TFPN_rpos(npnl+ipnl) = TOFL_rpos + TFPN_dr(npnl+ipnl)
c         rpos                 = TFPN_rpos(npnl+ipnl) + pTFPN_dim(1)
c         TFPN_phi(npnl+ipnl)  = 202.5 + TFPN_dphi(npnl+ipnl)
c         phi                  = TFPN_phi(npnl+ipnl)*pi/180.
c         if(mod(npnl_opt,2).eq.0)then
c            TFPN_zpos(npnl+ipnl) = TFPN_dimen(3)*
c     &           float(1-npnl_opt + 2*(ipnl-1)) + TFPN_dz(npnl+ipnl)
c         else
c            TFPN_zpos(npnl+ipnl) = TFPN_dimen(3)*
c     &           float( -npnl_opt + 2*(ipnl-1)) + TFPN_dz(npnl+ipnl)
c         endif
c         gpos(1) = rpos*cos(phi)
c         gpos(2) = rpos*sin(phi)
c         gpos(3) = TFPN_zpos(npnl+ipnl)
c         call GSROTM(irot_tof+11+npnl+ipnl,
c     &        90.,TFPN_phi(npnl+ipnl),90.,90.+TFPN_phi(npnl+ipnl),0.,0.)
c         call GSPOS(v_i_name,nr,v_m_name,gpos(1),gpos(2),gpos(3),
c     &        irot_tof+11+npnl+ipnl,'ONLY')

c    Moving from West to East (Akio Kiyomichi, 10/29/97)
c    Change to no rotation,  change -x to +x  (CFM, 11/29/96)
c      added -22.5 deg rotaion around z-axis (KK, 12/6/96)

c         nr = nr + 1
c      enddo

      v_m_name  =   'TFWG'      !mother volume  
c     =========================================      
c TFRU : TOF Rail Upper
      v_i_name  =   'TFRU'      !daughter volume for upper secter
      nr        =   1           !copy number
      pos_rail(1) = -TOFL_rpos - TFRL_dim(1)*2. + TFRU_dim(1)
      pos_rail(2) = pTFPN_dim(2)+TFRU_dim(2)
      pos_rail(3) = TFRU_dim(3)
      gpos(1) = pos_rail(1)
      gpos(2) = pos_rail(2)
      gpos(3) = pos_rail(3)
      call GSPOS('TFRU',nr,'TFWG',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY')

      nr        =   2           !copy number
      gpos(3) = -pos_rail(3)
      call GSPOS('TFRU',nr,'TFWG',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY')

      nr        =   3           !copy number
      gpos(1) = pos_rail(1)*cos(22.5*pi/180.)
     &     - pos_rail(2)*sin(22.5*pi/180.)
      gpos(2) = pos_rail(1)*sin(22.5*pi/180.)
     &     + pos_rail(2)*cos(22.5*pi/180.)
      gpos(3) = pos_rail(3)
      call GSPOS('TFRU',nr,'TFWG',gpos(1),gpos(2),gpos(3),
     &     irot_tof+10,'ONLY')

      nr        =   4           !copy number
      gpos(3)  = -pos_rail(3)
      call GSPOS('TFRU',nr,'TFWG',gpos(1),gpos(2),gpos(3),
     &     irot_tof+10,'ONLY')

c TFRL : TOF Rail Lower
      v_i_name  =   'TFRL'      !daughter volume for upper secter
      nr        =   1           !copy number
       phi = 0.
      pos_rail(1) = -(TOFL_rpos + TFRL_dim(1))
      pos_rail(2) = -(pTFPN_dim(2) + TFRL_dim(2))
      pos_rail(3) = TFRL_dim(3)
      gpos(1) = pos_rail(1)
      gpos(2) = pos_rail(2)
      gpos(3) = pos_rail(3)
      call GSPOS('TFRL',nr,'TFWG',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY')

      nr        =   2           !copy number
      gpos(3) = -pos_rail(3)
      call GSPOS('TFRL',nr,'TFWG',gpos(1),gpos(2),gpos(3),
     &     irot_tof,'ONLY')

      nr        =   3           !copy number
      gpos(1) = pos_rail(1)*cos(22.5*pi/180.)
     &     - pos_rail(2)*sin(22.5*pi/180.)
      gpos(2) = pos_rail(1)*sin(22.5*pi/180.)
     &     + pos_rail(2)*cos(22.5*pi/180.)
      gpos(3) = pos_rail(3)
      call GSPOS('TFRL',nr,'TFWG',gpos(1),gpos(2),gpos(3),
     &     irot_tof+10,'ONLY')

      nr        =   4           !copy number
      gpos(3) = -pos_rail(3)
      call GSPOS('TFRL',nr,'TFWG',gpos(1),gpos(2),gpos(3),
     &     irot_tof+10,'ONLY')

ckn
      v_m_name  =   'TFWG'      !mother volume  
c     =========================================      
      v_i_name  =   'TFR1'      !daughter volume for upper secter
      nr        =   1           !copy number
      pos_rack(1) = -TOFL_rpos - pTFR1_dim(1) 
      pos_rack(2) = 0.
c      pos_rack(3) = -(pTFPN_dim(3)*2.*4. + pTFR1_dim(3) + 0.5)   ! -229.
      pos_rack(3) = TFPN_zpos(1) - (pTFPN_dim(3)+pTFR1_dim(3)+0.5) ! -229.
      call GSPOS(v_i_name,nr,v_m_name,pos_rack(1),pos_rack(2),
     &     pos_rack(3),irot_tof+8,'ONLY')

      v_i_name  =   'TFR3'      !daughter volume for lower secter
      nr        =   1           !copy number
c      pos_rack(1) = -TOFL_rpos*cos(22.5*pi/180.) - 5. !-503. - 7.8  
c      pos_rack(2) = -TOFL_rpos*sin(22.5*pi/180.) -5. !-200. 
      pos_rack(1) = (-TOFL_rpos-pTFR3_dim(1))*cos(22.5*pi/180.)
      pos_rack(2) = (-TOFL_rpos-pTFR3_dim(1))*sin(22.5*pi/180.)
c      pos_rack(3) = -(pTFPN_dim(3)*2.*4. + pTFR3_dim(3) + 0.5)   ! -229.
      pos_rack(3) = TFPN_zpos(9) - (pTFPN_dim(3)*7+pTFR1_dim(3)+0.5) ! -229.
cak      call GSROTM(100,90.,180.+22.5,90.,90.+22.5,180.,0.)
      call GSPOS(v_i_name,nr,v_m_name,pos_rack(1),pos_rack(2),
     &     pos_rack(3),irot_tof+11,'ONLY')

      v_i_name  =   'TFR2'      !daughter volume for upper secter
      nr        =   1           !copy number
      pos_rack(1) = -TOFL_rpos - pTFR2_dim(1)     
      pos_rack(2) = 0.
c      pos_rack(3) = pTFPN_dim(3)*2.*4. + pTFR2_dim(3) + 0.5      ! +229.
      pos_rack(3) = TFPN_zpos(8) + (pTFPN_dim(3)+pTFR2_dim(3)+0.5) ! +229.
      call GSPOS(v_i_name,nr,v_m_name,pos_rack(1),pos_rack(2),
     &     pos_rack(3),irot_tof+8,'ONLY')
ckn
      v_i_name  =   'TFR4'      !daughter volume for lower secter
      nr        =   1           !copy number
c      pos_rack(1) = -TOFL_rpos*cos(22.5*pi/180.) -5. !-503. - 7.8
c      pos_rack(2) = -TOFL_rpos*sin(22.5*pi/180.) -5. !-200.
      pos_rack(1) = (-TOFL_rpos-pTFR4_dim(1))*cos(22.5*pi/180.)
      pos_rack(2) = (-TOFL_rpos-pTFR4_dim(1))*sin(22.5*pi/180.)
c      pos_rack(3) = pTFPN_dim(3)*2.*4. + pTFR4_dim(3) + 0.5      ! +229.
      pos_rack(3) = TFPN_zpos(10) + (pTFPN_dim(3)*7+pTFR2_dim(3)+0.5) ! +229.
cak      call GSROTM(100,90.,180.+22.5,90.,90.+22.5,180.,0.)
      call GSPOS(v_i_name,nr,v_m_name,pos_rack(1),pos_rack(2),
     &     pos_rack(3),irot_tof+11,'ONLY')

c     END volume definitions

c     Get slat dimensions :
c     For hadron sector

      if(TOFL_arms .eq. 1)then  ! if TOF panel is in West Arm
         do ipnl = 1, npnl+npnl_opt
            TFPN_phi(ipnl)  = 180. - TFPN_phi(ipnl)
            TFPN_zpos(ipnl) =   0. - TFPN_zpos(ipnl)
         enddo
      endif
      TFSS_dimen(1) = SCTS_dim(2)
      TFSS_dimen(2) = SCTS_dim(3)
      TFSS_dimen(3) = SCTS_dim(1)
      TFSS_length   = TFSS_dimen(2)*2.
      TFLS_dimen(1) = SCTL_dim(2)
      TFLS_dimen(2) = SCTL_dim(3)
      TFLS_dimen(3) = SCTL_dim(1)
      TFLS_length   = TFLS_dimen(2)*2.
      mFF_HADdets    = mFF_pmts*TFPN_nslat
      mFF_ELEdets    = mFF_pmts*TFPN_nslat
      mFF_ALLdets    = mFF_HADdets

c     Only book detectors if input parameters request it
      if (CVOLU_OPT(1,7) .ne. 'FULL')then
         write(*,'(1x,a)')
     &   ' TOF : Volumes defined but no detectors'
         goto 9999
      endif

c     Put TFPN in set 'TOF '

      set_id    = 'TOF '        ! put it in a SET
      nwpa      = 200           ! for now
      nwsa      = 200           ! for now
c GFPATH assumes GSDETV not GSDET 1/13/96
      call GSDETV(set_id,'SCTL',idtype,nwpa,nwsa,iset,idet)
      call GSDETV(set_id,'SCTS',idtype,nwpa,nwsa,iset,idet)
      call GSDETH(set_id,'SCTL',11,namesh,
     $     nbitsh,orig,fact)
      call GSDETH(set_id,'SCTS',11,namesh,
     $     nbitsh,orig,fact)

c     End of detector geometrys set up

c     Zebra Bank :  For storing User parameters

c      chform = '4F 1I 4F 2I'    ! TYPE A
      chform = '40F 3I'         ! TYPE B
      call MZFORM('PARU',chform,iofu)         ! book characteristics

c     Write parameters to a zebra bank.
c     Later they will go to output file.
c     tof_paru_nd is the # of data words.
c     iof is the IO format word.

c     tof_par and fpflink.inc are re-chainged
c     adopt the TYPE-B                  29-May-2000 Akio Kiyomichi

c      tof_paru_nd = 11          ! TYPE A
      tof_paru_nd = 43          ! TYPE B
      call MZBOOK(
     $     ixdiv_fr,
     $     lFF_PARU,
     $     lFF_PARU,
     $     1,
     $     'PARU',
     $     0,
     $     0,
     $     TOF_PARU_ND,
     $     iofu,
     $     0)

c     Copy raw geometry parameters into 'EPRU' bank.
c     Fill the bank

c TYPE-A
c      qf( IPOINT  + ofea_TFSS_dimen_1)  =   TFSS_dimen(1)
c      qf( IPOINT  + ofea_TFSS_dimen_2)  =   TFSS_dimen(2)
c      qf( IPOINT  + ofea_TFSS_dimen_3)  =   TFSS_dimen(3)
c      qf( IPOINT  + ofea_TFSS_length )  =   TFSS_length
c      iqf( IPOINT  + ofea_mFF_HADdets )  =   mFF_HADdets

c      qf( IPOINT  + ofea_TFLS_dimen_1)  =   TFLS_dimen(1)
c      qf( IPOINT  + ofea_TFLS_dimen_2)  =   TFLS_dimen(2)
c      qf( IPOINT  + ofea_TFLS_dimen_3)  =   TFLS_dimen(3)
c      qf( IPOINT  + ofea_TFLS_length )  =   TFLS_length
c      iqf( IPOINT  + ofea_mFF_ELEdets )  =   mFF_ELEdets
c      iqf( IPOINT  + ofea_mFF_ALLdets )  =   mFF_ALLdets

c     tof_par and fpflink.inc are re-chainged
c     adopt the TYPE-B                  29-May-2000 Akio Kiyomichi

c TYPE-B
      IPOINT = LFF_PARU + 1
      qf( IPOINT  + ofea_TFPN_rpos_0 )  =   TFPN_rpos(1)
      qf( IPOINT  + ofea_TFPN_rpos_1 )  =   TFPN_rpos(2)
      qf( IPOINT  + ofea_TFPN_rpos_2 )  =   TFPN_rpos(3)
      qf( IPOINT  + ofea_TFPN_rpos_3 )  =   TFPN_rpos(4)
      qf( IPOINT  + ofea_TFPN_rpos_4 )  =   TFPN_rpos(5)
      qf( IPOINT  + ofea_TFPN_rpos_5 )  =   TFPN_rpos(6)
      qf( IPOINT  + ofea_TFPN_rpos_6 )  =   TFPN_rpos(7)
      qf( IPOINT  + ofea_TFPN_rpos_7 )  =   TFPN_rpos(8)
      qf( IPOINT  + ofea_TFPN_rpos_8 )  =   TFPN_rpos(9)
      qf( IPOINT  + ofea_TFPN_rpos_9 )  =   TFPN_rpos(10)
      qf( IPOINT  + ofea_TFPN_phi_0  )  =   TFPN_phi(1)
      qf( IPOINT  + ofea_TFPN_phi_1  )  =   TFPN_phi(2)
      qf( IPOINT  + ofea_TFPN_phi_2  )  =   TFPN_phi(3)
      qf( IPOINT  + ofea_TFPN_phi_3  )  =   TFPN_phi(4)
      qf( IPOINT  + ofea_TFPN_phi_4  )  =   TFPN_phi(5)
      qf( IPOINT  + ofea_TFPN_phi_5  )  =   TFPN_phi(6)
      qf( IPOINT  + ofea_TFPN_phi_6  )  =   TFPN_phi(7)
      qf( IPOINT  + ofea_TFPN_phi_7  )  =   TFPN_phi(8)
      qf( IPOINT  + ofea_TFPN_phi_8  )  =   TFPN_phi(9)
      qf( IPOINT  + ofea_TFPN_phi_9  )  =   TFPN_phi(10)
      qf( IPOINT  + ofea_TFPN_zpos_0 )  =   TFPN_zpos(1)
      qf( IPOINT  + ofea_TFPN_zpos_1 )  =   TFPN_zpos(2)
      qf( IPOINT  + ofea_TFPN_zpos_2 )  =   TFPN_zpos(3)
      qf( IPOINT  + ofea_TFPN_zpos_3 )  =   TFPN_zpos(4)
      qf( IPOINT  + ofea_TFPN_zpos_4 )  =   TFPN_zpos(5)
      qf( IPOINT  + ofea_TFPN_zpos_5 )  =   TFPN_zpos(6)
      qf( IPOINT  + ofea_TFPN_zpos_6 )  =   TFPN_zpos(7)
      qf( IPOINT  + ofea_TFPN_zpos_7 )  =   TFPN_zpos(8)
      qf( IPOINT  + ofea_TFPN_zpos_8 )  =   TFPN_zpos(9)
      qf( IPOINT  + ofea_TFPN_zpos_9 )  =   TFPN_zpos(10)
      qf( IPOINT  + ofea_TFSS_dimen_x)  =   TFSS_dimen(1)
      qf( IPOINT  + ofea_TFSS_dimen_y)  =   TFSS_dimen(2)
      qf( IPOINT  + ofea_TFSS_dimen_z)  =   TFSS_dimen(3)
      qf( IPOINT  + ofea_TFSS_length )  =   TFSS_length
      qf( IPOINT  + ofea_TFLS_dimen_x)  =   TFLS_dimen(1)
      qf( IPOINT  + ofea_TFLS_dimen_y)  =   TFLS_dimen(2)
      qf( IPOINT  + ofea_TFLS_dimen_z)  =   TFLS_dimen(3)
      qf( IPOINT  + ofea_TFLS_length )  =   TFLS_length
      qf( IPOINT  + ofea_scint_vlight)  =   scint_vlight
      qf( IPOINT  + ofea_scint_lambda)  =   scint_lambda
      iqf(IPOINT  + ofea_mFF_HADdets )  =   mFF_HADdets
      iqf(IPOINT  + ofea_mFF_ELEdets )  =   mFF_ELEdets
      iqf(IPOINT  + ofea_mFF_ALLdets )  =   mFF_ALLdets



 9999 continue
      return
 999  write(*,*)'tof - Read error in tof_par segment ***'
      stop ' tof - namelist mis-match in tof_par segment ?'
      end
