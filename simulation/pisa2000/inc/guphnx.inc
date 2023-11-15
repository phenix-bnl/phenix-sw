c $Id: guphnx.inc,v 1.21 2018/07/05 16:50:11 lajoie Exp $
c     File GUPHNX.INC

      INTEGER*4   LUN_INIT /10/     ! lun for init data
      INTEGER*4   LUN_DINP /70/     ! lun for input data
      INTEGER*4   LUN_DOUT          ! lun for output data (set by CFOPEN)
      INTEGER*4   LUN_DMCI /72/     ! lun for MC data
      INTEGER*4   NIN_KEYS    ! input data structure(s)
      INTEGER*4   IN_KEYS(10)
      CHARACTER*4 CIN_KEYS(10)
      EQUIVALENCE   ( CIN_KEYS(1), IN_KEYS(1) )
      
      INTEGER*4   LUNINIT     ! lun for init data
      INTEGER*4   LUNDINP     ! lun for input data
      INTEGER*4   LUNDOUT     ! lun for output data (set by CFOPEN)
      INTEGER*4   LUNDMCI     ! lun for MC data
      INTEGER*4   NINKEYS     ! input data structure(s)
      INTEGER*4   INKEYS(10)
      CHARACTER*4 CINKEYS(10)
      
      equivalence (luninit, lun_init)
      equivalence (lundinp, lun_dinp)
      equivalence (lundout, lun_dout)
      equivalence (lundmci, lun_dmci)
      equivalence (ninkeys, nin_keys)
      equivalence (inkeys(1), in_keys(1))
      equivalence (cinkeys(1), cin_keys(1))
      
      INTEGER*4   NOUT_KEYS      ! output data structure(s)
      INTEGER*4   OUT_KEYS(10)
      CHARACTER*4 COUT_KEYS(10)
      EQUIVALENCE   ( COUT_KEYS(1), OUT_KEYS(1) )
      INTEGER*4   NSTE_KEYS      ! steer path of PISA
      INTEGER*4   STE_KEYS(10)
      CHARACTER*4 CSTE_KEYS(10)
      EQUIVALENCE   ( CSTE_KEYS(1), STE_KEYS(1) )
      
      INTEGER*4   NOUTKEYS      ! output data structure(s)
      INTEGER*4   OUTKEYS(10)
      CHARACTER*4 COUTKEYS(10)
      EQUIVALENCE (COUTKEYS(1), OUTKEYS(1))
      INTEGER*4   NSTEKEYS      ! steer path of PISA
      INTEGER*4   STEKEYS(10)
      CHARACTER*4 CSTEKEYS(10)
      EQUIVALENCE (CSTEKEYS(1), STEKEYS(1))
      
      equivalence (noutkeys, nout_keys)
      equivalence (outkeys(1), out_keys(1))
      equivalence (coutkeys(1), cout_keys(1))
      equivalence (nstekeys, nste_keys)
      equivalence (stekeys(1), ste_keys(1))
      equivalence (cstekeys(1), cste_keys(1))
      
      CHARACTER*80   CINIT_FILE,        ! file for initialisation data
     1  CDINP_FILE, CDOUT_FILE,  ! files for in/output data
     1  CDMCI_FILE,              ! file for MC data
     1  CHBK_FILE,               ! hbook output file
     1  CPAR_FILE,               ! phnx.par file name
     1  CMNAME_FILE,             ! name of magnetic field map file
     1  PISA_FILE                ! name of output pisa file name
            
      CHARACTER*80   CINITFILE,        ! file for initialisation data
     1  CDINPFILE, CDOUTFILE,   ! files for in/output data
     &  CDMCIFILE,              ! file for MC data
     1  CHBKFILE,               ! hbook output file
     1  CPARFILE                ! phnx.par file name
        
      equivalence (cinitfile, cinit_file)
      equivalence (cdinpfile, cdinp_file)
      equivalence (cdoutfile, cdout_file)
      equivalence (cdmcifile, cdmci_file)
      equivalence (chbkfile, chbk_file)
      equivalence (cparfile, cpar_file)
      
      
      INTEGER*4   PHNX_DVOL   ! number of active PHENIX detector components
      
      PARAMETER     ( PHNX_DVOL = 28)
      INTEGER*4       IVOL(PHNX_DVOL)
      CHARACTER*4     CVOL(PHNX_DVOL)
     +  /'VER','BBC','SVX','ITR','CRK','TRD',
     +  'TOF','EMC','PAD','MUM','MUN','TFW','ZDC',
     +  'AER','HBD','NTC','TPC','RXN','FCL',
     +  'MUPC','RLT','NCC','MPC','MPCX','MXPS','EXAB','MPAL','EXNT'/
        
      EQUIVALENCE   ( IVOL(1), CVOL(1) )
      INTEGER*4   P_VOLU(PHNX_DVOL)       ! pointer SET -> CVOL
      INTEGER*4   IVOLU_OPT(10,PHNX_DVOL)
      CHARACTER*4 CVOLU_OPT(10,PHNX_DVOL)
      REAL*4       RVOLU_OPT(10,PHNX_DVOL)
      EQUIVALENCE (IVOLU_OPT(1,1), CVOLU_OPT(1,1), RVOLU_OPT(1,1))
C
C     switch to control response of specific volume
c     NOTE: These are relics from the FOPI program and should be revised.
C     1        geometry
C     2        hit structure
C     3        tracking option
C     4        digitization
C     5-10     user parameter
C
      INTEGER*4   HEBAG       ! HELIUM BAG (0 not installed, 1 installed)
      
c     following variables are commented. They have been moved to 
c     file mfm.inc, that belongs to offline/packages/mfm
c     and is common to pisa and offline reconstruction.
c     the corresponding variables have been moved to a separate COMMON block
c     Author: Hugo Pereira Da Costa
            
c     INTEGER*4   PHNX_PVOL   ! number of PHNX passive volumes
c     PARAMETER     ( PHNX_PVOL = 10)
c     INTEGER*4       IPVOL(PHNX_PVOL)
c     CHARACTER*4 CPVOL(PHNX_PVOL)
c     EQUIVALENCE (IPVOL(1), CPVOL(1))
c     INTEGER*4   IPVOLU_OPT(10,PHNX_PVOL)
c     CHARACTER*4 CPVOLU_OPT(10,PHNX_PVOL)
c     REAL*4       RPVOLU_OPT(10,PHNX_PVOL)
c     EQUIVALENCE (IPVOLU_OPT(1,1), CPVOLU_OPT(1,1), RPVOLU_OPT(1,1))
      
      include 'mfm.inc'
            
      INTEGER*4   DO_KINE, DO_JXYZ, DO_HITS, DO_TRAK, DO_DIGI
      INTEGER*4   IU_INI_PAR
      
      INTEGER*4   DOKINE, DOJXYZ, DOHITS, DOTRAK, DODIGI
      INTEGER*4   IUINIPAR
      
      equivalence (dokine, do_kine)
      equivalence (dojxyz, do_jxyz)
      equivalence (dohits, do_hits)
      equivalence (dotrak, do_trak)
      equivalence (dodigi, do_digi)
      
      INTEGER*4   ROOT_OUTPUT, ROOTOUTPUT
      INTEGER*4   ZEBRA_OUTPUT, ZEBRAOUTPUT
      
      INTEGER*4   RHICRUN, RHICSUBRUN
      
      equivalence (rootoutput, root_output)
      equivalence (zebraoutput, zebra_output)
            
c     The following common has been modified 
c     to remove the variables that are used in 
c     mfm.inc and included in a separate COMMON block.
c     Author: Hugo Pereira Da Costa
            
      COMMON /GUPHNX/
     1  NIN_KEYS, IN_KEYS, NOUT_KEYS, OUT_KEYS, NSTE_KEYS, STE_KEYS,
     1  CINIT_FILE, CDINP_FILE, CDOUT_FILE, CDMCI_FILE,
     1  IVOLU_OPT, P_VOLU,
     1  DO_KINE, DO_JXYZ, DO_HITS, DO_TRAK, DO_DIGI,
     1  IU_INI_PAR,
     1  CHBK_FILE, CPAR_FILE, LUN_DOUT, HEBAG, ROOT_OUTPUT, 
     1  ZEBRA_OUTPUT, RHICRUN, RHICSUBRUN, CMNAME_FILE, PISA_FILE
        
        
