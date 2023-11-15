c
c     Author C.F. Maguire
c
c     level names for the IBM RS6000 FORTRAN compatibility
c
c     We could use the CERN character to integer functions?
c
c     Revision History:
c	C.F. Maguire   April 1, 1996    Put in SECT mother for TEC system
c       C.F. Maguire   February 5, 1997 Put in CPMS, CMIF for Kenta Shigaki
c       C.F. Maguire   June 28, 2001    Put in SILI for upgrades work
c

      character*4 ch_CPMS /'CPMS'/
      integer CPMS
      equivalence (CPMS, ch_CPMS)
      character*4 ch_CMIF /'CMIF'/
      integer CMIF
      equivalence (CMIF, ch_CMIF)
      character*4 ch_MAGU /'MAGU'/
      integer MAGU
      equivalence (MAGU, ch_MAGU)
      character*4 ch_MAGD /'MAGD'/
      integer MAGD
      equivalence (MAGD, ch_MAGD)
      character*4 ch_PIST /'PIST'/
      integer PIST
      equivalence (PIST, ch_PIST)
      character*4 ch_DRAD /'DRAD'/
      integer DRAD
      equivalence (DRAD, ch_DRAD)
      character*4 ch_DSHE /'DSHE'/
      integer DSHE
      equivalence (DSHE, ch_DSHE)
      character*4 ch_DPD1 /'DPD1'/
      integer DPD1
      equivalence (DPD1, ch_DPD1)
      character*4 ch_DPD2 /'DPD2'/
      integer DPD2
      equivalence (DPD2, ch_DPD2)
      character*4 ch_DBF1 /'DBF1'/
      integer DBF1
      equivalence (DBF1, ch_DBF1)
      character*4 ch_DBF2 /'DBF2'/
      integer DBF2
      equivalence (DBF2, ch_DBF2)
      character*4 ch_EMCL /'EMCL'/
      integer EMCL
      equivalence (EMCL, ch_EMCL)
      character*4 ch_TOFL /'TOFL'/
      integer TOFL
      equivalence (TOFL, ch_TOFL)
      character*4 ch_SECT /'SECT'/
      integer SECT
      equivalence (SECT, ch_SECT)
      character*4 ch_INTR /'INTR'/
      integer INTR
      equivalence (INTR, ch_INTR)
      character*4 ch_INNR /'INNR'/
      integer INNR
      equivalence (INNR, ch_INNR)
      character*4 ch_VERT /'VERT'/
      integer INTVERT
      equivalence (INTVERT, ch_VERT)
      character*4 ch_PIPE /'PIPE'/
      integer PIPE
      equivalence (PIPE, ch_PIPE)
      character*4 ch_SIL1 /'SI01'/, ch_SIL2 /'SI02'/
      integer SIL1, SIL2
      equivalence (SIL1, ch_SIL1), (SIL2, ch_SIL2)

      character*4 ch_SILI /'SILI'/
      integer SILI
      equivalence (SILI, ch_SILI)

c
c     for RICH, 24.Oct.96
c
      character*4 ch_CARM /'CARM'/
      integer CARM
      equivalence (CARM, ch_CARM)
      character*4 ch_CSHA /'CSHA'/
      integer CSHA
      equivalence (CSHA, ch_CSHA)
      character*4 ch_CSHB /'CSHB'/
      integer CSHB
      equivalence (CSHB, ch_CSHB)
      character*4 ch_CRDA /'CRDA'/
      integer CRDA
      equivalence (CRDA, ch_CRDA)
      character*4 ch_CRDB /'CRDB'/
      integer CRDB
      equivalence (CRDB, ch_CRDB)
      character*4 ch_CMIR /'CMIR'/
      integer CMIR
      equivalence (CMIR, ch_CMIR)
      character*4 ch_CPHO /'CPHO'/
      integer CPHO
      equivalence (CPHO, ch_CPHO)
      character*4 ch_CSPM /'CSPM'/
      integer CSPM
      equivalence (CSPM, ch_CSPM)
      character*4 ch_CPMT /'CPMT'/
      integer CPMT
      equivalence (CPMT, ch_CPMT)
      character*4 ch_CPME /'CPME'/
      integer CPME
      equivalence (CPME, ch_CPME)
      character*4 ch_CBAF /'CBAF'/
      integer CBAF
      equivalence (CBAF, ch_CBAF)
      character*4 ch_CMG1 /'CMG1'/
      integer CMG1
      equivalence (CMG1, ch_CMG1)
      character*4 ch_CMG2 /'CMG2'/
      integer CMG2
      equivalence (CMG2, ch_CMG2)
      character*4 ch_CMG3 /'CMG3'/
      integer CMG3
      equivalence (CMG3, ch_CMG3)
      character*4 ch_CMG4 /'CMG4'/
      integer CMG4
      equivalence (CMG4, ch_CMG4)
      character*4 ch_CMCV /'CMCV'/
      integer CMCV
      equivalence (CMCV, ch_CMCV)
      character*4 ch_CEND /'CEND'/
      integer CEND
      equivalence (CEND, ch_CEND)
      character*4 ch_CWI1 /'CWI1'/
      integer CWI1
      equivalence (CWI1, ch_CWI1)
      character*4 ch_CWI2 /'CWI2'/
      integer CWI2
      equivalence (CWI2, ch_CWI2)
      character*4 ch_CTR1 /'CTR1'/
      integer CTR1
      equivalence (CTR1, ch_CTR1)
      character*4 ch_CTR2 /'CTR2'/
      integer CTR2
      equivalence (CTR2, ch_CTR2)
