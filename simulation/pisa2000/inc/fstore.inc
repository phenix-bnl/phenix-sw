*-- Author :
C
C --- fstore.inc -------------------------------------------------------
C
C Parameters and Declarations for the FOPI ZEBRA Store (FSTORE)
C
C Store size:
C
      integer  pf_store_size
      integer  pf_store_fence
      integer  pf_store_div12
C Increased pf_store_size for high energy showers
C      parameter   (pf_store_size =  800000)
C      parameter   (pf_store_size =  4000000)
      parameter   (pf_store_size =  6000000)
      parameter   (pf_store_fence   =      20)
      parameter   (pf_store_div12   =    2000)
C
C Division sizes for
C
C PDiv for Parameters (quasi static data)
C RDiv for Run data (Wiped per Run)
C EDiv for Event data (Wiped per Event)
C
CTON  INTEGER  pf_pdiv_size
CTON  INTEGER  pf_pdiv_max
      INTEGER  pf_rdiv_size
      INTEGER  pf_rdiv_max
      INTEGER  pf_ediv_size
      INTEGER  pf_ediv_max
CTON  parameter   (pf_pdiv_size  =   100000)
CTON  parameter   (pf_pdiv_max   =  200000)
      parameter   (pf_rdiv_size  =   10000)
      parameter   (pf_rdiv_max   =  40000)
C Increased pf_ediv_size/max for high energy showers
C     parameter   (pf_ediv_size  =  400000)
C     parameter   (pf_ediv_max   =  800000)
C      parameter   (pf_ediv_size  =  2000000)
C      parameter   (pf_ediv_max   =  4000000)
      parameter   (pf_ediv_size  =  3000000)
      parameter   (pf_ediv_max   =  6000000)
        INTEGER*4 mFence_F, m_FSTORE
        PARAMETER ( mFence_F = pf_store_fence )
        PARAMETER ( m_FSTORE  = pf_store_size )
C
C
C
      integer*4   ixdiv_Fp
      integer*4   ixdiv_Fr
      integer*4   ixdiv_Fe
      integer*4   lqFence_F(pf_store_fence)
      integer*4   lq_F(pf_store_size)
      integer*4   iq_F(pf_store_size)
      REAL*4       q_F(pf_store_size)
      integer*4   lqf_fence, lqf
      integer*4   iqF(pf_store_size)
      REAL*4       qF(pf_store_size)
      COMMON /FSTORE/
     #   ixdiv_fp,
     #   ixdiv_fr,
     #   ixdiv_fe,
     #   lqf_fence(pf_store_fence), lqf(pf_store_size)
      equivalence (qf(1),iqf(1),lqf(9))
        EQUIVALENCE ( lqf(1), lq_F(1) )
        EQUIVALENCE ( iqf(1), iq_F(1) )
        EQUIVALENCE (  qf(1),  q_F(1) )
        EQUIVALENCE ( lqf_fence(1), lqFence_F(1) )
C
C --- end of fstore.inc --------------------------------------------------
C

