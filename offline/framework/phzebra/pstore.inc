#include "pisasize.h"

C
C --- pstore.inc -------------------------------------------------------
C
C Parameters and Declarations for the PISA output ZEBRA Store (PSTORE)
C
C Store size:
C
      integer  pf_store_size
      integer  pf_store_div12
 
      parameter   (pf_store_size = PF_STORE_SIZE )
      parameter   (pf_store_div12   =    10000)
C
C Division sizes for
C
C PDiv for Parameters (quasi static data)
C EDiv for Event data (Wiped per Event)
C
      INTEGER  pf_pdiv_size
      INTEGER  pf_pdiv_max
      INTEGER  pf_ediv_size
      INTEGER  pf_ediv_max
 
      parameter   (pf_pdiv_size  =    25000)
      parameter   (pf_pdiv_max   =   400000)
      parameter   (pf_ediv_size  =   200000)
      parameter   (pf_ediv_max   =  5500000)
 
C
      integer*4   ixdiv_Fp
      integer*4   ixdiv_Fe
 
      integer*4   lqf(pf_store_size)
      integer*4   iqF(pf_store_size)
      REAL*4       qF(pf_store_size)

      integer lsafe(10)

      integer fence(10)
      integer lstruc
      integer lrefer
      integer div12(10000)

      equivalence (qf(1),iqf(1),lqf(9))
      equivalence (lqf(1),lsafe)
 
      COMMON /PSTORE/
     #   ixdiv_fp,
     #   ixdiv_fe,
     +   fence, lsafe,      ! unused structural links
     +   lstruc,
     +   lrefer,
     #   div12
 
 
C
C --- end of pstore.inc --------------------------------------------------
C
 
 
 
 
