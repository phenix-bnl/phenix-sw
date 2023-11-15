c $Id: crk.f,v 1.4 2008/05/21 08:21:54 hpereira Exp $
      subroutine CRK(FULL,NH)
*********************
*  FILE crk.f
*********************

      implicit none

#include "g77trigdef.inc"


c     C.F. Maguire 30-Nov-1989  (After  ZER.FOR  N. Herrmann   24-JUL-89)

c Modified by Y. Akiba from a template written by C.F.M

c Major upgrade of CRK geometry
c Now individual PMT is installed in "SUPERMODULE".

c Description
c   This is a geometry definition subroutine for CRK detector of PHENIX
c   All volumes are store in CERK volume, mother volume for Cerenkov detector.
c   CERK volume is defined as TUBE (Rin=250, Rout=410, Z=300.)
c Note by K.Shigaki, Feb.96
c   CERK is now PCON; see pisa/inc/gugeom.inc and pisa/src/phnxcore/hall.F
c Note by K.Shigaki, 26.Feb.96
c   The mother volume was chaged from CERK to HALL by C.F.Maguire

c Structure in CERK
c   Revised on 15.Feb.96 by K.Shigaki (added many support structures)
c Structure of RICH in HALL
c   Revised on 26.Feb.96 by K.Shigaki (changed mother volume to HALL)
c   Revised on 29.Feb.96 by K.Shigaki (added windows CWI1/2)
c   Revised on  1.Mar.96 by K.Shigaki (moved virtual trackers CTR1/2 into CRAD)
c   Revised on 25.Mar.96 by Y.Akiba   (added baffle CBAF)
c   Revised on  9.Aug.96 by K.Shigaki (added window supports CBM7/8)
c   Revised on  3.Sep.96 by K.Shigaki (replaced CBFA-CBFP with virtual CBAF)
c   Revised on 16.Oct.96 by K.Shigaki (replaced 2 CSEC with 1 CARM)
c   Revised on 23.Oct.96 by K.Shigaki (two kinds of CSHE,CRAD: CSHA/B,CRDA/B)
c   Revised on 20.Nov.96 by K.Shigaki (modified mirror structure; under CMST)
c   Revised on 21.Jan.97 by K.Shigaki (added PMT shield CPMS)
c   Revised on 22.Jan.97 by K.Shigaki (added strongback CSBK)
c   Revised on  6.May,97 by K.Shigaki (added cooling plate CCLP)
c   Revised on  8.Jun.98 by K.Shigaki (added mirror support beams CMCB, CMRB)
c   Revised on 25.Nov.98 by K.Shigaki (added light shield CLS1, CLS2)

c                   HALL
c                    |
c                   CARM
c                 (arm = 1/2 of the RICH system)
c                    |
c               +----+----+-----+------------+-----+-----------+-----+
c               |         |     |            |     |           |     |
c               |        CBM1  CBM2         CBM7  CBM8        CLS1  CLS2
c               |      (outside beams)    (window supports) (light shield)
c               |         |     |            |     |
c               |        CBI1  CBI2         CBI7  CBI8
c               |      (inside of tubing) (inside of tubing)
c              CSHA/B
c            (shell incl. rolled plates & side plate)
c               |
c          +----+---------+-----+
c          |              |     |
c          |             CWI1  CWI2
c          |           (kapton windows)
c         CRDA/B
c       (Cherenkov radiator)
c          |
c     +----+----------+--------+-----+--------+------------+----+-----+
c     |    |          |        |     |        |            |    |     |
c     |   CEND       CHTC     CBM3  CBM4     CCLP          |   CTR1  CTR2
c     | (end plate) (hatch) (inside beams) (cooling plate) | (virtual trackers)
c     |               |        |     |                     |
c     |            +--+--+    CBI3  CBI4                   |
c     |            |     |  (inside of tubing)             |
c     |           CBM5  CBM6                               |
c     |         (beams around hatch)  +------------+-------+----+
c     |            |     |            |            |            |
c     |           CBI5  CBI6         CMCB         CMRB         CMST
c     |         (inside of tubing) (center beam) (rib beams) (mirror structure)
c     |                                                         |
c    CPHO                          +--------+--------------+----+-------+
c  (photon detector)               |        |              |            |
c     |                           CMIR     CMIF           CMG1-4       CMCV 
c    CSPM                       (mirror) (support form) (section gap) (cover)
c  (supermodule)
c     |
c     +-----------+------------+
c     |           |            |
c    CPMT        CSBK        (CBAF)
c  (phototube) (strongback) (optional baffles)
c     |
c     +--------------------+
c     |                    |
c    CPME                 CPMS
c  (phototube entrance) (shield)

c Sensitive volumes (detectors):
c     original note by Y.Akiba
c     last updated on 20.Nov.96 by K.Shigaki

c CTR1 and CTR2 are "virtual tracking detector" to aid Cerenkov analysis
c Five volumes CMIR, CPME, CTR1, CTR2, CBAF are declared as sensitive volume,
c and they are put in detector set 'CRK '
c Among them, CPME is the only "real detector".
c CMIR is there just to provide an auxially information where of Mirror the
c Cerenkov photon hits
c CTR1, CTR2 are there just to provide the tracking information of all
c charged particle. They are used to "tune up" Cerenkov analysis program, or
c to evaluate the Cerenkov pid capability.

c Detailed description of revisons:

c  YA    05/12/92
c     Volume option 5 is used to select Cerenkov radiator.
c  YA    09/04/92
c     To be compatible with Release 2.0 of PISA, geometry parameters are
c     stored in 'PARA' bank.
c  YA    02/01/96
c     In order to describe realistic geometry of RICH gas vessel, and RICH
c     photon detector, the geometry description is completely updated.
c  KS    02/05/96
c     Geometry description for the support structures updated.
c  YA    03/25/96
c     Optional volume CBAF is added. This is to limit the Cerenkov light
c     from background. At this moment, it is not decided we actually install
c     such device in RICH or not.
c  KS    06/24/96
c     CPHO changed from TUBS to PCON to accomodate longer CBAF.
c  KS    07/03/96
c     Replaced CBAF with 16 different volumes CBFA-CBFP to allow baffles
c     of different lengths attached on PMT s at different z.
c  KS    09/03/96
c     Replaced CBFA-CBFP back with CBAF, identical 'virtual' baffles
c     attached to all PMT shields.  The baffles no longer stop Cherenkov
c     photons (in crk_gustep.f), but the hit position etc. are recorded
c     in CHIT bank (in crk_digi.f).  The baffles can be hence turned on/off
c     and their length also can be adjusted in PISORP.
c  KS    09/19/96
c     PMT positions and angles imported from the final virsion drawings.
c     The numbers change very little though.
c  KS    10/16/96
c     Two CSEC replaced with 1 CARM (arm = 1/2 of RICH).
c     It now contains 2 CSHE, one of which is rotated (not reflected)
c     to descrube correct PMT staggering on the both sides.
c  KS    10/23/96
c     Two kinds of CRAD = CRDA, CRDB created.
c     CRDA with CPHO inside is neither simple reflection or rotation of CRDB,
c     because of PMT staggering and asymmetric phi position of CPHO.
c     Two kinds of CSHE = CSHA, CSHB also created as mother volumes of CRDA/B.
c  KS    11/20/96
c     Mirror structure modified; envelope volume CMST introduced.
c  KS    01/21/97
c     Implemented realistic PMT shield CPMS.
c  KS    01/22/97
c     Implemented strongback CSBK in CSPM.
c  KS    05/06/97
c     Added cooling plates CCLP.
c     Enlarged CPHO to contain CPMS and CSBK.
c  KS    06/08/98
c     Added mirror support beams CMCB and CMRB.
c  KS    11/25/98
c     Added light shield CLS1/2.
c  KS    12/01/98
c     Corrected tracking medium for beams as 24=G10 (previously 17=Cu).

*KEEP,GCFLAG.
#include "gcflag.inc"
*KEEP,GCLIST.
#include "gclist.inc"
*KEEP,GCONST.
#include "gconst.inc"
*KEEP,GUGEOM.
#include "gugeom.inc"
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,SUBLINK.
#include "sublink.inc"
*KEEP,FPCLINK.
#include "fpclink.inc"
*KEND.

c     main geometry parameter file (phnx.par) segment

      namelist /crk_par/
     $     mirr_r, mirr_thck, mirr_the1, mirr_the2, mirr_thcut,
     $     mirr_phi1,mirr_phi2,mirr_dz

c ==== BEGINNING OF HARDWIRED GEOMETRY PARAMETER BLOCK ====

c NOTE: 01.Feb.96, YA
c In order to describe realistic geometry of RICH gas vessel, and RICH
c photon detector, the geometry description is completely updated as of
c Feb 1996.

c Note: 03.Feb.96, YA
c The UPDATED geometry is defined in the following data statements.
c GEOMETRY parameters of CSHE, CRAD, CPHO, CSPM, CPMT, CPME are defined here.

      integer NCSECT, NCSPM, NCPMT
      parameter (NCSECT =  4)   ! number of sectors
      parameter (NCSPM  = 40)   ! number of supermodule in one sector
      parameter (NCPMT  = 32)   ! number of PMT in on supermodule

      real phi_cntr, phi_open
      real dphi_carm, phi_carm1, phi_carm2
      real dphi_cshe, phi_cshe1, phi_cshe2
      real dphi_cmcb, phi_cmcb1, phi_cmcb2
      real dphi_cpho, phi_cpho_off, phi_cpho1, phi_cpho2
      parameter (phi_cntr     =  90.00)
      parameter (phi_open     =  78.75)
      parameter (dphi_carm    = 100.00)
      parameter (dphi_cshe    =  92.722) ! info. from Sue Wang; 12.Feb.96
      parameter (dphi_cmcb    =  90.00)  ! added be KS; 05.Jun.98
      parameter (dphi_cpho    =  88.973) ! finalized by KS; 02.Jan.97
      parameter (phi_cpho_off =   0.200) ! finalized by KS; 02.Jan.97
      parameter (phi_carm1 = phi_cntr - dphi_carm / 2.0)
      parameter (phi_carm2 = phi_cntr + dphi_carm / 2.0)
      parameter (phi_cshe1 = phi_cntr - dphi_cshe / 2.0)
      parameter (phi_cshe2 = phi_cntr + dphi_cshe / 2.0)
      parameter (phi_cmcb1 = phi_cntr - dphi_cmcb / 2.0)
      parameter (phi_cmcb2 = phi_cntr + dphi_cmcb / 2.0)
      parameter (phi_cpho1 = phi_cntr - dphi_cpho / 2.0)
      parameter (phi_cpho2 = phi_cntr + dphi_cpho / 2.0)

c CARM: it is PCON
c     created on 15.Feb.96 by K.Shigaki as CSEC
c     "sector" = 1/2 arm = 1/4 of the System
c     modified on 16.Oct.96 by K.Shigaki as CARM = 1 arm

      integer ncarm_poly
      parameter (ncarm_poly = 30)
      real carm_poly(ncarm_poly)
     O     /  phi_carm1, dphi_carm, 9.0, ! PHI1, DPHI, # ZPLANES
     5     -300.00, 263.15, 410., ! Z plane 5 (Z < 0)
     4     -271.61, 240.00, 410., ! Z plane 4
     3     -137.30, 240.00, 410., ! Z plane 3
     2     -137.30, 257.50, 410., ! Z plane 2
     1        0.00, 257.50, 410., ! Z plane 1 (Z = 0)
     2      137.30, 257.50, 410., ! Z plane 2
     3      137.30, 240.00, 410., ! Z plane 3
     4      271.61, 240.00, 410., ! Z plane 4
     5      300.00, 263.15, 410./ ! Z plane 5 (Z > 0)

c CBM1: it is "BOX "

      real dim_cbm1(3) / 76.24, 3.81, 3.81/
      real pos_cbm1(3,3)
     &   / 333.75,  3.81,   3.81,
     &     333.75, -3.81, 122.05,
     &     333.75,  3.81, 122.05/ ! (r,d,z) -> covert w/ crk_rot

c CBI1: it is "BOX "

      real dim_cbi1(3) / 76.24, 3.18, 3.18/
      real pos_cbi1(3) /  0.00, 0.00, 0.00/

c CBM2: it is "BOX "

      real dim_cbm2(3) / 84.99, 3.81, 3.81/
      real pos_cbm2(3,2)
     &   / 325.00, -3.81, 143.61,
     &     325.00,  3.81, 143.61/ ! (r,d,z) -> covert w/ crk_rot

c CBI2: it is "BOX "

      real dim_cbi2(3) / 84.99, 3.18, 3.18/
      real pos_cbi2(3) /  0.00, 0.00, 0.00/

c CBM7: it is "BOX"
c     created on  9.Aug.96 by K.Shigaki
c     Oct.16.96, changed to span from z<0 to z>0 -- K.Shigaki
c     Nov.25.98, changed thickness from 1.270 to 1.220 cm -- K.Shigaki

ccc      real dim_cbm7(3) /   0.635, 2.540, 57.85/
ccc      real pos_cbm7(3) / 258.135, 0.000, 57.85/
ccc      real dim_cbm7(3) /   0.635, 2.540, 115.70/
ccc      real pos_cbm7(3) / 258.135, 0.000,   0.00/
      real dim_cbm7(3) /   0.610, 2.540, 115.70/
      real pos_cbm7(3) / 258.160, 0.000,   0.00/
      integer n_cbm7
      parameter (n_cbm7 = 13)

c CBI7: it is "BOX"
c     created on  9.Aug.96 by K.Shigaki

ccc      real dim_cbi7(3) / 0.317, 2.222, 57.85/
      real dim_cbi7(3) / 0.317, 2.222, 115.70/
      real pos_cbi7(3) / 0.00,  0.00,   0.00/

c CBM8: it is "BOX"
c     created on  9.Aug.96 by K.Shigaki
c     Oct.16.96, changed to span from z<0 to z>0 -- K.Shigaki
c     Nov.25.98, changed thickness from 3.048 to 2.998 cm -- K.Shigaki

ccc      real dim_cbm8(3) /   1.524, 3.175, 92.80/
ccc      real pos_cbm8(3) / 408.476, 0.000, 92.80/
ccc      real dim_cbm8(3) /   1.524, 3.175, 185.60/
ccc      real pos_cbm8(3) / 408.476, 0.000,   0.00/
      real dim_cbm8(3) /   1.499, 3.175, 185.60/
      real pos_cbm8(3) / 408.451, 0.000,   0.00/
      integer n_cbm8
      parameter (n_cbm8 = 15)

c CBI8: it is "BOX"
c     created on  9.Aug.96 by K.Shigaki

ccc      real dim_cbi8(3) / 1.206, 2.857, 92.80/
      real dim_cbi8(3) / 1.206, 2.857, 185.60/
      real pos_cbi8(3) / 0.00,  0.00,   0.00/

c CLS1: it is TUBS
c     created on 25.nov.98 by K.Shigaki
c     thickness = 0.014 inch (according to D.Crook)

      real dim_cls1(5) / 257.500, 257.536, 115.70, phi_cshe1, phi_cshe2/
      real pos_cls1(3) /  00.0,    00.0,    00.0/

c CLS2: it is TUBS
c     created on 25.nov.98 by K.Shigaki
c     thickness = 0.014 inch (according to D.Crook)

      real dim_cls2(5) / 409.964, 410.000, 185.60, phi_cshe1, phi_cshe2/
      real pos_cls2(3) /  00.0,    00.0,    00.0/

c CSHE: it is PCON
c     updated on 02/05/96 by K.Shigaki; devided into 2 pieces (z>0 & z<0)
c     updated on 02/13/96 by K.Shigaki; based on FSU drawings

      integer ncshe_poly
      parameter (ncshe_poly = 36)
      real cshe_poly(ncshe_poly) 
     O     / phi_cshe1, dphi_cshe, 11.0, ! PHI1, DPHI, # ZPLANES
     1       0.0,  258.77, 406.952, ! Z plane  1 (Z = 0)
     2     115.70, 258.77, 406.952, ! Z plane  2
     3     115.70, 257.50, 406.952, ! Z plane  3
     4     137.30, 257.50, 406.952, ! Z plane  4
     5     137.30, 240.0,  406.952, ! Z plane  5
     6     185.6,  240.0,  406.952, ! Z plane  6
     7     185.6,  240.0,  410.0,   ! Z plane  7
     8     207.30, 240.0,  410.0,   ! Z plane  8
     9     207.30, 240.0,  399.43,  ! Z plane  9
     A     271.61, 240.0,  304.84,  ! Z plane 10
     B     300.00, 263.15, 263.15/  ! Z plane 11

c CWI1: it is TUBS
c     created on 29.Feb.96 by K.Shigaki

      real dim_cwi1(5) / 258.770, 258.783, 57.85, phi_cshe1, phi_cshe2/
      real pos_cwi1(3) /  00.0,    00.0,   57.85/

c CWI2: it is TUBS
c     created on 29.Feb.96 by K.Shigaki

      real dim_cwi2(5) / 406.939, 406.952, 92.80, phi_cshe1, phi_cshe2/
      real pos_cwi2(3) /  00.0,    00.0,   92.80/

c CRAD: it is PCON
c     updated on 05.Feb.96 by K.Shigaki; devided into 2 pieces (z>0 & z<0)
c     updated on 13.Feb.96 by K.Shigaki; based on FSU drawings

      integer ncrad_poly
      parameter (ncrad_poly = 39)
      real crad_poly(ncrad_poly)
     O     / phi_cshe1, dphi_cshe, 12.0,  ! PHI1, PHI2, # ZPLANES
     1            0.0,  258.783, 406.939, ! Z plane  1 (Z = 0)
     2          115.70, 258.783, 406.939, ! Z plane  2
     3          115.70, 264.07,  406.939, ! Z plane  3
     4          125.86, 264.07,  406.939, ! Z plane  4
     5          125.86, 260.05,  406.939, ! Z plane  5
     6          139.84, 260.05,  406.939, ! Z plane  6
     7          139.84, 240.64,  406.939, ! Z plane  7
     8          185.6,  240.64,  406.939, ! Z plane  8
     9          185.6,  240.64,  402.0,   ! Z plane  9
     A          204.68, 240.64,  402.0,   ! Z plane 10
     B          271.38, 240.64,  304.05,  ! Z plane 11
     C          299.23, 263.15,  263.15/  ! Z plane 12

c CEND: it is PCON
c     created on 14.Feb.96 by K.Shigaki

      integer ncend_poly
      parameter (ncend_poly = 39)
      real cend_poly(ncend_poly)
     O     /    -0.00182, 0.00364, 12.0,      ! PHI1, PHI2, # ZPLANES
     1            0.0,  10258.783, 10406.939, ! Z plane  1 (Z = 0)
     2          115.70, 10258.783, 10406.939, ! Z plane  2
     3          115.70, 10264.07,  10406.939, ! Z plane  3
     4          125.86, 10264.07,  10406.939, ! Z plane  4
     5          125.86, 10260.05,  10406.939, ! Z plane  5
     6          139.84, 10260.05,  10406.939, ! Z plane  6
     7          139.84, 10240.64,  10406.939, ! Z plane  7
     8          185.6,  10240.64,  10406.939, ! Z plane  8
     9          185.6,  10240.64,  10402.0,   ! Z plane  9
     A          204.68, 10240.64,  10402.0,   ! Z plane 10
     B          271.38, 10240.64,  10304.05,  ! Z plane 11
     C          299.23, 10263.15,  10263.15/  ! Z plane 12
      real pos_cend(3,2)
     &   / -10000.00,  0.3175, 0.00,
     &     -10000.00, -0.3175, 0.00/ ! (r,d,z) -> covert w/ crk_rot

c CBM3: it is "BOX "

      real dim_cbm3(3) / 76.24, 2.54, 5.08/
      real pos_cbm3(3,4)
     &   / 333.75,  3.18,   0.00,
     &     333.75, -3.18,   0.00,
     &     333.75,  3.18, 120.78,
     &     333.75, -3.18, 120.78/ ! (r,d,z) -> covert w/ crk_rot

c CBI3: it is "BOX "

      real dim_cbi3(3) / 76.24, 1.91, 4.45/
      real pos_cbi3(3) /  0.00, 0.00, 0.00/

c CBM4: it is "BOX "

      real dim_cbm4(3) / 84.67, 2.54, 5.08/
      real pos_cbm4(3,2)
     &   / 325.31,  3.18, 144.88,
     &     325.31, -3.18, 144.88/ ! (r,d,z) -> covert w/ crk_rot

c CBI4: it is "BOX "

      real dim_cbi4(3) / 84.67, 1.91, 4.45/
      real pos_cbi4(3) /  0.00, 0.00, 0.00/

c CHTC: it is "BOX "

      real dim_chtc(3) /  83.99, 52.00,   3.81/
      real pos_chtc(3) / 329.76,  0.00, 247.82/
      real tilt_chtc   /  34.25/
      real dphi_chtc   /  27.0/

c CBM5: it is "BOX "

      real dim_cbm5(3) / 83.99, 3.81, 3.81/
      real pos_cbm5(3,2)
     &   / 0.00, -48.19, 0.00,
     &     0.00,  48.19, 0.00/

c CBI5: it is "BOX "

      real dim_cbi5(3) / 83.99, 3.18, 3.18/
      real pos_cbi5(3) /  0.00, 0.00, 0.00/

c CBM6: it is "BOX "

      real dim_cbm6(3) / 3.81, 44.38, 3.81/
      real pos_cbm6(3,2)
     &  / -30.60, 0.00, 0.00,
     &     65.78, 0.00, 0.00/

c CBI6: it is "BOX "

      real dim_cbi6(3) / 3.18, 44.38, 3.18/
      real pos_cbi6(3) / 0.00,  0.00, 0.00/

c CCLP: it is TUBS
c     created on 06.May,97 by K.Shigaki

      real dim_cclp(5) / 243.81, 245.40,  65.77, phi_cpho1, phi_cpho2/
      real pos_cclp(3) /  00.00,  00.00, 206.88/

c CPHO: it is PCON
c     changed from TUBS to PCON on 06/24/96 by K.Shigaki
c     CPHO is a sort of arbitrary envelope for CPMT and CBAF.

c     (old comment)
c     size of CPHO. It is now hardwired in the code. The old parameters
c     (pho_rin, pho_rout, etc) are kept, but are no longer used.

      integer ncpho_poly
cks     06.May,97 KS enlarged CPHO to contain CPMS and CSBK.
cks      parameter (ncpho_poly = 15)
cks      real cpho_poly(ncpho_poly)
cks     O     / phi_cpho1, dphi_cpho,  4.0, ! PHI1, DPHI, # ZPLANES
cks     1     123.0, 270.0, 300.0, ! Z plane  1
cks     2     150.0, 255.0, 300.0, ! Z plane  2
cks     3     270.0, 255.0, 300.0, ! Z plane  3
cks     4     280.0, 260.0, 285.0/ ! Z plane  4
      parameter (ncpho_poly = 21)
      real cpho_poly(ncpho_poly)
     O     / phi_cpho1, dphi_cpho,  6.0, ! PHI1, DPHI, # ZPLANES
     1     123.0, 270.0, 298.0, ! Z plane  1
     2     140.0, 260.5, 298.0, ! Z plane  2
     3     140.0, 245.5, 298.0, ! Z plane  3
     4     270.0, 245.5, 298.0, ! Z plane  4
     5     275.0, 245.5, 291.0, ! Z plane  5
     6     290.0, 258.0, 270.0/ ! Z plane  6

c CSPM: CPHO is divided into CSPM; # of division is defined as NCSPM

c CPMT: # of PMTs in a supermodule is defined as NCPMT

      integer ipmt

c CPMT: size (actually the "head" part of "flush-light" shield case)

cks      real dim_cpmt(3) /0.0, 2.75, 2.5/

c CPMT: it is PCON
c     changed from TUBE on 21.Jan.97 by K.Shigaki

      integer ncpmt_poly
      parameter (ncpmt_poly = 15)
      real cpmt_poly(ncpmt_poly)
     O     / 0.0, 360.0, 4.0,   ! PHI1, DPHI, # ZPLANES
     1     -20.7, 0.00, 1.95,   ! Z plane 1 (Z < 0)
     2     -6.70, 0.00, 1.95,   ! Z plane 2
     3     -4.60, 0.00, 2.75,   ! Z plane 3
     4     -0.00, 0.00, 2.75/   ! Z plane 4 

c CPMT: position, angle

      real cpmt_dx(NCPMT) /16*2.424,16*-2.424/ ! finalized by KS; 03.Jan.97

c finalized on 19.Sep.96, by K.Shigaki

      real cpmt_r(NCPMT)         ! height = r position (cm)
     1   / 14*263.490, 265.428, 269.301,
     2     14*263.490, 267.363, 271.239/
      real cpmt_z(NCPMT)         ! z position (cm)
     1   / 141.227, 150.056, 158.885, 167.714,
     2     176.543, 185.372, 194.201, 203.030,
     3     211.859, 220.688, 229.532, 237.948,
     4     246.055, 253.983, 260.134, 264.587,
     1     145.530, 154.359, 163.188, 172.017,
     2     180.846, 189.675, 198.504, 207.333,
     3     216.162, 225.217, 233.848, 242.047,     
     4     250.061, 257.907, 262.362, 266.815/
      real cpmt_th(NCPMT)       ! elevation angle (degree)
     1   / 10*35.487,
     2     37.847, 40.347, 43.012, 45.802,
     3     2*48.992,
     1     9*35.487,
     2     37.847, 40.347, 43.012, 45.802,
     3     3*48.992/

c CPME: size (entrance of Winstone cone. 50mm in diameter)

      real dim_cpme(3) /0.0,2.5,0.05/

c CPMS: it is PCON
c     created on 21.Jan.97 by K.Shigaki

      integer ncpms_poly
      parameter (ncpms_poly = 21)
      real cpms_poly(ncpms_poly)
     O     / 0.0, 360.0, 6.0,   ! PHI1, DPHI, # ZPLANES
     1     -20.7, 1.65, 1.95,   ! Z plane 1 (Z < 0)
     2     -6.70, 1.65, 1.95,   ! Z plane 2
     3     -4.60, 2.45, 2.75,   ! Z plane 3
     4     -3.75, 2.45, 2.75,   ! Z plane 4
     5     -3.75, 2.65, 2.75,   ! Z plane 5
     6     -0.00, 2.65, 2.75/   ! Z plane 6

c CSBK: it is PCON
c     created on 22.Jan.97 by K.Shigaki

      integer ncsbk_poly
      parameter (ncsbk_poly = 27)
      real csbk_poly(ncsbk_poly) 
     O     / -0.0555, 0.111, 8.0,  ! PHI1, DPHI, # ZPLANES
     1     146.83, 252.94, 257.46, ! Z plane  1
     2     152.88, 245.71, 257.46, ! Z plane  2
     3     229.49, 245.71, 257.46, ! Z plane  3
     4     229.49, 245.71, 256.83, ! Z plane  4
     5     260.99, 245.71, 256.83, ! Z plane  5
     6     270.48, 245.71, 264.79, ! Z plane  6
     7     278.11, 252.11, 271.20, ! Z plane  7
     8     287.50, 260.01, 260.01/ ! Z plane  8

c CBAF: optional "Cereknov light limitter"
c     physically it is just a small plane placed paralell to the PMTs
c     changed to TUBS from BOX by KS on 9.Jul.96

      real dim_cbaf(5) / 2.75, 2.85, 10.00, 120., 240./
      real dx_cbaf, dz_cbaf
      logical cbaf_install
      save cbaf_install

c CMST: Cerenkov mirror structure envelope; SPHE

      real dim_cmst(6)
      real cmst_drin / 0.20/    ! radial space inside the mirror surface
      real cmst_dphi / 0.10/    ! azimuthal space from the mirror edges

c CMIR: Cerenkov mirror; SPHE

      real dim_cmir(6)
      real pos_cmir(3)

c default values; to be overwritten by phnx.par

      real mirr_r     /403.00/
      real mirr_thck  /  0.01/
      real mirr_the1  / 96.41/
      real mirr_the2  /122.24/
      real mirr_thcut / 98.99/
      real mirr_phi1  /-44.10/
      real mirr_phi2  / 44.10/
      real mirr_dz    /215.00/

c CMIF: supporting structural form for the mirror; SPHE

      real dim_cmif(6)
      real thick_cmif / 1.0/

c CMG1: gap between mirror sections at constant phi; TUBS

      real dim_cmg1(5)
      real cmg1_wdth /  0.30/   ! full width
      real cmg1_thck /  0.005/
      integer n_cmg1
      parameter (n_cmg1 = 13)

c CMG2: gap between mirror sections at constant theta; CONS

      real dim_cmg2(7)
      real cmg2_wdth /  0.15/   ! full width
      real cmg2_thck /  0.005/
      real cmg2_rout, cmg2_the1, cmg2_the2, cmg2_z, cmg2_dz

c CMG3: gap between mirror sections at constant theta; CONS

      real dim_cmg3(7)
      real cmg3_the0 /109.32/   ! theta of center; from the mirror center
      real cmg3_wdth /  0.30/   ! full width
      real cmg3_thck /  0.005/
      real cmg3_rout, cmg3_the1, cmg3_the2, cmg3_z, cmg3_dz

c CMG4: gap between mirror sections at constant theta; CONS

      real dim_cmg4(7)
      real cmg4_wdth /  0.15/   ! full width
      real cmg4_thck /  0.005/
      real cmg4_rout, cmg4_the1, cmg4_the2, cmg4_z, cmg4_dz

c CMCV: mirror edge cover to effectively shorten the mirror; CONS

      real dim_cmcv(7)
      real cmcv_thck  /  0.01/
      real cmcv_rout, cmcv_the1, cmcv_the2, cmcv_z, cmcv_dz


c CMCB: mirror support center beam; TUBS

      real dim_cmcb(5) / 346.71, 350.52, 1.588, phi_cmcb1, phi_cmcb2/
      real pos_cmcb(3) /   0.0,    0.0,  1.588/

c CMCI: inside of mirror support center beam; TUBS

      real dim_cmci(5)
      real cmci_thck   / 0.318/

c CMRB: mirror support rib beams; TUBS

      real dim_cmrb(5)
      real cmrb_rin
      real cmrb_thck / 3.175/
      real cmrb_wdth / 7.620/ ! full width
      real cmrb_the1, cmrb_the2
      real cmrb_wall / 0.318/
      real pos_cmrb(3)
      integer n_cmrb
      parameter (n_cmrb = 13)
      real phi_cmrb(n_cmrb) /
     &     -42.75, -36.75, -29.40, -22.05, -14.70,  -7.35,   0.00,
     &       7.35,  14.70,  22.05,  29.40,  36.75,  42.75 /

c CMRI: inside of mirror support rib beams; TUBS

      real dim_cmri(5)
      real cmri_thck   / 0.318/

c CTR1: virtual tracker 1

      real vtr1_rin  /258.783/
      real vtr1_rout /258.784/  ! virtual 10 um tracker
      real vtr1_z    / 57.35/
      real vtr1_phi1 /phi_cpho1/
      real vtr1_phi2 /phi_cpho2/
      real dim_vtr1(5)

c CTR2: virtual tracker 2

      real vtr2_rin  /406.936/  ! virtual 30 um tracker
      real vtr2_rout /406.939/
      real vtr2_z    / 92.30/
      real vtr2_phi1 /phi_cpho1/
      real vtr2_phi2 /phi_cpho2/
      real dim_vtr2(5)

      integer kSeen /0/   ! variable for "SEEN" option


c==== END OF HARDWIRED GEOMETRY PARAMETER BLOCK =========

      character*4 v_m_name,v_m_name1,v_m_name2,v_i_name
ccc      character*4 v_d_name,set_id,namesv(10)
      character*4 set_id,namesv(10)

ccc      integer  nr,npar,nmed,ivolu,inull,ndiv,nv,idtype,nbitsv(10),
ccc     @  iaxis,nwpa,nwsa,iset,idet,IVAL
      integer  nr,npar,nmed,ivolu,nv,idtype,nbitsv(10),
     @  nwpa,nwsa,iset,idet
      integer nmed_gas
      integer irot_west, irot_east
      integer irot_xref, irot_zref, irot_zxref, irot_cshb
      integer irot_phi1, irot_phi2
      integer irot_cpho(2)
      integer irot_chtc(3), irot_cbm7(n_cbm7), irot_cbm8(n_cbm8)
      integer irot_cmg1(n_cmg1)
      integer irot_cmrb(n_cmrb)

      real phi

      integer*4 nh              ! set before call in gugeom
      character*4 full          ! set before call in gugeom


c--> common to communicate with CRK_GUSTEP and CRK_DIGI
      real Mirr_cent(3)
      common/CRK_COMMON/Mirr_cent

c parameters
      real RAD_TO_DEG
      parameter (RAD_TO_DEG = 180. / 3.141592654)
      real DEG_TO_RAD
      parameter (DEG_TO_RAD = 3.141592654 / 180.)

c local vars
      integer i
      integer IOPARA            !IO format word of 'PARA' bank
      integer IOPARU            !IO format word of 'PARU' bank
      
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c BEGIN
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c     Read the geometery file segment


      write( *,* ) 'crk - reading parameter from common interface'
      rewind( unit = itf_lun )
      read( unit = itf_lun, nml = crk_par, err = 999 )


c Geometry parameters are read. Now store it in 'PARA' bank

c BOOK 'PARA' bank

      call MZFORM('PARA',       !bank name
     $     '-F',                !all data in the bank is Float
     $     IOPARA)
      call MZBOOK(
     $     ixdiv_fr,            !division
     $     lFC_PARA,            !return address of the bank
     $     lFC_PARA,            !supporting link
     $     1,                   !JB=1 means create top level bank
     $     'PARA',              !bank name
     $     0,                   !NL=0. No links
     $     0,                   !NS=0. No supporting (down) links
     $     CRK_PARA_ND,         !# of data words
     $     IOPARA,              !IO format word
     $     0)                   !NZERO=0 means that whole bank is cleared

c copy geometry parameters in 'PARA' bank

c Note by K.Shigaki, 24.Jun.96

c     CPHO is no longer TUBS, but PCON.
c     PARA bank is not updated accordingly.
c     I put old values (when CPHO was TUBS) in PARA bank...

c     01.Nov.96, KS, updated values of cpho_rin, cpho_rout, cpho_phi1/2

      qf(lFC_PARA + oC_PHI_CNTR)     = phi_cntr
      qf(lFC_PARA + oC_PHI_OPEN)     = phi_open
      qf(lFC_PARA + oC_DPHI_CARM)    = dphi_carm
      qf(lFC_PARA + oC_DPHI_CSHE)    = dphi_cshe
      qf(lFC_PARA + oC_DPHI_CPHO)    = dphi_cpho
      qf(lFC_PARA + oC_PHI_CPHO_OFF) = phi_cpho_off

      qf(lFC_PARA + oC_N_SECT)       = NCSECT
      qf(lFC_PARA + oC_N_SPM)        = NCSPM
      qf(lFC_PARA + oC_N_PMT)        = NCPMT

      qf(lFC_PARA + oC_R_PMT_ENT)    = dim_cpme(2)
      do i = 1, NCPMT
        qf(lFC_PARA + oC_DX_PMT    + i - 1) = cpmt_dx(i)
        qf(lFC_PARA + oC_R_PMT     + i - 1) = cpmt_r(i)
        qf(lFC_PARA + oC_Z_PMT     + i - 1) = cpmt_z(i)
        qf(lFC_PARA + oC_THETA_PMT + i - 1) = cpmt_th(i)
      enddo

      qf(lFC_PARA + oC_MIR_RIN)      = mirr_r
      qf(lFC_PARA + oC_MIR_THCK)     = mirr_thck
      qf(lFC_PARA + oC_MIR_THETA1)   = mirr_the1
      qf(lFC_PARA + oC_MIR_THETA2)   = mirr_the2
      qf(lFC_PARA + oC_MIR_THETACUT) = mirr_thcut
      qf(lFC_PARA + oC_MIR_PHI1)     = mirr_phi1
      qf(lFC_PARA + oC_MIR_PHI2)     = mirr_phi2
      qf(lFC_PARA + oC_MIR_DZ)       = mirr_dz

      qf(lFC_PARA + oC_WI1_RIN)      = dim_cwi1(1)
      qf(lFC_PARA + oC_WI1_THCK)     = dim_cwi1(2) - dim_cwi1(1)
      qf(lFC_PARA + oC_WI1_ZEND)     = dim_cwi1(3) * 2
      qf(lFC_PARA + oC_WI2_RIN)      = dim_cwi2(1)
      qf(lFC_PARA + oC_WI2_THCK)     = dim_cwi2(2) - dim_cwi2(1)
      qf(lFC_PARA + oC_WI2_ZEND)     = dim_cwi2(3) * 2

      qf(lFC_PARA + oC_TR1_RIN)      = vtr1_rin
      qf(lFC_PARA + oC_TR1_THCK)     = vtr1_rout - vtr1_rin
      qf(lFC_PARA + oC_TR1_ZEND)     = vtr1_z * 2
      qf(lFC_PARA + oC_TR2_RIN)      = vtr2_rin
      qf(lFC_PARA + oC_TR2_THCK)     = vtr2_rout - vtr2_rin
      qf(lFC_PARA + oC_TR2_ZEND)     = vtr2_z * 2

c Additional USER parameters are stored in 'PARU' bank.
c BOOK 'PARU' bank

      call MZFORM('PARU',       !bank name
     $     '-F',                !all data in the bank is Float
     $     IOPARU)
      call MZBOOK(
     $     ixdiv_fr,            !division
     $     lFC_PARU,            !return address of the bank
     $     lFC_PARU,            !supporting link
     $     1,                   !JB=1 means create top level bank
     $     'PARU',              !bank name
     $     0,                   !NL=0. No links
     $     0,                   !NS=0. No supporting (down) links
     $     CRK_PARU_ND,         !# of data words
     $     IOPARU,              !IO format word
     $     0)                   !NZERO=0 means that whole bank is cleared

c copy parameters to 'PARU' bank

      if(cvolu_opt(5,5).eq.'CH4 ') then
        qf(lFC_PARU + oCRAD_GAS)  = CRAD_GAS_CH4
      else if(cvolu_opt(5,5).eq.'C2H6') then
        qf(lFC_PARU + oCRAD_GAS)  = CRAD_GAS_C2H6
      else if(cvolu_opt(5,5).eq.'N2  ') then
        qf(lFC_PARU + oCRAD_GAS)  = CRAD_GAS_N2
      else if(cvolu_opt(5,5).eq.'CO2 ') then       ! CO2
        qf(lFC_PARU + oCRAD_GAS)  = CRAD_GAS_CO2
      else if(cvolu_opt(5,5).eq.'FR13') then      ! Freon 13
        qf(lFC_PARU + oCRAD_GAS)  = CRAD_GAS_FR13
      endif

      qf(lFC_PARU + oCRK_N0INI)  = N0_INIT

C  only book volumes if input parameters are OK

      if ( CVOLU_OPT(1,5) .eq. 'FULL' .or.
     &     CVOLU_OPT(1,5) .eq. 'VOLS' .or.
     &     cvolu_opt(1,5) .eq. 'CBAF' ) then

C--> Option...CBAF

      if ( cvolu_opt(1,5) .eq. 'CBAF' ) then
        write(*,*) 'CRK-I install CBAF'
        cbaf_install = .true.
      else
        write(*,*) 'CRK-I CBAF is not installed'
        cbaf_install = .false.
      endif

C-->     Define new material and Medium


c Define gas radiators for Cerenkov  (removed by cfm 5/4/92)
c        call CRK_MAT_MED     ! defined in MAT_MIXT_MED.FOR

c Now put in geometry into proper mother volume

C--> Create Volume 'CARM'

        v_i_name = 'CARM'
C-YA      nmed = 19                 ! air w/ magnetic field
        nmed = 1                ! Helium gas w/ magnetic field (same as HALL)
        call gsvolu(v_i_name,'PCON',nmed,carm_poly,ncarm_poly,ivolu)
        CALL GSATT(v_i_name,'SEEN',0)

C--> Create Volume 'CBM1'

        v_i_name = 'CBM1'
        nmed = 26               ! Al
        call gsvolu(v_i_name,'BOX ',nmed,dim_cbm1,3,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CBI1'

        v_i_name = 'CBI1'
        nmed = 19               ! air w/ magnetic field
        call gsvolu(v_i_name,'BOX ',nmed,dim_cbi1,3,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CBM2'

        v_i_name = 'CBM2'
        nmed = 26               ! Al
        call gsvolu(v_i_name,'BOX ',nmed,dim_cbm2,3,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CBI2'

        v_i_name = 'CBI2'
        nmed = 19               ! air w/ magnetic field
        call gsvolu(v_i_name,'BOX ',nmed,dim_cbi2,3,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CBM7'

        v_i_name = 'CBM7'
        nmed = 24               ! G10; TEMPORARY
        call gsvolu(v_i_name,'BOX ',nmed,dim_cbm7,3,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CBI7'

        v_i_name = 'CBI7'
        nmed = 19               ! air w/ magnetic field
        call gsvolu(v_i_name,'BOX ',nmed,dim_cbi7,3,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CBM8'

        v_i_name = 'CBM8'
        nmed = 24               ! G10; TEMPORARY
        call gsvolu(v_i_name,'BOX ',nmed,dim_cbm8,3,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CBI8'

        v_i_name = 'CBI8'
        nmed = 19               ! air w/ magnetic field
        call gsvolu(v_i_name,'BOX ',nmed,dim_cbi8,3,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CLS1'

        v_i_name = 'CLS1'
        nmed = 821              ! PVC
        call gsvolu(v_i_name,'TUBS',nmed,dim_cls1,5,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        CALL GSATT(v_i_name,'COLO',2)

C--> Create Volume 'CLS2'

        v_i_name = 'CLS2'
        nmed = 821              ! PVC
        call gsvolu(v_i_name,'TUBS',nmed,dim_cls2,5,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        CALL GSATT(v_i_name,'COLO',2)

C--> Create Volume 'CSHA/B'

        nmed =  26              ! Al (according to PISA primer ver2.0 1995 Sep)

        v_i_name = 'CSHA'
        call gsvolu(v_i_name,'PCON',nmed,cshe_poly,ncshe_poly,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

        v_i_name = 'CSHB'
        call gsvolu(v_i_name,'PCON',nmed,cshe_poly,ncshe_poly,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CWI1'

        v_i_name = 'CWI1'
        nmed = 25               ! mylar; should be replaced with kapton
        call gsvolu(v_i_name,'TUBS',nmed,dim_cwi1,5,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CWI2'

        v_i_name = 'CWI2'
        nmed = 25               ! mylar; should be replaced with kapton
        call gsvolu(v_i_name,'TUBS',nmed,dim_cwi2,5,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CRDA/B'

        if(cvolu_opt(5,5) .eq. 'C2H6') then
          nmed_gas = 501
          write(6,*) 'CRK<I> C2H6 radiator '
        else if(cvolu_opt(5,5) .eq. 'CH4 ') then
          nmed_gas = 502
          write(6,*) 'CRK<I> CH4 radiator '
        else if(cvolu_opt(5,5) .eq. 'N2  ') then
          nmed_gas = 4
          write(6,*) 'CRK<I> N2 radiator '
        else if(cvolu_opt(5,5) .eq. 'CO2 ') then
          nmed_gas = 503
          write(6,*) 'CRK<I> CO2 radiator '
        else if(cvolu_opt(5,5) .eq. 'FR13') then
          nmed_gas = 504
          write(6,*) 'CRK<I> FREON 13 radiator '
        else
          write(6,*)
     &         'CRK<W> Cerenkov radiator has to be C2H6, CH4 or N2'
          write(6,*)
     &         'CRK<W> Default is taken....C2H6 gas'
          nmed_gas = 501
        endif

c      npar = 5
c      dim_rad(1) =rad_rin       ! Rmin
c      dim_rad(2) =rad_rout      ! Rmax
c      dim_rad(3) =rad_z         ! Z(half)
c      dim_rad(4) =rad_phi1      ! Phi1
c      dim_rad(5) =rad_phi2      ! Phi2
c      call gsvolu(v_i_name,'TUBS',nmed_gas,dim_rad,npar,ivolu)

        v_i_name = 'CRDA'
        call gsvolu(v_i_name,'PCON',nmed_gas,crad_poly,ncrad_poly,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        v_i_name = 'CRDB'
        call gsvolu(v_i_name,'PCON',nmed_gas,crad_poly,ncrad_poly,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CEND'

        v_i_name = 'CEND'
        nmed = 26               ! Al
        call gsvolu(v_i_name,'PCON',nmed,cend_poly,ncend_poly,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CBM3'

        v_i_name = 'CBM3'
        nmed = 26               ! Al
        call gsvolu(v_i_name,'BOX ',nmed,dim_cbm3,3,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CBI3'

        v_i_name = 'CBI3'
        call gsvolu(v_i_name,'BOX ',nmed_gas,dim_cbi3,3,ivolu) ! Radiator Gas
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CBM4'

        v_i_name = 'CBM4'
        nmed = 26               ! Al
        call gsvolu(v_i_name,'BOX ',nmed,dim_cbm4,3,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CBI4'

        v_i_name = 'CBI4'
        call gsvolu(v_i_name,'BOX ',nmed_gas,dim_cbi4,3,ivolu) ! Radiator Gas
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CHTC'

        v_i_name = 'CHTC'
        call gsvolu(v_i_name,'BOX ',nmed_gas,dim_chtc,3,ivolu) ! Radiator Gas
        CALL GSATT(v_i_name,'SEEN',0)

C--> Create Volume 'CBM5'

        v_i_name = 'CBM5'
        nmed = 26               ! Al
        call gsvolu(v_i_name,'BOX ',nmed,dim_cbm5,3,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CBI5'

        v_i_name = 'CBI5'
        call gsvolu(v_i_name,'BOX ',nmed_gas,dim_cbi5,3,ivolu) ! Radiator Gas
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CBM6'

        v_i_name = 'CBM6'
        nmed = 26               ! Al
        call gsvolu(v_i_name,'BOX ',nmed,dim_cbm6,3,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CBI6'

        v_i_name = 'CBI6'
        call gsvolu(v_i_name,'BOX ',nmed_gas,dim_cbi6,3,ivolu) ! Radiator Gas
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CCLP'

        v_i_name = 'CCLP'
        nmed = 26               ! Al
        call gsvolu(v_i_name,'TUBS',nmed,dim_cclp,5,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        CALL GSATT(v_i_name,'COLO',2)

C---> Create volume 'CPHO'...Cerenkov Photon Detector

        v_i_name = 'CPHO'

c      nmed = 10                 ! SILICON (sensitive but non-magnetic here)

c     CPHO is now a volume to hold "CPSM" (supermodule).
c     The medium of CPHO is the same as CRAD.

c     CPHO changed from TUBS to PCON by K.Shigaki on 24.Jun.96

        call gsvolu(v_i_name,'PCON',nmed_gas,cpho_poly,ncpho_poly,ivolu)
        CALL GSATT(v_i_name,'SEEN',0)

c--> Create volume 'CSPM'...RICH Super Module (32 PMT. 2x16)
c    CSPM is a phi-division of CPHO. "strongback" is not implemented, since
c    I belive it is irrelevant for the simulation.

        call gsdvn('CSPM','CPHO',NCSPM,2)

c--> Create CPMT (PMT of RICH).

        v_i_name = 'CPMT'
cks        nmed = 26               ! Al
cks        call gsvolu(v_i_name,'TUBE',nmed,dim_cpmt,3,ivolu)
        call gsvolu(v_i_name,'PCON',nmed_gas,cpmt_poly,ncpmt_poly,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        CALL GSATT(v_i_name,'COLO',4)

c--> Create PMT entrance (sensitive area)

        v_i_name = 'CPME'
        nmed = 10               ! sensivite Silicon
        call gsvolu(v_i_name,'TUBE',nmed,dim_cpme,3,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        CALL GSATT(v_i_name,'COLO',2)

c--> Create CPMS (shield of PMT)

        v_i_name = 'CPMS'
        nmed = 5                ! Fe
        call gsvolu(v_i_name,'PCON',nmed,cpms_poly,ncpms_poly,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        CALL GSATT(v_i_name,'COLO',4)

C--> Create Volume 'CSBK'

        nmed =  26              ! Al
        v_i_name = 'CSBK'
        call gsvolu(v_i_name,'PCON',nmed,csbk_poly,ncsbk_poly,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        CALL GSATT(v_i_name,'COLO',2)

c--> Create CBAF (optional)

        if (cbaf_install) then
          v_i_name = 'CBAF'
          nmed = 10             ! Silicion (sensitive)
          call gsvolu(v_i_name,'TUBS',nmed,dim_cbaf,5,ivolu)
          CALL GSATT(v_i_name,'SEEN',0)
          write(*,*) 'CRK-I Created CBAF'
        endif

C---> Create Volume 'CMST'... envelope for mirror

        v_i_name = 'CMST'
        dim_cmst(1) = mirr_r - cmst_drin ! Rmin
        dim_cmst(2) = mirr_r + mirr_thck + thick_cmif ! Rmax
        dim_cmst(3) = mirr_the1 ! theta1
        dim_cmst(4) = mirr_the2 ! theta2
        dim_cmst(5) = phi_cntr + mirr_phi1 - cmst_dphi ! phi1
        dim_cmst(6) = phi_cntr + mirr_phi2 + cmst_dphi ! phi2
        call gsvolu(v_i_name, 'SPHE', nmed_gas, dim_cmst, 6, ivolu)
        CALL GSATT(v_i_name,'SEEN',0)

C---> Create Volume 'CMIR'... mirror for Cherenkov

        v_i_name = 'CMIR'
        nmed = 10               ! SILICON (sensitive but non-magnetic here)
                                ! TEMPORARY; to be replaced with carbon-epoxy
        dim_cmir(1) = mirr_r    ! Rmin
        dim_cmir(2) = mirr_r + mirr_thck ! Rmax
        dim_cmir(3) = mirr_the1 ! theta1
        dim_cmir(4) = mirr_the2 ! theta2
        dim_cmir(5) = phi_cntr + mirr_phi1 ! phi1
        dim_cmir(6) = phi_cntr + mirr_phi2 ! phi2
        call gsvolu(v_i_name, 'SPHE', nmed, dim_cmir, 6, ivolu)
        CALL GSATT(v_i_name,'SEEN',0)

C---> Create Volume 'CMIF'

        v_i_name = 'CMIF'
        nmed = 511              ! RICH mirror substrate defined in mat_mixt_med.f
        dim_cmif(1) = mirr_r + mirr_thck ! Rmin
        dim_cmif(2) = mirr_r + mirr_thck + thick_cmif ! Rmax
        dim_cmif(3) = mirr_the1 ! theta1
        dim_cmif(4) = mirr_the2 ! theta2
        dim_cmif(5) = phi_cntr + mirr_phi1 ! phi1
        dim_cmif(6) = phi_cntr + mirr_phi2 ! phi2
        call gsvolu(v_i_name, 'SPHE', nmed, dim_cmif, 6, ivolu)
        CALL GSATT(v_i_name,'SEEN',0)
ccc        call gpmate(nmed)
ccc        call gptmed(nmed)

C---> Create Volume 'CMG1'

        v_i_name = 'CMG1'
        nmed = 24               ! G10; TEMPORARY
        dim_cmg1(1) = mirr_r - cmg1_thck ! Rmin
        dim_cmg1(2) = mirr_r    ! Rmax
        dim_cmg1(3) = cmg1_wdth / 2. ! dz
        dim_cmg1(4) = mirr_the1 ! phi1
        dim_cmg1(5) = mirr_the2 ! phi2
        call gsvolu(v_i_name, 'TUBS', nmed, dim_cmg1, 5, ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        CALL GSATT(v_i_name,'COLO',3)

C---> Create Volume 'CMG2'

        v_i_name = 'CMG2'
        nmed = 24               ! G10; TEMPORARY

        cmg2_rout = mirr_r - cmg1_thck
        cmg2_the1 = mirr_the2 - cmg2_wdth / cmg2_rout * RAD_TO_DEG
        cmg2_the2 = mirr_the2
        cmg2_z    = cmg2_rout * (cosd(cmg2_the1) + cosd(cmg2_the2)) / 2.
        cmg2_dz   = cmg2_rout * (cosd(cmg2_the1) - cosd(cmg2_the2)) / 2.

        dim_cmg2(1) = cmg2_dz   ! dz
        dim_cmg2(2) = cmg2_rout * sind(cmg2_the2) - cmg2_thck ! Rmin at -dz
        dim_cmg2(3) = cmg2_rout * sind(cmg2_the2)             ! Rmax at -dz
        dim_cmg2(4) = cmg2_rout * sind(cmg2_the1) - cmg2_thck ! Rmin at +dz
        dim_cmg2(5) = cmg2_rout * sind(cmg2_the1)             ! Rmax at +dz
        dim_cmg2(6) = phi_cntr + mirr_phi1 ! phi1
        dim_cmg2(7) = phi_cntr + mirr_phi2 ! phi2
        call gsvolu(v_i_name, 'CONS', nmed, dim_cmg2, 7, ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        CALL GSATT(v_i_name,'COLO',3)

C---> Create Volume 'CMG3'

        v_i_name = 'CMG3'
        nmed = 24               ! G10; TEMPORARY

        cmg3_rout = mirr_r - cmg1_thck
        cmg3_the1 = cmg3_the0 - cmg3_wdth / 2. / cmg3_rout * RAD_TO_DEG
        cmg3_the2 = cmg3_the0 + cmg3_wdth / 2. / cmg3_rout * RAD_TO_DEG
        cmg3_z    = cmg3_rout * (cosd(cmg3_the1) + cosd(cmg3_the2)) / 2.
        cmg3_dz   = cmg3_rout * (cosd(cmg3_the1) - cosd(cmg3_the2)) / 2.

        dim_cmg3(1) = cmg3_dz   ! dz
        dim_cmg3(2) = cmg3_rout * sind(cmg3_the2) - cmg3_thck ! Rmin at -dz
        dim_cmg3(3) = cmg3_rout * sind(cmg3_the2)             ! Rmax at -dz
        dim_cmg3(4) = cmg3_rout * sind(cmg3_the1) - cmg3_thck ! Rmin at +dz
        dim_cmg3(5) = cmg3_rout * sind(cmg3_the1)             ! Rmax at +dz
        dim_cmg3(6) = phi_cntr + mirr_phi1 ! phi1
        dim_cmg3(7) = phi_cntr + mirr_phi2 ! phi2
        call gsvolu(v_i_name, 'CONS', nmed, dim_cmg3, 7, ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        CALL GSATT(v_i_name,'COLO',3)

C---> Create Volume 'CMG4'

        v_i_name = 'CMG4'
        nmed = 24               ! G10; TEMPORARY

        cmg4_rout = mirr_r - cmg1_thck
        cmg4_the1 = mirr_the1
        cmg4_the2 = mirr_the1 + cmg4_wdth / cmg4_rout * RAD_TO_DEG
        cmg4_z    = cmg4_rout * (cosd(cmg4_the1) + cosd(cmg4_the2)) / 2.
        cmg4_dz   = cmg4_rout * (cosd(cmg4_the1) - cosd(cmg4_the2)) / 2.

        dim_cmg4(1) = cmg4_dz   ! dz
        dim_cmg4(2) = cmg4_rout * sind(cmg4_the2) - cmg4_thck ! Rmin at -dz
        dim_cmg4(3) = cmg4_rout * sind(cmg4_the2)             ! Rmax at -dz
        dim_cmg4(4) = cmg4_rout * sind(cmg4_the1) - cmg4_thck ! Rmin at +dz
        dim_cmg4(5) = cmg4_rout * sind(cmg4_the1)             ! Rmax at +dz
        dim_cmg4(6) = phi_cntr + mirr_phi1 ! phi1
        dim_cmg4(7) = phi_cntr + mirr_phi2 ! phi2
        call gsvolu(v_i_name, 'CONS', nmed, dim_cmg4, 7, ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        CALL GSATT(v_i_name,'COLO',3)

C---> Create Volume 'CMCV'

        v_i_name = 'CMCV'
        nmed = 24               ! G10; TEMPORARY

        cmcv_rout = mirr_r - cmg1_thck - cmg4_thck
        cmcv_the1 = mirr_the1
        cmcv_the2 = mirr_thcut
        cmcv_z    = cmcv_rout * (cosd(cmcv_the1) + cosd(cmcv_the2)) / 2.
        cmcv_dz   = cmcv_rout * (cosd(cmcv_the1) - cosd(cmcv_the2)) / 2.

        dim_cmcv(1) = cmcv_dz   ! dz
        dim_cmcv(2) = cmcv_rout * sind(cmcv_the2) - cmcv_thck ! Rmin at -dz
        dim_cmcv(3) = cmcv_rout * sind(cmcv_the2)             ! Rmax at -dz
        dim_cmcv(4) = cmcv_rout * sind(cmcv_the1) - cmcv_thck ! Rmin at +dz
        dim_cmcv(5) = cmcv_rout * sind(cmcv_the1)             ! Rmax at +dz
        dim_cmcv(6) = phi_cntr + mirr_phi1 ! phi1
        dim_cmcv(7) = phi_cntr + mirr_phi2 ! phi2
        call gsvolu(v_i_name, 'CONS', nmed, dim_cmcv, 7, ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        CALL GSATT(v_i_name,'COLO',5)

C--> Create Volume 'CMCB'

        v_i_name = 'CMCB'
        nmed = 24               ! G10; TEMPORARY

        call gsvolu(v_i_name,'TUBS',nmed,dim_cmcb,5,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        CALL GSATT(v_i_name,'COLO',2)

C--> Create Volume 'CMCI'

        v_i_name = 'CMCI'
        nmed = nmed_gas

        dim_cmci(1) = dim_cmcb(1) + cmci_thck
        dim_cmci(2) = dim_cmcb(2) - cmci_thck
        dim_cmci(3) = dim_cmcb(3) - cmci_thck
        dim_cmci(4) = dim_cmcb(4)
        dim_cmci(5) = dim_cmcb(5)
        call gsvolu(v_i_name,'TUBS',nmed,dim_cmci,5,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C--> Create Volume 'CMRB'

        v_i_name = 'CMRB'
        nmed = 24               ! G10; TEMPORARY

        cmrb_rin =
     &       sqrt ((mirr_dz - dim_cmcb(3) * 2.) ** 2
     &           + ((dim_cmcb(1)+dim_cmcb(2)) / 2.) ** 2)
     &       - cmrb_thck / 2.
        dim_cmrb(1) = cmrb_rin
        dim_cmrb(2) = cmrb_rin + cmrb_thck
        dim_cmrb(3) = cmrb_wdth / 2.
        dim_cmrb(4) =
     &       180. - acosd ((mirr_dz - dim_cwi2(3) * 2.) / dim_cmrb(1))
        dim_cmrb(5) =
     &       180. - acosd ((mirr_dz - dim_cmcb(3) * 2.) / dim_cmrb(2))
        call gsvolu(v_i_name,'TUBS',nmed,dim_cmrb,5,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)
        CALL GSATT(v_i_name,'COLO',2)

C--> Create Volume 'CMRI'

        v_i_name = 'CMRI'
        nmed = nmed_gas

        dim_cmri(1) = dim_cmrb(1) + cmri_thck
        dim_cmri(2) = dim_cmrb(2) - cmri_thck
        dim_cmri(3) = dim_cmrb(3) - cmri_thck
        dim_cmri(4) = dim_cmrb(4)
        dim_cmri(5) = dim_cmrb(5)
        call gsvolu(v_i_name,'TUBS',nmed,dim_cmri,5,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C The two volume follows are "virtual volume"s ...just to
C provide auxially tracking information to aid Cerenkov analysis.
C They are decalred as made of CO2...As a real detector, this is completely
C unrealistic. I chose nmed=8 simply because this is very light and very
C think in terms of radiation length. Therefore, the presence of volume
C CTR1 and CTR2 does not affect the tracks that passed through it.

C---> Create volume 'CTR1'... virtual tracking detector

        v_i_name = 'CTR1'
c      nmed = 8      ! sensitive CO2 gas (to simulate ZERO material)
        nmed = 10               ! Silicion (sensitive)
        npar = 5
        dim_vtr1(1) = vtr1_rin
        dim_vtr1(2) = vtr1_rout
        dim_vtr1(3) = vtr1_z
        dim_vtr1(4) = vtr1_phi1
        dim_vtr1(5) = vtr1_phi2
        call gsvolu(v_i_name,'TUBS',nmed,dim_vtr1,npar,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C---> Create volume 'CTR2'... virtual tracking detector

        v_i_name = 'CTR2'
c      nmed = 8      ! sensitive CO2 gas (to simulate ZERO material)
        nmed = 10               ! Silicion (sensitive)
        npar = 5
        dim_vtr2(1) = vtr2_rin
        dim_vtr2(2) = vtr2_rout
        dim_vtr2(3) = vtr2_z
        dim_vtr2(4) = vtr2_phi1
        dim_vtr2(5) = vtr2_phi2
        call gsvolu(v_i_name,'TUBS',nmed,dim_vtr2,npar,ivolu)
        CALL GSATT(v_i_name,'SEEN',kSeen)

C---> Define Rotation Matrix

c     by cfm, may 1, 1992
c        the rotation matrix numbers have to be flexible;
c        previous code actually gave a conflict with subsequent
c        rotation matrix definitions

c     Define rotation matrix for West arm (#1)

        irot = irot + 1
        call gsrotm(irot,
     &       90., -phi_open, 90., 90.-phi_open, 0., 0.)
        irot_west = irot        ! 30.Oct.96 KS

c     Define rotation matrix for East arm (#2)

        irot = irot + 1
        call gsrotm(irot,
     &       90., phi_cntr*2.+phi_open,
     &       90., phi_cntr*2.+phi_open-90.,
     &       180., 0.)
        irot_east = irot        ! 30.Oct.96 KS

c     Define rotation matrix for reflection relative to z=0 plane.

        irot = irot + 1         ! new cfm
        call gsrotm(irot, 90., 0., 90., 90., 180., 0.) !x=x',y=y',z=-z
        irot_zref = irot        ! 05.Feb.96 KS

c     Define rotation matrix for reflection relative to x=0 plane.

        irot = irot + 1         ! new cfm
        call gsrotm(irot, 90., 180., 90., 90., 0., 0.) !x=-x',y=y',z=z
        irot_xref = irot        ! 05.Feb.96 KS

c     Define rotation matrix for reflection relative to z=0 & x=0 planes.

        irot = irot + 1
        call gsrotm(irot, 90., 180., 90., 90., 180., 0.) !x=-x',y=y',z=-z
        irot_zxref = irot       ! 05.Feb.96 KS

c     Define rotation matrix for second "sector" (CSHB in CARM)

        irot = irot + 1
        call gsrotm(irot,
     &       90., phi_cntr*2., 90., phi_cntr*2.-90., 180., 0.)
        irot_cshb = irot       ! 18.Oct.96 KS

c     Define rotation matrix for CPHO in first "sector" (CRDA)

        irot = irot + 1
        call gsrotm(irot,
     &       90., +phi_cpho_off, 90., 90.+phi_cpho_off, 0., 0.)
        irot_cpho(1) = irot       ! 23.Oct.96 KS

c     Define rotation matrix for CPHO in second "sector" (CRDB)

        irot = irot + 1
        call gsrotm(irot,
     &       90., -phi_cpho_off, 90., 90.-phi_cpho_off, 0., 0.)
        irot_cpho(2) = irot       ! 23.Oct.96 KS

c     Define rotation matrix for first end plate

        irot = irot + 1
        call gsrotm(irot, 90., phi_cshe1, 90., phi_cshe1+90., 0., 0.)
                                ! (x,y) rotated by phi_cshe1
        irot_phi1 = irot        ! 12.Feb.96 KS

c     Define rotation matrix for second end plate

        irot = irot + 1
        call gsrotm(irot, 90., phi_cshe2, 90., phi_cshe2+90., 0., 0.)
                                ! (x,y) rotated by phi_cshe2
        irot_phi2 = irot        ! 12.Feb.96 KS

c     Define rotation matrices for hatches

        irot = irot + 1
        call gsrotm(irot,
     &       tilt_chtc+90., phi_cntr-dphi_chtc,
     &                 90., phi_cntr-dphi_chtc+90.,
     &       tilt_chtc,     phi_cntr-dphi_chtc)
        irot_chtc(1) = irot     ! 15.Feb.96 KS

        irot = irot + 1
        call gsrotm(irot,
     &       tilt_chtc+90., phi_cntr,
     &                 90., phi_cntr+90.,
     &       tilt_chtc,     phi_cntr)
        irot_chtc(2) = irot     ! 15.Feb.96 KS

        irot = irot + 1
        call gsrotm(irot,
     &       tilt_chtc+90., phi_cntr+dphi_chtc,
     &                 90., phi_cntr+dphi_chtc+90.,
     &       tilt_chtc,     phi_cntr+dphi_chtc)
        irot_chtc(3) = irot     ! 15.Feb.96 KS

c Define rotation matrices for window supports
c     12.Aug.96 KS

        do i = 1, n_cbm7
          irot = irot + 1
          phi = phi_cshe1 + dphi_cshe / float(n_cbm7 + 1) * float(i)
          call gsrotm(irot, 90., phi, 90., phi+90., 0., 0.)
                                ! (x,y) rotated by phi
          irot_cbm7(i) = irot
        end do

        do i = 1, n_cbm8
          irot = irot + 1
          phi = phi_cshe1 + dphi_cshe / float(n_cbm8 + 1) * float(i)
          call gsrotm(irot, 90., phi, 90., phi+90., 0., 0.)
                                ! (x,y) rotated by phi
          irot_cbm8(i) = irot
        end do

c Define rotation matrices for gap between mirror sections at const. phi
c     20.Nov.96 KS

        do i = 1, n_cmg1
          irot = irot + 1
          phi = phi_cntr + mirr_phi1
     &         + (mirr_phi2 - mirr_phi1) / float(n_cmg1-1) * float(i-1)
          call gsrotm(irot, 0., 0., 90., phi, 90., 90.+phi)
          irot_cmg1(i) = irot
        end do

c Define rotation matrices for mirror support rib beams
c     05.Jun.98 KS

        do i = 1, n_cmrb
          irot = irot + 1
          phi = phi_cntr + phi_cmrb(i)
          call gsrotm(irot, 0., 0., 90., phi, 90., 90.+phi)
          irot_cmrb(i) = irot
        end do

C---> Place CARM in HALL (changed from CERK on 26.Feb.96)
C     We place four copies.
C     One in null rotation, three in mirror positions
C     relative to x=0 and z=0 planes.

        v_i_name = 'CARM'
c        v_m_name = 'CERK'
        v_m_name = 'HALL'
c first copy of CARM
        nr = 1                  !copy number
c       call gspos(v_i_name,nr,v_m_name,0.0,0.0,0.0,irot_west,'ONLY')
        call gspos(v_i_name,nr,wCRK,0.0,0.0,0.0,irot_west,'ONLY')
c second copy of CARM
        nr = 2                  !copy number
ccc        call gspos(v_i_name,nr,v_m_name,0.0,0.0,0.0,irot_zref,'ONLY')
cccc third copy of CSEC
ccc        nr = 3                  !copy number
ccc        call gspos(v_i_name,nr,v_m_name,0.0,0.0,0.0,irot_xref,'ONLY')
cccc fourth copy of CSEC
ccc        nr = 4                  !copy number
c       call gspos(v_i_name,nr,v_m_name,0.0,0.0,0.0,irot_east,'ONLY')
        call gspos(v_i_name,nr,eCRK,0.0,0.0,0.0,irot_east,'ONLY')

C---> Place CBM1 in CARM (outside of end-plates)

        v_i_name = 'CBM1'
        v_m_name = 'CARM'
c place three beams (CBM1) outside of end-plates of a CARM
c     16.Oct.96, 6 beams (3 in z>0 and 3 in z<0) -- K.Shigaki
c first beam (lower side)
        call crk_rot(pos_cbm1(1,1), phi_cshe2)
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbm1(1,1), pos_cbm1(2,1), pos_cbm1(3,1),
     &       irot_phi2, 'ONLY')
        nr = 4                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbm1(1,1), pos_cbm1(2,1), -pos_cbm1(3,1),
     &       irot_phi2, 'ONLY')
c second beam (upper side)
        call crk_rot(pos_cbm1(1,2), phi_cshe1)
        nr = 2                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbm1(1,2), pos_cbm1(2,2), pos_cbm1(3,2),
     &       irot_phi1, 'ONLY')
        nr = 5                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbm1(1,2), pos_cbm1(2,2), -pos_cbm1(3,2),
     &       irot_phi1, 'ONLY')
c third beam (lower side)
        call crk_rot(pos_cbm1(1,3), phi_cshe2)
        nr = 3                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbm1(1,3), pos_cbm1(2,3), pos_cbm1(3,3),
     &       irot_phi2, 'ONLY')
        nr = 6                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbm1(1,3), pos_cbm1(2,3), -pos_cbm1(3,3),
     &       irot_phi2, 'ONLY')

C---> Place CBI1 in CBM1

        v_i_name = 'CBI1'
        v_m_name = 'CBM1'
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbi1(1), pos_cbi1(2), pos_cbi1(3), irotnull, 'ONLY')

C---> Place CBM2 in CARM (outside of end-plates)

        v_i_name = 'CBM2'
        v_m_name = 'CARM'
c place two beams (CBM2) outside of end-plates of a CARM
c     16.Oct.96, 4 beams (2 in z>0 and 2 in z<0) -- K.Shigaki
c first beam (upper)
        call crk_rot(pos_cbm2(1,1), phi_cshe1)
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbm2(1,1), pos_cbm2(2,1), pos_cbm2(3,1),
     &       irot_phi1, 'ONLY')
        nr = 3                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbm2(1,1), pos_cbm2(2,1), -pos_cbm2(3,1),
     &       irot_phi1, 'ONLY')
c second beam (lower)
        call crk_rot(pos_cbm2(1,2), phi_cshe2)
        nr = 2                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbm2(1,2), pos_cbm2(2,2), pos_cbm2(3,2),
     &       irot_phi2, 'ONLY')
        nr = 4                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbm2(1,2), pos_cbm2(2,2), -pos_cbm2(3,2),
     &       irot_phi2, 'ONLY')

C---> Place CBI2 in CBM2

        v_i_name = 'CBI2'
        v_m_name = 'CBM2'
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbi2(1), pos_cbi2(2), pos_cbi2(3), irotnull, 'ONLY')

C---> Place CBM7 in CARM (outside of entrance window)

        v_i_name = 'CBM7'
        v_m_name = 'CARM'

        do i = 1, n_cbm7
          nr = i                ! copy number
          phi = phi_cshe1 + dphi_cshe / float(n_cbm7 + 1) * float(i)

          call gspos(v_i_name, nr, v_m_name,
     &         pos_cbm7(1)*cosd(phi)-pos_cbm7(2)*sind(phi),
     &         pos_cbm7(1)*sind(phi)+pos_cbm7(2)*cosd(phi),
     &         pos_cbm7(3), irot_cbm7(i), 'ONLY')

        end do

C---> Place CBI7 in CBM7

        v_i_name = 'CBI7'
        v_m_name = 'CBM7'
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbi7(1), pos_cbi7(2), pos_cbi7(3), irotnull, 'ONLY')

C---> Place CBM8 in CARM (outside of entrance window)

        v_i_name = 'CBM8'
        v_m_name = 'CARM'

        do i = 1, n_cbm8
          nr = i                ! copy number
          phi = phi_cshe1 + dphi_cshe / float(n_cbm8 + 1) * float(i)

          call gspos(v_i_name, nr, v_m_name,
     &         pos_cbm8(1)*cosd(phi)-pos_cbm8(2)*sind(phi),
     &         pos_cbm8(1)*sind(phi)+pos_cbm8(2)*cosd(phi),
     &         pos_cbm8(3), irot_cbm8(i), 'ONLY')

        end do

C---> Place CBI8 in CBM8

        v_i_name = 'CBI8'
        v_m_name = 'CBM8'
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbi8(1), pos_cbi8(2), pos_cbi8(3), irotnull, 'ONLY')

C---> Place CLS1 in CARM

        v_i_name = 'CLS1'
        v_m_name = 'CARM'

        nr = i                  ! copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cls1(1), pos_cls1(2), pos_cls1(3), irotnull, 'ONLY')

C---> Place CLS2 in CARM

        v_i_name = 'CLS2'
        v_m_name = 'CARM'

        nr = i                  ! copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cls2(1), pos_cls2(2), pos_cls2(3), irotnull, 'ONLY')

C---> Place in CSHA/B in CARM

        v_m_name = 'CARM'

        v_i_name = 'CSHA'
        nr = 1                  !copy number
        call gspos(v_i_name,nr,v_m_name,0.0,0.0,0.0,irotnull,'ONLY')

        v_i_name = 'CSHB'
        nr = 1                  !copy number
        call gspos(v_i_name,nr,v_m_name,0.0,0.0,0.0,irot_cshb,'ONLY')
cc
cc ADDED BY K.SHIGAKI ON 18.OCT.96
cc
c        nr = 2                  !copy number
c        call gspos(v_i_name,nr,v_m_name,0.0,0.0,0.0,irot_cshe,'ONLY')

C---> Place CWI1 in CSHE

        v_i_name  = 'CWI1'
        v_m_name1 = 'CSHA'
        v_m_name2 = 'CSHB'
        nr = 1                  !copy number
        call gspos(v_i_name,nr,v_m_name1,
     &       pos_cwi1(1), pos_cwi1(2), pos_cwi1(3),
     &       irotnull, 'ONLY')
        call gspos(v_i_name,nr,v_m_name2,
     &       pos_cwi1(1), pos_cwi1(2), pos_cwi1(3),
     &       irotnull, 'ONLY')

C---> Place CWI2 in CSHE

        v_i_name  = 'CWI2'
        v_m_name1 = 'CSHA'
        v_m_name2 = 'CSHB'
        nr = 1                  !copy number
        call gspos(v_i_name,nr,v_m_name1,
     &       pos_cwi2(1), pos_cwi2(2), pos_cwi2(3),
     &       irotnull, 'ONLY')
        call gspos(v_i_name,nr,v_m_name2,
     &       pos_cwi2(1), pos_cwi2(2), pos_cwi2(3),
     &       irotnull, 'ONLY')

C---> Place CRDA/B in CSHA/B

        v_i_name = 'CRDA'
        v_m_name = 'CSHA'
        nr = 1                  !copy number
        call gspos(v_i_name,nr,v_m_name,0.0,0.0,0.0,irotnull,'ONLY')

        v_i_name = 'CRDB'
        v_m_name = 'CSHB'
        nr = 1                  !copy number
        call gspos(v_i_name,nr,v_m_name,0.0,0.0,0.0,irotnull,'ONLY')

C---> Place CEND in CRDA/B

        v_i_name  = 'CEND'
        v_m_name1 = 'CRDA'
        v_m_name2 = 'CRDB'
c first wall (end-plate)
        nr = 1                  !copy number
        call crk_rot(pos_cend(1,1), phi_cshe1)
        call gspos(v_i_name, nr, v_m_name1,
     &       pos_cend(1,1), pos_cend(2,1), pos_cend(3,1),
     &       irot_phi1, 'ONLY')
        call gspos(v_i_name, nr, v_m_name2,
     &       pos_cend(1,1), pos_cend(2,1), pos_cend(3,1),
     &       irot_phi1, 'ONLY')
c second wall (end-plate)
        nr = 2                  !copy number
        call crk_rot(pos_cend(1,2), phi_cshe2)
        call gspos(v_i_name, nr, v_m_name1,
     &       pos_cend(1,2), pos_cend(2,2), pos_cend(3,2),
     &       irot_phi2, 'ONLY')
        call gspos(v_i_name, nr, v_m_name2,
     &       pos_cend(1,2), pos_cend(2,2), pos_cend(3,2),
     &       irot_phi2, 'ONLY')

C---> Place CBM3 in CRDA/B (inside of end-plates)

        v_i_name  = 'CBM3'
        v_m_name1 = 'CRDA'
        v_m_name2 = 'CRDB'
c first beam (upper)
        nr = 1                  !copy number
        call crk_rot(pos_cbm3(1,1), phi_cshe1)
        call gspos(v_i_name, nr, v_m_name1,
     &       pos_cbm3(1,1), pos_cbm3(2,1), pos_cbm3(3,1),
     &       irot_phi1, 'ONLY')
        call gspos(v_i_name, nr, v_m_name2,
     &       pos_cbm3(1,1), pos_cbm3(2,1), pos_cbm3(3,1),
     &       irot_phi1, 'ONLY')
c second beam (lower)
        nr = 2                  !copy number
        call crk_rot(pos_cbm3(1,2), phi_cshe2)
        call gspos(v_i_name, nr, v_m_name1,
     &       pos_cbm3(1,2), pos_cbm3(2,2), pos_cbm3(3,2),
     &       irot_phi2, 'ONLY')
        call gspos(v_i_name, nr, v_m_name2,
     &       pos_cbm3(1,2), pos_cbm3(2,2), pos_cbm3(3,2),
     &       irot_phi2, 'ONLY')
c third beam (upper)
        nr = 3                  !copy number
        call crk_rot(pos_cbm3(1,3), phi_cshe1)
        call gspos(v_i_name, nr, v_m_name1,
     &       pos_cbm3(1,3), pos_cbm3(2,3), pos_cbm3(3,3),
     &       irot_phi1, 'ONLY')
        call gspos(v_i_name, nr, v_m_name2,
     &       pos_cbm3(1,3), pos_cbm3(2,3), pos_cbm3(3,3),
     &       irot_phi1, 'ONLY')
c fourth beam (lower)
        nr = 4                  !copy number
        call crk_rot(pos_cbm3(1,4), phi_cshe2)
        call gspos(v_i_name, nr, v_m_name1,
     &       pos_cbm3(1,4), pos_cbm3(2,4), pos_cbm3(3,4),
     &       irot_phi2, 'ONLY')
        call gspos(v_i_name, nr, v_m_name2,
     &       pos_cbm3(1,4), pos_cbm3(2,4), pos_cbm3(3,4),
     &       irot_phi2, 'ONLY')

C---> Place CBI3 in CBM3

        v_i_name = 'CBI3'
        v_m_name = 'CBM3'
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbi3(1), pos_cbi3(2), pos_cbi3(3), irotnull, 'ONLY')

C---> Place CBM4 in CRDA/B (inside of end-plates)

        v_i_name  = 'CBM4'
        v_m_name1 = 'CRDA'
        v_m_name2 = 'CRDB'
c first beam (upper)
        nr = 1                  !copy number
        call crk_rot(pos_cbm4(1,1), phi_cshe1)
        call gspos(v_i_name, nr, v_m_name1,
     &       pos_cbm4(1,1), pos_cbm4(2,1), pos_cbm4(3,1),
     &       irot_phi1, 'ONLY')
        call gspos(v_i_name, nr, v_m_name2,
     &       pos_cbm4(1,1), pos_cbm4(2,1), pos_cbm4(3,1),
     &       irot_phi1, 'ONLY')
c second beam (lower)
        nr = 2                  !copy number
        call crk_rot(pos_cbm4(1,2), phi_cshe2)
        call gspos(v_i_name, nr, v_m_name1,
     &       pos_cbm4(1,2), pos_cbm4(2,2), pos_cbm4(3,2),
     &       irot_phi2, 'ONLY')
        call gspos(v_i_name, nr, v_m_name2,
     &       pos_cbm4(1,2), pos_cbm4(2,2), pos_cbm4(3,2),
     &       irot_phi2, 'ONLY')

C---> Place CBI4 in CBM4

        v_i_name = 'CBI4'
        v_m_name = 'CBM4'
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbi4(1), pos_cbi4(2), pos_cbi4(3), irotnull, 'ONLY')

C---> Place CHCT in CRDA/B

        v_i_name  = 'CHTC'
        v_m_name1 = 'CRDA'
        v_m_name2 = 'CRDB'
c first hatch
        nr = 1                  ! copy number
        call crk_rot(pos_chtc, (phi_cntr-dphi_chtc))
        call gspos(v_i_name, nr, v_m_name1,
     &       pos_chtc(1), pos_chtc(2), pos_chtc(3),
     &       irot_chtc(1), 'ONLY')
        call gspos(v_i_name, nr, v_m_name2,
     &       pos_chtc(1), pos_chtc(2), pos_chtc(3),
     &       irot_chtc(1), 'ONLY')
c second hatch
        nr = 2                  ! copy number
        call crk_rot(pos_chtc, dphi_chtc)
        call gspos(v_i_name, nr, v_m_name1,
     &       pos_chtc(1), pos_chtc(2), pos_chtc(3),
     &       irot_chtc(2), 'ONLY')
        call gspos(v_i_name, nr, v_m_name2,
     &       pos_chtc(1), pos_chtc(2), pos_chtc(3),
     &       irot_chtc(2), 'ONLY')
c third hatch
        nr = 3                  ! copy number
        call crk_rot(pos_chtc, dphi_chtc)
        call gspos(v_i_name, nr, v_m_name1,
     &       pos_chtc(1), pos_chtc(2), pos_chtc(3),
     &       irot_chtc(3), 'ONLY')
        call gspos(v_i_name, nr, v_m_name2,
     &       pos_chtc(1), pos_chtc(2), pos_chtc(3),
     &       irot_chtc(3), 'ONLY')

C---> Place CBM5 in CHCT

        v_i_name = 'CBM5'
        v_m_name = 'CHTC'
c first beam
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbm5(1,1), pos_cbm5(2,1), pos_cbm5(3,1),
     &       irotnull, 'ONLY')
c second beam
        nr = 2                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbm5(1,2), pos_cbm5(2,2), pos_cbm5(3,2),
     &       irotnull, 'ONLY')

C---> Place CBI5 in CBM5

        v_i_name = 'CBI5'
        v_m_name = 'CBM5'
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbi5(1), pos_cbi5(2), pos_cbi5(3), irotnull, 'ONLY')

C---> Place CBM6 in CHCT

        v_i_name = 'CBM6'
        v_m_name = 'CHTC'
c first beam
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbm6(1,1), pos_cbm6(2,1), pos_cbm6(3,1),
     &       irotnull, 'ONLY')
c second beam
        nr = 2                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbm6(1,2), pos_cbm6(2,2), pos_cbm6(3,2),
     &       irotnull, 'ONLY')

C---> Place CBI6 in CBM6

        v_i_name = 'CBI6'
        v_m_name = 'CBM6'
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     &       pos_cbi6(1), pos_cbi6(2), pos_cbi6(3), irotnull, 'ONLY')

C---> Place CMST in CRDA/B

        v_i_name  = 'CMST'      !Volume name
        v_m_name1 = 'CRDA'      !Mother volume name
        v_m_name2 = 'CRDB'      !Mother volume name
        nr = 1                  !copy number
        pos_cmir(1) = 0.        !x of the center
        pos_cmir(2) = 0.        !y of the center
        pos_cmir(3) = mirr_dz   !z of the center
        Mirr_cent(1)=pos_cmir(1) ! center of the mirror is passed to CRK_GUSTEP
        Mirr_cent(2)=pos_cmir(2)
        Mirr_cent(3)=pos_cmir(3)
        call gspos(v_i_name,nr, v_m_name1,
     $       pos_cmir(1), pos_cmir(2), pos_cmir(3),
     $       irotnull, 'ONLY')
        call gspos(v_i_name,nr, v_m_name2,
     $       pos_cmir(1), pos_cmir(2), pos_cmir(3),
     $       irotnull, 'ONLY')

C---> Place CMIR in CMST

        v_i_name = 'CMIR'      !Volume name
        v_m_name = 'CMST'       !Mother volume name
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     $       0.0, 0.0, 0.0, irotnull, 'ONLY')

C---> Place CMIF in CMST

        v_i_name = 'CMIF'       !Volume name
        v_m_name = 'CMST'       !Mother volume name
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     $       0.0, 0.0, 0.0, irotnull, 'ONLY')

C---> Place CMG1 in CMST

        v_i_name = 'CMG1'       !Volume name
        v_m_name = 'CMST'       !Mother volume name
        do nr = 1, n_cmg1       !copy number
          call gspos(v_i_name, nr, v_m_name,
     $         0.0, 0.0, 0.0, irot_cmg1(nr), 'ONLY')
        enddo

C---> Place CMG2 in CMST

        v_i_name = 'CMG2'       !Volume name
        v_m_name = 'CMST'       !Mother volume name
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     $       0.0, 0.0, cmg2_z, irotnull, 'ONLY')

C---> Place CMG3 in CMST

        v_i_name = 'CMG3'       !Volume name
        v_m_name = 'CMST'       !Mother volume name
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     $       0.0, 0.0, cmg3_z, irotnull, 'ONLY')

C---> Place CMG4 in CMST

        v_i_name = 'CMG4'       !Volume name
        v_m_name = 'CMST'       !Mother volume name
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     $       0.0, 0.0, cmg4_z, irotnull, 'ONLY')

C---> Place CMCV in CMST

        v_i_name = 'CMCV'       !Volume name
        v_m_name = 'CMST'       !Mother volume name
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     $       0.0, 0.0, cmcv_z, irotnull, 'ONLY')

C---> Place CMCI in CMCB

        v_i_name = 'CMCI'       !Volume name
        v_m_name = 'CMCB'       !Mother volume name
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     $       0.0, 0.0, 0.0, irotnull, 'ONLY')

C---> Place CMCB in CRDA/B

        v_i_name  = 'CMCB'      !Volume name
        v_m_name1 = 'CRDA'      !Mother volume name
        v_m_name2 = 'CRDB'      !Mother volume name
        nr = 1                  !copy number
        call gspos(v_i_name,nr, v_m_name1,
     $       pos_cmcb(1), pos_cmcb(2), pos_cmcb(3),
     $       irotnull, 'ONLY')
        call gspos(v_i_name,nr, v_m_name2,
     $       pos_cmcb(1), pos_cmcb(2), pos_cmcb(3),
     $       irotnull, 'ONLY')

C---> Place CMRI in CMRB

        v_i_name = 'CMRI'       !Volume name
        v_m_name = 'CMRB'       !Mother volume name
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name,
     $       0.0, 0.0, 0.0, irotnull, 'ONLY')

C---> Place CMRB in CRDA/B

        v_i_name  = 'CMRB'      !Volume name
        v_m_name1 = 'CRDA'      !Mother volume name
        v_m_name2 = 'CRDB'      !Mother volume name

        pos_cmrb(1) = 0.        !x of the center
        pos_cmrb(2) = 0.        !y of the center
        pos_cmrb(3) = mirr_dz   !z of the center

        do nr = 1, n_cmg1       !copy number
          call gspos(v_i_name, nr, v_m_name1,
     $         pos_cmrb(1), pos_cmrb(2), pos_cmrb(3),
     $         irot_cmrb(nr), 'ONLY')
          call gspos(v_i_name, nr, v_m_name2,
     $         pos_cmrb(1), pos_cmrb(2), pos_cmrb(3),
     $         irot_cmrb(nr), 'ONLY')
        enddo

C---> Place CCLP...cooling plate...in CRDA/B

        v_i_name  = 'CCLP'       !Volume name
        v_m_name1 = 'CRDA'       !Mother volume name
        v_m_name2 = 'CRDB'       !Mother volume name
        nr = 1                  !copy number
        call gspos(v_i_name,nr,v_m_name1,
     &       pos_cclp(1), pos_cclp(2), pos_cclp(3),
     &       irot_cpho(1), 'ONLY')
        call gspos(v_i_name,nr,v_m_name2,
     &       pos_cclp(1), pos_cclp(2), pos_cclp(3),
     &       irot_cpho(2), 'ONLY')

C---> Place CPHO...photon detector for Cerenkov...in CRDA/B

        v_i_name  = 'CPHO'       !Volume name
        v_m_name1 = 'CRDA'       !Mother volume name
        v_m_name2 = 'CRDB'       !Mother volume name
        nr = 1                  !copy number
        call gspos(v_i_name, nr, v_m_name1,
     &             0.0, 0.0, 0.0, irot_cpho(1), 'ONLY')
        call gspos(v_i_name, nr, v_m_name2,
     &             0.0, 0.0, 0.0, irot_cpho(2), 'ONLY')

c---> Place 32 CPMTs in CSPM

        do ipmt = 1, NCPMT

c position in cpmt_z() is in 10inch unit. (Why? Ask Tom Hemmick and his
c designer), and measured from Z=0 of GLOBAL coordinate.
c convert it to cm, and subtract offset of CPHO center.

c          cpmt_z(ipmt) = cpmt_z(ipmt)*25.4 - pos_phot(3)

c 24.Jun.96, K.Shigaki
c     As CPHO is now PCON, not TUBS, its position offset no longer exists.
c 19.Sep.96, K.Shigaki
c     unit of cpmt_z() changed to cm in the data statement.
c     unit conversion is no longer required.

cks          cpmt_z(ipmt) = cpmt_z(ipmt)*25.4

c Now cpmt_z() is in cm unit. However, this position is measured as the
c "center-of-entrance". In GEANT system, we must position it in terms of
c "center-of-volume". The following two line does the necessary traslation.
c Note that cpmt_th() is the orientaion angle (in degree) of PMT.

c 21.Jan.97, K.Shigaki
c     CPMT changed from TUBE to PCON; no offset is needed any more.

cks          cpmt_z(ipmt) = cpmt_z(ipmt) + dim_cpmt(3)*cosd(cpmt_th(ipmt))
cks          cpmt_r(ipmt) = cpmt_r(ipmt) - dim_cpmt(3)*sind(cpmt_th(ipmt))

c Now cpmt_z() and cpmt_r() is in proper coordinate system.

c Next, I define the rotation matrix for each PMT.

          irot = irot + 1
          cpmt_th(ipmt) = 180.0 - cpmt_th(ipmt)
          call gsrotm(irot,
     $         90.+cpmt_th(ipmt),0.,
     $         90.,90.,
     $         cpmt_th(ipmt),0.)

c Finally, I place a PMT in CSPM.

          call gspos('CPMT',ipmt,'CSPM',
     $         cpmt_r(ipmt),cpmt_dx(ipmt),cpmt_z(ipmt),
     $         irot,'ONLY')

c Also install CBAF in CSPM (optional)

          if (cbaf_install) then
            dx_cbaf = dim_cbaf(3) * sind(cpmt_th(ipmt))
            dz_cbaf = dim_cbaf(3) * cosd(cpmt_th(ipmt))
            call gspos('CBAF',ipmt,'CSPM',
     $           cpmt_r(ipmt)+dx_cbaf,
     $           cpmt_dx(ipmt),
     $           cpmt_z(ipmt)+dz_cbaf,
     $           irot,'ONLY')
          endif

        enddo                   ! loop for ipmt

c---> Place CSBK in CSPM

        call gspos('CSBK', 1, 'CSPM', 0., 0., 0., irotnull,'ONLY')

c---> Place CPME in CPMT

        call gspos('CPME',1,'CPMT',
cks     $       0.,0.,(dim_cpmt(3)-dim_cpme(3)),
     $       0.,0.,-dim_cpme(3),
     $       irotnull,'ONLY')

c---> Place CPMS in CPMT

        call gspos('CPMS', 1, 'CPMT', 0., 0., 0., irotnull,'ONLY')

C---> place "virtual tracker" CTR1 and CTR2 in CRDA/B

c$$$        v_m_name = 'CERK'
        v_m_name1 = 'CRDA'
        v_m_name2 = 'CRDB'
c place CTR1
        v_i_name = 'CTR1'
        nr = 1                  !copy number
        call gspos(v_i_name,nr,v_m_name1,0.0,0.0,vtr1_z,irotnull,'ONLY')
        call gspos(v_i_name,nr,v_m_name2,0.0,0.0,vtr1_z,irotnull,'ONLY')
c place CTR2
        v_i_name = 'CTR2'
        nr = 1                  !copy number
        call gspos(v_i_name,nr,v_m_name1,0.0,0.0,vtr2_z,irotnull,'ONLY')
        call gspos(v_i_name,nr,v_m_name2,0.0,0.0,vtr2_z,irotnull,'ONLY')
      ELSE
        WRITE(6,*)' Ring Imaging Cerenkov: No volumes defined'
      END IF
C  only book detectors & hits if input parameters instruct
      if ( CVOLU_OPT(1,5) .eq. 'FULL' .or.
     &     cvolu_opt(1,5) .eq. 'CBAF' ) then
c     put volume elemtents together into a set

c We put the following two detectors in the set 'CRK '
c     CPME   Cerenkov PMT entrance
c     CMIR   Cerenkov Mirror
c     CBAF   Virtual Light Baffle
c     CTR1/2 Virtual Detector; Aid for analysis tune up, and diagonositcs

c put CPME in set 'CRK '

        set_id = 'CRK '         ! put it in a SET
        v_i_name = 'CPME'
        idtype = 2
        nv = 5
        namesv(1) = 'CARM'
        namesv(2) = 'CSHA'      ! TEMPORARY BY K.SHIGAKI
        namesv(3) = 'CSHB'      ! TEMPORARY BY K.SHIGAKI
        namesv(4) = 'CSPM'
        namesv(5) = 'CPMT'
        nbitsv(1) = 8
        nbitsv(2) = 8
        nbitsv(3) = 8
        nbitsv(4) = 8
        nbitsv(5) = 8
        nwpa = 100              ! for now
        nwsa = 100              ! for now
        call gsdet(set_id,v_i_name,nv,namesv,nbitsv,idtype,nwpa,nwsa,
     1       iset,idet)
        call gsdeth(set_id,v_i_name,nh,namesh,nbitsh,orig,fact)

c put 'CMIR' in set 'CRK '. All parameters except for v_i_name are the
c same as those of 'CPME'

        set_id = 'CRK '         ! put it in a SET
        v_i_name = 'CMIR'
        idtype = 0
        nv = 3
        namesv(1) = 'CARM'
        namesv(2) = 'CSHA'
        namesv(3) = 'CSHB'
        call gsdet(set_id,v_i_name,nv,namesv,nbitsv,idtype,nwpa,nwsa,
     1       iset,idet)
        call gsdeth(set_id,v_i_name,nh,namesh,nbitsh,orig,fact)

c put 'CBAF' in set 'CRK '. All parameters except for v_i_name are the
c same as those of 'CPME'

        if (cvolu_opt(1,5) .eq. 'CBAF') then
          set_id = 'CRK '       ! put it in a SET
          v_i_name = 'CBAF'
          idtype = 2
          nv = 5
          namesv(1) = 'CARM'
          namesv(2) = 'CSHA'
          namesv(3) = 'CSHB'
          namesv(4) = 'CSPM'
          namesv(5) = 'CBAF'
          call gsdet(set_id,v_i_name,nv,namesv,nbitsv,idtype,nwpa,nwsa,
     1         iset,idet)
          call gsdeth(set_id,v_i_name,nh,namesh,nbitsh,orig,fact)
        endif

c put virtual tracker 'CTR1' and 'CTR2' in set 'CRK '

c put CTR1
        set_id = 'CRK '         ! put it in a SET
        v_i_name = 'CTR1'
        idtype = 0
        nv = 3
        namesv(1) = 'CARM'
        namesv(2) = 'CSHA'
        namesv(3) = 'CSHB'
        call gsdet(set_id,v_i_name,nv,namesv,nbitsv,idtype,nwpa,nwsa,
     1       iset,idet)
        call gsdeth(set_id,v_i_name,nh,namesh,nbitsh,orig,fact)
c put CTR2
        set_id = 'CRK '         ! put it in a SET
        v_i_name = 'CTR2'
        idtype = 0
        nv = 3
        namesv(1) = 'CARM'
        namesv(2) = 'CSHA'
        namesv(3) = 'CSHB'
        call gsdet(set_id,v_i_name,nv,namesv,nbitsv,idtype,nwpa,nwsa,
     1       iset,idet)
        call gsdeth(set_id,v_i_name,nh,namesh,nbitsh,orig,fact)
      ELSE
        WRITE(6,*)' Ring Imaging Cerenkov: No detectors',
     &       ' & hits defined'
      END IF

c   End of detector geometry set up

 9999 continue
      return

 999  continue
      write(6,1000)
      
 1000 format(/'crk - read error in crk_par segment')
      stop 'crk -  Namelist mis-match in crk_par segment'
      end
