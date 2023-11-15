C $Id: hbd_trk_media.f,v 1.14 2010/07/14 12:05:48 ravini Exp $

C       File name: hbd_trk_media.f
C       ---------
C     Authors: Christine Aidala, Nikolai Smirnov (STAR)
C     Additions by Ilia Ravinovich
C     Purpose: Define the materials used in the TPC/HBD

      SUBROUTINE hbd_trk_media(gas)

      Implicit none

C       Global Declarations
C       -------------------
#include "tpc_ckovpar.inc"
#include "hbd_mednmb.inc"
*CAA Use gctmed.inc for magnetic field in materials variables
#include "gctmed.inc"

C       Local Declarations
C       ------------------

      real Amylar(3), Zmylar(3), Wmylar(3),             ! Mylar for gas volumes
     >     Ag10(5), Zg10(5), Wg10(5),                     ! G10 board for FEE
     >     Acsi(2), Zcsi(2), Wcsi(2),                          ! CsI for HBD
     >     Ach4gas(2), Zch4gas(2), Wch4gas(2),    ! CH4 as possible radiator/drift gas
     >     Acf4gas(2), Zcf4gas(2), Wcf4gas(2)       ! CF4 as possible radiator/drift gas

      real a, z, dens, radl, absl, ubuf
      integer imate, nwbuf, i100

      ! make sure materials
      ! are initialized only once
      integer icall
      data icall/0/

      integer ii
      character*3 gas

      Data Amylar / 12.01, 1.01, 15.99/
      Data Zmylar / 6., 1., 8. /
      Data Wmylar / 5., 4., 2. /
      Data Ag10   / 28.08, 16., 12., 1., 16./
      Data Zg10   / 14., 8., 6., 1., 8. /
      Data Wg10   / 0.28, 0.32, 0.22, 0.03, 0.15 /
      Data Acsi   / 132., 126. /
      Data Zcsi   / 55., 53. /
      Data Wcsi   / 1., 1. /
* Info for various possible gases that may be used in the TPC/HBD
* CH4
      Data Ach4gas / 12.01, 1.01/
      Data Zch4gas / 6., 1. /
      Data Wch4gas / 1., 4. /
* CF4
      Data Acf4gas / 12.01, 19.00 /
      Data Zcf4gas / 6., 9. /
      Data Wcf4gas / 1., 4. /

* ...............................................................
*CAA Info for Cerenkov radiation

*CAA Array of momentum values for Cerenkov photons (eV/c)
      data ppckov/ 5.5e-9,  5.6e-9,  5.7e-9,  5.8e-9,  5.9e-9,
     >             6.0e-9,  6.1e-9,  6.2e-9,  6.3e-9,  6.4e-9,
     >             6.5e-9,  6.6e-9,  6.7e-9,  6.8e-9,  6.9e-9,
     >             7.0e-9,  7.1e-9,  7.2e-9,  7.3e-9,  7.4e-9,
     >             7.5e-9,  7.6e-9,  7.7e-9,  7.8e-9,  7.9e-9,
     >             8.0e-9,  8.1e-9,  8.2e-9,  8.4e-9,  8.6e-9,
     >             8.8e-9,  9.0e-9,  9.2e-9,  9.4e-9,  9.6e-9,
     >             9.8e-9, 10.0e-9, 10.2e-9, 10.4e-9, 10.6e-9,
     >            10.8e-9, 11.0e-9, 11.2e-9, 11.4e-9, 11.5e-9 /


*CAA Index of refraction for G10, mylar, and CsI unnecessary because they're absorbers, not radiators
*CAA --put a dummy value of 1.
      data rindex_dummy  /Nck * 1.      /   ! dummy value
      data rindex_gas    /Nck * -1.     /   ! Initialize index of refraction for gas

*CAA Calculate index of refraction for various momenta from the following equation:
*CAA n - 1 = A / (1/lambda0**2 - 1/lambda**2)

*CAA CH4: A = 0.05517E-6, 1/lambda0**2 = 1.27677E-4    --Values from A. Milov
       data rindex_ch4/1.000511, 1.000514, 1.000518, 1.000521, 1.000525,
     >                1.000529, 1.000533, 1.000537, 1.000541, 1.000546,
     >                1.000550, 1.000555, 1.000560, 1.000565, 1.000570,
     >                1.000575, 1.000581, 1.000587, 1.000593, 1.000599,
     >                1.000605, 1.000612, 1.000618, 1.000625, 1.000633,
     >                1.000640, 1.000648, 1.000656, 1.000674, 1.000692,
     >                1.000712, 1.000734, 1.000758, 1.000784, 1.000813,
     >                1.000844, 1.000879, 1.000917, 1.000959, 1.001007,
     >                1.001061, 1.001122, 1.001192, 1.001272, 1.001318 /

C---- The refractivity of CF4 at room temperature (T=293.15K) (A.Kozlov, 13.08.04)
       data rindex_cf4/1.000480, 1.000482, 1.000483, 1.000485, 1.000486,
     >        1.000488, 1.000490, 1.000491, 1.000493, 1.000495,
     >        1.000497, 1.000498, 1.000500, 1.000502, 1.000504,
     >        1.000506, 1.000508, 1.000510, 1.000512, 1.000514,
     >        1.000517, 1.000519, 1.000521, 1.000524, 1.000526,
     >        1.000529, 1.000531, 1.000534, 1.000539, 1.000545,
     >        1.000550, 1.000557, 1.000563, 1.000570, 1.000577,
     >        1.000584, 1.000592, 1.000600, 1.000608, 1.000617,
     >        1.000626, 1.000636, 1.000646, 1.000657, 1.000662 /

* Absorption lengths (in cm)
      data absco_gri     /Nck * 0.00001  /
      data absco_csi     /Nck * 0.00001  /
      data absco_mylar   /Nck * 0.00001  /
      data absco_gas     /Nck * -1.      /  ! Initialize absorption lengths of gas

      data absco_ch4  /
     >     1.e6,   1.e6,   1.e6,   1.e6,   1.e6,
     >     1.e6,   1.e6,   1.e6,   1.e6,   1.e6,
     >     1.e6,   1.e6,   1.e6,   1.e6,   1.e6,
     >     1.e6,   1.44e6, 1.28e6, 1.12e6, 9.6e5,
     >     8.0e5,  6.41e5, 4.81e5, 3.21e5, 1.62e5,
     >     1791.,  351.,   50.5,   11.2,   0.001,
     >     0.0,    0.0,    0.0,    0.0,    0.0,
     >     0.0,    0.0,    0.0,    0.0,    0.0,
     >     0.0,    0.0,    0.0,    0.0,    0.0 /

* Absorption lengths (in cm)
* IR, 13.07.2010: absorption lengths based on the transmission measurements during Run-10
      data absco_cf4  /
     >     1.e6,   1.e6,   1.e6,   1.e6,   1.e6,
     >     1.e6,   1.e6,   126774,   170870,   95854,
     >     151154,   170870,   140357,   63387,   37788,
     >     17545,   13986,   9752,   8415,   7661,
     >     6496,   5946,   5282,   4962,   4775,
     >     4446,   4119,   3786,   3411,   3185,
     >     3126,   3450,   6528,   6422,   5240,
     >     5622,   4735,   3301,   7211,   8415,
     >     7844,   12358,   12358,   12358,   12358/

* ---- Detection efficiencies (quantum efficiency for CsI--for others take eff=1)

      data effic_dummy /Nck * 1./

C-------------------------------------------------------------------------------------------
C    CsI QE in vacuum measured by IR. Optical transparencies of the drift cathode and GEM
C    are not taken into account. The quantum efficiency was calculated for the energy values
C    of Cerenkov photons given and fitted by a polynome of the 1st order,
C   y = P0 + P1*x, with the folowing constants: P0 = -1.08538e+02, P1= 1.75898e+01
C 13.07.2010: added collection efficiency vs wavelength measured by BNL group.

       data effic_csi /
     >     0.0,      0.0,      0.0,      0.0,     0.0,
     >     0.0,      0.0,      0.0042,  0.0162, 0.0282,
     >     0.0401,  0.0521,  0.0634,  0.0761, 0.0881,
     >     0.0980,  0.1103,  0.1206,  0.1313, 0.1437,
     >     0.1582,  0.1703,  0.1808,  0.1908, 0.2010,
     >     0.2112,  0.2217,  0.2314,  0.2541, 0.2730,
     >     0.2945,  0.3109,  0.3259,  0.3377, 0.3519,
     >     0.3762,  0.3902,  0.3977,  0.4174, 0.4373,
     >     0.4568,  0.4766,  0.4963,  0.5160, 0.5258 /

      ! make sure material definition is done only once.
      if(icall.eq.0) then
          write( *, * ) 'hbd_trk_media - initializing'
          icall = 1
      else
          return
      endif


C----------------------------------------------------------------------------------------
C Correction for the mesh transparency (88.5%) and GEM transparency (83%)
       do I100=1,45
          effic_csi(I100) = effic_csi(I100)*0.735
       enddo
*-----------------------------------------------------------------
* Start executable code


*CAA All TPC/HBD-specific materials and tracking media are numbered 15xx
*CAA Define mixtures and compounds.  These are new _materials_ which
*CAA will be used in the _tracking media_ definitions below.
*CAA Use material numbers 1520 and above, leaving room for possible new
*CAA gas mixtures to be defined.

*CAA Mylar to contain gas volumes
      Call gsmixt(1520, 'Mylar$', Amylar, Zmylar, 1.39,
     >     -3, Wmylar)
*CAA G10 board, used in Front-end Electronics
      Call gsmixt(1521, 'G10$', Ag10, Zg10, 1.7,
     >     5, Wg10)
*CAA Cesium Iodide, used for detection of Cerenkov photons in HBD
      Call gsmixt(1522, 'CsI$', Acsi, Zcsi, 4.96,
     >     -2, Wcsi )
*CAA Gas definitions for possible radiator/drift gases
      Call gsmixt(1523, 'CH4$', Ach4gas, Zch4gas, 0.000717,
     >     -2, Wch4gas)
*CAA Density of gas is kg/L at STP  (22.414 L/mole for ideal gas)
      Call gsmixt(1524, 'CF4$', Acf4gas, Zcf4gas, 0.003926,
     >     -2, Wcf4gas)

      IMATE = 1525
      A = 12.01
      Z = 6.
      DENS = 0.024
      RADL = 8170.
      ABSL = 99999.
      NWBUF = 1
      CALL GSMATE(IMATE,'HEXCELL$',A,Z,DENS,RADL,ABSL,UBUF,NWBUF)

* ................................................................

*CAA Tracking medium numbers in the TPC/HBD
      nmylar           = 1501
      nfee             = 1502
      ncsi             = 1503
      nfee_csi         = 1504
      nalbar           = 1505
      ngas             = 1506
      ngas_act         = 1507
      ngas_csi         = 1508
      ngas_act_csi     = 1509
      nair             = 1510
      nnitrogen        = 1511
      nhexcell         = 1512
      naluminium       = 1513
      ncopper          = 1514

*CAA Tracking media parameters:
*CAA Turn on the field, make it max 0.5 T (= 5.0 kG).  The actual values will be read
*CAA in from a field map.
      IFIELD = 1                ! Magnetic field
      FIELDM = 5.0              ! Max field (kG)
      TMAXFD = 1.               ! Max angular deviation due to field in one step (deg)
      STEMAX = 5.0              ! Max displacement due to mult scatt in one step (cm)
      DEEMAX = 0.2              ! Max fractional energy loss in one step
      EPSIL =  0.01             ! Boundary crossing precision (cm)
      STMIN =  0.01             ! Min step due to E loss, mult scatt, Cerenkov, or field effects (cm)

      CALL GSTMED( nmylar, '  Mylar$', 1520, 0, IFIELD,
     >     FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
      CALL GSTMED( nfee, '  G10-FEE$', 1521, 0, IFIELD,
     >     FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
      CALL GSTMED( ncsi, '  CsI$', 1522, 1, IFIELD,
     >     FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
      CALL GSTMED( nfee_csi, '  G10-FEE$', 1521, 0, IFIELD,
     >     FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
      CALL GSTMED( nalbar, ' Al bars$', 9, 0, IFIELD,                      ! Al=material 9 in GEANT
     >     FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
      CALL GSTMED( nair, ' Air$', 15, 0, IFIELD,                               ! Air=material 15 in GEANT
     >     FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
      CALL GSTMED( nnitrogen, ' Nitrogen$', 7, 0, IFIELD,             ! Nitrogen=material 7 in GEANT
     >     FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
      CALL GSTMED( nhexcell, ' Honeycomb$', 1525, 0, IFIELD,
     >     FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )

      CALL GSTMED( naluminium, '  Aluminium$', 9, 0, IFIELD,
     >     FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
      CALL GSTMED( ncopper, '  Copper$', 11, 0, IFIELD,
     >     FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )


      if (gas.eq.'CH4') then        ! CH4
         do ii = 1,Nck
             rindex_gas(ii) = rindex_ch4(ii)
            absco_gas (ii) = absco_ch4 (ii)
         enddo
         CALL GSTMED( ngas, '  CH4$', 1523, 0, IFIELD,
     >        FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
         CALL GSTMED( ngas_act, '  CH4_act$', 1523, 1, IFIELD,
     >        FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
         CALL GSTMED( ngas_csi, '  CH4$', 1523, 0, IFIELD,
     >        FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
         CALL GSTMED( ngas_act_csi, '  CH4_act$', 1523, 1, IFIELD,
     >        FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )

      else if (gas.eq.'CF4') then   ! CF4
         do ii = 1,Nck
            rindex_gas(ii) = rindex_cf4(ii)
            absco_gas (ii) = absco_cf4 (ii)
         enddo
         CALL GSTMED( ngas, '  CF4$', 1524, 0, IFIELD,
     >        FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
         CALL GSTMED( ngas_act, '  CF4_act$', 1524, 1, IFIELD,
     >        FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
         CALL GSTMED( ngas_csi, '  CF4$', 1524, 0, IFIELD,
     >        FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
         CALL GSTMED( ngas_act_csi, '  CF4_act$', 1524, 1, IFIELD,
     >        FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0  )
      else
         write(6,*) 'Error in hbd_trk_media.f!  Not a defined
     >        gas for TPC/HBD.'

      endif  ! Check on gas


*CAA Define Cerenkov radiators and absorbers for HBD
      CALL GSCKOV(ngas,Nck,ppckov,absco_gas,effic_dummy,
     >          	rindex_gas)
      CALL GSCKOV(ngas_act,Nck,ppckov,absco_gas,effic_dummy,
     >  	  rindex_gas)
      CALL GSCKOV(ngas_csi,Nck,ppckov,absco_gas,effic_dummy,
     >          	rindex_gas)
!      CALL GSCKOV(nfee_csi,Nck,ppckov,absco_gri,effic_dummy,
!     >          	rindex_dummy)
      CALL GSCKOV(ngas_act_csi,Nck,ppckov,absco_gas,effic_dummy,
     >  	  rindex_gas)
      CALL GSCKOV(ncsi,Nck,ppckov,absco_csi,effic_csi,
     >  	  rindex_dummy)
      CALL GSCKOV(nmylar,Nck,ppckov,absco_mylar,effic_dummy,
     >            rindex_dummy)

      end
