C...  Material Definition file for RLT RPC's
C...  Authors: M. C. McCain and L. A. Linden Levy 16.02.2004
C...  What needs to be fixed:
C...  Gas Density and Maybe Glass material and maybe gas
      SUBROUTINE RLTGeom_material

      IMPLICIT NONE

#include "gcflag.inc"
#include "gconst.inc"
#include "gugeom.inc"
#include "sublink.inc"
#include "fpilink.inc"
#include "fstore.inc"

      real A, Z, DENS, RADL, ABSL, UBUF
      real FIELDM, TMAXFD, DMAXMS, DEEMAX, EPSIL, STMIN
      real AGAS(4), ZGAS(4), WGAS(4)
      integer NWBUF, IFIELD
      integer rltRPCglass, rltRPC1gas, rltRPC2gas, 
     *        rltRPC3gas, gasmix

      common /rltRPCmaterial/rltRPCglass, rltRPC1gas, 
     *                       rltRPC2gas, rltRPC3gas

      rltRPCglass = 4300
      gasMix = 4301
      rltRPC1gas = 4301
      rltRPC2gas = 4302
      rltRPC3gas = 4303

C... RPC Glass just plain old glass (need to come back and fix this)

      A = 18.14
      Z = 9.065
      DENS = 1.7
      RADL = 19.4
      ABSL = 56.7
      NWBUF = 1

      CALL GSMATE(rltRPCglass,'RLT RPC G10$',A,Z,DENS,RADL,
     *            ABSL,UBUF,NWBUF)    

C... RPC Gas (tetroFluoroEthane-isoButane-SF6 90%-5%-5%)
C... Atomic weights of C, H, F and S:
     
      AGAS(1) = 12.01           !Carbon
      AGAS(2) =  1.01           !Hydrogen
      AGAS(3) = 19.00           !Flourine
      AGAS(4) = 32.07           !Sulfur

C... Mass numbers of C, H, F and S:

      ZGAS(1) =  6              !Carbon
      ZGAS(2) =  1              !Hydrogen
      ZGAS(3) =  9              !Flourine
      ZGAS(4) = 16              !Sulfur

C... Proportions of 0.9 C2H2F4 + 0.05 C4H10 + 0.05 SF6

      WGAS(1) = 0.239           !Carbon
      WGAS(2) = 0.261           !Hydrogen
      WGAS(3) = 0.493           !Flourine
      WGAS(4) = 0.007           !Sulfur

      DENS=0.0010394            !What should the density be

      CALL GSMIXT(gasMix,'RLT RPC GAS$',AGAS,ZGAS,DENS,4,WGAS)

C...  Define User Tracking Media

      IFIELD = 0     ! magnetic field; tracking performed wi     FIELDM = 20.0  ! max field value (in Kilogauss);
      TMAXFD = 1.0   ! maximum angle due to field in one step (in degrees);
      DMAXMS = 0.5   ! max disp. due to mult. scatt. in one step (in cm);
      DEEMAX = 0.2   ! max fractional energy loss in one step;
      EPSIL = 0.01   ! tracking precision (in cm);
      STMIN = 0.01   ! min step due to energy loss or mult. scatt. (in cm);

      CALL GSTMED(rltRPC1gas,'RLT RPC1 GAS$', gasMix, 1, IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0) 
      CALL GSTMED(rltRPC2gas,'RLT RPC2 GAS$', gasMix, 1, IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0) 
      CALL GSTMED(rltRPC3gas,'RLT RPC3 GAS$', gasMix, 1, IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0) 
      CALL GSTMED(rltRPCglass,'RLT RPC GLASS$', rltRPCglass, 0, IFIELD,
     *            FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)

      RETURN
      END
      
