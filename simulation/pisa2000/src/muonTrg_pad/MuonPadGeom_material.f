* material defination for pad chamber for Muon trigger upgrade
*-- Author :    Wei Xie  09/25/2003

      SUBROUTINE MuonPadGeom_material

      IMPLICIT NONE
 
#include "gcflag.inc"
#include "gconst.inc"
#include "gugeom.inc"
#include "sublink.inc"
#include "fpilink.inc"
#include "fstore.inc"

      real A, Z, DENS, RADL, ABSL, UBUF
      real FIELDM, TMAXFD, DMAXMS, DEEMAX, EPSIL, STMIN
      integer NWBUF, IFIELD
      real AGAS(3), ZGAS(3), WGAS(3)
  
      integer S2_glass
      integer Argon_Ethane, MuPC1Gas, MuPC2Gas, MuPC3Gas
      common  /material_utrPC/S2_glass, MuPC1Gas, MuPC2Gas, MuPC3Gas

      S2_glass = 1000
      MuPC1Gas = 1001
      MuPC2Gas = 1002
      MuPC3Gas = 1003
      Argon_Ethane = 1001
      
C.. S2-glass/Epoxy - fiberglass material with properties close to G10
 
      A = 18.14
      Z = 9.065
      DENS = 1.7
      RADL = 19.4
      ABSL = 56.7
      NWBUF = 1
      CALL GSMATE(S2_glass,'G10$',A,Z,DENS,RADL,ABSL,UBUF,NWBUF)    

C.. Chamber gas (Argon-Ethane 50%-50%)
*   Atomic weights of Ar, C, and H:
      AGAS(1) = 39.95           
      AGAS(2) = 12.01           
      AGAS(3) =  1.01           

*   Mass numbers of Ar, C, and H:
      ZGAS(1) = 18.            
      ZGAS(2) = 6.             
      ZGAS(3) = 1.             

*   Proportions: 0.5 Ar + 0.5 C2H6
      WGAS(1) = 0.5            
      WGAS(2) = 0.125
      WGAS(3) = 0.375

      DENS = 0.0010394
      CALL GSMIXT(Argon_Ethane,'MuonPC GAS$',AGAS,ZGAS,DENS,3,WGAS)

C.. ----------------- DEFINE USER TRACKING MEDIA -------------------

      IFIELD = 1     ! magnetic field; tracking performed with GRKUTA;
      FIELDM = 20.0  ! max field value (in Kilogauss);
      TMAXFD = 1.0   ! maximum angle due to field in one step (in degrees);
      DMAXMS = 0.5   ! max disp. due to mult. scatt. in one step (in cm);
      DEEMAX = 0.2   ! max fractional energy loss in one step;
      EPSIL = 0.01   ! tracking precision (in cm);
      STMIN = 0.01   ! min step due to energy loss or mult. scatt. (in cm);
 
      CALL GSTMED(S2_glass,'MuonPC S2-GLASS $', S2_glass,  0,  IFIELD,
     +                 FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(MuPC1Gas,'MuonPC1 GAS$', Argon_Ethane, 
     *      1,  IFIELD, FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(MuPC2Gas,'MuonPC2 GAS$', Argon_Ethane, 
     *      1,  IFIELD, FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)
      CALL GSTMED(MuPC3Gas,'MuonPC3 GAS$', Argon_Ethane, 
     *      1,  IFIELD, FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0, 0)

      return
      end
