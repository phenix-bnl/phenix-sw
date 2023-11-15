C...  Position of the RLT RPCs Near Interaction Region
C...   -->>The positions come from the common block rltRPCpos
C...       which is read in from a file in rltGeom.f
C...
C...  Author : L. A. Linden Levy and M. C. McCain 16.02.2004
C...
C...  ============================================================
C...
C...  CHANGELOG:
C...  16.02.04 23:21 ALL: File roughed in.
C...  17.02.04 23:21 Mother volumes rtsX positioned in hall.
C...                   
C...  ============================================================

      SUBROUTINE RLTGeom_pos

      IMPLICIT NONE

#include "gcflag.inc"
#include "gconst.inc"
#include "gugeom.inc"
#include "sublink.inc"
#include "fpilink.inc"
#include "fstore.inc"

      real rltRPC1xcent, rltRPC1ycent, rltRPC1zcent,
     *     rltRPC2xcent, rltRPC2ycent, rltRPC2zcent,
     *     rltRPC3xcent, rltRPC3ycent, rltRPC3zcent

      common /rltRPCpos/rltRPC1xcent, rltRPC1ycent, rltRPC1zcent,
     *               rltRPC2xcent, rltRPC2ycent, rltRPC2zcent,
     *               rltRPC3xcent, rltRPC3ycent, rltRPC3zcent

C...  Names of all the RPC subvolume shells.

      character*4 rltRPC1shell/'rts1'/
      character*4 rltRPC2shell/'rts2'/
      character*4 rltRPC3shell/'rts3'/
      
C...  Now Position the RPCs in the HALL mother volume.


      CALL GSPOS(rltRPC1shell, 1, 'HALL', rltRPC1xcent, rltRPC1ycent,
     *           rltRPC1zcent, 0, 'ONLY')

      CALL GSPOS(rltRPC2shell, 1, 'HALL', rltRPC2xcent, rltRPC2ycent,
     *           rltRPC2zcent, 0, 'ONLY')

      CALL GSPOS(rltRPC3shell, 1, 'HALL', rltRPC3xcent, rltRPC3ycent,
     *           rltRPC3zcent, 0, 'ONLY')

      RETURN
      END
