      subroutine dcshieldRunX

c *********************************************************************

c Author: Charles F. Maguire (Vanderbilt)

c Date: May 14, 2002 (originally named dcshieldRun2)

c Purpose: Install what were renamed "photon shields" as of Run2
c          These shields intercept photons which would convert in the Dch frames
c          and then spray into the RICH.  This fixes a design oversight in the
c          construction of the Drift Chamber and RICH PMTs location. 
c          The name "DC Shield" was changed to "Photon Shield".

c Method: Use GEANT volume type polycone for four volumes at locations
c         provided by Tony Frawley resend mail of May 13, 2002.   The
c         locations are NE, NW, SE, and SW.  The shield names are
c         PSNE, PSNW, PSSE, and PSSW.  The (Z,R) coordinates on the
c         North side are (76.38, 188.0), (79.21, 198.0), (102.78, 198.0),
c         and (99.14, 188.0).  The South side has negative Z values.
c         The West angular coordinates are -36.75 to +59.25.  The
c         East angular coordinates, by implication, will be 120.75 to
c         216.75 degrees.  The coverage is 96 degrees azimuth in each arm.
c         The tracking material is 94% Pb, 6% Sb.

c Revision History:
c     Based on dcshield version originally written by K. Shigaki (1996)
c     which has the following Revision History

c     05/Aug/96 KS, added Al support
c     07/Jul/97 CFM, provide for installation into HALL or HEB2 mother
c                    volume depending on whether Helium bag is installed.
c                    Previous mother volume was INTR, but now the INTR
c                    volume includes only DC/PC1

c     06/Apr03 CFM   Run3 has only the West Arm shield
c                    Change the name to dcshieldRunX
c                    Check the RHICRUN number in order to decide
c                    whether to install the East Arm shield (Run2 version)
c     May 2010 HvH:  Switching mother volumes (the '97 mod) is silly, and 
c                    it causes a volume conflict with HEB2. Place the blocks
c                    in HALL, and wrap HEB2 around it.
c                    Also make just one volume PBSH (lead shield) and place
c                    it 4 times by rotations.
c *********************************************************************

      IMPLICIT NONE
 
#include "guphnx.inc"
#include "gconst.inc"
#include "gugeom.inc"
#include "sublink.inc"
#include "fpilink.inc"
#include "fstore.inc"

      integer irun /3/  ! distinguish Run3 or later when East Arm shield was removed

      character*4 v_i_name, v_m_name
      integer nmed, ivolu

      integer npar
      parameter (npar = 15)

      real pconNW_par(npar) /-36.75,  96.0, 4.0,    ! North West defs: start phi, delta-phi, nplanes
     +                       +76.38, 188.0, 188.0,  ! Starting Z plane
     +                       +79.21, 188.0, 198.0,  ! Second Z plane
     +                       +99.14, 188.0, 198.0,  ! Third Z plane
     +                      +102.78, 198.0, 198.0 / ! Fourth Z plane

c===============================================================================

      write(6,*) 'DCSHIELDRUNX <I>: execution starting.'

      if(RHICRUN.EQ.3)THEN
         write(6,*) '  RHIC Run = ', RHICRUN,
     +              ', using only West Arm shield'  
      endif

      if(RHICRUN.EQ.2 .OR. RHICRUN.GE.4)THEN
         write(6,*) '  RHIC Run = ', RHICRUN,
     +              ', using both East and West Arm shields'
         irun = 2
      endif

      if(RHICRUN.LT.2)THEN
         write(6,*) '  Unknown RHIC Run = ', RHICRUN,
     +              ', default to using only West Arm shield' 
      endif

      nmed = 9  ! using lead for now, will change to Pb + Sb later

c     Create the North West volume, and place it 2 or 4 times
      v_m_name = 'HALL'
      v_i_name = 'PBSH'
      call gsvolu(v_i_name, 'PCON', nmed, pconNW_par, npar, ivolu)
      call gsatt(v_i_name,'SEEN',1)
      CALL gsatt(v_i_name,'COLO',2)
                            !  position the NW shield
      call gspos(v_i_name, 1, v_m_name, 0., 0., 0., irotnull, 'ONLY')
                            ! position the SW shield
      irot = irot + 1       ! rotate 180 around x:
      call gsrotm(irot, 90., 0., 90., 90., 180., 0.)
      call gspos(v_i_name, 2, v_m_name, 0., 0., 0., irot, 'ONLY')

      if(irun.eq.2)then     ! true for run 2 or run >=4
                            ! position the SE shield
        irot = irot + 1     ! rotate 180 around y:
        call gsrotm(irot, 90., 180., 90., 90., 180., 0. )
        call gspos(v_i_name, 3, v_m_name, 0., 0., 0., irot, 'ONLY')
                            ! position the NE shield
        irot = irot + 1     ! rotate 157.5 about z:
        call gsrotm(irot, 90., 157.5, 90., 157.5+90., 0., 0.)
        call gspos(v_i_name, 4, v_m_name, 0., 0., 0., irot, 'ONLY')
      endif  ! check on using East Arm shield

c wrap it up

      write(6,*) 'DCSHIELDRUNX <I>: execution completed successfully.'

      return
      end
