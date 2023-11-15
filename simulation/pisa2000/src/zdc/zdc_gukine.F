c $Id: zdc_gukine.F,v 1.1 2008/05/21 08:22:22 hpereira Exp $
******************************************************************
* RHIC-PHENIX ZDC detector simulation.
* GEANT tracking user routines.
* Author: Andras Ster, KFKI/Hungary
* Creation date: 20.April.1999
******************************************************************

      !****************************
      SUBROUTINE zdc_gukine(i_particle)

***   Routine called at begining of tracking

      IMPLICIT NONE

#include "event.inc"

      INTEGER i_particle

      COMMON /spectator/ spectator_particles(maxptl), I_spectator,
     +n_participant_part
      INTEGER spectator_particles, I_spectator, n_participant_part

      COMMON /zdchit/ ihit   ! this common block is not used ?
      INTEGER ihit

      ihit = 0

      I_spectator= -i_particle
      IF(spectator_particles(i_particle) .NE. 0) I_spectator=i_particle

      RETURN
      END

