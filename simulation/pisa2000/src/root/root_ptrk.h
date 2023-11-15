#ifndef __ROOT_PTRK_H__
#define __ROOT_PTRK_H__




#include "KinPISAHit.h"
#include "PriPISAHit.h"

/*
*  
* root_ptrk.h - sets up track number utility routine prototypes
*
* Original Author: Charles F. Maguire (Vanderbilt University)
* Creation Date: June 2, 1997  (was dio_trk.hh in STAF)
*
* Revision History
*   Name                 Date         Comments
* Charles F. Maguire   July 22, 1999  Converted to ROOT version
*
*/

//! get PriPISAHit index matching given true track
extern "C" int dio_TrueTrackToEtrack (int *true_track, int *etrack, int *nfile);

//! get Primary particle information matching given primary index
extern "C" int dio_Etrackstack (int *etrack, int *nfile, int *error, float *px,
				float *py, float *pz,  int *idpart);

//! get kinematic information for track identified by true_track (it correspond to the track momentum, energy, position at track starting point)
extern "C" int dio_ptrkstack(
  int *true_track, int *nfile,int *error, 
  float *ptot, float *ptheta, float *pphi,
  float *r_vertex, float *z_vertex, 
  float *theta_vertex, float *phi_vertex,
  int *itparent, int *idparent, int *idpart);

//! get kinematic information of primary particle corresponding track identified by true_track 
/*! it correspond to the track momentum, energy, position of the particle oldest parent */
extern "C" int dio_ptrkorigin(
  int *true_track, int *nfile, int *error,
  float *ptot, float *ptheta, float *pphi,
  float *r_vertex, float *z_vertex,
  float *theta_vertex, float *phi_vertex,
  int *itorigin, int *idorigin, int *idpart);


#endif /* __ROOT_PTRK_H__ */



