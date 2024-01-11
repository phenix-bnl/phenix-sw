#ifndef __DIO_TRK_HH__
#define __DIO_TRK_HH__

/*
 * dio_trk.hh - sets up track number utility routine prototypes
 *
 * Original Author: Charles F. Maguire (Vanderbilt University)
 * Creation Date: June 2, 1997
 */

#ifdef __cplusplus
extern "C" {
#endif

  int dio_EtrackToTrueTrack (int *etrack, int *nfile, int *true_track);

  int dio_TrueTrackToEtrack (int *true_track, int *etrack, int *nfile);

  int dio_Etrackstack (int *etrack, int *nfile, int *error, float *px,
		       float *py, float *pz,  int *idpart);

  int dio_ptrkstack(int *true_track, int *nfile,int *error, 
		    float *ptot, float *ptheta, float *pphi,
		    float *r_vertex, float *z_vertex, 
		    float *theta_vertex, float *phi_vertex,
		    int *itparent, int *idparent, int *idpart);

  int dio_truetrack(int *ntrack, int *isubevent, int *nfile,
		    int *error);

  int dio_ptrkorigin(int *true_track, int *nfile, int *error,
		     float *ptot, float *ptheta, float *pphi,
		     float *r_vertex, float *z_vertex,
		     float *theta_vertex, float *phi_vertex,
		     int *itorigin, int *idorigin, int *idpart);
  /*
   *  NOTE: idpart is primary particle id (=idorigin), not the input
   *  particle id.  Future version will have input particle id
   *  returned in this location.
   */

#ifdef __cplusplus
}
#endif

#endif /* __DIO_TRK_HH__ */
