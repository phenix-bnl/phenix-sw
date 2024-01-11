#ifndef __FKIN_H__
#define __FKIN_H__
/*! 
KIN structure keeps track of all track kinematical variables, and ancestry information. 
True_track is the unique index used to identify tracks in a given event from PISA file.
The parent true_track id is only valid if > 0. (positive)
If non-positive, the track is a primary track.
If negative the indicated number refer to one of the daughter of the current track.
A single true_track might appear multiple times in the KIN table, with different _negative_ true_track daughter ids.
*/

typedef struct {
  
  //! true track id, unique within event, (including multiple files and multiple subevents)
  int true_track;
  
  //! sub-event id
  int subevent;
  
  //! geant track id, unique within file and su-event
  int ntrack;
  
  //! total momentum
  float ptot;
  
  //! momentum theta angle (with respect to z axis)
  float pthet;
  
  //! momentum azimuthal angle (in xOy plane)
  float pphi;
  
  //! origin radial position
  float r_vertex;
  
  //! origin z position
  float z_vertex;
  
  //! origin theta angle (with respect to z = 0 and z axis)
  float th_vertx;
  
  //! origin azimuthal angle 
  float ph_vertx;
  
  //! parent track id
  int itparent;
  
  //! parent particle id
  int idparent;
  
  //! particle id
  int idpart;
  
  //! file id
  int nfile;
  
} FKIN_ST;
#endif /*__FKIN_H__*/
