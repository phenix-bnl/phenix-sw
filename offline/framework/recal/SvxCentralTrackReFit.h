#ifndef __SVXCENTRALTRACKREFIT_H__
#define __SVXCENTRALTRACKREFIT_H__

//
// Note :
//  In order to activate this refit class, you have to set
//  "SVXFIT" flag through recoConsts at your macro.
//
//  ex)
//  recoConsts *rc = recoConsts::instance();
//  rc->set_IntFlag("SVXFIT",1)
//

#include "Recalibrator.h"
#include <fstream>

#include <SvxParameters.h>
#include <svxDetectorGeo.hh>
#include <SvxTracker.h>
#include <PHTimeStamp.h>

class PHTimeStamp;
class PHCentralTrack;
class SvxCentralTrackRecal;

class SvxCentralTrackReFit: public Recalibrator
{
 public:
  SvxCentralTrackReFit(const std::string &name = "SvxCentralTrackReFit");
  virtual ~SvxCentralTrackReFit() {};

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode){return 0;}
  int isValidRun(const int runnumber) const;

  enum { SVXNOFIT=0 };

 protected:
  PHTimeStamp m_timeStamp;
  svxDetectorGeo *m_geometry;
  PHCentralTrack *m_cnt;
  float m_fieldScale;

  float m_rsublayer[svxDetectorGeo::SVXNSUBLAYER];

  SvxTracker m_tracker;

  void ReFit_SvxCNT(SvxCentralTrackRecal *svxcntrecal, float vx, float vy, float vz);
  void calcDCAbyCircleProjection
    (float pt, float phi, float the,  // pt in xy-plane, phi, theta at inner most layer
     int charge,                      // charge of the track
     float hx, float hy, float hz,    // hit position at inner most layer
     float vx, float vy,              // primary vertex
     float* dx, float* dy, float* dz, // dca position
     float* d2dca);                    // return
  void CircleProjection
    (float pt, float phi0, float the0, // pt & phi, theta at the closest approach
     int charge,
     float cax, float cay, float caz,  // closest approach
     float r,                          // radius of projected cylinder
     float &xproj, float &yproj, float &zproj); // projected position
  
  int m_ievt;
  
};

#endif



