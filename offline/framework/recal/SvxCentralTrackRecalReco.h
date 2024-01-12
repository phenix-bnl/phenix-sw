#ifndef __SVXCENTRALTRACKRECALRECO_H__
#define __SVXCENTRALTRACKRECALRECO_H__

#include "Recalibrator.h"
#include <fstream>

#include <SvxParameters.h>
#include <svxDetectorGeo.hh>
#include <PHTimeStamp.h>

class PHTimeStamp;
class PHSnglCentralTrack;
class PHCentralTrack;
class SvxHitMapEntry;
class SvxCentralTrackMapEntry;
class SvxCentralTrackRecalList;
class SvxCentralTrackRecal;

class SvxCentralTrackRecalReco: public Recalibrator
{
 public:
  SvxCentralTrackRecalReco(const std::string &name = "SvxCentralTrackRecalReco");
  virtual ~SvxCentralTrackRecalReco() {};

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode){return 0;}
  int isValidRun(const int runnumber) const;

  enum { SVXDEFAULTPATTERN=0 };
  enum { SVXDEFAULTBG=0 };

 protected:
  SvxCentralTrackRecalList *m_svxcntrecallist;
  SvxCentralTrackRecalList *m_svxcntrecallist_bg;
  
  PHTimeStamp m_timeStamp;
  svxDetectorGeo *m_geometry;
  float m_fieldScale;

  int m_layer[svxDetectorGeo::SVXNSUBLAYER];
  float m_rlow[SVXLAYERNUMBER];
  float m_rhigh[SVXLAYERNUMBER];
  float m_rsublayer[svxDetectorGeo::SVXNSUBLAYER];
  int m_max_sensor[SVXLAYERNUMBER];
  int m_startladder[SVXLAYERNUMBER][2];
  int m_endladder[SVXLAYERNUMBER][2];
  float m_sensorZwidth[SVXLAYERNUMBER];
  bool m_pattern;
  bool m_bg;

  std::vector<const SvxHitMapEntry*> m_vcluster[SVXLAYERNUMBER];

  void make_SvxCNTRecal(const SvxCentralTrackMapEntry *svxcnt,
			SvxCentralTrackRecal *svxcntrecal,
			float vx, float vy, float vz);
  void get_LadderSensor(float gx, float gy, float gz, int layer,
			int &ladder, int &sensor);
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
  
  PHCentralTrack *m_cnt;
  int m_ievt;
  
};

#endif
