#ifndef __SVXVERTEXRECAL__
#define __SVXVERTEXRECAL__


class SvxCentralTrackList;
class PHCompositeNode;
class VtxOut;
class SvxCentralTrackRecalList;
class VertexFinder;
class CylinderKalman;
class SvxClusterList;
class PHCentralTrack;

#include "SubsysReco.h"
#include <vector>

#include <SimpleHit3D.h>
#include <SimpleTrack3D.h>

#ifndef __CINT__
// hiding this stuff from the ROOT interpreter which hates templates
#include <sPHENIXTracker.h>
#include <VertexFinder.h> 
#endif // __CINT__

class SvxVertexRecal : public SubsysReco
{
public:
  SvxVertexRecal();
  ~SvxVertexRecal();
  
  int Init(PHCompositeNode *topNode){return 0;}
  int InitRun(PHCompositeNode *topNode){return 0;}
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode){return 0;};
  int getNodes(PHCompositeNode *topNode);
  
  VtxOut* vtxout;
  SvxCentralTrackRecalList* svxcntrecallist;
  SvxCentralTrackList* svxcentral;
  PHCentralTrack* central;
  SvxClusterList* clusters;
  
  std::vector<SimpleTrack3D> tracks;
  std::vector<SimpleHit3D> hits;
  
  CylinderKalman* kalman;
  
  VertexFinder vertexFinder;
  
  std::vector<float> vertex;
};



#endif
