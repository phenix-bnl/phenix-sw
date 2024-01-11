//+++++++++++++++++++++++++++++++++++++++++++++++++++++++
//$Id: HbdMinPadClusterizer.h,v 1.5 2012/06/27 16:01:39 makek Exp $
//$Log: HbdMinPadClusterizer.h,v $
//Revision 1.5  2012/06/27 16:01:39  makek
//Fixed a memory leak.
//
//Revision 1.4  2012/03/19 14:44:57  makek
//Added optional single/double rejection, using flag HBD_DOUBLE_REJECTION=1 (default is 0).
//Also some fixes and updates.
//
//Revision 1.3  2011/12/06 05:10:09  phnxbld
//remove unused SyncObject include, general cleanup of include files
//
//Revision 1.2  2011/08/02 12:26:27  makek
//Removed the explicit use of McEvalSingleList_v1 from the hbd package.
//
//Revision 1.1  2011/07/31 07:24:13  makek
//Introducing HbdMinPadClusterizer.
//
//Revision 1.5  2010/12/14 07:09:28  yosuke
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++
#ifndef __HBDMINPADCLUSTERIZER__
#define __HBDMINPADCLUSTERIZER__

#include "hbdDetectorGeo.hh"
#include "YWParticle.h"

#include <SubsysReco.h>
#include <string>

class HbdBlobList;
class HbdCellList;
class HbdPreClusterMaker;
class PHCompositeNode;
class YWCutter;

class HbdMinPadClusterizer: public SubsysReco
{
 public:
  HbdMinPadClusterizer(const char *NodeName = "hbdclusterizer", const char *CNTName = "EWGCentralTrack");
 
  virtual ~HbdMinPadClusterizer();
    
  int Init         (PHCompositeNode *topNode);
  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);
  void Verbosity (int verbosity){verbose = verbosity;}

 protected:
  //
  // DataNodes for the HBD ...
  //
  bool verbose;

  HbdCellList *CellList;
  HbdBlobList *BlobList;

  hbdDetectorGeo hbdgeo;
  HbdPreClusterMaker *premaker;

  int mc_flag;
  int clst_flag;
  int sd_flag;

  //
  // Output node name;
  //
  std::string d_NodeName;   // node for real
  std::string d_CNTName;    // node name for CNT or EWG or cEWG
 
  YWParticle electron, selectron, electron2;
  YWCutter *cutter, *cutter2;
  short rejection;
  float bbcq;
};

struct Pad{
  int padkey;
  float charge;
  float distance;
};

struct Triplet{
  short padkey[3];
  float charge;
  float distance;
  float position[3];
  float padcharge[3];
};

bool sort_by_distance(Pad const& first, Pad const& second);
bool sort_Triplet_by_distance(Triplet const& first, Triplet const& second);

#endif
