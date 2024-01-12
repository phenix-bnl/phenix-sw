#ifndef CHARGEDVETORECO_H__
#define CHARGEDVETORECO_H__

#include <SubsysReco.h>

class PHCompositeNode;
class PHCentralTrack;
class PadCluster;
class VtxOut;
class emcClusterContainer;

//  Hello Photon Fans:
//    This is a simple Reco Module whose sole purpose is to search for
//  possible charged particle vetoes for the EMC Clusters and update the
//  clusters with this information.
//
//                                                TKH  SM
//                                                8-4-2003

class ChargedvetoReco: public SubsysReco
{
 public:
 ChargedvetoReco( const std::string &name ="CHARGEDVETO"):SubsysReco(name) {}
  virtual ~ChargedvetoReco() {}
  int process_event(PHCompositeNode *topNode);

 protected:
};

#endif /* __CHARGEDVETORECO_H__ */
