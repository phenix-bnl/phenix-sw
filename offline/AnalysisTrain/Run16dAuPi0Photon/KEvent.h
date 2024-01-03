#ifndef __KEvent__
#define __KEvent__

#include <vector>
#include <algorithm>

class PHCentralTrack;
class emcClusterContainer;
class KCluster;
class mEmcGeometryModule;
class THmulf; 
class TH2F;

//class HotTwrList;
//class TowerHelper;
//class Warnmap;

// Here is a trick to get around the fact that rootcint is 
// too stoopid to understand the boost headers
//
#ifdef __CINT__
typedef KCluster* CLUSTER_PTR;
#else
#include "boost/shared_ptr.hpp"
typedef boost::shared_ptr<KCluster> CLUSTER_PTR;
#endif

#define MAX_NCLUSTER 1024

// The following unary function object can be used in an STL algorithm, 
// such as:
//   std::for_each( clusterVector.begin(), clusterVector.end(),  DeleteObject() );
//
// Note the since we now use boost::shared_ptr for the culsterVector,
// this is sort of obsolete.
//
struct DeleteObject {
  template <typename T> void operator() (const T* ptr) const { delete ptr; }
};

class KEvent {
public:

  KEvent();
  virtual ~KEvent();

  std::vector<CLUSTER_PTR> clusterVector;                // Cluster Array

  int      g_multiplicity;             // Number of clusters stored in the class as gamma

  void     clear();
  void     removeTrigger();

  int      getEvent( emcClusterContainer *emccont, PHCentralTrack* phCentralTrack_ptr,
		     float bbcT0, float bbcVz, mEmcGeometryModule *EmcGeo, 
		     THmulf *gamma1, THmulf *gammarap, THmulf *tower, THmulf *towertof,THmulf *towertof_3ns, ErtOut *ertout, int runn,
                     TNtuple *gnt, int is_bookNt, float inPtcut, int ERTb,
                     //int _status_array[2][4][48][96],
                     //float _low_limit_array[2][4][48][96],
                     //float _high_limit_array[2][4][48][96],
                     float bbcqn, float bbcqs, int nevent,TH1F *cluster_counter,
					 TH2F *bbcqs_ecoreMax, TH2F *bbcqs_ecoreTotal, TH2F *bbcqs_ecoreAverage, 
					 TH2F *bbcqn_ecoreMax, TH2F *bbcqn_ecoreTotal, TH2F *bbcqn_ecoreAverage , THmulf *cent_fw_corr);
//		     TNtuple *gnt, int is_bookNt); 
//		     int runn, TNtuple *gnt, int is_bookNt); 
                     //THmulf *gamma3, THmulf *tower, THmulf* charge1 );


  float  getEtByMease() const;             // Calc total Et by mease and return
  
  const KCluster* cluster_element(unsigned int n) const;

  void setCentralityBin(int inCentBin) { centralityBin = inCentBin; }
  void setTheta(float inTheta)         { theta = inTheta; }
  void setReacBin(int inReacBin)       { reacBin = inReacBin; }
//  void setWarnmap(Warnmap *inTwrVeto)  { TwrVeto = inTwrVeto; }

  float getTheta() const { return theta; }
  int  getCentralityBin() const { return centralityBin; }
  int  getGammaMultiplicity() const { return g_multiplicity; }
  int  getTrig() const { return ntrig; }
  void setTrig(int intrig) { ntrig = intrig; }


private:

  int  ntrig;
  int      centralityBin;
  float    theta;
  int      reacBin;
  int      trigger_photon;  // index of cluster that triggered event

//  HotTwrList *TwrVeto; Run7
//  TowerHelper *TwrHlp;
//  Warnmap *TwrVeto;
};

#endif


