#ifndef __MTECHOUGHTRACKMODULE_H__
#define __MTECHOUGHTRACKMODULE_H__


#include "TecCalibrationObject.hh"
#include "phool.h"
#include <vector>

class PHCompositeNode;
class TecTrackCluster;

/** TEC tracking using Hough transform.
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/tec/houghtracking.html}
*/
class mTecHoughTrackModule
{

public:
///
  mTecHoughTrackModule();
///
  virtual ~mTecHoughTrackModule();
///
  PHBoolean event(PHCompositeNode*);
///
  PHBoolean event(PHCompositeNode*, TecGeometryObject*, TecCalibrationObject*);
///
  int GetEntryExit(float slope, float intercept,
                 TecGeometryObject* TGO,                       
                 int index, int Debug,                      
                 float &xx0, float &yy0, float &xx1, float &yy1,
                 float &erxx0, float &eryy0, float &erxx1, float &eryy1);
///
  int DoClustering2(int *Array, std::vector<TecTrackCluster> &ClusterQueue,
                    int &maximum, int algorithm, int Debug);
///
  int GetClusters2(int *Array,
             std::vector<TecTrackCluster> &ClusterQueue, int maximum,
             std::vector<float> &slope, std::vector<float> &intercept,
             std::vector<float> &erslope, std::vector<float> &erint,
             std::vector<float> &chi2, int &ntrk, int index,
	     float midphi, int Debug);


///
  void set_Verbose(int verbose){Verbose=verbose;}
///
  void set_Write2File(int w2f){Write2File=w2f;}
///
  void set_RandSeed(long int rs){RandSeed=rs;}
///
  void set_Statistics(int stat){ Statistics1=stat; }
///
  void set_fillRawTrack(int frt){fillRawTrack=frt;}
///
  void set_whichplane(int wp){whichplane=wp;}
///
  void set_minbins(int mb){minbins=mb;}
///
  void set_minPlanes(int mp){minPlanes=mp;}
///
  void set_algorithm(int alg){algorithm=alg;}
///
  void set_INDEX(int ind){INDEX=ind;}
///
  void set_Rebin(int rebin){Rebin=rebin;}
///
  void set_angLim(float al){angLim=al;}
///
  void set_clPar(int i, float clpar){clPar[i]=clpar;}
///
  void set_Radius0(float r0){Radius0=r0;}
///
  void set_Dist0(float d0){Dist0=d0;}
///
  void set_m1(int m){m1=m;}
///
  void set_m2(int m){m2=m;}
///
  void set_y11(float y){y11=y;}
///
  void set_y12(float y){y12=y;}
///
  void set_y21(float y){y21=y;}
///
  void set_y22(float y){y22=y;}
///
  void set_fillVector(int fv){fillVector=fv;}
///
  void set_fillVectTrack(int fvt){fillVectTrack=fvt;}
///
  void set_Refit(int refit){Refit=refit;}
///
  void set_Beg1(float b) { Beg1 = b; }
///
  void set_Beg2(float b) { Beg2 = b; }
///
  float get_Beg1() { return Beg1; }
///
  float get_Beg2() { return Beg2; }
///
  void set_LowThreshold(float lf) {LowThreshold=lf;}
///
  void set_CopyTecOut(int i) {CopyTecOut=i;}
///
  void set_StripOrphanHits(int i) {StripOrphanHits=i;}
///
  void set_LastEventWithHits(int i);
///
  void set_occupancy_limit(unsigned int i) {occupancy_limit = i;}


private:
///
  long int RandSeed;
/// 
  int Statistics;
/// 
  int Statistics1;
/// 
  int Statistics2;
/// 
  int Statistics3;
///
  int fillRawTrack;
///
  int whichplane;
///
  int minbins;
///
  int algorithm;
///
  int INDEX;
///
  int Rebin;
///
  float angLim;
///
  float clPar[6];
///
  float Radius0;
///
  float Dist0;
///
  int m1;
///
  int m2;
///
  float y11;
///
  float y12;
///
  float y21;
///
  float y22;
///
  int fillVector;
///
  int fillVectTrack;
///
  int Refit;
///
  int Verbose;
///
  int Write2File;
///
  float LowThreshold;
///
  float Beg1; 
///
  float Beg2;
///
  int minPlanes;
///
  int CopyTecOut;
///
  int StripOrphanHits;
///
  int LastEventWithHits;
///  
  unsigned int occupancy_limit;
};

class TecTrackCluster {

public:

///
  TecTrackCluster() { }
///
  ~TecTrackCluster() { Clear(); } 
///
  void AddMaximum(int i) {Maxima.push_back(i);}
///
  void AddModule(int i) {Modules.push_back(i);}
///
  void Clear() { Maxima.clear(); Modules.clear(); }
///
  void Reset() { Clear(); }
///
  int getNMax() { return Maxima.size(); }
///
  int getNMod() { return Modules.size(); }
///
  int getMax(int i) { return Maxima[i]; }
///
  int getMod(int i) { return Modules[i]; }

protected:

///
  std::vector<int> Maxima;
///
  std::vector<int> Modules;

};

#endif /*__MTECHOUGHTRACKMODULE_H__*/
