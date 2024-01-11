#ifndef __MMPCEXSINGLETRACKPI0FINDER_H__
#define __MMPCEXSINGLETRACKPI0FINDER_H__


class TMpcExHitContainer;
class TMpcExShower;
class TMpcExSingleTrackPi0;
class TSpectrum;
class Exogram;
class mMpcExMiniClusters;

#include "mMpcExMiniClusters.h"

class mMpcExSingleTrackPi0Finder {

 public:
  enum Method{
    MINICLUSTER = 0,
    TSPECTRUM
  };

  mMpcExSingleTrackPi0Finder();

  virtual ~mMpcExSingleTrackPi0Finder();

  void SetSmooth(int smooth) { _cutSmooth = smooth; }

  void SetSigma(float sigma) { _cutSigma = sigma; }

  void SetHeight(int height) { _cutHeight = height; }

  void SetMethod(Method md) {_method = md;}

  //parameters for MiniClusters Construction
  
  //threshold for RMS, default is 2.2(2.2*RMS) 
  void SetTHRMS(double val) {_ex_mini_clusters->SetThresholdRMS(val);}
  //threshold for the Asymmetry between 
  //RMS_X and RMS_Y
  //default is 0.5 (which means max(rms_x/rms_y)<=3)
  void SetTHRMSAsy(double val) {_ex_mini_clusters->SetThresholdRMSAsy(val);}

  //threshold for distance between the 
  //peak position and E weighted mean position
  //default is 1 (1 rms of cluster)
  void SetTHdPKMeanDST(double val) {_ex_mini_clusters->SetThresholdPKMeanDST(val);}
  //maximum iterate number of single cluster
  //default is 100
  void SetMaxIterate(int n) {_ex_mini_clusters->SetMaxIterate(n);}

  
  //cuts for square (2mm x 2mm box) before construct
  //minicluster 
  //default is false;
  void SetSqCuts(bool val) {_ex_mini_clusters->SetSqCuts(val);}

  void SetMiniClusterFilter(MiniClusterFilter* filter) {_ex_mini_clusters->SetFilter(filter);}

  unsigned int GetNMiniCluster(){return _ex_mini_clusters->GetNMiniCluster();}
  
  //miniclusters after the filter
  std::vector<TMpcExMiniCluster*> GoodMiniClusters(){
    return _ex_mini_clusters->GoodMiniClusters();
  }

  TMpcExMiniCluster* GetMiniCluster(unsigned int i){return _ex_mini_clusters->GetMiniCluster(i);}

  TMpcExSingleTrackPi0* ReconstructSingleTrackPi0(TMpcExShower* shower, TMpcExHitContainer* hits, float mindist = -9999.0);

  TMpcExSingleTrackPi0* TSpecPi0Reco(TMpcExShower* shower,TMpcExHitContainer* hits);

  TMpcExSingleTrackPi0* MiniClusterPi0Reco(TMpcExShower* shower,TMpcExHitContainer* hits, float mindist = -9999.0);

 private:

  
  TSpectrum* _peakFinder;

  //parameters for TSpectrum
  int _cutSmooth;

  float _cutSigma;

  float _cutHeight;

  mMpcExMiniClusters* _ex_mini_clusters;
  
  //method for single track pi0 reconstruction
  Method _method;

  Exogram* _exo;

};

#endif /* __MMPCEXSINGLETRACKPI0FINDER_H__ */
