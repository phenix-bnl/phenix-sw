#ifndef __TMPCEXSINGLETRACKPI0_H__
#define __TMPCEXSINGLETRACKPI0_H__

#include <cmath>
#include <iostream>
#include <vector>
#include <phool.h>
#include <TMpcExShower.h>
#include "TMpcExMiniCluster.h"

class mpcTowerContainer; 
class mpcRawContainer; 

struct TMpcExPeakInfo {
  TMpcExPeakInfo() : position(-9999.), height(-9999.) {}
  TMpcExPeakInfo(const TMpcExPeakInfo& peak) {
    position = peak.position;
    height = peak.height;
  }
  float position;
  float height;
};

#ifdef __CINT__
typedef TMpcExPeakInfo* TMpcExPeakInfo_t;
#else
#include <boost/smart_ptr.hpp>
typedef boost::shared_ptr<TMpcExPeakInfo> TMpcExPeakInfo_t;
#endif

class TMpcExSingleTrackPi0 : public TMpcExShower {

 public:
  TMpcExSingleTrackPi0(TMpcExShower *shower) : TMpcExShower(shower) {
    for(int dim=0; dim<2; dim++){
      _peakPositions[dim] = std::vector<TMpcExPeakInfo_t>();
    }
    
    CombinedMPCEnergy = -9999.0; 

    for(int i=0; i<2; i++){
      _STmpcCentTwr[i] = -9999.0; 
      _STNmpc3x3[i] = -9999.0; 
      _STNmpc5x5[i] = -9999.0; 
      _STmpc_pkix[i] = -9999.0; 
      _STmpc_pkiy[i] = -9999.0; 
      for(int j=0; j<5; j++){
	for(int k=0; k<5; k++){
	  _STmpcTwrE[i][j][k] = 0.0; 
	  _STmpcTwrTOF[i][j][k] = -9999.0; 
	  _STmpcTwrHS[i][j][k][0] = -9999.0; 
	  _STmpcTwrHS[i][j][k][1] = -9999.0; 
	  _STmpcQual[i][j][k] = -9999.0; 
	}
      }
    }

  }

  TMpcExSingleTrackPi0* Clone() {
    TMpcExSingleTrackPi0* clone = new TMpcExSingleTrackPi0(this);
    for(int dim=0; dim<2; dim++){
      unsigned int npeaks = GetNpeaks(dim);
      for(unsigned int peak=0; peak<npeaks; peak++){
	clone->AddPeak(dim,GetPeakPosition(dim,peak),GetPeakHeight(dim,peak));
      }
    }

    unsigned int nmc = GetNMiniClusters();
    for(unsigned int mc=0; mc<nmc; mc++){
      clone->AddMiniCluster(GetMiniCluster(mc)->Clone());
    }

    return clone;
  }

  virtual ~TMpcExSingleTrackPi0() {
    for(unsigned int i=0;i<_mini_clusters_list.size();i++){
      if(_mini_clusters_list[i]) delete _mini_clusters_list[i];
      _mini_clusters_list[i] = NULL;
    }
  }

  void AddPeak(int dim, double position, double height) {
    if(dim<0 || dim>1){
      std::cerr<<PHWHERE<<": Dimension should be 0 or 1 -- Doing Nothing"<<std::endl;
	return;
    }
    TMpcExPeakInfo *peak = new TMpcExPeakInfo();
    peak->position = position;
    peak->height = height;
    _peakPositions[dim].push_back(TMpcExPeakInfo_t(peak));
  }

  unsigned int GetNpeaks(int dim) const {
    if(dim<0 || dim>1){
      std::cerr<<PHWHERE<<": Dimension should be 0 or 1 -- Returning garbage"<<std::endl;
	return 9999.;
    }
    return _peakPositions[dim].size();
  }

  float GetPeakPosition(int dim, unsigned int peak) const {
    if(dim<0 || dim>1){
      std::cerr<<PHWHERE<<": Dimension should be 0 or 1 -- Returning garbage"<<std::endl;
	return -9999.;
    }
    unsigned int npeaks = GetNpeaks(dim);
    if(peak>=npeaks){
      std::cerr<<PHWHERE<<": Attempting to grab more than "<<npeaks<<" peaks -- Returning garbage"<<std::endl;
	return -9999.;
    }
    return _peakPositions[dim][peak]->position;
  }

  float GetPeakHeight(int dim, unsigned int peak) const {
    if(dim<0 || dim>1){
      std::cerr<<PHWHERE<<": Dimension should be 0 or 1 -- Returning garbage"<<std::endl;
	return -9999.;
    }
    unsigned int npeaks = GetNpeaks(dim);
    if(peak>=npeaks){
      std::cerr<<PHWHERE<<": Attempting to grab more than "<<npeaks<<" peaks -- Returning garbage"<<std::endl;
	return -9999.;
    }
    return _peakPositions[dim][peak]->height;
  }

  enum Pattern {
    kX1Y1=0,
    kX2Y1,
    kX1Y2,
    kX2Y2
  };

  Pattern GetPeakPattern() const {
    unsigned int npeaksx = GetNpeaks(0);
    unsigned int npeaksy = GetNpeaks(1);
    if(npeaksx==1 && npeaksy>=2)
      return kX1Y2;
    if(npeaksy==1 && npeaksx>=2)
      return kX2Y1;
    if(npeaksx>=2 && npeaksy>=2)
      return kX2Y2;
    return kX1Y1;
  }

  float GetPeakDistance() const {
    Pattern patID = GetPeakPattern();
    if(patID == kX2Y1) {
      return std::abs(_peakPositions[0][0]->position - _peakPositions[0][1]->position);
    }
    if(patID == kX1Y2){
      return std::abs(_peakPositions[1][0]->position - _peakPositions[1][1]->position);
    }
    if(patID == kX2Y2)
      {
	float dx = _peakPositions[0][0]->position - _peakPositions[0][1]->position;
	float dy = _peakPositions[1][0]->position - _peakPositions[1][1]->position;
	return std::sqrt(dx*dx+dy*dy);
      }
    std::cerr<<PHWHERE<<": "<<patID<<" not a useful pattern -- returning garbage"<<std::endl;
    return -9999.;
  }

  float GetOpeningAngle(float zvtx) const {
    float dpeaks = GetPeakDistance();
    if(dpeaks<0){
      std::cerr<<PHWHERE<<": Invalid peak distance -- returning garbage"<<std::endl;
      return -9999.;
    }
    int arm = get_arm();
    int firstLayer = get_first_layer();

    float dz = std::abs(MpcExConstants::layerZ[arm][firstLayer] - zvtx);

    return 2*std::atan2(0.5*dpeaks,dz);
  }

  float GetMass(float zvtx) const {
    int arm = get_arm();
    int firstLayer = get_first_layer();
    float showerE = get_roughTotE();
    float dpeaks = GetPeakDistance();
    if(dpeaks<0){
      std::cerr<<PHWHERE<<": Invalid peak distance -- returning garbage"<<std::endl;
      return -9999.;
    }

    float dz = std::abs(MpcExConstants::layerZ[arm][firstLayer] - zvtx);

    //we could calculate the std::cos(GetOpeningAngle(zvtx))
    //but this is cos(2*atan(dr/(2dz))) = (1-(dr/(2dz))^2)/(1+(dr/(2dz))^2)
    double xxx = 0.5*dpeaks/dz;
    double cosAngle = (1-xxx*xxx)/(1+xxx*xxx);
    //assumes an asymmetry of alpha = 0.5 so we divide the energy equally between the photons
    return std::sqrt(0.5*showerE*showerE*(1-cosAngle));
  }

  //minicluster staff
  unsigned int GetNMiniClusters() const { return _mini_clusters_list.size(); }

  void AddMiniCluster(TMpcExMiniCluster* mclus){if(mclus) _mini_clusters_list.push_back(mclus);}
  unsigned int GetNMiniCluster() {return _mini_clusters_list.size();}
  TMpcExMiniCluster* GetMiniCluster(unsigned int i){
    if(i<_mini_clusters_list.size()) return _mini_clusters_list[i];
    else return NULL;
  }

  TMpcExMiniCluster* GetMiniCluster(unsigned int which) const {
    if(which>=_mini_clusters_list.size()){
      std::cout<<PHWHERE<<" You requested minicluster #"<<which<<" out of "<<_mini_clusters_list.size()<<". Returning garbage."<<std::endl;
      return NULL;
    }
    return _mini_clusters_list[which];
  }

  // Get MPC energy based on cluster locations
  void RecalculateShowerEnergy(mpcTowerContainer *_mpc_tower_container, mpcRawContainer *_mpcraw2_container, bool mixed = false); 

  double get_CombinedMPCEnergy() const { return CombinedMPCEnergy; }; 

 private:

  void getMPCTwrInfo(mpcTowerContainer *_mpc_tower_container, mpcRawContainer *_mpcraw2_container, 
		     int arm, double shx, double shy, int &iMPCTwr_peak, double MPCE[5][5], 
		     double MPCTOF[5][5], double MPCHS[5][5][2], double MPCQUAL[5][5],
		     int &N_3x3, int &N_5x5, int &fiducial, int &pkix, int &pkiy ); 


  void getMPCTwrInfoMixed(int arm, double shx, double shy, int &iMPCTwr_peak, double MPCE[5][5], 
			  double MPCTOF[5][5], double MPCHS[5][5][2], double MPCQUAL[5][5],
			  int &N_3x3, int &N_5x5, int &fiducial, int &pkix, int &pkiy ); 

  std::vector<TMpcExPeakInfo_t> _peakPositions[2];
  std::vector<TMpcExMiniCluster*> _mini_clusters_list;

  // Data on the energy behind the cluster locations in the MPC
  // (only the first two)

  int _STmpcCentTwr[2]; 
  int _STNmpc3x3[2]; 
  int _STNmpc5x5[2]; 
  double _STmpcTwrE[2][5][5]; 
  double _STmpcTwrTOF[2][5][5]; 
  double _STmpcTwrHS[2][5][5][2]; 
  double _STmpcQual[2][5][5]; 
  int _STmpc_pkix[2]; 
  int _STmpc_pkiy[2]; 

  double CombinedMPCEnergy; 

};

#endif /* __TMPCEXSINGLETRACKPI0_H__ */
