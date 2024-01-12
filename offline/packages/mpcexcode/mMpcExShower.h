#ifndef __MMPCEXSHOWER_HH__
#define __MMPCEXSHOWER_HH__

#include <memory>
#include <vector>
#include <map>
#include <TLorentzVector.h>
#ifndef __CINT__
#include <SubsysReco.h>
#endif

#include "MpcExConstants.h"
#include <iostream>

class SubsysReco;
class PHCompositeNode;
class MpcMap;
class mpcClusterContainer;
class mpcTowerContainer;
class mpcRawContainer;
class TMpcExHitContainer;
class TMpcExShowerContainer; 
class TMpcExShower; 
class MpcExMapper; 
class TMpcExHit; 
class MpcExCalibrateShowerEnergy; 

// Shower algorithm parameters
#define MAX_ITERATIONS 100
#define MAX_SUBSHOWER_ITERATIONS MAX_ITERATIONS
#define HOUGH_CONVERGE 0.00025  

// MPC-EX shower cone size (cm) 
#define SHOWER_BACK_RADIUS 1.60 
#define SHOWER_FRONT_RADIUS 0.80
#define SHOWER_RADIUS SHOWER_BACK_RADIUS

class mMpcExShower: public SubsysReco
{
 public:

  mMpcExShower( const char* name = "MMPCEXSHOWER" );
  virtual int Init(PHCompositeNode*);
  virtual int InitRun(PHCompositeNode*);
  virtual int process_event(PHCompositeNode*);
  virtual ~mMpcExShower();
  virtual int End(PHCompositeNode *topNode);

  void EnableSTReco() {DoSTReco = 1; std::cout << "mMpcExShower: single track reconstruction enabled" << std::endl;}
  void DisableSTReco() {DoSTReco = 0; std::cout << "mMpcExShower: single track reconstruction disabled" << std::endl;}

  void EnableSouthReco() {DisableSouth = 0; std::cout << "mMpcExShower: south arm reconstruction enabled" << std::endl;}
  void DisableSouthReco() {DisableSouth = 1; std::cout << "mMpcExShower: south arm reconstruction disabled" << std::endl;}

  void EnableNorthReco() {DisableNorth = 0; std::cout << "mMpcExShower: north arm reconstruction enabled" << std::endl;}
  void DisableNorthReco() {DisableNorth = 1; std::cout << "mMpcExShower: north arm reconstruction disabled" << std::endl;}

  void SetSecondStageExpansionParameters(double expansionFactor=1.3, double fractionalEnergyLimit=0.02, double expansionFactorLimit=4.0) {
    _EXPANSION_START = expansionFactor;
    _FRACTIONAL_ENERGY_LIMIT = fractionalEnergyLimit;
    _EXP_FACTOR_LIMIT = expansionFactorLimit;
  }

 private:

  void set_interface_ptrs(PHCompositeNode* top_node);
  bool FindShowers(PHCompositeNode* top_node); 
  void ClusterDeadAreas(); 
  void SingleTrackReco(); 
  void MergeShowers(TMpcExShower *main, TMpcExShower *splinter);
  void CalculateShowerProperties(TMpcExShower *shower_v1);

  void getMPC_TwrSum(int arm, double shx, double shy, int &iMPCTwr_peak, double MPCE[5][5], 
		     double MPCTOF[5][5], double MPCHS[5][5][2], double MPCQUAL[5][5],
		     int &N_3x3, int &N_5x5, int &fiducial, int &pkix, int &pkiy ); 
  void MatchMPC(int arm, double fMPCEXHoughX, double fMPCEXHoughY, float &fMPC_dhough, 
		float &distx, float &disty, float &clustE, int &clustNum);

  double CombinedHitEnergy(TMpcExHit* hit_ptr);

  TMpcExHitContainer* _hit_map;
  TMpcExShowerContainer* _shower_map; 
  mpcClusterContainer *_mpc_cluster_container;
  mpcTowerContainer *_mpc_tower_container;
  mpcRawContainer *_mpcraw2_container;
  MpcMap *_mpc_map;
  MpcExMapper *_mapper; 

  MpcExCalibrateShowerEnergy* _CalEnergy; 

  int fLayerValid[MpcExConstants::NLAYERS];

  /** Parameters for second stage expansion */
  double _EXPANSION_START;
  double _FRACTIONAL_ENERGY_LIMIT;
  double _EXP_FACTOR_LIMIT;

  protected:

  double _vertex;
  double _t0; 

  int DoSTReco; 
  int DisableSouth; 
  int DisableNorth; 
 
};

#endif /* __MMPCEXSHOWER_H__ */ 
