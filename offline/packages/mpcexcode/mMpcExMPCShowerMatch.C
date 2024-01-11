#include <mMpcExMPCShowerMatch.h>

#include <TMpcExShowerContainer.h>
#include <TMpcExShower.h>

#include <TMpcExHitContainer.h>
#include <TMpcExHit.h>

//constants
#include "mMpcExShower.h"
#include "MpcExConstants.h"

#include "mpcClusterContainer.h"
#include "mpcClusterContent.h"
#include "mpcTowerContainer.h"
#include "mpcTowerContent.h"
#include "MpcMap.h"

#include "PHGlobal.h"
#include "BbcOut.h"

#include "PHIODataNode.h"
#include "getClass.h"

#include "Fun4AllReturnCodes.h"

// STL/BOOST
#include <iostream>
#include <vector>
#include <cstdlib>
#include <algorithm>

using namespace std;
using namespace findNode;

typedef PHIODataNode<PHObject> PHObjectNode_t; 

//_______________________________________________________
mMpcExMPCShowerMatch::mMpcExMPCShowerMatch(const char* name):
  SubsysReco( name )
{
  verbosity = false;
  CleanShowerList = true;

  //from mMpcExShower.h
  Econv = 1.0e6;
  ShowerR = 3.0;

  EMax_high = (255. - 20.)*(147.6/19.);
  hlslope = 4.5;
  //  EMax_high = (255. - 20.)*(147.6/19.)*4.5;

}

//_______________________________________________________
// destructor
int mMpcExMPCShowerMatch::End(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

//_______________________________________________________
// destructor
mMpcExMPCShowerMatch::~mMpcExMPCShowerMatch()
{
  
}

//_______________________________________________________
// initialization
int mMpcExMPCShowerMatch::Init(PHCompositeNode* top_node)
{
  return EVENT_OK; 
}

int mMpcExMPCShowerMatch::InitRun(PHCompositeNode* top_node)
{  

  return EVENT_OK;
}

//_______________________________________________________
// Event method.
int mMpcExMPCShowerMatch::process_event(PHCompositeNode* top_node)
{  
  if (verbosity) cout << " mMpcExMPCShowerMatch::process_event " << endl;
  set_interface_ptrs(top_node);

  //get shower container
  TMpcExShowerContainer *fShowers = getClass<TMpcExShowerContainer>(top_node,"TMpcExShowerContainer");
  if (!fShowers)
    {
      cout << PHWHERE << " No TMpcExShowerContainer exiting" << endl;
      exit(1);
    }

  unsigned int NShowers = fShowers->size();
  for (unsigned int i=0; i < NShowers; i++) {    
    TMpcExShower *AShower = fShowers->getShower(i);
   
    CalcShowerHitInfo(AShower); //calc hit information
    MatchMPC_HoughSpace(AShower);  //associate shower to MPC cluster
    MatchMPCTower_HoughSpace(AShower);  //associate shower to MPC tower
  }    
  FindMPCclosest(fShowers);

  if (CleanShowerList)
    {
      //find showers to merge
      vector<PairIndices> ToMerge = CleanShowers(fShowers);

      //loop over vector of pairs to merge
      vector<unsigned int>ToErase;
      for (unsigned int i=0; i < ToMerge.size(); i++) {
	unsigned int IndexA = ToMerge[i].ClusterA; //the main ie the closest
	unsigned int IndexB = ToMerge[i].ClusterB; //the splinter ie not the closest

	TMpcExShower *ShowerA = fShowers->getShower(IndexA);
	TMpcExShower *ShowerB = fShowers->getShower(IndexB);
	MergeShowers(ShowerA, ShowerB);

	//recalculate mpc match and other hit info for combined shower
	CalcShowerPosition(ShowerA); //calc hit information
	CalcShowerHitInfo(ShowerA);
	MatchMPC_HoughSpace(ShowerA);	
	MatchMPCTower_HoughSpace(ShowerA);	

	ToErase.push_back(IndexB);
      }

      //reverse sort -- so indices won't changes as you remove them
      sort(ToErase.begin(),ToErase.end(),greater<unsigned int>());
      for (unsigned int i=0; i < ToErase.size(); i++) {
	//erase the splinter
	fShowers->removeShower( ToErase[i] );
      }

      //reassign closest
      FindMPCclosest(fShowers);
    }

  //removes nonclosest showers, so splinters disappear
  if (CleanShowerList) fShowers->Clean();

  return EVENT_OK;
}

void mMpcExMPCShowerMatch::set_interface_ptrs(PHCompositeNode* top_node)
{
  if (verbosity) cout << " mMpcExMPCShowerMatch::set_interface_ptrs " << endl;
  _fmpcClusters = getClass<mpcClusterContainer>(top_node,"mpcClusterContainer");
  if (!_fmpcClusters)
    {
      cout << PHWHERE << " No mpcClusterContainer exiting" << endl;
      exit(1);
    }

  _fmpcTowers = getClass<mpcTowerContainer>(top_node,"mpcTowerContainer");
  if (!_fmpcTowers)
    {
      cout << PHWHERE << " No mpcClusterContainer exiting" << endl;
      exit(1);
    }

  _fmpc_map = MpcMap::instance();
  if (!_fmpc_map) 
    {
      cout << PHWHERE << " No MpcMap exiting" << endl;
      exit(1);
    }

  _fHits = getClass<TMpcExHitContainer>(top_node,"TMpcExHitContainer");
  if (!_fmpcClusters)
    {
      cout << PHWHERE << " No TMpcExHitContainer exiting" << endl;
      exit(1);
    }

  _fGlobal = getClass<PHGlobal>(top_node,"PHGlobal");
  _fBbcout = getClass<BbcOut>(top_node,"BbcOut");
//  if (!phglobal && !bbcout)
//    {
//      cout << PHWHERE << " no phglobal or bbcout!  no sense continuing" << endl;
//      exit(1);
//    }

  return;
}

double mMpcExMPCShowerMatch::get_vertex()
{
  double vertex = 0.0;
  if (!_fBbcout && !_fGlobal) 
    vertex = 0.0;
  else
    vertex = (_fGlobal==0) ? _fGlobal->getBbcZVertex() : _fBbcout->get_VertexPoint();
  return vertex; 
}

//almost verbatim from mMpcExShowers
vector<mMpcExMPCShowerMatch::PairIndices> mMpcExMPCShowerMatch::CleanShowers(TMpcExShowerContainer *mpcexShwrs)
{
  if (verbosity) cout << " mMpcExMPCShowerMatch::CleanShowers " << endl;

  double vtx = get_vertex();

  vector<PairIndices> ToMerge;
  vector<unsigned int> UsedSplinters; //keep track so don't reuse a splinter
  
  unsigned int NShowers = mpcexShwrs->size();
  for (unsigned int i=0 ; i < NShowers; i++) { 
    TMpcExShower *AShower = mpcexShwrs->getShower(i);

    //main must be a closest
    int closest = AShower->get_ClosestMPCClusterClosestFlag();
    if (closest!=1) continue;
    
    //must have a valid cluster number
    int AclustNum = AShower->get_ClosestMPCClusterIndex();
    if (AclustNum < 0) continue;
    
    float hsxA = AShower->get_hsx();
    float hsyA = AShower->get_hsy();
    int armA = AShower->get_arm();
    
    if (verbosity)    cout << i << "th shower, checking if needs to be cleaned " << AclustNum << endl;
    for (unsigned int j=0; j < NShowers; j++) {	
      //can't be the same shower
      if (i==j) continue; 

      //can't already be merged with different main 
      //probably overkill since there is only one closest but just in case
      if (find(UsedSplinters.begin(),UsedSplinters.end(),j)==UsedSplinters.end()) continue;      

      TMpcExShower *BShower = mpcexShwrs->getShower(j);
      
      //must be in the same arm
      int armB = BShower->get_arm();
      if (armA!=armB) continue; 

      //must point to the same mpc cluster
      int BclustNum = BShower->get_ClosestMPCClusterIndex();
      if (AclustNum!=BclustNum) continue; 
      
      float hsxB = BShower->get_hsx();
      float hsyB = BShower->get_hsy();
      double dHough = sqrt((hsxA-hsxB)*(hsxA-hsxB) + (hsyA-hsyB)*(hsyA-hsyB));
      
      float PS_refZ = MpcExConstants::PS_REFERENCE_Z_N;
      if (armA==0) PS_refZ = MpcExConstants::PS_REFERENCE_Z_S;
      float cluster_radius = fabs(ShowerR/(PS_refZ - vtx));
      
      if (dHough < (1.25*cluster_radius) )
	{
	  if (verbosity)    
	    cout << "   " << j << "th shower matches to same MPC cluster " << BclustNum << "  dHough: " << dHough << endl;
	  //save indices of pair to merge
	  PairIndices mergeIndices;
	  mergeIndices.ClusterA = i;
	  mergeIndices.ClusterB = j;
	  ToMerge.push_back(mergeIndices);	  
	  UsedSplinters.push_back(j);
	}
    } //j loop
  } //i loop
  return ToMerge;
}

void mMpcExMPCShowerMatch::CalcShowerPosition(TMpcExShower *Shwr)
{
  if (verbosity) cout << " mMpcExMPCShowerMatch::CalcShowerPosition " << endl;
  double vtx = get_vertex();

  //initialize
  float cluster_hsx = 0;
  float cluster_hsy = 0;
  float cluster_Normx = 0;
  float cluster_Normy = 0;

  //from mMpcExShower
  unsigned int Nhits = Shwr->sizeHits();
  for (unsigned int i=0; i < Nhits; i++) {
    unsigned int Hit_key = Shwr->getHit(i);
    TMpcExHit *AHit = _fHits->get_hit_by_key(Hit_key);

    double e_hit = CombinedHitEnergy(AHit);
    float x = AHit->x();
    float y = AHit->y();
    float z = AHit->z() - vtx;
    float hsx_hit = x/z;
    float hsy_hit = y/z;

    float Xwidth = AHit->minipad_x_width();
    float Ywidth = AHit->minipad_y_width();
    float sigmaX = Xwidth/sqrt(12.)/fabs(z);
    float sigmaY = Ywidth/sqrt(12.)/fabs(z);
    float weightX = e_hit/(sigmaX*sigmaX);
    float weightY = e_hit/(sigmaY*sigmaY);
    
    cluster_hsx = cluster_hsx + hsx_hit*weightX;
    cluster_hsy = cluster_hsy + hsy_hit*weightY;
    cluster_Normx = cluster_Normx + weightX;
    cluster_Normy = cluster_Normy + weightY;
  }

  cluster_hsx = cluster_hsx/cluster_Normx;
  cluster_hsy = cluster_hsy/cluster_Normy;

  Shwr->set_hsx(cluster_hsx);
  Shwr->set_hsy(cluster_hsy);
}

void mMpcExMPCShowerMatch::CalcShowerHitInfo(TMpcExShower *Shwr)
{
  if (verbosity) cout << " mMpcExMPCShowerMatch::CalcShowerHitInfo " << endl;
  double vtx = get_vertex();

  //initialize variables for energy, layers
  int NLayerHits[MpcExConstants::NLAYERS];
  float e_layer[MpcExConstants::NLAYERS];
  for (unsigned int i=0; i < MpcExConstants::NLAYERS; i++) {
    NLayerHits[i] = 0;
    e_layer[i] = 0;
  }

  float rms_hsx = 0;
  float rms_hsy = 0;
  float sum_weight_X = 0;
  float sum_weight_Y = 0;

  float disp_hsx =0;
  float disp_hsy =0;
  float sum_weight_Xdisp = 0;
  float sum_weight_Ydisp = 0;

  float eweight =0; //is this right?  not sure if it is the same as mMpcExShower

  unsigned int n_sat_minipads = 0;

  //loop over hits to get info on each hit
  unsigned int Nhits = Shwr->sizeHits();
  for (unsigned int i=0; i < Nhits; i++) {
    unsigned int Hit_key = Shwr->getHit(i);

    float hsx_shwr = Shwr->get_hsx();
    float hsy_shwr = Shwr->get_hsy();

    TMpcExHit *AHit = _fHits->get_hit_by_key(Hit_key);    
    unsigned int layer = AHit->layer();
    double e_combo = CombinedHitEnergy(AHit);

    if (e_combo >= EMax_high*hlslope) n_sat_minipads++;
    if (e_combo > 0.0) NLayerHits[layer]++;

    eweight = eweight + e_combo; 
    e_layer[layer] = e_layer[layer] + e_combo;

    float x = AHit->x();
    float y = AHit->y();
    float z = AHit->z() - vtx;
    float hsx_hit = x/z;
    float hsy_hit = y/z;

    float Xwidth = AHit->minipad_x_width();
    float Ywidth = AHit->minipad_y_width();
    float sigmaX = Xwidth/sqrt(12.)/fabs(z);
    float sigmaY = Ywidth/sqrt(12.)/fabs(z);

    float weight_X = e_combo/(sigmaX*sigmaX);
    float weight_Y = e_combo/(sigmaY*sigmaY);

    rms_hsx = rms_hsx + (hsx_hit - hsx_shwr)*(hsx_hit - hsx_shwr)*weight_X;
    rms_hsy = rms_hsy + (hsy_hit - hsy_shwr)*(hsy_hit - hsy_shwr)*weight_Y;

    sum_weight_X = sum_weight_X + weight_X;
    sum_weight_Y = sum_weight_Y + weight_Y;


    float weight_Xdisp = TMath::Max(0.0, (4.5 + log(e_combo/eweight))/(sigmaX*sigmaX));
    float weight_Ydisp = TMath::Max(0.0, (4.5 + log(e_combo/eweight))/(sigmaY*sigmaY));
      
    disp_hsx = disp_hsx + (hsx_hit-hsx_shwr)*(hsx_hit-hsx_shwr)*weight_Xdisp;
    disp_hsy = disp_hsy + (hsy_hit-hsy_shwr)*(hsy_hit-hsy_shwr)*weight_Ydisp;

    sum_weight_Xdisp = sum_weight_Xdisp + weight_Xdisp;
    sum_weight_Ydisp = sum_weight_Ydisp + weight_Ydisp;
    }

  if (Nhits > 1){
    rms_hsx = sqrt(rms_hsx/sum_weight_X);
    rms_hsy = sqrt(rms_hsy/sum_weight_Y);
  
    disp_hsx = sqrt(disp_hsx/sum_weight_Xdisp);
    disp_hsy = sqrt(disp_hsy/sum_weight_Ydisp);
    }
  else
    {
      rms_hsx = 0;
      rms_hsy = 0;
 
      disp_hsx = 0;
      disp_hsy = 0;
    }

  //sum counters for energy, layers
  float esum = 0;
  unsigned int nlayers =0;
  unsigned int first_layer = MpcExConstants::NLAYERS;
  for (unsigned int i=0; i < MpcExConstants::NLAYERS; i++) {
    if (NLayerHits[i] > 0) nlayers++;
    if (NLayerHits[i] > 0 && first_layer > i) first_layer = i;
    esum = esum + e_layer[i];
  }

  //energy correction
  int arm = Shwr->get_arm();
  int firstlayerBin = first_layer;

  float Elow_Ecorr = MpcExConstants::efcp[arm][firstlayerBin][0]*(esum/Econv) + 
    MpcExConstants::efcp[arm][firstlayerBin][1]*(esum/Econv)*(esum/Econv) +
    MpcExConstants::efcp[arm][firstlayerBin][2]*(esum/Econv)*(esum/Econv)*(esum/Econv);

  Elow_Ecorr = Elow_Ecorr/(1 + MpcExConstants::mexlcor[arm][0] + MpcExConstants::mexlcor[arm][1]*Elow_Ecorr + 
			   MpcExConstants::mexlcor[arm][2]*Elow_Ecorr*Elow_Ecorr +
			   MpcExConstants::mexlcor[arm][3]*Elow_Ecorr*Elow_Ecorr*Elow_Ecorr +
			   MpcExConstants::mexlcor[arm][4]*Elow_Ecorr*Elow_Ecorr*Elow_Ecorr*Elow_Ecorr +
			   MpcExConstants::mexlcor[arm][5]*Elow_Ecorr*Elow_Ecorr*Elow_Ecorr*Elow_Ecorr*Elow_Ecorr +
			   MpcExConstants::mexlcor[arm][6]*Elow_Ecorr*Elow_Ecorr*Elow_Ecorr*Elow_Ecorr*Elow_Ecorr*Elow_Ecorr);

  unsigned int calibEinRange = Shwr->get_CalibEInRange();
  if (esum/Econv < MpcExConstants::fitUpLimit[firstlayerBin])	
    calibEinRange = 1;

  //set values in shower
  Shwr->set_rms_hsx(rms_hsx);
  Shwr->set_rms_hsy(rms_hsy);

  Shwr->set_disp_hsx(disp_hsx);
  Shwr->set_disp_hsy(disp_hsy);

  Shwr->set_esum(Elow_Ecorr);
  Shwr->set_raw_esum(esum/Econv);
  Shwr->set_CalibEInRange(calibEinRange);

  Shwr->set_nlayers(nlayers);
  Shwr->set_first_layer(first_layer);
  Shwr->set_n_sat_minipads(n_sat_minipads);

  for (unsigned int i=0; i < MpcExConstants::NLAYERS; i++) {
    Shwr->set_e_layer(i,e_layer[i]);
  }

  return;
}

void mMpcExMPCShowerMatch::FindMPCclosest(TMpcExShowerContainer *mpcexShwrs)
{
  if (verbosity) cout << " mMpcExMPCShowerMatch::FindMPCclosest " << endl;
  //find out how many mpcex clusters point to the same mpc cluster
  map<int, int> MPCclusters;
  unsigned int NShowers = mpcexShwrs->size();
  for (unsigned int i=0; i < NShowers; i++) {
    TMpcExShower *AShower = mpcexShwrs->getShower(i);

    int clustNum = AShower->get_ClosestMPCClusterIndex();
    if (MPCclusters.find(clustNum)==MPCclusters.end())
      MPCclusters[clustNum]=1;
    else	
      MPCclusters[clustNum]++;
  }

  //find closest MPC matches --logic taken from mMpcExShower
  for (unsigned int i=0; i < NShowers; i++) {    
    TMpcExShower *AShower = mpcexShwrs->getShower(i);
  
    //assume it is the closest -- search for conflicts
    AShower->set_ClosestMPCClusterClosestFlag(1);

    //not closest if nonsense mpcex arm, no mpc match
    int AclustNum = AShower->get_ClosestMPCClusterIndex();
    if (AclustNum < 0)  
      {
	if (verbosity) cout << i << "  not closest  " << AclustNum << endl;
	AShower->set_ClosestMPCClusterClosestFlag(0);
	continue;
      }

    //no other cluster matches -- it's the closest
    if (MPCclusters[AclustNum]==1)
      {
	if (verbosity)  cout << "      closest! -- because no other matches " << MPCclusters[AclustNum] << endl;
	continue;
      }

    float Adist = AShower->get_ClosestMPCClusterDistance();
    unsigned int armA = AShower->get_arm();
    if (verbosity)  
      cout << "   " << i << " arm: " << armA << " MPCdist: " << Adist << " MPCcluster: " << AclustNum 
	   << "  number of times that MPCcluster used: " << MPCclusters[AclustNum] << endl;

    //search for a closer mpcex shower for the same mpc cluster
    for (unsigned int j=0; j < NShowers; j++) {
      if (i==j) continue; //not the same shower
      TMpcExShower *BShower = mpcexShwrs->getShower(j);

      int clustNumB = BShower->get_ClosestMPCClusterIndex();
      if (clustNumB < 0 ) continue; //no mpc match
      
      unsigned int armB = BShower->get_arm();
      if (armA!=armB) continue; //in the same arm
      
      int BclustNum = BShower->get_ClosestMPCClusterIndex();
      if (BclustNum!=AclustNum) continue; //point to the same mpc cluster
      
      float Bdist = BShower->get_ClosestMPCClusterDistance();
      if (Bdist < Adist) //another one is closer
	{
	  AShower->set_ClosestMPCClusterClosestFlag(0);
	  if (verbosity)   
	    cout << "         NOT closest!  " << j << " is closer: " 
		 << Bdist << " < " << Adist << endl;
	  break;
	}
    }
  }
  return;
}

double mMpcExMPCShowerMatch::CombinedHitEnergy(TMpcExHit *AHit) 
{
  double CombinedEnergy = 0;

  if (AHit->status()==TMpcExHit::GAIN_CALIBRATED)
    {
      if (AHit->high() < EMax_high) CombinedEnergy = AHit->high();
      else CombinedEnergy = AHit->low();
    }

  return CombinedEnergy;
}

//taken verbatim from mMpcExShower
void mMpcExMPCShowerMatch::MergeShowers(TMpcExShower *main, TMpcExShower *splinter)
{
  if (verbosity) cout << " mMpcExMPCShowerMatch::MergeShowers " << endl;

  unsigned int nhits = splinter->sizeHits();
  for (unsigned int i=0; i< nhits; i++)
    main->addHit(splinter->getHit(i));

  main->set_merged(1); //flag to id merged showers

  return;
}

//taken almost verbatim from mMpcExShower 
void mMpcExMPCShowerMatch::MatchMPC_HoughSpace(TMpcExShower *AShower)
{
  if (verbosity) cout << " mMpcExMPCShowerMatch::MatchMPC_HoughSpace " << endl;
  double vtx = get_vertex();

  //initial values
  float dist = 1.0;     
  float distX = 1.0;     
  float distY = 1.0;     
  float clustE = 0.0;    
  int clustNum = -1;    
  int closest = 0; //not closest    

  //mpcex info
  int mpcex_arm = AShower->get_arm();
  float mpcex_hsx = AShower->get_hsx();
  float mpcex_hsy = AShower->get_hsy();

  int NClusters = _fmpcClusters->size();   
  for(int iMPCClus=0 ; iMPCClus< NClusters ; iMPCClus++)                   
    {                                                                         
      mpcClusterContent *clus =  _fmpcClusters->getCluster(iMPCClus);                                                                              
      // dont match if not the same arm!!                                     
      if(clus->arm()!= mpcex_arm) continue;

      //calculate distance in hough space between mpcex shower and mpc cluster
      float delta_hsx = mpcex_hsx - ((clus->x()/(clus->z()-vtx))); 
      float delta_hsy = mpcex_hsy - ((clus->y()/(clus->z()-vtx))); 
      double ThisDist = sqrt(delta_hsx*delta_hsx + delta_hsy*delta_hsy); 
      if( ThisDist < dist ){                              
	dist  = ThisDist;                      
	distX = delta_hsx;
	distY = delta_hsy;
	clustE = clus->e();                                   
	clustNum = iMPCClus;                                  
	if (verbosity)  cout << "   possible MPC cluster match!" << ThisDist << " " << iMPCClus << "     " << clus->x() << "  "<< clus->y() << " " << clus->z() << " " << vtx  << "  " << clus->x()/(clus->z() - vtx) << "  " << clus->y()/(clus->z() - vtx) << "     " << mpcex_hsx << " " << mpcex_hsy << endl; 
      }     
    }

  AShower->set_ClosestMPCClusterDistance(dist);
  AShower->set_ClosestMPCClusterDistanceX(distX);
  AShower->set_ClosestMPCClusterDistanceY(distY);
  AShower->set_ClosestMPCClusterEnergy(clustE);
  AShower->set_ClosestMPCClusterIndex(clustNum);
  AShower->set_ClosestMPCClusterClosestFlag(closest);
  
  return;    
}

void mMpcExMPCShowerMatch::MatchMPCTower_HoughSpace(TMpcExShower *AShower)
{
  double vtx = get_vertex();

  //mpcex info
  int mpcex_arm = AShower->get_arm();
  float mpcex_hsx = AShower->get_hsx();
  float mpcex_hsy = AShower->get_hsy();

  //initial values
  float dist = 9999.0;     
  float gridx_peak = 0.0;     
  float gridy_peak = 0.0;     
  int towerNum = -1;    

  int NTowers = _fmpcTowers->size();   
  for(int i=0 ; i < NTowers ; i++)
    {
      mpcTowerContent *twr =  _fmpcTowers->getTower(i);
      int channel = twr->get_ch();
      int mpc_arm = _fmpc_map->getArm(channel);

      // dont match if not the same arm!!                                     
      if(mpc_arm != mpcex_arm) continue;

      float mpc_x = _fmpc_map->getX(channel);
      float mpc_y = _fmpc_map->getY(channel);
      float mpc_z =  MpcExConstants::MPC_REFERENCE_Z;
      if (mpc_arm==0) mpc_z = -1.*MpcExConstants::MPC_REFERENCE_Z;

      float mpc_hsx = mpc_x/(mpc_z-vtx);
      float mpc_hsy = mpc_y/(mpc_z-vtx);

      //calculate distance in hough space between mpcex shower and mpc cluster
      float delta_hsx = mpcex_hsx - mpc_hsx;
      float delta_hsy = mpcex_hsy - mpc_hsy;
      double ThisDist = sqrt(delta_hsx*delta_hsx + delta_hsy*delta_hsy); 
      if( ThisDist < dist ){                              
	dist  = ThisDist;                      
	towerNum = i;
	gridx_peak = _fmpc_map->getGridX(channel);
	gridy_peak = _fmpc_map->getGridY(channel);
	
	if (verbosity)  
	  cout << "   possible MPC tower match!" << ThisDist << " " << i 
	       << "     " << mpc_x << "  "<< mpc_y << " " << mpc_z 
	       << " " << vtx  << "  " << mpc_hsx << "  " << mpc_hsy 
	       << "     " << mpcex_hsx << " " << mpcex_hsy << endl; 
      }     
    }

  //initialize values to 0
  int fiducial = 0;
  int pkix = -1;
  int pkiy = -1;
  int N_5x5 =0;
  int N_3x3 =0;
  float E_5x5 = 0.0;
  float E_3x3 = 0.0;
  float MPC_E[5][5];
  for (unsigned int i=0; i < 5; i++) {
    for (unsigned int j=0; j < 5; j++) {
      MPC_E[i][j] = 0.0;
    }
  }

  if (towerNum >= 0)
    {
      pkix = gridx_peak;
      pkiy = gridy_peak;

      //calculate information in area around matched tower
      for(int i=0 ; i < NTowers ; i++)
	{
	  mpcTowerContent *twr =  _fmpcTowers->getTower(i);
	  double mpc_e = twr->get_energy(); 
	  int channel = twr->get_ch();
	  
	  // dont match if not the same arm!!                                     
	  int mpc_arm = _fmpc_map->getArm(channel);	  
	  if(mpc_arm != mpcex_arm) continue;
	  
	  int gridx_twr = _fmpc_map->getGridX(channel);
	  int gridy_twr = _fmpc_map->getGridY(channel);
	  
	  //it's in a 5x5 block
	  if ( abs(gridx_twr - gridx_peak) <= 2 && abs(gridy_twr - gridy_peak) <= 2) //abs or fabs? 
	    {
	      int grid_indexX = gridx_twr - gridx_peak + 2;
	      int grid_indexY = gridy_twr - gridy_peak + 2;
	      if (mpc_e >= 0. && !isnan(mpc_e)) 
		{
		  E_5x5 = E_5x5 + mpc_e;
		  MPC_E[grid_indexX][grid_indexY] = mpc_e;
		}
	      
	      //it's in a 3x3 block
	      if ( abs(grid_indexX) <= 1 && abs(grid_indexY) <= 1) 
		{
		  if (mpc_e >= 0. && !isnan(mpc_e)) 
		    E_3x3 = E_3x3 + mpc_e;
		}
	    }
	}//end for loop over towers      

      //count number of legitimate towers in 5x5 and 3x3 area
      for (unsigned int i = gridx_peak - 2; i < gridx_peak + 2; i++) {
	for (unsigned int j = gridy_peak - 2; j  < gridy_peak + 2; j++) {
	  if (_fmpc_map->getFeeCh(i,j,mpcex_arm) >= 0) { //how tell if legitimate or valid
	    N_5x5++;
	    if (fabs(gridx_peak - i) < 1 && fabs(gridy_peak - j) < 1) N_3x3++; 
	  }
	}
      }

      if (N_3x3 == 9) fiducial = 1; //set fiducial true if have all 9 in the 3x3
    }

  //fill shower with mpc tower info
  AShower->set_mpcE3x3(E_3x3);
  AShower->set_mpcE5x5(E_5x5);
  AShower->set_mpcN3x3(N_3x3);
  AShower->set_mpcN5x5(N_5x5);
  AShower->set_mpcCentTwr(towerNum);
  AShower->set_mpcCentTwrE(MPC_E[2][2]); //matched peak energy
  AShower->set_fiducial(fiducial);
  AShower->set_mpcPeakix(pkix);
  AShower->set_mpcPeakiy(pkiy);

  for (unsigned int i=0; i < 5; i++) {
    for (unsigned int j=0; j < 5; j++) {
      AShower->set_mpcTwrE(i,j,MPC_E[i][j]);
    }
  }

  //emprical correction to mpcE
  int n33_bin = N_3x3 - 5;
  float E33_Corr = 0;
  if (n33_bin >= 0)
    {
      E33_Corr = MpcExConstants::mefcp[mpcex_arm][n33_bin][0]*E_3x3 + 
	MpcExConstants::mefcp[mpcex_arm][n33_bin][1]*E_3x3*E_3x3 + 
	MpcExConstants::mefcp[mpcex_arm][n33_bin][2]*E_3x3*E_3x3*E_3x3 +
	MpcExConstants::mefcp[mpcex_arm][n33_bin][3]*E_3x3*E_3x3*E_3x3*E_3x3 + 
	MpcExConstants::mefcp[mpcex_arm][n33_bin][4]*E_3x3*E_3x3*E_3x3*E_3x3*E_3x3 + 
	MpcExConstants::mefcp[mpcex_arm][n33_bin][5]*E_3x3*E_3x3*E_3x3*E_3x3*E_3x3*E_3x3;
    }

  E33_Corr = E33_Corr/(1.0 + MpcExConstants::mpclcor[mpcex_arm][0] + 
		       MpcExConstants::mpclcor[mpcex_arm][1]*E33_Corr + 
		       MpcExConstants::mpclcor[mpcex_arm][2]*E33_Corr*E33_Corr + 
		       MpcExConstants::mpclcor[mpcex_arm][3]*E33_Corr*E33_Corr*E33_Corr + 
		       MpcExConstants::mpclcor[mpcex_arm][4]*E33_Corr*E33_Corr*E33_Corr*E33_Corr + 
		       MpcExConstants::mpclcor[mpcex_arm][5]*E33_Corr*E33_Corr*E33_Corr*E33_Corr*E33_Corr + 
		       MpcExConstants::mpclcor[mpcex_arm][6]*E33_Corr*E33_Corr*E33_Corr*E33_Corr*E33_Corr*E33_Corr);

  float Tot_Ecorr = AShower->get_esum() + E33_Corr;

  Tot_Ecorr = Tot_Ecorr/(1.0 + MpcExConstants::comblcor[mpcex_arm][0] + MpcExConstants::comblcor[mpcex_arm][1]*Tot_Ecorr + 
			 MpcExConstants::comblcor[mpcex_arm][2]*Tot_Ecorr*Tot_Ecorr + 
			 MpcExConstants::comblcor[mpcex_arm][3]*Tot_Ecorr*Tot_Ecorr*Tot_Ecorr + 
			 MpcExConstants::comblcor[mpcex_arm][4]*Tot_Ecorr*Tot_Ecorr*Tot_Ecorr*Tot_Ecorr + 
			 MpcExConstants::comblcor[mpcex_arm][5]*Tot_Ecorr*Tot_Ecorr*Tot_Ecorr*Tot_Ecorr*Tot_Ecorr + 
			 MpcExConstants::comblcor[mpcex_arm][6]*Tot_Ecorr*Tot_Ecorr*Tot_Ecorr*Tot_Ecorr*Tot_Ecorr*Tot_Ecorr);

  //  unsigned int calibEinRange = isCalibEinRange(mpcex_e, E_3x3, mpcex_arm);
  unsigned int calibEinRange = AShower->get_CalibEInRange();
  if (n33_bin >= 0)
    if (E_3x3 < MpcExConstants::mpcfitUpLimit[n33_bin])	
      calibEinRange = 1;

  //  float roughTotE = findCorrectEnergy(mpcex_e, E_3x3, mpcex_arm);
  AShower->set_mpcECorr(E33_Corr);
  AShower->set_roughTotE(Tot_Ecorr);
  AShower->set_CalibEInRange(calibEinRange);

  return;    
}
