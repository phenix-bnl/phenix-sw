#ifndef __MPCGEACLUSTERRECO_H__
#define __MPCGEACLUSTERRECO_H__

#include <SubsysReco.h>
#include <primaryWrapper.h>
#include <fkinWrapper.h>
#include <string>

class PHCompositeNode;
class MpcMap;
class mpcClusterContent;
class mpcClusterContainer;
class mpcTowerContainer;
class mpcGeaTowerContainer;
class mpcGeaClusterContainer;
class PHPythiaHeader;
class PHPythiaContainer;

/*
November 6, 2010
Beau Meredith

This SubSysReco module allows one to identify the ptcl composition of
clusters.  The output is mpcGeaClusterContainer on the node tree.  The
ctor parameters are the node name containing the sim towers (gnmame),
the nodename containing the pythia or hijing event record (pname), the
node index (important when you are putting multiple
mpcGeaClusterContainers on the node tree...start with 0 and increment
by 1 for each additional embedded node...this feeds into
MpcClusterIDReco very easily by assigning the containers the name
mpcGeaClusterContainer, mpcGeaClusterContainer2,XXX3,...) and the
algorithm (default should be used).
*/





class MpcGeaClusterReco: public SubsysReco
{
public:
  MpcGeaClusterReco(const std::string &name = "MpcGeaClusterReco",const std::string gname = "DEFAULT",std::string pname = "NONE",int node_index=0,int alg = 1); 
  //gname == default means top node
  //leave pname to none if you are not using pythia info
  
  virtual ~MpcGeaClusterReco();

  //int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int process_event_bam(PHCompositeNode *topNode, int alg = 1);
  int process_event_mc(PHCompositeNode *topNode, int alg = 4);
  int process_event_pythia();
  int EndRun(const int runno);
  void Print(const std::string& ="ALL") const;

  float GetGeaEnergy(int track);
  
  //for different algorithms
  void SetAlgorithm(int whichalg){ fAlgorithm = whichalg;}
  int GetAlgorithm(){ return fAlgorithm;}
  


  //copied over from MpcSectorRecV2.C
  void SetProfileParametersV2_2(float Energy);
  float EvalShowPar2(int ipar, float e);
  float PredictEnergyV2_2(float x, float y, float Energy);
  float PredictEnergyV2_2(float r,float Energy);
  bool GetEcoreStatus(mpcClusterContent* clus, int feech);
  
  //adds mpcGeaClusterContainer to node tree
  int CreateNodeTree(PHCompositeNode *topNode);






private:
  MpcMap *mpcmap;

  mpcClusterContainer *mpcclus;
  mpcTowerContainer *mpctow;
  mpcGeaTowerContainer *mpcgeatow;
  mpcGeaClusterContainer *mpcgeaclus;

  float fShowPar2[6][4]; //exp(-r)+exp^(-r^3)+exp^(-r^5) used in sharing
  float fShow2[6]; //parameters used in sharing

  int fAlgorithm;
  PHCompositeNode* geaNode;
  std::string gea_name;

  PHCompositeNode* pythiaNode;
  std::string pythia_name;

  PHPythiaContainer *phpythia;

  primaryWrapper *primary;

  int whichnode; //index for the case where we have more than 1 GeaClus on tree
};

#endif /* __MPCGEACLUSTERRECO_H__ */

