#include "PHGlobalv10.h"
#include "PHGlobal_Central.h"
#include "PHGlobal_Muon.h"

//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "Fun4AllServer.h"
#include "getClass.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"

#include <map>

using namespace std;

ClassImp(PHGlobalv10)

static map<const PHGlobal *, PHCompositeNode *> MyTopNode;
  
PHGlobalv10::PHGlobalv10()
{
  Reset();
}

void PHGlobalv10::Reset()
{
  bbcn = 0;
  bbcs = 0;
  bbcqn = 0.;
  bbcqs = 0.;
  bbcz = -9999.;
  bbczerr = -9999.;
  bbct0 = -9999.;
  centrality = -9999.;
  zdcen = 0.;
  zdces = 0.;
  zdcz = -9999.;
  zdczerr = -9999.;
  zdct0 = -9999.;
  SmdXN = -9999.;
  SmdXS = -9999.;
  SmdYN = -9999.;
  SmdYS = -9999.;
  SmdEN = -9999.;
  SmdES = -9999.;
}

void PHGlobalv10::identify(ostream& os) const
{
  os << "identify yourself: PHGlobalv10 Object, Global Event Information." << std::endl;
}

int PHGlobalv10::isValid() const
{
  return ((bbct0 > -9990. || zdct0 > -9990.) ? 1 : 0);
}

void 
PHGlobalv10::setZdcEnergyNS(const float zdceNorth, const float zdceSouth)
{
  zdcen = zdceNorth;
  zdces = zdceSouth;
  return;
}

void 
PHGlobalv10::setBbcMultNS(const short int bbcNorth, const short int bbcSouth)
{
  bbcn = bbcNorth;
  bbcs = bbcSouth;
}

void 
PHGlobalv10::setBbcChargeNS(const float bbcqNorth, const float bbcqSouth)
{
  bbcqn = bbcqNorth;
  bbcqs = bbcqSouth;
}

short int
PHGlobalv10::getNumberDchTracks() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberDchTracks();
}

short int
PHGlobalv10::getNumberPC1Hits() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberPC1Hits();
}

short int
PHGlobalv10::getNumberPC2Hits() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberPC2Hits();
}

short int
PHGlobalv10::getNumberPC3Hits() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberPC3Hits();
}

short int
PHGlobalv10::getNumberTecTracks() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberTecTracks();
}

short int
PHGlobalv10::getNumberEmcClusters() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberEmcClusters();
}

short int
PHGlobalv10::getNumberTofHits() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberTofHits();
}

short int
PHGlobalv10::getNumberCerenkovHits() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberCerenkovHits();
}

float
PHGlobalv10::getEmcEnergyW() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getEmcEnergyW();
}

float
PHGlobalv10::getEmcEnergyE() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getEmcEnergyE();
}

float
PHGlobalv10::getEmcEnergy() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getEmcEnergy();
}

//______________________________________________________
int PHGlobalv10::get_nMuidHits(const int arm, const int plane) const
{
  PHGlobal_Muon* globmuon = getMuon();
  if( !globmuon ){
    return -1;
  }

  return globmuon->get_nMuidHits(arm, plane);
}


//______________________________________________________
int PHGlobalv10::get_nMutrHits(const int arm, const int station) const
{
  PHGlobal_Muon* globmuon = getMuon();
  if( !globmuon ){
    return -1;
  }

  return globmuon->get_nMutrHits(arm, station);
}


PHGlobal_Central* PHGlobalv10::getCentral() const
{
  map<const PHGlobal *, PHCompositeNode *>::const_iterator iter = MyTopNode.find(this);
  if (iter == MyTopNode.end())
    {
      Fun4AllServer *se = Fun4AllServer::instance();
      MyTopNode[this] = se->topNode();
      iter = MyTopNode.find(this);
    }

  return findNode::getClass<PHGlobal_Central>((iter->second),"PHGlobal_CENTRAL");
}

PHGlobal_Muon* PHGlobalv10::getMuon() const
{
  map<const PHGlobal *, PHCompositeNode *>::const_iterator iter = MyTopNode.find(this);
  if (iter == MyTopNode.end())
    {
      Fun4AllServer *se = Fun4AllServer::instance();

      MyTopNode[this] = se->topNode();
      iter = MyTopNode.find(this);
    }

  return findNode::getClass<PHGlobal_Muon>((iter->second),"PHGlobal_MUON");
}
