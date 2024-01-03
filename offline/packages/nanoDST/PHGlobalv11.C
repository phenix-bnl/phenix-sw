#include "PHGlobalv11.h"
#include "PHGlobal_Central.h"
#include "PHGlobal_Muon.h"

#include "Fun4AllServer.h"
#include "getClass.h"

#include <map>

using namespace std;

ClassImp(PHGlobalv11)

static map<const PHGlobal *, PHCompositeNode *> MyTopNode;
  
PHGlobalv11::PHGlobalv11()
{
  Reset();
}

void PHGlobalv11::Reset()
{
  bbcn = 0;
  bbcs = 0;
  bbcqn = 0.;
  bbcqs = 0.;
  bbcz = -9999.;
  bbczerr = -9999.;
  bbct0 = -9999.;
  bbcts = -9999.;
  bbctn = -9999.;
  centrality = -9999.;
  zdcen = 0.;
  zdces = 0.;
  zdcz = -9999.;
  zdczerr = -9999.;
  zdct0 = -9999.;
  zdcts = -9999.;
  zdctn = -9999.;
  SmdXN = -9999.;
  SmdXS = -9999.;
  SmdYN = -9999.;
  SmdYS = -9999.;
  SmdEN = -9999.;
  SmdES = -9999.;
}

void PHGlobalv11::identify(std::ostream& os) const
{
  os << "identify yourself: PHGlobalv11 Object, Global Event Information." << std::endl;
}

int PHGlobalv11::isValid() const
{
  return ((bbct0 > -9990. || zdct0 > -9990.) ? 1 : 0);
}

void 
PHGlobalv11::setZdcEnergyNS(const float zdceNorth, const float zdceSouth)
{
  zdcen = zdceNorth;
  zdces = zdceSouth;
  return;
}

void 
PHGlobalv11::setBbcMultNS(const short int bbcNorth, const short int bbcSouth)
{
  bbcn = bbcNorth;
  bbcs = bbcSouth;
}

void 
PHGlobalv11::setBbcChargeNS(const float bbcqNorth, const float bbcqSouth)
{
  bbcqn = bbcqNorth;
  bbcqs = bbcqSouth;
}

short int
PHGlobalv11::getNumberDchTracks() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberDchTracks();
}

short int
PHGlobalv11::getNumberPC1Hits() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberPC1Hits();
}

short int
PHGlobalv11::getNumberPC2Hits() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberPC2Hits();
}

short int
PHGlobalv11::getNumberPC3Hits() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberPC3Hits();
}

short int
PHGlobalv11::getNumberTecTracks() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberTecTracks();
}

short int
PHGlobalv11::getNumberEmcClusters() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberEmcClusters();
}

short int
PHGlobalv11::getNumberTofHits() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberTofHits();
}

short int
PHGlobalv11::getNumberCerenkovHits() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getNumberCerenkovHits();
}

float
PHGlobalv11::getEmcEnergyW() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getEmcEnergyW();
}

float
PHGlobalv11::getEmcEnergyE() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getEmcEnergyE();
}

float
PHGlobalv11::getEmcEnergy() const
{
  PHGlobal_Central* globcent = getCentral();
  if( !globcent ){
    return -1;
  }

  return globcent->getEmcEnergy();
}

//______________________________________________________
int PHGlobalv11::get_nMuidHits(const int arm, const int plane) const
{
  PHGlobal_Muon* globmuon = getMuon();
  if( !globmuon ){
    return -1;
  }

  return globmuon->get_nMuidHits(arm, plane);
}


//______________________________________________________
int PHGlobalv11::get_nMutrHits(const int arm, const int station) const
{
  PHGlobal_Muon* globmuon = getMuon();
  if( !globmuon ){
    return -1;
  }

  return globmuon->get_nMutrHits(arm, station);
}


PHGlobal_Central* PHGlobalv11::getCentral() const
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

PHGlobal_Muon* PHGlobalv11::getMuon() const
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
