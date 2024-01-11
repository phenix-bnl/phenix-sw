#include "emcOFTowerContainer.h"
#include "dEmcCalibTowerWrapper.h"
#include "PHTypedNodeIterator.h"
#include "PHIODataNode.h"
#include "emcTowerContainerv1.h"
#include <cassert>
#include <cmath>
#include "EmcIndexer.h"

namespace
{
  template<class T>
  T*
  objectHandle(PHCompositeNode* node, const char* name)
  {
    PHTypedNodeIterator<T> iter(node);
    PHIODataNode<T>* objectNode = iter.find(name);
    if (!objectNode) 
      {
	return 0;
      }
    else
      {
	T* rv = objectNode->getData();
	return rv;
      }
  }

  emcOFTowerContainer gemcOFTowerContainer;
}

using namespace std;

//_____________________________________________________________________________
emcOFTowerContainer::emcOFTowerContainer() : emcObjectFiller()
{
}

//_____________________________________________________________________________
emcOFTowerContainer::~emcOFTowerContainer()
{
}

//_____________________________________________________________________________
bool 
emcOFTowerContainer::canFill(PHObject& destination, int verbose) const
{
  emcTowerContainerv1* tc = 
    dynamic_cast<emcTowerContainerv1*>(&destination);
  if ( tc ) 
    {
      return true;
    }
  else
    {
      if ( verbose ) 
	{
	  std::cerr << "emcOFTowerContainer::canFill : destination "
		    << "is not an emcTowerContainerv1" << std::endl;
	}
      return false;
    } 
}

//_____________________________________________________________________________
bool 
emcOFTowerContainer::canFill(PHCompositeNode* topNode, 
			     PHObject& destination,
			     int verbose) const
{
  bool rv = canFill(destination);
  if ( rv ) 
    {
      dEmcCalibTowerWrapper* ct = objectHandle<dEmcCalibTowerWrapper>
	(topNode,"dEmcCalibTower");
      if ( ct ) 
	{
	  rv = true;
	}
      else
	{
	  rv = false;
	}
    }
  return rv;
}

//_____________________________________________________________________________
bool 
emcOFTowerContainer::fill(PHCompositeNode* topNode, 
			  PHObject& destination,
			  int verbose)
{
  assert(canFill(topNode,destination,false)==true);
  dEmcCalibTowerWrapper* ct = objectHandle<dEmcCalibTowerWrapper>
    (topNode,"dEmcCalibTower");
  emcTowerContainerv1& tc = static_cast<emcTowerContainerv1&>(destination);

  size_t ntowers = ct->RowCount();

  tc.Reset();

  for ( size_t i = 0; i < ntowers; ++i ) 
    {
      emcTowerContent* t = tc.addTower(i);

      int towerID = EmcIndexer::TowerID(ct->get_swkey(i));
      int ifem, ichannel;
      EmcIndexer::PXPXSM144CH(towerID,ifem,ichannel);
      t->SetID(ifem,ichannel);

      t->SetCalibrated(ct->get_ecal(i),ct->get_tof(i));
      t->SetNeighbours(ct->get_deadmap(i),ct->get_warnmap(i));
      t->SetADCTDC(static_cast<int>(rint(ct->get_adc(i))),
		   static_cast<int>(rint(ct->get_tac(i))));
    }
  return true;
}
  
//_____________________________________________________________________________
void 
emcOFTowerContainer::identify(std::ostream& os) const
{
  os << "emcOFTowerContainer" << std::endl;
}

//_____________________________________________________________________________
int 
emcOFTowerContainer::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void 
emcOFTowerContainer::Reset()
{
}
