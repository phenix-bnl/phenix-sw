#include "emcOFClusterContainer.h"
#include "dEmcClusterLocalExtWrapper.h"
#include "PHTypedNodeIterator.h"
#include "PHIODataNode.h"
#include "emcClusterContainerv1.h"
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

  emcOFClusterContainer gemcOFClusterContainer;
}

using namespace std;

//_____________________________________________________________________________
emcOFClusterContainer::emcOFClusterContainer() : emcObjectFiller()
{
}

//_____________________________________________________________________________
emcOFClusterContainer::~emcOFClusterContainer()
{
}

//_____________________________________________________________________________
bool 
emcOFClusterContainer::canFill(PHObject& destination, int verbose) const
{
  emcClusterContainerv1* tc = 
    dynamic_cast<emcClusterContainerv1*>(&destination);
  if ( tc ) 
    {
      return true;
    }
  else
    {
      if ( verbose ) 
	{
	  std::cerr << "emcOFClusterContainer::canFill : destination "
		    << "is not an emcClusterContainerv1" << std::endl;
	}
      return false;
    } 
}

//_____________________________________________________________________________
bool 
emcOFClusterContainer::canFill(PHCompositeNode* topNode, 
			     PHObject& destination,
			     int verbose) const
{
  bool rv = canFill(destination);
  if ( rv ) 
    {
      dEmcClusterLocalExtWrapper* ct = objectHandle<dEmcClusterLocalExtWrapper>
	(topNode,"dEmcClusterLocalExt");
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
emcOFClusterContainer::fill(PHCompositeNode* topNode, 
			  PHObject& destination,
			  int verbose)
{
  assert(canFill(topNode,destination,false)==true);
  dEmcClusterLocalExtWrapper* cc = objectHandle<dEmcClusterLocalExtWrapper>
    (topNode,"dEmcClusterLocalExt");
  emcClusterContainerv1& clus = 
    static_cast<emcClusterContainerv1&>(destination);

  size_t nclusters = cc->RowCount();

  clus.Reset();

  for ( size_t i = 0; i < nclusters; ++i ) 
    {
      emcClusterContent* c = clus.addCluster(i);
      c->set_arm(cc->get_arm(i));
      c->set_chi2(cc->get_chi2(i));
      c->set_disp(cc->get_disp(1,i),cc->get_disp(0,i));
      c->set_e(cc->get_e(i));
      int is = EmcIndexer::
	sectorOfflineToOnline(cc->get_arm(i),cc->get_sector(i));
      c->set_ecore( (is<6) ? cc->get_ecore(i) : cc->get_ecorr(i) );
      c->set_ecent(cc->get_ecent(i));
      c->set_etofmin(cc->get_etofmin(i));
      c->set_etofmax(cc->get_etofmax(i));
      c->set_id(cc->get_id(i));
      c->set_ipos(cc->get_ind(1,i),cc->get_ind(0,i));
      c->set_quality(cc->get_qual(i));
      c->set_maps(cc->get_deadmap(i),cc->get_warnmap(i));
      c->set_multiplicity(cc->get_twrhit(i));
      c->set_padisp(cc->get_padisp(1,i),cc->get_padisp(0,i));
      
      for ( int j = 0; j < cc->get_twrhit(i); ++j ) 
	{
	  c->set_partesum(j,cc->get_partesum(j,i));
	  c->set_towerid(j,cc->get_twrlist(j,i));
	}
      
      c->set_prob_photon(cc->get_prob_photon(i));
      c->set_phi(cc->get_phi(i));
      c->set_pid(static_cast<int>(rint(cc->get_pid(i))));
      c->set_sector(cc->get_sector(i));
      c->set_tof(cc->get_tof(i));
      c->set_tofmin(cc->get_tofmin(i));
      c->set_tofmax(cc->get_tofmax(i));
      c->set_theta(cc->get_theta(i));
      c->set_type(cc->get_type(i));
      c->set_xyz(cc->get_xyz(0,i),cc->get_xyz(1,i),cc->get_xyz(2,i));
    }
  return true;
}
  
//_____________________________________________________________________________
void 
emcOFClusterContainer::identify(std::ostream& os) const
{
  os << "emcOFClusterContainer" << std::endl;
}

//_____________________________________________________________________________
int 
emcOFClusterContainer::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void 
emcOFClusterContainer::Reset()
{
}
