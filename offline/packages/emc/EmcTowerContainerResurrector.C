#include <cstdlib>
#include <cassert>
#include <iostream>

#include "Fun4AllReturnCodes.h"
#include "getClass.h"

#include "emcNodeHelper.h"
#include "emcTowerContainerDST.h"

#include "EmcTowerContainerResurrector.h"



using namespace std;
using namespace findNode;





EmcTowerContainerResurrector::EmcTowerContainerResurrector()
{
  
  _emcHitContainerNodeName = "emcHitContainer";
  _emcTowerContainerNodeName = "emcTowerContainer";
  Name("EmcTowerContainerResurrector");
}



EmcTowerContainerResurrector::~EmcTowerContainerResurrector()
{
}



int EmcTowerContainerResurrector::InitRun(PHCompositeNode *topNode)
{
  // now find the DST tree and make the emcClusterContainer

  emcNodeHelper nh;

  PHCompositeNode* dstNode =  nh.findCompositeNode(topNode,"DST");

  nh.addObject<emcTowerContainerDST>(dstNode,_emcTowerContainerNodeName.c_str());

  return EVENT_OK;
 
}



int EmcTowerContainerResurrector::Reset(PHCompositeNode *topNode){

  emcTowerContainerDST *_emcTowerContainer_ptr = getClass<emcTowerContainerDST>(topNode, _emcTowerContainerNodeName.c_str());
  if ( !_emcTowerContainer_ptr )
    {
      std::cout << PHWHERE << "Could not find " << _emcTowerContainerNodeName << " Node" << std::endl;
      return ABORTRUN;
    }

  _emcTowerContainer_ptr->Reset();

  return EVENT_OK;
}



#define CHECK(x) if( t->x != newt->x ){ std::cout << "id = " << t->TowerID() << ": " << #x << ": " << t->x << " " << newt->x << std::endl; exit(-1); }

int EmcTowerContainerResurrector::process_event(PHCompositeNode *topNode)
{

  emcTowerContainerDST * _emcHitContainer_ptr = getClass<emcTowerContainerDST>(topNode, _emcHitContainerNodeName.c_str());
  if ( !_emcHitContainer_ptr ) 
    {
      std::cout << PHWHERE << "Could not find " << _emcHitContainerNodeName << " Node" << std::endl;
      return ABORTRUN;
    }


  emcTowerContainerDST *_emcTowerContainer_ptr = getClass<emcTowerContainerDST>(topNode, _emcTowerContainerNodeName.c_str());
  if ( !_emcTowerContainer_ptr )
    {
      std::cout << PHWHERE << "Could not find " << _emcTowerContainerNodeName << " Node" << std::endl;
      return ABORTRUN;
    }


    
  for(size_t i = 0; i < _emcHitContainer_ptr->size(); i++){

    emcTowerContentDST * t  = _emcHitContainer_ptr->getTower( i );
    if (!t) continue;


    emcTowerContentDST * newt = _emcTowerContainer_ptr->findTower( t->TowerID() );

    if( newt == NULL ){

      _emcTowerContainer_ptr->addTower( _emcTowerContainer_ptr->size(), *t );

    } else {

      // the most important sanity checks
      /*CHECK( Energy() );*/ CHECK( ADC() );
      CHECK( ToF() ); CHECK( TDC() );
      CHECK( WarnNeighbours() );
      CHECK( ErrorNeighbours() );

      newt->SetCalibrated( newt->Energy() + t->Energy(), t->ToF() );

    }
  }



  if( verbosity )
    std::cout << PHWHERE << "size   in = " << _emcHitContainer_ptr->size() << "   out = " << _emcTowerContainer_ptr->size() << std::endl;


  return EVENT_OK;
}

