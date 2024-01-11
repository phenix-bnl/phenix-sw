/*
 * $Id: EmcUnclusterizer.cc,v 1.1 2009/02/15 20:27:50 mazsi Exp $
 *
 * see header for description
 *
 */


#include <iostream>

#include <Fun4AllReturnCodes.h>
#include <emcNodeHelper.h>

#include <emcClusterContainer.h>
#include <emcGeaClusterContainer.h>

#include <EmcUnclusterizer.h>

using namespace std;



ClassImp(EmcUnclusterizer);





int EmcUnclusterizer::InitRun(PHCompositeNode * root){

  PHCompositeNode* dst = emcNodeHelper::findCompositeNode(root, "DST"); 
  EMCNODEASSERT( dst );

  emcClusterContainer * clusters = emcNodeHelper::getObject< emcClusterContainer >("emcClusterContainer", dst);
  EMCNODEASSERT( clusters );

  return EVENT_OK;
}





int EmcUnclusterizer::process_event(PHCompositeNode * root){

  PHCompositeNode* dst = emcNodeHelper::findCompositeNode(root, "DST"); 
  EMCNODEASSERT( dst );

  emcClusterContainer * clusters = emcNodeHelper::getObject< emcClusterContainer >("emcClusterContainer", dst);
  EMCNODEASSERT( clusters );

  clusters->Reset();

  emcGeaClusterContainer * geaclusters = dynamic_cast< emcGeaClusterContainer * >( clusters );
  if( geaclusters ) geaclusters->invcache();

  return EVENT_OK;
}



