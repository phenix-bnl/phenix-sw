// $Id: T0Simreco.C,v 1.1 2008/10/18 12:44:21 hpereira Exp $

/*! 
  \file T0Simreco.h
  \brief get simulated vertex and store into BBC node
  \version $Revision: 1.1 $
  \date $Date: 2008/10/18 12:44:21 $
*/

#include <BbcOutv1.h>
#include <PISAEventHeader.h>
#include <getClass.h>
#include <gsl/gsl_randist.h>
#include <iostream>
#include <Fun4AllReturnCodes.h>

#include "T0Simreco.h"
#include "SIMRECO.h"

using namespace std;

//______________________________________________________
T0Simreco::T0Simreco(const std::string &name): 
  T0Reco(name.c_str()),
  _t0(0),
  _t0_sigma(0),
  _overwrite_bbc( true ),  
  rng( gsl_rng_alloc (gsl_rng_mt19937) )
{ gsl_rng_set(rng, 1234); }

//______________________________________________________
T0Simreco::~T0Simreco( void )
{ gsl_rng_free( rng ); }

//______________________________________________________
int T0Simreco::Init(PHCompositeNode *top_node)
{
  
  SIMRECO::PRINT( cout, "T0SimReco::Init" );

  // print configuration parameters
  cout << "_t0 = " << _t0 << endl;	
  cout << "_t0_sigma = " << _t0_sigma << endl;	
  cout << "_overwrite_bbc = " << ( (_overwrite_bbc) ? "true":"false" ) << endl;	
  
  // it is important to call the base class method to make 
  // sure that the T0Out node is properly created, with correct
  // version number
  int return_code = T0Reco::InitRun( top_node );
  if( return_code != EVENT_OK ) return return_code;

  BbcOut* bbcout = findNode::getClass<BbcOut>(top_node, "BbcOut");
  if (!bbcout)
  {
    
    cout << "creating new BbcOut node." << endl;
    
    PHNodeIterator iter(top_node);
    PHCompositeNode *DstNode;
    DstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
    
    bbcout = new BbcOutv1();
    PHIODataNode <BbcOut>* BbcOutNode = new PHIODataNode <BbcOut>(bbcout, "BbcOut", "PHObject");
    DstNode->addNode(BbcOutNode);
  }
  
  SIMRECO::PRINT( cout, "**" );

  return EVENT_OK;
  
}

//______________________________________________________
int T0Simreco::process_event(PHCompositeNode *top_node)
{
  
  // assign t0
  float current_t0 = _t0;

  // smear and assign errors to t0
  float current_t0_error = 0;
  if( _t0_sigma > 0.0)
  { 
    current_t0 += gsl_ran_gaussian(rng, _t0_sigma); 
    current_t0_error = _t0_sigma;
  }
  
  // overwrite bbcout node contents
  if( _overwrite_bbc )
  {
    BbcOut *bbcout = findNode::getClass<BbcOut>(top_node, "BbcOut");
    bbcout->set_TimeZero( current_t0, current_t0_error );
  }

  // right now T0Reco is not suited to get a dedicated T0 from sim
  // it is this left unaffected (T0Reco should take care of copying the BbcOut T0 to T0Out)
  // TODO: change T0Out so that it contains 'generic' T0 calculations such as T0Out
  
  return EVENT_OK;
}
