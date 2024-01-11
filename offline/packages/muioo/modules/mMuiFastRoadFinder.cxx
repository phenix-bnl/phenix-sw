// $Id: mMuiFastRoadFinder.cxx,v 1.7 2009/06/09 17:20:07 hpereira Exp $

/*!
	 \file mMuiFastRoadFinder.cxx
	 \brief store LL1 emulator in maps
	 \author S. Kelly D. Silvermyr
	 \version $Revision: 1.7 $
	 \date $Date: 2009/06/09 17:20:07 $
*/

#include <PHIODataNode.h>
#include <MuIDLl1.h>
#include "mMuiFastRoadFinder.h"

#include <iostream>
#include <fstream>

#include <TMuiPseudoLL1Map.h>

#include "mMuiFastRoadFinderPar.h"

using namespace std;

//____________________________________
mMuiFastRoadFinder::mMuiFastRoadFinder() :
  _mod_par(0),
  _ll1_emulator(0),
  _ll1_map(0),
  _timer( PHTimeServer::get()->insert_new( "mMuiFastRoadFinder" ) )
{

  // Initialize accept counter array to zero.
  for (short i=0; i<MUIOO::MAX_ARM; i++) 
    {
      for (short j=0; j<mMuiFastRoadFinderPar::NMODES; j++) 
      { _naccept[i][j]=0; }
    }

}

//_____________________________________________________________________________
mMuiFastRoadFinder::~mMuiFastRoadFinder( void )
{ if( _ll1_emulator ) delete _ll1_emulator; }

//_____________________________________________________________________________
PHBoolean mMuiFastRoadFinder::event(PHCompositeNode *top_node) 
{
  _timer.get()->restart();
  
  try {
    
    // Reset IOC pointers
    set_interface_ptrs(top_node);
    
    // clear maps
    _ll1_map->clear();
    
    // run ll1 emulator
    ll1_emulator().getDataFromMutoo( top_node );
    
    // fill arrays
    fill_map( MUIOO::South, accept_event_decision( MUIOO::South ) );
    fill_map( MUIOO::North, accept_event_decision( MUIOO::North ) );

  } catch(std::exception& e) {
    MUIOO::TRACE(e.what());
    return False;
  }
  
  _timer.get()->stop();
		
  return 0;

}

//_____________________________________________________________________________
void mMuiFastRoadFinder::set_interface_ptrs(PHCompositeNode* top_node)
{
  // module runtime parameters
  _mod_par = TMutNode<mMuiFastRoadFinderPar>::find_node(top_node,"mMuiFastRoadFinderPar");
  
  // interface map 
  _ll1_map = TMutNode<TMuiPseudoLL1Map>::find_node(top_node,"TMuiPseudoLL1Map");
  
  return;	
}

//_________________________________________________________________
void mMuiFastRoadFinder::fill_map(UShort_t arm, int accept) 
{
  // Take the output from emulator, fill pseudo-LL1 map.
  TMuiPseudoLL1Map::iterator ll1_iter = _ll1_map->insert_new(arm);
  int pseudoll1_bits = accept;
  ll1_iter->get()->set_emulator_decision(pseudoll1_bits);  
  return;

}

//_____________________________________________________________________________
int mMuiFastRoadFinder::accept_event_decision(UShort_t arm) 
{
  int accept = 0;

  // shallow decision
  if( ( arm == MUIOO::South && ll1_emulator().GL1_1Shal_S() ) || ( arm == MUIOO::North && ll1_emulator().GL1_1Shal_N() ) )
    { 
      _naccept[arm][mMuiFastRoadFinderPar::Shallow]++; 
      accept |= (1 << mMuiFastRoadFinderPar::Shallow);
    }
  
  // deep decision
  if( ( arm == MUIOO::South && ll1_emulator().GL1_1Deep_S() ) || ( arm == MUIOO::North && ll1_emulator().GL1_1Deep_N() ) )
    {

      _naccept[arm][mMuiFastRoadFinderPar::Deep]++; 
      accept |= (1 <<	mMuiFastRoadFinderPar::Deep);
    }
  
	
  // deep-shallow decision
  if( ( arm == MUIOO::South && ll1_emulator().GL1_1Deep1Shallow_S() ) || ( arm == MUIOO::North && ll1_emulator().GL1_1Deep1Shallow_N() ) )
    {
      _naccept[arm][mMuiFastRoadFinderPar::DeepShallow]++; 
      accept |= (1 <<	mMuiFastRoadFinderPar::DeepShallow);
    }
	
  if( ( arm == MUIOO::South && ll1_emulator().GL1_2Deep_S() ) || ( arm == MUIOO::North && ll1_emulator().GL1_2Deep_N() ) )
    {
      _naccept[arm][mMuiFastRoadFinderPar::DeepDeep]++; 
      accept |= (1 <<	mMuiFastRoadFinderPar::DeepDeep);
    }

  return accept;

}

//_____________________________________________________________________________
MuIDLl1& mMuiFastRoadFinder::ll1_emulator( void )
{
  if( !_ll1_emulator ) 
  {
    MUIOO::TRACE( "mMuiFastRoadFinder::ll1_emulator - creating new MuIDLl1 object. \n" );
    _ll1_emulator = new MuIDLl1();
    
    // disable use of hadron 1D trigger
    if( _mod_par )
    {
      _ll1_emulator->setOneHadronTrigger( _mod_par->get_use_hadron_trigger() );
    } else {
      
      MUIOO::TRACE( "mMuiFastRoadFinder::ll1_emulator - no parameters module found. \n" );
      MUIOO::TRACE( "mMuiFastRoadFinder::ll1_emulator - will use default. \n" );
      _ll1_emulator->setOneHadronTrigger( false );
      
    }
    
  }
  
  return *_ll1_emulator;
}

//_____________________________________________________________________________
void mMuiFastRoadFinder::print(std::ostream& os) const
{  
  MUIOO::PRINT( os, "mMuiFastRoadFinder::print" );
  for (UShort_t arm = 0; arm < MUIOO::MAX_ARM; arm++) {
    os << " Arm " << arm << " # accepted : " << endl;
    os << " Shallow     : " << _naccept[arm][mMuiFastRoadFinderPar::Shallow] << endl;
    os << " Deep        : " << _naccept[arm][mMuiFastRoadFinderPar::Deep] << endl;
    os << " DeepShallow : " << _naccept[arm][mMuiFastRoadFinderPar::DeepShallow] << endl;
    os << " DeepDeep    : " << _naccept[arm][mMuiFastRoadFinderPar::DeepDeep] << endl;
    os << endl;
  }

  MUIOO::PRINT( os, "**" );  
  return;

}
