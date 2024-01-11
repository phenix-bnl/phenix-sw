// $Id: FvtxPackPRDF.cxx,v 1.2 2015/07/02 17:43:40 jinhuang Exp $

/*!
  \file    FvtxPackPRDF.cxx
  \ingroup supermodules
  \brief   reads fvtx hit maps from fvtxoo nodes, packs them to a PRDF
  \author  Zhengyun You
  \version $Revision: 1.2 $
  \date    $Date: 2015/07/02 17:43:40 $
*/

#include <recoConsts.h>
#include <FVTXOO.h>

// FVTXOO module.
#include <mFvtxPackPRDF.h>

// Include online runtime parameter.
#include<mFvtxPackPRDFPar.h>

#include<PHTimer.h>
#include<PHTimeServer.h>

#include "FvtxPackPRDF.h"

//______________________________________________________
FvtxPackPRDF::FvtxPackPRDF( const char* name ) :
	SubsysReco( name ),
  _timer( PHTimeServer::get()->insert_new(name) ),
  _do_fvtx( true )
{}

//______________________________________________________
int FvtxPackPRDF::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();

  if ( rc->get_IntFlag("FVTX", 1) == 0) {
    _do_fvtx = false;
    FVTXOO::TRACE("FvtxPackPRDF::InitRun - skipping FVTX");
  }

  // Create Node Tree
  CreateNodeTree(topNode);
  
  return 0;
}

//__________________________________________________________________________
int FvtxPackPRDF::CreateNodeTree(PHCompositeNode *top_node)
{
  // Instantiate nodes for fvtxoo containers
  //
  {
    PHNodeIterator nodeItr(top_node);
    fvtxoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "FVTXOO"));
    if(!fvtxoo_node){
      fvtxoo_node = new PHCompositeNode("FVTXOO");
      top_node->addNode(fvtxoo_node);
    }
  }

  // Module parameter tables
  mFvtxPackPRDFPar* fvtx_pack_par = TMutNode<mFvtxPackPRDFPar>::new_node(fvtxoo_node,"mFvtxPackPRDFPar");
 
  // Default parameters are defined in parameter table constructor initialization list.
  // Change the default values here.
  fvtx_pack_par->set_verbosity( FVTXOO::NONE );
  
  return 0;
}

//________________________________________________________________________________________
int FvtxPackPRDF::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();

  // Call FVTXOO modules for track momentum reconstruction and vertex finding
  try {

    // Call fvtxoo PRDF packers
    mFvtxPackPRDF_mod.event(top_node);
            
  } catch (std::exception& e) { FVTXOO::TRACE(e.what()); }

  _timer.get()->stop();

  return 0;

}


//____________________________________________________________
int FvtxPackPRDF::End(PHCompositeNode* top_node) 
{
  _timer.get()->print_stat();
  return 0;
}
