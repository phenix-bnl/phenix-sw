// $Id: MuonPackPRDF.cxx,v 1.10 2011/07/14 22:27:10 pinkenbu Exp $

#include <recoConsts.h>
#include <MUIOO.h>

// MUTOO module.
#include <mMuiPackPRDF.h>
#include <mMutPackPRDF.h>
#include <mRpcPackPRDF.h>

// Include online runtime parameter.
#include<mMuiPackPRDFPar.h>
#include<mMutPackPRDFPar.h>

#include<PHTimer.h>
#include<PHTimeServer.h>

#include "MuonPackPRDF.h"

//______________________________________________________
MuonPackPRDF::MuonPackPRDF( const char* name ) :
	SubsysReco( name ),
  _timer( PHTimeServer::get()->insert_new(name) ),
  _do_mui( true ),
  _do_mut( true )
{}

//______________________________________________________
int MuonPackPRDF::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();

  // check if we should skip mui and mut parts
  if ( rc->get_IntFlag("MUID", 1) == 0) { 
    _do_mui = false;
    MUTOO::TRACE("MuonUnpackPRDF::InitRun - skipping MUID");
  }
  
  if ( rc->get_IntFlag("MUTR", 1) == 0) {
    _do_mut = false;
    MUTOO::TRACE("MuonUnpackPRDF::InitRun - skipping MUTR");
  }

  // Create Node Tree
  CreateNodeTree(topNode);
  
  return 0;
}

//__________________________________________________________________________
int MuonPackPRDF::CreateNodeTree(PHCompositeNode *top_node)
{
  // Instantiate nodes for mutoo containers
  //
  {
    PHNodeIterator nodeItr(top_node);
    mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
    if(!mutoo_node){
      mutoo_node = new PHCompositeNode("MUTOO");
      top_node->addNode(mutoo_node);
    }
  }

  // Instantiate nodes for muioo containers
  //
  {
    PHNodeIterator nodeItr(top_node);
    muioo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUIOO"));
    if(!muioo_node){
      muioo_node = new PHCompositeNode("MUIOO");
      top_node->addNode(muioo_node);
    }
  }

  // Module parameter tables
  mMuiPackPRDFPar* mui_pack_par = TMutNode<mMuiPackPRDFPar>::new_node(muioo_node,"mMuiPackPRDFPar");
  mMutPackPRDFPar* mut_pack_par = TMutNode<mMutPackPRDFPar>::new_node(mutoo_node,"mMutPackPRDFPar");
 
  // Default parameters are defined in parameter table constructor initialization list.
  // Change the default values here.
  mui_pack_par->set_verbosity( MUIOO::NONE );
  mut_pack_par->set_verbosity( MUTOO::NONE );
  
  return 0;
}

//________________________________________________________________________________________
int MuonPackPRDF::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();

  // Call MUTOO modules for track momentum reconstruction and vertex finding
  try {

    // Call mutoo/muioo PRDF packers
    mMuiPackPRDF_mod.event(top_node);
    mMutPackPRDF_mod.event(top_node);
    mRpcPackPRDF_mod.event(top_node);
            
  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }
    
  _timer.get()->stop();

  return 0;

}


//____________________________________________________________
int MuonPackPRDF::End(PHCompositeNode* top_node) 
{
//   _timer.get()->print_stat();
  return 0;
}
