// $Id: VtxSimreco.C,v 1.6 2015/06/24 03:25:44 slash Exp $

/*! 
  \file VtxSimreco.h
  \brief get simulated vertex and store into BBC node
  \version $Revision: 1.6 $
  \date $Date: 2015/06/24 03:25:44 $
*/

#include "VtxSimreco.h"
#include "SIMRECO.h"

#include <headerWrapper.h>
#include <getClass.h>

#include <Fun4AllReturnCodes.h>
#include <BbcOutv1.h>
#include <VtxOrdercodes.h>
#include <VtxOut.h>
#include <PHPoint.h>

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#include <TH1.h>

#include <cassert>
#include <iostream>


using namespace std;

//______________________________________________________
VtxSimreco::VtxSimreco(const std::string &name): 
  VtxReco(name),
  _vtx_source( PISA ),
  _smear_x( true ),
  _smear_y( true ),
  _smear_z( true ),
  _use_vtxout_error( false ),
  vtxout( NULL ),
  _smear_z_dist( false ),
  _use_xy( false ),
  _overwrite_bbc( true ),
  _fixed_vtx( 3, 0 ),
  _vtx_sigma( 3, 0 ),
  rng( gsl_rng_alloc (gsl_rng_mt19937) )
{ 
  
  // initialize gsl random seed
  gsl_rng_set(rng, 1234); 
  
  // assign default values for the vertex errors
  // X and Y smearing are set to 1 for consistancy with TMutExtVtx old interface
  // x and y values are ignored by default (meaning: set to 0)
  // but these sigma are used as default errors in muon vertex fit.
  _vtx_sigma[0] = 1;
  _vtx_sigma[1] = 1;
  
  // Z smearing is set to 0 for consistancy with VtxSimReco old interface
  // remark: this is different from the old TMutExtVtx interface, for which the error
  // was set to 2 (corresponding to the p+p case) by default.
  _vtx_sigma[2] = 0;
  
}

//______________________________________________________
VtxSimreco::~VtxSimreco( void )
{ gsl_rng_free( rng ); }

//______________________________________________________
int VtxSimreco::Init(PHCompositeNode *top_node)
{
  
  SIMRECO::PRINT( cout, "VtxSimreco::Init" );
  cout << "_vtx_source = " << source_name() << endl;	
  
  // fixed vertex
  if( _vtx_source == FIXED )
  {
    cout << "_fixed_vtx = (" 
      << _fixed_vtx[0] << ","
      << _fixed_vtx[1] << ","
      << _fixed_vtx[2] << ")"
      << endl;	
  }

  // vertex error and smearing
  cout << "_vtx_sigma = (" 
    << _vtx_sigma[0] << ","
    << _vtx_sigma[1] << ","
    << _vtx_sigma[2] << ")"
    << endl;	
  
  // smearing configuration
  cout << "_use_xy = " << ( (_use_xy) ? "true":"false" ) << endl;	
  if( _use_xy )
  {
    cout << "_smear_x = " << ( (_smear_x) ? "true":"false" ) << endl;	
    cout << "_smear_y = " << ( (_smear_y) ? "true":"false" ) << endl;	
  }
  cout << "_smear_z = " << ( (_smear_z) ? "true":"false" ) << endl;	
  
  cout << "_overwrite_bbc = " << ( (_overwrite_bbc) ? "true":"false" ) << endl;	

  // dump errors
  cout << "_vtx_sigma = ("	
    << _vtx_sigma[0] << ","
    << _vtx_sigma[1] << ","
    << _vtx_sigma[2] << ")"
    << endl;

  // pisa header node name
  if( !_pisa_header_node.empty() ) cout << "_pisa_header_node: " << _pisa_header_node << endl;
  
  SIMRECO::PRINT( cout, "**" );

  return EVENT_OK;
  
}

//______________________________________________________
int VtxSimreco::InitRun(PHCompositeNode *top_node)
{
  
  // initRun does nothing specific on top of VtxReco for now.
  cout << "VtxSimreco::InitRun." << endl;
  
  // it is important to call the base class method to make 
  // sure that the VtxOut node is properly created, with correct
  // version number
  int return_code = VtxReco::InitRun( top_node );
  if( return_code != EVENT_OK ) return return_code;
  
  // BBCOut node
  if( _overwrite_bbc )
  {
    
    BbcOut* bbcout = findNode::getClass<BbcOut>(top_node, "BbcOut");
    if (!bbcout)
    {
      
      cout << "VtxSimReco::InitRun - creating new BbcOut node." << endl;
      
      PHNodeIterator iter(top_node);
      PHCompositeNode *dst_node;
      dst_node = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
      
      bbcout = new BbcOutv1();
      PHIODataNode <BbcOut>* BbcOutNode = new PHIODataNode <BbcOut>(bbcout, "BbcOut", "PHObject");
      dst_node->addNode(BbcOutNode);
    }
  
  }
  
  return EVENT_OK;
  
}

//______________________________________________________
int VtxSimreco::process_event(PHCompositeNode *top_node)
{

  // store in VtxOut node
  vtxout = findNode::getClass<VtxOut>(top_node, "VtxOut");
  if( !vtxout )
  { 
    cout << "VtxSimreco::process_event - no VtxOut node found." << endl;
    return DISCARDEVENT;
  }
  
  if( _smear_z_dist )
    {
      _vtx_sigma[2] = _zvtx_sigma_histo->GetRandom();
    }
  
  // prepare vertex
  _current_vtx = _fixed_vtx;
  _current_vtx_error = _vtx_sigma;
  
  
 
  switch (_vtx_source)
  {
    case PISA:
    get_mc_vertex( top_node );
    break;

    case FIXED:
    _current_vtx = _fixed_vtx;
    break;

    default:
    cout << "VtxSimreco::process_event - unrecognized vertex selection mode: " << _vtx_source << endl;
    exit(1);
    break;
  }
  
  // apply smearing
  smear_vertex();
    
  vtxout->AddVtx( "SIM", &_current_vtx[0], &_current_vtx_error[0], VTX::SIMORDER );

  // overwrite bbcout node contents
  if( _overwrite_bbc )
  {
    BbcOut *bbcout = findNode::getClass<BbcOut>(top_node, "BbcOut");
    bbcout->set_Vertex(_current_vtx[2], _current_vtx_error[2] );
  }
  
  return EVENT_OK;
  
}

//______________________________________________________
bool VtxSimreco::get_mc_vertex(PHCompositeNode *top_node)
{
      
  // retrieve header wrapper
  headerWrapper* header(0);
  
  if( !_pisa_header_node.empty() )
  {
 
    // node iterator
    PHNodeIterator node_iter(top_node);
   
    // retrieve parent node
    PHCompositeNode* header_node = static_cast<PHCompositeNode*>(node_iter.findFirst("PHCompositeNode", "SIGNAL"));
    
    // retrieve header node from there if valid, or throw error message
    if( header_node ) header = findNode::getClass<headerWrapper>(header_node, "header");
    else {
      cout << "VtxSimreco::get_mc_vertex - unable to load pisa \"header\" node from \""<< _pisa_header_node << "\"." << endl;
      return false;
    }
    
  } else {
    
    // retrieve header node directly from top node
    header = findNode::getClass<headerWrapper>(top_node, "header");
    
  }
  
  // check header and load data
  HEADER_ST* header_table(0);
  if( header && ( header_table	= header->TableData() ) ) 
  {
    
    // store vtx z
    _current_vtx[2] = header_table[0].vertex[2];      
  
    // stores vtx x and y if required
    if( _use_xy ) {
      _current_vtx[0] = header_table[0].vertex[0];      
      _current_vtx[1] = header_table[0].vertex[1];      
    }
            
    return true;

  } else {
    
    cout << "VtxSimreco::get_mc_vertex - unable to load vertex from pisa \"header\" node." << endl;
    return false;
    
  }

}

//______________________________________________________
void VtxSimreco::smear_vertex( void )
{

  float vtx_smear[3] = {_vtx_sigma[0], _vtx_sigma[1], _vtx_sigma[2]};

  if ( _use_vtxout_error )
    {
      PHPoint vtxerror = PHPoint(-999,-999,-999);
      if (!isnan(vtxout->get_VertexError("FVTX").getZ()))
	  vtxerror = vtxout->get_VertexError("FVTX");
      else if (!isnan(vtxout->get_VertexError("SVX_PRECISE").getZ()))
	vtxerror = vtxout->get_VertexError("SVX_PRECISE");
      else if (!isnan(vtxout->get_VertexError("BBC").getZ()))
	vtxerror = vtxout->get_VertexError("BBC");
      
      if (vtxerror.getZ()!=-999)
	{
	  vtx_smear[0] = vtxerror.getX();
	  vtx_smear[1] = vtxerror.getY();
	  vtx_smear[2] = vtxerror.getZ();

	  _current_vtx_error[0] = vtxerror.getX();
	  _current_vtx_error[1] = vtxerror.getY();
	  _current_vtx_error[2] = vtxerror.getZ();
	}
    }
  // smear z
  if( _smear_z && _vtx_sigma[2] > 0 ) _current_vtx[2] += gsl_ran_gaussian(rng, vtx_smear[2] );

  // smear x and y if required
  if( _use_xy )
  {
    if( _smear_x && _vtx_sigma[0] > 0 ) _current_vtx[0] += gsl_ran_gaussian(rng, vtx_smear[0] );
    if( _smear_y && _vtx_sigma[1] > 0 ) _current_vtx[1] += gsl_ran_gaussian(rng, vtx_smear[1] );
  }
  
  return;
      
}
