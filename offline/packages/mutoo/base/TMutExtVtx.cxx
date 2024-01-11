// $Id: TMutExtVtx.cxx,v 1.14 2014/02/08 19:48:32 slash Exp $
//////////////////////////////////////////////////////////////////
/*! 
  \file TMutExtVtx.cxx
  \brief external vertex singleton
  \author	Hugo Pereira
  \version $Revision: 1.14 $
  \date		$Date: 2014/02/08 19:48:32 $
*/
//////////////////////////////////////////////////////////////////

#include <VtxOut.h>

#include "TMutExtVtx.h"

// MUTOO
#include "MUTOO.h"
#include "PHException.h"
#include "TMutNode.h"


using namespace std;

//______________________________________________________
TMutExtVtx::TMutExtVtx( void ):
  _error( false ),
  _parent_node_name("")
{
  vtx_prio.push_back("FVTX");
  vtx_prio.push_back("SVX_PRECISE");
  vtx_prio.push_back("BBC");
}

//_______________________________________
bool TMutExtVtx::load_vtx( PHCompositeNode* top_node )
{
  
  _error = true;
  
  // reset vertex
  _vtx = default_vertex();
  
  try
  {

    PHCompositeNode* parent_node = top_node;
    if( !_parent_node_name.empty() )
    {
      
      PHNodeIterator nodeItr(top_node);
      parent_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", _parent_node_name.c_str() ) );
      if( !parent_node ) 
      {
        ostringstream what;
        what << "TMutExtVtx::load_vtx - unable to find node named " << _parent_node_name << endl;
        throw std::runtime_error( what.str() );
      }
    
    }
    
    // retrieve VtxOut object
    VtxOut* vtx_out( TMutNode<VtxOut>::find_io_node( parent_node, "VtxOut" ) );
    string vtxname = "";
    for (size_t iprio=0; iprio<vtx_prio.size(); iprio++)
      {
	float Xvtx = vtx_out->get_Vertex(vtx_prio[iprio].c_str()).getX();
	float Yvtx = vtx_out->get_Vertex(vtx_prio[iprio].c_str()).getY();
	float Zvtx = vtx_out->get_Vertex(vtx_prio[iprio].c_str()).getZ();
	if ((Xvtx>-200 && Xvtx<200) && (Yvtx>-200 && Yvtx<200) && (Zvtx>-200 && Zvtx<200))
	  {
	    _vtx.first = vtx_out->get_Vertex(vtx_prio[iprio].c_str());
	    _vtx.second = vtx_out->get_VertexError(vtx_prio[iprio].c_str());
	    vtxname = vtx_prio[iprio];
	    _error = false;
	    break;
	  }
      } 

    // print information
    if( _verbosity >= MUTOO::SOME )
    {
      cout 
        << "TMutExtVtx::load_vtx -"
        << " vertex used: " << vtxname
        << endl;
    }
    
  } catch (std::exception& e) {
    
    static int counts = 0;
    
    if( counts < 10 )
    {
      MUTOO::TRACE( e.what() );
      cout << "TMutExtVtx::load_vtx - failed.\n";    
      counts++;
      if( counts == 10 ) cout << "TMutExtVtx::load_vtx - message disabled." << endl;
    }
    
    
    _error = true;
  
  }  
      
  // print information
  if( _verbosity >= MUTOO::SOME )
  {
    cout 
      << "TMutExtVtx::load_vtx -"
      << " position: ("
      << _vtx.first.getX() << ","
      << _vtx.first.getY() << "," 
      << _vtx.first.getZ() << ")" 
      << " +- ("
      << _vtx.second.getX() << ","
      << _vtx.second.getY() << ","
      << _vtx.second.getZ() << ") [cm] "
      << "error: " << _error
      << endl;
  }

  return _error;

}

//__________________________________________________________
TMutExtVtx::VertexPair TMutExtVtx::default_vertex( void ) const
{
  /* 
  default vertex is at 0,0
  errors along x and y are 1cm
  error along z is 30cm 
  (this is the canonical width of Gaussian vertex distribution)
  */
  return make_pair( PHPoint(0,0,0), PHPoint( 1, 1, 30.0 ) );
}
