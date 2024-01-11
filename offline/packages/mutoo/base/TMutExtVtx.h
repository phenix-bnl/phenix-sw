// $Id: TMutExtVtx.h,v 1.18 2014/02/08 19:48:32 slash Exp $

//////////////////////////////////////////////////////////////////
/*! 
  \file TMutExtVtx.h
  \brief external vertex singleton
  \author Hugo Pereira
  \version $Revision: 1.18 $
  \date $Date: 2014/02/08 19:48:32 $
*/
//////////////////////////////////////////////////////////////////

#ifndef __TMUTEXTVTX_H__
#define __TMUTEXTVTX_H__

#include <MUTOO.h>
#include <PHCompositeNode.h>
#include <PHPoint.h>

#include <map>
#include <vector>

#ifndef __CINT__
#include "PHGslRng.h"
#endif

/*! 
  \class TMutExtVtx.h
  \brief external vertex singleton
*/
class TMutExtVtx 
{

  public:
  
  //! returns TMutExtVtx singleton
  static TMutExtVtx& get( void )
  {
    static TMutExtVtx _ext_vtx;
    return _ext_vtx;
  }
  
  //! returns external vertex 
  PHPoint get_vtx( bool &error )
  { 
    error = _error;
    return _vtx.first; 
  }
  
  //! returns external vertex error
  /*! 
    returns external vertex error
    taken either for MC, BBC or default (0,0,0)
  */
  PHPoint get_vtx_error( void )
  { return _vtx.second; }

  //! load vertex
  /*! use VtxOut to get the vertex */
  bool load_vtx( PHCompositeNode *top_node );

  //! Verbosity level
  void set_verbosity(MUTOO::Verbosity verbosity) 
  {_verbosity = (unsigned short) verbosity;}
  
  //! Verbosity level
  MUTOO::Verbosity get_verbosity() const 
  { return (MUTOO::Verbosity) _verbosity; }	 

  //! set parent node name
  void set_parent_node_name( const char* name )
  { 
    if( name ) 
    {
      _parent_node_name = name;
      std::cout << "TMutExtVtx::set_parent_node_name - will try read VtxOut object from node named " << _parent_node_name << std::endl;
    }
  }

  void set_priorities(std::string vtx_source, size_t prio)
  {
    if (prio>=vtx_prio.size())
      std::cout << "priority larger than number of vertex sources available." << std::endl;
    vtx_prio[prio] = vtx_source;
  }

  private:
  
  //! constructor
  TMutExtVtx( void );
    
  // typedef for vertex point and error
  typedef std::pair< PHPoint, PHPoint > VertexPair;
 
  //! default vertex
  VertexPair default_vertex( void ) const;

  //! current vertex
  VertexPair _vtx;
  
  //! ext vertex verbosity
  unsigned short _verbosity;
   
  //! true if loading of the vertex failed
  bool _error;
  
  //! name of the node from which the VtxOut is to be retrieved.
  /*! the default is "" (empty), meaning that the first encountered VtxOut node found from 
  top node is used. Non default values are usefull when running, say, embedding, from which you
  might have a VtxOut node in both the signal, and the background input, and are only interested 
  in the one that comes from the 'signal' node */
  std::string _parent_node_name;
    
  //!set priority for vertexes
  std::vector<std::string> vtx_prio;
};
#endif
