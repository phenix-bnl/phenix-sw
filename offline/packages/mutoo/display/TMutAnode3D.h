// $Id: TMutAnode3D.h,v 1.2 2008/12/02 00:45:05 hpereira Exp $
#ifndef TMutAnode3D_h
#define TMutAnode3D_h

/*!
\file TMutAnode3D.h
\brief 3D object for mutr anodes
\author Hugo Pereira
\version $Revision: 1.2 $
\date $Date: 2008/12/02 00:45:05 $
*/

#include <iostream>
#include "TMutIndex.h"
#include "PHObj3D.h"

class MutWire;
class MutOctant;
class MutGap;

/*! \ingroup display */
/*!
\class TMutAnode3D
\brief 3D object for mutr coordinates
*/

class TMutAnode3D:public PHObj3D
{
  
  public:
  
  //! constructor
  TMutAnode3D(  const TMutIndex& index, const unsigned int& card_id, TNode* parent );
  
  //! equal operator
  bool operator == (const TMutAnode3D& obj ) const
  { return _index == obj._index && _card_id == obj._card_id; }
  
  //! print geometry information
  void print( std::ostream& out = std::cout ) const;
  
  private:
  
  //! create nodes and shapes
  void _make_nodes( void );
  
  //! location
  TMutIndex _index;
  
  //! wire index
  unsigned int _card_id;
  
  //! object name
  std::string _name;
  
  //! pointer to mutr octant geom structure
  MutOctant* _octant_ptr;
  
  //! pointer to gap geometry
  MutGap* _gap_ptr;
  
  //! inner radius (calculated in constructor)
  double _r_inner;
  
  //! outer radius (calculated in constructor)
  double _r_outer;
  
};

#endif
