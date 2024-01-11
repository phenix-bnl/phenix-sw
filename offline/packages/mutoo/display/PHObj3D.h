// $Id: PHObj3D.h,v 1.4 2007/04/11 18:31:25 hpereira Exp $
#ifndef _PHOBJ3D_H_
#define _PHOBJ3D_H_

/*!
   \file    PHObj3D.h
   \brief   generic 3D object
   \author  Hugo Pereira
   \version $Revision: 1.4 $
   \date    $Date: 2007/04/11 18:31:25 $
*/

#include <TROOT.h>
#include <TObject.h>
#include <TShape.h>
#include <TNode.h>
#include <TRotMatrix.h>
#include <PHPoint.h>
#include <list>
#include <iostream>

/*!
  \class PHObj3D
  \brief generic 3D object
*/

class PHObj3D {
  public:

  //! constructor
  PHObj3D( TNode *parent ):
    _parent( parent ),
    _is_valid( false ),
    _drawn( false ),
    _line_color( 1 ),
    _line_width( 1 )
  { }

  //! destructor
  virtual ~PHObj3D( void )
  { _clear_nodes(); }

  //! draw all nodes
  virtual void draw( void );

  //! hide all nodes
  virtual void hide( void );

  //! returns true if nodes are drawn
  virtual bool drawn( void ) const
  { return _drawn; }

  //! return true if object is valid
  bool is_valid( void ) const
  { return _is_valid; }

  //! changes line color
  void set_line_color( const unsigned int &value )
  { _line_color = value; }

  //! changes line width
  void set_line_width( const unsigned int &value )
  { _line_width = value; }

  //! print method
  virtual void print( std::ostream& out = std::cout ) const
  {}

  protected:

  /*!
    create node associated to two points
    is needed for TMutTracks3D, TMutCoord3D, TMutStub3D and TMutCoord3D
  */
  virtual void _make_segment(
    const PHPoint& first_point,
    const PHPoint& second_point,
    const double &radius );

  //! create nodes and shapes
  virtual void _make_nodes( void ) {};

  //! delete (clear) nodes and shapes
  virtual void _clear_nodes( void );

  //! sort PHPoint according to decreasing z
  struct point_less_ftor
  {
    bool operator() ( const PHPoint& p0, const PHPoint& p1 ) const
    { return p0.getZ() < p1.getZ(); }
  };

  //! parent node
  TNode* _parent;

  //! 3D objects node list
  std::list< TNode* > _nodes;

  //! 3D shapes list
  std::list< TShape* > _shapes;

  //! rotation matrices
  std::list< TRotMatrix* > _matrices;

  //! true if object initiailization is done
  bool _is_valid;

  //! true if object is drawn
  bool _drawn;

  //! objects line color
  unsigned int _line_color;

  //! objects line width
  unsigned int _line_width;

};

#endif
