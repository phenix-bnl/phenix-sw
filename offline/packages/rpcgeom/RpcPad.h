// $Id: RpcPad.h,v 1.2 2005/09/15 15:42:33 hpereira Exp $

#ifndef _RpcPad_h_
#define _RpcPad_h_

/*!
	\file RpcPad.h
	\brief RPC pad base class.
	only the pad center is accessible directly. Needs to cast to type1 or type2 pad to access pad size
	\author Hugo Pereira da costa
	\version $Revision: 1.2 $
	\date    $Date: 2005/09/15 15:42:33 $
*/

#include <PHPoint.h>
#include <TObject.h>
#include <iostream>

//! RPC pad base class.
/*!
	RPC pad base class.
	only the pad center is accessible directly. Needs to cast to type1 or type2 pad to access pad size
*/
class RpcPad: public TObject
{
	
	public:
	
	//! constructor
	RpcPad( void ):
		_x(0),
		_y(0),
		_z(0)
	{}
					
	//! destructor
	virtual ~RpcPad( void )
	{}
	
	//! pad center
	virtual PHPoint get_center( void ) const
	{ return PHPoint( _x, _y, _z ); }
	
	//! pad center
	virtual void set_center( const PHPoint& point )
	{ 
		_x = point.getX(); 
		_y = point.getY();
		_z = point.getZ();
	}	
	
	//! print
	virtual void print( std::ostream& out = std::cout ) const
	{
		out 
				<< "RpcPad::print - position: (" 
				<< _x << ","  << _y << "," << _z << ")" 
				<< std::endl; 
	}
			
	protected:
	
	//! x 
	Float_t _x;
	
	//! y
	Float_t _y;
	
	//! z
	Float_t _z;
	
	ClassDef( RpcPad, 1 )
	
};

#endif
