// $Id: TRpcMCStrip_v1.h,v 1.1 2008/08/28 00:50:22 kempel Exp $

#ifndef _TRpcMCStrip_v1_h_
#define _TRpcMCStrip_v1_h_

/*!
	\file TRpcMCStrip_v1.h
	\brief fired Rpc strip
	\author H. Pereira Da Costa
	\version $Revision: 1.1 $
	\date $Date: 2008/08/28 00:50:22 $
*/

#include <TObject.h>
#include <iostream>

#include "TRpcMCStrip.h"

/*! @ingroup interface */
//! fired Rpc strip. Stores the charge deposed by a particle on a single strip
class TRpcMCStrip_v1 : public TRpcMCStrip
{
	
	public:

	//! constructor
	TRpcMCStrip_v1():
			_stripid( 0 ),
			_q( 0 )
	{}		

	//! constructor
	TRpcMCStrip_v1( UShort_t stripid, Float_t q ):
			_stripid( stripid ),
			_q( q )
	{}		

	//! constructor
	TRpcMCStrip_v1( const TRpcMCStrip *ptr ):
			TRpcMCStrip( *ptr ),
			_stripid( ptr->get_stripid() ),
			_q( ptr->get_q() )
	{}		

	//! constructor
	TRpcMCStrip_v1( const TRpcMCStrip& ref ):
			TRpcMCStrip( ref ),
			_stripid( ref.get_stripid() ),
			_q( ref.get_q() )
	{}		
	
	//! destructor
	virtual ~TRpcMCStrip_v1( void )
	{}
	
	//! strip index
	virtual void set_stripid( UShort_t stripid ) 
	{ _stripid = stripid; }
	
	//! charge
	virtual void set_q( Float_t q ) 
	{ _q = q; }

	//! strip index
	virtual UShort_t get_stripid() const 
	{return _stripid;}
		
	//! charge
	virtual Float_t get_q() const
	{ return _q; }

	//! print
	virtual void print(std::ostream& os = std::cout) const 
	{ os << " stripid: " << _stripid << " q: " << _q << std::endl; }
	
	private:

	//! strip id			
	UShort_t _stripid;
	
	//! charge deposit
	Float_t _q;
	
	//! ROOT dictionary
	ClassDef( TRpcMCStrip_v1, 1 );
			
};

#endif
