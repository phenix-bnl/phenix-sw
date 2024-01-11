// $Id: TRpcMCPad_v1.h,v 1.2 2006/05/29 04:43:54 hpereira Exp $

#ifndef _TRpcMCPad_v1_h_
#define _TRpcMCPad_v1_h_

/*!
	\file TRpcMCPad_v1.h
	\brief fired Rpc pad
	\author H. Pereira Da Costa
	\version $Revision: 1.2 $
	\date $Date: 2006/05/29 04:43:54 $
*/

#include <TObject.h>
#include <iostream>

#include "TRpcMCPad.h"

/*! @ingroup interface */
//! fired Rpc pad. Stores the charge deposed by a particle on a single pad
class TRpcMCPad_v1 : public TRpcMCPad
{
	
	public:

	//! constructor
	TRpcMCPad_v1():
			_pad( 0 ),
			_q( 0 )
	{}		

	//! constructor
	TRpcMCPad_v1( UShort_t pad, Float_t q ):
			_pad( pad ),
			_q( q )
	{}		

	//! constructor
	TRpcMCPad_v1( const TRpcMCPad *ptr ):
			TRpcMCPad( *ptr ),
			_pad( ptr->get_pad() ),
			_q( ptr->get_q() )
	{}		

	//! constructor
	TRpcMCPad_v1( const TRpcMCPad& ref ):
			TRpcMCPad( ref ),
			_pad( ref.get_pad() ),
			_q( ref.get_q() )
	{}		
	
	//! destructor
	virtual ~TRpcMCPad_v1( void )
	{}

	//! pad index
	virtual UShort_t get_pad() const 
	{return _pad;}
	
	//! pad index
	virtual void set_pad( UShort_t pad ) 
	{ _pad = pad; }
	
	//! charge
	virtual Float_t get_q() const
	{ return _q; }
	
	//! charge
	virtual void set_q( Float_t q ) 
	{ _q = q; }
	
	//! print
	virtual void print(std::ostream& os = std::cout) const 
	{ os << " pad: " << _pad << " q: " << _q << std::endl; }
	
	private:
	
	//! pad id			
	UShort_t _pad;
	
	//! charge deposit
	Float_t _q;
	
	//! ROOT dictionary
	ClassDef( TRpcMCPad_v1, 1 );
			
};

#endif
