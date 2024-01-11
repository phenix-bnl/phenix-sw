// $Id: TRpcMCStrip.h,v 1.1 2008/08/28 00:50:22 kempel Exp $

#ifndef _TRpcMCStrip_h_
#define _TRpcMCStrip_h_

/*!
	\file TRpcMCStrip.h
	\brief fired Rpc strip
	\author H. Pereira Da Costa
	\version $Revision: 1.1 $
	\date $Date: 2008/08/28 00:50:22 $
*/

#include <TObject.h>
#include <iostream>

/*! @ingroup interface */
//! fired Rpc strip. Stores the charge deposed by a particle on a single strip
class TRpcMCStrip : public TObject
{
	
	public:

	//! constructor
	TRpcMCStrip()
	{}		

	//! constructor
	TRpcMCStrip( UShort_t stripid, Float_t q )
	{}		
	
	//! destructor
	virtual ~TRpcMCStrip( void )
	{}
	
	//! strip index
	virtual void set_stripid( UShort_t stripid ) 
	{}
		
	//! charge
	virtual void set_q( Float_t q ) 
	{}

	//! strip index
	virtual UShort_t get_stripid() const 
	{return 0;}

	//! charge
	virtual Float_t get_q() const
	{ return 0; }
	
	//! print
	virtual void print(std::ostream& os = std::cout) const 
	{}	
			
	//! ROOT dictionary
	ClassDef( TRpcMCStrip, 1 );
			
};

#endif
