// $Id: TRpcMCPad.h,v 1.2 2006/05/29 04:43:54 hpereira Exp $

#ifndef _TRpcMCPad_h_
#define _TRpcMCPad_h_

/*!
	\file TRpcMCPad.h
	\brief fired Rpc pad
	\author H. Pereira Da Costa
	\version $Revision: 1.2 $
	\date $Date: 2006/05/29 04:43:54 $
*/

#include <TObject.h>
#include <iostream>

/*! @ingroup interface */
//! fired Rpc pad. Stores the charge deposed by a particle on a single pad
class TRpcMCPad : public TObject
{
	
	public:

	//! constructor
	TRpcMCPad()
	{}		

	//! constructor
	TRpcMCPad( UShort_t pad, Float_t q )
	{}		
	
	//! destructor
	virtual ~TRpcMCPad( void )
	{}

	//! pad index
	virtual UShort_t get_pad() const 
	{return 0;}
	
	//! pad index
	virtual void set_pad( UShort_t pad ) 
	{}
	
	//! charge
	virtual Float_t get_q() const
	{ return 0; }
	
	//! charge
	virtual void set_q( Float_t q ) 
	{}
	
	//! print
	virtual void print(std::ostream& os = std::cout) const 
	{}	
			
	//! ROOT dictionary
	ClassDef( TRpcMCPad, 1 );
			
};

#endif
