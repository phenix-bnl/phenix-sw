// $Id: TFvtxMCStrip.h,v 1.2 2011/12/01 04:16:20 slash Exp $

#ifndef _TFvtxMCStrip_h_
#define _TFvtxMCStrip_h_

/*!
	\file TFvtxMCStrip.h
	\brief fired Rpc pad
	\author H. Pereira Da Costa
	\version $Revision: 1.2 $
	\date $Date: 2011/12/01 04:16:20 $
*/

#include <TObject.h>
#include <iostream>

/*! @ingroup interface */
//! fired FVTX strip. Stores the charge deposed by a particle on a single strip
class TFvtxMCStrip : public TObject
{
	
	public:

	//! constructor
	TFvtxMCStrip()
	{}		

	//! constructor
	TFvtxMCStrip( const unsigned short& strip, const Float_t& q )
	{}		
	
	//! destructor
	virtual ~TFvtxMCStrip( void )
	{}

	//! pad index
	virtual unsigned short get_strip() const 
	{return 0;}
	
	//! pad index
	virtual void set_strip( const unsigned short& ) 
	{}
	
	//! charge
	virtual Float_t get_q() const
	{ return 0; }
	
	//! charge
	virtual void set_q( const Float_t & ) 
	{}
	
	//! print
	virtual void print(std::ostream& os = std::cout) const 
	{}	
			
	//! ROOT dictionary
	ClassDef( TFvtxMCStrip, 1 );
			
};

#endif
