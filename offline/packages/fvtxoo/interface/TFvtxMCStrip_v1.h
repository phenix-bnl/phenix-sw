// $Id: TFvtxMCStrip_v1.h,v 1.2 2011/12/01 04:16:20 slash Exp $

#ifndef _TFvtxMCStrip_v1_h_
#define _TFvtxMCStrip_v1_h_

/*!
	\file TFvtxMCStrip_v1.h
	\brief fired FVTX MC strip
	\author H. Pereira Da Costa
	\version $Revision: 1.2 $
	\date $Date: 2011/12/01 04:16:20 $
*/

#include "TFvtxMCStrip.h"

/*! @ingroup interface */
//! fired FVTX strip. Stores the charge deposed by a particle on a single strip
class TFvtxMCStrip_v1 : public TFvtxMCStrip
{
	
	public:

	//! constructor
	TFvtxMCStrip_v1():
		TFvtxMCStrip(),
		_strip(0),
		_q(0)
	{}		

	//! constructor
	TFvtxMCStrip_v1( const unsigned short& strip, const Float_t& q ):
		TFvtxMCStrip( strip, q ),
		_strip(strip),
		_q(q)
	{}		
	
	//! destructor
	virtual ~TFvtxMCStrip_v1( void )
	{}

	//! pad index
	virtual unsigned short get_strip() const 
	{return _strip;}
	
	//! pad index
	virtual void set_strip( const unsigned short& strip ) 
	{ _strip = strip; }
	
	//! charge
	virtual Float_t get_q() const
	{ return _q; }
	
	//! charge
	virtual void set_q( const Float_t &q ) 
	{ _q = q; }
	
	//! print
	virtual void print(std::ostream& os = std::cout) const 
	{ os << " strip: " << _strip << " q: " << _q << std::endl; }
	
	private:
	
	//! pad id			
	unsigned short _strip;
	
	//! charge deposit
	Float_t _q;
			
	//! ROOT dictionary
	ClassDef( TFvtxMCStrip_v1, 1 );
			
};

#endif
