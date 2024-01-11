// $Id: RpcStationIOArray.h,v 1.3 2008/08/28 00:48:31 kempel Exp $

#ifndef _RpcStationIOArray_h_
#define _RpcStationIOArray_h_

/*!
	\file RpcStationIOArray.h
	\brief simple container for TClonesArray
	\author Hugo Pereira da costa
	\version $Revision: 1.3 $
	\date    $Date: 2008/08/28 00:48:31 $
*/

#include <PHObject.h>
#include <TClonesArray.h>
#include "RPCGEOM.h"

//! simple container for station TClonesArray
class RpcStationIOArray: public PHObject
{
	public:
	
	//! constructor
	RpcStationIOArray( TClonesArray* array_ptr = 0 ):
			_array_ptr(array_ptr)
	{}	
	
	//! destructor
  /*! Do nothing. Internal array has external ownership */
	virtual ~RpcStationIOArray( void )
	{}
  
  //! This class is for IO only.
  virtual TClonesArray* get_array() 
	{ return _array_ptr; }

  //! Set the array array pointer
  virtual void set_array(TClonesArray* array_ptr) 
	{ _array_ptr = array_ptr;}
  
	//! dumper
  virtual void print(std::ostream& os = std::cout) const 
	{
    RPCGEOM::TRACE("PHIOArray_v1::print()");
    if(_array_ptr) {
      for(int i = 0; i<_array_ptr->GetEntries(); ++i)
			_array_ptr->At(i)->Print();
    }
  }
	
	private:
			
	//! pointer to TClonesArray	
  // *NOTE* || below instructs rootcint not split array
	TClonesArray* _array_ptr;	//||

	ClassDef( RpcStationIOArray, 1 )
	
};

#endif
