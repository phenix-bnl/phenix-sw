// $Id: RpcArm_proto.h,v 1.1 2009/04/03 18:40:39 richi Exp $

#ifndef _RpcArm_proto_h_
#define _RpcArm_proto_h_

/*!
  \file RpcArm_proto.h
  \brief RPC Arm geometry
  Initialize and provide access to RPC stations
  \author Hugo Pereira da costa
  \version $Revision: 1.1 $
  \date $Date: 2009/04/03 18:40:39 $
*/

#include <iostream>
#include <stdexcept>

#include "RPCPROTOGEOM.h"
#include "RpcStation_proto.h"
#include <RpcArm.h>

class RpcArm;

//! RPC Arm geometry.
/*! 
  Initialize and provide access to RPC stations 
*/
class RpcArm_proto: public RpcArm
{
	
 public:
			
  //! constructor
  RpcArm_proto( unsigned int index = 0 ):
    _index( index ),
    _station1( 0 ),
    _station2( 0 ),
    _station3( 0 )
    {}
	
  //! destructor
  virtual ~RpcArm_proto()
    {
      if( _station1 ) delete _station1;
      if( _station2 ) delete _station2;
      if( _station3 ) delete _station3;
    }
	
  //! @name accessors
  //@{
	
  //! station from index
  virtual RpcStation* get_station( unsigned int index );
			
  //! station1
  virtual RpcStation* station1( void ) const
    { 
      if( !_station1 ){
	RPCPROTOGEOM::TRACE("RpcArm_proto::station1 - not initialized");
	exit(1);
      }
      return _station1; 
    }
			
  //! station2
  virtual RpcStation* station2( void ) const
    { 
      if( !_station2 ){
	RPCPROTOGEOM::TRACE("RpcArm_proto::station1 - not initialized");
	exit(1);
      }
      return _station2; 
    }

  //! station3
  virtual RpcStation* station3( void ) const
    { 
      if( !_station3 ){
	RPCPROTOGEOM::TRACE("RpcArm_proto::station1 - not initialized");
	exit(1);
      }
      return _station3; 
    }
			
  //! station1
  virtual void set_station1( RpcStation* station )
    {                                   
      if( _station1 ){
	RPCPROTOGEOM::TRACE("RpcArm_proto::set_station1 - already initialized");
	exit(1);
      }
      _station1 = station; 
    }
			
  //! station1
  virtual void set_station2( RpcStation* station )
    { 
      if( _station2 ){
	RPCPROTOGEOM::TRACE("RpcArm_proto::set_station2 - already initialized");
	exit(1);
      }
      _station2 = station; 
    }
			
  //! station1
  virtual void set_station3( RpcStation* station )
    { 
      if( _station3 ){
	RPCPROTOGEOM::TRACE("RpcArm_proto::set_station3 - already initialized");
	exit(1);
      }
      _station3 = station; 
    }
	
  //@}
	
  //! @name dumper
  //@{
	
  //! print
  virtual void print( std::ostream& out = std::cout ) const
    {
      out << "RpcArm_proto::print - index: " << _index << std::endl;
      _station1->print( out );
      _station2->print( out );
      _station3->print( out );
    }
	
  //@}
		
 private:
	
  //! arm index
  unsigned int _index;
			
  //! pointer to station1
  RpcStation* _station1;
	
  //! pointer to station2
  RpcStation* _station2;

  //! pointer to station3
  RpcStation* _station3;	
	
};

#endif
