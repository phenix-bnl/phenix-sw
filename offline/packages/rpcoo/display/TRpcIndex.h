// $Id: TRpcIndex.h,v 1.6 2011/11/30 17:25:28 richi Exp $

#ifndef __TRpcIndex_H__
#define __TRpcIndex_H__

/*!
	\file TRpcIndex.h
	\brief returns unique integer associated to Rpc plane location
	\author H. Pereira Da Costa
	\version $Revision: 1.6 $
	\date $Date: 2011/11/30 17:25:28 $
*/

#include <RPCOO.h>
#include <RPCPROTOGEOM.h>
#include <RPCFULLGEOM.h>
#include <RPCFINALGEOM.h>
#include <iostream>
#include <recoConsts.h>

/*! @ingroup display */
//! returns unique integer associated to rpc station location
class TRpcIndex {
	
	public:
	
	//! constructor
	TRpcIndex( 
		unsigned long arm, 
		unsigned long station, 
		unsigned long octant, 
		unsigned long halfoct, 
		unsigned long radseg,
		unsigned long strip
		)
	  {
	    _arm=arm;
	    _station=station;
	    _octant=octant;
	    _halfoct=halfoct;
	    _radseg=radseg;
	    _strip=strip;
	    //update( arm, station );
	    //64 strips, 3 rad segs, 2 half oct, 8 octants, 3 stations, 2 arms
	    _index=arm*64*3*2*8*3+station*64*3*2*8+octant*64*3*2+halfoct*64*3+radseg*64+strip;
	    if( !check() ) {
	      /*std::cout << "TRpcIndex::TRpcIndex - invalid index "
		<< *this << std::endl; */}
	    else {
	      /*std::cout << "TRpcIndex::TRpcIndex - VALID index "
		<< *this << std::endl; */}
	  }
	
	//! check indexes
	bool check( void )
	  {
	    if(this->index()>999999) { return true; }
	    recoConsts *myrc = recoConsts::instance();
	    
	    int fThisRPCGeom = 1;//Prototype is default
	    if(myrc->FlagExist("RpcGeomType")) {
	      if(myrc->get_IntFlag("RpcGeomType")==0) {
		std::cout << "UNKNOWN RPC GEOMETRY" << std::endl;
		return false; }
	      if(myrc->get_IntFlag("RpcGeomType")==1) { fThisRPCGeom=1; }
	      if(myrc->get_IntFlag("RpcGeomType")==2) { fThisRPCGeom=2; }
	      if(myrc->get_IntFlag("RpcGeomType")==3) { fThisRPCGeom=3; } }

	    if(fThisRPCGeom==1) {
	      RPCPROTOGEOM proto(RPCPROTOGEOM::ArmNumber(arm()),
				 RPCPROTOGEOM::StationNumber(station()),
				 octant(),halfoct(),
				 RPCPROTOGEOM::RadialSegment(radseg()),
				 strip());
	      return proto.checkStrip(); }
	    else if(fThisRPCGeom==2) {
	      RPCFULLGEOM fullgeom(RPCFULLGEOM::ArmNumber(arm()),
				   RPCFULLGEOM::StationNumber(station()),
				   octant(),halfoct(),
				   RPCFULLGEOM::RadialSegment(radseg()),
				   strip());
	      return fullgeom.checkStrip(); }    
	    else if(fThisRPCGeom==3) {
	      RPCFINALGEOM finalgeom(RPCFINALGEOM::ArmNumber(arm()),
				   RPCFINALGEOM::StationNumber(station()),
				   octant(),halfoct(),
				   RPCFINALGEOM::RadialSegment(radseg()),
				   strip());
	      return finalgeom.checkStrip(); }    
	    else {
	      std::cout << "UNKNOWN GEOMETRY: " << fThisRPCGeom << std::endl;
	      return false; }
	  }
	
	//! equal to operator
	bool operator == ( const TRpcIndex& index ) const
	  { return _index == index._index; }
	
	//! equal to operator
	bool operator < ( const TRpcIndex& index ) const
	{ return _index < index._index; }

	//! print
	friend std::ostream& operator << (std::ostream &out,const TRpcIndex &index)
	{
	  out << "(" << index.arm() << "," << index.station() << "," << index.octant() << "," << index.halfoct() << "," << index.radseg() << "," << index.strip() << ") (" << index._index << ")";
	  return out;
	}
	
	//! return arm
	int arm( void ) const     { return _arm;     }
	
	//! return station
	int station( void ) const { return _station; }

	//! return octant
	int octant( void ) const  { return _octant;  }

	//! return half-octant
	int halfoct( void ) const { return _halfoct; }

	//! return radial segment
	int radseg( void ) const  { return _radseg;  }
	
	//! return strip
	int strip( void ) const   { return _strip;  }

	//! return index
	int index( void ) const   { return _index;  }
	
	//! set a strip index for the "frame" istop is 1 for outer coord and 0 for inner coord
	void setstrip1(int fstrip, int istop) { _strip1 = fstrip; _istop1 = istop; }
	void setstrip2(int fstrip, int istop) { _strip2 = fstrip; _istop2 = istop; }
	int strip1( void ) const   { return _strip1;  }
	int strip2( void ) const   { return _strip2;  }
	int istop1( void ) const   { return _istop1;  }
	int istop2( void ) const   { return _istop2;  }
	
 private:
	
	//! update index from locator
	void update( unsigned long arm, unsigned long station , unsigned long octant , unsigned long halfoct , unsigned long radseg )
	  { }//_index = (arm << ARM_SHIFT)|(station << STATION_SHIFT); }	
	int _arm;
	int _station;
	int _octant;
	int _halfoct;
	int _radseg;
	int _strip;
	
	int _index;

	int _strip1;
	int _strip2;
	int _istop1;
	int _istop2;
};

#endif
