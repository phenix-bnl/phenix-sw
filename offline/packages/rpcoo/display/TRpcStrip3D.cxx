
/*!
   \file    TRpcStrip3D.cxx
   \brief   3D object for RPC strips
   \author  R. Hollis (UCR - rhollis@ucr.edu)
   \version 
   \date    
*/

#include "TRpcStrip3D.h"
#include "RpcGeom.h"
#include "RPCPROTOGEOM.h"
#include "RPCFULLGEOM.h"
#include "RPCFINALGEOM.h"
#include "RPCOO.h"
#include <sstream>
#include <TTUBE.h>
#include <recoConsts.h>


using namespace std;

TRpcStrip3D::TRpcStrip3D( const TRpcIndex &index, TNode* parent ):
	PHObj3D( parent ),
	_index( index ), 
	_name( "unnamed" )//,
	//	_strip_ptr( 0 )
{
  _line_color = 1;
  
  // check indexes
  if( !_index.check() ) 
    {
      //cout << "TRpcStrip3D::TRpcStrip3D - invalid ids.\n";
      return;
    }
  
  // create name
  ostringstream what;
  what << "RpcStrip3D_" 
       << _index.arm() 
       << _index.station()  
       << _index.octant() 
       << _index.halfoct()
       << _index.radseg()
       << _index.strip();
  _name = what.str();
  
  _is_valid = true;
  
}

//_______________________________________________________
void TRpcStrip3D::_make_nodes( void )
{
  
  if( !_is_valid ) {
    //cerr << "TRpcStrip3D::make_nodes - invalid object.\n"; 
    return; }
  
  if( _parent ) { _parent->cd(); }

  
  recoConsts *myrc = recoConsts::instance();
  
  int fThisRPCGeom = 1;//Prototype is default
  if(myrc->FlagExist("RpcGeomType")) {
    if(myrc->get_IntFlag("RpcGeomType")==0) {
      cout << "UNKNOWN RPC GEOMETRY" << endl; return; }
    if(myrc->get_IntFlag("RpcGeomType")==1) { fThisRPCGeom=1; }
    if(myrc->get_IntFlag("RpcGeomType")==2) { fThisRPCGeom=2; }
    if(myrc->get_IntFlag("RpcGeomType")==3) { fThisRPCGeom=3; } }

  if(fThisRPCGeom==1) {
    RPCPROTOGEOM proto(RPCPROTOGEOM::ArmNumber(_index.arm()),
		       RPCPROTOGEOM::StationNumber(_index.station()),
		       _index.octant(),_index.halfoct(),
		       RPCPROTOGEOM::RadialSegment(_index.radseg()),
		       _index.strip());
    
    _make_segment(proto.GetBegin(),proto.GetEnd(),0.1);//last is radius
  }
  else if(fThisRPCGeom==2) {
    if(_index.index()<999999) { //then this is the real strips
      RPCFULLGEOM fullgeom(RPCFULLGEOM::ArmNumber(_index.arm()),
			   RPCFULLGEOM::StationNumber(_index.station()),
			   _index.octant(),_index.halfoct(),
			   RPCFULLGEOM::RadialSegment(_index.radseg()),
			   _index.strip());
      
      _make_segment(fullgeom.GetBegin(),fullgeom.GetEnd(),0.1);//last is radius
    }
    else {//this is the outline
      
	RPCFULLGEOM fullgeom1(RPCFULLGEOM::ArmNumber(_index.arm()),
			      RPCFULLGEOM::StationNumber(_index.station()),
			      _index.octant(),_index.halfoct(),
			      RPCFULLGEOM::RadialSegment(_index.radseg()),
			      _index.strip1());
	RPCFULLGEOM fullgeom2(RPCFULLGEOM::ArmNumber(_index.arm()),
			      RPCFULLGEOM::StationNumber(_index.station()),
			      _index.octant(),_index.halfoct(),
			      RPCFULLGEOM::RadialSegment(_index.radseg()),
			      _index.strip2());
	/*std::cout << _index.index() << std::endl;
	  fullgeom1.GetBegin().print();
	  fullgeom2.GetBegin().print();
	  std::cout << std::endl; */
	if(_index.istop1()==0) {//end
	  if(_index.istop2()==0) { //end
	    _make_segment(fullgeom1.GetEnd(),fullgeom2.GetEnd(),0.1); }
	  else { //begin
	    _make_segment(fullgeom1.GetEnd(),fullgeom2.GetBegin(),0.1); } }
	else {//begin
	  if(_index.istop2()==0) { //end
	    _make_segment(fullgeom1.GetBegin(),fullgeom2.GetEnd(),0.1); }
	  else { //begin
	    _make_segment(fullgeom1.GetBegin(),fullgeom2.GetBegin(),0.1); } }
    }
  }
  else if(fThisRPCGeom==3) {
    if(_index.index()<999999) { //then this is the real strips
      RPCFINALGEOM finalgeom(RPCFINALGEOM::ArmNumber(_index.arm()),
			   RPCFINALGEOM::StationNumber(_index.station()),
			   _index.octant(),_index.halfoct(),
			   RPCFINALGEOM::RadialSegment(_index.radseg()),
			   _index.strip());
      
      _make_segment(finalgeom.GetBegin(),finalgeom.GetEnd(),0.1);//last is radius
    }
    else {//this is the outline
      
	RPCFINALGEOM finalgeom1(RPCFINALGEOM::ArmNumber(_index.arm()),
			      RPCFINALGEOM::StationNumber(_index.station()),
			      _index.octant(),_index.halfoct(),
			      RPCFINALGEOM::RadialSegment(_index.radseg()),
			      _index.strip1());
	RPCFINALGEOM finalgeom2(RPCFINALGEOM::ArmNumber(_index.arm()),
			      RPCFINALGEOM::StationNumber(_index.station()),
			      _index.octant(),_index.halfoct(),
			      RPCFINALGEOM::RadialSegment(_index.radseg()),
			      _index.strip2());
	/*std::cout << _index.index() << std::endl;
	  finalgeom1.GetBegin().print();
	  finalgeom2.GetBegin().print();
	  std::cout << std::endl; */
	if(_index.istop1()==0) {//end
	  if(_index.istop2()==0) { //end
	    _make_segment(finalgeom1.GetEnd(),finalgeom2.GetEnd(),0.1); }
	  else { //begin
	    _make_segment(finalgeom1.GetEnd(),finalgeom2.GetBegin(),0.1); } }
	else {//begin
	  if(_index.istop2()==0) { //end
	    _make_segment(finalgeom1.GetBegin(),finalgeom2.GetEnd(),0.1); }
	  else { //begin
	    _make_segment(finalgeom1.GetBegin(),finalgeom2.GetBegin(),0.1); } }
    }
  }
  else {
    cout << "UNKNOWN geometry type" << endl; }
  
}

//_______________________________________________________
void TRpcStrip3D::print( std::ostream& out ) const
{
  RPCPROTOGEOM proto(RPCPROTOGEOM::ArmNumber(_index.arm()),
		     RPCPROTOGEOM::StationNumber(_index.station()),
		     _index.octant(),_index.halfoct(),
		     RPCPROTOGEOM::RadialSegment(_index.radseg()),
		     _index.strip());
  
  // some dump
  RPCOO::PRINT( cout, "TRpcStrip3D::print" );
  cout << "location : [" << _index.arm() << "," << _index.station() << ","
       << _index.octant() << "," << _index.halfoct() << ","
       << _index.radseg() << "," << _index << "]" << endl;
  cout << "x=("
       << proto.GetBegin().getX() << "," << proto.GetEnd().getX() << ")"
       << endl;
  cout << "y=("
       << proto.GetBegin().getY() << "," << proto.GetEnd().getY() << ")"
       << endl;
  cout << "z="
       << proto.GetBegin().getZ() << "," << proto.GetEnd().getZ() << ")"
       << endl;
  RPCOO::PRINT( cout, "**" ); 
}
