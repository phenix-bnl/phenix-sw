
#include "RpcStation_proto.h"
#include "RPCPROTOGEOM.h"

#include <gsl/gsl_math.h>

#include <cmath>
#include <cstdlib>
#include <stdexcept>
#include <sstream>

using namespace std;

ClassImp( RpcStation_proto )

  //__________________________________________________________________
  void RpcStation_proto::create_strips( void )
{
  set_z(RPCPROTOGEOM::GlobalFramePosition_Z[_index][_arm]);
  
  clear_strips();
  
  for(UShort_t octant=0 ; octant<8 ; octant++){
    for(UShort_t halfoctant=0 ; halfoctant<2 ; halfoctant++){
      for(UShort_t radseg=0 ; radseg<_R_segmentation ; radseg++){
	for( UShort_t strip = 0; strip < 64; strip++ ){
	  RPCPROTOGEOM proto(RPCPROTOGEOM::ArmNumber(_arm),
			     RPCPROTOGEOM::StationNumber(_index),
			     octant,halfoctant,
			     RPCPROTOGEOM::RadialSegment(radseg),
			     strip);
	  if(proto.checkStrip()) {
	    RpcStrip_proto proto_strip;
	    proto_strip.SetStrip(_arm,_index,octant,halfoctant,radseg,strip);
	    
	    _strips.push_back( proto_strip );}}}}}
  
  ostringstream what;
  what << "RpcStation_proto::create_strips - created " << _strips.size()
       << " strips for Station " << _index << " in arm " <<_arm;
  RPCPROTOGEOM::TRACE( what.str() );
}

//__________________________________________________________________
RpcStrip* RpcStation_proto::get_strip( Int_t index )
{
	
  if( !_strips.size() ){
    RPCPROTOGEOM::TRACE("RpcStation_proto::get_strip - strips not created" );
    exit(1);
  }
  
  if((index > (Int_t)_strips.size())||(index < 0)){
    RPCPROTOGEOM::TRACE("RpcStation_proto::get_strip - invalid index",index);
    
    //return an initialized strip
    initvals.Init();

    return &initvals;
  }

  return &_strips[index];
}

//__________________________________________________________________
Int_t RpcStation_proto::get_strip_index( const double& x, const double& y ) const
{
  //********************************************************
  //This function is needed for the MCs,
  //- returns THREE strip numbers, one for each radial segment
  //********************************************************
  
  RpcStrip_proto temp;
  Int_t fThisStripID =0;
  for(Int_t i=0 ; i<3 ; i++) {
    temp.SetStrip(x,y,_arm,_index,i);
    if(temp.GetStripId()>-999) {
      fThisStripID += abs(int(pow(1000.,i))*temp.GetStripId()); } }
  if(fThisStripID==0) { fThisStripID = temp.GetStripId(); }
  
  //if(fThisStripID>0) {
  //cout << "********* " << fThisStripID << " *********************" << endl;}
  
  return fThisStripID;
}
