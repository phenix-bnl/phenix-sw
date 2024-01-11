
#include "RpcStation.h"
#include "RPCGEOM.h"

#include <gsl/gsl_math.h>

#include <cmath>
#include <cstdlib>
#include <stdexcept>
#include <sstream>

using namespace std;

ClassImp( RpcStation )

  //__________________________________________________________________
  void RpcStation::create_strips( void )
{
  clear_strips();
	
  //strip to get access to class in order to get num of strips
  RpcStrip access;

  for(UShort_t octant_index=0;octant_index<8;octant_index++){
    for(UShort_t halfoctant_index=0;halfoctant_index<2;halfoctant_index++){
      for(UShort_t R_index=0;R_index<_R_segmentation-1;R_index++){
	UShort_t n_strips =  access.GetNumStrips(_arm,_index,octant_index,halfoctant_index,R_index);
	for( UShort_t strip_index = 0; strip_index < n_strips; strip_index++ )
	  {
		
	    // create new strip
	    RpcStrip strip;
	    strip.SetStrip(_arm,_index,octant_index,halfoctant_index,R_index,strip_index);

	    _strips.push_back( strip );
	      
	  }
      }
    }
  }
  ostringstream what;
  what << "RpcStation::create_strips - created " << _strips.size() << " strips for Station " << _index;
  RPCGEOM::TRACE( what.str() );
	
}

//__________________________________________________________________
RpcStrip* RpcStation::get_strip( Int_t index )
{
	
  if( !_strips.size() ){
    RPCGEOM::TRACE("RpcStation::get_strip - strips not created" );
    exit(1);
  }
	
  if((index > (Int_t)_strips.size())||(index < 0)){
    RPCGEOM::TRACE("RpcStation::get_strip - invalid index",index);
 
    //return an initialized strip
    initvals.Init();

    return &initvals;
  }

  return &_strips[index];
	
}

//__________________________________________________________________
Int_t RpcStation::get_strip_index( const double& x, const double& y ) const
{
	
  RpcStrip temp;
  temp.SetStrip(x,y,_arm,_index,_R_segmentation);

  // calculate strip index
  int strip_octant = temp.GetOctant();
  int strip_halfoctant = temp.GetHalfOctant();
  int strip_Rseg = temp.GetRSeg();
  int strip_index = temp.GetStripId();

  //  temp.print(std::cout);

  if(temp.IsEmpty()) return -999;

  //count strips from start of indexing until we get to the 
  //R segment before the one our strip is in.  Then add strip 
  //number plus one (since strip number starts at 0).  
  //This gets us back to station indices.

  //Surely there's a better way to do this...

  RpcStrip access;
  UShort_t station_strip_index = 0;

  for(UShort_t octant_index=0;octant_index<strip_octant+1;octant_index++){

    int max_halfoctant;
    if((octant_index==strip_octant)&&(strip_halfoctant==0)){max_halfoctant=1;}
    else max_halfoctant=2;
    
    for(UShort_t halfoctant_index=0;halfoctant_index<max_halfoctant;halfoctant_index++){
      int max_Rseg;
      if((octant_index==strip_octant)&&(halfoctant_index==strip_halfoctant)){max_Rseg=strip_Rseg;}
      else max_Rseg=_R_segmentation-1;      

      for(UShort_t R_index=0;R_index<max_Rseg;R_index++){

	station_strip_index += access.GetNumStrips(_arm,_index,octant_index,halfoctant_index,R_index);
	  }
    }
  }
	
  station_strip_index+=strip_index;

  return station_strip_index;	
}
