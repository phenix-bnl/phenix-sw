#include "emcTacPedFEM.h"
#include "emcTracedValue.h"
#include "emcCalFEMFactory.h"
#include <string>
#include <cmath>

using namespace std;

namespace
{
  static string name = "emcTacPedFEM" ;
  static string title = "TAC Pedestal Drift" ;
  static string classname = "emcTacPedFEM" ;

  emcCalFEM* creator(int absPosition,
		     const PHTimeStamp& start,
		     const PHTimeStamp& end,
		     bool isDefault)
  {
    if ( isDefault )
      {
	return emcTacPedFEM::Default(absPosition,start,end);
      }
    else
      {
	return new emcTacPedFEM(absPosition,start,end);
      }
  }

  static bool r = emcCalFEMFactory::registerCreator("TacPeds",
						    creator);

}

//_____________________________________________________________________________
emcTacPedFEM::emcTacPedFEM(int absPosition)
  : emcGainFEM(absPosition)
{
  NameIt(name,title,classname) ;
}

//_____________________________________________________________________________
emcTacPedFEM::emcTacPedFEM(int absPosition,
		       const PHTimeStamp& tStart, const PHTimeStamp& tEnd)
  : emcGainFEM(absPosition,tStart,tEnd)
{
  NameIt(name,title,classname) ;
}

//_____________________________________________________________________________
void
emcTacPedFEM::AddNewItem(int ichannel, emcTracedValue* item)
{
  if (item) {
    item->MakeConstant(true) ;
    emcTracedFEM::AddNewItem(ichannel,item) ;
  }
}

//_____________________________________________________________________________
bool
emcTacPedFEM::AreDifferent(float v1, float v2, float epsilon) const
{
  float diff = fabs(v1-v2) ;
  return ( diff > epsilon ) ;
}

//_____________________________________________________________________________
float 
emcTacPedFEM::Compact(float epsilon)
{
  size_t i ;

  int nitems = fNItems ;

  for ( i = 0 ; i < fItems.size() ; i++) {
    CompactOneChannelConstants(i,epsilon) ;
  }

  float compression = fNItems/static_cast<float>(nitems) ;
  return compression ;
}

//_____________________________________________________________________________
emcTacPedFEM*
emcTacPedFEM::Default(int absPosition, 
		    const PHTimeStamp& tStart, const PHTimeStamp& tEnd)
{
  emcTacPedFEM* fem = new emcTacPedFEM(absPosition,tStart,tEnd) ;

  fem->SetNumberOfChannels(144) ;
  
  size_t i ;
  for ( i = 0 ; i < 144 ; i++ ) {
    fem->AddNewItem(i,new emcTracedValue(0,1,0)) ;
  }

  return fem ;

}
