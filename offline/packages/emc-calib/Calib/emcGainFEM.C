#include "emcGainFEM.h"
#include "emcDefines.h"
#include "EmcIndexer.h"
#include "emcTracedValue.h"
#include "emcCalFEMFactory.h"
#include <cmath>
#include <iostream>

#include <string>

using std::endl;
using std::cerr;

namespace
{
  static std::string name = "emcGainFEM" ;
  static std::string title = "Gain calibration data" ;
  static std::string classname = "emcGainFEM" ;

  emcCalFEM* creator(int absPosition,
		     const PHTimeStamp& start,
		     const PHTimeStamp& end,
		     bool isDefault)
  {
    if ( isDefault )
      {
	return emcGainFEM::Default(absPosition,start,end);
      }
    else
      {
	return new emcGainFEM(absPosition,start,end);
      }
  }

  static bool r = emcCalFEMFactory::registerCreator("Gains",
						    creator);
}

//_____________________________________________________________________________
emcGainFEM::emcGainFEM(int absPosition)
  : emcTracedFEM(absPosition)
{
  NameIt(name,title,classname) ;
}

//_____________________________________________________________________________

emcGainFEM::emcGainFEM(int absPosition,
		       const PHTimeStamp& tStart, const PHTimeStamp& tEnd)
  : emcTracedFEM(absPosition,tStart,tEnd)
{
  NameIt(name,title,classname) ;
}

//_____________________________________________________________________________
void 
emcGainFEM::AddNewItem(int channel, emcTracedValue* item)
{
  if (item) {
    if ( EmcIndexer::isPbScFEM(AbsolutePosition()) ) {
      item->MakeConstant(false) ;
    }
    else {
      item->MakeConstant(true) ;
    }
    emcTracedFEM::AddNewItem(channel,item) ;
  }
}

//_____________________________________________________________________________
bool
emcGainFEM::AreDifferent(float v1, float v2, float epsilon) const
{
  float diff = fabs((v1-v2)/v1) ;
  return ( diff > epsilon ) ;
}

//_____________________________________________________________________________
float 
emcGainFEM::Compact(float epsilon)
{
  size_t nitems = GetNumberOfItems();
  size_t nchannels = GetNumberOfChannels();

  if ( EmcIndexer::isPbScFEM(AbsolutePosition()) ) 
    {
      for ( size_t i = 0 ; i < nchannels ; i++) 
	{
	  CompactOneChannelLines(i,epsilon) ;
	}
    }
  else 
    {
      if ( EmcIndexer::isPbGlFEM(AbsolutePosition()) ) 
	{
	  for ( size_t i = 0 ; i < nchannels ; i++) 
	    {
	      CompactOneChannelConstants(i,epsilon) ;
	    }
	}
      else 
	{
	  std::cerr << "<EMC-ERROR> emcGainFEM::Compact : "
		    << " found a FEM which is not PBGL nor PBSC ?!" 
		    << std::endl;
	  return 1.0;
	}
    }
    
  float compression = GetNumberOfItems()/static_cast<float>(nitems);
  return compression;
}

//_____________________________________________________________________________
bool
emcGainFEM::CompactOneChannelLines(int ichannel, float epsilon)
{
  // Here ichannel is supposed to be valid

  size_t i ;

  emcItemVector* pvec = fItems[ichannel] ;

  if ( !pvec || pvec->size()<2 ) return false ;

  emcItemVector& vec = *pvec ;

  emcTracedValue* t1 ;
  emcTracedValue* t2 ;

  for ( i = 0 ; i < vec.size()-1 ; i++) {

    t1 = vec[i] ;
    t2 = vec[i+1] ;

    if ( t2->GetConstant() != 0 ) { // protection against "zero-ed" lines.

      // xs is the start X of the line
      float xs1 = t1->GetX() ;
      float xs2 = t2->GetX() ;
      // xe is the end X of the 2nd line
      float xe2 ;
      if ( i < vec.size()-2 ) {
	xe2 = vec[i+2]->GetX() ;
      }
      else {
	xe2 = GetXmax() ;
      }
      
      float xc1 = 0.5*(xs1+xs2) ;
      float xc2 = 0.5*(xs2+xe2) ;
      float yc1 = t1->getValue(xc1) ;
      float yc2 = t2->getValue(xc2) ;
      
      float slope = (yc2-yc1)/(xc2-xc1) ;
      float intercept = yc1 + (xs1-xc1)*slope ;
      float diff1 = fabs( 2.0 * (intercept-t1->GetConstant()) /
			  (intercept+t1->GetConstant()) ) ;
      
      if ( diff1 > epsilon ) continue ;
      
      float ys2 = intercept + slope*(xs2-xs1) ;
      float diff2 = fabs( 2.0 * (ys2-t2->GetConstant()) /
			  (ys2+t2->GetConstant()) ) ;
      
      if (diff2 > epsilon) continue ;
      
      // extand t1 line and delete t2 (i.e. t1 "absorbs" t2).
      t1->Set(t1->GetX(),intercept,slope) ;

    }      
    delete t2 ;
    t2 = 0 ;
    fNItems-- ;
    size_t j ;
    for ( j = i+1 ; j < vec.size()-1 ; j++ ) {
      vec[j] = vec[j+1] ;
    }
    vec.pop_back() ;
    --i ;
  }

  return true ;
}

//_____________________________________________________________________________
bool
emcGainFEM::CompactOneChannelConstants(int ichannel, float epsilon)
{
  // Here ichannel is supposed to be valid.

  size_t i ;

  emcItemVector* pvec = fItems[ichannel] ;

  if ( !pvec || pvec->size()<2 ) return false ;

  emcItemVector& vec = *pvec ;

  emcTracedValue* t1 ;
  emcTracedValue* t2 ;

  for ( i = 0 ; i < vec.size()-1 ; i++) {

    t1 = vec[i] ;
    t2 = vec[i+1] ;

    if ( !AreDifferent(t1->GetConstant(),t2->GetConstant(),epsilon) ) {

      // merge t1 and t2 
      float mean = 0.5*(t1->GetConstant()+t2->GetConstant()) ;
      t1->Set(t1->GetX(),mean,t1->GetSlope()) ;
      delete t2 ;
      t2 = 0 ;
      fNItems-- ;
      size_t j ;
      for ( j = i+1 ; j < vec.size()-1 ; j++ ) {
	vec[j] = vec[j+1] ;
      }
      vec.pop_back() ;
      --i ;
    }
  }
  return true ;
}


//_____________________________________________________________________________
emcGainFEM*
emcGainFEM::Default(int absPosition, 
		    const PHTimeStamp& tStart, const PHTimeStamp& tEnd)
{
  emcGainFEM* fem = new emcGainFEM(absPosition,tStart,tEnd) ;

  fem->SetNumberOfChannels(144) ;
  
  size_t i ;
  for ( i = 0 ; i < 144 ; i++ ) {
    fem->AddNewItem(i,new emcTracedValue(0,1,0)) ;
  }

  return fem ;

}
