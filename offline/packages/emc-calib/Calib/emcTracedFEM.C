#include "emcTracedFEM.h"
#include "emcTracedValue.h"
#include <cassert>
#include <cmath>
#include <cstring>
#include <ctime>
#include <iostream>
#include <string>

using std::string;

static string name = "emcTracedFEM" ;
static string title = "Traced calibration data (base class)" ;
static string classname = "emcTracedFEM" ;

namespace {

  bool compare(Float_t x1, Float_t x2, float epsilon=1E-4) 
  {
    if ( x1 == 0 && x2 == 0 )
      {
	return true;
      }

    if ( std::abs((x1-x2)/x2) < epsilon ) 
      {
	return true;
      }
    else
      {
	return false;
      }
  }
}

//_____________________________________________________________________________
emcTracedFEM::emcTracedFEM(int absPosition)
  : emcCalFEM(absPosition),
    fNItems(0),fCurrentItem(0),fCurrentPosition(0)

{
  NameIt(name,title,classname) ;
}

//_____________________________________________________________________________
emcTracedFEM::emcTracedFEM(int absPosition,
			   const PHTimeStamp& t1, 
			   const PHTimeStamp& t2)
  : emcCalFEM(absPosition,t1,t2),
    fNItems(0),fCurrentItem(0),fCurrentPosition(0)
{  
  NameIt(name,title,classname) ;
}

//_____________________________________________________________________
emcTracedFEM::emcTracedFEM(const emcTracedFEM& o)
  : emcCalFEM(o.AbsolutePosition())
{
  o.Copy(*this) ;
}

//_____________________________________________________________________________
emcTracedFEM&
emcTracedFEM::operator=(const emcTracedFEM& o)
{
  if ( this == &o ) return *this ;
  // de-allocate what we already have.
  Delete() ;
  o.Copy(*this) ;
  return *this ;
}

//_____________________________________________________________________
void 
emcTracedFEM::Copy(emcTracedFEM& o) const
{
  emcCalFEM::Copy(o) ;
  o.fItems.resize(fItems.size(),0) ;
  size_t i ;
  int nitems = 0 ;

  for (i=0;i<fItems.size();i++) {
    o.fItems[i] = new emcItemVector ;
    size_t j ;
    for (j=0;j<fItems[i]->size();j++) {
      emcTracedValue* tv = (*fItems[i])[j] ;
      o.fItems[i]->push_back(new emcTracedValue(tv->GetX(),
						tv->GetConstant(),
						tv->GetSlope(),
						tv->isConstant())) ;
      nitems++ ;
    }
  }
  o.fNItems = nitems ;
  assert(o.fNItems==fNItems) ;
  o.fCurrentItem = fCurrentItem ;
  o.fCurrentPosition = fCurrentPosition ;
}

//_____________________________________________________________________________

emcTracedFEM::~emcTracedFEM()
{
  Delete() ;
}

//_____________________________________________________________________________

void emcTracedFEM::AddNewItem(int channel, emcTracedValue* item)
{
  // Add a new item to one channel.
  //
  // It is assumed that item->GetX() is either greater than
  // any of the existing items for this channel or equal to
  // one of them (in which case item replaces the old one).
  // To put it another way, items are NOT time ordered by this method.

  assert (channel>=0 && channel<static_cast<int>(fItems.size())) ;
  assert(fItems[channel]!=0) ;
  emcItemVector& itemVector = *(fItems[channel]) ;
  emcTracedValue* currentitem ;
  size_t i ;
  bool inserted = false ;

  for ( i = 0 ; i < itemVector.size() && !inserted ; i++ ) {
    currentitem = itemVector[i] ;
    assert(currentitem!=0) ;
    if ( currentitem->GetX() == item->GetX() ) {
      // we replace currentitem by item, because both are starting
      // at the same time
      delete currentitem ;
      itemVector[i] = item ;
      inserted = true ;
    }
  }
  if (!inserted) {
    fNItems++ ; 
    itemVector.push_back(item) ;
  }
}


//_____________________________________________________________________________
void emcTracedFEM::Delete(void)
{
  // Delete internal storage memory space.
  size_t i,j ;

  for ( i = 0 ; i < fItems.size() ; i++ ) {
    assert(fItems[i]!=0) ;
    emcItemVector& itemVector = *(fItems[i]) ;
    for ( j = 0 ; j < itemVector.size() ; j++ ) {
      delete itemVector[j] ;      
      itemVector[j] = 0 ;
    }
    delete fItems[i] ;
    fItems[i] = 0 ;
  }

  fItems.clear() ;
  fNItems = 0 ;
}

//_____________________________________________________________________________
void emcTracedFEM::FirstItem(int channel) const
{
    // Position the internal iterator to the first item for channel
  assert (channel>=0 && channel<static_cast<int>(fItems.size()));
  fCurrentItem = channel ;
  fCurrentPosition = 0 ;
}

//_____________________________________________________________________________
size_t
emcTracedFEM::GetNumberOfItems(int ichannel) const
{
  if ( ichannel < 0 || ichannel >= static_cast<int>(fItems.size()) ) return 0;
  emcItemVector* vec = fItems[ichannel] ;
  if ( vec ) return vec->size() ;
  return 0 ;
}

//_____________________________________________________________________________
emcTracedValue* 
emcTracedFEM::getTV(int ichannel, time_t absoluteX, size_t& thecase) const 
{
  /* 
     For a given channel, recover the piece of line valid for absoluteX.

     (absolute)X have 2 different meanings :
     a) For PbSc : X=tics=a "time"
     b) For PbGl : X=run number

     The internal value used is not directly absoluteX, but 
     x = absoluteX - GetXmin(), i.e. a relative X.

     Depending on x, 4 cases are to be considered :
     1. x is < xmin of the first tracedvalue
     2. x falls within the validity range of one of the tracedvalues
     3. x is after the last tracedvalue valid x, but
        before GetXmax
     4. x is after GetXmax
   */

  int x = static_cast<int>(absoluteX - GetXmin()) ; 
  
  emcItemVector* vec = fItems[ichannel] ;
  assert(vec!=0) ;
  emcItemVector& fTracedValues = *vec ;

  if (fTracedValues.size()==0) { return 0 ; }

  if ( x <= fTracedValues[0]->GetX() ) {
    // x requested is before first valid x : we return
    // the first item
    thecase = 1 ; 
    return fTracedValues[0] ;
  }

  bool inside = false ;
  int i ;
  int n = static_cast<int>(fTracedValues.size()) ;

  for ( i = 0 ; i < n-1 && !inside ; i++ ) {
    assert(fTracedValues[i]!=0) ;
    assert(fTracedValues[i+1]!=0) ;
    if ( x >= fTracedValues[i]->GetX() && 
         x < fTracedValues[i+1]->GetX() ) inside = true ;
  } 

  if (inside) {
    thecase = 2 ; 
    // requested x falls within validity period of a tracedvalue
    // we return this tracedvalue
    assert(fTracedValues[i-1]!=0) ;    
    return fTracedValues[i-1] ;
  }

  int xrelmax = GetXmax()-GetXmin() ;
  assert(fTracedValues[n-1]!=0) ;
  if ( x>= fTracedValues[n-1]->GetX() && x<=xrelmax ) {
    thecase = 3 ;
    // Requested x is after last tracedvalue x, but still
    // in authorized extrapolation region
  }
  else {
    // Requested x is after xmax. 
    thecase = 4 ;
  }
  return fTracedValues[n-1] ;
}

//_____________________________________________________________________________
float 
emcTracedFEM::getValueFast(int ichannel, time_t absoluteX) const
{
  /* 
     (absolute)X have 2 different meanings :
     a) For PbSc : X=tics=a "time"
     b) For PbGl : X=run number

     The internal value used is not directly absoluteX, but 
     x = absoluteX - GetXmin(), i.e. a relative X.

     Depending on x, 4 cases are to be considered :
     1. x is < xmin of the first tracedvalue
        -> we return the constant of the first tracedvalue
     2. x falls within the validity range of one of the tracedvalues
        -> we let this tracedvalue compute the correct value for this x
     3. x is after the last tracedvalue valid x, but
        before GetXmax
        -> we let the last tracedvalue extrapolate to x
     4. x is after GetXmax
        -> we return the last tracedvalue extrapolation up to Xmax
   */

  size_t thecase ;

  emcTracedValue* tv = getTV(ichannel, absoluteX, thecase) ;

  float rv = 0.0 ;

  if (!tv) thecase = 0 ;

  int x = static_cast<int>(absoluteX - GetXmin()) ; 
  int xrelmax = static_cast<int>(GetXmax()-GetXmin()) ;

  switch (thecase) {

  case 0 :
    rv = 1.0 ;
    break;
  case 1:
    rv = tv->GetConstant() ;
    break ;
  case 2:
  case 3: // same as 2
    rv = tv->getValue(x) ;
    break ;
  case 4:
    rv = tv->getValue(xrelmax) ;
    break ;
  default:
    assert(0==1) ;
    break;
  }

  return rv ;
}

//_____________________________________________________________________________
bool
emcTracedFEM::IsEqual(const emcCalFEM& obj) const 
{
  const emcTracedFEM* tfem = dynamic_cast<const emcTracedFEM*>(&obj) ;
  if (!tfem) return false ;

  if ( size() != obj.size() ) return false ;

  emcTracedValue* me;
  emcTracedValue* other;

  for ( size_t ichannel = 0 ; ichannel < size() ; ichannel++) {
    FirstItem(ichannel) ;
    tfem->FirstItem(ichannel) ;
    while ( ( me = NextItem() ) ) {
      other = tfem->NextItem() ;
      // tricky equality between emcTracedValue, due
      // to strange semantics of emcTracedValue::operator== ?!?!
      if ( me->GetX() != other->GetX() ) 
	{
	  return false ;
	}
      if ( !compare(me->GetConstant(),other->GetConstant())) 
	{
	  return false ;
	}
      if ( !compare(me->GetSlope(),other->GetSlope())) 
	{
	  return false ;
	}
      if ( me->isConstant() != other->isConstant()) 
	{
	  return false ;
	}
    }
  }

  return true ;
}

//_____________________________________________________________________________
emcTracedValue* 
emcTracedFEM::LastItem(int channel) const
{
  if ( channel >= static_cast<int>( fItems.size() ) ||
       channel < 0 ) {
    return 0 ;
  }
 
  emcTracedValue* tv = 0 ;      
  emcItemVector* vec = fItems[channel] ;

  if (vec) {
    int i = vec->size()-1 ;
    if (i>=0) {
      tv = (*vec)[i] ;
    }
  }
  return tv ;
}

//_____________________________________________________________________________
emcTracedValue* emcTracedFEM::NextItem(void) const
{
  // Iterate forward
  emcTracedValue* rv = 0 ;

  if (fCurrentItem<fItems.size()) {
    emcItemVector* vec = fItems[fCurrentItem] ;
    assert(vec!=0) ;
    if (fCurrentPosition<vec->size()) {
      rv = (*vec)[fCurrentPosition] ;
      fCurrentPosition++ ;
    }
  }
  return rv ;
}

//_____________________________________________________________________
std::ostream&
emcTracedFEM::Print(std::ostream& out, int level) const
{
  emcCalFEM::Print(out,level) ;

  if (level) {
    out << "xmin=" << GetXmin() << " xmax=" << GetXmax() << std::endl ;
    for ( size_t i = 0 ; i < fItems.size() ; i++) {
      emcItemVector* vec = fItems[i] ;
      if (vec) {
	for (size_t j = 0; j < vec->size() ; j++) {
	  out << i << " " << *((*vec)[j]) ;
	}
      }
    }
  }
  return out ;
}

//_____________________________________________________________________________
void
emcTracedFEM::RemoveLastItems(void)
{
  // Remove the last item of each channel.

  for (size_t i = 0 ; i < fItems.size() ; ++i) {
    emcItemVector* vec = fItems[i] ;
    if (vec) {
      emcTracedValue* last = vec->back() ;
      delete last ;
      last = 0 ;
      vec->pop_back() ;
      --fNItems ;
    }
  }
}

//_____________________________________________________________________________
void emcTracedFEM::Reset(void)
{
  Delete() ;
  SetXmin(0) ;
  SetXmax(0) ;
}

//_____________________________________________________________________________
void emcTracedFEM::SetNumberOfChannels(int number_of_channels)
{
  assert (number_of_channels>=0) ;

  Delete() ;
  fItems.resize(number_of_channels) ;

  for( size_t i = 0 ; i < fItems.size() ; i++ ) {
    fItems[i] = new emcItemVector() ;
  }
}

//_____________________________________________________________________________
void emcTracedFEM::writeDataToFile(FILE* fp) const
{
  time_t tics = GetStartValTime().getTics(); 
  char timeString[25]; 
  timeString[24] = '\0'; 
  strncpy(timeString, ctime(&tics), 24);   
  fprintf(fp,"%s\n",timeString) ;

  size_t i,j ;
  int item = 0 ;

  for (i=0;i<fItems.size();i++) {
    assert(fItems[i]!=0) ;
    emcItemVector& itemVector = *(fItems[i]) ;
    for (j=0;j<itemVector.size();j++) {    
      fprintf(fp," %5d", item) ;
      itemVector[j]->writeData(fp) ;
      fprintf(fp,"\n") ;
    }
    item++ ;
  }
  fprintf(fp,"144 %ld 0 0\n",GetXmax());
  fprintf(fp,"145 %ld 0 0\n",GetXmin());
}
