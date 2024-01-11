// Implementation of class : emcMixedDataObject
//
// Author: Yves Schutz (yves.schutz@subatech.in2p3.fr)

#include "emcMixedDataObject.h"
#include "emcRawDataAccessor.h"
#include "TString.h"
#include <cassert>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <map>

ClassImp(emcMixedDataObject)

  using namespace std;

//_____________________________________________________________________________
emcMixedDataObject::emcMixedDataObject() : emcDataObject()
{
  fADC = fTDC = fHG = fLG = 0 ;
  fOwnAllocation  = false ;
  fPedestal       = false ;
  fHLRatio        = false ;
  UseHGLG(false) ;
}



//_____________________________________________________________________________
emcMixedDataObject::~emcMixedDataObject()
{
  delete[] fADC ;
  delete[] fTDC ;
  delete[] fHG ;
  delete[] fLG ;
}

//_____________________________________________________________________________
void emcMixedDataObject::Get(Int_t index, Float_t& adc, Float_t& tdc) const
{
  /* Get adc,tdc using tower indexing. 
     If towerindex is not valid, returned values of adc and tdc are -1
  */

  if (index>=0) {
    adc        = fADC[index] ;
    tdc        = fTDC[index] ;
  }
  else {
    adc = tdc = 0. ;
  }
}

//_____________________________________________________________________________
void emcMixedDataObject::Get(Int_t index, Float_t& adc, Float_t& tdc,
			     Float_t& hg, Float_t& lg) const
{
  /* Get adc,tdc,hg,lg using tower indexing.
     If towerindex is not valid, returned values of adc=tdc=hg=lg=-1
  */

  if (index>=0) {
    adc        = fADC[index] ;
    tdc        = fTDC[index] ;
    if (IsUsingHGLG()) {
      hg = fHG[index] ;
      lg = fLG[index] ;
    }
    else {
      cerr << "<E> emcMixedDataObject::Get : You are requesting HG, LG "
	   << " but this MDO was not created for that. See UseHGLG(bool) "
	   << " method."
	   << endl ;
    }
  }
  else {
    adc=tdc=hg=lg= 0. ;
  }
}

//_____________________________________________________________________________
Float_t emcMixedDataObject::GetADC(Int_t index) const 
{
  // Returns ADC value or -1 is towerindex is not valid.

  if (index>=0) {
    return fADC[index] ; 
  }
  else {
    return 0. ; 
  }
} 

//_____________________________________________________________________________
Float_t emcMixedDataObject::GetHG(Int_t index) const 
{
  // Returns HG value or -1 is towerindex is not valid.

  if (!IsUsingHGLG()) {
    cerr << "<E> emcMixedDataObject::GetHG : I am not using HGLG" << endl ;
    return -1 ;
  }

  if (index>=0) {
    return fHG[index] ; 
  }
  else {
    return 0. ; 
  }
}

//_____________________________________________________________________________
Float_t emcMixedDataObject::GetLG(Int_t index) const 
{
  // Returns Low Gain value or -1 if towerindex is not valid.

  if (!IsUsingHGLG()) {
    cerr << "<E> emcMixedDataObject::GetLG : I am not using HGLG" << endl ;
    return 0. ;
  }

  if (index>=0) {
    return fLG[index] ; 
  }
  else {
    return 0. ; 
  }
} 

//_____________________________________________________________________________
void emcMixedDataObject::GetPointers(float*& ADC, float*& TDC,int*& KEY) 
{
  // WARNING. USE THIS METHOD IS STRONGLY DISCOURAGED. USE AT YOUR OWN RISK !
  ADC = fADC ;
  TDC = fTDC ;
  KEY = fDataMap ;
}

//_____________________________________________________________________________
void emcMixedDataObject::GetPointers(float*& ADC, float*& TDC, 
		   float*& HG, float*& LG, int*& KEY) 
{
  // WARNING. USE THIS METHOD IS STRONGLY DISCOURAGED. USE AT YOUR OWN RISK !
  ADC = fADC ;
  TDC = fTDC ;
  if (IsUsingHGLG()) {
    LG = fLG ;
    HG = fHG ;
  }
  else {
    cerr << "<E> emcMixedDataObject::GetPointers : You are requesting HG, LG "
	 << " but this MDO was not created for that. See UseHGLG(bool) "
	 << " method."
	 << endl ;
  }
  KEY = fDataMap ;
}

//_____________________________________________________________________________
Float_t emcMixedDataObject::GetTDC(Int_t index) const 
{
  // Returns TDC value or -1 if towerindex is not valid.

  if (index>=0) {
    return fTDC[index] ; 
  }
  else {
    return 0. ; 
  }
} 

//_____________________________________________________________________________
TString emcMixedDataObject::Status() 
{
  TString status ; 
  TString blank = "     " ; 
  status = "emcMixedDataObject status : \n" ;
  status += blank ; 
  if ( IsPedestalSubtracted() ) 
    status += "Pedestals have been subtracted\n" ; 
  else
    status += "Pedestals have NOT been subtracted\n" ;

  status += blank ; 
  if ( IsHLRatioReal() ) 
    status += "Real H/L ratios have been applied\n" ; 
  else
    status += "H/L ratios = 16 have been applied\n" ; 

  return status ; 
}

//_____________________________________________________________________________
void emcMixedDataObject::Set(int index,  float adc, float tdc)
{
  /* Set the data for a given tower, referenced by its index.
     Caller must insure that index < GetMaxSize()
   */
  assert (ValidIndex(index)) ;

  if ( index+1 > fSize) 
    fSize = index+1 ;

  fADC[index]        = adc ;
  fTDC[index]        = tdc ; 

}

//_____________________________________________________________________________
void emcMixedDataObject::Set(int index,  float adc, float tdc,
			     float hg, float lg)
{
  /* Set the data for a given tower, referenced by its index.
     Caller must insure that index < GetMaxSize()
   */
  assert (ValidIndex(index)) ;

  if ( index+1 > fSize) 
    fSize = index+1 ;

  fADC[index]        = adc ;
  fTDC[index]        = tdc ; 
  fHG[index] = hg ;
  fLG[index] = lg ;
}

//_____________________________________________________________________________
void emcMixedDataObject::SetToZero(int index)
{
  if (IsUsingHGLG()) {
    Set(index,0.0,0.0,0.0,0.0) ;
  }
  else {
    Set(index,0.0,0.0) ;
  }
}

//_____________________________________________________________________________
bool emcMixedDataObject::IsZero(Int_t index) const 
{
  /* Tells if a channel is zero (zero suppressed by DCM's). Will return true also if   index is not valid. */

  if (!ValidIndex(index)) return true ;
 
  if ( fADC[index] == 0.0 ) return true ;
  return false ;
}

//_____________________________________________________________________________
void emcMixedDataObject::SetMaxSize(Int_t thesize)
{
  /** Sets the maximum size of this object. 
      If thesize is different from previous fMaxSize, 
      all the arrays are deleted and then re-allocated.
  */

  if ( fMaxSize != thesize) {
    // Happens when there is a new configuration file
    delete[] fADC ;
    delete[] fTDC ;
    delete[] fHG ;
    delete[] fLG ;

    fMaxSize   = thesize ;
    fSize = 0 ;
    if(thesize){
      fADC        = new Float_t[fMaxSize] ;
      fTDC        = new Float_t[fMaxSize] ;
      if (IsUsingHGLG()) {
	fHG        = new Float_t[fMaxSize] ;
	fLG        = new Float_t[fMaxSize] ;
      }
    } else {
      fADC = fTDC = fHG = fLG = 0;
    }
    fPedestal = fHLRatio = false ; 
  }
}

//_____________________________________________________________________________
void emcMixedDataObject::Streamer(TBuffer&)
{
   // Stream an object of class emcMixedDataObject.

  cout << "<W> emcMixedDataObject::Streamer disabled for the moment. Contact aphecetc@in2p3.fr." << endl ;
  exit(1);
  //  if (R__b.IsReading()) { // reading part
    // Version_t R__v = R__b.ReadVersion(); if (R__v) { }
//     emcDataObject::Streamer(R__b); // stream the mother class
//     R__b >> fPedestal ; 
//     R__b >> fHLRatio ; 

//     delete[] fADC ;
//     delete[] fTDC ;

//     // we allocate enough space for consistency...
//     fADC        = new Float_t[fMaxSize] ;
//     fTDC        = new Float_t[fMaxSize] ;
    
//     // ... but we read only what is on disk = non-zero towers
//     R__b.ReadFastArray(fADC,fSize);
//     R__b.ReadFastArray(fTDC,fSize);

//     // It was decided that HG and LG values are not kept,
//     // so we tell here that we do not use HG LG values
//     UseHGLG(false) ;
    
//   } else { // writing part
//     R__b.WriteVersion(emcMixedDataObject::IsA());
//     emcDataObject::Streamer(R__b); // stream the mother class
//     R__b << fPedestal ; 
//     R__b << fHLRatio ; 

//     // we write only non-zero towers
//     R__b.WriteFastArray(fADC, fSize);
//     R__b.WriteFastArray(fTDC, fSize);
//   }
}

//_____________________________________________________________________________
void emcMixedDataObject::UseHGLG(bool use)
{
  // Decide if this object must take care of raw High and Low Gain values
  // (the LG and HG array will only be usable after a proper call to
  // the Set method).

  delete[] fLG ;
  delete[] fHG ;

  if ( use == true && fMaxSize) {
    fLG = new Float_t[fMaxSize] ;
    fHG = new Float_t[fMaxSize] ;    
  } else {
    fHG = fLG = 0;
  }
  
  fUseHGLG = use ;
}

//_____________________________________________________________________________
ostream& operator << (ostream& out, const emcMixedDataObject& mdo) 
{
  string head = " Tower :      arm    sector yrow   zrow   ADC    TDC    " ;
  if (mdo.IsUsingHGLG()) head += " HG     LG" ;
  head += "  ERR" ;
  head += " DeadNeighbours" ;

  int i ;
  int counter = 0 ; 
  out << head << endl ;
  out << dec ;
  Int_t arm, sector, yrow, zrow ; 
  TString ans ; 

  for (i = 0; i < mdo.GetSize(); i++) {
    out << dec ;
    counter++ ; 
    if ( counter == 30 ) {
      counter = 0 ; 
      cout << "S(top) or C(ontinue) ?  " ;
      cin >> ans ; 
      cout << endl ; 
      if ( ans == "s" || ans == "S" ) 
	break ; 
      out << head << endl ;
    }
    mdo.DecodeKey( mdo.GetSoftwareKey(mdo.fDataMap[i]), arm,sector, yrow, zrow ) ; 
    out << "#" << mdo.GetItemId(i) << " : "
	<< arm <<" "
	<< sector <<" "
	<< yrow <<" "
	<< zrow <<" * "
	<< mdo.fADC[i] <<" "
	<< mdo.fTDC[i] <<" ";
    if (mdo.IsUsingHGLG()) {
      out << mdo.fHG[i] 
	  << mdo.fLG[i] ;
    }
    out << ") " 
	<< hex << mdo.GetDataError(i) << dec << " D" 
	<< hex << mdo.GetDead(i)
	<< endl ;
  }
  
  return out ;

}
