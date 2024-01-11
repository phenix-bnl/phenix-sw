// Implementation of class : DataObject
//
// Author: Yves Schutz (schutz@in2p3.fr)

#include "emcDataObject.h"
#include "emcRawDataAccessor.h"
#include <iomanip>
#include <cassert>
#include "TString.h"
#include "EmcIndexer.h"

ClassImp(emcDataObject)

//_____________________________________________________________________________
emcDataObject::emcDataObject() 
{ 
  fMaxSize = 0;
  fSize    = 0; 
  fDataErrors = 0;
  fDataMap    = 0;
  fSoftKey    = 0;
  fDeadMap = 0;
  fOwnDeadMap = false;
  fReadOnly = false;

  // MV 2001/12/08
  fWarnMap=0;
  fOwnWarnMap=false;

}

//_____________________________________________________________________________
emcDataObject::~emcDataObject()
{
  if(false){
    //  FIXME: neither of these areas belonge to this object......
    delete[] fDataErrors; 
    delete[] fDataMap; 
    delete[] fSoftKey; 
  }
  if (fOwnDeadMap) delete[] fDeadMap;

  // MV 2001/12/08
  if (fOwnWarnMap) delete[] fWarnMap;
}

//_____________________________________________________________________________
void emcDataObject::Reset()
{
  fMaxSize = 0;
  fSize    = 0; 
  if(false){
    delete[] fDataErrors; 
    delete[] fDataMap; 
    delete[] fSoftKey; 
  }
  fDataErrors = 0; 
  fDataMap    = 0; 
  fSoftKey    = 0;
  fDeadMap = 0;
  fOwnDeadMap = false;
  fReadOnly = false;

  // MV 2001/12/08
  fWarnMap=0;
  fOwnWarnMap=false;
}

//_____________________________________________________________________________
void emcDataObject::SetDataDescriptionPointers(Int_t * dataMap,
					       long  * softwareKey,
					       Int_t * dataerrors)
{
  fDataMap    = dataMap;
  fSoftKey    = softwareKey;
  fDataErrors = dataerrors;
}

//_____________________________________________________________________________
void emcDataObject::GetDataDescriptionPointers(Int_t * & dataMap,
					       long  * & softwareKey,
					       Int_t * & dataerrors) const
{
  dataMap     = fDataMap;
  softwareKey = fSoftKey;
  dataerrors  = fDataErrors;
}

//_____________________________________________________________________________
void emcDataObject::SetDataDescriptionPointers(Int_t * dataMap,
					       long  * softwareKey,
					       Int_t * dataerrors,
					       Int_t * deadmap)
{
  fDataMap    = dataMap;
  fSoftKey    = softwareKey;
  fDataErrors = dataerrors;
  fDeadMap = deadmap;
}

//_____________________________________________________________________________
void emcDataObject::GetDataDescriptionPointers(Int_t * & dataMap,
					       long  * & softwareKey,
					       Int_t * & dataerrors,
					       Int_t * & deadmap) const
{
  dataMap     = fDataMap;
  softwareKey = fSoftKey;
  dataerrors  = fDataErrors;
  deadmap     = fDeadMap;
}

//_____________________________________________________________________________
void emcDataObject::SetDataDescriptionPointers(Int_t * dataMap,
					       long  * softwareKey,
					       Int_t * dataerrors,
					       Int_t * deadmap,
					       Int_t * warnmap)
{
  // MV 2001/12/08
  fDataMap    = dataMap;
  fSoftKey    = softwareKey;
  fDataErrors = dataerrors;
  fDeadMap = deadmap;
  fWarnMap=warnmap;
}

//_____________________________________________________________________________
void emcDataObject::GetDataDescriptionPointers(Int_t * & dataMap,
					       long  * & softwareKey,
					       Int_t * & dataerrors,
					       Int_t * & deadmap,
					       Int_t * & warnmap) const
{
  // MV 2001/12/08
  dataMap     = fDataMap;
  softwareKey = fSoftKey;
  dataerrors  = fDataErrors;
  deadmap     = fDeadMap;
  warnmap=fWarnMap;
}


//_____________________________________________________________________________
void emcDataObject::DecodeKey(long key, Int_t& arm, Int_t& sector, Int_t& yrow, Int_t& zrow) const
{
  arm    = key / 100000; 
  sector = ( key - arm * 100000 ) / 10000; 
  yrow   = ( key - arm * 100000 - sector * 10000 ) / 100; 
  zrow   = key - arm * 100000 - sector * 10000 - yrow * 100; 
}

//_____________________________________________________________________________
long emcDataObject::GenerateSoftwareKey(Int_t ItemId) const
{

  int iS;   // sector number (0-7)
  int iSMT; // tower_index number within Sector
  int iarm; // arm:0=West, 1=East;
  int iy;   // row number of the tower_index within a sector (bottom=>row=0)
  int iz;   /* column number of the tower_index within a sector. column=0 for
                lower bottom left tower_index, when sector is viewed from back 
                (i.e. you are looking at the electronics).
              */
  int swkey; /* software key = 
                 100000 * iarm +
                  10000 * iS +
                    100 * iy +
                          iz
               */

  EmcIndexer::iPXiSiST(ItemId,iS,iSMT); 
  // defined in the Static package of online monitoring

  iarm = ((iS<4)?0:1);
  if (iS<6) {
    iy = iSMT/72;
    iz = iSMT%72;
  }
  else {
    iy = iSMT/96;
    iz = iSMT%96; 
  }
  
  if(iS>=4){
    // change sector numbers to comply with PHENIX geography
    iS = ((iS -=6)<0)? iS+4 : iS;
  }
  swkey = 100000*iarm + 10000*iS + 100*iy + iz;
       
  return swkey;
}

//_____________________________________________________________________________
Int_t emcDataObject::GetDead(Int_t index) const
{
  if (fDeadMap && index>=0 && index<fMaxSize) {
    return fDeadMap[index];
  }
  else {
    return 0;
  }
}

//_____________________________________________________________________________
Int_t emcDataObject::GetWarn(Int_t index) const
{
  // MV 2001/12/08

  if (fWarnMap && index>=0 && index<fMaxSize) {
    return fWarnMap[index];
  }
  else {
    return 0;
  }
}

//_____________________________________________________________________________
bool emcDataObject::HasErrors(void) const
{
  bool rv = false;
  if (fDataErrors) {
    int i;
    for (i=0;i<fSize && rv==false;i++) {
      if (fDataErrors[i]) rv = true;
    }
  }
  return rv;
}


//______________________________________________________________________________
void emcDataObject::Streamer(TBuffer &R__b)
{
  // Stream an object of class emcDataObject.
  
  // MV 2001/12/12 Incremented class version 1 -> 2

  if (R__b.IsReading()) {
    
    Version_t R__v = R__b.ReadVersion(); if (R__v) { }
    TObject::Streamer(R__b);
    
    int thesize;
    R__b >> thesize;
    R__b >> fSize;
    
    if (thesize!=fMaxSize) {
      delete[] fDataErrors;
      fDataErrors = 0;
      if (fOwnDeadMap) {
	delete[] fDeadMap;
	fDeadMap = 0;
      }
      // MV 2001/12/08
      if (fOwnWarnMap) {
	delete[] fWarnMap;
	fWarnMap = 0;
      }       	
    }
    
    fMaxSize = thesize;
    
    if (!fDataErrors) fDataErrors = new Int_t[fMaxSize];
    if (!fDeadMap) {
      fDeadMap = new Int_t[fMaxSize];      
      fOwnDeadMap = true;
    }
    
    R__b.ReadFastArray(fDataErrors,fSize);
    R__b.ReadFastArray(fDeadMap,fSize);      
    fReadOnly = true;
    
    // MV 2001/12/08
    if (!fWarnMap) {
      fWarnMap = new Int_t[fMaxSize];      
      fOwnWarnMap = true;
    }
    
    if(R__v>1){
      
      R__b.ReadFastArray(fWarnMap, fSize);
      
    } else{
      
      memset((void*)fWarnMap, 0, fSize*sizeof(Int_t));
      
    }
   } else {

      R__b.WriteVersion(emcDataObject::IsA());
      TObject::Streamer(R__b);
      R__b << fMaxSize;
      R__b << fSize;

      Int_t* tmp = 0;

      // MV 2001/12/08 added fWarnMap
      if (!fDataErrors || !fDeadMap || !fWarnMap) {
	// Prepare an empty array
	tmp = new Int_t[fSize];
	int i;
	for (i=0;i<fSize;i++) tmp[i]=0;
      }

      if (fDataErrors) {
	R__b.WriteFastArray(fDataErrors,fSize);
      }
      else {
	R__b.WriteFastArray(tmp,fSize);
      }

      if (fDeadMap) {
	R__b.WriteFastArray(fDeadMap,fSize);
      }
      else {
	R__b.WriteFastArray(tmp,fSize);
      }

      // MV 2001/12/08
      if (fWarnMap) {
	R__b.WriteFastArray(fWarnMap,fSize);
      }
      else {
	R__b.WriteFastArray(tmp,fSize);
      }

      delete[] tmp;       
   }
}




