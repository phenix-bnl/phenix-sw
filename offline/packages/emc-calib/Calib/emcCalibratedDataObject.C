// $Id: emcCalibratedDataObject.C,v 1.25 2015/01/28 08:00:13 mazsi Exp $
//-------------------------------------------------------------------------
//
// Package: Calib
// 
// Copyright (C) PHENIX collaboration, 1999-2001
//
// Implementation of class : emcCalibratedDataObject
//
//-------------------------------------------------------------------------

#include "emcCalibratedDataObject.h"
#include <iomanip>
#include <cassert>
#include "TMath.h"
#include "EmcIndexer.h"

ClassImp(emcCalibratedDataObject)

  using namespace std;

//_____________________________________________________________________________
emcCalibratedDataObject::emcCalibratedDataObject() : PHObject()
{
  fEnergy = 0 ;
  fETotal = 0 ;
  fTime = 0 ;
  fTowerId = 0 ;
  fSoftwareKey = 0;
  fDeadNeighbours = 0 ;
  fEnergyCalibrated = fTimeCalibrated = fZeroSuppressed = false ; 
  fSize = fMaxSize = 0 ;
  fReadOnly = false ;

  // MV 2001/12/08
  fWarnNeighbours=0;
}

//_____________________________________________________________________________
emcCalibratedDataObject::emcCalibratedDataObject(const emcCalibratedDataObject& cdo) : PHObject()
{
  cdo.copyTo(*this) ;
}

//_____________________________________________________________________________
emcCalibratedDataObject&
emcCalibratedDataObject::operator=(const emcCalibratedDataObject& cdo)
{
  if ( &cdo != this )
    {
      delete[] fEnergy;
      delete[] fTime;
      delete[] fTowerId;
      delete[] fDeadNeighbours;
      delete[] fWarnNeighbours;
      delete[] fSoftwareKey;

      fEnergy = NULL;
      fTime = NULL;
      fTowerId = NULL;
      fDeadNeighbours = NULL;
      fWarnNeighbours = NULL;
      fSoftwareKey = NULL;

      cdo.copyTo(*this);
    }
  return *this;
}
//_____________________________________________________________________________
void emcCalibratedDataObject::copyTo(emcCalibratedDataObject& cdo) const
{
  // Copy this into cdo

  //  TObject::Copy(cdo) ;

  cdo.fMaxSize = fMaxSize ;
  cdo.fSize = fSize ;
  cdo.fEnergyCalibrated = fEnergyCalibrated ;
  cdo.fTimeCalibrated = fTimeCalibrated ;
  cdo.fZeroSuppressed = fZeroSuppressed ;

  Int_t i ;

  cdo.fEnergy = new Float_t[fMaxSize] ;
  cdo.fTime = new Float_t[fMaxSize] ;
  cdo.fTowerId = new Int_t[fMaxSize] ;
  cdo.fDeadNeighbours = new Int_t[fMaxSize] ;
  cdo.fWarnNeighbours = new Int_t[fMaxSize] ;
  cdo.fSoftwareKey = new long[fMaxSize] ;

  for ( i = 0 ; i < fSize ; i++ ) {
    cdo.fEnergy[i] = fEnergy[i] ;
    cdo.fTime[i] = fTime[i] ;
    cdo.fTowerId[i] = fTowerId[i] ;
    cdo.fDeadNeighbours[i] = fDeadNeighbours[i] ;

    // MV 2001/12/08
    cdo.fWarnNeighbours[i]=fWarnNeighbours[i];
  }

  // (re-)build index map and Software Key -
  cdo.fIndexMap.clear() ;
  for ( i = 0 ; i < fSize ; i++ ) {
    cdo.fIndexMap[ cdo.fTowerId[i] ] = i ;
    cdo.fSoftwareKey[i] = GenerateSoftwareKey( cdo.fTowerId[i] ) ;
  }

  cdo.ComputeTotalEnergy() ;
}

//_____________________________________________________________________________
emcCalibratedDataObject::~emcCalibratedDataObject()
{
  delete[] fEnergy ;
  delete[] fTime ;
  delete[] fTowerId ; 
  delete[] fSoftwareKey ;
  delete[] fDeadNeighbours ;

  // MV 2001/12/08
  delete [] fWarnNeighbours;
}

//_____________________________________________________________________________
emcCalibratedDataObject& emcCalibratedDataObject::operator += (const emcCalibratedDataObject& obj)
{
  /* To be able to add obj to this, we assume that both objects have
     the same fMaxSize, and that they have the same Zero-Suppression 
     status */
  
  if ( fZeroSuppressed != obj.fZeroSuppressed ) {
    cerr << "<W> emcCalibratedDataObject::operator+= : "
	 << " different fZeroSuppressed !"
	 << " Operation aborted (object unchanged)" 
	 << endl ;
    return *this ; // do nothing

  }

  // Save the ReadOnly flag
  bool ro = fReadOnly ;

  // Allow this object to be modified whatever 
  // its actual ReadOnly flag is.
  fReadOnly = false ;

  Int_t i ; 
  Int_t loop = obj.GetSize() ;
  Int_t twrIndex = 0 ;
  Int_t index = 0 ;
  float energy = 0.;
  float thetime = 0.;
  int neighbour = 0 ;
  int neighbourWarn = 0; 
  long softkey = 0;
  int towerid = 0 ;

  for ( i = 0 ; i < loop ; i++ ) {

    twrIndex = obj.GetTowerId(i) ;
    index = GetIndexFromTowerId(twrIndex) ;
    
    if (index==-1) {
      // new tower
      Set(fSize,obj.GetTowerId(i),obj.GetSoftwareKey(i),0,
	  obj.GetEnergy(i),obj.GetTime(i),obj.GetDead(i), obj.GetWarn(i)) ;
    }
    else {
      // merge 2 signals into one tower
      energy = obj.fEnergy[i] + fEnergy[index] ;
      // we take the minimum of the TOFs as the overlap time tower.
      // *Coarse* approx. for Run-2 LED mode. 
      // *Wrong* approx. for Run-1 CDF mode (min. TOF was somewhat shifted towards the later TOF)
      thetime = TMath::Min(obj.fTime[i],fTime[index]) ;
      neighbour = ( obj.GetDead(i) | GetDead(index) ) ;
      neighbourWarn = (obj.GetWarn(i) | GetWarn(index));

      // those 2 guys should be equal but in the case one of the cdo's does not have all data members set
      // this shouldn't happen though ...
      softkey = (GetSoftwareKey(index)) ? GetSoftwareKey(index) : obj.GetSoftwareKey(i); 
      towerid = (GetTowerId(index)) ? (GetTowerId(index)) : obj.GetTowerId(i);

      // if both cdo's do not have all data members set.
      // this shouldn't happen though ...
      if (!softkey)  softkey = GenerateSoftwareKey(index);

      Set(index,towerid,softkey,0,energy,thetime,neighbour,neighbourWarn) ;
    }
  }

  // (re-)build index map and Software Key -
  Update();

  // Restore the ReadOnly flag.
  fReadOnly = ro ;

  return *this ;
}

//_____________________________________________________________________________
// emcCalibratedDataObject emcCalibratedDataObject::operator + (const emcCalibratedDataObject& obj) const
// { 
//   (*this)+= obj; 
//   return *this ;
// }

//_____________________________________________________________________________
Int_t emcCalibratedDataObject::CountHits(Float_t twrEThreshold) const
{
  int hits  = 0 ;
  if (fEnergyCalibrated) {
    int i ;
    for ( i = 0 ; i < fSize ; i++ ) hits += ( fEnergy[i] > twrEThreshold ) ;
  }
  return hits ;
}

//_____________________________________________________________________________
Float_t emcCalibratedDataObject::ComputeTotalEnergy()  
{
  fETotal = 0 ;
  if (fEnergyCalibrated) {
    fETotal = 0 ;
    int i ;
    for ( i = 0 ; i < fSize ; i++) fETotal += fEnergy[i] ;
    return fETotal;
  } else { return 0.;}   
}

//_____________________________________________________________________________
void emcCalibratedDataObject::DecodeKey(long key, Int_t& arm, Int_t& sector, Int_t& yrow, Int_t& zrow) const
{
  arm    = key / 100000 ; 
  sector = ( key - arm * 100000 ) / 10000 ; 
  yrow   = ( key - arm * 100000 - sector * 10000 ) / 100 ; 
  zrow   = key - arm * 100000 - sector * 10000 - yrow * 100 ; 
}

//_____________________________________________________________________________
long emcCalibratedDataObject::GenerateSoftwareKey(Int_t ItemId) const
{

  int iS ;   // sector number (0-7)
  int iSMT ; // tower_index number within Sector
  int iarm ; // arm:0=West, 1=East;
  int iy ;   // row number of the tower_index within a sector (bottom=>row=0)
  int iz ;   /* column number of the tower_index within a sector. column=0 for
                lower bottom left tower_index, when sector is viewed from back 
                (i.e. you are looking at the electronics).
              */
  int swkey ; /* software key = 
                 100000 * iarm +
                  10000 * iS +
                    100 * iy +
                          iz
               */

  EmcIndexer::iPXiSiST(ItemId,iS,iSMT) ; 

  iarm = ((iS<4)?0:1) ;
  if (iS<6) {
    iy = iSMT/72 ;
    iz = iSMT%72 ;
  }
  else {
    iy = iSMT/96 ;
    iz = iSMT%96 ; 
  }
  
  if(iS>=4){
    // change sector numbers to comply with PHENIX geography
    iS = ((iS -=6)<0)? iS+4 : iS;
  }
  swkey = 100000*iarm + 10000*iS + 100*iy + iz ;
       
  return swkey ;
}


//_____________________________________________________________________________
void emcCalibratedDataObject::Get (Int_t index, 
				   Float_t& energy, Float_t& tof) const
{
  /* Get energy, tof using internal indexing.
     If index is not valid, energy=tof=-1
  */
  if ( ValidIndex(index) ) {
    tof        = fTime[index] ;
    energy     = fEnergy[index] ;
  }
  else {
    energy = tof = 0 ;
  }
}

//_____________________________________________________________________________
void emcCalibratedDataObject::Get( Int_t index, 
				   Int_t& TowerId, long & softwareKey, 
				   Int_t & dummy, 
				   Float_t& energy, Float_t& tof ) const
{
  /* Get energy, tof and all Tower primitives using internal indexing.
     If index is not valid, energy=tof=-1
  */ 
  if ( ValidIndex(index) ) {
    energy        = fEnergy[index] ;
    tof           = fTime[index] ;
    TowerId       = fTowerId[index] ; 
    softwareKey   = fSoftwareKey[index] ;
    dummy = 0 ;
  }
  else {
    energy = tof = 0 ;
  }
}

//_____________________________________________________________________________
void emcCalibratedDataObject::GetByTowerId( Int_t TowerId,  
					  Float_t& energy, Float_t& tof ) const
{
  // Returns energy for a given tower or -1 if towerindex is not valid.
  Int_t index = GetIndexFromTowerId(TowerId) ;
  if (index>=0) {
    energy =  fEnergy[index] ; 
    tof    =  fTime[index];
  } else {
    energy = 0.;
    tof    = 0.;
  }
}

//_____________________________________________________________________________
Int_t emcCalibratedDataObject::GetDead(Int_t index) const
{
  Int_t dead = 0 ;

  if ( ValidIndex(index) ) dead = fDeadNeighbours[index] ;

  return dead ;
}

//_____________________________________________________________________________
Int_t emcCalibratedDataObject::GetWarn(Int_t index) const
{
  // MV 2001/12/08

  Int_t warn=0;

  if(ValidIndex(index)) warn=fWarnNeighbours[index] ;

  return warn;
}

//_____________________________________________________________________________
Int_t emcCalibratedDataObject::GetDeadByTowerId(Int_t TowerId) const
{
  // Returns energy for a given tower or 0 if towerindex is not valid.
  Int_t index = GetIndexFromTowerId(TowerId) ;

  if (index>=0) {
    return fDeadNeighbours[index] ; 
  }
  else {
    return 0 ; 
  }
}

//____________________________________________________________________________
Int_t emcCalibratedDataObject::GetWarnByTowerId(Int_t TowerId) const
{
  // MV 2001/12/08

  Int_t index = GetIndexFromTowerId(TowerId) ;

  if (index>=0) {
    return fWarnNeighbours[index] ; 
  }
  else {
    return 0 ; 
  }
}

//_____________________________________________________________________________
Float_t emcCalibratedDataObject::GetEnergy(Int_t index) const
{
  Float_t energy = 0 ;

  if (ValidIndex(index)) energy = fEnergy[index] ;

  return energy ;
}

//_____________________________________________________________________________
Float_t emcCalibratedDataObject::GetEnergyByTowerId(Int_t TowerId) const 
{
  // Returns energy for a given tower or 0 if towerindex is not valid.
  Int_t index = GetIndexFromTowerId(TowerId) ;

  if (index>=0) {
    return fEnergy[index] ; 
  }
  else {
    return 0.; 
  }
} 

//_____________________________________________________________________________
Int_t emcCalibratedDataObject::GetIndexFromTowerId(Int_t TowerId) const
{
  // Returns the index (of fADC, fTDC, etc.. arrays) from towerindex.
  map<int,int>::const_iterator p = fIndexMap.find(TowerId) ;
  if (p!=fIndexMap.end()) {
    return p->second ;
  }
  else {
    return -1;
  }
}

//_____________________________________________________________________________
void emcCalibratedDataObject::GetPointers(float*& ENERGY, float*& TOF) 
{
  // WARNING. USE THIS METHOD IS STRONGLY DISCOURAGED. USE AT YOUR OWN RISK !
  ENERGY = fEnergy ;
  TOF = fTime ;
}


//_____________________________________________________________________________
void emcCalibratedDataObject::GetSectorEnergies(Float_t * e)
{
  // Loop over towers
  int i ;
  for (i=0;i<8;i++) e[i]=0;
  for (int index = 0; index < GetSize(); index++ ) {
    int is = 4*(fSoftwareKey[index]/100000) +  
      (fSoftwareKey[index]%100000)/10000 ;
    e[is] += fEnergy[index];
  }
}

//_____________________________________________________________________________
void emcCalibratedDataObject::GetSMEnergies(Float_t* e)
{
  int i ;
  for (i=0;i<GetMaxSize()/144;i++) e[i]=0;
  int towerid ;
  int sector ;
  int sm144,sm144sector ;
  int st ;
  int sm144t ;

  for ( i = 0 ; i < GetSize() ; i++) {
    
    towerid = fTowerId[i] ;

    ///  PHENIX Tower number                -> Sector, i(Sector Tower)
    EmcIndexer::iPXiSiST(towerid,sector,st) ;

    /// SectorNumber,SectorTower           -> SM144 NUmber (Sector scope), SM144 Tower
    EmcIndexer::iSiSTiSMiSMT(sector,st,sm144sector,sm144t) ;

    ///  Sector, SM144 Number(Sector Scope) -> PHENIX SM144 (1-172)
    sm144 = EmcIndexer::iSiSM144_PXSM144(sector , sm144sector);

    e[sm144] += fEnergy[i] ;
  }
}

//_____________________________________________________________________________
long emcCalibratedDataObject::GetSoftwareKey(Int_t index) const
{
  long softkey = 0 ;

  if (ValidIndex(index)) softkey = fSoftwareKey[index] ;

  return softkey ;
}

//_____________________________________________________________________________
Float_t emcCalibratedDataObject::GetTime(Int_t index) const
{
  Float_t t = 0 ;
  
  if (ValidIndex(index)) t = fTime[index] ;

  return t ;
}

//_____________________________________________________________________________
Float_t emcCalibratedDataObject::GetTimeByTowerId(Int_t TowerId) const 
{
  // Get tof value or -1 if towerindex is not valid.
  Int_t index = GetIndexFromTowerId(TowerId) ;
 
  if (index>=0) {
    return fTime[index] ; 
  }
  else {
    return 0.; 
  }
} 

//_____________________________________________________________________________
Int_t emcCalibratedDataObject::GetTowerId(Int_t index) const
{
  Int_t towerId = 0 ;

  if (ValidIndex(index)) towerId = fTowerId[index] ;

  return towerId ;
}

//_____________________________________________________________________________
void emcCalibratedDataObject::identify(ostream& out) const
{
  out << "I am an emcCalibratedDataObject" << endl ;
}

//_____________________________________________________________________________
Bool_t emcCalibratedDataObject::IsCalibrated(TString what) const 
{
  if ( what == "energy" )
    return fEnergyCalibrated ;
  else if ( what == "time" )
    return fTimeCalibrated ;
  else {
    cout << "emcRawDataCalibrator::IsCalibrated > " << what << " is a wrong option" << endl ;
    return kFALSE ;
  }  
}

//_____________________________________________________________________________
void emcCalibratedDataObject::Set(int index, int TowerId, long softwareKey, 
				  int /*dummy*/, float energy, float time,
				  Int_t deadNeighbours, Int_t warnNeighbours)
{
  // MV 2001/12/08 added warnNeighbours

  if (fReadOnly) {
    cerr << "<W> emcCalibratedDataObject::Set(all stuff) : Object is read-only" << endl ;
  }
  else {

    if (index>=fMaxSize) {
      assert(fSize==fMaxSize) ;
      if (!fMaxSize) {
	// Very first allocation
	fMaxSize = 1000 ;
	fEnergy = new Float_t[fMaxSize] ;
	fTime = new Float_t[fMaxSize] ;
	fTowerId = new Int_t[fMaxSize] ;
	fSoftwareKey = new long[fMaxSize] ;
	fDeadNeighbours = new Int_t[fMaxSize] ;

	// MV 2001/12/08
	fWarnNeighbours=new Int_t[fMaxSize];

	int i ;
	for (i=0;i<fMaxSize;i++) {
	  fEnergy[i]=fTime[i]=0;
	  fTowerId[i]=fDeadNeighbours[i]=0;
	  fSoftwareKey[i]=0;
	}
      }
      else {
	int verymaxsize = 25000 ;
	int oldsize = fMaxSize ; 
	// Double allocation, up to verymaxsize.
	fMaxSize = TMath::Min(verymaxsize,fMaxSize*2) ;
	
	Float_t* newEnergy = new Float_t[fMaxSize] ;
	Float_t* newTime = new Float_t[fMaxSize] ;
	Int_t* newTowerId = new Int_t[fMaxSize] ;
	long* newSoftwareKey = new long[fMaxSize] ;
	Int_t* newDeadNeighbours = new Int_t[fMaxSize] ;

	// MV 2001/12/08
	Int_t* newWarnNeighbours=new Int_t[fMaxSize];
	
	int i ;
	for (i=0;i<oldsize;i++) {
	  newEnergy[i] = fEnergy[i] ;
	  newTime[i] = fTime[i] ;
	  newTowerId[i] = fTowerId[i] ;
	  newSoftwareKey[i] = fSoftwareKey[i] ;
	  newDeadNeighbours[i] = fDeadNeighbours[i] ;

	  // MV 2001/12/08
	  newWarnNeighbours[i]=fWarnNeighbours[i];

	}
	
	delete[] fEnergy ;
	delete[] fTime ;
	delete[] fTowerId ;
	delete[] fSoftwareKey ;
	delete[] fDeadNeighbours ;

	// MV 2001/12/08
	delete [] fWarnNeighbours;
	
	fEnergy = newEnergy ;
	fTime = newTime ;
	fTowerId = newTowerId ;
	fSoftwareKey = newSoftwareKey ;
	fDeadNeighbours = newDeadNeighbours ;

	// MV 2001/12/08
	fWarnNeighbours=newWarnNeighbours;

      }
    }

    fSize = TMath::Max(fSize,index+1) ;

    fIndexMap[TowerId] = index ;
    fTowerId[index] = TowerId ;
    fSoftwareKey[index] = softwareKey ;
    fEnergy[index] = energy ;
    fTime[index] = time ;
    fDeadNeighbours[index] = deadNeighbours ;

    // MV 2001/12/08
    fWarnNeighbours[index]=warnNeighbours;

  }
}

//_____________________________________________________________________________
void emcCalibratedDataObject::SetMaxSize(Int_t thesize)
{
  Reset();
  if (!fMaxSize) {
    // Very first allocation only
    fMaxSize = thesize ;
    fEnergy = new Float_t[fMaxSize] ;
    fTime = new Float_t[fMaxSize] ;
    fTowerId = new Int_t[fMaxSize] ;
    fSoftwareKey = new long[fMaxSize] ;
    fDeadNeighbours = new Int_t[fMaxSize] ;
    fWarnNeighbours = new Int_t[fMaxSize];

    int i ;
    for (i=0;i<fMaxSize;i++) {
      fEnergy[i]=fTime[i]=0;
      fTowerId[i]=fDeadNeighbours[i]=0;
      fSoftwareKey[i]=0;
    }
  }
}


//_____________________________________________________________________________
void emcCalibratedDataObject::Reset(void)
{
  fSize=0;
  fIndexMap.clear() ;
  fEnergyCalibrated=fTimeCalibrated=false ;
}

//*************************************************************************

void emcCalibratedDataObject::resetByIndex(int index)
{
  if (index<fSize) {
      fEnergy[index]        = 0.;
      fTime[index]          = 0.;  
  }
}

//*************************************************************************

void emcCalibratedDataObject::resetByTowerId(int TowerId)
{
  Int_t index = GetIndexFromTowerId(TowerId) ;
  if(index>=0){
    fEnergy[index]        = 0.;
    fTime[index]          = 0.; 
  } 
}

//_____________________________________________________________________________
TString emcCalibratedDataObject::Status() 
{
  TString status ; 
  TString blank = "     " ; 
  status = "emcCalibratedDataObject status : \n" ;
  status += blank ; 

  if ( IsCalibrated("energy") )
    status += "Energy calibration have been applied\n" ; 
  else
    status += "Energy calibration have NOT been applied\n" ; 

  status += blank ; 

  if ( IsCalibrated("time") ) 
    status += "Time calibration have been applied\n" ; 
  else
    status += "Time calibration have NOT been applied\n" ; 

  status += blank ; 
  if ( IsZeroSuppressed()  ) 
    status += "Zero suppression has been applied\n" ; 
  else
    status += "Zero suppression has not been applied\n" ; 

  return status ; 
}

//_____________________________________________________________________________
void emcCalibratedDataObject::Streamer(TBuffer &R__b)
{
  // Stream an object of class emcCalibratedDataObject.
  
  // MV 2001/12/12 Incremented class version 2 -> 3
  
  if (R__b.IsReading()) {
    
    Version_t R__v = R__b.ReadVersion();
    
    if (R__v==1) { cerr << "CDO Streamer : Cannot read anylonger this version of CDO!" << endl;}
    
    TObject::Streamer(R__b) ;
    
    R__b >> fSize ;
    
    if ( fSize > fMaxSize) {

      // Change allocation
      delete[] fEnergy ;
      delete[] fTime ;
      delete[] fTowerId ;
      delete[] fSoftwareKey;
      delete[] fDeadNeighbours ;
      
      // MV 2001/12/08
      delete [] fWarnNeighbours;
      
      fMaxSize = fSize ;
      
      fEnergy = new Float_t[fMaxSize] ;
      fTime = new Float_t[fMaxSize] ;
      fTowerId = new Int_t  [fMaxSize] ;
      fSoftwareKey = new long [fMaxSize] ;
      fDeadNeighbours   = new Int_t  [fMaxSize] ;
      
      // MV 2001/12/08
      fWarnNeighbours=new Int_t[fMaxSize];
      
    }
    
    R__b >> fEnergyCalibrated;
    R__b >> fTimeCalibrated;
    R__b >> fZeroSuppressed;
    
    R__b.ReadFastArray(fEnergy, fSize);
    R__b.ReadFastArray(fTime, fSize);
    R__b.ReadFastArray(fTowerId, fSize);
    R__b.ReadFastArray(fDeadNeighbours, fSize);
    
    // MV 2001/12/08
    if(R__v>2){

      R__b.ReadFastArray(fWarnNeighbours, fSize);

    } else{

      memset((void*)fWarnNeighbours, 0, fSize*sizeof(Int_t));

    }
    
    // we rebuild the IndexMap (which is only used for speeding up 
    // accessing in the Get... methods) and the fSoftwareKey array.
    int i ;
    fIndexMap.clear() ;
    for ( i = 0 ; i < fSize ; i++ ) {
      fIndexMap[ fTowerId[i] ] = i ; 
      fSoftwareKey[i] = GenerateSoftwareKey( fTowerId[i] ) ;
    }
    
    for ( i = fSize ; i < fMaxSize ; i++ ) {
      fEnergy[i] = fTime[i] = 0 ;
      fTowerId[i] = 0 ;
      fSoftwareKey[i] = 0 ;
      fDeadNeighbours[i] = 0 ;
      
      // MV 2001/12/08
      fWarnNeighbours[i]=0;
      
    }
    
    ComputeTotalEnergy() ;
    
    fReadOnly = true ;
    
  } else {
    
    R__b.WriteVersion(emcCalibratedDataObject::IsA());
    TObject::Streamer(R__b);
    
    R__b << fSize ;
    
    R__b << fEnergyCalibrated ; 
    R__b << fTimeCalibrated ; 
    R__b << fZeroSuppressed ; 
    
    // we write only non-zero towers
    R__b.WriteFastArray(fEnergy, fSize) ;
    R__b.WriteFastArray(fTime, fSize) ;
    R__b.WriteFastArray(fTowerId, fSize);
    R__b.WriteFastArray(fDeadNeighbours, fSize);
    
    // MV 2001/12/08
    R__b.WriteFastArray(fWarnNeighbours, fSize);
    
  }
}

//_____________________________________________________________________________
void emcCalibratedDataObject::Update(void)
{
  Int_t i;

  fIndexMap.clear() ;
  for ( i = 0 ; i < fSize ; i++ ) {
    fIndexMap[ fTowerId[i] ] = i ;
    fSoftwareKey[i] = GenerateSoftwareKey( fTowerId[i] ) ;
  }
  ComputeTotalEnergy() ;
}

//_____________________________________________________________________________
ostream& operator << (ostream& out, const emcCalibratedDataObject& cdo) 
{
  if (cdo.fTowerId) {
    int i ;

    // MV 2001/12/08 added warnings for neighbors
    const char* head = " Tower :      arm    sector yrow   zrow   GeV     ns   DeadNeighbours   WarnNeighbours";
    out << head << endl ;
    out << dec ;
    Int_t arm, sector, yrow, zrow ; 
    TString ans ; 
    for (i = 0; i < cdo.GetSize(); i++) {
   
      if ( (i+1) % 1000 == 0 ) {
	cout << "S(top) or C(ontinue) ?  " ;
	cin >> ans ; 
	cout << endl ; 
	if ( ans == "s" || ans == "S" ) 
	  break ; 
	out << head << endl ;
      }

      cdo.DecodeKey(  cdo.GetSoftwareKey(i), arm,sector, yrow, zrow) ; 
      out << "#" << cdo.GetTowerId(i)  << " : "
	  << arm 
	  << sector 
	  << yrow 
	  << setprecision(2)
	  << zrow
	  << " " 
	  << setprecision(8) << cdo.fEnergy[i] 
	  << " "
	  << setprecision(8) << cdo.fTime[i]
	  << ") " 
	  << " D" << cdo.GetDead(i)
 
	// MV 2001/12/08
	  << " W" << cdo.GetWarn(i)
	  << endl ;
    }
  }
  return out ;
}
