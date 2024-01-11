// Author: Laurent Aphecetche (aphecetc@in2p3.fr)
// E.Kistenev - 04/25/00 - softkey fixed
// Y.Schutz   - 04/28/00 - derivation from emcDataObject
//-------------------------------------------------------------------------

#include "emcRawDataObject.h"
#include <iostream>
#include <iomanip>
#include <cassert>
#include "TString.h"
#include "EmcFEM.h"
#include <cstdlib>

ClassImp(emcRawDataObject)

  using namespace std;

//_____________________________________________________________________________
emcRawDataObject::emcRawDataObject() : emcDataObject()
{
  fTAC = fHGPost = fLGPost = fHGPre = fLGPre = 0;
  fAMUcells = 0;
  fOwnAllocation = false;
}

//_____________________________________________________________________________
emcRawDataObject::emcRawDataObject(Int_t    thesize,
				   int    * dataMap,
				   Float_t* tac,
				   Float_t* hgpost, Float_t* lgpost,
				   Float_t* hgpre, Float_t* lgpre,
				   const cells* amucells,
				   int        * dataerrors)
{
  if (thesize%144==0) {
    fNumberOfWordsPerFEM = 144;
  }
  else if (thesize%192==0) {
    fNumberOfWordsPerFEM = 192;
  } 
  else {
    assert (0==1);
  }

  fOwnAllocation = false;
  fMaxSize    = fSize = thesize;
  fTAC        = tac;
  fHGPost     = hgpost;
  fLGPost     = lgpost;
  fHGPre      = hgpre;
  fLGPre      = lgpre;
  fDataMap    = dataMap;
  fDataErrors = dataerrors;
  fAMUcells   = const_cast<cells*>(amucells);

  fSoftKey    = new long [fMaxSize];
  //  if DataMap is present - convert it into Software Keys and fIndexMap
  if(fDataMap){
    for (int Item = 0; Item<thesize; Item++) {
      fSoftKey[Item] = GenerateSoftwareKey(fDataMap[Item]);
      fIndexMap[fDataMap[Item]]=Item;
    }
  }
  else {
    cout << "<W> RDO : fDataMap not present. Cannot compute IndexMap" << endl;
  }
}

//_____________________________________________________________________________
emcRawDataObject::emcRawDataObject(const emcRawDataObject&) : emcDataObject()
{
  std::cerr << __FILE__ << ":" << __LINE__ << "METHOD IS NOT IMPLEMENTED!!"
	    << std::endl;
  exit(1);
}

//_____________________________________________________________________________
emcRawDataObject&
emcRawDataObject::operator=(const emcRawDataObject&)
{
  std::cerr << __FILE__ << ":" << __LINE__ << "METHOD IS NOT IMPLEMENTED!!"
	    << std::endl;
  return *this;
}

//_____________________________________________________________________________
emcRawDataObject::~emcRawDataObject()
{
  if (fOwnAllocation) {
    delete[] fTAC;
    delete[] fHGPost;
    delete[] fLGPost;
    delete[] fHGPre;
    delete[] fLGPre;
    delete[] fAMUcells;
    delete[] fDataMap;
  }
  // By default - This object owns SoftwareKeys only 
  if(fSoftKey) delete [] fSoftKey;
}
//_____________________________________________________________________________

void emcRawDataObject::SetCells(int iSM, int ctac, int cpre, int cpost){
  fAMUcells[iSM].tac  = ctac;
  fAMUcells[iSM].pre  = cpre;
  fAMUcells[iSM].post = cpost;
}
//_____________________________________________________________________________
void emcRawDataObject::GetCells(Int_t    index,
			   int& amupre, int& amupost, int& amutac) const
{
    amupre  = fAMUcells[index/fNumberOfWordsPerFEM].pre;
    amupost = fAMUcells[index/fNumberOfWordsPerFEM].post;
    amutac  = fAMUcells[index/fNumberOfWordsPerFEM].tac;
}
//_____________________________________________________________________________
int emcRawDataObject::GetTACCell(Int_t    index) const
{
    return  fAMUcells[index/fNumberOfWordsPerFEM].tac;
}

//_____________________________________________________________________________

void emcRawDataObject::Set(const int  index,
			   const int  & tac,
			   const int  & hgpost, const int& lgpost,
			   const int  & hgpre,  const int& lgpre,
			   const int  & dataerror){
    fTAC[index]        = (float)tac; 
    fHGPost[index]     = (float)hgpost;
    fHGPre[index]      = (float)hgpre;
    fLGPost[index]     = (float)lgpost;
    fLGPre[index]      = (float)lgpre;
    fDataErrors[index] = dataerror;
}

//_____________________________________________________________________________
void emcRawDataObject::Get(Int_t    index,
			   Float_t& tac,
			   Float_t& hgpost, Float_t& lgpost,
			   Float_t& hgpre, Float_t& lgpre,
			   int& amupre, int& amupost, int& amutac,
			   int& dataerror) const
{
  //  if (ValidIndex(index)) {
    tac     = fTAC[index];
    hgpost  = fHGPost[index];
    hgpre   = fHGPre[index];
    lgpost  = fLGPost[index];
    lgpre   = fLGPre[index];
    amupre  = fAMUcells[index/fNumberOfWordsPerFEM].pre;
    amupost = fAMUcells[index/fNumberOfWordsPerFEM].post;
    amutac  = fAMUcells[index/fNumberOfWordsPerFEM].tac;
    dataerror = fDataErrors[index];
//    }
//    else {
//      cerr << "<E> emcRawDataObject::Get - index out of bounds" << endl;
//    }
}

//_____________________________________________________________________________
void emcRawDataObject::Get(Int_t    index,
			   Float_t& tac,
			   Float_t& hgpost, Float_t& lgpost,
			   Float_t& hgpre, Float_t& lgpre,
			   int& dataerror) const
{
  //  if (ValidIndex(index)) {
    tac     = fTAC[index];
    hgpost  = fHGPost[index];
    hgpre   = fHGPre[index];
    lgpost  = fLGPost[index];
    lgpre   = fLGPre[index];
    dataerror = fDataErrors[index];
//    }
//    else {
//      cerr << "<E> emcRawDataObject::Get - index out of bounds" << endl;
//    }
}

//_____________________________________________________________________________
int emcRawDataObject::GetIndexByTowerId(int towerid) const
{
  map<int,int>::const_iterator p;
  p = fIndexMap.find(towerid);
  if (p!=fIndexMap.end()) {
    return p->second;
  }
  else {
    return -1;
  }
}
//_____________________________________________________________________________
void emcRawDataObject::resetAMUAddresses(const int fem) const
{
  fAMUcells[fem].pre = 0;
  fAMUcells[fem].post= 0;
  fAMUcells[fem].tac = 0;
  
}

//_____________________________________________________________________________
bool emcRawDataObject::IsZero(Int_t index) const 
{
  /* Tells if a channel is zero (zero suppressed by DCM's). Will return true also if   index is not valid. */

  //  if (!ValidIndex(index)) return true;
 
  //  float zero = 4095.0;

//      if ( fTAC[index] != zero ||
//           fLGPost[index] != zero ||
//           fHGPost[index] != zero ||
//           fLGPre[index] != zero ||
//           fHGPre[index] != zero 
//  	 ) 
      return ((fDataErrors[index]&0x2000)? true : false);;
      //  return true;
}

//_____________________________________________________________________________
void emcRawDataObject::Streamer(TBuffer &R__b)
{
   // Stream an object of class emcRawDataObject.

   int i;

   if (R__b.IsReading()) {
      Version_t R__v = R__b.ReadVersion(); if (R__v) { }

      emcDataObject::Streamer(R__b);

      R__b >> fNumberOfWordsPerFEM;

      if (fOwnAllocation) {
	delete[] fTAC;
	delete[] fHGPost;
	delete[] fLGPost;
	delete[] fHGPre;
	delete[] fLGPre;
	delete[] fAMUcells;
      }

      fOwnAllocation = true;

      fTAC    = new Float_t[fSize];
      fHGPost = new Float_t[fSize];
      fLGPost = new Float_t[fSize];
      fHGPre  = new Float_t[fSize];
      fLGPre  = new Float_t[fSize];

      Byte_t* fAMUPre  = new Byte_t[fSize/fNumberOfWordsPerFEM];
      Byte_t* fAMUPost = new Byte_t[fSize/fNumberOfWordsPerFEM];
      Byte_t* fAMUTAC  = new Byte_t[fSize/fNumberOfWordsPerFEM];

      R__b.ReadFastArray(fTAC,fSize);
      R__b.ReadFastArray(fHGPost,fSize);
      R__b.ReadFastArray(fLGPost,fSize);
      R__b.ReadFastArray(fHGPre,fSize);
      R__b.ReadFastArray(fLGPre,fSize);
      
      R__b.ReadFastArray(fAMUPre,fSize/fNumberOfWordsPerFEM);
      R__b.ReadFastArray(fAMUPost,fSize/fNumberOfWordsPerFEM);
      R__b.ReadFastArray(fAMUTAC,fSize/fNumberOfWordsPerFEM);

      fAMUcells = new cells[fSize/fNumberOfWordsPerFEM];

      /* We fill the fAMUcells structures. You may ask why
	 we do this strange yo-yo between "struct cells" and
	 plain Byte_t arrays... The answer is : to allow
	 the RawDataObject to point directly to arrays that are
	 created by other object (EMCalFEE in this case), i.e.
	 it is the responsability of the RDO to adapt to EMCalFEE,
	 and not the other way around...
       */
      for ( i = 0; i < fSize/fNumberOfWordsPerFEM; i++ ) {
	fAMUcells[i].pre  = fAMUPre[i];
	fAMUcells[i].post = fAMUPost[i];
	fAMUcells[i].tac  = fAMUTAC[i];
      }

      delete[] fAMUPre;
      delete[] fAMUPost;
      delete[] fAMUTAC;

      /* FIXME: this is weird, and should be changed in some way...
	 We hereby try to reconstruct the fDataMap, assuming that
	 the emcRawDataAccessor has already been properly set up (i.e.
	 with the same configuration file that was used to store this
	 object...) : this is probably a dangerous assumption, but I
	 don't know how to do it otherwise. 
       */
      
   } else {
     
      R__b.WriteVersion(emcRawDataObject::IsA());

      emcDataObject::Streamer(R__b);

      R__b << fNumberOfWordsPerFEM;

      R__b.WriteFastArray(fTAC, fSize);
      R__b.WriteFastArray(fHGPost, fSize);
      R__b.WriteFastArray(fLGPost, fSize);
      R__b.WriteFastArray(fHGPre, fSize);
      R__b.WriteFastArray(fLGPre, fSize);

      Byte_t* fAMUPre = new Byte_t[fSize/fNumberOfWordsPerFEM];
      Byte_t* fAMUPost = new Byte_t[fSize/fNumberOfWordsPerFEM];
      Byte_t* fAMUTAC = new Byte_t[fSize/fNumberOfWordsPerFEM];

      for ( i = 0; i < fSize/fNumberOfWordsPerFEM; i++ ) {
	fAMUPre[i] = fAMUcells[i].pre;
	fAMUPost[i] = fAMUcells[i].post;
	fAMUTAC[i] = fAMUcells[i].tac;
      }

      R__b.WriteFastArray(fAMUPre, fSize/fNumberOfWordsPerFEM);
      R__b.WriteFastArray(fAMUPost, fSize/fNumberOfWordsPerFEM);
      R__b.WriteFastArray(fAMUTAC, fSize/fNumberOfWordsPerFEM);

      delete[] fAMUPre;
      delete[] fAMUPost;
      delete[] fAMUTAC;
   }
}

//_____________________________________________________________________________
ostream& operator << (ostream& out, const emcRawDataObject& rdo) 
{
  if ( rdo.fDataMap ) {
    int i;
    int counter = 0; 
    const char* head = " Tower :      TAC HGPost LGPost  HGPre  LGPre (amupre/post/tac) ERR";
    out << head << endl;
    out << dec;
    TString ans; 
    for (i = 0; i < rdo.GetSize(); i++) {
    counter++; 
    if ( counter == 30 ) {
      counter = 0; 
      cout << "S(top) or C(ontinue) ?  ";
      cin >> ans; 
      cout << endl; 
      if ( ans == "s" || ans == "S" ) 
	break; 
      out << head << endl;
   }
      out << "#" << rdo.fDataMap[i] << " : " 
	  << rdo.fTAC[i]<<" "
	  << rdo.fHGPost[i]<<" "
	  << rdo.fLGPost[i]<<" "
	  << rdo.fHGPre[i]<<" "
	  << rdo.fLGPre[i]<<" "
	  << " (" << rdo.GetAMUPre(i)
	  << "/"  << rdo.GetAMUPost(i)
	  << "/"  << rdo.GetAMUTAC(i)
	  << ") " 
	  << "       |"  << hex << rdo.GetDataError(i) << dec 
	  << endl;
    }
  }
  return out;
}



