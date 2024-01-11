// $Id: emcQAs.C,v 1.11 2015/01/29 05:26:38 mazsi Exp $

#include "emcQAs.h"
#include <fstream>
#include "emcDataManager.h"
#include <string.h>
#include "EmcIndexer.h"
#include "emcCalFEM.h"
#include "emcRawDataAccessor.h"
#include "emcRawDataObject.h"
#include <fstream>
#include <cstdio>

using namespace std;

//_____________________________________________________________________________
// MV 2001/12/04 initialize static bad masks and warning masks
// for problems found from monitoring data, masks can, in principle,
// be different for warnings and errors
INT32 const emcQAs::fMoniAmplMask=0x40000;
INT32 const emcQAs::fMoniTofMask=0x0; // should be set to a sensible value!
INT32 const emcQAs::fMoniAmplWarnMask=0x0; // should be set to a sensible value!
INT32 const emcQAs::fMoniTofWarnMask=0x0; // should be set to a sensible value!

// for problems found from physics data, let's assume the masks are the same
unsigned char const emcQAs::fPhysAmplMask=0x0f;
unsigned char const emcQAs::fPhysTofMask=0xf0;

//_____________________________________________________________________________
bool emcQAs::ApplyExtraRejectList(void)
{

  // MV 2001/12/04 function completely re-written
  
  char s[256] ;
  
  int sector=-999;
  int z=-999;
  int y=-999;
  int amplError=0;
  int tofError=0;
  unsigned char physErrorBits=0;
  int amplWarning=0;
  int tofWarning=0;
  unsigned char physWarningBits=0;
  int itemId=-999;

  map<int, unsigned char>::iterator physRejectIterator, physWarningIterator;

  if(fExtraRejectListFilename=="") return true;
  
  ifstream in(fExtraRejectListFilename.c_str()) ;
  if(!in.is_open()){
    
    cerr <<"<E> emcQAs::ApplyExtraRejectList: Cannot open " 
	 <<fExtraRejectListFilename<<endl;
    return false ;
    
  }
  
  while(in.getline(s, 255, '\n')){
    
    if(s[0]=='#') continue; // skip comment lines
    if(7!=sscanf(s, "%d %d %d %d %d %d %d", &sector, &z, &y, &amplError, &tofError, &amplWarning, &tofWarning))
      continue; // skip incomplete lines

    if(EmcIndexer::IsValid(sector, z, y)){

      itemId = EmcIndexer::getTowerId(sector, z, y);
      
      physErrorBits=tofError<<4 | amplError;

      //=====> check if this channel is already in the map.
      //=====> if this is the case, do a bit-or on the error bits
      physRejectIterator=fPhysRejectMap.find(itemId);
      if(physRejectIterator!=fPhysRejectMap.end()){
	    
	physErrorBits|=fPhysRejectMap[itemId];
	
      }	    

      //=====> fill the bad map
      fPhysRejectMap[itemId]=physErrorBits;

      //=====> now deal with warnings
      physWarningBits=tofWarning<<4 | amplWarning;

      //=====> check if this channel is already in the map.
      //=====> if this is the case, do a bit-or on the warning bits
      physWarningIterator=fPhysWarningMap.find(itemId);
      if(physWarningIterator!=fPhysWarningMap.end()){
	    
	physWarningBits|=fPhysWarningMap[itemId];
	
      }	    

      //=====> fill the warning map
      fPhysWarningMap[itemId]=physWarningBits;

    } else{
      
      cerr <<"<E> emcQAs::ApplyExtraRejectList: no such tower:"<<endl
	   <<"sector="<<sector<<" z="<<z<<" y="<<y<<endl;
      
    }
    
  }
  
  in.close();
  return true;
  
}

//_____________________________________________________________________________
bool emcQAs::BuildDeadMap(void)
{
  
  // MV 2001/12/04 function (almost) completely re-written

  map<int, unsigned char>::iterator physRejectIterator, physWarningIterator;
  bool badAmpl=false;
  bool badTof=false;
  bool warnAmpl=false;
  bool warnTof=false;
  bool badMonitorAmpl=false;
  bool badMonitorTof=false;
  bool warnMonitorAmpl=false;
  bool warnMonitorTof=false;

  //=====> apply physics reject list first so that it gets fair treatment
  ApplyExtraRejectList();
  
  emcDataManager* dm = emcDataManager::GetInstance() ;

  delete[] fDeadMap ;
  fDeadMap = 0 ;

  delete [] fWarnMap;
  fWarnMap=0;

  emcRawDataAccessor* rda = emcRawDataAccessor::GetInstance() ;
  if (!rda) {
    if (dm->GetVerboseLevel()>=1) {
      cerr << "<E> emcQAs::BuildDeadMap() : Cannot access RDA" << endl ;
    }
    return false ;
  }

  emcRawDataObject* rdo = rda->GetRawDataObject() ;
 
  fDeadMap = new Int_t[rdo->GetMaxSize()] ;
  fWarnMap = new Int_t[rdo->GetMaxSize()] ;

  int i ;
  int sector ;
  int tower ;
  int x,y ;
  int neighborBad, neighborWarn;

  // Compute the neighbour error and warning flags for each tower
  //
  // For amplitude bits are:
  // ---------------------
  // |   | 18| 19| 20|   |
  // ---------------------
  // | 13| 14| 15| 16| 17|
  // ---------------------  ^ y
  // | 8 | 9 | 10| 11| 12|  |
  // ---------------------  |
  // | 3 | 4 | 5 | 6 | 7 |  |
  // ---------------------  ------> z(x)
  // |   | 0 | 1 | 2 |   |
  // ---------------------
  // as viewed from the back of the central tower (which has bit 10 set
  // to 1 if it's itself a bad module); corner towers are excluded
  //
  // For ToF bits are :
  // -------------
  // | 27| 28| 29|  ^ y
  // -------------  |
  // | 24| 25| 26|  |
  // -------------  |
  // | 21| 22| 23|  ------> z(x)
  // -------------
  // as viewed from the back of the central tower (which has bit 25 set
  // to 1 if it's itself a bad module)
  //
  // So, a channel has a problem with amlitude measurements if its neighbor
  // error bit map  satisfies this mask:
  //            0x400
  // Actually, this mask should be returned by the IamDeadMask() method
  // so that the amlitude for bad modules can be set to 0 at the calibration
  // stage.
  //
  // Some other useful masks.
  // The mask to look for amplitude errors or warnings in the 3x3 region
  // around a tower is:
  //          0x1ce70
  // In the 5x5 region:
  //         0x1fffff
  // To see if there are ToF problems for this tower:
  //        0x2000000
  
  int xoff, yoff;

  for (i=0; i<rdo->GetMaxSize(); i++){
    
    tower=rdo->GetItemId(i);

    EmcIndexer::decodeTowerId(tower, sector, x, y) ;
    
    neighborBad=0;
    neighborWarn=0;

    int neib ;
    int index ;

    int bitOffset=0;

    //=====> set amplitude flags first
    for(yoff=-2; yoff<=2; yoff++){
      for(xoff=-2; xoff<=2; xoff++){
	

	//=====> check if this is a corner tower
	bool corner=
	  (xoff==-2 && yoff==-2) ||
	  (xoff==-2 && yoff== 2) ||
	  (xoff== 2 && yoff==-2) ||
	  (xoff== 2 && yoff== 2);

	if(corner) continue;

	if(EmcIndexer::IsValid(sector, x+xoff, y+yoff)){

	  neib = EmcIndexer::getTowerId(sector, x+xoff, y+yoff) ;
	  index = rdo->GetIndexByTowerId(neib) ;

	  //=====> deal with bad channels first
	  //=====> give each channel benefit of a doubt
	  badAmpl=false;
	  badMonitorAmpl=false;
	  
	  //=====> see if this channel have errors found from physics
	  physRejectIterator=fPhysRejectMap.find(neib);
	  if(physRejectIterator!=fPhysRejectMap.end()){
	    
	    if( fPhysRejectMap[neib] & fPhysAmplMask ) badAmpl = true;
	    
	  }	    
	  
	  //=====> see if this channel have errors found from monitoring
	  if( GetError(index) & fMoniAmplMask ) badMonitorAmpl = true;
	  
	  if(badMonitorAmpl || badAmpl){

	    // module has bad amplitude
	    neighborBad |= (1<<bitOffset) ;

	  }

	  //=====> deal with warnings
	  //=====> give each channel benefit of a doubt
	  warnAmpl=false;
	  warnMonitorAmpl=false;
	  
	  //=====> see if this channel have warnings found from physics
	  physWarningIterator=fPhysWarningMap.find(neib);
	  if(physWarningIterator!=fPhysWarningMap.end()){
	    
	    if( fPhysWarningMap[neib] & fPhysAmplMask ) warnAmpl = true;
	    
	  }	    
	  
	  //=====> see if this channel have errors found from monitoring
	  if( GetWarning(index) & fMoniAmplWarnMask ) warnMonitorAmpl = true;
	  
	  if(warnMonitorAmpl || warnAmpl){

	    // module has amplitude warning
	    neighborWarn |= (1<<bitOffset) ;

	  }
	} else{

	  // physical boundaries (edges)
	  neighborBad |= (1<<bitOffset) ;
	  neighborWarn |= (1<<bitOffset) ;

	}

	bitOffset++;

      } // end z(x) loop on neighbor channels
    } // end y loop on neighbor channels

    //=====> set ToF flags
    for(yoff=-1; yoff<=1; yoff++){
      for(xoff=-1; xoff<=1; xoff++){
	
	if(EmcIndexer::IsValid(sector, x+xoff, y+yoff)){

	  neib = EmcIndexer::getTowerId(sector, x+xoff, y+yoff) ;
	  index = rdo->GetIndexByTowerId(neib) ;

	  //=====> first, look for bad ToF
	  //=====> give each channel benefit of a doubt
	  badTof=false;
	  badMonitorTof=false;
	  
	  //=====> see if this channel have errors found from physics
	  physRejectIterator=fPhysRejectMap.find(neib);
	  if(physRejectIterator!=fPhysRejectMap.end()){
	    
	    if( fPhysRejectMap[neib] & fPhysTofMask ) badTof = true;
	    
	  }	    
	  
	  //=====> see if this channel have errors found from monitoring
	  if( GetError(index) & fMoniTofMask ) badMonitorTof = true;
	  
	  if(badMonitorTof || badTof){

	    // module has bad ToF
	    neighborBad |= (1<<bitOffset) ;

	  }


	  //=====> now look for ToF warnings
	  //=====> give each channel benefit of a doubt
	  warnTof=false;
	  warnMonitorTof=false;
	  
	  //=====> see if this channel have warnings found from physics
	  physWarningIterator=fPhysWarningMap.find(neib);
	  if(physWarningIterator!=fPhysWarningMap.end()){
	    
	    if( fPhysWarningMap[neib] & fPhysTofMask ) warnTof = true;
	    
	  }	    
	  
	  //=====> see if this channel have warnings found from monitoring
	  if( GetWarning(index) & fMoniTofWarnMask ) warnMonitorTof = true;
	  
	  if(warnMonitorTof || warnTof){

	    // module has a ToF warning
	    neighborWarn |= (1<<bitOffset) ;

	  }
	} else{

	  // physical boundaries (edges)
	  neighborBad |= (1<<bitOffset) ;
	  neighborWarn |= (1<<bitOffset) ;

	}

	bitOffset++;

      } // end z(x) loop on neighbor channels
    } // end y loop on neighbor channels

    //printf("index %d tower %d bits %x\n",i,tower,neighbours) ;
    fDeadMap[i] = neighborBad;
    fWarnMap[i] = neighborWarn;

  } // end loop on rdo channels

  return true;

}

//_____________________________________________________________________________
Int_t* emcQAs::GetDeadMap(void)
{
  if (!fDeadMap) BuildDeadMap() ;
  return fDeadMap ;
}

//_____________________________________________________________________________
Int_t emcQAs::GetDead(int ichannel)
{
  return (GetDeadMap())[ichannel] ;
}

//_____________________________________________________________________________
Int_t* emcQAs::GetWarnMap(void)
{
  // MV 2001/12/08

  if (!fWarnMap) BuildDeadMap() ;
  return fWarnMap ;
}

//_____________________________________________________________________________
Int_t emcQAs::GetWarn(int ichannel)
{
  // MV 2001/12/08

  return (GetWarnMap())[ichannel] ;
}

//_____________________________________________________________________________
INT32 emcQAs::GetError(int ichannel) const
{
  emcQAFEM* qafem = dynamic_cast<emcQAFEM*>(fFEMs[ichannel/144]) ;
  if (qafem) {
    return static_cast<INT32>(qafem->getValue(ichannel%144,0)) ;
  }
  else {
    return 0 ;
  }
}

//_____________________________________________________________________________
INT32 emcQAs::GetWarning(int ichannel) const
{
  emcQAFEM* qafem = dynamic_cast<emcQAFEM*>(fFEMs[ichannel/144]) ;
  if (qafem) {
    return static_cast<INT32>(qafem->getValue(ichannel%144,1)) ;
  }
  else {
    return 0 ;
  }
}

//_____________________________________________________________________________
bool emcQAs::WriteDataToFile(const char* producerName, int femCode,
			     const PHTimeStamp& tStart, 
			     const PHTimeStamp& tEnd,
			     INT8* errors, INT8* warnings) 
{
  // femCode is absolute (i.e. SM) position of FEM
  // errors and warnings arrays _MUST_ be of size 144

  char filename[256] ;

  int SN,SM ;
  EmcIndexer::PXSM144_iSiSM144(femCode , SN, SM);

  emcDataManager* dm = emcDataManager::GetInstance() ;

  snprintf(filename,255,"%s/QA/%sSM%02d.%s.QA",
	   dm->GetDestinationDir(),
	   EmcIndexer::EmcSectorId(SN),SM,
	   producerName) ;

  ofstream out(filename) ;
  if (!out) {
    cerr << "<E> emcQAs::WriteDataToFile - Can not create file " 
	 << filename << endl ;
    return false ;
  }

  out << tStart << endl ;
  out << tEnd << endl ;
  int i ;
  for ( i = 0 ; i < 144 ; i++ ) {
    out << i << " " << (int)errors[i] << " " << (int)warnings[i] << endl ;
  }
  out.close() ;
  return true ;
}  
