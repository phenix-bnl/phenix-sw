// $Id: PbGlSector.C,v 1.20 2015/03/22 15:02:13 mazsi Exp $

#include "PbGlSector.h"
#include <iostream>
#include <fstream>
#include <cstdio>
#include <cstdlib>
#include <cassert>
#include "emcDataManager.h"
#include "emcCalibrationData.h"
#include <string>
#include "emcDefines.h"

using std::cout ;
using std::cerr ;
using std::endl ;
using std::ifstream ;
using std::string;

size_t PbGlSector::fgSize = 4608 ;
size_t PbGlSector::fgNumberOfSuperModules = 192 ;

//____________________________________________________________________
PbGlSector::PbGlSector(int & SectorNumber, PHTimeStamp* ts) 
{ 
  SectorN = SectorNumber;

  fC0.resize(fgSize) ;
  fG0.resize(fgSize) ;
  fCF.resize(fgSize) ;
  SM.resize(fgNumberOfSuperModules) ;

  for (size_t i = 0 ; i < fgNumberOfSuperModules ; i++ ) SM[i]=0;

  if (ts) {
    BuildFromDB(ts) ;
  }
  else {
    BuildFromFiles() ;
  }
  
  if (!fIsOK) {
    cerr << EMC_ERROR_MSG << "PbGlSector::PbGlSector : Sector "
	 << SectorNumber << " can not be built" << endl ;
    Reset() ;
  }
}


//____________________________________________________________________
PbGlSector::~PbGlSector() 
{ 
  Reset() ;
}

//_____________________________________________________________________________
void 
PbGlSector::Reset(void)
{
  for ( size_t i = 0 ; i < SM.size() ; i++ ) {
    delete SM[i] ;  
    SM[i] = 0 ;
  }
  SM.clear() ;

  for ( size_t i = 0 ; i < fgSize ; i++) {
    fC0[i] = fG0[i] = fCF[i] = 0.0 ;
  }

  fIsOK = false ;
}


//_____________________________________________________________________________
void
PbGlSector::BuildFromFiles(void)
{  
  string filenameShort ;
  string filename ;

  if (SectorN == 6) {
    filenameShort = "Calibr/database_Pbgl_1.txt" ;
  }
  else {
    filenameShort = "Calibr/database_Pbgl_2.txt" ;
  }

  char* s = getenv("EMCAL_DATA_PBGL") ;

  if (!s) { 
    fIsOK = false ; 
  }
  else {

    filename += s ;
    filename += filenameShort ;

    FILE* fp = fopen(filename.c_str(),"r") ; 
    
    if ( !fp ) { 
      cerr << EMC_ERROR_MSG  <<"PbGlSector::BuildFromFiles : File open error "
	   << filename << endl ; 
      fIsOK = false ;
    } 
    else {
      
      cerr << EMC_INFO_MSG << "PbGlSector::BuildFromFiles : File opened : "
	   << filename << endl ;
      
      PbGlIndexer *   gPbGl  = PbGlIndexer::buildPbGlIndexer();
      
      int iS ;
      float C0, G0, F ;
      
      for (int iSM24=0; 
	   iSM24<static_cast<int>(fgNumberOfSuperModules); 
	   iSM24++){ 
	
	SM[iSM24] = new PbGlSuperModule(iSM24,SectorN); 
	
	LoadNextSMData(fp, SM[iSM24]);
	
	//  convert calibration data from CERN format to that suited for  
	//  analysis chain 
	for (int iSM24T=0; iSM24T<24; iSM24T++) { 
	  
	  iS = gPbGl->SM24iSM24TiST(iSM24, iSM24T); 
	  
	  // C0 is the WA98 calibration coeficient
	  C0 = SM[iSM24]->getC0(iSM24T);
	  
	  // G0_i = ( ( ADC(pmt)_i / ADC(pin)_i ))_t=WA98
	  G0 = SM[iSM24]->getG0(iSM24T);
	  
	  // F is the "fudge factor" based on Maxims and Stefans analysis of the
	  // gain corrections factors
	  F  = SM[iSM24]->getCF(iSM24T);
	  
	  // Kappa is an overall energy scale correction factor. For now it
	  // is the ratio:
	  // Kappa = < ADC(pin)_i / MDO(pin)_i > averaged over SM's and time
	  // It corrects for the change in the PIN readout.
	  //      float kappa = 1./5.581;
	  //      float kappa = 1./5.995;
	  
	  /* 
	     02-27-2001 HeB
	     now all three variables are returned by GetEnergyCalibration and
	     handed over to emcRawDataCalibrator where the sum is computed.
	     modification necessary for transition ASCII->Objectivity.
	  */      
	  fC0[iS] = C0;
	  fG0[iS] = G0;
	  fCF[iS] = F;
	  
	} 
      }
      fclose(fp) ;
      fIsOK = true ;
    }
  }
}

//_____________________________________________________________________________
void
PbGlSector::BuildFromDB(PHTimeStamp* when)
{
  emcCalibrationData* inical ; 
  emcCalibrationData driver(emcCalibrationData::kIniCal,SectorN) ;
  driver.SetSource(emcManageable::kDB_Objy);
  emcDataManager* dm = emcDataManager::GetInstance() ;

  inical =  dynamic_cast<emcCalibrationData*>(dm->Collect(driver,*when)) ;
  if (!inical) {
    cerr << "<E> PbGlSector::BuildFromDB() : Failed to fetch initial "
	 << " calibration data for sector " << SectorN 
	 << " at time " << *when << endl ;
    fIsOK = false ;
  }
  else {

    PbGlIndexer *   gPbGl  = PbGlIndexer::buildPbGlIndexer();
 
    size_t iSM24 ;
    size_t iSM24T ; 
    int iS ;

    for ( iSM24 = 0 ; iSM24 < fgNumberOfSuperModules ; iSM24++ ) { 
      
      for ( iSM24T = 0 ; iSM24T < 24 ; iSM24T++ ) { 
	
	iS = gPbGl->SM24iSM24TiST(iSM24, iSM24T); 
	
	// C0 is the WA98 calibration coeficient
	float C0 = inical->GetValue(iS,0) ;
	
	// G0_i = ( ( ADC(pmt)_i / ADC(pin)_i ))_t=WA98
	float G0 = inical->GetValue(iS,1) ;
	
	// F is the "fudge factor" based on Maxims and Stefans analysis of the
	// gain corrections factors
	float F  = inical->GetValue(iS,2) ;
	
	// Kappa is an overall energy scale correction factor. For now it
	// is the ratio:
	// Kappa = < ADC(pin)_i / MDO(pin)_i > averaged over SM's and time
	// It corrects for the change in the PIN readout.
	//      float kappa = 1./5.581;
	//float kappa = 1./5.995;

	fC0[iS] = C0;
	fG0[iS] = G0;
	fCF[iS] = F;
      }  
    }
    fIsOK = true ;
  }
}

//____________________________________________________________________
void 
PbGlSector::GetEnergyCalibration(int iTower, float& c0, float& g0, float& cf)
{ 
  assert( iTower>=0 && static_cast<size_t>(iTower) < fC0.size() ) ;
  assert( iTower>=0 && static_cast<size_t>(iTower) < fG0.size() ) ;
  assert( iTower>=0 && static_cast<size_t>(iTower) < fCF.size() ) ;

  c0 = fC0[iTower] ;
  g0 = fG0[iTower] ;
  cf = fCF[iTower] ;
}

 
                    
//_____________________________________________________________________________
void 
PbGlSector::LoadNextSMData(FILE * fp, EmcSuperModule * SM24) 
{ 
  //  Load calibration from 902 and old CERN data into SuperModule object 
  //  identified by its pointer SM24 

  float dummy[33];
  char dummyc1[5];
  char dummyc2[10]; 

  PbGlSuperModule* sm = dynamic_cast<PbGlSuperModule*>(SM24) ;
  assert(sm!=0) ;

  // read in data

  for (int i1=3; i1>-1; i1--) {

    for (int i2=0; i2<6; i2++) {

      for (int j=0; j<3; j++) {
	assert( fscanf(fp,"%e", dummy+j) == 1 );
      }
      assert( fscanf(fp,"%s",dummyc1) == 1 );
      assert( fscanf(fp,"%e", dummy+3) == 1 );
      assert( fscanf(fp,"%e", dummy+4) == 1 );
      assert( fscanf(fp,"%s",dummyc2) == 1 );

      for(int j=5; j<33; j++) {
	assert( fscanf(fp,"%e", dummy+j) == 1 );
      }
      sm->LoadTowerData((i1*6+i2), dummy, dummyc1, dummyc2);
    }
   }
} 

//_____________________________________________________________________________
void 
PbGlSector::CorrectEnergyCalibration(const char * fname)
{
  cerr << "PbGlSector::CorrectEnergyCalibration : PLEASE CHECK THIS METHOD!"
       << endl ;
  assert(0==1) ;

  ifstream fin;
  //  ofstream fout;
  fin.open(fname,std::ios::in);
  if(!fin) {
    cout<<"File "<<fname<<"  not found - proceed with CERN calibration"<<endl;
    return;
  }
  int n, TowerId;
  float c;
  while(fin>>n>>TowerId>>c){
    // commented out L.A. June-16-2001 because it's not
    // used anylonger in GetCalibrationData ?!
    //    PbGlECalib[n] = c;
    //    PbGlNorm0[n]  = 1.;
  }
  fin.close();
}







