// Author: Laurent Aphecetche (aphecetc@in2p3.fr)
// <EK> 111302 - Included reference to emcRawDataCalibratorv2 - Run3
#include "emcCalibratorFactory.h"
#include "emcCalibrator.h"

#include "emcRawDataCalibrator.h"
#include "emcRawDataCalibratorV1.h"
#include "emcRawDataCalibratorV2.h"
#include <string>
#include <iostream>
#include <cassert>

using namespace std;

emcCalibrator* emcCalibratorFactory::fCalibrator = 0 ;

//_____________________________________________________________________________
emcCalibratorFactory::~emcCalibratorFactory()
{
}

//_____________________________________________________________________________
emcCalibrator* emcCalibratorFactory::GetCalibrator(void)
{
  if (!fCalibrator) {
    cerr << "<E> emcCalibratorFactory::GetCalibrator : Factory has not "
	 << "    been initialized. Please call "
	 << "    emcCalibratorFactory::Initialize first. " << endl ;
  }

  return fCalibrator ;
}

//_____________________________________________________________________________
bool emcCalibratorFactory::Initialize(const char* calibrator_classname)
{
  bool ok = false ;
  string newname = calibrator_classname ;
  string oldname ;

  if (fCalibrator) {
    oldname = fCalibrator->GetName() ;    
    if ( oldname != newname ) {
      cout << "<W> emcCalibratorFactory::Initialize : You requested " << endl
	   << "    a different calibrator type. " << endl 
	   << "    I'm switching from :" << oldname << endl 
	   << "    to : " << newname << endl ;
    }
    delete fCalibrator ;
    fCalibrator = 0 ;
  }

  assert(fCalibrator==0) ;

  if ( newname != "emcRawDataCalibrator" &&
       newname != "emcRawDataCalibratorV1" &&
       newname != "emcRawDataCalibratorV2") {
    cerr << "<E> emcCalibratorFactory::Initialize : Wrong calibrator type." 
	 << endl 
	 << "Initialization failed." << endl ;
  }
  else {
    if ( newname == "emcRawDataCalibrator") {
      fCalibrator = new emcRawDataCalibrator() ;      
      ok=true ;
    }
    else if ( newname == "emcRawDataCalibratorV1") {
      fCalibrator = new emcRawDataCalibratorV1() ;      
      ok=true;
    }
    else if ( newname == "emcRawDataCalibratorV2") {
      fCalibrator = new emcRawDataCalibratorV2() ;      
      ok=true;
    }
  }
  return ok ;
}

