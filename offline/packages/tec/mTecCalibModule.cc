#include "mTecCalibModule.h"
#include "TecOutV1.hh"
#include "TecHit.hh"
#include "mTecUtilities.h"

#include <PHNode.h>
#include <PHIODataNode.h>
#include <PHTypedNodeIterator.h>

#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <iostream>

typedef PHIODataNode<TecOutV1> TecOutNode_t;

using namespace std;
using namespace TecUtilities;

mTecCalibModule::mTecCalibModule()
{
  Verbose=0;
  UseObjy=0;
  DrawCut=0;
  DrawScaleX=1;
  DrawScaleY=1;
  FillHistos=0;
  SaveHistosEvent=9999;
  RunNumber=0;
  HistCut=2;
  DrawSave2gif=0;
  InputADC=1;
  ApplyGains=1;
}

mTecCalibModule::~mTecCalibModule(){}

PHBoolean mTecCalibModule::event(PHCompositeNode *root) {
 return False;
}

//======================================================================

// Merge two dst events.
// The code assumes that there are two nodes containing
// TecOutV1 class objects: TecOutV1 and TecOutV1TEST
// Charge is assumed to be already calibrated.

PHBoolean mTecCalibModule::mixdst(PHCompositeNode *root) {

using namespace TecUtilities;
  
//------ Get pointers to the Tables ------------------------------------

  PHTypedNodeIterator<TecOutV1> teciter(root);

  TecOutNode_t *TecOutNode = teciter.find("TecOutV1");
  TecOutV1* tecout;
  if(TecOutNode) {
    tecout = (TecOutV1*)TecOutNode->getData();
  }
  else {
    if(Verbose>0) cout << "mTecCalibModule::mixdst ERROR: Can not find TecOutV1." << endl;
    return False;
  }

  TecOutNode_t *TecOutNodeTest = teciter.find("TecOutV1TEST");
  TecOutV1* tecouttest;
  if(TecOutNodeTest) {
    tecouttest = (TecOutV1*)TecOutNodeTest->getData();
  }
  else {
    if(Verbose>0) cout << "mTecCalibModule::mixdst ERROR: Can not find TecOutV1TEST." << endl;
    return False;
  }

  if(Verbose>0) {
    cout << "mTecCalibModule::mixdst First TecOut:  " << tecout->getNHits() << endl;
    cout << "mTecCalibModule::mixdst Second TecOut: " << tecouttest->getNHits() << endl;
  }

//--------------------------------------------------------------------

  int nmixed=0;

  for(int k=0; k<tecout->getNHits(); k++) {

    int glindex=tecout->getHitGlobalIndex(k);

// Check if this hit exist in the second file
    int mix = -1;
    for(int kk=0; kk<tecouttest->getNHits(); kk++) {
      int glindextest=tecouttest->getHitGlobalIndex(kk);
      if(glindex==glindextest) {
        mix = kk;
        break;
      }
    } 

    if(mix==-1) {       // no need to mix. do nothing

    }
    else {              // do mixing

      int adc = tecout->getHitADC(k);
      float charge = tecout->getHitCharge(k);
          float chargetest = tecouttest->getHitCharge(mix);

            float chargesum = charge + chargetest;
            int adcsum = Charge2Ampl(chargesum);
            if(adcsum<adc) adcsum=adc;
            tecout->setHitCharge(k,chargesum);
            tecout->setHitADC(k,adcsum);

      nmixed++;

    } // mixing if

  } // end k loop

// Second file

  for(int kk=0; kk<tecouttest->getNHits(); kk++) {

    int glindextest = tecouttest->getHitGlobalIndex(kk);

// Check if we already added this entry
    int mix = -1;
    for(int k=0; k<tecout->getNHits(); k++) {
      int glindex = tecout->getHitGlobalIndex(k);
      if(glindex==glindextest) {
        mix = k;
        break;
      }
    }

    if(mix==-1) {       // add this hit to tecout
      int index = tecouttest->getHitIndex(kk);
      int wire = tecouttest->getHitWire(kk);
      int bin = tecouttest->getHitTimeBin(kk);
      int adc = tecouttest->getHitADC(kk);
      int trackid = tecouttest->getHitTrackID(kk);
      float charge = tecouttest->getHitCharge(kk);
      float xyz[2];
      xyz[0] = tecouttest->getHitX(kk);
      xyz[1] = tecouttest->getHitY(kk);
        tecout->AddTecHit(index, wire, bin, adc, charge, xyz, trackid);
    }
    else 
      { 
	// do nothing -> this entry was already mixed in
      }
  } // kk loop

  if(Verbose>0) {
    cout << "mTecCalibModule::mixdst TecOut final number of hits = " << tecout->getNHits() << endl;
    cout << "mTecCalibModule::mixdst Number of mixed hits = " << nmixed << endl;
    cout << "mTecCalibModule::mixdst Finished. " << endl;
  }

  return True;
}

// Merge two prdf events with different calibration constants
// The code assumes that there are two nodes containing 
// TecOutV1 class objects: TecOutV1 and TecOutV1TEST
PHBoolean 
mTecCalibModule::event(PHCompositeNode *root,
		       TecAddressObject* TAO,
		       TecCalibrationObject* TCO,
		       TecCalibrationObject* TCO2) 
{
  if(Verbose>0) cout << "mTecCalibModule: Started MERGING..." << endl;

  //------ Get pointers to the Tables ------------------------------------

  PHTypedNodeIterator<TecOutV1> teciter(root);

  TecOutNode_t *TecOutNode = teciter.find("TecOutV1");
  TecOutV1* tecout;
  if(TecOutNode) {
    tecout = (TecOutV1*)TecOutNode->getData();
    if(Verbose>0) cout << "mTecCalibModule: Found TecOutV1." << endl;
  }
  else {
    if(Verbose>0) cout << "mTecCalibModule ERROR: Can not find TecOut." << endl;
    return False;
  }

  TecOutNode_t *TecOutNodeTest = teciter.find("TecOutV1TEST");
  TecOutV1* tecouttest;
  if(TecOutNodeTest) {
    tecouttest = (TecOutV1*)TecOutNodeTest->getData();
    if(Verbose>0) cout << "mTecCalibModule: Found TecOutV1TEST." << endl;
  }
  else {
    if(Verbose>0) cout << "mTecCalibModule ERROR: Can not find TecOutV1TEST." << endl;
    return False;
  }

  if(Verbose>0) {
    cout << "mTecCalibModule: First TecOut:  " << tecout->getNHits() << endl;
    cout << "mTecCalibModule: Second TecOut: " << tecouttest->getNHits() << endl;
  }

  float gainAtest,gainA;
  float rawcharge;
  int hotdeadlist[TECMAXNUMHOT];
  int hotdeadlisttest[TECMAXNUMHOT];
  
  int nhotdead=0;
  nhotdead=TCO->getHotDeadList(hotdeadlist);
  if(Verbose>0) cout << "mTecCalibModule:  number of hot/dead wires in the first file  = "
                     << nhotdead << endl;
  int nhotdeadtest=0;
  nhotdeadtest=TCO2->getHotDeadList(hotdeadlisttest);
  if(Verbose>0) cout << "mTecCalibModule:  number of hot/dead wires in the second file = "
                     << nhotdeadtest << endl;

  int nmixed=0;

  for(int k=0; k<tecout->getNHits(); k++) {
    int glindex=tecout->getHitGlobalIndex(k);

    // Check if this hit exist in the second file
    int mix = -1;
    for(int kk=0; kk<tecouttest->getNHits(); kk++) {
      int glindextest=tecouttest->getHitGlobalIndex(kk);
      if(glindex==glindextest) {
        mix = kk;
        break;
      }
    } // end kk loop

    if(mix==-1) {	// no need to mix

       int index = tecout->getHitIndex(k);
       int wire = tecout->getHitWire(k);
       int adc = tecout->getHitADC(k);
       if(ApplyGains==0) {
         gainA = 1.0;
       }
       else {
         gainA = TCO->getAbsoluteGain(index)*TCO->getRelativeGain(index,wire);
       }
         // Check if this wire is hot or dead
         int glindexdead = index*1000+wire;
         for(int j2=0; j2<nhotdead; j2++) {
           if(hotdeadlist[j2]==glindexdead) { gainA=0.; }
         }
           rawcharge = Ampl2Charge(adc);
           tecout->setHitCharge(k,rawcharge*gainA);
           if(gainA==0.) tecout->setHitADC(k,0);

    }
    else {		// do mixing
      int index = tecout->getHitIndex(k);
      int wire = tecout->getHitWire(k);
      int adc = tecout->getHitADC(k);
      if(ApplyGains==0) {
        gainA = 1.0;
      }
      else {
        gainA = TCO->getAbsoluteGain(index)*TCO->getRelativeGain(index,wire);
      }
      // Check if this wire is hot or dead
        int glindexdead = index*1000+wire;
        for(int j2=0; j2<nhotdead; j2++) {
          if(hotdeadlist[j2]==glindexdead) { gainA=0.; }
        }
        if(gainA==0.) adc=0;
        float rawcharge = Ampl2Charge(adc);

          int indextest = tecouttest->getHitIndex(mix);
          int wiretest = tecouttest->getHitWire(mix);
          int adctest = tecouttest->getHitADC(mix);
          if(ApplyGains==0) {
            gainAtest = 1.0;
          }
          else {
            gainAtest = TCO2->getAbsoluteGain(indextest)*TCO2->getRelativeGain(indextest,wiretest);
          }
          // Check if this wire is hot or dead
            int glindexdeadtest = indextest*1000+wiretest;
            for(int j2=0; j2<nhotdeadtest; j2++) {
              if(hotdeadlisttest[j2]==glindexdeadtest) { gainAtest=0.; }
            }
            if(gainAtest==0.) adctest=0;
            float rawchargetest = Ampl2Charge(adctest);

            float chargesum = rawcharge*gainA + rawchargetest*gainAtest;
	    int adcsum = Charge2Ampl(chargesum);
            tecout->setHitCharge(k,chargesum);
            tecout->setHitADC(k,adcsum);
        
      nmixed++;
    } // mixing if
  } // end k loop

  // Calibrate second file
  for(int kk=0; kk<tecouttest->getNHits(); kk++) {
    int glindextest = tecouttest->getHitGlobalIndex(kk);

    // Check if we already added this entry
    int mix = -1;
    for(int k=0; k<tecout->getNHits(); k++) {
      int glindex = tecout->getHitGlobalIndex(k);
      if(glindex==glindextest) {
        mix = k;
        break;
      }
    }

    if(mix==-1) {       // add this hit to tecout
      int indextest = tecouttest->getHitIndex(kk);
      int wiretest = tecouttest->getHitWire(kk);
      int adctest = tecouttest->getHitADC(kk);
      if(ApplyGains==0) {
        gainAtest = 1.0;
      }
      else {
        gainAtest = TCO2->getAbsoluteGain(indextest)*TCO2->getRelativeGain(indextest,wiretest);
      }
      // Check if this wire is hot or dead
        int glindexdeadtest = indextest*1000+wiretest;
        for(int j2=0; j2<nhotdeadtest; j2++) {
          if(hotdeadlisttest[j2]==glindexdeadtest) { gainAtest=0.; }
        }
          float rawchargetest = Ampl2Charge(adctest);
          tecouttest->setHitCharge(kk,rawchargetest*gainAtest);
          if(gainAtest==0.) tecouttest->setHitADC(kk,0);

            int index = tecouttest->getHitIndex(kk);
            int wire = tecouttest->getHitWire(kk);
            int bin = tecouttest->getHitTimeBin(kk);
            int adc = tecouttest->getHitADC(kk);
            int trackid = tecouttest->getHitTrackID(kk);
            float charge = tecouttest->getHitCharge(kk);
            float xyz[2];
            xyz[0] = tecouttest->getHitX(kk);
            xyz[1] = tecouttest->getHitY(kk);
            tecout->AddTecHit(index, wire, bin, adc, charge, xyz, trackid);


    }
    else 
      {
	// do nothing -> this entry was already mixed in
      }
  } // kk loop

  if(Verbose>0) {
    cout << "mTecCalibModule: TecOut final number of hits = " << tecout->getNHits() << endl;
    cout << "mTecCalibModule: Number of mixed hits = " << nmixed << endl;
    cout << "mTecCalibModule: Finished. " << endl;
  }

  return True;

}

PHBoolean 
mTecCalibModule::event(PHCompositeNode *root,
		       TecAddressObject* TAO,
		       TecCalibrationObject* TCO) 
{
  static int EventNumber = 0;

  if(Verbose>0) cout << "mTecCalibModule: Started event # " << EventNumber << endl;

  //------ Get pointers to the Tables ------------------------------------

  PHTypedNodeIterator<TecOutV1> teciter(root);
  TecOutNode_t *TecOutNode = teciter.find("TecOutV1");
  TecOutV1* tecout;

  if(TecOutNode) {
    tecout = (TecOutV1*)TecOutNode->getData();
    if(Verbose>0)
    cout << "mTecCalibModule: Found TecOut." << endl;
  }
  else {         
    if(Verbose>0) cout << "mTecCalibModule ERROR: Can not find TecOut." << endl;
    return False;
  }

// ------ Calibrate Data --------------------------------------------------

  float gainA,rawcharge;
  int hotdeadlist[TECMAXNUMHOT];

  int nhotdead=0;
  nhotdead=TCO->getHotDeadList(hotdeadlist);
  if(Verbose>0) cout << "mTecCalib:  number of hot/dead wires = " 
                     << nhotdead << endl;

  if(Verbose>0) cout << "mTecCalib TecOut: " << tecout->getNHits() << " " 
                     << tecout->getNTracks() << endl;

// Calibrate hits

    for(int i1=0; i1<tecout->getNHits(); i1++) {
       int index = tecout->getHitIndex(i1); 
       int wire = tecout->getHitWire(i1); 
       int adc = tecout->getHitADC(i1); 
       if(ApplyGains==0) { 
         gainA = 1.0; 
       }
       else {
         gainA = TCO->getAbsoluteGain(index)*TCO->getRelativeGain(index,wire);
       }
         // Check if this wire is hot or dead
         int glindex = index*1000+wire;
         for(int j2=0; j2<nhotdead; j2++) {
           if(hotdeadlist[j2]==glindex) { gainA=0.; }
         }
	 rawcharge = Ampl2Charge(adc);
	 tecout->setHitCharge(i1,rawcharge*gainA);
	 if(gainA==0.) tecout->setHitADC(i1,0);
    }

    EventNumber++;
    
  if(Verbose>0) {
    cout << "mTecCalibModule: Finished: " << tecout->getNHits() << endl;
  }

  return True;
}




