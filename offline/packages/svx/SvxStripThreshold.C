
#include "SvxStripThreshold.h"
#include "svxAddress.hh"

#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbSvxStripThreshold.hh>

#include <getClass.h>
#include <PHCompositeNode.h>

#include <fstream>
#include <iostream>

using namespace std;

SvxStripThreshold::SvxStripThreshold(PHCompositeNode *topNode) :
  UseDatabase(false),
  verbosity(0)
{ 

  SvxAddressObject = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( SvxAddressObject == NULL) {
    cerr << PHWHERE<< "Can't find svxAddress. " << endl; 
  }

  memset(thresh,0,sizeof(thresh));
}

SvxStripThreshold::SvxStripThreshold(svxAddress* address):
  UseDatabase(false),
  SvxAddressObject(address),
  verbosity(0)
{
  memset(thresh,0,sizeof(thresh));
}


//---------------------------------------------------------------------------------

bool SvxStripThreshold::readFromThresholdFile(const std::string &filename)
{
    int layer;
    int ladder;
    int sensor;
    int roc;
    int rocchnl;
    int th;

    ifstream infile;
    infile.open(filename.c_str());
    if (!infile) {
        std::cerr << PHWHERE << " Can't open input file " << filename << std::endl;
        return false;
    }

    int count=0;
    while (infile >> layer >> ladder >> sensor >> roc >> rocchnl >> th) {
        count++;
        if (verbosity > 0) {
            std::cout << "Read layer " << layer
                << ", ladder " << ladder
                << ", sensor " << sensor
                << ", roc " << roc
                << ", roc channel " << rocchnl
                << ", Threshold = " << th << std::endl;
        }
        thresh[layer-2][ladder][sensor][roc][rocchnl] = th;
    }

    cout << PHWHERE << " Total number of records = " << count << endl;
    infile.close();
    return true;
}

//---------------------------------------------------------------------------------

void SvxStripThreshold::print() {
  for(int ilay=0; ilay<2; ilay++){
    int nlad = (ilay==0) ? SVXLADDERSLAYER2 : SVXLADDERSLAYER3;
    int nsen = (ilay==0) ? SVXSENSORSLAYER2 : SVXSENSORSLAYER3;
    for(int ilad=0; ilad<nlad; ilad++){
      for(int isen=0; isen<nsen; isen++){
        cout<<ilay<<" "<<ilad<<" "<<isen<<" ";
        print(ilay, ilad, isen);
      }
    }
  }
}

void SvxStripThreshold::printROC(const int layer, const int ladder, const int sensor) const {
//    int thresh[SVXLAYERNUMBER-2][SVXLADDERNUMBER][SVXSENSORNUMBER][12][128];
  if(layer<0 || layer>1) {cout << "ERROR: Wrong layer = " << layer << endl; return;}
  if(ladder<0 || ladder>=SVXLADDERNUMBER) {cout << "ERROR: Wrong ladder = " << ladder << endl; return;}
  if(sensor<0 || sensor>=SVXSENSORNUMBER) {cout << "ERROR: Wrong sensor = " << sensor << endl; return;}

  for(int i=0; i<12; i++) {
    for(int j=0; j<128; j++) { 
      int th = thresh[layer][ladder][sensor][i][j];
      cout << layer << " " << ladder << " " << sensor << " " << i << " " << j << "   " << th << endl;
    }
  }

}

//---------------------------------------------------------------------------------

void SvxStripThreshold::print(const int layer, const int ladder, const int sensor) const {
//    int thresh[SVXLAYERNUMBER-2][SVXLADDERNUMBER][SVXSENSORNUMBER][12][128];
  if(layer<0 || layer>1) {cout << "ERROR: Wrong layer = " << layer << endl; return;}
  if(ladder<0 || ladder>=SVXLADDERNUMBER) {cout << "ERROR: Wrong ladder = " << ladder << endl; return;}
  if(sensor<0 || sensor>=SVXSENSORNUMBER) {cout << "ERROR: Wrong sensor = " << sensor << endl; return;}

  for (size_t sensorSection = 0; sensorSection < SVXSENSECSTRIPIX; sensorSection++) {
    for (size_t sensorReadout = 0; sensorReadout < 2; sensorReadout++) {
      for (size_t channel = 0; channel < 384; channel++) {
        int roc=SvxAddressObject->getStripRoc(sensorSection,sensorReadout,channel);
        int rocchnl=SvxAddressObject->getStripRocChannel(sensorSection,sensorReadout,channel);
        if( roc==-1 || rocchnl==-1 ){
          cerr<<"SvxStripThreshold::"<<__FUNCTION__<<" : "
              <<"Roc or RocChannel is out of range : "
              <<"ROC="<<roc
              <<"ROCChannel="<<rocchnl<<endl;
          continue;;
        }

        int th = thresh[layer][ladder][sensor][roc][rocchnl];
        cout << layer << " " << ladder << " " << sensor << " " << sensorSection << " " << sensorReadout << " " << channel << "   " << th << endl; 
      }
    }
  }

}

//---------------------------------------------------------------------------------

bool SvxStripThreshold::readFromFile(const std::string &filename)
{
    int layer;
    int ladder;
    int sensor;
    int sensorSection;
    int readout;
    int channel;
    float adcmean;
    float adcrms;
    int chanStatus;
    int th;

    if(!SvxAddressObject) {
        std::cerr << PHWHERE << " Can't find Address Object." << std::endl;
        return false;
    }

    ifstream infile;
    infile.open(filename.c_str());
    if (!infile) {
        std::cerr << PHWHERE << "Can't open input file " << filename << std::endl;
        return false;
    }

    int count=0;

    while (infile >> layer >> ladder >> sensor >>
           sensorSection >> readout >> channel >> adcmean >> adcrms >> chanStatus >> th) {

        count++;

        if (verbosity > 0) {
            std::cout << "Read layer " << layer
                << ", ladder " << ladder
                << ", sensor " << sensor
                << ", sensor section " << sensorSection
                << ", readout " << readout
                << ", channel " << channel
                << ", Threshold = " << th << std::endl;
        }

        int roc=SvxAddressObject->getStripRoc(sensorSection,readout,channel);
        int rocchnl=SvxAddressObject->getStripRocChannel(sensorSection,readout,channel);
        if( roc==-1 || rocchnl==-1 ){
          cerr<<"SvxStripThreshold::"<<__FUNCTION__<<" : "
              <<"Roc or RocChannel is out of range : "
              <<"ROC="<<roc
              <<"ROCChannel="<<rocchnl<<endl;
          cerr<<"  skip this channel"<<endl;
          continue;
        }

        thresh[layer][ladder][sensor][roc][rocchnl] = th; // in this file strip layers are numbered 0 and 1

    }

    cout << PHWHERE << " Total number of records = " << count << endl;
    infile.close();
    return true;
}

//---------------------------------------------------------------------------------

bool SvxStripThreshold::readFromDatabase(PHTimeStamp * T)
{
    bool success = true;
    PHTimeStamp Tsearch = *T;
    PdbCalBank *svxBank = NULL;
    const char *tableName = "svx.stripthresholds.data";
    PdbBankID bankID;

    if(!SvxAddressObject) {
        std::cerr << PHWHERE << "Can't find Address Object." << std::endl;
        return false;
    }

    PdbBankManager *bankManager = PdbBankManager::instance();
    PdbApplication *application = bankManager->getApplication();

    if (!application->startRead()) {
        application->abort();
        std::cerr << PHWHERE << " ERROR -> Transaction aborted. Database NOT available." << std::endl;
        return false;
    }

    bankID.setInternalValue(0);
    cout << "tableName = " << tableName << endl;
    cout << "bankID = " << bankID.getInternalValue() << endl;
    cout << "Search TimeStamp: " << Tsearch << endl;

    svxBank = bankManager->fetchBank("PdbSvxStripThresholdBank", bankID, tableName, Tsearch);

    if (svxBank) {
        //svxBank->printHeader();

        int banklength = (int) svxBank->getLength();
        cout << "Bank length = " << banklength << endl;
        
        for (int i = 0; i < banklength; i++) {
            PdbSvxStripThreshold *rec = (PdbSvxStripThreshold *) &(svxBank->getEntry (i));
            
            //if (verbosity > 0) {
            //    cout << "Read database entry: ";
            //    rec->print();
            //}
            
            short layer  = rec->getLayer();
            short ladder = rec->getLadder();
            short sensor = rec->getSensor();
            short sensorSection = rec->getSensorSection();
            short sensorReadout = rec->getSensorReadout();
            short channel = rec->getChannel();
            int th = rec->getThreshold();
            
            int roc=SvxAddressObject->getStripRoc(sensorSection,sensorReadout,channel);
            int rocchnl=SvxAddressObject->getStripRocChannel(sensorSection,sensorReadout,channel);
            if( roc==-1 || rocchnl==-1 ){
              cerr<<"SvxStripThreshold::"<<__FUNCTION__<<" : "
                  <<"Roc or RocChannel is out of range : "
                  <<"ROC="<<roc
                  <<"ROCChannel="<<rocchnl<<endl;
              cerr<<"  skip this channel"<<endl;
              continue;
            }

            thresh[layer][ladder][sensor][roc][rocchnl] = th;

        }
    }
    else {
        cerr << PHWHERE << " ERROR -> bankManager returned null pointer." << endl;
        success = false;
    }
    
    if (success) {
        application->commit();
    }
    else {
        application->abort();
    }

    delete svxBank; // you can delete null pointers (it is a No Op)

    return success;
}

//---------------------------------------------------------------------------------

bool SvxStripThreshold::writeToDatabase(PHTimeStamp * Tbeg, PHTimeStamp * Tend)
{
    bool success = true;
    PdbBankManager *bankManager = PdbBankManager::instance();
    PdbApplication *application = bankManager->getApplication();
    unsigned recordNumber = 0;

    PdbCalBank *svxBank = NULL;

    if(!SvxAddressObject) {
        std::cerr << "Can't find Address Object." << std::endl;
        return false;
    }

    if (application->startUpdate()) {

        const char *tableName = "svx.stripthresholds.data";
        const char *description = "SVX Strip Thresholds";
        PHTimeStamp Tstart = *Tbeg;
        PHTimeStamp Tstop = *Tend;

        PdbBankID bankID;

        bankID.setInternalValue(0);

        cout << "tableName = " << tableName << endl;
        cout << "description = " << description << endl;
        cout << "bankID = " << bankID.getInternalValue() << endl;
        cout << "Validity range: " << Tstart << " - " << Tstop << endl;

        svxBank = bankManager->createBank("PdbSvxStripThresholdBank",
                                          bankID, description,
                                          Tstart, Tstop, tableName);

        if (svxBank) {

            //thresh[SVXLAYERNUMBER-2][SVXLADDERNUMBER][SVXSENSORNUMBER][12][128];
            unsigned numRecords = 2 * SVXLADDERNUMBER * SVXSENSORNUMBER * 12 * 128;

            cout << "SvxDeadMap::writeToDatabase: Number of records = " << numRecords << endl;
            svxBank->setLength(numRecords);

            // And now this loop writes out the data in the deadMap maps

            for (size_t layer = 0; layer < SVXLAYERNUMBER-2; layer++) {
              for (size_t ladder = 0; ladder < SVXLADDERNUMBER; ladder++) {
                for (size_t sensor = 0; sensor < SVXSENSORNUMBER; sensor++) {
                  for (size_t sensorSection = 0; sensorSection < SVXSENSECSTRIPIX; sensorSection++) {
                    for (size_t sensorReadout = 0; sensorReadout < 2; sensorReadout++) {
                      for (size_t channel = 0; channel < 384; channel++) {
                        int roc=SvxAddressObject->getStripRoc(sensorSection,sensorReadout,channel);
                        int rocchnl=SvxAddressObject->getStripRocChannel(sensorSection,sensorReadout,channel);
                        if( roc==-1 || rocchnl==-1 ){
                          cerr<<"SvxStripThreshold::"<<__FUNCTION__<<" : "
                              <<"Roc or RocChannel is out of range : "
                              <<"ROC="<<roc
                              <<"ROCChannel="<<rocchnl<<endl;
                          cerr<<"  skip this channel"<<endl;
                          continue;
                        }
                        PdbSvxStripThreshold *rec = (PdbSvxStripThreshold *) & (svxBank->getEntry(recordNumber));
                        rec->setAll(layer, ladder, sensor, sensorSection, sensorReadout, channel, thresh[layer][ladder][sensor][roc][rocchnl]);
                        recordNumber++;
                      }
                    }
                  }
                }
              }
            }

        }
        else {
            cerr << PHWHERE << " ERROR: can not create svxBank." << endl;
            success = false;
        }
    }
    else {
        cerr << PHWHERE << "ERROR: Database not writable, aborting." << endl;
        success = false;
    }

    if (success) {
        application->commit();
        cout << "Successfully written " << recordNumber << " records." << endl;
    }
    else {
        application->abort();
    }

    if (svxBank)
        delete svxBank;

    return success;
}

int SvxStripThreshold::getThreshold (const int layer,
                                     const int ladder,
                                     const int sensor,
                                     const int roc,
                                     const int rocchnl) const
{
  return thresh[layer][ladder][sensor][roc][rocchnl];
}

int SvxStripThreshold::getThreshold (const int layer,
                                     const int ladder,
                                     const int sensor,
                                     const int sensorsection,
                                     const int sensorreadout,
                                     const int channel) const
{
  int roc    =SvxAddressObject->getStripRoc(       sensorsection,sensorreadout,channel);
  int rocchnl=SvxAddressObject->getStripRocChannel(sensorsection,sensorreadout,channel);

  if( roc==-1 || rocchnl==-1 ){
    cerr<<"SvxStripThreshold::"<<__FUNCTION__<<" : "
        <<"Roc or RocChannel is out of range : "
        <<"ROC="<<roc
        <<"ROCChannel="<<rocchnl<<endl;
    cerr<<"  return -1 as threshold"<<endl;
    return -1;
  }

  return thresh[layer][ladder][sensor][roc][rocchnl];
}

