#include <CalibratorPeak.hh>

#include <BbcEvent.hh>

#include <TH1.h>

#include <cmath>
#include <iostream>

using namespace std;

void CalibratorPeak::Initualize( const BbcTime_t& time, char* CalibType ){

  Peak.restore( time, CalibType );
  Peak.setPrevious();

  char ObjectName[6];
  char HistName[72];

  for ( int ipmt=0;ipmt<BBC_N_PMT;ipmt++ ){
    cout << ipmt << endl;
    sprintf(ObjectName,"Pmt%3.3d",ipmt);
    sprintf(HistName,  "Calibration histgram for Pmt%3.3d",ipmt);
    PeakHist[ipmt] = new TH1F( ObjectName, HistName, 2048, 0., 4096);
  }
   
}

void CalibratorPeak::FillHist( BbcEvent* bbc, char* type ){
  for ( int ipmt=0;ipmt<128;ipmt++ ) {
    if ( strcmp(type,"pedestal")==0 ){
      if (!bbc->isHit(ipmt)) {
        PeakHist[ipmt]->Fill( bbc->getAdc(ipmt) );
      }
    }
    else 
    if ( strcmp(type,"overflow0")==0 ){
      if (!bbc->isHit(ipmt)) {
        PeakHist[ipmt]->Fill( bbc->getTdc0(ipmt) );
      }
    }
    else 
    if ( strcmp(type,"overflow1")==0 ){
      if (!bbc->isHit(ipmt)) {
        PeakHist[ipmt]->Fill( bbc->getTdc1(ipmt) );
      }
    }
    else 
    if ( strcmp(type,"pmtgain")==0 ){
      if ( bbc->isHit(ipmt) ) {
        PeakHist[ipmt]->Fill( bbc->getTrueAdc(ipmt) );
        HistStatistics[ipmt]=(int)(PeakHist[ipmt]->GetEntries()); 
      }
    }
  }
}

void CalibratorPeak::Calculate(){
  for (int ipmt=0;ipmt<128;ipmt++){
    (Peak.getCalibPar(ipmt))->setPeakChannel(PeakHist[ipmt]->GetMean());
    (Peak.getCalibPar(ipmt))->setDeviation(PeakHist[ipmt]->GetRMS());
  }
}

void CalibratorPeak::Evaluate(){
  for (int ipmt=0;ipmt<128;ipmt++){
    float delta = fabs( (Peak.getCalibPar(ipmt))->getPeakChannel() 
                       -(Peak.getPrevCalib(ipmt))->getPeakChannel() );
    if ( delta > ( (Peak.getCalibPar(ipmt))->getDeviation() )*3.0 ) {
      (Peak.getCalibPar(ipmt))->setStatus(1);
    } else {  
      (Peak.getCalibPar(ipmt))->setStatus(0);
    }
  }
}

void CalibratorPeak::StoreToDB(const BbcTime_t& time, char* CalibType){
  Peak.store(time,CalibType);
}

