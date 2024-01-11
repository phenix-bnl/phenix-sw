#include "mBbcSetUcalModule.h"
#include "dBbcGeoWrapper.h"
#include "dBbcGhitRawParWrapper.h"
#include "dBbcUcalWrapper.h"
#include "BbcCalib.hh"

#include "RunToTime.hh"

//INCLUDECHECKER: Removed this line: #include "PHNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
//INCLUDECHECKER: Removed this line: #include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
#include "PHTable.hh"

#include "getClass.h"

#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean mBbcSetUcalModule::event(PHCompositeNode *root) {
  PHPointerList<PHNode> nodes;
  PHNodeIterator i(root), *j;
  PHNode *n;
  TableNode_t *d;
  PHTable *w;
  PHCompositeNode *outNode;

  outNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
  if (!outNode) {
    outNode = new PHCompositeNode("PAR");
    root->addNode(outNode);
  }

  n = i.findFirst("PHIODataNode", "dBbcGeo");
  if (!n) {
    cout << "ERROR:  'in' parameter dBbcGeo not found" << endl;
     w = new dBbcGeoWrapper("dBbcGeo", 1);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dBbcGeo");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
  if (!outNode) {
    outNode = new PHCompositeNode("PAR");
    root->addNode(outNode);
  }

  dBbcGhitRawParWrapper* BbcGhitRawParWrapper;

  j = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dBbcGhitRawPar"));
  if (!d) {
    cout << "ERROR:  'inout' parameter dBbcGhitRawPar not found" << endl;
    BbcGhitRawParWrapper = new dBbcGhitRawParWrapper("dBbcGhitRawPar", 1);
    if (!BbcGhitRawParWrapper) {
      return 1;
    }
    d = new TableNode_t(BbcGhitRawParWrapper,"dBbcGhitRawPar");
    outNode->addNode(d);
  }
  else {
    BbcGhitRawParWrapper = static_cast<dBbcGhitRawParWrapper*>(d->getData());
    if (!BbcGhitRawParWrapper) {
      return 1;
    }
  }
  delete j;
  nodes.append(d);

  outNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
  if (!outNode) {
    outNode = new PHCompositeNode("PAR");
    root->addNode(outNode);
  }

  BbcCalib* calib = findNode::getClass<BbcCalib>(outNode, "BbcCalibPar");
  if (!calib)
    {
      cout << "mBbcSetUcalModule> No BbcCalibPar node. Calibration constants will be restored." << endl;

      RunToTime *runtime = RunToTime::instance();
      calib = new BbcCalib();
      int run = BbcGhitRawParWrapper->get_RunNumber(0);
      int version = BbcGhitRawParWrapper->get_CalibVersion(0);
      calib->restore(*(runtime->getBeginTime(run)), version);

    }

  outNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "BBC"));
  if (!outNode) {
    outNode = new PHCompositeNode("BBC");
    root->addNode(outNode);
  }

  dBbcUcalWrapper* BbcUcalWrapper;

  j = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dBbcUcal"));
  if (!d) {
    cout << "ERROR:  'inout' parameter dBbcUcal not found" << endl;
    BbcUcalWrapper = new dBbcUcalWrapper("dBbcUcal", 128);
    if (!BbcUcalWrapper) {
      return 1;
    }
    d = new TableNode_t(BbcUcalWrapper,"dBbcUcal");
    outNode->addNode(d);
  }
  else {
    BbcUcalWrapper = static_cast<dBbcUcalWrapper*>(d->getData());
    if (!BbcUcalWrapper) {
      return 1;
    }
  }
  delete j;
  nodes.append(d);

  if ( BbcGhitRawParWrapper->get_SimFlag(0) == 2000 ) {
	  cout << "I am in mBbcSetUcalModule." << endl;

    BbcGhitRawParWrapper->set_AngleCut(0,45.0);
    BbcGhitRawParWrapper->set_Nindex(0,1.47);
    BbcGhitRawParWrapper->set_N0(0,50.0);
    BbcGhitRawParWrapper->set_MomLowerLim(0,0.00001);
    BbcGhitRawParWrapper->set_MaxAdc(0,4095.0);
    BbcGhitRawParWrapper->set_MaxTdc(0,4095.0);
    BbcGhitRawParWrapper->set_randseed(0,80298);
    BbcGhitRawParWrapper->set_MeanTDC_offset(0, 0.0); // valid from Y2
    BbcGhitRawParWrapper->set_Z0overC_offset(0, 0.0); // valid from Y2
    BbcGhitRawParWrapper->set_RunByRun_offset(0, 0.0);// valid from Y2
    BbcGhitRawParWrapper->set_ThresholdFactor(0, 1.0);// valid from Y2

    DBBCUCAL_ST* bbcucal = BbcUcalWrapper->TableData();
    DBBCUCAL_ST* bbcucalp;

    for (int ipmt=0;ipmt<128;ipmt++) {
      bbcucalp=bbcucal+ipmt;
      bbcucalp->Pmt            = ipmt;
      bbcucalp->AdcChGain      =  0.070;
      bbcucalp->TdcChGain0     =  0.007;
      bbcucalp->TdcChGain1     =  0.007;
      bbcucalp->TdcOffset0     =  0.;  // valid only for Y1
      bbcucalp->TdcOffset1     =  0.;  // valid only for Y1
      bbcucalp->TdcOver0_mean  =  0.0; // valid from Y2
      bbcucalp->TdcOver0_sigma =  0.0; // valid from Y2
      bbcucalp->TdcOver1_mean  =  0.0; // valid from Y2
      bbcucalp->TdcOver1_sigma =  0.0; // valid from Y2
      bbcucalp->TdcThreshold0  =  0.0; // valid from Y2
      bbcucalp->TdcThreshold1  =  0.0; // valid from Y2
      bbcucalp->PMTGain        =  0.0; // valid from Y2
      bbcucalp->PulseHeightReso=  0.2;
      bbcucalp->Pedestal       =  890.;
      bbcucalp->AdcGainFac     =  1.0;
      bbcucalp->SlewParA       =  0.0; // valid only for Y1
      bbcucalp->SlewParA0      =  0.0; // valid from Y2
      bbcucalp->SlewParB0      =  0.0; // valid from Y2
      bbcucalp->SlewParC0      =  0.0; // valid from Y2
      bbcucalp->SlewParA1      =  0.0; // valid from Y2
      bbcucalp->SlewParB1      =  0.0; // valid from Y2
      bbcucalp->SlewParC1      =  0.0; // valid from Y2
      bbcucalp->NoiseHeight    =  0.0;
      bbcucalp->NoiseHitProb   =  0.0;
      bbcucalp->TimeReso       =  0.050;
      bbcucalp->FakePede_mean  =  0.0; // valid from Y2
      bbcucalp->FakePede_sigma =  0.0; // valid from Y2
    }

  } else if ( BbcGhitRawParWrapper->get_SimFlag(0) == 2001 ) {

    // Default parameters for Y2 simulation
    BbcGhitRawParWrapper->set_AngleCut(0, 45.0);
    BbcGhitRawParWrapper->set_Nindex(0, 1.47);
    BbcGhitRawParWrapper->set_N0(0, 24.8);
    BbcGhitRawParWrapper->set_MomLowerLim(0, 0.00001);
    BbcGhitRawParWrapper->set_MaxAdc(0, 4095.);
    BbcGhitRawParWrapper->set_MaxTdc(0, 3200.);
    BbcGhitRawParWrapper->set_randseed(0, 80298);
    // TDC offset absorption only in MC if neccessary.
    // This should be obtained by BbcCalib.tzero-MeanTDC_offset*0.00725
    // For Y2 calibration Version 1002,
    // BbcCalib.tzero was fixed at 5.79867ns which is almost 800*0.00725ns
    // No need to absorbe at the moment of 2002/5/29
    BbcGhitRawParWrapper->set_MeanTDC_offset(0, 800.0);
    BbcGhitRawParWrapper->set_RunByRun_offset(0, 0.0);
    BbcGhitRawParWrapper->set_ThresholdFactor(0, 1.0);
    // This should be identical to BbcCalib.offset
    // For Y2 calibration Version 1002,
    // BbcCalib.offset was fixed at 0.0358581ns.
    BbcGhitRawParWrapper->set_Z0overC_offset(0, 0.0358581);


    DBBCUCAL_ST* bbcucal = BbcUcalWrapper->TableData();
    DBBCUCAL_ST* bbcucalp;

    int run;
    run = BbcGhitRawParWrapper->get_RunNumber(0);

    // ADC
    char calib_pedestal[50];
    char calib_adcconv[50];
    sprintf(calib_pedestal,"%d.pedestal",run);
    sprintf(calib_adcconv,"%d.adc",run);
    float par_pedestal[128];
    float par_adcconv[128];

    // TDC
    char calib_tdc0[50];
    char calib_tdc1[50];
    sprintf(calib_tdc0,"%d.tdc0",run);
    sprintf(calib_tdc1,"%d.tdc1",run);
    float par_tdc0[128];
    float par_tdc1[128];

    char calib_overflow0[50];
    char calib_overflow1[50];
    sprintf(calib_overflow0,"%d.overflow0",run);
    sprintf(calib_overflow1,"%d.overflow1",run);
    float par_overflow0_mean[128];
    float par_overflow0_sigma[128];
    float par_overflow1_mean[128];
    float par_overflow1_sigma[128];

    char calib_timereso[50];
    sprintf(calib_timereso,"%d.timereso",run);
    float par_timereso[128];

    char calib_adcreso[50];
    sprintf(calib_adcreso,"%d.pmtgain",run);
    float par_adcreso[128];
    float par_pmtgain[128];

    char calib_fakeped[50];
    sprintf(calib_fakeped,"%d.fakePedestal",run);
    float par_fakeped_mean[128];
    float par_fakeped_sigma[128];

    // Slewing Parameter
    char calib_slewpar0[50];
    char calib_slewpar1[50];
    sprintf(calib_slewpar0,"%d.slewpar0",run);
    sprintf(calib_slewpar1,"%d.slewpar1",run);
    float par_slewpar0A[128];
    float par_slewpar0B[128];
    float par_slewpar0C[128];
    float par_slewpar1A[128];
    float par_slewpar1B[128];
    float par_slewpar1C[128];

    // TDC Threshold [ch]
    char calib_thre[50];
    sprintf(calib_thre,"%d.threshold",run);
    float par_tdcthre0[128];
    float par_tdcthre1[128];

    float dum;

    FILE *f1;
    FILE *f2;
    FILE *f3;
    FILE *f4;
    FILE *f5;
    FILE *f6;
    FILE *f7;
    FILE *f8;
    FILE *f9;
    FILE *f10;
    FILE *f11;
    FILE *f12;

    f1 = fopen(calib_pedestal,"r");
    f2 = fopen(calib_adcconv,"r");
    f3 = fopen(calib_tdc0,"r");
    f4 = fopen(calib_tdc1,"r");
    f5 = fopen(calib_slewpar0,"r");
    f6 = fopen(calib_slewpar1,"r");
    f7 = fopen(calib_overflow0,"r");
    f8 = fopen(calib_overflow1,"r");
    f9 = fopen(calib_timereso,"r");
    f10 = fopen(calib_adcreso,"r");
    f11 = fopen(calib_fakeped,"r");
    f12 = fopen(calib_thre,"r");

    for(int i=0; i<128; i++){
      fscanf(f1,"%f %f %f",&par_pedestal[i],&dum,&dum);
      fscanf(f2,"%f %f %f %f %f %f %f",&dum,&par_adcconv[i],&dum,&dum,&dum,&dum,&dum);
      fscanf(f3,"%f %f %f %f %f %f %f",&dum,&par_tdc0[i],&dum,&dum,&dum,&dum,&dum);
      fscanf(f4,"%f %f %f %f %f %f %f",&dum,&par_tdc1[i],&dum,&dum,&dum,&dum,&dum);
      fscanf(f5,"%f %f %f %f %f %f %f",&par_slewpar0A[i],&par_slewpar0B[i],&par_slewpar0C[i],&dum,&dum,&dum,&dum);
      fscanf(f6,"%f %f %f %f %f %f %f",&par_slewpar1A[i],&par_slewpar1B[i],&par_slewpar1C[i],&dum,&dum,&dum,&dum);
      fscanf(f7,"%f %f %f",&par_overflow0_mean[i],&par_overflow0_sigma[i],&dum);
      fscanf(f8,"%f %f %f",&par_overflow1_mean[i],&par_overflow1_sigma[i],&dum);
      fscanf(f9,"%f",&par_timereso[i]);
      fscanf(f10,"%f %f %f",&par_pmtgain[i],&par_adcreso[i],&dum);
      fscanf(f11,"%f %f %f",&par_fakeped_mean[i],&par_fakeped_sigma[i],&dum);
      fscanf(f12,"%f %f",&par_tdcthre0[i],&par_tdcthre1[i]);
    }
    fclose(f1);
    fclose(f2);
    fclose(f3);
    fclose(f4);
    fclose(f5);
    fclose(f6);
    fclose(f7);
    fclose(f8);
    fclose(f9);
    fclose(f10);
    fclose(f11);
    fclose(f12);

    int ipmt;
    for (int jpmt=0;jpmt<128;jpmt++) {
      if(jpmt>63) {ipmt = jpmt - 64;}
      if(jpmt<64) {ipmt = jpmt + 64;}

      bbcucalp=bbcucal+jpmt;
      bbcucalp->Pmt            =  jpmt;
      bbcucalp->AdcChGain      =  par_adcconv[ipmt];
      bbcucalp->TdcChGain0     =  par_tdc0[ipmt];
      bbcucalp->TdcChGain1     =  par_tdc1[ipmt];
      bbcucalp->TdcOffset0     =  0.0; // valid only for Y1
      bbcucalp->TdcOffset1     =  0.0; // valid only for Y1
      bbcucalp->TdcOver0_mean  =  par_overflow0_mean[ipmt];
      bbcucalp->TdcOver0_sigma =  par_overflow0_sigma[ipmt];
      bbcucalp->TdcOver1_mean  =  par_overflow1_mean[ipmt];
      bbcucalp->TdcOver1_sigma =  par_overflow1_sigma[ipmt];
      bbcucalp->TdcThreshold0  =  (float) ((par_tdcthre0[ipmt] - par_pedestal[ipmt])*par_adcconv[ipmt]);
      bbcucalp->TdcThreshold1  =  (float) ((par_tdcthre1[ipmt] - par_pedestal[ipmt])*par_adcconv[ipmt]);
      bbcucalp->PulseHeightReso=  par_adcreso[ipmt];
      bbcucalp->PMTGain        =  par_pmtgain[ipmt];
      bbcucalp->Pedestal       =  par_pedestal[ipmt];
      bbcucalp->AdcGainFac     =  1.0;
      bbcucalp->SlewParA       =  0.0; // valid only for Y1
      bbcucalp->SlewParA0      =  par_slewpar0A[ipmt];
      bbcucalp->SlewParB0      =  par_slewpar0B[ipmt];
      bbcucalp->SlewParC0      =  par_slewpar0C[ipmt];
      bbcucalp->SlewParA1      =  par_slewpar1A[ipmt];
      bbcucalp->SlewParB1      =  par_slewpar1B[ipmt];
      bbcucalp->SlewParC1      =  par_slewpar1C[ipmt];
      bbcucalp->NoiseHeight    =  0.0;
      bbcucalp->NoiseHitProb   =  0.0;
      bbcucalp->TimeReso       =  par_timereso[ipmt];
      bbcucalp->FakePede_mean  =  par_fakeped_mean[ipmt];
      bbcucalp->FakePede_sigma =  par_fakeped_sigma[ipmt];

    }

  } else { 

    // This mode requires version 3000 or later.
    if (calib->getCalibrationVersion() < 3000) {
      cout << "mBbcSetUcalModule> This simulation mode requires version 3000 or later." << endl;
      cout << "mBbcSetUcalModule> Check the used calibration version !" << endl;
      return 1;
    }

    // Default parameters for Y3 simulation
    BbcGhitRawParWrapper->set_AngleCut(0, 45.0);
    BbcGhitRawParWrapper->set_Nindex(0, 1.47);
    BbcGhitRawParWrapper->set_N0(0, 24.8);
    BbcGhitRawParWrapper->set_MomLowerLim(0, 0.00001);
    BbcGhitRawParWrapper->set_MaxAdc(0, 4095.);
    BbcGhitRawParWrapper->set_MaxTdc(0, 3200.);
    BbcGhitRawParWrapper->set_randseed(0, 80298);
    BbcGhitRawParWrapper->set_RunByRun_offset(0, 0.0);
    BbcGhitRawParWrapper->set_ThresholdFactor(0, 1.0);
    // TDC offset absorption only in MC if neccessary.
    // This should be obtained so that  BbcCalib.tzero equal to MeanTDC_offset*0.00725
    float MeanTdcOffset = (((calib->getTZeroOff())->getCalibPar(0))->getPeakChannel())/0.00725;
    BbcGhitRawParWrapper->set_MeanTDC_offset(0, MeanTdcOffset);
    // This should be identical to BbcCalib.offset in the second row
    float ZOffset = ((calib->getOffset())->getCalibPar(1))->getPeakChannel();
    BbcGhitRawParWrapper->set_Z0overC_offset(0, ZOffset);


    DBBCUCAL_ST* bbcucal = BbcUcalWrapper->TableData();
    DBBCUCAL_ST* bbcucalp;

    // ADC
    float par_pedestal[128];
    float par_adcconv[128];
    float par_adcreso[128];
    float par_pmtgain[128];
    float par_fakeped_mean[128];
    float par_fakeped_sigma[128];

    // TDC
    float par_tdc0[128];
    float par_tdc1[128];
    float par_overflow0_mean[128];
    float par_overflow0_sigma[128];
    float par_overflow1_mean[128];
    float par_overflow1_sigma[128];
    float par_timereso[128];
    float par_tdcthre0[128];
    float par_tdcthre1[128];

    // Slewing Parameter
    float par_slewpar0A[128];
    float par_slewpar0B[128];
    float par_slewpar0C[128];
    float par_slewpar1A[128];
    float par_slewpar1B[128];
    float par_slewpar1C[128];

    for(int i=0; i<128; i++){
      par_pedestal[i] = ((calib->getPedestal())->getCalibPar(i))->getPeakChannel();
      par_adcconv[i] = ((calib->getAdcGain())->getCalibPar(i))->getPar1();
      par_tdc0[i] = ((calib->getTdcGain0())->getCalibPar(i))->getPar1();
      par_tdc1[i] = ((calib->getTdcGain1())->getCalibPar(i))->getPar1();
      par_slewpar0A[i] = ((calib->getSlewing0())->getCalibPar(i))->getPar0();
      par_slewpar0B[i] = ((calib->getSlewing0())->getCalibPar(i))->getPar1();
      par_slewpar0C[i] = ((calib->getSlewing0())->getCalibPar(i))->getPar2();
      par_slewpar1A[i] = ((calib->getSlewing1())->getCalibPar(i))->getPar0();
      par_slewpar1B[i] = ((calib->getSlewing1())->getCalibPar(i))->getPar1();
      par_slewpar1C[i] = ((calib->getSlewing1())->getCalibPar(i))->getPar2();
      par_overflow0_mean[i]  = ((calib->getOverflow0())->getCalibPar(i))->getPeakChannel();
      par_overflow0_sigma[i] = ((calib->getOverflow0())->getCalibPar(i))->getDeviation();
      par_overflow1_mean[i]  = ((calib->getOverflow1())->getCalibPar(i))->getPeakChannel();
      par_overflow1_sigma[i] = ((calib->getOverflow1())->getCalibPar(i))->getDeviation();
      par_timereso[i]  = ((calib->getTimeReso())->getCalibPar(i))->getPeakChannel();
      par_pmtgain[i]  = ((calib->getPmtGain())->getCalibPar(i))->getPeakChannel();
      par_adcreso[i]  = ((calib->getPmtGain())->getCalibPar(i))->getDeviation();
      par_fakeped_mean[i]   = ((calib->getFakePed())->getCalibPar(i))->getPeakChannel();
      par_fakeped_sigma[i]  = ((calib->getFakePed())->getCalibPar(i))->getDeviation();
      // Threshold bank contains ADC values for TDC0 and TDC1 in the 1st and 2nd column respectively.
      par_tdcthre0[i]   = ((calib->getThreshold())->getCalibPar(i))->getPeakChannel();
      par_tdcthre1[i]   = ((calib->getThreshold())->getCalibPar(i))->getDeviation();
    }

    int ipmt;
    for (int jpmt=0;jpmt<128;jpmt++) {
      if(jpmt>63) {ipmt = jpmt - 64;}
      if(jpmt<64) {ipmt = jpmt + 64;}

      bbcucalp=bbcucal+jpmt;
      bbcucalp->Pmt            =  jpmt;
      bbcucalp->AdcChGain      =  par_adcconv[ipmt];
      bbcucalp->TdcChGain0     =  par_tdc0[ipmt];
      bbcucalp->TdcChGain1     =  par_tdc1[ipmt];
      bbcucalp->TdcOffset0     =  0.0; // valid only for Y1
      bbcucalp->TdcOffset1     =  0.0; // valid only for Y1
      bbcucalp->TdcOver0_mean  =  par_overflow0_mean[ipmt];
      bbcucalp->TdcOver0_sigma =  par_overflow0_sigma[ipmt];
      bbcucalp->TdcOver1_mean  =  par_overflow1_mean[ipmt];
      bbcucalp->TdcOver1_sigma =  par_overflow1_sigma[ipmt];
      bbcucalp->TdcThreshold0  =  (float) ((par_tdcthre0[ipmt] - par_pedestal[ipmt])*par_adcconv[ipmt]);
      bbcucalp->TdcThreshold1  =  (float) ((par_tdcthre1[ipmt] - par_pedestal[ipmt])*par_adcconv[ipmt]);
      bbcucalp->PulseHeightReso=  par_adcreso[ipmt];
      bbcucalp->PMTGain        =  par_pmtgain[ipmt];
      bbcucalp->Pedestal       =  par_pedestal[ipmt];
      bbcucalp->AdcGainFac     =  1.0;
      bbcucalp->SlewParA       =  0.0; // valid only for Y1
      bbcucalp->SlewParA0      =  par_slewpar0A[ipmt];
      bbcucalp->SlewParB0      =  par_slewpar0B[ipmt];
      bbcucalp->SlewParC0      =  par_slewpar0C[ipmt];
      bbcucalp->SlewParA1      =  par_slewpar1A[ipmt];
      bbcucalp->SlewParB1      =  par_slewpar1B[ipmt];
      bbcucalp->SlewParC1      =  par_slewpar1C[ipmt];
      bbcucalp->NoiseHeight    =  0.0;
      bbcucalp->NoiseHitProb   =  0.0;
      bbcucalp->TimeReso       =  par_timereso[ipmt];
      bbcucalp->FakePede_mean  =  par_fakeped_mean[ipmt];
      bbcucalp->FakePede_sigma =  par_fakeped_sigma[ipmt];
    }

  }

  return 1;   
}
