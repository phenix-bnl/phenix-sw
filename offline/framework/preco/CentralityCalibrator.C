#include "CentralityCalibrator.h"

#include <RunNumberRanges.h>
#include <recoConsts.h>
#include <getClass.h>
#include <Fun4AllReturnCodes.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <RunHeader.h>
#include <PHGlobal.h>

#include <TFile.h>
#include <TF1.h>
#include <TH1F.h>
#include <TH2F.h>
#include <TProfile.h>
#include <TNtuple.h>

#include <iostream>
#include <gsl/gsl_math.h> 

//using namespace std;

CentralityCalibrator::CentralityCalibrator(const char* Type, const char* name) : SubsysReco(name)
{
  Input=1;
  bbcFraction=0.93; // fraction of total crossection seen by BBC
  bbcScale=5.6;     // calibration parameters
  bbcShift=1.0;
  bbcPower=0.25;
  zdcMax=9000.;     // This number in run4 increased by a factor of 2.0 compared to run2
  OutputFile=NULL;
  InputFile=NULL;
}

void CentralityCalibrator::Book() 
{
  nbins=100000; start=-2.; stop=2.;
  hBbcZdcAngle = new TH1F("hBbcZdcAngle","BBC/ZDC angle",nbins,start,stop);
  hBbcZdcAngleCSum = new TH1F("hBbcZdcAngleCSum","BBC/ZDC angle cumulative sum",nbins,start,stop);

  prof1 = new TProfile("prof1"," ",80,0.22,0.92);
  hcalib1 = new TH1F("hcalib1","",25000,0.,1.0);
  perpCSum = new TH1F("perpSCum","cumulative sum for perp method",25000,0.,1.);

  return;
}

void CentralityCalibrator::CalibrateFromNtuple(char* fname)
{
  InputFile = new TFile(fname,"READ");
  InputFile->ls();
  Book();
  InputFile->cd();
  ntpcent = (TNtuple*)InputFile->Get("ntpcent");

  ntpcent->Print();
  int nentries = (int)ntpcent->GetEntries();
  std::cout << PHWHERE << " Calibrating from Ntuple; Nentries = " << nentries << std::endl;

  float bbc1; float bbc2; float zdc1; float zdc2;

  ntpcent->SetBranchAddress("bbc1", &bbc1);
  ntpcent->SetBranchAddress("bbc2", &bbc2);
  ntpcent->SetBranchAddress("zdc1", &zdc1);
  ntpcent->SetBranchAddress("zdc2", &zdc2);

// Loop over all entries and fill histograms
// for CLOCK method and for first step for PERP method
  std::cout << PHWHERE << " First Pass Starting... " << std::endl;
  for(int i=0; i<nentries; i++) {

    ntpcent->GetEvent(i);

    if(bbc1!=0 && bbc2!=0 && zdc1!=0 && zdc2!=0) {

      double x = (pow(bbc1+bbc2,bbcPower)-bbcShift)/bbcScale;
      double y = (zdc1+zdc2)/zdcMax;
                                                                                                                             
      double angle = atan2(x-0.5,y);
      hBbcZdcAngle->Fill(float(angle));
                                                                                                                             
      prof1->Fill(x,y);

    }

  }

  std::cout << PHWHERE << " Calibrating by CLOCK... " << std::endl;
  CalibrateByClock();

// Fit the results of the first step for PERP method
  prof1->Fit("pol4");
  TF1* fit1 = prof1->GetFunction("pol4");

  std::cout << PHWHERE << " Starting Second Pass for PERP..." << std::endl;
// Second step for PERP method. Use fit results from step one
  for(int i=0; i<nentries; i++) {
  if(i%1000==0) std::cout << i << std::endl;
                                                                                                                             
    ntpcent->GetEvent(i);
                                                                                                                             
    if(bbc1!=0 && bbc2!=0 && zdc1!=0 && zdc2!=0) {
                                                                                                                             
      float xx = (pow(bbc1+bbc2,bbcPower)-bbcShift)/bbcScale;
      float yy = (zdc1+zdc2)/zdcMax;
                                                                                                                             
      float error = 9999.; int i=0; float X=0.,Y=-999.;
      if(xx>0.2) { X=xx-0.2; }
        while(i<400) {
          float value = yy - fit1->Eval(X);
          value = (xx-X)*(xx-X)+value*value;
            if(value<error) { error=value; Y=X; }
            X += 0.001; i++;
        }
        hcalib1->Fill(1.-Y);
    }
                                                                                                                             
  }

  std::cout << PHWHERE << " Calibrating by PERP... " << std::endl;
  CalibrateByPerp();

  return;
}

int CentralityCalibrator::Init(PHCompositeNode *topNode)
{
  if(Input==0) { OutputFile = new TFile("centcalib.root","RECREATE"); }
  Book();
  ntpcent = new TNtuple("ntpcent","BBC and ZDC","bbc1:bbc2:zdc1:zdc2");

  return EVENT_OK;
}

int CentralityCalibrator::InitRun(PHCompositeNode *topNode)
{
/*
  //if(!GetNodes(topNode)){ return ABORTRUN; }
  RunHeader * d_runhdr = findNode::getClass<RunHeader>(topNode, "RunHeader");

  if (!d_runhdr)    {
    std::cout << PHWHERE << " ERROR: RunHeader not in the Node Tree" << std::endl;
    return -1;
  }
  
  //unsigned int runnumber = d_global->getRunNumber();
  unsigned int runnumber = d_runhdr->get_RunNumber();
  if(runnumber<BEGIN_OF_RUN4||runnumber>200000) { 
    std::cout << PHWHERE << " ERROR: CentralityCalibrator currently works only for Run4 " << std::endl; 
    return ABORTRUN; 
  }
*/
  return EVENT_OK;
}

int CentralityCalibrator::GetNodes(PHCompositeNode *topNode){
  d_global =   findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  if(!d_global){
    std::cout<< PHWHERE <<" ERROR: PHGlobal not in the Node Tree"<<std::endl;
    return 0;
  }
  return 1;
}

int CentralityCalibrator::process_event(PHCompositeNode *topNode)
{
  if(!GetNodes(topNode)){ return ABORTRUN; }

  float nc[4];
  float bbc1 = d_global->getBbcChargeN();
  float bbc2 = d_global->getBbcChargeS();
  float zdc1 = d_global->getZdcEnergyN();
  float zdc2 = d_global->getZdcEnergyS();

  if(bbc1!=0 && bbc2!=0 && zdc1!=0 && zdc2!=0) {

    double x = (pow(bbc1+bbc2,bbcPower)-bbcShift)/bbcScale;
    double y = (zdc1+zdc2)/zdcMax;

    double angle = atan2(x-0.5,y);
    hBbcZdcAngle->Fill(float(angle));
  
    prof1->Fill(x,y); 

      nc[0]=bbc1;
      nc[1]=bbc2;
      nc[2]=zdc1;
      nc[3]=zdc2;
      ntpcent->Fill(nc);
  }

  return EVENT_OK;
}

void CentralityCalibrator::CalibrateByPerp() {

  int lastbin = int(bbcFraction*100.);
                                                                                                                             
  float csum=0.;
  for(int i=1; i<=25000; i++) {
    float a = hcalib1->GetBinContent(i);
    csum = csum + a;
    float bin = hcalib1->GetBinCenter(i);
    perpCSum->Fill(bin,csum);
  }
  perpCSum->Scale(1./csum*bbcFraction);
                                                                                                                             
  std::cout << "Centrality by PERP:" << std::endl;
  for(int j=0; j<=lastbin; j++) {
    for(int i=1; i<25000; i++) {
      if( perpCSum->GetBinContent(i) >= (0.01*j) ) {
        std::cout << "    phiCut[" << j << "] = " << perpCSum->GetBinCenter(i) << ";" << std::endl;
        break;
      }
    }
  }

  return;
}

void CentralityCalibrator::CalibrateByClock() {

  int lastbin = int(bbcFraction*100.);
                                                                                                                             
  float csum=0.;
  for(int i=1; i<=nbins; i++) {
    float a = hBbcZdcAngle->GetBinContent(i);
    csum = csum + a;
    float bin = hBbcZdcAngle->GetBinCenter(i);
    hBbcZdcAngleCSum->Fill(bin,csum);
  }
  hBbcZdcAngleCSum->Scale(1./csum*bbcFraction);
                                                                                                                             
  std::cout << "Centrality by CLOCK:" << std::endl;
  for(int j=lastbin; j>=0; j--) {
    for(int i=nbins; i>0; i--) {
      if( (hBbcZdcAngleCSum->GetBinContent(i)-0.000001) <= (0.01*j) ) {
        std::cout << "    phiCut[" << lastbin-j << "] = " << hBbcZdcAngleCSum->GetBinCenter(i) << ";" << std::endl;
        break;
      }
    }
  }

  return;
}

int CentralityCalibrator::End(PHCompositeNode *topNode) {

  CalibrateByClock();
  //CalibrateByPerp();

// Write out the results
  if(Input==0 && OutputFile) {
    OutputFile->Write();
    OutputFile->Close();
    delete OutputFile;
    OutputFile=0;
  }

  return EVENT_OK;
}




