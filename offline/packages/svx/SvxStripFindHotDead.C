// ===============
// FILE: SvxStripFindHotDead.C
// ===============

// *****************************************************************************************************
// Author:  Sasha Lebedev (lebedev@iastate.edu)
// Revisions: September 2010 - initial version
// Updated by Rachid Nouicer
// Revision: February 2012 -  
// implementation of dead ladders and Hybrids of Run-11 and Run-12
// output set to be as Ntuple in root file  
// tuned for pp on May 12, 2012; Rachid Nouicer
//December 14, 2012 (Rachid Nouicer): Run-11, new ladders have been masked B2L14, B3L10 and B3L00 
// *****************************************************************************************************

#include <cstdlib>
#include <fstream>
//#include <ifstream>
#include <iomanip>
#include <cmath>
#include <cstdio>
#include <phool.h>
#include <iostream>
#include <string>

//#include <SvxCommon.h>
#include <SvxParameters.h>
#include <SvxStripFindHotDead.h>
#include <SvxRawhitv2.h>
#include <SvxRawhitListv2.h>
#include <svxAddress.hh>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>

#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <getClass.h>

#include <TFile.h>
#include <TH2F.h>
#include <TNtuple.h>

//#include "threshold.h"

//using namespace std;

// Helpers for scanning Node Tree...
typedef PHIODataNode <PHObject>             PHObjectNode_t;
typedef PHIODataNode <SvxRawhitList>        SvxRawhitListNode_t;

//----------------------------------------------------------------------------------------------------

SvxStripFindHotDead::SvxStripFindHotDead(const std::string &name): SubsysReco(name)
{
  char def_threshfile[256] = "threshold.h";
  thresholdfilename = def_threshfile;
  
  fOutputName = "VtxStripDeadChannels.root";
  OutputFileName="none";//"svxStripDeadMap.txt";
  fRunFlag = SvxStripFindHotDead::Run11;
  EventNumber=0;
}

//------------------------------------------------------------------------------------------------------

// Run-independent initialization
int SvxStripFindHotDead::Init(PHCompositeNode *topNode)
{
  if(verbosity>0) std::cout << "SvxStripFindHotDead::Init() Execution started..." << std::endl;

  if(set_Threshold()==1) {std::cout<<"Can't load ADC threshold value. Stop!"<<std::endl; return ABORTRUN;}

  char hname[100];
  int nbinschnl=6*128;
  int nbinsadc=256;
  float startchnl = -0.5; float stopchnl = 6*128.-0.5;
  float startadc = -0.5; float stopadc = 256.-0.5;
  int Nladder[]={SVXLADDERSLAYER2*2,SVXLADDERSLAYER3*2};
  int Nsensor[]={SVXSENSORSLAYER2,SVXSENSORSLAYER3};
  for(int k=0;k<SVXLAYERNUMBER-2;k++) {
    for(int i=0;i<Nladder[k];i++){
      for(int j=0;j<Nsensor[k];j++){

	sprintf(hname,"hsvxrawhit_lay%d_lad%d_sens%d_X",k+2,i,j);
	hsvxrawhit[k][i][j][0] = new TH2F(hname,hname,nbinschnl,startchnl,stopchnl,nbinsadc,startadc,stopadc);
	sprintf(hname,"hsvxrawhit_lay%d_lad%d_sens%d_U",k+2,i,j);
	hsvxrawhit[k][i][j][1] = new TH2F(hname,hname,nbinschnl,startchnl,stopchnl,nbinsadc,startadc,stopadc);
      }
    }
  }
  if( verbosity>0)std::cout << "SvxStripFindHotDead::Init() Execution completed." << std::endl;  //set adc threshold
  
  fOutFile = new TFile(fOutputName,"RECREATE");
  fOutFile->cd();

  nt = new TNtuple("nt","VTX Strip Dead Channels","layer:ladder:sensor:section:readout:channel:adcmean:adcrms:flag:threshold");
  nt->SetDirectory(fOutFile);

  ntev = new TNtuple("ntev","Events Processed","event");
  ntev->SetDirectory(fOutFile);

  return EVENT_OK;

}

//----------------------------------------------------------------------------------------------------

// Run-dependent initialization
int SvxStripFindHotDead::InitRun(PHCompositeNode *topNode)
{

  if(verbosity>0) std::cout << "SvxStripFindHotDead::InitRun() Execution started..." << std::endl;

  ///////////////////////
  // added by T.Hachiya 2011.06.17
  address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( address == NULL) {
    if(verbosity>0) { std::cout << PHWHERE<< "Can't find svxAddress. " << std::endl; }
    return ABORTRUN;
  }

  if(verbosity>0) std::cout << "SvxStripFindHotDead::InitRun() Execution finished." << std::endl;

  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------

int SvxStripFindHotDead::process_event(PHCompositeNode *topNode)
{

  //if(verbosity>0) std::cout << "SvxStripFindHotDead::process_event() Execution started..." <<std::endl;

  //if(verbosity>0 && EventNumber==0)  { std::cout << "SvxStripFindHotDead topNode:"<< std::endl; topNode->print(); }

  if(verbosity>0&&EventNumber%10000==0) std::cout << "SvxStripFindHotDead::process_event() Getting raw hits...    event# "<<EventNumber <<std::endl;


  SvxRawhitList* d_rawhit = NULL;
  PHTypedNodeIterator<SvxRawhitList> iRAWHIT(topNode);
  SvxRawhitListNode_t *RAWHIT = iRAWHIT.find("SvxRawhitList");
  if (RAWHIT) d_rawhit = RAWHIT->getData();
  if (!d_rawhit) {
    std::cout << PHWHERE << "SvxStripFindHotDead ERROR: rawhit data not in the Node Tree" << std::endl;
    return EVENT_OK;
  }

  int nSvxRawhits = d_rawhit->get_nRawhits();
  if(verbosity>0) {std::cout << "SvxStripFindHotDead: Initial number of raw hits = " << nSvxRawhits << std::endl;}

  for(int i=0; i<nSvxRawhits; i++) {
    SvxRawhit* tmp = d_rawhit->get_Rawhit(i);
    int layer = tmp->get_layer();
    if(layer<2) continue; // Stripixel only
    int ladder = tmp->get_ladder();
    int sensor = tmp->get_sensor();
    int sensorSection=tmp->get_sensorSection();
    int readout = tmp->get_sensorReadout();
    int channel = tmp->get_channel();
    int adc = tmp->get_adc();
    int roc=address->getStripRoc(sensorSection,readout,channel);
    int rocchnl=address->getStripRocChannel(sensorSection,readout,channel);

    if( roc==-1 || rocchnl==-1 ){
      std::cerr<<"SvxStripThreshold::"<<__FUNCTION__<<" : "
               <<"Roc or RocChannel is out of range : "
               <<"ROC="<<roc
               <<"ROCChannel="<<rocchnl<<std::endl;
      std::cerr<<"  skip this hit!!"<<std::endl;
      continue;
    }

      
    int th=thresh[layer-2][ladder][sensor][roc][rocchnl];
 
    if(th!=255&&th!=0) {
      th-=Threshold_shift;
      if(adc>th)    {
	hsvxrawhit[layer-2][ladder][sensor][readout]->Fill(sensorSection*128*3+channel,adc);
      }      
    }  
  }

  //if(verbosity>0) {std::cout << "SvxStripFindHotDead::process_event() Event processed." <<std::endl;}
   
  if(EventNumber%10000==0)std::cout<<"======== Event_b"<<" # "<<EventNumber<<std::endl;
  EventNumber++;
  return EVENT_OK;
  // std::cout<<""<<" # "<<EventNumber<<std::endl;

}

//---------------------------------------------------------------------------------------------------------
int SvxStripFindHotDead::End(PHCompositeNode *topNode)
{
  ntev->Fill(EventNumber);  

  if(verbosity>0) {std::cout << "SvxStripFindHotDead::End() Started..." <<std::endl;}
  
  int flag;
  int ladflag;
  int sensflag;
  int chipflag;
  
  //  char hname[100];
  //--char c_ro;
  int N_ladder[]={SVXLADDERSLAYER2*2,SVXLADDERSLAYER3*2};
  int N_sensor[]={SVXSENSORSLAYER2,SVXSENSORSLAYER3};
  int N_readout=2;
  int N_chnls=6*128;
  int N_chnls_sensec=3*128;
  
  bool kWriteTxt = false;
  if(!TString(OutputFileName.c_str()).Contains("none")) { kWriteTxt = true; }

  std::ofstream* outfile = NULL;
  if(kWriteTxt) {
    outfile = new std::ofstream(OutputFileName.c_str()); }

  for(int i=0;i<SVXLAYERNUMBER-2;i++)
  {  //layer
    for(int j=0;j<N_ladder[i];j++)
    {  //ladder
      ladflag= DeadLadder(i+2,j,Mark_Type);
      if(ladflag==1) {
	//std::cout<<"bad ladder: lay"<<i+2<<" lad"<<j<<std::endl;
	for(int k=0;k<N_readout;k++){  //readout
	  for(int isensor=0;isensor<N_sensor[i];isensor++){
	    for(int ichnl=0;ichnl<N_chnls;ichnl++) { //channel
	      int sensection=ichnl<N_chnls_sensec?0:1;
	      int chnl=ichnl<N_chnls_sensec?ichnl:ichnl-N_chnls_sensec;
	      int roc=address->getStripRoc(sensection,k,chnl);
	      int rocchnl=address->getStripRocChannel(sensection,k,chnl);

	      int th=-1;
              if( roc==-1 || rocchnl==-1 ){
                std::cerr<<"SvxStripThreshold::"<<__FUNCTION__<<" : "
                         <<"Roc or RocChannel is out of range : "
                         <<"ROC="<<roc
                         <<"ROCChannel="<<rocchnl<<std::endl;
                std::cerr<<"  threshold is set as -1"<<std::endl;
              }
              else {
	        th=thresh[i][j][isensor][roc][rocchnl];
              }

	      
	      if(kWriteTxt) {
		(* outfile) <<i<<" "<<j<<" "<<isensor<<" "<<sensection<<" "<<k<<" "<<chnl<<" "<<0<<" "<<0<<" "<<-1<<" "<<th<<std::endl; }
	      nt->Fill(i,j,isensor,sensection,k,chnl,0,0,-1,th);
	    }
	  }
	}
	continue;	 
      }

      for(int k=0;k<N_readout;k++){  //readout
	
	//--c_ro=(k==0)?'X':'U';
	
	for(int isensor=0;isensor<N_sensor[i];isensor++){  //sensor
	  
	  sensflag=DeadSensor(i+2,j,isensor,Mark_Type);
	  
	  if(sensflag==1){

	    //std::cout<<"bad sensor: lay"<<i+2<<" lad"<<j<<" sens"<<isensor<<" readout"<<k<<std::endl;
	    for(int ichnl=0;ichnl<N_chnls;ichnl++) { //channel

	      int sensection=ichnl<N_chnls_sensec?0:1;
	      int chnl=ichnl<N_chnls_sensec?ichnl:ichnl-N_chnls_sensec;
	      int roc=address->getStripRoc(sensection,k,chnl);
	      int rocchnl=address->getStripRocChannel(sensection,k,chnl);
	      int th=-1;
              if( roc==-1 || rocchnl==-1 ){
                std::cerr<<"SvxStripThreshold::"<<__FUNCTION__<<" : "
                         <<"Roc or RocChannel is out of range : "
                         <<"ROC="<<roc
                         <<"ROCChannel="<<rocchnl<<std::endl;
                std::cerr<<"  threshold is set as -1"<<std::endl;
              }
              else {
	        th=thresh[i][j][isensor][roc][rocchnl];
              }


	      if(kWriteTxt) {
		(* outfile)<<i<<" "<<j<<" "<<isensor<<" "<<sensection<<" "<<k<<" "<<chnl<<" "<<0<<" "<<0<<" "<<-1<<" "<<th<<std::endl; }
	      nt->Fill(i,j,isensor,sensection,k,chnl,0,0,-1,th);
	    }
	    continue;	   
	  }
	  
	 	    
	  for(int ichip=0;ichip<6;ichip++) {

	    int chip;//,chipchnl;
	    double mean, rms;
	    chip=ichip+k*6;

	    chipflag=DeadChip(i+2,j,isensor,chip,Mark_Type);
	    if(chipflag==1){
	      //std::cout<<"bad chip: lay"<<i+2<<" lad"<<j<<" sens"<<isensor<<" readout"<<k<<" chip"<<chip<<std::endl;

	      for(int ich=0;ich<128;ich++){

		int sensection=ichip<3?0:1;
		int chnl=ichip<3?ichip*128+ich:(ichip-3)*128+ich;
		int roc=address->getStripRoc(sensection,k,chnl);
		int rocchnl=address->getStripRocChannel(sensection,k,chnl);
	        int th=-1;
                if( roc==-1 || rocchnl==-1 ){
                  std::cerr<<"SvxStripThreshold::"<<__FUNCTION__<<" : "
                           <<"Roc or RocChannel is out of range : "
                           <<"ROC="<<roc
                           <<"ROCChannel="<<rocchnl<<std::endl;
                  std::cerr<<"  threshold is set as -1"<<std::endl;
                }
                else {
	          th=thresh[i][j][isensor][roc][rocchnl];
                }

		if(kWriteTxt) {
		  (* outfile)<<i<<" "<<j<<" "<<isensor<<" "<<sensection<<" "<<k<<" "<<chnl<<" "<<0<<" "<<0<<" "<<-1<<" "<<th<<std::endl; }
		nt->Fill(i,j,isensor,sensection,k,chnl,0,0,-1,th);
		
	      }
	      continue;
	    }
	  
	    for(int ichnl=ichip*128;ichnl<ichip*128+128;ichnl++) { //channel
 	      mean=Get_AdcMean(hsvxrawhit[i][j][isensor][k],ichnl);
 	      rms=Get_AdcRMS(hsvxrawhit[i][j][isensor][k],ichnl);
	      //if(mean>150) std::cout<<i<<" "<<j<<" "<<isensor<<" "<<((ichip<3)?0:1)<<" "<<k<<" "<<((ichip<3)?ichip*128+ichnl:(ichip-3)*128+ichnl)<<" "<<mean<<std::endl;
	      	      
	      flag = 0;  
	      // Dead channel
	      //if( mean<30)
	      if( mean<10)
		{flag = -1;}
	      
	      // Hot channels
	      else if (mean>180)
		
		{flag = 1; }
	      //bad_rms channels
	      //else if( (rms < 20||rms>100))
		else if(rms>100)
		{flag = 2; }
	      int sensection=ichnl<N_chnls_sensec?0:1;
	      int chnl=ichnl<N_chnls_sensec?ichnl:ichnl-N_chnls_sensec;
	      int roc=address->getStripRoc(sensection,k,chnl);
	      int rocchnl=address->getStripRocChannel(sensection,k,chnl);
	      int th=-1;
              if( roc==-1 || rocchnl==-1 ){
                std::cerr<<"SvxStripThreshold::"<<__FUNCTION__<<" : "
                         <<"Roc or RocChannel is out of range : "
                         <<"ROC="<<roc
                         <<"ROCChannel="<<rocchnl<<std::endl;
                std::cerr<<"  threshold is set as -1"<<std::endl;
              }
              else {
	        th=thresh[i][j][isensor][roc][rocchnl];
              }

	      if(kWriteTxt) {
		(* outfile)<<i<<" "<<j<<" "<<isensor<<" "<<sensection<<" "<<k<<" "<<chnl<<" "<<mean<<" "<<rms<<" "<<flag<<" "<<th<<std::endl;}
	      nt->Fill(i,j,isensor,sensection,k,chnl,mean,rms,flag,th);
	      //layer ladder sensor sensorSection sensorReadout channel mean rms flag threshold
	    }//chnl
	    	    
	  }//chip
	}//sensor
     }	//readout
    }//ladder
    }//layer

 
 
  if(kWriteTxt) {
    outfile->close(); 
    delete outfile;}

  fOutFile->cd();
  nt->Write();
  ntev->Write();
  fOutFile->Close();

  return EVENT_OK;
}

  
double SvxStripFindHotDead::Get_AdcMean(TH2F *h2, int ichnl)
{

  TH1F* h1;

  int binchnl=h2->GetXaxis()->FindBin((double)ichnl);

  h1=(TH1F*)h2->ProjectionY("hmean0",binchnl,binchnl);
  double mean = h1->GetMean();
  delete h1;

  return mean;
//  return h1->GetMean();
}

double  SvxStripFindHotDead::Get_AdcRMS(TH2F *h2, int ichnl)
{

  TH1F* h1;

  int binchnl=h2->GetXaxis()->FindBin((double)ichnl);

  h1=(TH1F*)h2->ProjectionY("hrms0",binchnl,binchnl);
  double rms = h1->GetRMS();
  delete h1;
  return rms;
//  return h1->GetRMS();
}




int SvxStripFindHotDead::DeadLadder(int lay, int lad,int type)
{
  if(fRunFlag==SvxStripFindHotDead::Run11)
    {
      if(lay==2)
	{ 
	  if(lad==5||lad==7||lad==9||lad==13||lad==15||lad==14)//lad=14 added in December 14, 2012 (Rachid)
	    {
	      return 1; //always dead ladder
	    }
	}
      else if(lay==3)
	{
	  if(lad==13) 
	    {
	      return 1; //always dead ladder
	    }
	}
    }
  
  return 0;
}

int  SvxStripFindHotDead::DeadSensor(int lay, int lad, int sens,int type)
{
  if(fRunFlag==SvxStripFindHotDead::Run11)
    {
      //std::cout << "Run 11" << std::endl;
      if(lay==3)
	{
	  if(lad==9)
	    {
	      if(sens==4 || sens==5)
		{
		  return 1; 
		}
	    }

//new B3L10 and B3L00 (Rachid: added on December 14, 2014)
//--------------------------------------------------------

      if(lay==3)
	{ 
	  if(lad==10||lad==0)
	    {
	      return 1; //always dead ladder
	    }
	}
//-------------------------------------------------------
	  if(lad==23)
	    {
	      if(sens==4)
		{
		  return 1; 
		}
	    }  
	}
    }
  
  if(fRunFlag==SvxStripFindHotDead::Run12)
    {
      //std::cout << "Run 12" << std::endl;
     if(lay==2)
	{
	  if(lad==6)
	    {
	      if(sens==4)
		{
		  return 1;
		}
	    }
	  if(lad==11)
	    {
	      if(sens==2||sens==4)
		{
		  return 1;
		}
	    }
	  if(lad==12)
	    {
	      if(sens==1)
		{
		  return 1;
		}
	    }
	}
      else if(lay==3)
	{
	  if(lad==6)
	    {
	      if(sens==1)
		{
		  return 1; 
		}
	    }
	  if(lad==9)
	    {
	      if(sens==5)
		{
		  return 1; 
		}
	    }  
	  if(lad==13)
	    {
	      if(sens==5)
		{
		  return 1; 
		}
	    }  
	}
    }

  return 0;
}


int  SvxStripFindHotDead::DeadChip(int lay, int lad, int sens, int chip, int type)
{
  if(fRunFlag==SvxStripFindHotDead::Run11)
    {
      if(lay==2)
	{
	  if(lad==4)
	    {
	      if(sens==1)
		{
		  if(chip==0||chip==6||chip==7)
		    {
		      return 1;
		    }
		}
	    }
	}
    }
  
  if(fRunFlag==SvxStripFindHotDead::Run12)
    {
      if(lay==2)
	{
	  if(lad==4)
	    {
	      if(sens==1)
		{
		  if(chip==9||chip==10||chip==11)
		    {
		      return 1;
		    }
		}
	    }
	}
      if(lay==3)
	{
	  if(lad==13)
	    {
	      if(sens==1)
		{
		  if(chip==6||chip==7||chip==8)
		    {
		      return 1;
		    }
		}
	    }
	  if(lad==23)
	    {
	      if(sens==4)
		{
		  if(chip==6||chip==7||chip==8)
		    {
		      return 1;
		    }
		}
	    }
	}
    }
  
  return 0;
}

int  SvxStripFindHotDead::set_Threshold()
{
  std::ifstream thresholdfile;
  thresholdfile.open(thresholdfilename,std::ios::in);
  int ilay,ilad,ircc,isvx,ichl;
  int th;
  if(thresholdfile.is_open())
    {
       while(!thresholdfile.eof())
       {
	 thresholdfile>>ilay>>ilad>>ircc>>isvx>>ichl>>th;
	 thresh[ilay-2][ilad][ircc][isvx][ichl]=th;
       }
    }
  else {
    std::cout<<"Error opening "<<thresholdfilename<<"!  No ADC threshold applied"<<std::endl;
    return 1;
  }

  thresholdfile.close();

  return 0;

}
