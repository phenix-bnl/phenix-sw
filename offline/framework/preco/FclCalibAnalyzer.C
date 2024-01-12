#include "FclOut.h"
#include "PHGlobal.h"
#include "getClass.h"
#include "TrigLvl1.h"
#include "TriggerHelper.h"
#include "FclIndexer.h"
#include "FclRaw.h"
#include "FclCalibAnalyzer.h"

#include "Fun4AllServer.h"

#include "TH2.h"

// Here are my histograms...

using namespace std;

FclCalibAnalyzer::FclCalibAnalyzer(const string &name): SubsysReco(name)
{
  return ;
}

int FclCalibAnalyzer::Init(PHCompositeNode *topNode)
{
   
   Fun4AllServer* se = Fun4AllServer::instance();
   char buff[255];
   for(int iside = 0; iside < 2; iside++)
      for(int ichan = 0; ichan < 90; ichan++)
      {
         int col = ichan/10;
         int row = ichan%10;
         sprintf(buff,"hFclZdc2D_%d_%d_%d",iside,col,row);
         hFcl2D[iside][ichan] = new TH2F(buff,buff,10,0,1000,1000,0,0.1);
         hFcl2D[iside][ichan]->SetBit(TH1::kCanRebin);
	 se->registerHisto(buff,hFcl2D[iside][ichan]);
      }
   hFclZdc[0] = new TH2F("hZdcNFclTotlN","North Zdc vs FCAL",100,-4.0,50.0,100,0.0,150.0);
   se->registerHisto("hZdcNFclTotlN",hFclZdc[0]);
   hFclZdc[1] = new TH2F("hZdcSFclGreyS","South Zdc vs FCAL",160,-10.0,150.0,100,0.0,5000.0);
   se->registerHisto("hZdcSFclGreyS",hFclZdc[1]);   
   hFclBbc = new TH2F("hFclGreySBbcS","FCAL Grey South vs Bbc South",100,0,100,160,-10,150.0);
   se->registerHisto("hFclGreySBbcS",hFclBbc);
   hFclGreyS = new TH1F("hFclGreyS","FCAL Grey South",1100,-10,100.0);
   se->registerHisto("hFclGreyS",hFclGreyS);
   hFclTotlS = new TH1F("hFclTotlS","FCAL Total South",320,-20,300.0);
   se->registerHisto("hFclTotlS",hFclTotlS);
   hFclSChan2D = new TH2F("hFclSChan2D","FCAL Channel South",9,-0.5,8.5,10,-0.5,9.5);
   se->registerHisto("hFclSChan2D",hFclSChan2D);

   return 0;
}

int FclCalibAnalyzer::process_event(PHCompositeNode *topNode)
{
  // these constructs make sure you get the right object
  // it will return NULL if your object is not on the node tree
  FclRaw* fclRS = findNode::getClass<FclRaw>(topNode,"fclRawSouth");
  FclRaw* fclRN = findNode::getClass<FclRaw>(topNode,"fclRawNorth");
  FclOut* fclSouth = findNode::getClass<FclOut>(topNode,"fclOutSouth");
  FclOut* fclNorth = findNode::getClass<FclOut>(topNode,"fclOutNorth");
  PHGlobal* globalO = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
  TrigLvl1* lvl1O = findNode::getClass<TrigLvl1>(topNode,"TrigLvl1");

  FclOut* fclO[2];
  fclO[FCALNORTH] = fclNorth;
  fclO[FCALSOUTH] = fclSouth;

  FclRaw* fclRO[2];
  fclRO[FCALNORTH] = fclRN;
  fclRO[FCALSOUTH] = fclRS;

  if(10<lvl1O->get_lvl1_clock_cross()&&lvl1O->get_lvl1_clock_cross()<36) return 0;
  if(fabs(globalO->getBbcZVertex())>30.0) return 0;
  
  TriggerHelper thelp(topNode);
  if(!thelp.IsEventMinBias()) return 0;

  FclIndexer* indexer = FclIndexer::Instance();

  //  Get the counts from each container...
  for(int ichan = 0; ichan < 90; ichan++)
  {
    int col = ichan/10;
    int row = ichan%10;

    int rawchan = -1;
    //find rawchannel
    for(int rchan = 0; rchan < CHANTOT; rchan++)
    {
      if(
	 (indexer->getRowSouth(rchan)==row)
	 &&(indexer->getColumnSouth(rchan)==col)
	){ rawchan = rchan; break; }
    }
    if(rawchan == -1)
    {
      std::cout<<PHWHERE<<" Channel not found!"<<std::endl;
      return 0;
    }

    hFclSChan2D->Fill(col,row,fclO[1]->getLowGain(row,col));
    if(globalO->getZdcEnergyS()>-100&&globalO->getZdcEnergyS()<1000)
      hFcl2D[1][ichan]->Fill(globalO->getZdcEnergyS(),fclRO[1]->getLowGain(rawchan));
    
  }

  hFclZdc[0]->Fill(globalO->get_FclTotlN(),globalO->getZdcEnergyN());
  hFclZdc[1]->Fill(globalO->get_FclGreyS(),globalO->getZdcEnergyS());
  hFclBbc->Fill(globalO->getBbcMultS(),globalO->get_FclGreyS());
  hFclGreyS->Fill(globalO->get_FclGreyS());
  hFclTotlS->Fill(globalO->get_FclTotlS());

  return 0;
}

