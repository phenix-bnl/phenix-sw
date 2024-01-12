#include <iostream>
#include <fstream>

#include <histMpc.h>
#include <QADefs.h>
//  GENERal PHENIX tools

#include <Fun4AllServer.h>
#include <getClass.h>
#include <PHCompositeNode.h>
#include <phool.h>
#include <Fun4AllHistoManager.h>
#include <Fun4AllReturnCodes.h>
#include <fstream>
#include <MpcCalib.h>

//  Data classes I am using in analysis
#include <TriggerHelper.h>
#include <TrigLvl1.h>
#include <PHGlobal.h>
#include <EventHeader.h>
#include <mpcRawContainer.h>
#include <mpcRawContent.h>
#include <MpcMap.h>
#include <BbcOut.h>
#include <Bbc.hh>

//  Root histogram types
#include <TH1.h>
#include <TH2.h>
#include <TFile.h>



Fun4AllHistoManager *HistoManager;

int EventCounter;

TH1 *htdc[576];
TH1 *hlopost[576];
TH1 *hlopre[576];
TH1 *hloadc[576];

TH1 *hloadc_driver[20];

TH1 *h_ntrig;

//MpcNoise *mpcnoise;
MpcCalib *mpccalib;
MpcMap *mpcmap;
TriggerHelper *trighelp;

using namespace std;
using namespace findNode;

int QAMpc::InitRun(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();

  
   //  Get the data I need...
  mpcRawContainer *mpcraw = getClass<mpcRawContainer>(topNode, "MpcRaw");
  EventHeader *evtheader = getClass<EventHeader>(topNode, "EventHeader");
  BbcOut *bbcout = getClass<BbcOut>(topNode, "BbcOut");
  if ( mpcraw==0 || evtheader==0 || bbcout==0 )
    {
      cout << "At least one of the objects are missed for MpcQA" << endl;
      se->unregisterSubsystem(this);
      return 0;
    }
  
  HistoManager = se->getHistoManager(HistoManagerName);
  if (!HistoManager)
    {
      HistoManager = new Fun4AllHistoManager(HistoManagerName);
      se->registerHistoManager(HistoManager);
    }

  trighelp = new TriggerHelper(topNode);
  if ( trighelp==0 )
    {
      cout << "MpcCheck::InitRun, TriggerHelper not found" << endl;
      return ABORTRUN;
    }
  mpccalib = MpcCalib::instance();
  mpcmap = MpcMap::instance();
 
  h_ntrig = new TH1F("h_ntrig","Number of Triggers",10,-0.5,9.5);
  HistoManager->registerHisto("h_ntrig",h_ntrig);
    
  TString name;
  for (int ich=0; ich<576; ich++)
    {
      name = "hlopost_"; name += ich;
      hlopost[ich] = new TH1F(name,name,4096,-0.015,60);
      //hlopost[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      HistoManager->registerHisto(name,hlopost[ich]);

      name = "hlopre_"; name += ich;
      hlopre[ich] = new TH1F(name,name,4096,-0.015,60);
      //hlopre[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      HistoManager->registerHisto(name,hlopre[ich]);
      /*
      name = "hhipost_"; name += ich;
      hhipost[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      HistoManager->registerHisto(name,hhipost[ich]);

      name = "hhipre_"; name += ich;
      hhipre[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      HistoManager->registerHisto(name,hhipre[ich]);
      */
      name = "htdc_"; name += ich;
      htdc[ich] = new TH1F(name,name,4096,-0.015,60);
      //htdc[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      HistoManager->registerHisto(name,htdc[ich]);
      
      name = "hloadc_"; name += ich;
      hloadc[ich] = new TH1F(name,name,4196,-1.5,60);
      //hloadc[ich] = new TH1F(name,name,4196,-100.5,4095.5);
      HistoManager->registerHisto(name,hloadc[ich]);
      /*
      name = "hhiadc_"; name += ich;
      hhiadc[ich] = new TH1F(name,name,4196,-100.5,4095.5);
      HistoManager->registerHisto(name,hhiadc[ich]);
      */
    }
  
    for (int idriver=0; idriver<20; idriver++)
    {
      name = "hloadc_driver_"; name += (idriver);
      hloadc_driver[idriver] = new TH1F(name,name,4196,-1.5,60);
//      hloadc_driver[idriver] = new TH1F(name,name,4196,-100.5,4095.5);
      HistoManager->registerHisto(name,hloadc_driver[idriver]);

      /*name = "hhiadc_driver_"; name += (idriver+1);
      hhiadc_driver[idriver] = new TH1F(name,name,4196,-100.5,4095.5);
      HistoManager->registerHisto(name,hhiadc_driver[idriver]);*/
    }

  // Check against crossing
  //h2_tdc_cross = new TH2F("h2_tdc_cross","tdc vs crossing",120,-0.5,119.5,300,0,3000);
  //HistoManager->registerHisto("h2_tdc_cross", h2_tdc_cross);
  

  
  return 0;
}


int QAMpc::process_event(PHCompositeNode *topNode)
{
  // informational message...
  static int ncalls = 0;
  ncalls++;
  if (ncalls % 1000 == 0 && verbosity)
    {
      cout << "MpcCheck Ncalls = " << ncalls << endl;
    }
  
  //  Get the data I need...
  mpcRawContainer *mpcraw = getClass<mpcRawContainer>(topNode, "MpcRaw");
  EventHeader *evtheader = getClass<EventHeader>(topNode, "EventHeader");
  BbcOut *bbcout = getClass<BbcOut>(topNode, "BbcOut");
  // if (ncalls % 1000 == 0 && verbosity) cout << mpcraw->size() << endl;
 
  if ( mpcraw==0 || evtheader==0 || bbcout==0 )
    {
      cout << "MpcCheck::process_event, mpcraw or phglobal or eventheader not found" << endl;
      cout << "\t" << (unsigned int)mpcraw
           << "\t" << (unsigned int)evtheader
           << "\t" << (unsigned int)bbcout
           << endl;
      return ABORTEVENT;
    }
  
  //AlignmentDisplay->Reset();
  
  const int MBIAS = 0;
  const int ERT4X4B = 3;
  const int MPC4X4A = 1;
  const int MPC4X4B = 2;
  const int ZDCLL1wide = 4;
  int trigger[5] = { 0 };

  TrigLvl1 *trigl1 = trighelp->get_trigLvl1();
  if ( trigl1==0 ) cout << "couldn't find TrigLvl1" << endl;
  //int cross = trigl1->get_lvl1_clock_cross();
  

  trigger[MBIAS] = trighelp->trigScaled("BBCLL1(>0 tubes)") ? 1 : 0;
  trigger[ERT4X4B] = trighelp->trigScaled("ERTLL1_4x4b&BBCLL1") ? 1 : 0;
  trigger[MPC4X4A] = trighelp->trigScaled("MPC_4x4A") ? 1 : 0;  //These have to change with run, run type
  trigger[MPC4X4B] = trighelp->trigScaled("MPC_4x4B") ? 1 : 0;
  trigger[ZDCLL1wide] = trighelp->trigScaled("ZDCLL1wide") ? 1 : 0;

  

  //h_ntrig: 0 = minbias
  //         1 = MPC4X4A
  //         2 = MPC4X4B
  //         3 = MPC4X4A given mbias
  //         4 = MPC4X4B given mbias
  if(trigger[MBIAS] == 1)
    h_ntrig->Fill(MBIAS);
  if(trigger[MPC4X4A] == 1)
    h_ntrig->Fill(MPC4X4A);
  if(trigger[MPC4X4B] == 1)
    h_ntrig->Fill(MPC4X4B);

 
  //cout << "trigger bits -- mbias, zdc: " << trigger[MBIAS] << "\t"
  //   << trigger[ZDCLL1wide] << endl;
  
  if(trigger[MBIAS] == 0) 
     return EVENT_OK;
  
  if(trigger[MPC4X4A] == 1)
    h_ntrig->Fill(3);//MPC4X4A+2);
  if(trigger[MPC4X4B] == 1)
    h_ntrig->Fill(4);//MPC4X4B+2);

   // require bbc&&zdc
  // if ( trigger[MBIAS]==0 || trigger[ZDCLL1wide]==0 ) return EVENT_OK;
  EventCounter++;
  
  //cout << "BBC " << trighelp->getLevel1BitNumber("BBCLL1(>0 tubes)") 
  //     << "ZDC " << trighelp->getLevel1BitNumber("ZDCLL1wide") << endl;
  
  int mpc4x4 = 0;
  if ( trigger[MPC4X4A]==1 || trigger[MPC4X4B]==1 ) mpc4x4 = 1;
  
  //cout << evtheader->get_EvtSequence() << "\t" << mpcraw->size() << endl;
  //cout << trigger[MBIAS] << "\t" << trigger[MPC4X4A] << "\t" << trigger[MPC4X4B] << endl;
  
  float adcgain = 1.0;
  //float mpc_esum = 0.;
  //float mpcn_esum = 0.;
  //float mpcs_esum = 0.;
  
  if (ncalls % 1000 == 0 && verbosity)
    { 
      cout << "MpcRaw size: " << mpcraw->size() << endl;
      cout << "Event #: " << EventCounter << endl;
    }
  int post_amu = mpcraw->get_post_amu();
  int pre_amu = mpcraw->get_pre_amu();
  for (unsigned int ich = 0; ich < mpcraw->size(); ich++)
    {
      mpcRawContent *tower = mpcraw->getTower(ich);
      int fee576_ch = tower->get_ch();
      if( mpcmap->getGridX(fee576_ch) < 0 ) continue;
      
      adcgain = mpccalib->get_adc_gain(ich); //uncomment this once gains are available

      int tdc = tower->get_tdc();
      int lopost = tower->get_lopost();
      int lopre = tower->get_lopre();
      int loadc = lopost - lopre;
      // int hipost = tower->get_hipost();
      //int hipre = tower->get_hipre();
      //int hiadc = hipost - hipre;
      
      float loped = mpccalib->get_ped_lgpost(fee576_ch,post_amu) - mpccalib->get_ped_lgpre(fee576_ch,pre_amu);
      //float hiped = mpccalib->get_ped_hgpost(fee576_ch,post_amu) - mpccalib->get_ped_hgpre(fee576_ch,pre_amu);
      
      hlopost[fee576_ch]->Fill( lopost );
      hlopre[fee576_ch]->Fill( lopre );
      //hhipost[fee576_ch]->Fill( hipost );
      //hhipre[fee576_ch]->Fill( hipre );
      htdc[fee576_ch]->Fill( tdc );

      //h2_tdc_cross->Fill(cross,tdc);
            
      /*
      mpc_esum = mpc_esum + ((lopost - lopre) * adcgain);
      if ( fee576_ch < 288 )	// south mpc
        {
          mpcs_esum += ((lopost - lopre) * adcgain);
        }
      else			// north mpc
        {
          mpcn_esum += ((lopost - lopre) * adcgain);
        }
      */
      hloadc[fee576_ch]->Fill( (loadc-loped)*adcgain );
      //hhiadc[fee576_ch]->Fill( hiadc-hiped);
      int driver = mpcmap->getDriver( fee576_ch );  //returns int between 1,20
      hloadc_driver[driver-1]->Fill( (loadc-loped)*adcgain);
      //hhiadc_driver[driver-1]->Fill( hiadc );
            
   }
    
   // get bbc energy
  //float bbcs_q = bbcout->get_ChargeSum(Bbc::South);
  //float bbcs_nhit = bbcout->get_nPmt(Bbc::South);
  //float bbcn_q = bbcout->get_ChargeSum(Bbc::North);
  //float bbcn_nhit = bbcout->get_nPmt(Bbc::North);
  
  /* if ( trigger[MBIAS]==1 )
    {
      h2s_bbcvsmpc->Fill(mpcs_esum,bbcs_q);
      h2n_bbcvsmpc->Fill(mpcn_esum,bbcn_q);
      //if ( mpc4x4 ) AlignmentDisplay->Fill(mpcesum,bbcs_nhit);
    }
  */
  return EVENT_OK;
}

int QAMpc::End(PHCompositeNode *topNode)
{
  if(OutFileName != "NONE") HistoManager->dumpHistos(OutFileName.c_str());
  if(verbosity) cout << "Ending.....bye" << endl;
  return 0;
}

