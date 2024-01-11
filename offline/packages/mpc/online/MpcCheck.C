#include <MpcCheck.h>

//  GENERal PHENIX tools
#include <Fun4AllServer.h>
#include <getClass.h>
#include <PHCompositeNode.h>
#include <phool.h>
//#include <Fun4AllHistoManager.h>
#include <Fun4AllReturnCodes.h>

//  Data classes I am using in analysis
//#include <PHCentralTrack.h>
//#include <PreviousEvent.h>
#include <TriggerHelper.h>
#include <TrigLvl1.h>
#include <PHGlobal.h>
#include <EventHeader.h>
#include <mpcRawContainer.h>
#include <mpcRawContent.h>
#include <mpcClusterContainer.h>
#include <mpcClusterContent.h>
#include <MpcMap.h>
#include <MpcCalib.h>
#include <BbcOut.h>
#include <Bbc.hh>

//  Root histogram types
#include <TH1.h>
#include <TH2.h>
#include <TDirectory.h>
#include <TFile.h>

using namespace std;
using namespace findNode;

MpcCheck::MpcCheck(const char* outfile) : SubsysReco("MpcCheck")
{
  //  Take the output file name as an argument
  OutFileName = outfile;
  //HistoManager = new Fun4AllHistoManager("MpcCheck");

  EventCounter = 0;
  mpcmap = 0;
  mpccalib = 0;

  return ;
}

int MpcCheck::InitRun(PHCompositeNode *topNode)
{
  trighelp = new TriggerHelper(topNode);
  if ( trighelp==0 )
    {
      cout << "MpcCheck::InitRun, TriggerHelper not found" << endl;
      return ABORTRUN;
    }

  //if (mpcmap!=0 ) mpcmap->Download_Maps(topNode);
  mpcmap = getClass<MpcMap>(topNode,"MpcMap");
  mpccalib = getClass<MpcCalib>(topNode,"MpcCalib");

  TDirectory *orig_directory = gDirectory;

  savefile = new TFile("mpccheck.root","RECREATE");

  h2s_bbcvsmpc = new TH2F("h2s_bbcvsmpc","MPC vs. BBC Energy, South",4000,0,4000,4000,0,80000);
  h2s_bbcvsmpc->SetXTitle("MPC");
  h2s_bbcvsmpc->SetYTitle("BBC");
  h2s_bbcvsmpc->SetMarkerColor(2);
  h2s_bbcvsmpc->SetMarkerStyle(20);
  h2n_bbcvsmpc = new TH2F("h2n_bbcvsmpc","MPC vs. BBC Energy, North",4000,0,4000,4000,0,80000);
  h2n_bbcvsmpc->SetXTitle("MPC");
  h2n_bbcvsmpc->SetYTitle("BBC");
  h2n_bbcvsmpc->SetMarkerColor(2);
  h2n_bbcvsmpc->SetMarkerStyle(20);

  //HistoManager->registerHisto("h2s_bbcvsmpc", h2s_bbcvsmpc);
  //HistoManager->registerHisto("h2n_bbcvsmpc", h2n_bbcvsmpc);

  h2_amu = new TH2F("h2_amu","pre vs post amu",64,-0.5,63.5,64,-0.5,63.5);
  h2_amu->SetXTitle("post amu");
  h2_amu->SetYTitle("pre amu");
  //HistoManager->registerHisto("h2_amu", h2_amu);
 
  h_ntrig = new TH1F("h_ntrig","number of triggers",6,-0.5,5.5);
  h_ntrig->SetXTitle("trigger");
  //HistoManager->registerHisto("h_ntrig", h_ntrig);

  TString name;
  for (int ich=0; ich<576; ich++)
    {
      if ( mpcmap->getGridX(ich)<0 ) continue;

      name = "hlopost_"; name += ich;
      hlopost[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      //HistoManager->registerHisto(name,hlopost[ich]);

      name = "hlopre_"; name += ich;
      hlopre[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      //HistoManager->registerHisto(name,hlopre[ich]);

      name = "htdc_"; name += ich;
      htdc[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      //HistoManager->registerHisto(name,htdc[ich]);

      name = "hadc_"; name += ich;
      hadc[ich] = new TH1F(name,name,4196,-100.5,4095.5);
      //HistoManager->registerHisto(name,hadc[ich]);

      name = "hadc_amu_"; name += ich;
      hadc_amu[ich] = new TH2F(name,name,2196,-100.5,2095.5,64,-0.5,63.5);
      //HistoManager->registerHisto(name,hadc_amu[ich]);

      // hi gain
      name = "hhipost_"; name += ich;
      hhipost[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      //HistoManager->registerHisto(name,hhipost[ich]);

      name = "hhipre_"; name += ich;
      hhipre[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      //HistoManager->registerHisto(name,hhipre[ich]);

      name = "hhi_"; name += ich;
      hhi[ich] = new TH1F(name,name,4196,-100.5,4095.5);
      //HistoManager->registerHisto(name,hhi[ich]);

      // which bits are on or off
      name = "hlobits_"; name += ich;
      hlobits[ich] = new TH1F(name,name,12,-0.5,11.5);
      //HistoManager->registerHisto(name,hlobits[ich]);

      name = "hhibits_"; name += ich;
      hhibits[ich] = new TH1F(name,name,12,-0.5,11.5);
      //HistoManager->registerHisto(name,hhibits[ich]);

      if ( mpcmap->isCrystal(ich)==0 ) continue;

      int arm = mpcmap->getArm(ich);
      int xpos = mpcmap->getGridX(ich);
      int ypos = mpcmap->getGridY(ich);

      name = "hx_"; name += ich; name += "_"; name += xpos; name += "_"; name += ypos;
      hx[arm][xpos][ypos] = new TH1F(name,name,600,-6.0,6.0);

      name = "hy_"; name += ich; name += "_"; name += xpos; name += "_"; name += ypos;
      hy[arm][xpos][ypos] = new TH1F(name,name,600,-6.0,6.0);
    }

  for (int idriver=0; idriver<20; idriver++)
    {
      name = "hadc_driver"; name += (idriver+1);
      hadc_driver[idriver] = new TH1F(name,name,4196,-100.5,4095.5);
      //HistoManager->registerHisto(name,hadc_driver[idriver]);
    }

  // Check against crossing
  h2_tdc_cross = new TH2F("h2_tdc_cross","tdc vs crossing",120,-0.5,119.5,300,0,3000);
  //HistoManager->registerHisto("h2_tdc_cross", h2_tdc_cross);
 
  orig_directory->cd();

  return EVENT_OK;
}

int MpcCheck::Init(PHCompositeNode *topNode)
{
  //mpcmap = new MpcMap(topNode,0);


  return EVENT_OK;
}

int MpcCheck::process_event(PHCompositeNode *topNode)
{
  // informational message...
  static int ncalls = 0;
  ncalls++;;
  if (ncalls % 1000 == 0 && verbosity)
    {
      cout << "MpcCheck Ncalls = " << ncalls << endl;
    }

  //  Get the data I need...
  mpcRawContainer *mpcraw = getClass<mpcRawContainer>(topNode, "MpcRaw");
  mpcClusterContainer *mpcclus = getClass<mpcClusterContainer>(topNode, "mpcClusterContainer");
  PHGlobal *global = getClass<PHGlobal>(topNode, "PHGlobal");
  EventHeader *evtheader = getClass<EventHeader>(topNode, "EventHeader");
  //BbcOut *bbcout = getClass<BbcOut>(topNode, "BbcOut");

  if ( mpcraw==0 || global==0 || evtheader==0 )
  //if ( mpcraw==0 || global==0 || bbcout==0 )
    {
      cout << "MpcCheck::process_event, mpcraw or phglobal or eventheader not found" << endl;
      cout << "\t" << (unsigned int)mpcraw
           << "\t" << (unsigned int)global
           << "\t" << (unsigned int)evtheader
           << endl;
      return ABORTEVENT;
    }

  // 
  //AlignmentDisplay->Reset();
  const int NTRIG = 5;

  const int MBIAS = 0;
  const int ERT4X4B = 1;
  const int MPC4X4A = 2;
  const int MPC2X2 = 3;
  const int ZDCLL1wide = 4;
  int trigger[5] = { 0 };


  TrigLvl1 *trigl1 = trighelp->get_trigLvl1();
  if ( trigl1==0 ) cout << "couldn't find TrigLvl1" << endl;
  int cross = trigl1->get_lvl1_clock_cross();

  /*
  // TriggerHelper doesn't know the MPC bits yet...
  unsigned int scaledtrig = trigl1->get_lvl1_trigscaled();
  if ( (scaledtrig&0x02000000U) != 0 ) trigger[MPC4X4A] = 1;
  if ( (scaledtrig&0x04000000U) != 0 ) trigger[MPC2X2] = 1;
  */

  trigger[MBIAS] = trighelp->trigScaled("BBCLL1(>0 tubes) novertex") ? 1 : 0;
  trigger[ERT4X4B] = trighelp->trigScaled("ERTLL1_4x4b&BBCLL1") ? 1 : 0;
  trigger[MPC4X4A] = trighelp->trigScaled("MPC4x4a") ? 1 : 0;
  trigger[MPC2X2] = trighelp->trigScaled("MPC4x4b") ? 1 : 0;
  trigger[ZDCLL1wide] = trighelp->trigScaled("ZDCLL1wide") ? 1 : 0;

  for (int itrig=0; itrig<NTRIG; itrig++)
    {
      h_ntrig->Fill(0);
      if ( trigger[itrig] != 0 ) h_ntrig->Fill( itrig+1 );
    }

  // require bbc&&zdc
  //if ( trigger[MBIAS]==0 || trigger[ZDCLL1wide]==0 ) return EVENT_OK;
  if ( trigger[MBIAS]==0 ) return EVENT_OK;
  ++EventCounter;

  int mpc4x4 = 0;
  if ( trigger[MPC4X4A]==1 || trigger[MPC2X2]==1 ) mpc4x4 = 1;

  //cout << evtheader->get_EvtSequence() << "\t" << mpcraw->size() << endl;
  //cout << trigger[MBIAS] << "\t" << trigger[MPC4X4A] << "\t" << trigger[MPC2X2] << endl;

  const float adcgain = 1.0;
  float mpc_esum = 0.;
  float mpcn_esum = 0.;
  float mpcs_esum = 0.;

  Short_t post_amu = mpcraw->get_post_amu();
  Short_t pre_amu  = mpcraw->get_pre_amu();
  Short_t tdc_amu  = mpcraw->get_tdc_amu();
  Short_t post_amu_ungray = mpcraw->get_ungray_post_amu();
  Short_t pre_amu_ungray  = mpcraw->get_ungray_pre_amu();
  Short_t tdc_amu_ungray  = mpcraw->get_ungray_tdc_amu();
  h2_amu->Fill(post_amu_ungray,pre_amu_ungray);
  //cout << EventCounter << "\t" << post_amu << "\t" << pre_amu << "\t" << post_amu_ungray << "\t" << pre_amu_ungray << endl;

  for (unsigned int ich = 0; ich < mpcraw->size(); ich++)
    {
      mpcRawContent *tower = mpcraw->getTower(ich);

      int fee576_ch = tower->get_ch();
      if ( mpcmap->getGridX(fee576_ch) < 0 ) continue;	// skip empty channels

      int tdc = tower->get_tdc();
      int hipost = tower->get_hipost();
      int hipre = tower->get_hipre();
      int lopost = tower->get_lopost();
      int lopre = tower->get_lopre();
      int loadc = lopost - lopre;
      int hiadc = hipost - hipre;

      hlopost[fee576_ch]->Fill( lopost );
      hlopre[fee576_ch]->Fill( lopre );
      hhipost[fee576_ch]->Fill( hipost );
      hhipre[fee576_ch]->Fill( hipre );
      htdc[fee576_ch]->Fill( tdc );

      h2_tdc_cross->Fill(cross,tdc);

      float loped = mpccalib->get_ped_lgpost(fee576_ch,post_amu) - mpccalib->get_ped_lgpre(fee576_ch,pre_amu);
      mpc_esum = mpc_esum + ((lopost - lopre - loped) * adcgain);

      if ( fee576_ch < 288 )	// south mpc
        {
          mpcs_esum += ((lopost - lopre - loped) * adcgain);
        }
      else			// north mpc
        {
          mpcn_esum += ((lopost - lopre - loped) * adcgain);
        }

      hhi[fee576_ch]->Fill( hiadc );
      hadc_amu[fee576_ch]->Fill( loadc, post_amu );
      hadc[fee576_ch]->Fill( lopost - lopre - loped );

      int driver = mpcmap->getDriver( fee576_ch );

      // Check that every bit has a signal
      int lopost_temp = lopost;
      int lopre_temp = lopre;
      int hipost_temp = hipost;
      int hipre_temp = hipre;
      for ( int ibit=0; ibit<12; ibit++ )
        {
          int mask = (0x1 << ibit);
          if ( (lopost_temp&mask) != 0 ) hlobits[fee576_ch]->Fill(ibit);
          if ( (lopre_temp&mask)  != 0 ) hlobits[fee576_ch]->Fill(ibit);
          if ( (hipost_temp&mask) != 0 ) hhibits[fee576_ch]->Fill(ibit);
          if ( (hipre_temp&mask)  != 0 ) hhibits[fee576_ch]->Fill(ibit);
        }
      hadc_driver[driver-1]->Fill( lopost - lopre - loped );

      //cout << fee576_ch << "\t" << tdc << "\t" << lopost << "\t" << lopre << endl;
      //if ( energy < 30 ) continue;  // skip empty channels
   }

   // get bbc energy
/*
  float bbcs_q = bbcout->get_ChargeSum(Bbc::South);
  float bbcs_nhit = bbcout->get_nPmt(Bbc::South);
  float bbcn_q = bbcout->get_ChargeSum(Bbc::North);
  float bbcn_nhit = bbcout->get_nPmt(Bbc::North);

  if ( trigger[MBIAS]==1 )
    {
      h2s_bbcvsmpc->Fill(mpcs_esum,bbcs_q);
      h2n_bbcvsmpc->Fill(mpcn_esum,bbcn_q);
      //if ( mpc4x4 ) AlignmentDisplay->Fill(mpcesum,bbcs_nhit);

      // check that no channels are swapped
      for (int iclus=0; iclus<mpcclus->size(); iclus++)
        {
          mpcClusterContent *clus = mpcclus->getCluster(iclus);
          float energy = clus->ecore();
          if ( energy>4.0 && energy<20. )
            {
              int xpos = clus->ixpos();
              int ypos = clus->iypos();
              int arm = clus->arm();
              int ch = mpcmap->getFeeCh(xpos,ypos,arm);
              float x = clus->x();
              float y = clus->y();
              float xcent = mpcmap->getX(ch);
              float ycent = mpcmap->getY(ch);
              if ( hx[arm][xpos][ypos]==0 )
                {
                  cout << "ERROR, hx=0, " << arm << "\t" << xpos << "\t" << ypos << endl;
                  continue;
                }
              hx[arm][xpos][ypos]->Fill(x-xcent);
              hy[arm][xpos][ypos]->Fill(y-ycent);
            }
        }
    }
*/
 
  return EVENT_OK;
}

int MpcCheck::End(PHCompositeNode *topNode)
{
  for (int idriver=0; idriver<20; idriver++)
    {
      hadc_driver[idriver]->Sumw2();
      //hadc_driver[idriver]->Scale(1.0/EventCounter);
    }

  //AlignmentDisplay->Draw();
  //HistoManager->dumpHistos(OutFileName.c_str());
  if ( savefile )
    {
      savefile->Write();
      savefile->Close();
    } 

  return EVENT_OK;
}

