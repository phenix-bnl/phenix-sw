//  GENERal PHENIX tools
//#include "Fun4AllServer.h"
#include <getClass.h>
#include <PHCompositeNode.h>
#include <phool.h>
//#include "Fun4AllHistoManager.h"
//#include <Fun4AllReturnCodes.h>

//  Data classes I am using in analysis
#include <TriggerHelper.h>
#include <TrigLvl1.h>
#include <PHGlobal.h>
#include <EventHeader.h>
#include <VtxOut.h>
#include <mpcRawContainer.h>
#include <mpcRawContent.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <MpcMap.h>
#include <MpcCalib.h>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>

#include <MpcInvSlope.h>

#include <iomanip>
#include <stdint.h>

using namespace std;
using namespace findNode;

MpcInvSlope::MpcInvSlope(const char* outfile) : SubsysReco("MpcInvSlope")
{
  EventCounter = 0;
  OutFileName = "mpcinvslope.root";
}

int MpcInvSlope::InitRun(PHCompositeNode *topNode)
{
  //cout << "In MpcInvSlope::InitRun()" << endl;
  mpcmap = MpcMap::instance();

  TDirectory *orig_directory = gDirectory;

  //  Take the output file name as an argument
  savefile = new TFile(OutFileName.c_str(),"RECREATE");

  TString name, title;
  for (int ich=0; ich<576; ich++)
    {
      int gridx = mpcmap->getGridX(ich);
      int gridy = mpcmap->getGridY(ich);
      if ( gridx<0 ) continue;

      //int arm = mpcmap->getArm(ich);

      name = "hlo_"; name += ich; name += "_"; name += gridx; name += "_"; name += gridy;
      title = "hlo_"; title += ich; title += "_"; title += gridx; title += "_"; title += gridy;
      h_lo[ich] = new TH1F(name,name,4200,-104.5,4095.5);
      h_lo[ich]->SetXTitle("low gain adc");

      name = "hhi_"; name += ich; name += "_"; name += gridx; name += "_"; name += gridy;
      title = "hhi_"; title += ich; title += "_"; title += gridx; title += "_"; title += gridy;
      h_hi[ich] = new TH1F(name,name,4200,-104.5,4095.5);
      h_hi[ich]->SetXTitle("high gain adc");

      name = "hlopt_"; name += ich; name += "_"; name += gridx; name += "_"; name += gridy;
      title = "hlopt_"; title += ich; title += "_"; title += gridx; title += "_"; title += gridy;
      h_lopt[ich] = new TH1F(name,name,4200,-5.0,50.0);
      h_lopt[ich]->SetXTitle("pt, low gain adc");

      name = "hhipt_"; name += ich; name += "_"; name += gridx; name += "_"; name += gridy;
      title = "hhipt_"; title += ich; title += "_"; title += gridx; title += "_"; title += gridy;
      h_hipt[ich] = new TH1F(name,name,4200,-50.0,500.0);
      h_hipt[ich]->SetXTitle("pt, high gain adc");

      name = "he_"; name += ich; name += "_"; name += gridx; name += "_"; name += gridy;
      title = "he_"; title += ich; title += "_"; title += gridx; title += "_"; title += gridy;
      h_e[ich] = new TH1F(name,name,4200,-10.5,100.5);
      h_e[ich]->SetXTitle("Energy (GeV)");

      name = "het_"; name += ich; name += "_"; name += gridx; name += "_"; name += gridy;
      title = "het_"; title += ich; title += "_"; title += gridx; title += "_"; title += gridy;
      h_et[ich] = new TH1F(name,name,4200,-1.0,5.0);
      h_et[ich]->SetXTitle("E_{T} (GeV)");

    }

  hzvtx = new TH1F("hzvtx","z-vertex",400,-200,200);
  hsample = new TH1F("hsample","mpc sample number",160,0,16);
  hzvtxcut = new TH1F("hzvtxcut","z-vertex after cut",400,-200,200);
  hzvtxcut->SetLineColor(4);

  h2_ecent[0] = new TH2F("h2_ecent0","ecent south towers",18,-0.5,17.5,18,-0.5,17.5);
  h2_ecent[1] = new TH2F("h2_ecent1","ecent north towers",18,-0.5,17.5,18,-0.5,17.5);
  h2_e9[0] = new TH2F("h2_e90","e9 south towers",18,-0.5,17.5,18,-0.5,17.5);
  h2_e9[1] = new TH2F("h2_e91","e9 north towers",18,-0.5,17.5,18,-0.5,17.5);

  orig_directory->cd();

  trighelp = new TriggerHelper(topNode);

  mpccalib = MpcCalib::instance();

  return EVENT_OK;
}

int MpcInvSlope::process_event(PHCompositeNode *topNode)
{
  //cout << "In MpcInvSlope::process_event()" << endl;

  // informational message...
  static int ncalls = 0;
  ncalls++;;
  if ( ncalls%10000 == 0 )
    {
      cout << "MpcInvSlope Ncalls = " << ncalls << endl;
    }


  if ( trighelp!=0 )
    {
      //if ( trighelp->IsEventMinBias()==false ) return EVENT_OK;
      if ( trighelp->trigScaled("BBCLL1(>0 tubes) novertex")==false ) return EVENT_OK;
      //if ( trighelp->trigScaled("BBCLL1(noVertexCut)")==false ) return EVENT_OK;
      //if ( trighelp->trigScaled("BBCLL1(>0 tubes) narrowvtx")==false ) return EVENT_OK;
      //cout << "trig 0x" << hex << trighelp->get_trigLvl1()->get_lvl1_trigscaled() << dec << endl;
    }
  else
    {
      cout << "No TrigHelp" << endl;
      return ABORTRUN;
    }

  //  Get the data I need...
  mpcRawContainer *mpcraw = getClass<mpcRawContainer>(topNode, "MpcRaw");
  mpcRawContainer *mpcraw2 = getClass<mpcRawContainer>(topNode, "MpcRaw2");
  mpcTowerContainer *mpctow = getClass<mpcTowerContainer>(topNode, "mpcTowerContainer");
  //mpcClusterContainer *mpcclus = getClass<mpcClusterContainer>(topNode, "mpcClusterContainer");
  VtxOut *vtxout = getClass<VtxOut>(topNode, "VtxOut");
  PHGlobal *global = getClass<PHGlobal>(topNode, "PHGlobal");
  //EventHeader *evtheader = getClass<EventHeader>(topNode, "EventHeader");

  if ( (mpcraw==0&&mpcraw2==0) || mpctow==0 || (global==0&&vtxout==0) )
    {
      cout << "MpcInvSlope::process_event(), MpcRaw, MpcRaw2, mpcTowerContainer or PHGlobal and VtxOut not found" << endl;
      cout << "\t" << (uintptr_t)mpcraw << endl;
      cout << "\t" << (uintptr_t)mpcraw2 << endl;
      cout << "\t" << (uintptr_t)mpctow << endl;
      cout << "\t" << (uintptr_t)global << endl;
      cout << "\t" << (uintptr_t)vtxout << endl;
      //cout << "\t" << (uintptr_t)mpcclus << endl;
      return EVENT_OK;
    }

  float zvtx = -999.;
  if ( global ) zvtx = global->getBbcZVertex();
  else if ( vtxout ) zvtx = vtxout->get_ZVertex();
  hzvtx->Fill( zvtx );

  if ( fabs(zvtx) > 50. )
    {
      //cout << "Skipping, vtx = " << zvtx << endl;
      return EVENT_OK;
    }

  hzvtxcut->Fill( zvtx );

  Float_t maxadc = -1.;

  // fill 2d energy grid
  h2_ecent[0]->Reset();
  h2_ecent[1]->Reset();

  // New electronics
  for (unsigned int itow = 0; mpcraw2!=0 && itow < mpcraw2->size(); itow++)
    {
      mpcRawContent *tower = mpcraw2->getTower( itow );

      Short_t fee576_ch = tower->get_ch();

      if ( mpcmap->isCrystal( fee576_ch ) != 1 ) continue;	// skip if not a crystal

      int arm = mpcmap->getArm( fee576_ch );
      int ix  = mpcmap->getGridX( fee576_ch );
      int iy  = mpcmap->getGridY( fee576_ch );

      float e  = tower->get_adc();
      //Int_t qual  = tower->get_quality();
      //cout << "AAA " << fee576_ch << "\t" << e << "\t" <<  tdc << "\t" << qual << endl;

      float sample = tower->get_sample();
      if ( sample<4 || sample>9 ) continue;

      if ( e>maxadc ) maxadc = e;

      //if ( e<2.0 ) continue;

      h2_ecent[arm]->Fill( ix+1, iy+1, e );
    }
  // Old electronics
  //cout << "towers " << mpctow->size() << endl;
  for (unsigned int itow = 0; mpctow!=0 && (itow<mpctow->size()); itow++)
    {
      mpcTowerContent *tower = mpctow->getTower( itow );

      Short_t fee576_ch = tower->get_ch();

      if ( mpcmap->isCrystal( fee576_ch ) != 1 ) continue;	// skip if not a crystal

      int arm = mpcmap->getArm( fee576_ch );
      int ix  = mpcmap->getGridX( fee576_ch );
      int iy  = mpcmap->getGridY( fee576_ch );

      float e  = tower->get_energy( fee576_ch );
      //short tdc  = tower->get_tdc();

      if ( e>maxadc ) maxadc = e;

      //if ( e<2.0 ) continue;

      h2_ecent[arm]->Fill( ix+1, iy+1, e );
    }

  // Now fill 3x3 sum
  h2_e9[0]->Reset();
  h2_e9[1]->Reset();
  for (int iarm=0; iarm<2; iarm++)
    {
      for (int ix=0; ix<18; ix++)
        {
          for (int iy=0; iy<18; iy++)
            {
              float e9 = 0.;
              for (int ixsum=ix; ixsum<=ix+2; ixsum++)
                {
                  if ( ixsum<=0||ixsum>=19 ) continue;

                  for (int iysum=iy; iysum<=iy+2; iysum++)
                    {
                      if ( iysum<=0||iysum>=19 ) continue;

                      e9 += h2_ecent[iarm]->GetBinContent(ixsum,iysum);
                      //cout << ix << "\t" << iy << "\t" << ixsum << "\t" << iysum << "\t"
                      //     << h2_ecent[iarm]->GetBinContent(ixsum,iysum) << "\t" << e9 << endl;
                    }
                }
              h2_e9[iarm]->Fill( ix, iy, e9 );
            }
        }
    }

  // New Electronics
  for (unsigned int ich = 0; mpcraw2!=0 && (ich<mpcraw2->size()); ich++)
    {
      mpcRawContent *tower = mpcraw2->getTower(ich);

      int fee576_ch = tower->get_ch();
      if ( mpcmap->getGridX(fee576_ch) < 0 ) continue;	// skip empty channels

      float adc = tower->get_adc();
      h_hi[fee576_ch]->Fill( adc );

      // Make sure this isn't a single tower
      if ( IsSingleTower( fee576_ch )==1 ) continue;

      float sample = tower->get_sample();
      //float time = tower->get_time();

      if ( adc>10.) hsample->Fill( sample );

      if ( sample<4. || sample>9. ) continue;
      h_lo[fee576_ch]->Fill( adc );


      // now precompute 
      float x = mpcmap->getX(fee576_ch);
      float y = mpcmap->getY(fee576_ch);
      float rsqr = x*x + y*y;
      float r = sqrt( rsqr );
      float z = mpcmap->getZ(fee576_ch) - zvtx;
      float sintheta = float(r/sqrt(rsqr + z*z));

      h_lopt[fee576_ch]->Fill( adc*sintheta );
   }


  // Old Electronics
  int post_amu = 0;
  int pre_amu = 0;
  if ( mpcraw!=0 )
    {
      post_amu = mpcraw->get_post_amu();
      pre_amu = mpcraw->get_pre_amu();
    }

  for (unsigned int ich = 0; mpcraw!=0 && ich<mpcraw->size(); ich++)
    {
      mpcRawContent *tower = mpcraw->getTower(ich);

      int fee576_ch = tower->get_ch();
      if ( mpcmap->getGridX(fee576_ch) < 0 ) continue;	// skip empty channels

      // Make sure this isn't a single tower
      if ( IsSingleTower( fee576_ch )==1 ) continue;

      //int tdc = tower->get_tdc();
      float lopost = tower->get_lopost() - mpccalib->get_ped_lgpost(fee576_ch,post_amu);
      float lopre = tower->get_lopre() - mpccalib->get_ped_lgpre(fee576_ch,pre_amu);
      float hipost = tower->get_hipost() - mpccalib->get_ped_hgpost(fee576_ch,post_amu);
      float hipre = tower->get_hipre() - mpccalib->get_ped_hgpre(fee576_ch,pre_amu);

      float loadc = lopost - lopre;
      float hiadc = hipost - hipre;

      //cout << "loadc " << fee576_ch << "\t" << loadc << endl;
      h_lo[fee576_ch]->Fill( loadc );
      h_hi[fee576_ch]->Fill( hiadc );

      // now precompute 
      float x = mpcmap->getX(fee576_ch);
      float y = mpcmap->getY(fee576_ch);
      float rsqr = x*x + y*y;
      float r = sqrt( rsqr );
      float z = mpcmap->getZ(fee576_ch) - zvtx;
      float sintheta = float(r/sqrt(rsqr + z*z));

      h_lopt[fee576_ch]->Fill( loadc*sintheta );
      h_hipt[fee576_ch]->Fill( hiadc*sintheta );
   }

  // fill energy histograms
  for (unsigned int ich = 0; mpctow!=0 && ich<mpctow->size(); ich++)
    {
      mpcTowerContent *tower = mpctow->getTower(ich);
      Short_t fee576_ch = tower->get_ch();
      if ( mpcmap->getGridX(fee576_ch) < 0 ) continue;	// skip empty channels

      // Make sure this isn't a single tower
      if ( IsSingleTower( fee576_ch )==1 ) continue;

      // now precompute 
      double x = mpcmap->getX(fee576_ch);
      double y = mpcmap->getY(fee576_ch);
      double rsqr = x*x + y*y;
      double r = sqrt( rsqr );
      double z = mpcmap->getZ(fee576_ch) - zvtx;
      float sintheta = r/sqrt(rsqr + z*z);

      Float_t e = tower->get_energy();
      h_e[fee576_ch]->Fill( e );
      h_et[fee576_ch]->Fill( e*sintheta );

    }

/*
  //cout << "max adc " << maxadc << endl;
  if ( maxadc>400. )
    {
      //cout << "max adc2 " << maxadc << endl;
      static TCanvas *ac = new TCanvas("acinv","acinv",800,400);
      ac->Divide(1,2);
      ac->cd(1);
      h2_ecent[0]->Draw("colz");
      ac->cd(2);
      h2_ecent[1]->Draw("colz");
      gPad->Modified();
      gPad->Update();
      string junk;
      cin >> junk;
    }
*/

  return EVENT_OK;
}

int MpcInvSlope::End(PHCompositeNode *topNode)
{
  cout << "In MpcInvSlope::End" << endl;
  if ( savefile )
    {
      savefile->Write();
      savefile->Close();
    }
  return EVENT_OK;
}

// Single Tower Background Cut
int MpcInvSlope::IsSingleTower(const int fee576_ch)
{
  int arm = mpcmap->getArm( fee576_ch );
  int ix  = mpcmap->getGridX( fee576_ch );
  int iy  = mpcmap->getGridY( fee576_ch );
  double e9 = h2_e9[arm]->GetBinContent(ix+1,iy+1);

  if ( e9 <= 0. ) return 1;

  double ecent = h2_ecent[arm]->GetBinContent(ix+1,iy+1);
  double e9ratio = ecent/e9;

  //cout << "issgl2 " << fee576_ch << "\t" << e9ratio << endl;
  if ( e9ratio > 0.95 ) return 1;

  return 0;
}

