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
#include <mpcClusterContainer.h>
#include <mpcClusterContent.h>
#include <MpcMap.h>
#include <MpcCalib.h>

#include <TFile.h>
#include <TTree.h>
#include <TH1.h>
#include <TH2.h>

#include <MpcTime.h>

#include <iomanip>

using namespace std;
using namespace findNode;

MpcTime::MpcTime(const char* outfile) : SubsysReco("MpcTime")
{
  EventCounter = 0;
  OutFileName = "mpctimecalib.root";
}

int MpcTime::InitRun(PHCompositeNode *topNode)
{
  mpcmap = MpcMap::instance();
  mpccalib = MpcCalib::instance();

//cout << "In MpcTime::InitRun()" << endl;
  TDirectory *orig_directory = gDirectory;

  //  Take the output file name as an argument
  savefile = new TFile(OutFileName.c_str(),"RECREATE");

  TString name, title;
/*
  for (int ich=0; ich<576; ich++)
    {
      int gridx = mpcmap->getGridX(ich);
      int gridy = mpcmap->getGridY(ich);
      if ( gridx<0 ) continue;

      //int arm = ( ich<288 ) ? 0 : 1 ;
      int arm = mpcmap->getArm(ich);
    }
*/

  hzvtx = new TH1F("hzvtx","z-vertex",400,-200,200);
  hzvtxcut = new TH1F("hzvtxcut","z-vertex after cut",400,-200,200);
  hzvtxcut->SetLineColor(4);

  h2_ecent[0] = new TH2F("h2_ecent0","ecent south towers",18,-0.5,17.5,18,-0.5,17.5);
  h2_ecent[1] = new TH2F("h2_ecent1","ecent north towers",18,-0.5,17.5,18,-0.5,17.5);
  h2_e9[0] = new TH2F("h2_e90","e9 south towers",18,-0.5,17.5,18,-0.5,17.5);
  h2_e9[1] = new TH2F("h2_e91","e9 north towers",18,-0.5,17.5,18,-0.5,17.5);


  tree = new TTree("t","Mpc Time Calib Tree");
  tree->Branch("ch",&f_ch,"ch/I");
  tree->Branch("lo",&f_lo,"lo/F");
  tree->Branch("hi",&f_hi,"hi/F");
  tree->Branch("e",&f_e,"e/F");
  tree->Branch("tdc",&f_tdc,"tdc/F");
  tree->Branch("tdc2ns",&f_tdc2ns,"tdc2ns/F");
  tree->Branch("t0offset",&f_t0offset,"t0offset/F");
  tree->Branch("bbcz",&f_bbcz,"bbcz/F");
  tree->Branch("bbct0",&f_bbct0,"bbct0/F");
  tree->Branch("bbctn",&f_bbctn,"bbctn/F");
  tree->Branch("bbcts",&f_bbcts,"bbcts/F");

  orig_directory->cd();

  trighelp = new TriggerHelper(topNode);

  return EVENT_OK;
}

int MpcTime::process_event(PHCompositeNode *topNode)
{
//cout << "In MpcTime::process_event()" << endl;

  // informational message...
  static int ncalls = 0;
  ncalls++;;
  if (ncalls % 1 == 0 && verbosity)
    {
      cout << "MpcTime Ncalls = " << ncalls << endl;
    }

  //if ( trighelp->IsEventMinBias()==false ) return EVENT_OK;
  if ( trighelp->trigScaled("BBCLL1(>0 tubes) novertex")==false ) return EVENT_OK;
  //cout << "trig 0x" << hex << trighelp->get_trigLvl1()->get_lvl1_trigscaled() << dec << endl;

  //  Get the data I need...
  mpcRawContainer *mpcraw = getClass<mpcRawContainer>(topNode, "MpcRaw");
  mpcTowerContainer *mpctow = getClass<mpcTowerContainer>(topNode, "mpcTowerContainer");
  mpcClusterContainer *mpcclus = getClass<mpcClusterContainer>(topNode, "mpcClusterContainer");
  VtxOut *vtxout = getClass<VtxOut>(topNode, "VtxOut");
  PHGlobal *global = getClass<PHGlobal>(topNode, "PHGlobal");
  //EventHeader *evtheader = getClass<EventHeader>(topNode, "EventHeader");

  //if ( mpctow==0 )
  //if ( mpcraw==0 || mpctow==0 || vtxout==0 )
  if ( mpcraw==0 || mpctow==0 || mpcclus==0 || global==0 )
    {
      cout << "MpcTime::process_event(), MpcRaw or mpcTowerContainer or MpcClusterContainer not found" << endl;
      cout << "\t" << (unsigned int)mpcraw << endl;
      cout << "\t" << (unsigned int)mpctow << endl;
      cout << "\t" << (unsigned int)mpcclus << endl;
      cout << "\t" << (unsigned int)global << endl;
      return EVENT_OK;
    }

  f_bbcz = -9999.;
  if ( global )
    {
      f_bbcz = global->getBbcZVertex();
      f_bbct0 = global->getBbcTimeZero();
      f_bbctn = global->getBbcTimeN();
      f_bbcts = global->getBbcTimeS();
    }
  else if ( vtxout ) f_bbcz = vtxout->get_ZVertex();
  hzvtx->Fill( f_bbcz );

  if ( fabs(f_bbcz) > 100. ) return EVENT_OK;

  hzvtxcut->Fill( f_bbcz );

  // fill 2d energy grid
  h2_ecent[0]->Reset();
  h2_ecent[1]->Reset();
  for (unsigned int itow = 0; itow < mpctow->size(); itow++)
    {
      mpcTowerContent *tower = mpctow->getTower( itow );
      Short_t fee576_ch = tower->get_ch();
      if ( mpcmap->isCrystal( fee576_ch ) != 1 ) continue;	// skip if not a crystal

      int arm = mpcmap->getArm( fee576_ch );
      int ix  = mpcmap->getGridX( fee576_ch );
      int iy  = mpcmap->getGridY( fee576_ch );

      float e  = tower->get_energy( fee576_ch );

      h2_ecent[arm]->Fill( ix+1, iy+1, e );
    }

  // Now fill 3x3 sum, for the veto
  h2_e9[0]->Reset();
  h2_e9[1]->Reset();
  for (int iarm = 0; iarm < 2; iarm++)
    {
      for (int ix = 0; ix < 18; ix++)
        {
          for (int iy = 0; iy < 18; iy++)
            {
              float e9 = 0.;
              for (int ixsum=ix; ixsum<=ix+2; ixsum++)
                {
                  for (int iysum=iy; iysum<=iy+2; iysum++)
                    {
                      e9 += h2_ecent[iarm]->GetBinContent(ixsum,iysum);
                      //cout << ixsum << "\t" << iysum << "\t"
                      //     << h2_ecent[iarm]->GetBinContent(ixsum,iysum) << "\t" << e9 << endl;
                    }
                }
              h2_e9[iarm]->Fill( ix+1, iy+1, e9 );
            }
        }
    }

  int post_amu = mpcraw->get_post_amu();
  int pre_amu = mpcraw->get_pre_amu();

  // fill energy histograms
  for (unsigned int iclus = 0; iclus < mpcclus->size(); iclus++)
    {
      mpcClusterContent *clus = mpcclus->getCluster( iclus );
      int ixpos = clus->ixpos();
      int iypos = clus->iypos();
      int arm = clus->arm();
      Int_t fee576_ch = mpcmap->getFeeCh(ixpos,iypos,arm);

      // Make sure this isn't a single tower
      if ( IsSingleTower( fee576_ch ) ) continue;

      int tower_index = mpctow->findTower(fee576_ch);
      mpcTowerContent *tow = mpctow->getTower( tower_index );
      f_ch = tow->get_ch();
      if ( f_ch != fee576_ch )
        {
          cout << "Error XXX " << endl;
        }
      f_e = tow->get_energy();

      f_t0offset = 0.;
      f_tdc2ns = mpccalib->get_tdc_leastcount(f_ch);

      int foundtower = 0;
      for (unsigned int iraw=0; iraw<mpcraw->size(); iraw++)
        {
          mpcRawContent *raw = mpcraw->getTower(iraw);
          if ( raw->get_ch() != f_ch ) continue;

          Float_t loped = mpccalib->get_ped_lgpost(f_ch,post_amu) - mpccalib->get_ped_lgpre(f_ch,pre_amu);
          Float_t hiped = mpccalib->get_ped_hgpost(f_ch,post_amu) - mpccalib->get_ped_hgpre(f_ch,pre_amu);
          f_lo = raw->get_lopost() - raw->get_lopre() - loped;
          f_hi = raw->get_hipost() - raw->get_hipre() - hiped;
          f_tdc = raw->get_tdc();

          if ( f_tdc != clus->rawtdc() )
            {
              cout << "ERROR, f_tdc!=cluster->tdc " << f_tdc << "\t" << clus->rawtdc() << endl;
            }

          foundtower = 1;
          break;
        }

      if ( foundtower == 0 )
        {
          cout << "ERROR, did not find raw tower" << endl;
          f_lo = 0;
          f_hi = 0;
          f_tdc = 9999;
        }

      tree->Fill();

    }

  return EVENT_OK;
}

int MpcTime::End(PHCompositeNode *topNode)
{
//cout << "In MpcTime::End" << endl;
  if ( savefile )
    {
      savefile->Write();
      savefile->Close();
    }
  return EVENT_OK;
}

// Single Tower Background Cut
int MpcTime::IsSingleTower(const int fee576_ch)
{
  int arm = mpcmap->getArm( fee576_ch );
  int ix  = mpcmap->getGridX( fee576_ch );
  int iy  = mpcmap->getGridY( fee576_ch );
  double e9 = h2_e9[arm]->GetBinContent(ix+1,iy+1);
  if ( e9 <= 0. ) return 1;

  double ecent = h2_ecent[arm]->GetBinContent(ix+1,iy+1);
  double e9ratio = ecent/e9;

  if ( e9ratio < 0.95 ) return 1;

  return 0;
}

