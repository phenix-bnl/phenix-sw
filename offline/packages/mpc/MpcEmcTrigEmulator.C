#include <cassert>

//  GENERAL PHENIX tools
#include <Fun4AllServer.h>
#include <getClass.h>
#include <PHCompositeNode.h>
#include <phool.h>
//#include <Fun4AllHistoManager.h>
#include <Fun4AllReturnCodes.h>
#include <recoConsts.h>

//  Data classes I am using in analysis
#include <TriggerHelper.h>
#include <TrigLvl1.h>
#include <PHGlobal.h>
#include <VtxOut.h>
#include <BbcOut.h>
#include <SyncObject.h>
#include <EventHeader.h>
//#include <mpcClusterContainer.h>
//#include <mpcClusterContent.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
//#include <mpcRawContainer.h>
//#include <mpcRawContent.h>
#include <MpcMap.h>
#include <MpcCalib.h>

//  Root histogram types
#include <TDirectory.h>
#include <TFile.h>
#include <TH1.h>
#include <TH2.h>

#include <MpcEmcTrigEmulator.h>

using namespace std;
using namespace findNode;

MpcEmcTrigEmulator::MpcEmcTrigEmulator(const char* fname) : SubsysReco("MPCEMCTRIGEMULATOR")
{
  //  Take the output file name as an argument
  OutFileName = fname;

  return;
}

int MpcEmcTrigEmulator::InitRun(PHCompositeNode *topNode)
{
  mpcmap = findNode::getClass<MpcMap>(topNode,"MpcMap");
  if ( mpcmap==0 )
    {
      cout << "MPCMAP = 0" << endl;
    }

  mpccalib = MpcCalib::instance();
  if ( mpccalib==0 )
    {
      cout << PHWHERE << " Couldn't find mpccalib" << endl;
      return ABORTEVENT;
    }
  //mpccalib->Print("DEADHOT");

  //recoConsts *rc = recoConsts::instance();
  //f_run = rc->get_IntFlag("RUNNUMBER");

  TString name;

  TDirectory *orig_directory = gDirectory;
  savefile = new TFile(OutFileName.c_str(),"RECREATE");

  TH1 *temphist;
  TH2 *temp2hist;
  for (int itrig=0 ;itrig<MAXTRIG; itrig++)
    {
      // 2x2 tile distribution
      name = "h2x2_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,10000.);
      temphist->SetLineColor(itrig+1);
      h2x2.push_back(temphist);

      name = "hn_2x2_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,10000.);
      temphist->SetLineColor(itrig+1);
      hn_2x2.push_back(temphist);

      name = "hs_2x2_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,10000.);
      temphist->SetLineColor(itrig+1);
      hs_2x2.push_back(temphist);

      name = "h2x2max_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,10000.);
      temphist->SetLineColor(itrig+1);
      h2x2max.push_back(temphist);

      name = "hn_2x2max_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,10000.);
      temphist->SetLineColor(itrig+1);
      hn_2x2max.push_back(temphist);

      name = "hs_2x2max_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,10000.);
      temphist->SetLineColor(itrig+1);
      hs_2x2max.push_back(temphist);

      name = "h2_2x2max_trig"; name += itrig;
      temp2hist = new TH2F(name,name,10,-0.5,19.5,10,-0.5,9.5);
      h2_2x2max.push_back(temp2hist);

      // 4x4 tile distribution
      name = "h4x4_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,20000.);
      temphist->SetLineColor(itrig+1);
      h4x4.push_back(temphist);

      name = "hn_4x4_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,20000.);
      temphist->SetLineColor(itrig+1);
      hn_4x4.push_back(temphist);

      name = "hs_4x4_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,20000.);
      temphist->SetLineColor(itrig+1);
      hs_4x4.push_back(temphist);

      name = "h4x4max_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,20000.);
      temphist->SetLineColor(itrig+1);
      h4x4max.push_back(temphist);

      name = "hn_4x4max_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,20000.);
      temphist->SetLineColor(itrig+1);
      hn_4x4max.push_back(temphist);

      name = "hs_4x4max_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,20000.);
      temphist->SetLineColor(itrig+1);
      hs_4x4max.push_back(temphist);

      name = "h2_4x4max_trig"; name += itrig;
      temp2hist = new TH2F(name,name,9,-0.5,17.5,9,-0.5,8.5);
      h2_4x4max.push_back(temp2hist);

      // energy in each tower for each trigger
      name = "h2_energy_trig"; name += itrig;
      temp2hist = new TH2F(name,name,36,-0.5,35.5,18,-0.5,17.5);
      h2_energy.push_back(temp2hist);

      // number of hits in each tower for each trigger
      name = "h2_nhit_trig"; name += itrig;
      temp2hist = new TH2F(name,name,36,-0.5,35.5,18,-0.5,17.5);
      h2_nhit.push_back(temp2hist);
    }

  orig_directory->cd();

  return 0;
}

int MpcEmcTrigEmulator::process_event(PHCompositeNode *topNode)
{
  // informational message...
  static int ncalls = 0;
  ncalls++;
  if (ncalls % 100000 == 0 && verbosity)
    {
      cout << "MpcEmcTrigEmulator Ncalls = " << ncalls << endl;
    }

  //  Get the data I need...
  mpcmap = findNode::getClass<MpcMap>(topNode,"MpcMap");
  mpcTowerContainer *mpctowers = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  //mpcClusterContainer *mpcclus = findNode::getClass<mpcClusterContainer>(topNode,"mpcClusterContainer");
  //mpcRawContainer *mpcraw = findNode::getClass<mpcRawContainer>(topNode,"MpcRaw");
  //EventHeader *evtheader = getClass<EventHeader>(topNode, "EventHeader");

  if ( mpctowers==0 )
    {
      cout << "MpcEmcTrigEmulator::process_event, mpcclus or eventheader or bbcout not found" << endl;
      cout << "\t" << mpctowers
           //<< "\t" << mpcclus
           //<< "\t" << mpcraw
           << endl;
      return ABORTEVENT;
    }

  // Get Tile distributions
  Fill_Tile_Arrays(mpctowers);

  return EVENT_OK;
}

int MpcEmcTrigEmulator::End(PHCompositeNode *topNode)
{
  savefile->Write();
  savefile->Close();

  return EVENT_OK;
}

int MpcEmcTrigEmulator::Fill_Tile_Arrays(mpcTowerContainer *mtow)
{
  // Reset the arrays
  for (int ix=0; ix<10; ix++)
    {
      for (int iy=0; iy<10; iy++)
        {
          sums_2x2[0][ix][iy] = 0.;
          sums_2x2[1][ix][iy] = 0.;
        }
    }
  for (int ix=0; ix<9; ix++)
    {
      for (int iy=0; iy<9; iy++)
        {
          sums_4x4[0][ix][iy] = 0.;
          sums_4x4[1][ix][iy] = 0.;
        }
    }

  // reset the max tile
  max2x2 = 0.;
  max2x2n = 0.;
  max2x2s = 0.;
  max4x4 = 0.;
  max4x4n = 0.;
  max4x4s = 0.;

  max2x2n_xpos = -1;
  max2x2n_ypos = -1;
  max2x2s_xpos = -1;
  max2x2s_ypos = -1;
  max4x4n_xpos = -1;
  max4x4n_ypos = -1;
  max4x4s_xpos = -1;
  max4x4s_ypos = -1;

  // Note that these sums assume a simple geometry mapping
  // of the channels.  However, there are a few exceptions...
  int ntowers = mtow->size();
  for (int itow=0; itow<ntowers; itow++)
    {
      mpcTowerContent *tower = mtow->getTower(itow);

      int fee576ch = tower->get_ch();
      int gridx = mpcmap->getGridX( fee576ch );
      if ( mpcmap->getGridX( fee576ch ) < 0 ) continue;
      int gridy = mpcmap->getGridY( fee576ch );

      float e = tower->get_energy();

      // the 2x2 tiles form a 10x10 grid
      int xcol = static_cast<int>((gridx+1)/2);
      int yrow = static_cast<int>((gridy+1)/2);

      int arm = 0;
      if ( fee576ch>=288 ) arm = 1;

      assert( arm==0 || arm==1 );
/*
      if ( ch2x2<0 || ch2x2>=200 )
        {
          cerr << PHWHERE << " ERROR, illegal ch2x2 " << ch2x2 << endl;
          continue;
        }
*/

      sums_2x2[arm][xcol][yrow] += e;
    }

  // south 4x4 tiles
  for (int iarm=0; iarm<2; iarm++)
    {
      for (int ix=0; ix<9; ix++)
        {
          for (int iy=0; iy<9; iy++)
            {
              sums_4x4[iarm][ix][iy] = sums_2x2[iarm][ix][iy] + sums_2x2[iarm][ix+1][iy]
                                     + sums_2x2[iarm][ix][iy+1] + sums_2x2[iarm][ix+1][iy+1];
            }
        }
    }

  // get maximum 2x2
  for (int ix=0; ix<10; ix++)
    {
      for (int iy=0; iy<10; iy++)
        {
          if ( sums_2x2[0][ix][iy] > max2x2s )
            {
              max2x2s = sums_2x2[0][ix][iy];
              max2x2s_xpos = ix;
              max2x2s_ypos = iy;
            }
          if ( sums_2x2[1][ix][iy] > max2x2n )
            {
              max2x2n = sums_2x2[1][ix][iy];
              max2x2n_xpos = ix;
              max2x2n_ypos = iy;
            }

          if ( sums_2x2[0][ix][iy] > max2x2 ) max2x2 = sums_2x2[0][ix][iy];
          if ( sums_2x2[1][ix][iy] > max2x2 ) max2x2 = sums_2x2[1][ix][iy];
        }
    }

  // get maximum 4x4
  for (int ix=0; ix<9; ix++)
    {
      for (int iy=0; iy<9; iy++)
        {
          if ( sums_4x4[0][ix][iy] > max4x4s )
            {
              max4x4s = sums_4x4[0][ix][iy];
              max4x4s_xpos = ix;
              max4x4s_ypos = iy;
            }
          if ( sums_4x4[1][ix][iy] > max4x4n )
            {
              max4x4n = sums_4x4[1][ix][iy];
              max4x4n_xpos = ix;
              max4x4n_ypos = iy;
            }

          if ( sums_4x4[0][ix][iy] > max4x4 ) max4x4 = sums_4x4[0][ix][iy];
          if ( sums_4x4[1][ix][iy] > max4x4 ) max4x4 = sums_4x4[1][ix][iy];
        }
    }

  return 0;
}

