//  General PHENIX tools
#include <Fun4AllServer.h>
#include <getClass.h>
#include <PHCompositeNode.h>
#include <phool.h>
#include <Fun4AllReturnCodes.h>
#include <TriggerHelper.h>
#include <recoConsts.h>
#include <SyncObject.h>

//  Data classes I am using in analysis
//#include "PreviousEvent.h"
#include <MpcSimpleEvtDisp.h>
#include <mpcRawContainer.h>
#include <mpcRawContent.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <mpcClusterContainer.h>
#include <mpcClusterContent.h>
#include <MpcMap.h>

//  Root histogram types
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>
#include <TStyle.h>
#include <TSystem.h>

using namespace std;
using namespace findNode;

MpcSimpleEvtDisp::MpcSimpleEvtDisp(const char* outfile) : SubsysReco("MpcSimpleEvtDisp Analyzer")
{
  //  Take the output file name as an argument
  OutFileName = outfile;

  return ;
}

int MpcSimpleEvtDisp::InitRun(PHCompositeNode *topNode)
{
  //  All Histoes registered with Fun4All...
  //  Using the TLA "EXM" to make unique...
  mpcmap = MpcMap::instance();               // fee->grid map

  MpcDisplay[0] = new TH2F("MpcDisplay0","MPC.S Event Display",18,-0.5,17.5,18,-0.5,17.5);
  MpcDisplay[1] = new TH2F("MpcDisplay1","MPC.N Event Display",18,-0.5,17.5,18,-0.5,17.5);

  DisplayCanvas = new TCanvas("DisplayCanvas","MPC Display",600,600);
  DisplayCanvas->Divide(2,2);
  gStyle->SetPalette(1);

  trighelp = new TriggerHelper(topNode);
  if ( trighelp==0 )
    {
      cout << "MpcSimpleEvtDisp::InitRun, TriggerHelper not found" << endl;
      return ABORTRUN;
    }

  recoConsts *rc = recoConsts::instance();
  runnumber = rc->get_IntFlag("RUNNUMBER");

  return EVENT_OK;
}

int MpcSimpleEvtDisp::process_event(PHCompositeNode *topNode)
{
  SyncObject *sync = findNode::getClass<SyncObject>(topNode,"Sync");
  int event = sync->EventNumber();
  cout << "** Event " << event << endl;

  // informational message...
  static int ncalls = 0;
  ncalls++;;
  if (ncalls % 1000 == 0 && verbosity)
    {
      cout << "MpcSimpleEvtDisp Ncalls = " << ncalls << "\t" << event << endl;
    }

  //  Get the data I need...
  mpcraw = getClass<mpcRawContainer>(topNode, "MpcRaw");
  mpctowers = getClass<mpcTowerContainer>(topNode, "mpcTowerContainer");
  mpcclus = getClass<mpcClusterContainer>(topNode, "mpcClusterContainer");

  MpcDisplay[0]->Reset();
  MpcDisplay[1]->Reset();

  const int MBIAS = 0;
  const int MPC4X4A = 1;
  const int MPC4X4B = 2;
  const int MPC4X4CERTC = 3;

  int trigger[4] = {0};
  trigger[MBIAS] = trighelp->IsEventMinBias() ? 1 : 0;
  trigger[MPC4X4A] = trighelp->trigScaled("MPC4x4a") ? 1 : 0;
  trigger[MPC4X4B] = trighelp->trigScaled("MPC4x4b") ? 1 : 0;
  trigger[MPC4X4CERTC] = trighelp->trigScaled("MPC4x4c&ERTLL1_2x2") ? 1 : 0;

  //if ( trigger[MBIAS] == 0 ) return EVENT_OK;
  //if ( trigger[MPC4X4A] == 0 && trigger[MPC4X4B] == 0 ) return EVENT_OK;

  cout << "Nraw Ntower " << mpcraw->size() << "\t" << mpctowers->size() << endl;

  //FillRawDisplay();
  FillTowerDisplay();

  // go to next event after displaying canvas
  if ( trigger[MPC4X4A] == 1 )
    {
      DisplayCanvas->cd(1);
      MpcDisplay[0]->DrawCopy("colz");
      gPad->SetLogz(1);
      DisplayCanvas->cd(2);
      MpcDisplay[1]->DrawCopy("colz");
      gPad->SetLogz(1);
      DisplayCanvas->cd(3);
      MpcDisplay[0]->DrawCopy("lego2");
      DisplayCanvas->cd(4);
      MpcDisplay[1]->DrawCopy("lego2");
      gPad->Update();

/*
      TString junk;
      cin >> junk;
      if ( junk.BeginsWith("q") || junk.BeginsWith("Q") )
        {
          cout << "Quitting..." << endl;
          return ABORTRUN;
        }
      else if ( junk.BeginsWith("s") || junk.BeginsWith("S") )
        {
          TString name = "mpc_"; name += runnumber;
          name += "_"; name += ncalls; name += ".png";
        }
*/

      TString name = "mpc_"; name += runnumber;
      name += "_"; name += event; name += ".png";
      //DisplayCanvas->SaveAs( name );

    }

  for (unsigned int ich = 0; ich < mpcclus->size(); ich++)
    {
      mpcClusterContent *clus = mpcclus->getCluster(ich);

      float ecore = clus->ecore();
      if ( ecore>100 ) cout << "ecore " << ecore << endl;
      if ( ecore > 200. )
      //if ( ecore > 2. )
        {
          int arm = clus->arm();
          int gridx = clus->ixpos();
          int gridy = clus->iypos();
          float x = clus->x();
          float y = clus->y();
          float dispx = clus->corrdispx();
          float dispy = clus->corrdispy();
          float chi2core = clus->chi2core();
          int ndfcore = clus->ndfcore();
          int ntow = clus->multiplicity();

          cout << "xxxxxx" << endl;
          cout << ecore << "\t" << arm << "\t" << x << "\t" << y << endl;
          cout << "\t" << dispx << "\t" << dispy << "\t" << chi2core
               << "\t" << ndfcore << "\t" << ntow << endl << endl;
          float tower_ecent = MpcDisplay[arm]->GetBinContent(gridx+1,gridy+1);
          double e9sum = 0.;
          for (int iypos=gridy+1; iypos>=gridy-1; iypos--)
            {
              for (int ixpos=gridx-1; ixpos<=gridx+1; ixpos++)
                {
                  float val = MpcDisplay[arm]->GetBinContent(ixpos+1,iypos+1);
                  e9sum += val;
                  cout << "\t\t" << val;
                }
              cout << endl;
            }
          float ecent = clus->ecent();
          float e9 = clus->e9();
          cout << "\t" << e9 << "\t" << ecent << "\t" << ecent/e9 << endl;
          cout << "\t" << e9sum << "\t" << tower_ecent << "\t" << tower_ecent/e9sum << endl;
if ( ecent!=tower_ecent )
{
  cout << "NOT MATCHING " << ecent << "\t" << tower_ecent << endl;
}
if ( e9>e9sum )
{
  cout << "E9>E9SUM     " << e9 << "\t" << e9sum << endl;
}

/*
          string junk;
          cout << "? ";
          cin >> junk;
          if ( junk[0]=='q' )
            {
              gSystem->Exit(0);
            }
*/
        }
    }

  // any other return code might lead to aborting the event or analysis for everyone
  return EVENT_OK;
}

void MpcSimpleEvtDisp::FillRawDisplay()
{
  const float adcgain = 1.0;

  for (unsigned int ich = 0; ich < mpcraw->size(); ich++)
    {
      mpcRawContent *tower = mpcraw->getTower(ich);

      int fee576ch = tower->get_ch();

      if ( mpcmap->isCrystal( fee576ch ) == 0 ) continue;

      int tdc = tower->get_tdc();
      int lopost = tower->get_lopost();
      int lopre = tower->get_lopre();

      // make cut on timing
      //if ( tdc > 2300 ) continue;

      float energy = (lopost - lopre) * adcgain;
      cout << fee576ch << "\t" << tdc << "\t" << lopost << "\t" << lopre << endl;
      //if ( energy < 30 ) continue;  // skip empty channels

      // Fill crystal view for MPC triggers
      int xpos = mpcmap->getGridX( fee576ch );
      int ypos = mpcmap->getGridY( fee576ch );
      int arm  = mpcmap->getArm( fee576ch );

      MpcDisplay[arm]->Fill( xpos, ypos, energy);
    }
}

void MpcSimpleEvtDisp::FillTowerDisplay()
{
  for (unsigned int ich = 0; ich < mpctowers->size(); ich++)
    {
      mpcTowerContent *tower = mpctowers->getTower(ich);

      int fee576ch = tower->get_ch();
      float energy = tower->get_energy();

      if ( mpcmap->isCrystal( fee576ch ) == 0 ) continue;

      //cout << fee576ch << "\t" << tdc << "\t" << lopost << "\t" << lopre << endl;
      //if ( energy < 30 ) continue;  // skip empty channels

      // Fill crystal view for MPC triggers
      int xpos = mpcmap->getGridX( fee576ch );
      int ypos = mpcmap->getGridY( fee576ch );
      int arm  = mpcmap->getArm( fee576ch );

      MpcDisplay[arm]->Fill( xpos, ypos, energy);
    }
}

int MpcSimpleEvtDisp::End(PHCompositeNode *topNode)
{
  return 0;
}

