//  GENERal PHENIX tools
//#include "Fun4AllServer.h"
#include <getClass.h>
//#include <PHCompositeNode.h>
//#include <phool.h>
//#include "Fun4AllHistoManager.h"
//#include <Fun4AllReturnCodes.h>

//  Data classes I am using in analysis
//#include "TriggerHelper.h"
//#include "TrigLvl1.h"
//#include "PHGlobal.h"
#include <EventHeader.h>
#include <mpcRawContainer.h>
#include <mpcRawContent.h>
#include <MpcMap.h>
#include <MpcCalib.h>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>

#include <MpcHiLo.h>

using namespace std;
using namespace findNode;

MpcHiLo::MpcHiLo(const char* outfile) : SubsysReco("MpcHiLo")
{
  EventCounter = 0;
  OutFileName = outfile;
}

int MpcHiLo::InitRun(PHCompositeNode *topNode)
{
  mpcmap = MpcMap::instance();
  mpccalib = MpcCalib::instance();

  TDirectory *orig_directory = gDirectory;

  //  Take the output file name as an argument
  savefile = new TFile(OutFileName.c_str(),"RECREATE");

  TString name;
  for (int ich=0; ich<576; ich++)
    {
      if ( mpcmap->isCrystal(ich) != 1 ) continue;

      name = "hlovshi"; name += ich;
      hlovshi[ich] = new TH2D(name,name,1024,-0.5,4095.5,225,-50.5,849.5);
      hlovshi[ich]->SetXTitle("hi adc");
      hlovshi[ich]->SetYTitle("lo adc");

      name = "hhiloratio"; name += ich;
      hhiloratio[ich] = new TH1F(name,name,3000,0.,30.);
    }

  orig_directory->cd();

  return EVENT_OK;
}

int MpcHiLo::process_event(PHCompositeNode *topNode)
{
  // informational message...
  static int ncalls = 0;
  ncalls++;;
  if ( ncalls % 10000 == 0 )
    {
      cout << "MpcHiLo Ncalls = " << ncalls << endl;
    }

  //  Get the data I need...
  mpcRawContainer *mpcraw = getClass<mpcRawContainer>(topNode, "MpcRaw");
  EventHeader *evtheader = getClass<EventHeader>(topNode, "EventHeader");

  if ( mpcraw==0 || evtheader==0 )
    {
      cout << "MpcHiLo::process_event(), mpcraw or eventheader not found" << endl;
      cout << "\t" << (unsigned int)mpcraw
           << "\t" << (unsigned int)evtheader
           << endl;
      return ABORTEVENT;
    }

  int post_amu = mpcraw->get_post_amu();
  int pre_amu = mpcraw->get_pre_amu();

  for (unsigned int ich = 0; ich < mpcraw->size(); ich++)
    {
      mpcRawContent *tower = mpcraw->getTower(ich);

      int fee576_ch = tower->get_ch();

      // skip channels which are not towers
      if ( mpcmap->isCrystal(fee576_ch) != 1 ) continue;

      //int tdc = tower->get_tdc();
      float lopost = tower->get_lopost() - mpccalib->get_ped_lgpost(fee576_ch,post_amu);
      float lopre = tower->get_lopre() - mpccalib->get_ped_lgpre(fee576_ch,pre_amu);
      float hipost = tower->get_hipost() - mpccalib->get_ped_hgpost(fee576_ch,post_amu);
      float hipre = tower->get_hipre() - mpccalib->get_ped_hgpre(fee576_ch,pre_amu);

      float loadc = lopost - lopre;
      float hiadc = hipost - hipre;

      // A few sanity checks
      if ( hiadc<=0. ) continue;

      hlovshi[fee576_ch]->Fill( hiadc, loadc );

/*
      if ( hiadc<3000. && loadc>20. && hiadc>320. )
        {
          hhiloratio[fee576_ch]->Fill( hiadc/loadc );
        }
*/

   }

  return EVENT_OK;
}

int MpcHiLo::End(PHCompositeNode *topNode)
{
  if ( savefile )
    {
      savefile->Write();
      savefile->Close();
    }
  return EVENT_OK;
}

