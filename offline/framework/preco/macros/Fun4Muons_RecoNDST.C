// $Id: Fun4Muons_RecoNDST.C,v 1.10 2009/12/03 23:54:33 hpereira Exp $
/*!
  nanoDST reconstruction. Load nanoDST specified from a list.
  creates a picoDST.
*/
#include <string>
#include <iostream>
#include <fstream>

using namespace std;

//__________________________________________________
void Fun4Muons_RecoNDST(
  const char* ndst_list = "ndst_list",
  const char* outputfile = "pdst_out.root" )
{

  gSystem->Load("libfun4all");
  gSystem->Load("libmutoo_subsysreco");
  gSystem->Load("libfun4allfuncs_muons");
  gSystem->Load("liblvl2");
  gSystem->Load("libMWGpico");

  bool use_ll1 = false;

  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...

  // counter
  se->registerSubsystem( new MuonCounter() );

  // Muon readbackDST to load saved mutoo/muioo nodes from the nanoDST
  // and convert them into regular PHMap
  se->registerSubsystem( new MuonReadbackDST() );

  // minimum bias filter
  TrigSelect *trig_select;
  if( use_ll1 )
  {

    trig_select = new TrigSelect("MUID_LL1");
    trig_select->AddTrigger("MUIDLL1_S2D&BBCLL1");
    trig_select->AddTrigger("MUIDLL1_N2D&BBCLL1");
    trig_select->AddTrigger("(MUIDLL1_N2D||S2D)&BBCLL1");

    trig_select->SetReturnCode("ABORT");
    se->registerSubsystem(trig_select);
    cout << "Fun4Muons_FastCabanaboy - adding minimum bias trigger requirement" << endl;

  }


  MWGpico *picoDST = new MWGpico("simu");
  picoDST->MakePico( MWGpico::DIMUONSOO, outputfile );
  picoDST->MakePico( MWGpico::HISTOGRAMS, outputfile );
  picoDST->InitCuts("nocuts");
  picoDST->PrintCuts();
  se->registerSubsystem(picoDST);

  /////////////////////////////////////////////////////////////////
  //  Input Managers...
  Fun4AllDstInputManager *in = new Fun4AllDstInputManager("input","DST");
  se->registerInputManager(in);
  in->AddListFile( ndst_list );
  in->Print();

  se->run(1000);
  se->End();
  cout << "Completed reconstruction." << endl;

}
