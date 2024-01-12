#include <string>
#include <iostream>
#include <fstream>
using namespace std;
/* choice options are : (see LoadNano.h)
   "dAu03N2D", "dAu03NDS", "dAu03S2D", "dAu03SDS",
   "file", "file_simu" and "AuAu04". 
*/
void doMWGpicodAu03(int nevt=0, TString choice="dAu03N2D", TString type="dAu")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("liblvl2.so");
  gSystem->Load("libMWGpico.so");
  
  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  MWGpico *picoDST = new MWGpico(choice,"RCF");

  /* define data type : (see LoadNano.h)
     "AuAu" or "dAu"
     Needed only if choice="file" or "file_simu"
   */
  picoDST->DataType(type);
  picoDST->set_run_type(MWGpico::RUN3);
  //=== define nanoDST nodes to be accessed
  picoDST->AccessNode("MWGCuts");              // will access MWGCuts node
  picoDST->AccessNode("RunHeader");            // will access RunHeader node
  picoDST->AccessNode("TrigHelp");             // will make a TriggerHelper
  picoDST->AccessNode("PHGlobal");             // will access event information
  picoDST->AccessNode("dMuiPseudoTriggerOut"); // will access pseudo trigger information
  picoDST->AccessNode("PHMuoTracks");          // will access muon and dimuon information (old framework)

  //=== define output picoDSTs
  picoDST->MakePico("dimuons",choice+"_dimuons.root");      // will make an old framework dimuon picoDST 
//  picoDST->MakePico("sngmuons",choice+"_sngmuons.root");  // will make a new framework single muon picoDST
//  picoDST->MakePico("evtMix",choice+"_evtMix.root");      // will make an old framework event mixing picoDST 

  //=== define cuts to be used
//  picoDST->InitCuts("dAu03N2D");                 // standard dAu dimuon cuts applied
  picoDST->InitCuts("nocuts");                 // no cuts applied
  picoDST->PrintCuts();

  //=== register the picoDST 
  se->registerSubsystem(picoDST);
  picoDST->Verbosity(0);

  //=== Make Mut Efficiency map
//  picoDST->MakeMutEffic();  

  /////////////////////////////////////////////////////////////////
  //  Input Managers...
  Fun4AllDstInputManager *in = new Fun4AllDstInputManager("input","DST");
  se->registerInputManager(in);

  ////////////////////////////////////////////////////////////////
  //  OK, now loop over all the input files...
  for (int i=0; i!=picoDST->NanoListSize(); i++) {
    cout<<"accessing "<<picoDST->NanoFile(i)<<" ("<<(i+1)<<"/"<<picoDST->NanoListSize()<<")"<<endl;
    se->fileopen("input",picoDST->NanoFile(i));
    se->run(nevt);
  }
  se->End();
}
