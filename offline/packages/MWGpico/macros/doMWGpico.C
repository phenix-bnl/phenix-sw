#include <string>
#include <iostream>
#include <fstream>
using namespace std;
/* choice options are : (see LoadNano.h)
   "dAu03N2D", "dAu03NDS", "dAu03S2D", "dAu03SDS",
   "file", "file_simu" and "AuAu04". 
*/
void doMWGpico(int nevt=0, TString choice="file", TString type="AuAu")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libmutoo_subsysreco.so");
  gSystem->Load("liblvl2.so");
  gSystem->Load("libMWGpico.so");
  
  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  se->registerSubsystem( new MuonReadbackDST() );
  MWGpico *picoDST = new MWGpico(choice,"RCF");

  /* define data type : (see LoadNano.h)
     "AuAu" or "dAu"
     Needed only if choice="file" or "file_simu"
   */
  picoDST->DataType(type);

  /* set_run_type default : RUN3 */
  picoDST->set_run_type(MWGpico::RUN4);

  //=== define output picoDSTs
//  picoDST->MakePico("dimuons",choice+"_dimuons.root");      // will make an old framework dimuon picoDST 
  picoDST->MakePico("dimuonsOO",choice+"_dimuonsOO.root");  // will make a new framework dimuon picoDST
//  picoDST->MakePico("evtMixOO",choice+"_evtMixOO.root");  // will make a new framework evtMix picoDST

  //=== define cuts to be used
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
