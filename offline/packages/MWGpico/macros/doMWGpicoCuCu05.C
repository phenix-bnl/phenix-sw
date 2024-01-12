#include <string>
#include <iostream>
#include <fstream>
using namespace std;
/* choice options are : (see LoadNano.h)
   "CuCu05N", "CuCu05S", "file" and "file_simu".
*/
// put in nano_list the list of the nDSTs files to process
void doMWGpicoCuCu05(int nevt=0, char* nano_list="list.txt", TString choice="file", TString type="CuCu")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("librecal.so");
  gSystem->Load( "liblvl2.so");
  gSystem->Load("libmutoo_subsysreco.so");
  gSystem->Load("libMWGpico.so");

  // mutoo vertex source configuration

  // AR: I don't know which one is the good one? 
  // AR: there will be the following error message if no vertex config is given

  //----------------------------- TMutExtVtx::load_vtx -----------------------------
  // The vertex source is invalid. It must be set explicitely
  // in the running .C macro, using TMutExtVtx::get().set_vtx_source( ... )
  // on January 19, 2006, the possible values are:
  // - TMutExtVtx::NONE - to use no external vertex information
  // - TMutExtVtx::MC - to use the pisa event header
  // - TMutExtVtx::BBC - to use the BBC vertex
  // - TMutExtVtx::BBC_OR_MC - to try use the BBC vertex,
  //				       then the MC vertex, if the first is not valid.

  TMutExtVtx::get().set_vtx_source( TMutExtVtx::NONE );
  TMutExtVtx::get().set_verbosity( MUTOO::NONE );

  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  /////////////////////////////////////////////////////////////////
  // necessary to access to all the maps in the ndst
  se->registerSubsystem( new MuonReadbackDST() );

  MasterRecalibrator *mr = MasterRecalibrator::instance();
  //Frowned upon way to use only specific recalibrators
  // mr->UseOnly("BunchCross");
  // mr->AddRecalibrator("Run5CuCu200GeVCentralityReco");
  se->registerSubsystem(mr);

  MWGpico *picoDST = new MWGpico(choice,"RCF");

  /* define data type : (see LoadNano.h)
     "AuAu" or "dAu" or "CuCu"
     Needed only if choice="file" or "file_simu"
   */
  picoDST->DataType(type);

  /* set_run_type default : RUN3 */
  picoDST->set_run_type(MWGpico::RUN5);

  // customize name for the ndst file list
  picoDST->set_nano_file( nano_list );

  //=== define output picoDSTs
  picoDST->MakePico("dimuonsOO",choice+"_dimuonsOO.root");  // will make a new framework dimuon picoDST

  //=== define cuts to be used
  picoDST->InitCuts("nocuts");                 // no cuts applied
  picoDST->PrintCuts();

  //=== register the picoDST 
  se->registerSubsystem(picoDST);
  picoDST->Verbosity(0);

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
