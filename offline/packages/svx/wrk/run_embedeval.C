
void run_embedeval(
  char* infile1="/phenix/subsys/vtx/lebedev/embedding/simdst/simDST_electrons_011_nonoise.root",
  char* infile2="/phenix/subsys/vtx/lebedev/embedding/embed/newembed_electron_011_347128-0011.root",
  char* outfile="newembedeval_011.root"
) {

  gSystem->Load("libsvx.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("/phenix/subsys/vtx/lebedev/embedding/install/lib/libembedreco.so");
  gSystem->Load("librecal.so");
  gSystem->Load("libsimreco.so");

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  recoConsts *rc = recoConsts::instance();

  MasterRecalibrator *mr = new MasterRecalibrator();
  //mr->Verbosity(1);
  se->registerSubsystem(mr);
  
  SubsysReco *eval = new SvxEmbedEval(outfile);
  eval->Verbosity(2);
  se->registerSubsystem(eval);

  cout << "Analysis started " << endl;
  gSystem->Exec("date");


  Fun4AllInputManager *in1 = new Fun4AllNoSyncDstInputManager("DSTin1","DST","SINGLE");
  Fun4AllInputManager *in2 = new Fun4AllDstInputManager("DSTin2","DST");
  se->registerInputManager(in1);
  se->registerInputManager(in2);

  in1->AddFile(infile1);
  in2->AddFile(infile2);

//  in1->AddFile("/phenix/subsys/vtx/lebedev/embedding/simdst/simDST_electrons_010.root");
//  in2->AddFile("/phenix/subsys/vtx/lebedev/embedding/embed/embed_electron_010_347128-0010.root");
//  in1->AddFile("/phenix/subsys/vtx/lebedev/embedding/simdst/simDST_electrons_011.root");
//  in2->AddFile("/phenix/subsys/vtx/lebedev/embedding/embed/embed_electron_011_347128-0011.root");
//  in1->AddFile("/phenix/subsys/vtx/lebedev/embedding/simdst/simDST_electrons_012.root");
//  in2->AddFile("/phenix/subsys/vtx/lebedev/embedding/embed/embed_electron_012_347128-0012.root");
//  in1->AddFile("/phenix/subsys/vtx/lebedev/embedding/simdst/simDST_electrons_013.root");
//  in2->AddFile("/phenix/subsys/vtx/lebedev/embedding/embed/embed_electron_013_347128-0013.root");
//  in1->AddFile("/phenix/subsys/vtx/lebedev/embedding/simdst/simDST_electrons_014.root");
//  in2->AddFile("/phenix/subsys/vtx/lebedev/embedding/embed/embed_electron_014_347128-0014.root");
//  in1->AddFile("/phenix/subsys/vtx/lebedev/embedding/simdst/simDST_electrons_015.root");
//  in2->AddFile("/phenix/subsys/vtx/lebedev/embedding/embed/embed_electron_015_347128-0015.root");
//  in1->AddFile("/phenix/subsys/vtx/lebedev/embedding/simdst/simDST_electrons_016.root");
//  in2->AddFile("/phenix/subsys/vtx/lebedev/embedding/embed/embed_electron_016_347128-0016.root");
//  in1->AddFile("/phenix/subsys/vtx/lebedev/embedding/simdst/simDST_electrons_017.root");
//  in2->AddFile("/phenix/subsys/vtx/lebedev/embedding/embed/embed_electron_017_347128-0017.root");
//  in1->AddFile("/phenix/subsys/vtx/lebedev/embedding/simdst/simDST_electrons_018.root");
//  in2->AddFile("/phenix/subsys/vtx/lebedev/embedding/embed/embed_electron_018_347128-0018.root");
//  in1->AddFile("/phenix/subsys/vtx/lebedev/embedding/simdst/simDST_electrons_019.root");
//  in2->AddFile("/phenix/subsys/vtx/lebedev/embedding/embed/embed_electron_019_347128-0019.root");

  se->run();


  se->End();

  cout << "Analysis finished " << endl;
  gSystem->Exec("date");

}


