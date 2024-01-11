
void svxjeteval() {

  gSystem->Load("libPHPythia.so");
//  gSystem->Load("/phenix/u/workarea/lebedev/offline/analysis/run7_jpsi_isu/install/lib/libPhAnalysis.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libsvx.so");
  gSystem->Load("libsvxjet.so");
//  gSystem->Load("libKalFit.so");
  gSystem->Load("libPHPythiaEventGen.so");

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);
  recoConsts *rc = recoConsts::instance();

  SubsysReco *jeteval = new SvxJetRecoEval("Kt07JetsNoNeutralInVTX", "SVXJetsKt07");
  jeteval->Verbosity(0);
  se->registerSubsystem(jeteval);

// Input manager(s)
  Fun4AllInputManager *in1 = new Fun4AllNoSyncDstInputManager("DSTin1","DST");
  in1->Verbosity(0);
  se->registerInputManager(in1);
  Fun4AllInputManager *in2 = new Fun4AllNoSyncDstInputManager("DSTin2","DST");
  in2->Verbosity(0);
  se->registerInputManager(in2);

  cout << "Analysis started " << endl;
  gSystem->Exec("date");

  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_100kevts_110.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_100kevts_110.root");
  se->run();
  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_100kevts_111.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_100kevts_111.root");
  se->run();
  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_100kevts_112.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_100kevts_112.root");
  se->run();
  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_100kevts_113.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_100kevts_113.root");
  se->run();
  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_100kevts_114.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_100kevts_114.root");
  se->run();
  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_100kevts_115.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_100kevts_115.root");
  se->run();
  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_100kevts_116.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_100kevts_116.root");
  se->run();
  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_100kevts_117.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_100kevts_117.root");
  se->run();
  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_100kevts_118.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_100kevts_118.root");
  se->run();
  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_100kevts_119.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_100kevts_119.root");
  se->run();

/*
  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_1kevts_00.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_1kevts_00.root");
  se->run();
  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_1kevts_01.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_1kevts_01.root");
  se->run();
  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_1kevts_02.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_1kevts_02.root");
  se->run();
  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_1kevts_03.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_1kevts_03.root");
  se->run();
  se->fileopen("DSTin2","/phenix/subsys/vtx/lebedev/phpythia/simDST_jets_1kevts_04.root");
  se->fileopen("DSTin1","/phenix/subsys/vtx/lebedev/phpythia/phpythia_jets_1kevts_04.root");
  se->run();
*/

  se->End();

  cout << "Analysis finished " << endl;
  gSystem->Exec("date");

}


