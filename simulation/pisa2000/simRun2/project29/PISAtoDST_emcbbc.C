void PISAtoDST_emcbbc(const Int_t maxEvents=1, Int_t simFlag=2,
		      const char *pisaIFile="PISAEvent.root", const Int_t simVertexFlag=0,
		      Float_t simZ0Vertex=0.0, Float_t simT0Vertex=0.0,
		      Float_t simZ0VertexWidth=-9999.0, Float_t simT0VertexWidth=-9999.0,
		      const Int_t dchReqFlag=0, Int_t pc1ReqFlag=0, Int_t pc2ReqFlag=0,
		      const Int_t pc3ReqFlag=0, Int_t tofReqFlag=0, Int_t relOutFlag=0,
		      Int_t writeNullFlag=1, const Int_t forcedEMCalAccept=0, Int_t mergeFiles=0,
		      const char *pisaMerge1="/phenix/data11/rhphemds/run2/macroTest/PISAEvent.piminus5000-2.5.root",
		      const char *pisaMerge2="/phenix/data11/rhphemds/run2/macroTest/PISAEvent.piminus5000-3.5.root",
		      const char *pisaMerge3="/phenix/data11/rhphemds/run2/macroTest/PISAEvent.piminus5000-4.5.root",
		      const char *pisaMerge4="/phenix/data11/rhphemds/run2/macroTest/PISAEvent.piminus5000-5.5.root",
		      const Int_t debugFlag=0) {
  
  gSystem->Load("libpreco.so");  // requires the upgraded version of libpreco.so (Jan 3 or later)

  recoConsts *rc = recoConsts::instance(); // instantiate flags

  setSimulationFlag(simFlag);  // simFlag = 2 sets PISA-to-DST mode in PRECO
  setEvaluationFlag();
  setMapFileFlag(2);  // using November 2001 3D magnetic field map (default was Feb. '97 2D map)
  setMapFileScale(1.0); // set map file scale factor as 1.0 

//
// simVertexFlag = 0 (default) means that the BBC Z0 value will be used
// simVertexFlag = 1 means that the same simZ0Vertex value is used for all events
// simVertexFlag = 2 means that the Z0 is taken from the PISA event header for each event
//
  if(simVertexFlag>0) {
    setSimVertexFlag(simVertexFlag, simZ0Vertex, simT0Vertex, simZ0VertexWidth, simT0VertexWidth);
  } // check if user wants to set the Z0 and the T0 in the BBC output

  setPadTrackEvaluate(1);

  Int_t iTest;
  if(debugFlag) {
    //
    // Debug pause for doing attach gdb
    //
    cout << "\n debug pause " << endl;
    cin >> iTest;
    cout << "\n iTest = " << iTest << endl;
  } // used for attached debug mode

  rc->set_SysFlag("dchReqFlag", dchReqFlag);
  rc->set_SysFlag("pc1ReqFlag", pc1ReqFlag);
  rc->set_SysFlag("pc2ReqFlag", pc2ReqFlag);
  rc->set_SysFlag("pc3ReqFlag", pc3ReqFlag);
  rc->set_SysFlag("tofReqFlag", tofReqFlag);
  setEMCalForcedAcceptance(forcedEMCalAccept);

  rc->set_SysFlag("relOutFlag", relOutFlag);
  rc->set_SysFlag("writeNullFlag", writeNullFlag);
  
  rc->set_SysFlag("MVD",0);  // 0 means the system is OFF, 1 means the system is ON
  rc->set_SysFlag("ZDC",0);  // ZDC is orphaned in the integrated simulation
  rc->set_SysFlag("BBC",1);   
  rc->set_SysFlag("DCH",0);
  rc->set_SysFlag("PAD",0);
  rc->set_SysFlag("CRK",0); // CRK gives error message about dCrkCal data structure
  rc->set_SysFlag("TEC",0);
  rc->set_SysFlag("TOF",0);
  rc->set_SysFlag("EMC",1);
  rc->set_SysFlag("VTX",0);
  rc->set_SysFlag("CGL",0);
  rc->set_SysFlag("MOM",0);
  rc->set_SysFlag("MUTR",0);
  rc->set_SysFlag("MUID",0);
  rc->set_SysFlag("NTC",0);
  rc->set_SysFlag("LVL2",0);
  rc->set_SysFlag("SPIN",0);
  rc->set_SysFlag("TZR",0);
  rc->set_SysFlag("ERT",0);
  rc->set_SysFlag("HEAD",0);
  rc->set_SysFlag("TRIG",0);
  rc->set_SysFlag("T0",0);

  pisaFileOpen(pisaIFile);
  if(mergeFiles>0&&mergeFiles<5) {
    pisaSetMergeFiles(mergeFiles);     
    cout << "\n mergeFiles = " << mergeFiles << endl;
    pisaMergeFileOpen(pisaMerge1, 1);  // first Merge file

    if(mergeFiles>1)
      pisaMergeFileOpen(pisaMerge2, 2);  // second Merge file

    if(mergeFiles>2)
      pisaMergeFileOpen(pisaMerge3, 3);  // third Merge file

    if(mergeFiles>3)
      pisaMergeFileOpen(pisaMerge4, 4);  // fourth Merge file
  }  // check on mergeFiles

  simSetRunNumber(-1);  // Full field, normal geometry
  simDstFileOpen("simDST.root");
  relFileOpen("rawrel.root");

  gBenchmark->Start("eventLoop");
  pisaFileRun(maxEvents);
  gBenchmark->Show("eventLoop");
  pisaFileEnd();

  cout<<" End of PISAtoDST_emcbbc.C "<<endl;
  return;
}
