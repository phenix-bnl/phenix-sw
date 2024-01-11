void make_EWG(Int_t maxEvents=0,
	      const char *dstIFile="microDST.root", 
	      const char *rootOFile="hardDST.root",
	      const char *inputName="microDST",
	      const char *outputName="nanoDST")

{
  gSystem->Load("libmicroDST.so");
  gSystem->Load("libndst.so");

  //ndstSelect::setWorkingGroup("HardWorkingGroup");
  ndstSelect::setWorkingGroup("ElectronWorkingGroup");
  //ndstSelect::setWorkingGroup("MuonWorkingGroup");
  //ndstSelect::setWorkingGroup("PhotonWorkingGroup");
  //ndstSelect::setWorkingGroup("HadronWorkingGroup");
  //ndstSelect::setWorkingGroup("CentralTrackGroup");

  ndstSelect::setBurners       ("OFF");  //Assume input already burned.
  crkSelect::setRemap          ("OFF");

  ndstSelect::setElectrons     ("OFF") ;
  ndstSelect::setCentralTracks ("ON") ;
  ndstSelect::setHardScatters  ("OFF") ;
  ndstSelect::setHardBKG       ("OFF") ;
  ndstSelect::setHadrons       ("OFF") ;
  ndstSelect::setMuons         ("OFF") ;
  ndstSelect::setPhotons       ("OFF") ;
  ndstSelect::setLvl2Primitives("ON") ;
  ndstSelect::setMcEvalSingles ("OFF") ;
  ndstSelect::setLvl2Headers   ("ON") ;
  ndstSelect::setPHTrigLvl1    ("OFF") ;
  ndstSelect::setPHTrigLvl2    ("ON") ;
  ndstSelect::setERT           ("OFF") ;




  //setProcessFlag("ndst_test","test",dstIFile,rootOFile);  // Obsolete
  dinit();
  doutfileopen(rootOFile);
  dfileopen(dstIFile);
  set_debug(0);
  drun(maxEvents);
  dexit();
}

