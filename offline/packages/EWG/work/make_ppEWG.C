void make_ppEWG(Int_t maxEvents=0,
		const char *dstIFile="uuDST.root", 
		const char *rootOFile="ewgDST.root",
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
  ndstSelect::setLvl2Primitives("OFF") ;
  ndstSelect::setMcEvalSingles ("OFF") ;
  ndstSelect::setLvl2Headers   ("OFF") ;
  ndstSelect::setPHTrigLvl1    ("ON") ;
  ndstSelect::setPHTrigLvl2    ("OFF") ;
  ndstSelect::setERT           ("ON") ;

  //setProcessFlag("ndst_test","test",dstIFile,rootOFile);  // Obsolete
  dinit();
  doutfileopen(rootOFile);
  dfileopen(dstIFile);
  set_debug(1);
  drun(maxEvents);
  dexit();
}

