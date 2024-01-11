/*!
 prdf analysis loop for real data
 creates a reconstructed DST, a NanoDST and a picoDST
 it is configured to run on Run4. Should be easily tweaked
 for later runs

 Use VTX

 VtxOut* vtx = findNode::getClass<VtxOut>(top_node, "VtxOut" );
 if (!vtx)
 {
 cout << PHWHERE << "mFillSingleMuonContainer:: VtxOut not in Node Tree" << endl;
 //      return ABORTRUN;
 }

 if (vtx)
 {
 muons->set_Evt_vtxX(vtx->get_Vertex("SVX_PRECISE").getX());
 muons->set_Evt_vtxY(vtx->get_Vertex("SVX_PRECISE").getY());
 muons->set_Evt_vtxZ(vtx->get_Vertex("SVX_PRECISE").getZ());
 }


 */

void
Fun4FVTX_RecoPRDF(int nEvents = 0, int runnumber = 365720,
    char *input_file =
        "/phenix/subsys/fvtx/GoldenDimuon/run_0000365000_0000366000/GOLDENEVENT_DIMUON_run12_online_muon-0000365720-0035.PRDFF",
    char *ndst_file = "ndst_out.root", char *pdst_file = "pdst_out.root",
//char *qa_file = "qa_out.root",
//char *eval_file = "fvtx_eval.root"
    char *primvtxfile = "vertexprim_fvtx.root")

{
  char *dst_file = "dst_out.root";
  // load libraries
  gSystem->Load("libfun4all");
  gSystem->Load("libmutoo_subsysreco");
  gSystem->Load("libfun4allfuncs_muons");
  gSystem->Load("liblvl2");
  gSystem->Load("libfvtx_subsysreco.so");

  //  gSystem->Exec("ln -s /afs/rhic.bnl.gov/phenix/users/slash/FVTX/reco/fvtxgeom.root .");

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("SVXACTIVE", 1);

  //  TMutExtVtx::get().set_verbosity( MUTOO::SOME );

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  se->registerSubsystem(new HeadReco());
  se->registerSubsystem(new SyncReco());
  se->registerSubsystem(new TrigReco());

  TrigSelect *trgsel = new TrigSelect("MB");
  se->registerSubsystem(trgsel);
  trgsel->AddTrigger("BBCLL1(>1 tubes) narrowvtx");
  trgsel->AddTrigger("BBCLL1(>0 tubes) narrowvtx");
  //  trgsel->SetDefaultReturnCode("ABORT");

  //set BBC-vtx resolution
  BbcReco *bbc_reco = new BbcReco();
  bbc_reco->setBbcVtxError(0.5);
  se->registerSubsystem(bbc_reco);

  //  MuonTrigFilter * filter_fvtx = new MuonTrigFilter("FVTX_EVTS", MuonTrigFilter::Z_VERTEX, MuonTrigFilter::ABORT_EVENT);
  //  filter_fvtx->set_z_vertex_window(-15, 15);
  //  se->registerSubsystem(filter_fvtx);

  se->registerSubsystem(new ZdcReco());
  se->registerSubsystem(new VtxReco());

  // muon prdf unpacker
  MuonUnpackPRDF* muon_unpack_prdf(new MuonUnpackPRDF());
  muon_unpack_prdf->Verbosity(1);
  muon_unpack_prdf->set_flag(MuonUnpackPRDF::SKIP_ZERO_SUPPRESSION, 1);
  se->registerSubsystem(muon_unpack_prdf);

  // mutoo reconstruction
  se->registerSubsystem(new MuiooReco());
  se->registerSubsystem(new MuonDev());

  // SVX reconstruction"

  SvxParManager *svxpar = new SvxParManager();
  svxpar->Verbosity(0);
  se->registerSubsystem(svxpar);

  SvxDecode *svxdecode = new SvxDecode();
  svxdecode->Verbosity(0);
  svxdecode->includePixel(true);
  svxdecode->includeStripixel(true);
  svxdecode->setAdcOffset(24);
  svxdecode->setAdcCutoff(-24);
  se->registerSubsystem(svxdecode);

  //  SvxApplyHotDead *svxhotdead = new SvxApplyHotDead();
  //  svxhotdead->Verbosity(0);
  //  se->registerSubsystem(svxhotdead);

  SvxReco *svxrec = new SvxReco();
  svxrec->Verbosity(0);
  // svxrec->Load_ThresholdFile("threshold.h");
  svxrec->set_UseStripThresholdDatbase(true);
  svxrec->set_StripixelAdcSumThreshold(0);
  se->registerSubsystem(svxrec);

  SvxPriVertexSeedFinder *svxvtxseedfinder = new SvxPriVertexSeedFinder();
  svxvtxseedfinder->Verbosity(0);
  se->registerSubsystem(svxvtxseedfinder);

  SvxPriVertexSeedFinder *vtxseedw = new SvxPriVertexSeedFinder(
      "SVXPRIVERTEXFINDERW", 1);
  se->registerSubsystem(vtxseedw);

  SvxPriVertexSeedFinder *vtxseede = new SvxPriVertexSeedFinder(
      "SVXPRIVERTEXFINDERE", 2);
  se->registerSubsystem(vtxseede);

  SvxStandAloneReco *svxstandalone = new SvxStandAloneReco();
  svxstandalone->Verbosity(0);
  svxstandalone->setVertexRecoFlag(2);
  svxstandalone->setWindowScale(10);
  se->registerSubsystem(svxstandalone);

  SvxPrimVertexFinder *svxprimvtxfinder = new SvxPrimVertexFinder();
  svxprimvtxfinder->Verbosity(0);
  se->registerSubsystem(svxprimvtxfinder);

  FvtxUnpackPRDF *Fvtx_unpack = new FvtxUnpackPRDF();
  se->registerSubsystem(Fvtx_unpack);

  FvtxReco* fvtxreco = new FvtxReco();
  se->registerSubsystem(fvtxreco);

  //  FvtxEval* fvtxeval = new FvtxEval("FvtxEval",eval_file);
  //  se->registerSubsystem(fvtxeval);

  se->registerSubsystem(new GlobalReco());

  gSystem->Load("libMWGOO");
  SubsysReco *mwg = new MWGFvtxReco(new MWGInclusiveNanoCutsv2(), primvtxfile); //muon nDST module
  se->registerSubsystem(mwg);

  /*
   gSystem->Load("libpicodst_object");
   mFillSingleMuonContainer* msngl = new mFillSingleMuonContainer();
   se->registerSubsystem(msngl);
   msngl->set_bbcz_cut(50);
   msngl->set_pz_cut(2.0);
   msngl->set_lastgap_cut(4);
   //  msngl->set_DG0_cut(30);
   //  msngl->set_DDG0_cut(20);
   //  msngl->set_chi2_cut(20);
   //  msngl->set_nidhits_cut(5);
   //  msngl->set_dca_z_cut(9999.9);

   mFillDiMuonContainer* mdi = new mFillDiMuonContainer(false, 2.0);
   se->registerSubsystem(mdi);
   mdi->set_is_pp(true);
   mdi->set_mass_cut(0.5);
   */

  bool save_dst = false;
  if (save_dst)
    {
      Fun4AllDstOutputManager *dstManager = new Fun4AllDstOutputManager(
          "DSTOUT", dst_file);
      //FVTX nodes
      dstManager->AddNode("Sync");
      dstManager->AddNode("TrigLvl1");
      //      dstManager->AddNode("TFvtxHit");
      dstManager->AddNode("TFvtxTrk");

      // Muioo nodes
      //  dstManager->AddNode("TMuiHitO");
      //      dstManager->AddNode("TMuiRoadO");
      //  dstManager->AddNode("TMuiClusterO");
      //  dstManager->AddNode("TMui1DRoadO");
      //  dstManager->AddNode("TMuiPseudoBLTO");
      //      dstManager->AddNode("TMuiPseudoLL1");

      // Mutoo nodes
      //  dstManager->AddNode("TMutHit");
      //  dstManager->AddNode("TMutMuiRoad");
      //  dstManager->AddNode("TMutClus");
      //      dstManager->AddNode("TMutCoord");
      //  dstManager->AddNode("TMutGapCoord");
      //  dstManager->AddNode("TMutStub");
      //      dstManager->AddNode("TMutTrk");
      //      dstManager->AddNode("TMutVtx");

      se->registerOutputManager(dstManager);
    }

  Fun4AllDstOutputManager *nanodstManager = new Fun4AllDstOutputManager(
      "nanoDSTOUT", ndst_file);
  nanodstManager->AddNode("Sync");
  nanodstManager->AddNode("TrigLvl1");
  nanodstManager->AddNode("PHMuoTracksOO");
  nanodstManager->AddNode("VtxOut");
  nanodstManager->AddNode("PHGlobal");
  nanodstManager->AddNode("PHGlobal_MUON");
  se->registerOutputManager(nanodstManager);

  Fun4AllOutputManager *outee = new Fun4AllDstOutputManager("Outee", pdst_file);
  outee->AddNode("Sync");
  outee->AddNode("DiMuonContainer");
  outee->AddNode("TrigLvl1");
  outee->AddEventSelector("mFillDiMuonContainer");
  se->registerOutputManager(outee);

  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  //in->fileopen(input_file);
  in->AddListFile(input_file);
  se->registerInputManager(in);
  se->run(nEvents);
  se->End();

  cout << "Completed reconstruction." << endl;
}
