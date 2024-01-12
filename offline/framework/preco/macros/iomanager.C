void iomanagerdAu(const char *outfile ="dstdAu.root" )
{
  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *io = new Fun4AllDstOutputManager("DSTOUT", outfile);
  AddCommondAu(io);
  se->registerIOManager(io);
  io->Print();
}

void iomanagerPP()
{
  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *io = new Fun4AllDstOutputManager("DSTOUT", "dstpp.root");
  io->AddNode("BbcOut");
  io->AddNode("BbcRaw");
  io->AddNode("CglTrack");
  io->AddNode("CglTrackBack");
  io->AddNode("dCrkHit");
  io->AddNode("CrkHit");
  io->AddNode("CrkRing");
  io->AddNode("CrkRingBack");
  io->AddNode("DchHitLineTable");
  io->AddNode("DchTrack");
  io->AddNode("dEmcClusterLocalExt");
  io->AddNode("dEmcCalibTower");
  io->AddNode("EventHeader");
  io->AddNode("ErtOut");
  io->AddNode("NtcOut");
  io->AddNode("dPc1Cluster");
  io->AddNode("dPc2Cluster");
  io->AddNode("dPc3Cluster");
  io->AddNode("Pc1Cluster");
  io->AddNode("Pc2Cluster");
  io->AddNode("Pc3Cluster");
  io->AddNode("PcrOut");
  io->AddNode("PHTrackOut");
  io->AddNode("PHTrackOutBack");
  io->AddNode("dPHDchTrack");
  io->AddNode("PHDchTrackOut");
  io->AddNode("T0Out");
  io->AddNode("TecOut");
  io->AddNode("dTofReconstructed");
  io->AddNode("TofOut");
  io->AddNode("TrigLvl1");
  io->AddNode("TzrOut");
  io->AddNode("TzrRaw");
  io->AddNode("VtxOut");
  io->AddNode("ZdcOut");
  io->AddNode("ZdcRaw");

  se->registerIOManager(io);
  io->Print();
}

void iomanager1(const char *outfile ="dstdAu.root" )
{
  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *io = new Fun4AllDstOutputManager("DSTOUT", outfile);
  AddCommondAu(io);
  se->registerIOManager(io);
  io->Print();
}

void iomanagerMuon(const char *outfile ="dstdAuMuon.root" )
{
  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *io = new Fun4AllDstOutputManager("MUONTrig", outfile);
  AddCommondAu(io);
  io->AddEventSelector("MUON");
  se->registerIOManager(io);
  io->Print();
}

void iomanagerGamma(const char *outfile ="dstdAuGamma.root" )
{
  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *io = new Fun4AllDstOutputManager("ERTGamma", outfile);
  AddCommondAu(io);
  io->AddEventSelector("ERTGAMMA");
  se->registerIOManager(io);
  io->Print();
}
void iomanagerElectron(const char *outfile ="dstdAuElectron.root" )
{
  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *io = new Fun4AllDstOutputManager("ERTElectron", outfile);
  AddCommondAu(io);
  io->AddEventSelector("ERTELECTRON");
  se->registerIOManager(io);
  io->Print();
}

void iomanagerMinBias(const char *outfile ="dstdAuMinBias.root" )
{
  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *io = new Fun4AllDstOutputManager("MinBias", outfile);
  AddCommondAu(io);
  io->AddEventSelector("MINBIAS");
  se->registerIOManager(io);
  io->Print();
}

void AddCommondAu(Fun4AllDstOutputManager *io)
{
  io->AddNode("BbcOut");
  io->AddNode("BbcRaw");
  io->AddNode("CglTrack");
  io->AddNode("CglTrackBack");
  io->AddNode("CrkHit");
  io->AddNode("CrkRing");
  io->AddNode("CrkRingBack");
  io->AddNode("DchHitLineTable");
  io->AddNode("DchTrack");
  io->AddNode("dMuiPseudoTriggerOut");
  io->AddNode("dMuiRaw");
  io->AddNode("dMuiRoadRawRel");
  io->AddNode("dMutCalibCathodesOut");
  io->AddNode("dMutCathodeClusters");
  io->AddNode("dMuiClusters");
  io->AddNode("dMuiClusterRawRel");
  io->AddNode("dMuiRoads");
  io->AddNode("dMuiRoadClusterRel");
  io->AddNode("dMutSortedAnodes");
  io->AddNode("dMuoTracksOut");
  io->AddNode("dMuoTracks");
  io->AddNode("dMuoTrackRoadRel");
  io->AddNode("emcClusterContainer");
  io->AddNode("emcTowerContainer");
  io->AddNode("EventHeader");
  io->AddNode("ErtOut");
  io->AddNode("fclRawNorth");
  io->AddNode("fclRawSouth");
  io->AddNode("KalFitOut");
  io->AddNode("NtcpRaw");
  io->AddNode("Pc1Cluster");
  io->AddNode("Pc2Cluster");
  io->AddNode("Pc3Cluster");
  io->AddNode("PHTrackOut");
  io->AddNode("PHTrackOutBack");
  io->AddNode("PHDchTrackOut");
  io->AddNode("Sync");
  io->AddNode("T0Out");
  io->AddNode("TecOut");
  io->AddNode("TofOut");
  io->AddNode("TrigLvl1");
  io->AddNode("VtxOut");
  io->AddNode("ZdcOut");
  io->AddNode("ZdcRaw");
  return;
}
