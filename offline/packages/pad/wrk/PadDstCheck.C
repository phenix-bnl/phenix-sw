void PadDstCheck(Int_t maxEvents=200, const Float_t sigmaDiff=3.0, 
		 const Float_t fractionalDiff = 0.02, const char *dstIFile="dstData.root") {
  // *****************************Beginning of Header Description*********************************
  //
  // PadDstCheck macro for self-consistency check among PC1 and PC3 sectors
  // Author:  Charles F. Maguire
  // Creation Date: November 25, 2000
  //
  // Input arguments
  //   maxEvents = total number of events requested from DST file
  //   sigmaDiff = maximum sigma difference allowed (e.g. 3.0 = 99.7% confidence level)
  //   fractionalDiff = mininum fractional difference which must be exceeded to produce an error flag
  //   dstIFile = name of DST input file
  //
  // Output results
  //   pc1Status = 0 for normal, > 0 for abnormal  (checks PC1 East and West consistency)
  //   pc3Status = 0 for normal, > 0 for abnormal  (checks PC3 East against PC1 E+W average)
  //   pc1EastSatus = 0 for normal, > 0 for abnormal (sector-by-sector consistency in PC1 East)
  //   pc1WestSatus = 0 for normal, > 0 for abnormal (sector-by-sector consistency in PC1 West)
  //   pc3EastSatus = 0 for normal, > 0 for abnormal (sector-by-sector consistency in PC3 East)
  //
  //  Example error conditions
  //   pc1Status = 1 means the East and West PC1 sums differ by more than
  //               sigmaDiff*(sqrt(PC1 East sum) + sqrt(PC1 West sum)) +
  //               fractionalDiff*0.5*(PC1 East sum + PC1 West sum)  [only if fractionalDiff > 0]
  //
  //   pc1Status = 2, 4, or 6 means that PC1 East, West, or both, have 0 counts
  //
  //   pc1EastStatus = abcdefghI is a bit pattern printed out when pc1EastStatus > 0
  //                   I = pc1Status
  //                   a = sector 8 status (1 = North Error, 2 = South Error, 3 = both)
  //                   b = sector 7 status (1 = North Error, 2 = South Error, 3 = both)
  //                   c to h are for sectors 6 to 1
  //
  //  Example output from Run 11014 (bad for PCs, major problem with PC1 East HV)
  /*

  Run number 11014,   Events analyzed 200

 PC1 totals: mean = 34.43,  North = 16.43,  South = 18,  East = 20.45,  West = 13.98
   NorthEast: 1.335  1.395  1.755  1.565  0.96  0.885  0.885  1.025
   SouthEast: 1.635  1.56  1.68  1.41  1.095  1.02  1.07  1.175
   NorthWest: 0.83  0.775  0.925  0.74  0.9  0.79  0.805  0.86
   SouthWest: 0.97  0.97  1.005  0.9  0.845  0.89  0.93  0.845

  Difference PC1 West - PC1 East is too large:  PC1 West = 2796  PC1 East = 4090

  Difference PC1 Half-Total - PC3 East is too large:  PC1 Half-Total = 3443  PC3 East = 5429

 PC1 NorthEast Sector #3 - PC1 East Sector Average is too large:
  PC1 NorthEast Sector #3 351,   PC1 East Sector Average 255.625

 PC1 NorthEast Sector #5 - PC1 East Sector Average is too large:
  PC1 NorthEast Sector #5 192,   PC1 East Sector Average 255.625

 PC1 NorthEast Sector #6 - PC1 East Sector Average is too large:
  PC1 NorthEast Sector #6 177,   PC1 East Sector Average 255.625

 PC1 NorthEast Sector #7 - PC1 East Sector Average is too large:
  PC1 NorthEast Sector #7 177,   PC1 East Sector Average 255.625

 PC1 SouthEast Sector #3 - PC1 East Sector Average is too large:
  PC1 SouthEast Sector #3 336,   PC1 East Sector Average 255.625

 PC3 totals: mean = 27.145,  North = 12.65,  South = 14.495
   NorthEast: 2.865  3.475  3.12  3.19
   SouthEast: 3.515  3.56281  3.475  3.48

 Pad Chamber Totals Status Flags:  PC1 Status 1,  PC3 Status 1  PC1 abnormal  PC3 abnormal
 PC1 sector check has abnormal status flag    East sectors flag = 011103001   West sectors flag = 000000001

  */

  // Histograms are written to paddstcheck.root output file
  // Useful visual 2D displays are pc1yvsx, pc3yvsx, pc1eastthph, pc1westthph, pc3eastthph

  //
  // Revision History
  //

  // *****************************End of Header Description*********************************
  

  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libdgo.so");
  gSystem->Load("libphgeo.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea.so");

  gSystem->Load("libEG.so") ;
  gSystem->Load("libEmcDynamic.so") ;
  gSystem->Load("libemcCalib.so") ;
  gSystem->Load("libemcOM.so") ;
  
// Loading subsystem libraries
  gSystem->Load("liblv1.so");
  gSystem->Load("libbbc.so");
  gSystem->Load("libzdc.so");
  gSystem->Load("libmvd.so");
  gSystem->Load("libpad.so");
  gSystem->Load("libemc.so");
  gSystem->Load("libtof.so");
  gSystem->Load("libcgl.so");
  gSystem->Load("libdch.so");
  gSystem->Load("libcrk.so");
  gSystem->Load("libtec.so");
  gSystem->Load("libmom.so");
  gSystem->Load("libRawDataCheck.so");
  gSystem->Load("libheader.so");

  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  // Set up input and output files
  PHString dstInFile = dstIFile;
  PHNodeIOManager* dstIn = new PHNodeIOManager(dstInFile,PHReadOnly);
  dstIn->read(dstNode);


  // Initialize the tables

  PHIODataNode<PHTable>* dBbcOutNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dBbcOut");
  PHIODataNode<PHTable>* dBbcRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dBbcRaw");
  PHIODataNode<PHTable>* dZdcRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dZdcRaw");
  PHIODataNode<PHTable>* dZdcOutNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dZdcOut");
  PHIODataNode<PHTable>* dMvddNdEtaOutNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvddNdEtaOut");
  PHIODataNode<PHTable>* dMvdMultOutNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvdMultOut");
  PHIODataNode<PHTable>* dMvdVertexOutNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvdVertexOut");
  PHIODataNode<PHTable>* dMvbRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvbRaw");
  PHIODataNode<PHTable>* dMvcRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvcRaw");
  PHIODataNode<PHTable>* dDchHitNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dDchHit");
  PHIODataNode<PHTable>* dDchTracksNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dDchTracks");
  PHIODataNode<PHTable>* dPc1RawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc1Raw");
  PHIODataNode<PHTable>* dPc2RawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc2Raw");
  PHIODataNode<PHTable>* dPc3RawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc3Raw");
  PHIODataNode<PHTable>* dPc1ClusterNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc1Cluster");
  PHIODataNode<PHTable>* dPc2ClusterNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc2Cluster");
  PHIODataNode<PHTable>* dPc3ClusterNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc3Cluster");
  PHIODataNode<PHTable>* dCrkRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCrkRaw");
  PHIODataNode<PHTable>* dCrkHitNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCrkHit");
  PHIODataNode<PHTable>* dCrkPidNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCrkPid");
  PHIODataNode<PHTable>* dTecTrackNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTecTrack");
  PHIODataNode<PHTable>* dTecCalibNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTecCalib");
  PHIODataNode<PHTable>* dTecPIDNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTecPID");
  PHIODataNode<PHTable>* dTofRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTofRaw");
  PHIODataNode<PHTable>* dTofReconstructedNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTofReconstructed");
  PHIODataNode<PHTable>* dEmcCalibTowerNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dEmcCalibTower");
  PHIODataNode<PHTable>* dEmcClusterLocalNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dEmcClusterLocal");
  PHIODataNode<PHTable>* dEmcClusterLocalExtNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dEmcClusterLocalExt");
  PHIODataNode<PHTable>* dCglTrackNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCglTrack");
  PHIODataNode<PHTable>* dCglParticleNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCglParticle");
  PHIODataNode<PHTable>* dCglPidNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCglPid");
  PHIODataNode<PHTable>* dRunHeaderNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dRunHeader");
  PHIODataNode<PHTable>* dEventHeaderNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dEventHeader");
  
  PhRootHistogramFactory::buildFactory();

  // Book the DST checking histograms for the Pad Chamber

  Char_t padOutFile[30];
  sprintf(padOutFile,"paddstcheck.root");
  padhistfile = new TFile(padOutFile,"RECREATE","myDST");

  
  //************************************************************
  // PAD DST Check Booking Macro
  //************************************************************

  const Float_t DEGRAD = 57.295779513;

  //
  // PC1 summary
  //
  TH1F *pc1clustermult = new TH1F("pc1clustermult","PC1 clusters/event",100,0.0,800.0);
  TH1F *pc1clustersouth = new TH1F("pc1clustersouth","PC1 south clusters/event",100,0.0,400.0);
  TH1F *pc1clusternorth = new TH1F("pc1clusternorth","PC1 north clusters/event",100,0.0,400.0);
 
  TH2F *pc1yvsx = new TH2F("pc1yvsx","PC1 y vs x",200,-260.0,+260.0,200,-200.0,+250.0);
  pc1yvsx->SetMarkerStyle(4);
  pc1yvsx->SetMarkerSize(0.2);

  TH2F *pc1eastthph = new TH2F("pc1eastthph","PC1East Phi vs Theta",
			       100, 65., 115., 240, 120.0, 240.0);
  pc1eastthph->SetMarkerStyle(4);
  pc1eastthph->SetMarkerSize(0.2);

  TH2F *pc1westthph = new TH2F("pc1westthph","PC1West Phi vs Theta",
			       100, 65., 115., 240, -40.0, 80.0);
  pc1westthph->SetMarkerStyle(4);
  pc1westthph->SetMarkerSize(0.2);

  //
  // Now do the East Arm for PC1
  //
 
  TH1F *pc1eastclustermult = new TH1F("pc1eastclustermult","PC1EAST clusters/event",100,0.0,400.0);
  TH1F *pc1eastclustersouth = new TH1F("pc1eastclustersouth","PC1EAST south clusters/event",200,0.0,100.0);
  TH1F *pc1eastclusternorth = new TH1F("pc1eastclusternorth","PC1EAST north clusters/event",200,0.0,100.0);

  TH1F *pc1eastclustersec1s = new TH1F("pc1eastclustersec1s","PC1EAST sect 1S clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec1n = new TH1F("pc1eastclustersec1n","PC1EAST sect 1N clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec2s = new TH1F("pc1eastclustersec2s","PC1EAST sect 2S clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec2n = new TH1F("pc1eastclustersec2n","PC1EAST sect 2N clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec3s = new TH1F("pc1eastclustersec3s","PC1EAST sect 3S clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec3n = new TH1F("pc1eastclustersec3n","PC1EAST sect 3N clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec4s = new TH1F("pc1eastclustersec4s","PC1EAST sect 4S clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec4n = new TH1F("pc1eastclustersec4n","PC1EAST sect 4N clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec5s = new TH1F("pc1eastclustersec5s","PC1EAST sect 5S clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec5n = new TH1F("pc1eastclustersec5n","PC1EAST sect 5N clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec6s = new TH1F("pc1eastclustersec6s","PC1EAST sect 6S clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec6n = new TH1F("pc1eastclustersec6n","PC1EAST sect 6N clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec7s = new TH1F("pc1eastclustersec7s","PC1EAST sect 7S clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec7n = new TH1F("pc1eastclustersec7n","PC1EAST sect 7N clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec8s = new TH1F("pc1eastclustersec8s","PC1EAST sect 8S clusters/event",50,0.0,50.0);
  TH1F *pc1eastclustersec8n = new TH1F("pc1eastclustersec8n","PC1EAST sect 8N clusters/event",50,0.0,50.0);

  //
  // Now do the West arm for PC1
  //

  TH1F *pc1westclustermult = new TH1F("pc1westclustermult","PC1WEST clusters/event",100,0.0,400.0);
  TH1F *pc1westclustersouth = new TH1F("pc1westclustersouth","PC1WEST south clusters/event",200,0.0,100.0);
  TH1F *pc1westclusternorth = new TH1F("pc1westclusternorth","PC1WEST north clusters/event",200,0.0,100.0);

  TH1F *pc1westclustersec1s = new TH1F("pc1westclustersec1s","PC1WEST sect 1S clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec1n = new TH1F("pc1westclustersec1n","PC1WEST sect 1N clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec2s = new TH1F("pc1westclustersec2s","PC1WEST sect 2S clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec2n = new TH1F("pc1westclustersec2n","PC1WEST sect 2N clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec3s = new TH1F("pc1westclustersec3s","PC1WEST sect 3S clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec3n = new TH1F("pc1westclustersec3n","PC1WEST sect 3N clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec4s = new TH1F("pc1westclustersec4s","PC1WEST sect 4S clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec4n = new TH1F("pc1westclustersec4n","PC1WEST sect 4N clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec5s = new TH1F("pc1westclustersec5s","PC1WEST sect 5S clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec5n = new TH1F("pc1westclustersec5n","PC1WEST sect 5N clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec6s = new TH1F("pc1westclustersec6s","PC1WEST sect 6S clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec6n = new TH1F("pc1westclustersec6n","PC1WEST sect 6N clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec7s = new TH1F("pc1westclustersec7s","PC1WEST sect 7S clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec7n = new TH1F("pc1westclustersec7n","PC1WEST sect 7N clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec8s = new TH1F("pc1westclustersec8s","PC1WEST sect 8S clusters/event",50,0.0,50.0);
  TH1F *pc1westclustersec8n = new TH1F("pc1westclustersec8n","PC1WEST sect 8N clusters/event",50,0.0,50.0);

  //
  // PC3 summary
  //
  TH1F *pc3clustermult = new TH1F("pc3clustermult","PC3 clusters/event",100,0.0,800.0);
  TH1F *pc3clustersouth = new TH1F("pc3clustersouth","PC3 south clusters/event",100,0.0,400.0);
  TH1F *pc3clusternorth = new TH1F("pc3clusternorth","PC3 north clusters/event",100,0.0,400.0);
 
  TH2F *pc3yvsx = new TH2F("pc3yvsx","PC3 y vs x",200,-550., 0.0,200,-400.0,+500.0);
  pc3yvsx->SetMarkerStyle(4);
  pc3yvsx->SetMarkerSize(0.2);

  TH2F *pc3eastthph = new TH2F("pc3eastthph","PC3East Phi vs Theta",
			       100, 65., 115., 240, 120.0, 240.0);
  pc3eastthph->SetMarkerStyle(4);
  pc3eastthph->SetMarkerSize(0.2);


  //
  // Now do the East Arm for PC3
  //

  TH1F *pc3eastclustermult = new TH1F("pc3eastclustermult","PC3EAST clusters/event",100,0.0,400.0);
  TH1F *pc3eastclustersouth = new TH1F("pc3eastclustersouth","PC3EAST south clusters/event",200,0.0,100.0);
  TH1F *pc3eastclusternorth = new TH1F("pc3eastclusternorth","PC3EAST north clusters/event",200,0.0,100.0);

  TH1F *pc3eastclustersec1s = new TH1F("pc3eastclustersec1s","PC3EAST sect 1S clusters/event",50,0.0,50.0);
  TH1F *pc3eastclustersec1n = new TH1F("pc3eastclustersec1n","PC3EAST sect 1N clusters/event",50,0.0,50.0);
  TH1F *pc3eastclustersec2s = new TH1F("pc3eastclustersec2s","PC3EAST sect 2S clusters/event",50,0.0,50.0);
  TH1F *pc3eastclustersec2n = new TH1F("pc3eastclustersec2n","PC3EAST sect 2N clusters/event",50,0.0,50.0);
  TH1F *pc3eastclustersec3s = new TH1F("pc3eastclustersec3s","PC3EAST sect 3S clusters/event",50,0.0,50.0);
  TH1F *pc3eastclustersec3n = new TH1F("pc3eastclustersec3n","PC3EAST sect 3N clusters/event",50,0.0,50.0);
  TH1F *pc3eastclustersec4s = new TH1F("pc3eastclustersec4s","PC3EAST sect 4S clusters/event",50,0.0,50.0);
  TH1F *pc3eastclustersec4n = new TH1F("pc3eastclustersec4n","PC3EAST sect 4N clusters/event",50,0.0,50.0);

  // Time for the event loop
  Int_t eventNumber = 1;
  while (eventNumber <= maxEvents) {

    if (eventNumber >= 1) {
      if(!dstIn->read(dstNode)){
        cout << " end of file "<< endl;
	break;
      }  // check on valid dstIn return
    } // check on eventNumber >= 1

   if(eventNumber%100 == 1)
     cout << "\n Processing event " << eventNumber << endl;

    // Grab the data nodes

    if (!dBbcOutNode) {
      cout << "Dst Read: Could not find data node dBbcOut" << endl;
    } else {
      dBbcOutWrapper* dBbcOut = (dBbcOutWrapper*)dBbcOutNode->getData();
    }
    if (!dBbcRawNode) {
      cout << "Dst Read: Could not find data node dBbcRaw" << endl;
    } else {
      dBbcRawWrapper* dBbcRaw = (dBbcRawWrapper*)dBbcRawNode->getData();
    }
    if (!dMvddNdEtaOutNode) {
      cout << "Dst Read: Could not find data node dMvddNdEtaOut" << endl;
    } else {
      dMvddNdEtaOutWrapper* dMvddNdEtaOut = (dMvddNdEtaOutWrapper*)dMvddNdEtaOutNode->getData();
    }
    if (!dMvdMultOutNode) {
      cout << "Dst Read: Could not find data node dMvdMultOut" << endl;
    } else {
      dMvdMultOutWrapper* dMvdMultOut = (dMvdMultOutWrapper*)dMvdMultOutNode->getData();
    }
    if (!dMvdVertexOutNode) {
      cout << "Dst Read: Could not find data node dMvdVertexOut" << endl;
    } else {
      dMvdVertexOutWrapper* dMvdVertexOut = (dMvdVertexOutWrapper*)dMvdVertexOut Node->getData();
    }
    if (!dMvbRawNode) {
      cout << "Dst Read: Could not find data node dMvbRaw" << endl;
    } else {
      dMvbRawWrapper* dMvbRaw = (dMvbRawWrapper*)dMvbRawNode->getData();

    }
    if (!dMvcRawNode) {
      cout << "Dst Read: Could not find data node dMvcRaw" << endl;
    } else {
      dMvcRawWrapper* dMvcRaw = (dMvcRawWrapper*)dMvcRawNode->getData();
    }

    if (!dDchHitNode) {
      cout << "Dst Read: Could not find data node dDchHit" << endl;
    } else {
      dDchHitWrapper* dDchHit = (dDchHitWrapper*)dDchHitNode->getData();
    }
    if (!dDchTracksNode) {
      cout << "Dst Read: Could not find data node dDchTracks" << endl;
    } else {
      dDchTracksWrapper* dDchTracks = (dDchTracksWrapper*)dDchTracksNode->getData();
    }

    if (!dPc1RawNode) {
      cout << "Dst Read: Could not find data node dPc1Raw" << endl;
    } else {
      dPadRawWrapper* dPc1Raw = (dPadRawWrapper*)dPc1RawNode->getData();
    }
    if (!dPc2RawNode) {
      cout << "Dst Read: Could not find data node dPc2Raw" << endl;
    } else {

      dPadRawWrapper* dPc2Raw = (dPadRawWrapper*)dPc2RawNode->getData();
    }
    if (!dPc3RawNode) {
      cout << "Dst Read: Could not find data node dPc3Raw" << endl;
    } else {
      dPadRawWrapper* dPc3Raw = (dPadRawWrapper*)dPc3RawNode->getData();
    }

    if (!dPc1ClusterNode) {
      cout << "Dst Read: Could not find data node dPc1Cluster" << endl;
    } else {
      dPadClusterWrapper* dPc1Cluster = (dPadClusterWrapper*)dPc1ClusterNode->getData();
    }
    if (!dPc2ClusterNode) {
      cout << "Dst Read: Could not find data node dPc2Cluster" << endl;
    } else {
      dPadClusterWrapper* dPc2Cluster = (dPadClusterWrapper*)dPc2ClusterNode->getData();
    }
    if (!dPc3ClusterNode) {
      cout << "Dst Read: Could not find data node dPc3Cluster" << endl;
    } else {
      dPadClusterWrapper* dPc3Cluster = (dPadClusterWrapper*)dPc3ClusterNode->getData();
    }
    if (!dCrkHitNode) {
      cout << "Dst Read: Could not find data node dCrkHit" << endl;
    } else {
      dCrkHitWrapper* dCrkHit = (dCrkHitWrapper*)dCrkHitNode->getData();
    }
    if (!dCrkRawNode) {
      cout << "Dst Read: Could not find data node dCrkRaw" << endl;
    } else {
      dCrkRawWrapper* dCrkRaw = (dCrkRawWrapper*)dCrkRawNode->getData();
    }
    if (!dCrkPidNode) {
      cout << "Dst Read: Could not find data node dCrkPid" << endl;
    } else {
      dCrkPidWrapper* dCrkPid = (dCrkPidWrapper*)dCrkPidNode->getData();
    }
    if (!dTecTrackNode) {
      cout << "Dst Read: Could not find data node dTecTrack" << endl;
    } else {
      dTecTrackWrapper* dTecTrack = (dTecTrackWrapper*)dTecTrackNode->getData();
    }
    if (!dTecPIDNode) {
      cout << "Dst Read: Could not find data node dTecPID" << endl;
    } else {
      dTecPIDWrapper* dTecPID = (dTecPIDWrapper*)dTecPIDNode->getData();
    }
    if (!dTofReconstructedNode) {
      cout << "Dst Read: Could not find data node dTofReconstructed" << endl;
    } else {
      dTofReconstructedWrapper* dTofReconstructed = (dTofReconstructedWrapper*)dTofReconstructedNode->getData();
    }

    if (!dEmcClusterLocalNode) {
      cout << "Dst Read: Could not find data node dEmcClusterLocal" << endl;
    } else {
      dEmcClusterLocalWrapper* dEmcClusterLocal = (dEmcClusterLocalWrapper*)dEmcClusterLocalNode->getData();
    }
    if (!dEmcClusterLocalExtNode) {
      cout << "Dst Read: Could not find data node dEmcClusterLocalExt" << endl;
    } else {
      dEmcClusterLocalExtWrapper* dEmcClusterLocalExt = (dEmcClusterLocalExtWrapper*)dEmcClusterLocalExtNode->getData();
    }
    if (!dCglTrackNode) {
      cout << "Dst Read: Could not find data node dCglTrack" << endl;
    } else {
      dCglTrackWrapper* dCglTrack = (dCglTrackWrapper*)dCglTrackNode->getData();
    }
    if (!dCglParticleNode) {
      cout << "Dst Read: Could not find data node dCglParticle" << endl;
    } else {
      dCglParticleWrapper* dCglParticle = (dCglParticleWrapper*)dCglParticleNode->getData();
    }
    if (!dCglPidNode) {
      cout << "Dst Read: Could not find data node dCglPid" << endl;
    } else {
      dCglPidWrapper* dCglPid = (dCglPidWrapper*)dCglPidNode->getData();
    }

    if (!dRunHeaderNode) {
      cout << "Dst Read: Could not find data node dRunHeader" << endl;
    } else {
      dRunHeaderWrapper* dRunHeader = (dRunHeaderWrapper*)dRunHeaderNode->getData();
    }
    if (!dEventHeaderNode) {
      cout << "Dst Read: Could not find data node dEventHeader" << endl;
    } else {
      dEventHeaderWrapper* dEventHeader = (dEventHeaderWrapper*)dEventHeaderNode->getData();
    }


   Int_t runNumber   = dRunHeader->get_run(0);

   Int_t nPc1 = dPc1Cluster->RowCount(); // total number of PC1 clusters
   Int_t nPc1South = 0;
   Int_t nPc1North = 0;
   Int_t nPc1East = 0;
   Int_t nPc1West = 0;

   Int_t nPc1NorthEastSum = 0;
   Int_t nPc1SouthEastSum = 0;
   Int_t nPc1NorthWestSum = 0;
   Int_t nPc1SouthWestSum = 0;

   Int_t nPc1NorthEast[8] = {0, 0, 0, 0, 0, 0, 0, 0};
   Int_t nPc1SouthEast[8] = {0, 0, 0, 0, 0, 0, 0, 0};
   Int_t nPc1NorthWest[8] = {0, 0, 0, 0, 0, 0, 0, 0};
   Int_t nPc1SouthWest[8] = {0, 0, 0, 0, 0, 0, 0, 0};

   pc1clustermult->Fill(nPc1);

   //
   // Loop over PC1 clusters
   for(Int_t iPc1=0; iPc1<nPc1; iPc1++) {

     Float_t xPc1 = dPc1Cluster->get_xyz(0,iPc1);
     Float_t yPc1 = dPc1Cluster->get_xyz(1,iPc1);
     Float_t zPc1 = dPc1Cluster->get_xyz(2,iPc1);
     Int_t iSector = dPc1Cluster->get_sector(iPc1);

     pc1yvsx->Fill(xPc1,yPc1);

     Float_t phiTemp =  DEGRAD*atan2(yPc1,xPc1);
     if(phiTemp < -90.0)
       phiTemp = 360.0 + phiTemp;

     Float_t thetaTemp = DEGRAD*acos(zPc1/sqrt(xPc1*xPc1 +
				     yPc1*yPc1 + zPc1*zPc1));

     if(zPc1 < 0)
       nPc1South++;
     else
       nPc1North++;

     if(xPc1 < 0) {
       nPc1East++;
       pc1eastthph->Fill(thetaTemp, phiTemp);
       if(zPc1 < 0) {
	 nPc1SouthEastSum++;
	 nPc1SouthEast[iSector]++;
       }
       else {
	 nPc1NorthEastSum++;
	 nPc1NorthEast[iSector]++;
       }  // check for North or South Arm
     }
     else {
       nPc1West++;
       pc1westthph->Fill(thetaTemp, phiTemp);
       if(zPc1 < 0) {
	 nPc1SouthWestSum++;
	 nPc1SouthWest[iSector]++;
       }
       else {
	 nPc1NorthWestSum++;
	 nPc1NorthWest[iSector]++;
       }  // check for North or South Arm
     } // check for East or West Arm

   } // loop over PC1 clusters

   pc1clustersouth->Fill(nPc1South);
   pc1clusternorth->Fill(nPc1North);

   pc1eastclustermult->Fill(nPc1East);
   pc1eastclustersouth->Fill(nPc1SouthEastSum);
   pc1eastclusternorth->Fill(nPc1NorthEastSum);

   pc1eastclustersec1s->Fill(nPc1SouthEast[0]);
   pc1eastclustersec2s->Fill(nPc1SouthEast[1]);
   pc1eastclustersec3s->Fill(nPc1SouthEast[2]);
   pc1eastclustersec4s->Fill(nPc1SouthEast[3]);
   pc1eastclustersec5s->Fill(nPc1SouthEast[4]);
   pc1eastclustersec6s->Fill(nPc1SouthEast[5]);
   pc1eastclustersec7s->Fill(nPc1SouthEast[6]);
   pc1eastclustersec8s->Fill(nPc1SouthEast[7]);

   pc1eastclustersec1n->Fill(nPc1NorthEast[0]);
   pc1eastclustersec2n->Fill(nPc1NorthEast[1]);
   pc1eastclustersec3n->Fill(nPc1NorthEast[2]);
   pc1eastclustersec4n->Fill(nPc1NorthEast[3]);
   pc1eastclustersec5n->Fill(nPc1NorthEast[4]);
   pc1eastclustersec6n->Fill(nPc1NorthEast[5]);
   pc1eastclustersec7n->Fill(nPc1NorthEast[6]);
   pc1eastclustersec8n->Fill(nPc1NorthEast[7]);

   pc1westclustermult->Fill(nPc1West);
   pc1westclustersouth->Fill(nPc1SouthWestSum);
   pc1westclusternorth->Fill(nPc1NorthWestSum);

   pc1westclustersec1s->Fill(nPc1SouthWest[0]);
   pc1westclustersec2s->Fill(nPc1SouthWest[1]);
   pc1westclustersec3s->Fill(nPc1SouthWest[2]);
   pc1westclustersec4s->Fill(nPc1SouthWest[3]);
   pc1westclustersec5s->Fill(nPc1SouthWest[4]);
   pc1westclustersec6s->Fill(nPc1SouthWest[5]);
   pc1westclustersec7s->Fill(nPc1SouthWest[6]);
   pc1westclustersec8s->Fill(nPc1SouthWest[7]);

   pc1westclustersec1n->Fill(nPc1NorthWest[0]);
   pc1westclustersec2n->Fill(nPc1NorthWest[1]);
   pc1westclustersec3n->Fill(nPc1NorthWest[2]);
   pc1westclustersec4n->Fill(nPc1NorthWest[3]);
   pc1westclustersec5n->Fill(nPc1NorthWest[4]);
   pc1westclustersec6n->Fill(nPc1NorthWest[5]);
   pc1westclustersec7n->Fill(nPc1NorthWest[6]);
   pc1westclustersec8n->Fill(nPc1NorthWest[7]);

   Int_t nPc3 = dPc3Cluster->RowCount(); // total number of PC3 clusters
   Int_t nPc3South = 0;
   Int_t nPc3North = 0;
   Int_t nPc3East = 0;

   Int_t nPc3NorthEastSum = 0;
   Int_t nPc3SouthEastSum = 0;

   Int_t nPc3NorthEast[4] = {0, 0, 0, 0};
   Int_t nPc3SouthEast[4] = {0, 0, 0, 0};

   pc3clustermult->Fill(nPc3);

   //
   // Loop over PC3 clusters
   for(Int_t iPc3=0; iPc3<nPc3; iPc3++) {

     Float_t xPc3 = dPc3Cluster->get_xyz(0,iPc3);
     Float_t yPc3 = dPc3Cluster->get_xyz(1,iPc3);
     Float_t zPc3 = dPc3Cluster->get_xyz(2,iPc3);
     Int_t iSector = dPc3Cluster->get_sector(iPc3);

     pc3yvsx->Fill(xPc3,yPc3);

     Float_t phiTemp =  DEGRAD*atan2(yPc3,xPc3);
     if(phiTemp < -90.0)
       phiTemp = 360.0 + phiTemp;

     Float_t thetaTemp = DEGRAD*acos(zPc3/sqrt(xPc3*xPc3 +
				     yPc3*yPc3 + zPc3*zPc3));

     if(zPc3 < 0)
       nPc3South++;
     else
       nPc3North++;

     if(xPc3 < 0) {
       nPc3East++;
       pc3eastthph->Fill(thetaTemp, phiTemp);
       if(zPc3 < 0) {
	 nPc3SouthEastSum++;
	 nPc3SouthEast[iSector]++;
       }
       else {
	 nPc3NorthEastSum++;
	 nPc3NorthEast[iSector]++;
       }  // check for North or South Arm
     }
     else {
       cout << "\n Error  PC3 X = " << xPc3;
       cout << "  at event = " << eventNumber;
       cout << endl;
       break;  // skip event
     }
   } // loop over PC3 clusters

   pc3clustersouth->Fill(nPc3South);
   pc3clusternorth->Fill(nPc3North);

   pc3eastclustermult->Fill(nPc3East);
   pc3eastclustersouth->Fill(nPc3SouthEastSum);
   pc3eastclusternorth->Fill(nPc3NorthEastSum);

   pc3eastclustersec1s->Fill(nPc3SouthEast[0]);
   pc3eastclustersec2s->Fill(nPc3SouthEast[1]);
   pc3eastclustersec3s->Fill(nPc3SouthEast[2]);
   pc3eastclustersec4s->Fill(nPc3SouthEast[3]);

   pc3eastclustersec1n->Fill(nPc3NorthEast[0]);
   pc3eastclustersec2n->Fill(nPc3NorthEast[1]);
   pc3eastclustersec3n->Fill(nPc3NorthEast[2]);
   pc3eastclustersec4n->Fill(nPc3NorthEast[3]);

   eventNumber++;
  }
  // end event loop

  Int_t trueEvents = eventNumber - 1;
  cout << "\n  Run number " << runNumber;
  cout << ",   Events analyzed " << trueEvents << endl;

  cout << "\n PC1 totals: mean = " << pc1clustermult->GetMean();
  cout << ",  North = " << pc1clusternorth->GetMean();
  cout << ",  South = " << pc1clustersouth->GetMean();
  cout << ",  East = " << pc1eastclustermult->GetMean();
  cout << ",  West = " << pc1westclustermult->GetMean();
  cout << endl;

  cout << "   NorthEast: " <<  pc1eastclustersec1n->GetMean();
  cout << "  " << pc1eastclustersec2n->GetMean();
  cout << "  " << pc1eastclustersec3n->GetMean();
  cout << "  " << pc1eastclustersec4n->GetMean();
  cout << "  " << pc1eastclustersec5n->GetMean();
  cout << "  " << pc1eastclustersec6n->GetMean();
  cout << "  " << pc1eastclustersec7n->GetMean();
  cout << "  " << pc1eastclustersec8n->GetMean();
  cout << endl;
  cout << "   SouthEast: " <<  pc1eastclustersec1s->GetMean();
  cout << "  " << pc1eastclustersec2s->GetMean();
  cout << "  " << pc1eastclustersec3s->GetMean();
  cout << "  " << pc1eastclustersec4s->GetMean();
  cout << "  " << pc1eastclustersec5s->GetMean();
  cout << "  " << pc1eastclustersec6s->GetMean();
  cout << "  " << pc1eastclustersec7s->GetMean();
  cout << "  " << pc1eastclustersec8s->GetMean();
  cout << endl;
  cout << "   NorthWest: " <<  pc1westclustersec1n->GetMean();
  cout << "  " << pc1westclustersec2n->GetMean();
  cout << "  " << pc1westclustersec3n->GetMean();
  cout << "  " << pc1westclustersec4n->GetMean();
  cout << "  " << pc1westclustersec5n->GetMean();
  cout << "  " << pc1westclustersec6n->GetMean();
  cout << "  " << pc1westclustersec7n->GetMean();
  cout << "  " << pc1westclustersec8n->GetMean();
  cout << endl;
  cout << "   SouthWest: " <<  pc1westclustersec1s->GetMean();
  cout << "  " << pc1westclustersec2s->GetMean();
  cout << "  " << pc1westclustersec3s->GetMean();
  cout << "  " << pc1westclustersec4s->GetMean();
  cout << "  " << pc1westclustersec5s->GetMean();
  cout << "  " << pc1westclustersec6s->GetMean();
  cout << "  " << pc1westclustersec7s->GetMean();
  cout << "  " << pc1westclustersec8s->GetMean();
  cout << endl;

  Float_t pc1Status = 0;
  Float_t pc3Status = 0;
 
  Float_t pc1EastTotal = trueEvents*pc1eastclustermult->GetMean();
  Float_t pc1WestTotal = trueEvents*pc1westclustermult->GetMean();
  Float_t pc3EastTotal = trueEvents*pc3eastclustermult->GetMean();

  Float_t pc1EastSigma = 0.0;
  if(pc1EastTotal <= 0.0) {
    cout << "\n Bad value for PC1 East Total = " << pc1EastTotal << endl;
    pc1Status += 2;
  }
  else
    pc1EastSigma = sqrt(pc1EastTotal);

  Float_t pc1WestSigma = 0.0;
  if(pc1WestTotal <= 0.0) {
    cout << "\n Bad value for PC1 West Total = " << pc1WestTotal << endl;
    pc1Status += 4;
  }
  else
    pc1WestSigma = sqrt(pc1WestTotal);

  Float_t pc3EastSigma = 0.0;
  if(pc3EastTotal <= 0.0) {
    cout << "\n Bad value for PC3 East Total = " << pc3EastTotal << endl;
    pc3Status += 2;
  }
  else
    pc3EastSigma = sqrt(pc3EastTotal);

  const Float_t pc1EastRenorm = 1.0; // from Run 10527 and Run 12431
  if(abs(pc1WestTotal - pc1EastRenorm*pc1EastTotal) > sigmaDiff*(pc1WestSigma + pc1EastSigma) +
     fractionalDiff*0.5*(pc1WestTotal + pc1EastTotal)) {
    pc1Status += 1;
    cout << "\n  Difference PC1 West - PC1 East is too large:";
    cout << "  PC1 West = " << pc1WestTotal;
    cout << "  PC1 East = " << pc1EastTotal;
    cout << endl;
  }

  Float_t pc1Total = pc1EastTotal + pc1WestTotal;
  Float_t pc1Sigma = 0.0;
  if(pc1Total > 0)
    pc1Sigma = sqrt(pc1Total);

  const Float_t pc3EastRenorm = 1.146; // from Run 10527
  if(abs(0.5*pc1Total - pc3EastRenorm*pc3EastTotal) > sigmaDiff*(0.5*pc1Sigma + pc3EastSigma) +
     fractionalDiff*pc3EastTotal) {
    pc3Status += 1;
    cout << "\n  Difference PC1 Half-Total - PC3 East is too large:";
    cout << "  PC1 Half-Total = " << 0.5*pc1Total;
    cout << "  PC3 East = " << pc3EastTotal;
    cout << endl;
  }

  Float_t pc1EastAverage = pc1EastTotal/16.0;
  Float_t pc1WestAverage = pc1WestTotal/16.0;

  Float_t pc1EastAverageSigma = pc1EastSigma/16.0;
  Float_t pc1WestAverageSigma = pc1WestSigma/16.0;

  Float_t pc3EastAverage = pc3EastTotal/8.0;
  Float_t pc3EastAverageSigma = pc3EastSigma/8.0;

  Float_t pc1EastMinError = fractionalDiff*pc1EastAverage;
  Float_t pc1WestMinError = fractionalDiff*pc1WestAverage;
  Float_t pc3EastMinError = fractionalDiff*pc3EastAverage;

  Float_t pc1NorthEastRenorm[8] = {0.9678, 1.0052, 1.0173, 1.0101, 1.0192, 0.9806, 1.0386, 1.0419};
  Float_t pc1SouthEastRenorm[8] = {0.9670, 0.9616, 0.9972, 1.0298, 1.0369, 0.9865, 0.9915, 0.9512};
  Float_t pc1NorthWestRenorm[8] = {1.0077, 0.9809, 1.0074, 1.0216, 1.0407, 1.0481, 1.0300, 0.9745};
  Float_t pc1SouthWestRenorm[8] = {0.9569, 0.9645, 0.9778, 1.0128, 0.9907, 1.0291, 0.9612, 1.0086};

  Float_t pc3NorthEastRenorm[4] = {1.040, 1.0476, 1.0527, 0.9745};
  Float_t pc3SouthEastRenorm[4] = {0.9341, 0.9571, 0.9692, 0.9911};

  Int_t pc1EastStatus = pc1Status;
  if(abs(pc1NorthEastRenorm[0]*trueEvents*pc1eastclustersec1n->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec1n->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 NorthEast Sector #1 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 NorthEast Sector #1 " << trueEvents*pc1eastclustersec1n->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 10;
  }

  if(abs(pc1NorthEastRenorm[1]*trueEvents*pc1eastclustersec2n->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec2n->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 NorthEast Sector #2 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 NorthEast Sector #2 " << trueEvents*pc1eastclustersec2n->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 100;
  }

  if(abs(pc1NorthEastRenorm[2]*trueEvents*pc1eastclustersec3n->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec3n->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 NorthEast Sector #3 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 NorthEast Sector #3 " << trueEvents*pc1eastclustersec3n->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 1000;
  }

  if(abs(pc1NorthEastRenorm[3]*trueEvents*pc1eastclustersec4n->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec4n->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 NorthEast Sector #4 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 NorthEast Sector #4 " << trueEvents*pc1eastclustersec4n->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 10000;
  }

  if(abs(pc1NorthEastRenorm[4]*trueEvents*pc1eastclustersec5n->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec5n->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 NorthEast Sector #5 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 NorthEast Sector #5 " << trueEvents*pc1eastclustersec5n->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 100000;
  }

  if(abs(pc1NorthEastRenorm[5]*trueEvents*pc1eastclustersec6n->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec6n->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 NorthEast Sector #6 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 NorthEast Sector #6 " << trueEvents*pc1eastclustersec6n->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 1000000;
  }

  if(abs(pc1NorthEastRenorm[6]*trueEvents*pc1eastclustersec7n->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec7n->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 NorthEast Sector #7 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 NorthEast Sector #7 " << trueEvents*pc1eastclustersec7n->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 10000000;
  }

  if(abs(pc1NorthEastRenorm[7]*trueEvents*pc1eastclustersec8n->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec8n->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 NorthEast Sector #8 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 NorthEast Sector #8 " << trueEvents*pc1eastclustersec8n->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 100000000;
  }

  if(abs(pc1SouthEastRenorm[0]*trueEvents*pc1eastclustersec1s->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec1s->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 SouthEast Sector #1 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 SouthEast Sector #1 " << trueEvents*pc1eastclustersec1s->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 20;
  }

  if(abs(pc1SouthEastRenorm[1]*trueEvents*pc1eastclustersec2s->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec2s->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 SouthEast Sector #2 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 SouthEast Sector #2 " << trueEvents*pc1eastclustersec2s->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 200;
  }

  if(abs(pc1SouthEastRenorm[2]*trueEvents*pc1eastclustersec3s->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec3s->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 SouthEast Sector #3 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 SouthEast Sector #3 " << trueEvents*pc1eastclustersec3s->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 2000;
  }

  if(abs(pc1SouthEastRenorm[3]*trueEvents*pc1eastclustersec4s->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec4s->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 SouthEast Sector #4 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 SouthEast Sector #4 " << trueEvents*pc1eastclustersec4s->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 20000;
  }

  if(abs(pc1SouthEastRenorm[4]*trueEvents*pc1eastclustersec5s->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec5s->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 SouthEast Sector #5 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 SouthEast Sector #5 " << trueEvents*pc1eastclustersec5s->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 200000;
  }

  if(abs(pc1SouthEastRenorm[5]*trueEvents*pc1eastclustersec6s->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec6s->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 SouthEast Sector #6 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 SouthEast Sector #6 " << trueEvents*pc1eastclustersec6s->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 2000000;
  }

  if(abs(pc1SouthEastRenorm[6]*trueEvents*pc1eastclustersec7s->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec7s->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 SouthEast Sector #7 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 SouthEast Sector #7 " << trueEvents*pc1eastclustersec7s->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 20000000;
  }

  if(abs(pc1SouthEastRenorm[7]*trueEvents*pc1eastclustersec8s->GetMean() - pc1EastAverage) >
     pc1EastMinError + sigmaDiff*(sqrt(trueEvents*pc1eastclustersec8s->GetMean()) + pc1EastAverageSigma)) {
    cout << "\n PC1 SouthEast Sector #8 - PC1 East Sector Average is too large:";
    cout << "\n  PC1 SouthEast Sector #8 " << trueEvents*pc1eastclustersec8s->GetMean();
    cout << ",   PC1 East Sector Average " << pc1EastAverage;
    cout << endl;
    pc1EastStatus += 200000000;
  }

  Int_t pc1WestStatus = pc1Status;
  if(abs(pc1NorthWestRenorm[0]*trueEvents*pc1westclustersec1n->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec1n->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 NorthWest Sector #1 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 NorthWest Sector #1 " << trueEvents*pc1westclustersec1n->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 10;
  }

  if(abs(pc1NorthWestRenorm[1]*trueEvents*pc1westclustersec2n->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec2n->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 NorthWest Sector #2 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 NorthWest Sector #2 " << trueEvents*pc1westclustersec2n->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 100;
  }

  if(abs(pc1NorthWestRenorm[2]*trueEvents*pc1westclustersec3n->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec3n->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 NorthWest Sector #3 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 NorthWest Sector #3 " << trueEvents*pc1westclustersec3n->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 1000;
  }

  if(abs(pc1NorthWestRenorm[3]*trueEvents*pc1westclustersec4n->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec4n->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 NorthWest Sector #4 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 NorthWest Sector #4 " << trueEvents*pc1westclustersec4n->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 10000;
  }

  if(abs(pc1NorthWestRenorm[4]*trueEvents*pc1westclustersec5n->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec5n->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 NorthWest Sector #5 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 NorthWest Sector #5 " << trueEvents*pc1westclustersec5n->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 100000;
  }

  if(abs(pc1NorthWestRenorm[5]*trueEvents*pc1westclustersec6n->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec6n->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 NorthWest Sector #6 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 NorthWest Sector #6 " << trueEvents*pc1westclustersec6n->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 1000000;
  }

  if(abs(pc1NorthWestRenorm[6]*trueEvents*pc1westclustersec7n->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec7n->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 NorthWest Sector #7 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 NorthWest Sector #7 " << trueEvents*pc1westclustersec7n->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 10000000;
  }

  if(abs(pc1NorthWestRenorm[7]*trueEvents*pc1westclustersec8n->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec8n->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 NorthWest Sector #8 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 NorthWest Sector #8 " << trueEvents*pc1westclustersec8n->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 100000000;
  }

  if(abs(pc1SouthWestRenorm[0]*trueEvents*pc1westclustersec1s->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec1s->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 SouthWest Sector #1 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 SouthWest Sector #1 " << trueEvents*pc1westclustersec1s->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 20;
  }

  if(abs(pc1SouthWestRenorm[1]*trueEvents*pc1westclustersec2s->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec2s->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 SouthWest Sector #2 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 SouthWest Sector #2 " << trueEvents*pc1westclustersec2s->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 200;
  }

  if(abs(pc1SouthWestRenorm[2]*trueEvents*pc1westclustersec3s->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec3s->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 SouthWest Sector #3 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 SouthWest Sector #3 " << trueEvents*pc1westclustersec3s->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 2000;
  }

  if(abs(pc1SouthWestRenorm[3]*trueEvents*pc1westclustersec4s->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec4s->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 SouthWest Sector #4 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 SouthWest Sector #4 " << trueEvents*pc1westclustersec4s->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 20000;
  }

  if(abs(pc1SouthWestRenorm[4]*trueEvents*pc1westclustersec5s->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec5s->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 SouthWest Sector #5 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 SouthWest Sector #5 " << trueEvents*pc1westclustersec5s->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 200000;
  }

  if(abs(pc1SouthWestRenorm[5]*trueEvents*pc1westclustersec6s->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec6s->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 SouthWest Sector #6 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 SouthWest Sector #6 " << trueEvents*pc1westclustersec6s->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 2000000;
  }

  if(abs(pc1SouthWestRenorm[6]*trueEvents*pc1westclustersec7s->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec7s->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 SouthWest Sector #7 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 SouthWest Sector #7 " << trueEvents*pc1westclustersec7s->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 20000000;
  }

  if(abs(pc1SouthWestRenorm[7]*trueEvents*pc1westclustersec8s->GetMean() - pc1WestAverage) >
     pc1WestMinError + sigmaDiff*(sqrt(trueEvents*pc1westclustersec8s->GetMean()) + pc1WestAverageSigma)) {
    cout << "\n PC1 SouthWest Sector #8 - PC1 West Sector Average is too large:";
    cout << "\n  PC1 SouthWest Sector #8 " << trueEvents*pc1westclustersec8s->GetMean();
    cout << ",   PC1 West Sector Average " << pc1WestAverage;
    cout << endl;
    pc1WestStatus += 200000000;
  }

  Int_t pc3EastStatus = pc3Status;
  if(abs(pc3NorthEastRenorm[0]*trueEvents*pc3eastclustersec1n->GetMean() - pc3EastAverage) >
     pc3EastMinError + sigmaDiff*(sqrt(trueEvents*pc3eastclustersec1n->GetMean()) + pc3EastAverageSigma)) {
    cout << "\n PC3 NorthEast Sector #1 - PC3 East Sector Average is too large:";
    cout << "\n  PC3 NorthEast Sector #1 " << trueEvents*pc3eastclustersec1n->GetMean();
    cout << ",   PC3 East Sector Average " << pc3EastAverage;
    cout << endl;
    pc3EastStatus += 10;
  }

  if(abs(pc3NorthEastRenorm[1]*trueEvents*pc3eastclustersec2n->GetMean() - pc3EastAverage) >
     pc3EastMinError + sigmaDiff*(sqrt(trueEvents*pc3eastclustersec2n->GetMean()) + pc3EastAverageSigma)) {
    cout << "\n PC3 NorthEast Sector #2 - PC3 East Sector Average is too large:";
    cout << "\n  PC3 NorthEast Sector #2 " << trueEvents*pc3eastclustersec2n->GetMean();
    cout << ",   PC3 East Sector Average " << pc3EastAverage;
    cout << endl;
    pc3EastStatus += 100;
  }

  if(abs(pc3NorthEastRenorm[2]*trueEvents*pc3eastclustersec3n->GetMean() - pc3EastAverage) >
     pc3EastMinError + sigmaDiff*(sqrt(trueEvents*pc3eastclustersec3n->GetMean()) + pc3EastAverageSigma)) {
    cout << "\n PC3 NorthEast Sector #3 - PC3 East Sector Average is too large:";
    cout << "\n  PC3 NorthEast Sector #3 " << trueEvents*pc3eastclustersec3n->GetMean();
    cout << ",   PC3 East Sector Average " << pc3EastAverage;
    cout << endl;
    pc3EastStatus += 1000;
  }

  if(abs(pc3NorthEastRenorm[3]*trueEvents*pc3eastclustersec4n->GetMean() - pc3EastAverage) >
     pc3EastMinError + sigmaDiff*(sqrt(trueEvents*pc3eastclustersec4n->GetMean()) + pc3EastAverageSigma)) {
    cout << "\n PC3 NorthEast Sector #4 - PC3 East Sector Average is too large:";
    cout << "\n  PC3 NorthEast Sector #4 " << trueEvents*pc3eastclustersec4n->GetMean();
    cout << ",   PC3 East Sector Average " << pc3EastAverage;
    cout << endl;
    pc3EastStatus += 10000;
  }

  if(abs(pc3SouthEastRenorm[0]*trueEvents*pc3eastclustersec1s->GetMean() - pc3EastAverage) >
     pc3EastMinError + sigmaDiff*(sqrt(trueEvents*pc3eastclustersec1s->GetMean()) + pc3EastAverageSigma)) {
    cout << "\n PC3 SouthEast Sector #1 - PC3 East Sector Average is too large:";
    cout << "\n  PC3 SouthEast Sector #1 " << trueEvents*pc3eastclustersec1s->GetMean();
    cout << ",   PC3 East Sector Average " << pc3EastAverage;
    cout << endl;
    pc3EastStatus += 20;
  }

  if(abs(pc3SouthEastRenorm[1]*trueEvents*pc3eastclustersec2s->GetMean() - pc3EastAverage) >
     pc3EastMinError + sigmaDiff*(sqrt(trueEvents*pc3eastclustersec2s->GetMean()) + pc3EastAverageSigma)) {
    cout << "\n PC3 SouthEast Sector #2 - PC3 East Sector Average is too large:";
    cout << "\n  PC3 SouthEast Sector #2 " << trueEvents*pc3eastclustersec2s->GetMean();
    cout << ",   PC3 East Sector Average " << pc3EastAverage;
    cout << endl;
    pc3EastStatus += 200;
  }

  if(abs(pc3SouthEastRenorm[2]*trueEvents*pc3eastclustersec3s->GetMean() - pc3EastAverage) >
     pc3EastMinError + sigmaDiff*(sqrt(trueEvents*pc3eastclustersec3s->GetMean()) + pc3EastAverageSigma)) {
    cout << "\n PC3 SouthEast Sector #3 - PC3 East Sector Average is too large:";
    cout << "\n  PC3 SouthEast Sector #3 " << trueEvents*pc3eastclustersec3s->GetMean();
    cout << ",   PC3 East Sector Average " << pc3EastAverage;
    cout << endl;
    pc3EastStatus += 2000;
  }

  if(abs(pc3SouthEastRenorm[3]*trueEvents*pc3eastclustersec4s->GetMean() - pc3EastAverage) >
     pc3EastMinError + sigmaDiff*(sqrt(trueEvents*pc3eastclustersec4s->GetMean()) + pc3EastAverageSigma)) {
    cout << "\n PC3 SouthEast Sector #4 - PC3 East Sector Average is too large:";
    cout << "\n  PC3 SouthEast Sector #4 " << trueEvents*pc3eastclustersec4s->GetMean();
    cout << ",   PC3 East Sector Average " << pc3EastAverage;
    cout << endl;
    pc3EastStatus += 20000;
  }

  cout << "\n PC3 totals: mean = " << pc3clustermult->GetMean();
  cout << ",  North = " << pc3clusternorth->GetMean();
  cout << ",  South = " << pc3clustersouth->GetMean();
  cout << endl;
  cout << "   NorthEast: " <<  pc3eastclustersec1n->GetMean();
  cout << "  " << pc3eastclustersec2n->GetMean();
  cout << "  " << pc3eastclustersec3n->GetMean();
  cout << "  " << pc3eastclustersec4n->GetMean();
  cout << endl;
  cout << "   SouthEast: " <<  pc3eastclustersec1s->GetMean();
  cout << "  " << pc3eastclustersec2s->GetMean();
  cout << "  " << pc3eastclustersec3s->GetMean();
  cout << "  " << pc3eastclustersec4s->GetMean();
  cout << endl;

  cout << "\n Pad Chamber Totals Status Flags:  PC1 Status " << pc1Status;
  cout << ",  PC3 Status " << pc3Status;

  if(pc1Status)
    cout << "  PC1 abnormal";
  else
    cout << "  PC1 normal";

  if(pc3Status)
    cout << "  PC3 abnormal";
  else
    cout << "  PC3 normal";

  if(pc1EastStatus>9 || pc1WestStatus>9) {
    cout << "\n PC1 sector check has abnormal status flag ";
    //
    // Format for cout manipulators don't work in PHOOL??
    //
    cout << "   East sectors flag = ";
    Int_t iDiv = 100000000;
    for(Int_t kDiv=0; kDiv<9; kDiv++) {
      Int_t iTemp = pc1EastStatus/iDiv;
      cout << iTemp;
      pc1EastStatus = pc1EastStatus - iTemp*iDiv;
      iDiv = iDiv/10;
    }
    cout << "   West sectors flag = ";
    iDiv = 100000000;
    for(Int_t kDiv=0; kDiv<9; kDiv++) {
      Int_t iTemp = pc1WestStatus/iDiv;
      cout << iTemp;
      pc1WestStatus = pc1WestStatus - iTemp*iDiv;
      iDiv = iDiv/10;
    }
  }

  if(pc3EastStatus>9) {
    cout << "\n PC3 sector check has abnormal status flag ";
    //
    // Format for cout manipulators don't work in PHOOL??
    //
    cout << "   East sectors flag = ";
    Int_t iDiv = 10000;
    for(Int_t kDiv=0; kDiv<5; kDiv++) {
      Int_t iTemp = pc3EastStatus/iDiv;
      cout << iTemp ;
      pc3EastStatus = pc3EastStatus - iTemp*iDiv;
      iDiv = iDiv/10;
    }
  }

  cout << endl;

  padhistfile->Write();
}
