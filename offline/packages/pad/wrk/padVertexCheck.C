void padVertexCheck(Int_t maxEvents=2, Int_t kFiles=1,
	            const char *dstIFile="dstData.root", const char *relIFile="rawrel.root",
	            const char *pisaIFile="PISA2000Normal.root") {
/*
  PC1/PC3 Z0 Vertex evaluation macro
  Author: Charles F. Maguire
  Creation Date: February 10, 2001

  Input arguments
        maxEvents  total number events to be processed in a given input file
        kFiles     number of input files to be processed (sets of 3 input files)
                   after the first input file set, then the next sets of input files
                   are assumed to have names dstData01.root, pisaData01.root, relData01.root, etc.

        dstIFile   name of first DST input file
        relIFile   name of GEANT Association Tables file related to first DST input file 
        pisaIFile  name of PISA hits file which was the input source for the DST file

  Calling Map
        padVertexIni.C and padVertexNode.C are called for initialization, and re-initialization

  Method
       Uses padTrkVtx class to correlated clusters in PC1 and PC3
       See http://www.phenix.bnl.gov/~silvermy/offline/padTrkVtx.html/

  Qualifications
       There can be maximum of 99 input files.  After the first set, these will have names
       dstDatann.root, relDatann.root, pisaDatann.root where nn goes from 01 to 99

  Output
       An output NTUPLE file padDst.root is created diagnosing the PC1/PC3 associations;
       two NTUPLEs are produced.  First is for the PC Cluster singles information;
       second is for the PC1/PC3 Z0 diagnostics.

  Revision history
       February 11, 2001  C.F. Maguire  add Z0 and impact parameter to NTUPLE from PISA event header

       February 13, 2001  C.F. Maguire  add PC1 and PC3 parent track numbers to NTUPLE; add more comments
                                        do corrections for use of padTrkVtx as suggested by David Silvermyr

       February 15, 2001  C.F. Maguire  add arm information to NTUPLE output
                                        add second NTUPLE for PC cluster information
					add third NTUPLE for PC raw information

       February 22, 2001  C.F. Maguire  use HitsClear method to release memory used for PISA event hits
*/

  if(kFiles>99) {
    cerr << "\n Number of input files " << kFiles;
    cerr << " exceeds maximum allowed value of 99" << endl;
    return;
  } // safety check

  Int_t eventNumber = 0;
  Int_t eventGood = 0;
  const float DEGRAD = 57.295779513;

  static TFile *DstFile = 0;
  DstFile = new TFile("padDst.root", "recreate", "PC DST correlations");

  const int PC_RAW = 18;
  float pc_raw[PC_RAW];
  static TNtuple *PCRaw= 0;
  PCRaw = new TNtuple("PCRaw", "PC Raw Coordinates",
                          "IPC:KRAW:ARM:SECTOR:SIDE:PADX:PADZ:PADTYPE:"//
                          "Z0EVENT:BIMPACT:BBCNORTH:BBCSOUTH:BBCZVERT:"//
                          "NTRACK:NCLUSPC1:NCLUSPC3:IFILE:EVENT");

  const int PC_CLUSTER = 16;
  float pc_cluster[PC_CLUSTER];
  static TNtuple *PCCluster= 0;
  PCCluster = new TNtuple("PCCluster", "PC Cluster Coordinates",
                          "IPC:KCLUSTER:ARM:CLUSTERX:CLUSTERY:CLUSTERZ:"//
                          "Z0EVENT:BIMPACT:BBCNORTH:BBCSOUTH:BBCZVERT:"//
                          "NTRACK:NCLUSPC1:NCLUSPC3:IFILE:EVENT");

  const int PC_TRKVTX = 26;
  float pc_trkvtx[PC_TRKVTX];
  static TNtuple *PCTrk= 0;
  PCTrk = new TNtuple("PCTrkVtx", "PCTrkVtx  Ouput",
		      "KTRACK:TRACK1:IDPART1:TRACK3:IDPART3:PC1PC3Z0:"//
                      "RVERTEX1:ZVERTEX1:IDPARENT1:RVERTEX3:ZVERTEX3:IDPARENT3:"//
		      "ITPARNT1:ITPARNT3:ARM1:ARM3:"//
                      "Z0EVENT:BIMPACT:BBCNORTH:BBCSOUTH:BBCZVERT:"//
		      "NTRACK:NCLUSPC1:NCLUSPC3:IFILE:EVENT");

  gROOT->Macro("padVertexIni.C");
  char *pisaDataxx = "pisaData00.root";
  char *dstDataxx = "dstData00.root";
  char *relDataxx = "relData00.root";
  char digits[10] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'};

  //
  // set up padTrkVtx methods
  //
  padTrkVtx* mpadTrkVtx = new padTrkVtx();
  mpadTrkVtx->setDebugLevel(0);
  mpadTrkVtx->setMaxDeltaXYZR(100.0,100.0,100.0,20.0);
  mpadTrkVtx->setMethod(0);

  Int_t kevent = 0;  // counts number of PISA events processed

  //
  // Loop over sequential input files (triplets of DST, PISA, and relational table files)
  //
  for (Int_t iFile=0; iFile<kFiles; iFile++) {
    if(iFile>0) {
      //
      // Determine new file names, after first file is finished
      //
      Int_t iUnit = iFile%10;
      Int_t iTen = iFile/10;
      dstDataxx[7] = digits[iTen];
      dstDataxx[8] = digits[iUnit];
      dstInFile = dstDataxx;

      relDataxx[7] = digits[iTen];
      relDataxx[8] = digits[iUnit];
      relInFile = relDataxx;

      gROOT->Macro("padVertexNode.C");
      cout << "\n Opened DST input file " << dstInFile << endl;

      delete pisaFile;
      pisaDataxx[8] = digits[iTen];
      pisaDataxx[9] = digits[iUnit];
      pisaIFile = pisaDataxx;
      TFile *pisaFile = new TFile(pisaIFile);
      cout << "\n Opened PISA input file " << pisaIFile << endl;

      T = (TTree*)pisaFile.Get("T");
      branch = T->GetBranch("pisaevent");
      branch->SetAddress(&pisaevent);
      kevent = 0;
    }  // check on file not being the first file

    // Time for the event loop
    eventNumber = 1;
    while (eventNumber<=maxEvents && eventGood==1) {

   ioEval->read(evaNode);

    //
    //  Read in a PISA Event
    //
    pisarun->GetOneEvent(pisaevent,&kevent,T);

    PISAEventHeader *EventHeader = pisaevent->GetHeader();

    KinGetGEA(topNode);
    PadGetGEA(topNode);

      if (eventNumber > 1) {
	// Yes, there is easier logic that could be used,
	// but ROOT doesn't want to take it. Arrggghh.
	eventGood = 0;
	if (dstIn->read(dstNode)) eventGood = 1;
      }

      if (eventGood == 1) {

	cout << "Read event " << eventNumber << endl;

	// Obtain the BBC output and the PC Cluster nodes

        if (!dBbcOutNode) {
          cout << "Dst Read: Could not find data node dBbcOut" << endl;
        } else {
          dBbcOutWrapper* dBbcOut = (dBbcOutWrapper*)dBbcOutNode->getData();
        }

	if (!dPc1RawNode) {
	  cout << "Dst Read: Could not find data node dPc1Raw" << endl;
	} else {
	  dPadRawWrapper* dPc1Raw = (dPadRawWrapper*)dPc1RawNode->getData();
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

	if (!dPc3ClusterNode) {
	  cout << "Dst Read: Could not find data node dPc3Cluster" << endl;
	} else {
	  dPadClusterWrapper* dPc3Cluster = (dPadClusterWrapper*)dPc3ClusterNode->getData();
	}

        PHIODataNode<PHTable>* dPc1GhitClusNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode", "dPc1GhitClus");
        if(!dPc1GhitClusNode) {
          cout << "\n rawrel read: Could not find data node dPc1GhitClus" << endl;
          return;
        } else {
          dPadGhitClusWrapper* dPc1GhitClus = (dPadGhitClusWrapper*)dPc1GhitClusNode->getData();
        }

        PHIODataNode<PHTable>* dPc3GhitClusNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode", "dPc3GhitClus");
        if(!dPc3GhitClusNode) {
          cout << "\n rawrel read: Could not find data node dPc3GhitClus" << endl;
          return;
        } else {
          dPadGhitClusWrapper* dPc3GhitClus = (dPadGhitClusWrapper*)dPc3GhitClusNode->getData();
        }

	Float_t bbcZVertex = dBbcOut->get_VertexPoint(0);

	pc_trkvtx[PC_TRKVTX - 1] = eventNumber;
	pc_trkvtx[PC_TRKVTX - 2] = iFile;

        pc_cluster[PC_CLUSTER - 1] = eventNumber;
        pc_cluster[PC_CLUSTER - 2] = iFile;

        pc_raw[PC_RAW - 1] = eventNumber;
        pc_raw[PC_RAW - 2] = iFile;

        Int_t nPc1 = dPc1Cluster->RowCount();
        Int_t nPc3 = dPc3Cluster->RowCount();
	pc_trkvtx[PC_TRKVTX - 3] = nPc3;
	pc_trkvtx[PC_TRKVTX - 4] = nPc1;

        pc_cluster[PC_CLUSTER - 3] = nPc3;
	pc_cluster[PC_CLUSTER - 4] = nPc1;

        pc_raw[PC_RAW - 3] = nPc3;
	pc_raw[PC_RAW - 4] = nPc1;

	mpadTrkVtx->setPlaneOriginPoint(0.0, 0.0, bbcZVertex);  // recommended by David Silvermyr
        PHBoolean trksuccess = mpadTrkVtx->event(topNode);

        Int_t numAccTrks = mpadTrkVtx->getNumAccTrks();
	pc_trkvtx[PC_TRKVTX - 5] = numAccTrks;
	pc_cluster[PC_CLUSTER - 5] = numAccTrks;
	pc_raw[PC_RAW - 5] = numAccTrks;

        cout << "\n Number of tracks = " <<  numAccTrks << endl;

        pc_trkvtx[PC_TRKVTX - 6] = bbcZVertex;
        pc_trkvtx[PC_TRKVTX - 7] = dBbcOut->get_NhitPmtSouth(0);
        pc_trkvtx[PC_TRKVTX - 8] = dBbcOut->get_NhitPmtNorth(0);
        pc_trkvtx[PC_TRKVTX - 9] = EventHeader->GetImpactParameter();
        pc_trkvtx[PC_TRKVTX - 10] = EventHeader->GetZvertex();

        pc_cluster[PC_CLUSTER - 6] = bbcZVertex;
        pc_cluster[PC_CLUSTER - 7] = dBbcOut->get_NhitPmtSouth(0);
        pc_cluster[PC_CLUSTER - 8] = dBbcOut->get_NhitPmtNorth(0);
        pc_cluster[PC_CLUSTER - 9] = EventHeader->GetImpactParameter();
        pc_cluster[PC_CLUSTER - 10] = EventHeader->GetZvertex();

        pc_raw[PC_RAW - 6] = bbcZVertex;
        pc_raw[PC_RAW - 7] = dBbcOut->get_NhitPmtSouth(0);
        pc_raw[PC_RAW - 8] = dBbcOut->get_NhitPmtNorth(0);
        pc_raw[PC_RAW - 9] = EventHeader->GetImpactParameter();
        pc_raw[PC_RAW - 10] = EventHeader->GetZvertex();
  
        //
        // Store PC1 Raw information
        //
        pc_raw[0] = 1;
        for(Int_t jpc1=0; jpc1<nPc1; jpc1++) {
          pc_raw[1]= jpc1;
          pc_raw[2] = dPc1Raw->get_arm(jpc1);
          pc_raw[3] = dPc1Raw->get_sector(jpc1);
          pc_raw[4] = dPc1Raw->get_side(jpc1);
          pc_raw[5] = dPc1Raw->get_padx(jpc1);
          pc_raw[6] = dPc1Raw->get_padz(jpc1);
          pc_raw[7] = dPc1Raw->get_padtype(jpc1);

	  PCRaw->Fill(pc_raw);

	}  // loop over PC1 Raw

        //
        // Store PC3 Raw information
        //
        pc_raw[0] = 3;
        for(Int_t jpc3=0; jpc3<nPc3; jpc3++) {
          pc_raw[1]= jpc3;
          pc_raw[2] = dPc3Raw->get_arm(jpc3);
          pc_raw[3] = dPc3Raw->get_sector(jpc3);
          pc_raw[4] = dPc3Raw->get_side(jpc3);
          pc_raw[5] = dPc3Raw->get_padx(jpc3);
          pc_raw[6] = dPc3Raw->get_padz(jpc3);
          pc_raw[7] = dPc3Raw->get_padtype(jpc3);

	  PCRaw->Fill(pc_raw);

	}  // loop over PC3 Raw

        //
        // Store PC1 Cluster information
        //
        pc_cluster[0] = 1;
        for(Int_t kPc1=0; kPc1<nPc1; kPc1++) {
          pc_cluster[1]= kPc1;
          pc_cluster[2] = dPc1Cluster->get_arm(kPc1);
          pc_cluster[3] = dPc1Cluster->get_xyz(0,kPc1);
          pc_cluster[4] = dPc1Cluster->get_xyz(1,kPc1);
          pc_cluster[5] = dPc1Cluster->get_xyz(2,kPc1);
          PCCluster->Fill(pc_cluster);

        } // loop over PC1 clusters

	// 
	// Store PC3 Cluster information
	//
        pc_cluster[0] = 3;
        for(Int_t kPc3=0; kPc3<nPc3; kPc3++) {
          pc_cluster[1]= kPc3;
          pc_cluster[2] = dPc3Cluster->get_arm(kPc3);
          pc_cluster[3] = dPc3Cluster->get_xyz(0,kPc3);
          pc_cluster[4] = dPc3Cluster->get_xyz(1,kPc3);
          pc_cluster[5] = dPc3Cluster->get_xyz(2,kPc3);

          PCCluster->Fill(pc_cluster);

        } // loop over PC3 clusters

        //
        // Store PC1/PC3 Z0 vertex evaluation
        //
        for(Int_t kTrack=0; kTrack<numAccTrks; kTrack++) {

	  //
	  // Retrieve the PC1 information for this PC1/PC3 track
	  //
	  Int_t pc1AccNum = mpadTrkVtx->getPC1AccHitNum(kTrack);
          Int_t pc1ClusID =  mpadTrkVtx->getPC1hitId(pc1AccNum); // get the PC1 Cluster sequence number
	  if(pc1ClusID < 0 || pc1ClusID >= dPc1GhitClus->RowCount()) {
	    cerr << "\n PC1 ClusID error ";
	    cerr << ", kTrack = " << kTrack << ",  pc1ClusID = " << pc1ClusID;
	    cerr << ", dPc1GhitClus->RowCount() = " << dPc1GhitClus->RowCount() << endl;
	    exit(1);
	  } // safety check on pc1ClusID range

          Int_t pc1ghitID = dPc1GhitClus->get_ghitid(pc1ClusID);  // get the PC1 GEANT hit sequence number
	  if(pc1ghitID < 0 || pc1ghitID >= pc1ghit->RowCount()) {
	    cerr << "\n PC1 ghitID error ";
	    cerr << ", kTrack = " << kTrack << ",  pc1ghitID = " << pc1ghitID;
	    cerr << ", pc1ghit->RowCount() = " << pc1ghit->RowCount() << endl;
	    exit(1);
	  } // safety check on pc1ghitID range

          Int_t mcTrack1 = pc1ghit->get_mctrack(pc1ghitID);  //retrieve the PC1 GEANT track number
          Int_t idPart1;
          Int_t idParent1;
          Int_t itParent1;
          Float_t rVertex1;
          Float_t zVertex1;

          Float_t pTot;
          Float_t pPhi;
          Float_t pTheta;
          Float_t nFile;
          Float_t itOrigin;
          Float_t idOrigin;
          GeaTrkStack(mcTrack1, idPart1, idParent1, pTot, rVertex1, zVertex1, pTheta, pPhi, nFile,
                      itParent1, itOrigin, idOrigin);

	  //
	  // Retreive the PC3 information for this PC1/PC3 track
	  //
	  Int_t pc3AccNum = mpadTrkVtx->getPC3AccHitNum(kTrack);
          Int_t pc3ClusID =  mpadTrkVtx->getPC3hitId(pc3AccNum); // get the PC3 Cluster sequence number
	  if(pc3ClusID < 0 || pc3ClusID >= dPc3GhitClus->RowCount()) {
	    cerr << "\n PC3 ClusID error ";
	    cerr << ", kTrack = " << kTrack << ",  pc3ClusID = " << pc3ClusID;
	    cerr << ", dPc3GhitClus->RowCount() = " << dPc3GhitClus->RowCount() << endl;
	    exit(1);
	  } // safety check on pc3ClusID range

          Int_t pc3ghitID = dPc3GhitClus->get_ghitid(pc3ClusID);  // get the PC3 GEANT hits sequence number
	  if(pc3ghitID < 0 || pc3ghitID >= pc3ghit->RowCount()) {
	    cerr << "\n PC3 ghitID error ";
	    cerr << ", kTrack = " << kTrack << ",  pc3ghitID = " << pc3ghitID;
	    cerr << ", pc3ghit->RowCount() = " << pc3ghit->RowCount() << endl;
	    exit(1);
	  } // safety check on pc3ghitID range

          Int_t mcTrack3 = pc3ghit->get_mctrack(pc3ghitID);  // retrieve the PC3 GEANT track number
          Int_t idPart3;
          Int_t idParent3;
	  Int_t itParent3;
          Float_t rVertex3;
          Float_t zVertex3;
          GeaTrkStack(mcTrack3, idPart3, idParent3, pTot, rVertex3, zVertex3, pTheta, pPhi, nFile,
                      itParent3, itOrigin, idOrigin);

	  pc_trkvtx[0] = kTrack;
	  pc_trkvtx[1] = mcTrack1;
	  pc_trkvtx[2] = idPart1;
	  pc_trkvtx[3] = mcTrack3;
	  pc_trkvtx[4] = idPart3;
          pc_trkvtx[5] = mpadTrkVtx->getInterSectionZ(kTrack);

          pc_trkvtx[6] = rVertex1;
          pc_trkvtx[7] = zVertex1;
          pc_trkvtx[8] = idParent1;
          pc_trkvtx[9] = rVertex3;
          pc_trkvtx[10] = zVertex3;
          pc_trkvtx[11] = idParent3;

	  pc_trkvtx[12] = itParent1;
	  pc_trkvtx[13] = itParent3;

	  pc_trkvtx[14] = dPc1Cluster->get_arm(pc1ClusID);
	  pc_trkvtx[15] = dPc3Cluster->get_arm(pc3ClusID);

          PCTrk->Fill(pc_trkvtx);

        } // loop over accepted tracks

      }  // eventGood

      pisarun->HitsClear();
      eventNumber++;

    }  // loop over events

    cout << "\n Finished input file " << iFile;
    cout << "  with events = " << eventNumber;
    cout << endl;

  } // loop over DST input files

  DstFile->Write();
  DstFile->Close();

}
