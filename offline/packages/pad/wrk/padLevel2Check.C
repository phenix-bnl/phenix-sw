void padLevel2Check(Int_t maxEvents=2, Int_t kFiles=1,
	            const char *dstIFile="dstData.root", const char *relIFile="rawrel.root",
	            const char *pisaIFile="PISA2000Normal.root", const Int_t mergeFiles = 0,
                    const char *gname="piminus867.root",
                    const char *hname="piplus867.root",
                    const char *iname="positron1gev_20th85ph175_120400z00.root",
                    const char *jname="muon5GeV_1000th165ph10_050901.root",
                    const char *padIFile="") {
/*
  Pad Chamber Level2 evaluation macro
  Author: Charles F. Maguire
  Creation Date: June 19, 2001

  Input arguments
        maxEvents  total number events to be processed in a given input file
        kFiles     number of input files to be processed (sets of 3 input files)
                   after the first input file set, then the next sets of input files
                   are assumed to have names dstData01.root, pisaData01.root, relData01.root, etc.

        dstIFile   name of first DST input file
        relIFile   name of GEANT Association Tables file related to first DST input file 
        pisaIFile  name of PISA hits file which was the input source for the DST file

        mergeFiles, gname to jname as explained on the PISA MERGE WWW documentation site

        padIFile   workaround evaluator file in PISA-to-PRDF PRDF-to-DST sequence because
                   of design flaw in PC evaluators system; the padIFile is not needed for
                   the PISA-to-DST direct sequence

  Calling Map
        padVertexIni.C and padVertexNode.C are called for initialization, and re-initialization

  Qualifications
       There can be maximum of 99 input files.  After the first set, these will have names
       dstDatann.root, relDatann.root, pisaDatann.root where nn goes from 01 to 99

  Output
       An output NTUPLE file padDst.root is created diagnosing the PC1/PC3 associations;
       two NTUPLEs are produced.  First is for the PC Cluster singles information;
       second is for the PC1/PC3 Z0 diagnostics.

  Revision history

  C.F. Maguire    July 4, 2001     Expand the PC_LEVEL2 NTUPLE to include more checking
                                   of position information
                                   Document extra input arguments
                                   Put type (cluster shape) parameter into NTUPLEs

*/

  const Int_t maxFiles = 5;

  if(kFiles>1 && mergeFiles>0) {
    cerr << "\n Incompatible values of kFiles = " << kFiles;
    cerr << "  and mergeFiles = " << mergeFiles << endl;
    return;
  }  // safety check

  if(mergeFiles<0 || mergeFiles>maxFiles-1) {
    cerr << "\n Unacceptable value of mergeFiles = " << mergeFiles << endl;
    return;
  }  // safety check

  if(kFiles>99) {
    cerr << "\n Number of input files " << kFiles;
    cerr << " exceeds maximum allowed value of 99" << endl;
    return;
  } // safety check

  Int_t eventNumber = 0;
  Int_t eventGood = 0;
  const float DEGRAD = 57.295779513;

  static TFile *DstFile = 0;
  DstFile = new TFile("padLevel2.root", "recreate", "PC Level2 Checks");

  const int PC_RAW = 18;
  float pc_raw[PC_RAW];
  static TNtuple *PCRaw= 0;
  PCRaw = new TNtuple("PCRaw", "PC Raw Coordinates",
                          "IPC:KRAW:ARM:SECTOR:SIDE:PADX:PADZ:PADTYPE:"//
                          "Z0EVENT:BIMPACT:BBCNORTH:BBCSOUTH:BBCZVERT:"//
                          "NTRACK:NCLUSPC1:NCLUSPC3:NFILE:EVENT");

  const int PC_CLUSTER = 22;
  float pc_cluster[PC_CLUSTER];
  static TNtuple *PCCluster= 0;
  PCCluster = new TNtuple("PCCluster", "PC Cluster Coordinates",
                          "IPC:KCLUSTER:ARM:CLUSTERX:CLUSTERY:CLUSTERZ:"//
			  "THETA:PHI:THETAGEA:PHIGEA:TRACK:TYPE:"//
                          "Z0EVENT:BIMPACT:BBCNORTH:BBCSOUTH:BBCZVERT:"//
                          "ZORIGIN:NCLUSPC1:NCLUSPC3:NFILE:EVENT");

  const int PC_LEVEL2 = 61;
  float pc_level2[PC_LEVEL2];
  static TNtuple *PCLevel2= 0;
  PCLevel2 = new TNtuple("PCLevel2", "PCLevel2  Ouput",
		      "IPC:TRACK1:IDPART1:TRACK2:TRACK3:MPC123:GPC123:"//
                      "THETA1:THETA2:THETA3:PHI1:PHI2:PHI3:"// 
                      "IPC123:PTOT:PC2DELZ:PC2DELY:DELZGEA:DELYGEA:"//
		      "IDPARNT1:IDORIGN1:RVERTEX1:ZVERTEX1:NFILE1:"//
                      "X1CLUST:Y1CLUST:Z1CLUST:X1GEANT:Y1GEANT:Z1GEANT:"//
                      "X2CLUST:Y2CLUST:Z2CLUST:X2GEANT:Y2GEANT:Z2GEANT:"//
                      "X3CLUST:Y3CLUST:Z3CLUST:X3GEANT:Y3GEANT:Z3GEANT:"//
                      "Y1DFTRUE:Z1DFTRUE:Y1DFGPRO:Z1DFGPRO:Y1DFCPRO:Z1DFCPRO:"//
                      "PC1TYPE:PC2TYPE:PC3TYPE:"//
                      "Z0EVENT:BIMPACT:BBCNORTH:BBCSOUTH:BBCZVERT:"//
		      "ZORIGIN:NCLUSPC1:NCLUSPC3:IFILE:EVENT");

  gROOT->Macro("padVertexIni.C");
  char *pisaDataxx = "pisaData00.root";
  char *dstDataxx = "dstData00.root";
  char *relDataxx = "relData00.root";
  char digits[10] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'};

  Int_t kevent = 0;  // counts number of PISA events processed

  /*
  Int_t iTest = 0;
  cout <<  "\n debug pause " << endl;
  cin >> iTest;
  cout << " iTest = " << iTest << endl;
  */

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
    Int_t trueEvents = T->GetEntries();
    Int_t readEvents = maxEvents;
    if(trueEvents < readEvents) {
      readEvents = trueEvents;
      cout << "\n Number of events in this file is " << trueEvents << endl;
    }
    while (eventNumber<=readEvents && eventGood==1) {

    ioEval->read(evaNode);
    if(padFile) {
      padFile->read(padNode);
      cout << "\n read padNode information " << endl;
    }

    //
    //  Read in a PISA Event
    //
    // pisarun->GetOneEvent(pisaEventArray, &kevent, TTArray, mergeFiles+1);  // get GEANT hits

    if(mergeFiles>0)
      pisarun->GetOneEvent(pisaEventArray, &kevent, TTArray, mergeFiles+1);  // get GEANT hits
    else
      pisarun->GetOneEvent(pisaevent,&kevent,T);

    PISAEventHeader *EventHeader = pisaevent->GetHeader();
    Float_t z0Event = EventHeader->GetZvertex();

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

/*
	if(padFile) {
	  cout << "\n Calling PC evaluator modules " << endl;

	  dPadEvalPar->set_pcnumber(0,0);
	  mPc1Evaluate->set_pcnumber(0);
	  mPc1Evaluate->event(topNode);

	  dPadEvalPar->set_pcnumber(0,1);
	  mPc2Evaluate->set_pcnumber(1);
	  mPc2Evaluate->event(topNode);

	  dPadEvalPar->set_pcnumber(0,2);
	  mPc3Evaluate->set_pcnumber(2);
	  mPc3Evaluate->event(topNode);
	}
*/

        PHIODataNode<PHTable>* dPc1GhitClusNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode", "dPc1GhitClus");
        if(!dPc1GhitClusNode) {
          cout << "\n rawrel read: Could not find data node dPc1GhitClus" << endl;
          return;
        } else {
          dPadGhitClusWrapper* dPc1GhitClus = (dPadGhitClusWrapper*)dPc1GhitClusNode->getData();
        }

	cout << "\n dPc1GhitClus " << dPc1GhitClus->RowCount() << endl;

        PHIODataNode<PHTable>* dPc2GhitClusNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode", "dPc2GhitClus");
        if(!dPc2GhitClusNode) {
          cout << "\n rawrel read: Could not find data node dPc2GhitClus" << endl;
          return;
        } else {
          dPadGhitClusWrapper* dPc2GhitClus = (dPadGhitClusWrapper*)dPc2GhitClusNode->getData();
        }

	cout << "\n dPc2GhitClus " << dPc2GhitClus->RowCount() << endl;

        PHIODataNode<PHTable>* dPc3GhitClusNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode", "dPc3GhitClus");
        if(!dPc3GhitClusNode) {
          cout << "\n rawrel read: Could not find data node dPc3GhitClus" << endl;
          return;
        } else {
          dPadGhitClusWrapper* dPc3GhitClus = (dPadGhitClusWrapper*)dPc3GhitClusNode->getData();
        }

	cout << "\n dPc3GhitClus " << dPc3GhitClus->RowCount() << endl;

	Float_t bbcZVertex = dBbcOut->get_VertexPoint(0);
	Float_t zOrigin = z0Event;
	if(dBbcOut->get_NhitPmtSouth(0)>1 && dBbcOut->get_NhitPmtNorth(0)>1)
	   zOrigin = bbcZVertex;

	pc_level2[PC_LEVEL2 - 1] = eventNumber;
	pc_level2[PC_LEVEL2 - 2] = iFile;

        pc_cluster[PC_CLUSTER - 1] = eventNumber;
        pc_cluster[PC_CLUSTER - 2] = iFile;

        pc_raw[PC_RAW - 1] = eventNumber;
        pc_raw[PC_RAW - 2] = iFile;

        Int_t nPc1 = dPc1Cluster->RowCount();
        Int_t nPc2 = dPc2Cluster->RowCount();
        Int_t nPc3 = dPc3Cluster->RowCount();
	pc_level2[PC_LEVEL2 - 3] = nPc3;
	pc_level2[PC_LEVEL2 - 4] = nPc1;

        pc_cluster[PC_CLUSTER - 3] = nPc3;
	pc_cluster[PC_CLUSTER - 4] = nPc1;

        pc_raw[PC_RAW - 3] = nPc3;
	pc_raw[PC_RAW - 4] = nPc1;

	pc_level2[PC_LEVEL2 - 5] = zOrigin;
	pc_cluster[PC_CLUSTER - 5] = zOrigin;
	pc_raw[PC_RAW - 5] = zOrigin;

        cout << "\n zOrigin = " << zOrigin << ", bbcZVertex = " << bbcZVertex << endl;

        pc_level2[PC_LEVEL2 - 6] = bbcZVertex;
        pc_level2[PC_LEVEL2 - 7] = dBbcOut->get_NhitPmtSouth(0);
        pc_level2[PC_LEVEL2 - 8] = dBbcOut->get_NhitPmtNorth(0);
        pc_level2[PC_LEVEL2 - 9] = EventHeader->GetImpactParameter();
        pc_level2[PC_LEVEL2 - 10] = EventHeader->GetZvertex();
	
	pc_level2[PC_LEVEL2 - 5] = zOrigin;
	pc_cluster[PC_CLUSTER - 5] = zOrigin;
	pc_raw[PC_RAW - 5] = zOrigin;

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

          Float_t xPc1 = dPc1Cluster->get_xyz(0,kPc1);
          Float_t yPc1 = dPc1Cluster->get_xyz(1,kPc1);
          Float_t zPc1 = dPc1Cluster->get_xyz(2,kPc1);
          Float_t theta = DEGRAD*acos(zPc1/sqrt(xPc1*xPc1 + yPc1*yPc1 + zPc1*zPc1));
          Float_t phi = DEGRAD*atan2(yPc1, xPc1);
          if(phi < -90.0)
            phi += 360.0;

          Int_t pc1ghitID = dPc1GhitClus->get_ghitid(kPc1);  // get the PC1 GEANT hit sequence number
	  if(pc1ghitID < 0 || pc1ghitID >= pc1ghit->RowCount()) {
	    cerr << "\n PC1 ghitID error ";
	    cerr << ", kPc1 = " << kPc1 << ",  pc1ghitID = " << pc1ghitID;
	    cerr << ", pc1ghit->RowCount() = " << pc1ghit->RowCount() << endl;
	    exit(1);
	  } // safety check on pc1ghitID range

	  Float_t xPc1Geant = pc1ghit->get_xyzinglo(0,pc1ghitID);
	  Float_t yPc1Geant = pc1ghit->get_xyzinglo(1,pc1ghitID);
	  Float_t zPc1Geant = pc1ghit->get_xyzinglo(2,pc1ghitID);
          Float_t thetaGeant = DEGRAD*acos(zPc1Geant/sqrt(xPc1Geant*xPc1Geant +
							  yPc1Geant*yPc1Geant + zPc1Geant*zPc1Geant));
          Float_t phiGeant = DEGRAD*atan2(yPc1Geant, xPc1Geant);
          if(phiGeant < -90.0)
            phiGeant += 360.0;

	  pc_cluster[6] = theta;  // Theta value from PC Cluster
	  pc_cluster[7] = phi;    // Phi value from PC Cluster

	  pc_cluster[8] = thetaGeant;  // Theta value from GEANT hit
	  pc_cluster[9] = phiGeant;    // Phi value from GEANT hit

	  pc_cluster[10] = pc1ghit->get_mctrack(pc1ghitID);
	  pc_cluster[11] = dPc1Cluster->get_type(kPc1);

          PCCluster->Fill(pc_cluster);

        } // loop over PC1 clusters

	// 
	// Store PC2 Cluster information
	//
        pc_cluster[0] = 2;
        for(Int_t kPc2=0; kPc2<nPc2; kPc2++) {
          pc_cluster[1]= kPc2;
          pc_cluster[2] = dPc2Cluster->get_arm(kPc2);
          pc_cluster[3] = dPc2Cluster->get_xyz(0,kPc2);
          pc_cluster[4] = dPc2Cluster->get_xyz(1,kPc2);
          pc_cluster[5] = dPc2Cluster->get_xyz(2,kPc2);

          Float_t xPc2 = dPc2Cluster->get_xyz(0,kPc2);
          Float_t yPc2 = dPc2Cluster->get_xyz(1,kPc2);
          Float_t zPc2 = dPc2Cluster->get_xyz(2,kPc2);
          Float_t theta = DEGRAD*acos(zPc2/sqrt(xPc2*xPc2 + yPc2*yPc2 + zPc2*zPc2));
          Float_t phi = DEGRAD*atan2(yPc2, xPc2);
          if(phi < -90.0)
            phi += 360.0;

          Int_t pc2ghitID = dPc2GhitClus->get_ghitid(kPc2);  // get the PC2 GEANT hit sequence number
	  if(pc2ghitID < 0 || pc2ghitID >= pc2ghit->RowCount()) {
	    cerr << "\n PC2 ghitID error ";
	    cerr << ", kPc2 = " << kPc2 << ",  pc2ghitID = " << pc2ghitID;
	    cerr << ", pc2ghit->RowCount() = " << pc2ghit->RowCount() << endl;
	    exit(1);
	  } // safety check on pc2ghitID range

	  Float_t xPc2Geant = pc2ghit->get_xyzinglo(0,pc2ghitID);
	  Float_t yPc2Geant = pc2ghit->get_xyzinglo(1,pc2ghitID);
	  Float_t zPc2Geant = pc2ghit->get_xyzinglo(2,pc2ghitID);
          Float_t thetaGeant = DEGRAD*acos(zPc2Geant/sqrt(xPc2Geant*xPc2Geant +
							  yPc2Geant*yPc2Geant + zPc2Geant*zPc2Geant));
          Float_t phiGeant = DEGRAD*atan2(yPc2Geant, xPc2Geant);
          if(phiGeant < -90.0)
            phiGeant += 360.0;

	  pc_cluster[6] = theta;  // Theta value from PC Cluster
	  pc_cluster[7] = phi;    // Phi value from PC Cluster

	  pc_cluster[8] = thetaGeant;  // Theta value from GEANT hit
	  pc_cluster[9] = phiGeant;    // Phi value from GEANT hit

	  pc_cluster[10] = pc2ghit->get_mctrack(pc2ghitID);
          pc_cluster[11] = dPc2Cluster->get_type(kPc2);

          PCCluster->Fill(pc_cluster);

        } // loop over PC2 clusters

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

          Float_t xPc3 = dPc3Cluster->get_xyz(0,kPc3);
          Float_t yPc3 = dPc3Cluster->get_xyz(1,kPc3);
          Float_t zPc3 = dPc3Cluster->get_xyz(2,kPc3);
          Float_t theta = DEGRAD*acos(zPc3/sqrt(xPc3*xPc3 + yPc3*yPc3 + zPc3*zPc3));
          Float_t phi = DEGRAD*atan2(yPc3, xPc3);
          if(phi < -90.0)
            phi += 360.0;

          Int_t pc3ghitID = dPc3GhitClus->get_ghitid(kPc3);  // get the PC3 GEANT hit sequence number
	  if(pc3ghitID < 0 || pc3ghitID >= pc3ghit->RowCount()) {
	    cerr << "\n PC3 ghitID error ";
	    cerr << ", kPc3 = " << kPc3 << ",  pc3ghitID = " << pc3ghitID;
	    cerr << ", pc3ghit->RowCount() = " << pc3ghit->RowCount() << endl;
	    exit(1);
	  } // safety check on pc3ghitID range

	  Float_t xPc3Geant = pc3ghit->get_xyzinglo(0,pc3ghitID);
	  Float_t yPc3Geant = pc3ghit->get_xyzinglo(1,pc3ghitID);
	  Float_t zPc3Geant = pc3ghit->get_xyzinglo(2,pc3ghitID);
          Float_t thetaGeant = DEGRAD*acos(zPc3Geant/sqrt(xPc3Geant*xPc3Geant +
							  yPc3Geant*yPc3Geant + zPc3Geant*zPc3Geant));
          Float_t phiGeant = DEGRAD*atan2(yPc3Geant, xPc3Geant);
          if(phiGeant < -90.0)
            phiGeant += 360.0;

	  pc_cluster[6] = theta;  // Theta value from PC Cluster
	  pc_cluster[7] = phi;    // Phi value from PC Cluster

	  pc_cluster[8] = thetaGeant;  // Theta value from GEANT hit
	  pc_cluster[9] = phiGeant;    // Phi value from GEANT hit

	  pc_cluster[10] = pc3ghit->get_mctrack(pc3ghitID);
          pc_cluster[11] = dPc3Cluster->get_type(kPc3);

          PCCluster->Fill(pc_cluster);

        } // loop over PC3 clusters

        //
        // Store PC1/PC2/PC3 Level2 evaluation using PC1 base
        //
	pc_level2[0] = 1;  // identify which PC as the base
        for(Int_t kPc1=0; kPc1<nPc1; kPc1++) {
          Int_t ipc123 = 1;  // counts clusters hits in PC2/PC3 present with same track number in PC1
	  Int_t mpc123 = 1;  // counts clusters in PC2/PC3 matched with same track number in PC2/PC3
	  Int_t gpc123 = 1;  // counts if there were PC2/PC3 GEANT hits matching PC1 GEANT track number

          //
          // Initialize the GEANT values
          //
          Float_t xPc1Geant = -9999.0;
          Float_t yPc1Geant = -9999.0;
          Float_t zPc1Geant = -9999.0;
          Float_t rPc1Geant = -9999.0;
	  Float_t xPc2Geant = -9999.0;
	  Float_t yPc2Geant = -9999.0;
	  Float_t zPc2Geant = -9999.0;
	  Float_t rPc2Geant = -9999.0;
          Float_t xPc3Geant = -9999.0;
          Float_t yPc3Geant = -9999.0;
          Float_t zPc3Geant = -9999.0;
          Float_t rPc3Geant = -9999.0;

          Int_t pc1Type = dPc1Cluster->get_type(kPc1);
          Int_t pc2Type = -999;
          Int_t pc3Type = -999;

          Int_t pc1Arm = dPc1Cluster->get_arm(kPc1);
          Int_t pc1ghitID = dPc1GhitClus->get_ghitid(kPc1);  // get the PC1 GEANT hit sequence number
	  if(pc1ghitID < 0 || pc1ghitID >= pc1ghit->RowCount()) {
	    cerr << "\n PC1 ghitID error";
	    cerr << ", kPc1 = " << kPc1 << ",  pc1ghitID = " << pc1ghitID;
	    cerr << ", pc1ghit->RowCount() = " << pc1ghit->RowCount() << endl;
	    exit(1);
	  } // safety check on pc1ghitID range

          Int_t mcTrack1 = pc1ghit->get_mctrack(pc1ghitID);  //retrieve the PC1 GEANT track number
          xPc1Geant = pc1ghit->get_xyzinglo(0,pc1ghitID);
          yPc1Geant = pc1ghit->get_xyzinglo(1,pc1ghitID);
          zPc1Geant = pc1ghit->get_xyzinglo(2,pc1ghitID);
          rPc1Geant = sqrt(xPc1Geant*xPc1Geant + yPc1Geant*yPc1Geant);

          //
          // Check if there is track match in any PC2 cluster (ipc123)
          //
          if(pc1Arm == 1) {
            for(Int_t kPc2=0; kPc2<nPc2; kPc2++) {
              Int_t pc2ghitID = dPc2GhitClus->get_ghitid(kPc2);  // get the PC2 GEANT hit sequence number
	      if(pc2ghitID < 0 || pc2ghitID >= pc2ghit->RowCount()) {
	        cerr << "\n PC2 ghitID error";
	        cerr << ", kPc2 = " << kPc2 << ",  pc2ghitID = " << pc2ghitID;
	        cerr << ", pc2ghit->RowCount() = " << pc2ghit->RowCount() << endl;
	        exit(1);
	      } // safety check on pc2ghitID range
              Int_t mcTrack2 = pc2ghit->get_mctrack(pc2ghitID);  //retrieve the PC2 GEANT track number
              if(mcTrack2 == mcTrack1) {
                ipc123 += 10;
		xPc2Geant = pc2ghit->get_xyzinglo(0,pc2ghitID);
		yPc2Geant = pc2ghit->get_xyzinglo(1,pc2ghitID);
		zPc2Geant = pc2ghit->get_xyzinglo(2,pc2ghitID) - zOrigin;
		rPc2Geant = sqrt(xPc2Geant*xPc2Geant + yPc2Geant*yPc2Geant);
                break;
              }  // check for GEANT track match
            }  // loop over PC2 GEANT association
          }  // search PC2 only in West Arm

          //
          // Check if there is track match in any PC3 cluster (ipc123)
          //
          for(Int_t kPc3=0; kPc3<nPc3; kPc3++) {
            Int_t pc3ghitID = dPc3GhitClus->get_ghitid(kPc3);  // get the PC3 GEANT hit sequence number
            if(pc3ghitID < 0 || pc3ghitID >= pc3ghit->RowCount()) {
              cerr << "\n PC3 ghitID error";
              cerr << ", kPc3 = " << kPc3 << ",  pc3ghitID = " << pc3ghitID;
              cerr << ", pc3ghit->RowCount() = " << pc3ghit->RowCount() << endl;
              exit(1);
            } // safety check on pc3ghitID range
            Int_t mcTrack3 = pc3ghit->get_mctrack(pc3ghitID);  //retrieve the PC3 GEANT track number
            if(mcTrack3 == mcTrack1) {
              ipc123 += 100;
              xPc3Geant = pc3ghit->get_xyzinglo(0,pc3ghitID);
              yPc3Geant = pc3ghit->get_xyzinglo(1,pc3ghitID);
              zPc3Geant = pc3ghit->get_xyzinglo(2,pc3ghitID);
              rPc3Geant = sqrt(xPc3Geant*xPc3Geant + yPc3Geant*yPc3Geant);
              break;
            }  // check for GEANT track match
          }  // loop over PC3 GEANT association

          Int_t idPart1;
          Int_t idParent1;
          Int_t itParent1;
          Float_t rVertex1;
          Float_t zVertex1;

          Float_t pTot1;
          Float_t pPhi;
          Float_t pTheta;
          Int_t nFile1;
          Int_t itOrigin;
          Int_t idOrigin1;
          GeaTrkStack(mcTrack1, idPart1, idParent1, pTot1, rVertex1, zVertex1, pTheta, pPhi, nFile1,
                      itParent1, itOrigin, idOrigin1);

          Float_t xPc1 = dPc1Cluster->get_xyz(0,kPc1);
          Float_t yPc1 = dPc1Cluster->get_xyz(1,kPc1);
	  Float_t rPc1 = sqrt(xPc1*xPc1 + yPc1*yPc1);
          Float_t zPc1 = dPc1Cluster->get_xyz(2,kPc1) - zOrigin;
          Float_t theta1 = DEGRAD*acos(zPc1/sqrt(xPc1*xPc1 + yPc1*yPc1 + zPc1*zPc1));
          Float_t phi1 = DEGRAD*atan2(yPc1, xPc1);
          if(phi1 < -90.0)
            phi1 += 360.0;

          // Search for closest PC2 cluster
          Float_t theta2min = -9999.0;
          Float_t phi2min = -9999.0;
          Float_t delmin = +9999.0;
	  Float_t rPc2min = -9999.0;
	  Float_t xPc2min = -9999.0;
	  Float_t yPc2min = -9999.0;
	  Float_t zPc2min = -9999.0;
          Int_t kPc2min = -1;
          if(phi1 < 90.0) {
            for(Int_t kPc2=0; kPc2<nPc2; kPc2++) {
              Float_t xPc2 = dPc2Cluster->get_xyz(0,kPc2);
              Float_t yPc2 = dPc2Cluster->get_xyz(1,kPc2);
              Float_t zPc2 = dPc2Cluster->get_xyz(2,kPc2) - zOrigin;
              Float_t theta2 = DEGRAD*acos(zPc2/sqrt(xPc2*xPc2 + yPc2*yPc2 + zPc2*zPc2));
              Float_t phi2 = DEGRAD*atan2(yPc2, xPc2);
              if(phi2 < -90.0)
                phi2 += 360.0;

              if(abs(theta1-theta2) + abs(phi1-phi2) < delmin) {
                theta2min = theta2;
                phi2min = phi2;
                delmin = abs(theta1-theta2) + abs(phi1-phi2);
		xPc2min = xPc2;
		yPc2min = yPc2;
		zPc2min = zPc2;
		rPc2min = sqrt(xPc2*xPc2 + yPc2*yPc2);
		kPc2min = kPc2;
                pc2Type = dPc2Cluster->get_type(kPc2);
              } // check for new minimum
            } // closest PC2 search
          } //  check on West Arm for PC2 search

          // Search for closest PC3 cluster
          Float_t theta3min = -9999.0;
          Float_t phi3min = -9999.0;
	  Float_t rPc3min = -9999.0;
	  Float_t xPc3min = -9999.0;
	  Float_t yPc3min = -9999.0;
	  Float_t zPc3min = -9999.0;
          delmin = +9999.0;
          Int_t kPc3min = -1;
          for(Int_t kPc3=0; kPc3<nPc3; kPc3++) {
            Int_t pc3Arm = dPc3Cluster->get_arm(kPc3);
            if(pc3Arm != pc1Arm)
              continue;  // skip this cluster if the Arm does not match

            Float_t xPc3 = dPc3Cluster->get_xyz(0,kPc3);
            Float_t yPc3 = dPc3Cluster->get_xyz(1,kPc3);
            Float_t zPc3 = dPc3Cluster->get_xyz(2,kPc3) - zOrigin;
            Float_t theta3 = DEGRAD*acos(zPc3/sqrt(xPc3*xPc3 + yPc3*yPc3 + zPc3*zPc3));
            Float_t phi3 = DEGRAD*atan2(yPc3, xPc3);
            if(phi3 < -90.0)
              phi3 += 360.0;

            if(abs(theta1-theta3) + abs(phi1-phi3) < delmin) {
              theta3min = theta3;
              phi3min = phi3;
              delmin = abs(theta1-theta3) + abs(phi1-phi3);
	      xPc3min = xPc3;
	      yPc3min = yPc3;
	      zPc3min = zPc3;
	      rPc3min = sqrt(xPc3*xPc3 + yPc3*yPc3);
              kPc3min = kPc3;
              pc3Type = dPc3Cluster->get_type(kPc3);
            } // check for new minimum
          } // closest PC3 search

          Int_t kTrack2 = -1;
          Int_t idPart2 = -1;
          if(kPc2min != -1) {
            Int_t pc2ghitID = dPc2GhitClus->get_ghitid(kPc2min);  // get the PC2 GEANT hit sequence number
            if(pc2ghitID < 0 || pc2ghitID >= pc2ghit->RowCount()) {
              cerr << "\n PC2 ghitID error ";
              cerr << ", kPc2min = " << kPc2min << ",  pc2ghitID = " << pc2ghitID;
              cerr << ", pc2ghit->RowCount() = " << pc2ghit->RowCount() << endl;
              exit(1);
            } // safety check on pc2ghitID range

            kTrack2 = pc2ghit->get_mctrack(pc2ghitID);  //retrieve the PC2 GEANT track number
          }  // check on valid PC2 match
	  if(kTrack2 == mcTrack1) {
	    mpc123 += 10;
	    gpc123 += 10; // this tells us alrady that there was a matching GEANT hit, no need to search
	  }
	  else {
	    for(Int_t gpc2=0; gpc2<pc2ghit->RowCount(); gpc2++) {
	      if(mcTrack1 == pc2ghit->get_mctrack(gpc2)) {
		gpc123 += 10;
		break;
	      } // check for GEANT hit match in PC2 with PC1
	    } // search on all PC2 GEANT hits
	  } // check if the matched PC2 cluster had the right track number

          Int_t kTrack3 = -1;
          Int_t idPart3 = -1;
          if(kPc3min != -1) {
            Int_t pc3ghitID = dPc3GhitClus->get_ghitid(kPc3min);  // get the PC3 GEANT hit sequence number
            if(pc3ghitID < 0 || pc3ghitID >= pc3ghit->RowCount()) {
              cerr << "\n PC3 ghitID error ";
              cerr << ", kPc3min = " << kPc3min << ",  pc3ghitID = " << pc3ghitID;
              cerr << ", pc3ghit->RowCount() = " << pc3ghit->RowCount() << endl;
              exit(1);
            } // safety check on pc3ghitID range

            kTrack3 = pc3ghit->get_mctrack(pc3ghitID);  //retrieve the PC3 GEANT track number
          }  // check on valid PC3 match
	  if(kTrack3 == mcTrack1) {
	    mpc123 += 100;
	    gpc123 += 100; // this tells us alrady that there was a matching GEANT hit, no need to search
	  }
	  else {
	    for(Int_t gpc3=0; gpc3<pc3ghit->RowCount(); gpc3++) {
	      if(mcTrack1 == pc3ghit->get_mctrack(gpc3)) {
		gpc123 += 100;
		break;
	      } // check for GEANT hit match in PC3 with PC1
	    } // search on all PC3 GEANT hits
	  } // check if the matched PC3 cluster had the right track number

	  Float_t pc2DelZ = -99999.0;
	  Float_t pc2DelY = -99999.0;
	  Float_t delYGea = -99999.0;
	  Float_t delZGea = -99999.9;
	  if(pc1Arm==1 && rPc2Geant>0.0 && kPc2min>-1 && kPc3min>-1) {
	    Float_t rSlope = (zPc3min - zPc1)/(rPc3min - rPc1);
	    Float_t zIntercept = zPc3min - rSlope*rPc3min;
	    pc2DelZ = rSlope*rPc2min + zIntercept - zPc2Geant;
	    Float_t xSlope = (yPc3min - yPc1)/(xPc3min - xPc1);
	    Float_t yIntercept = yPc3min - xSlope*xPc3min;
	    pc2DelY = xSlope*xPc2min + yIntercept - yPc2Geant;
	    delZGea = zPc2min - zPc2Geant;
            delYGea = yPc2min - yPc2Geant;
	  }  // check on having a true PC2 cluster
	  
	  pc_level2[1] = mcTrack1;
	  pc_level2[2] = idPart1;
	  pc_level2[3] = kTrack2;
	  pc_level2[4] = kTrack3;
	  pc_level2[5] = mpc123;  // checks if the matched PC2/PC3 clusters have same track number as PC1
	  pc_level2[6] = gpc123;  // checks if there were GEANT hits in PC2/PC3 matching track number in PC1

          pc_level2[7] = theta1;
          pc_level2[8] = theta2min;
          pc_level2[9] = theta3min;
          pc_level2[10] = phi1;
          pc_level2[11] = phi2min;
          pc_level2[12] = phi3min;

	  pc_level2[13] = ipc123; // checks if there were PC2/PC3 clusters matching track number in PC1
	  pc_level2[14] = pTot1;

	  pc_level2[15] = pc2DelZ;
	  pc_level2[16] = pc2DelY;
	  pc_level2[17] = delZGea;
	  pc_level2[18] = delYGea;

	  pc_level2[19] = idParent1;
	  pc_level2[20] = idOrigin1;
	  pc_level2[21] = rVertex1;
	  pc_level2[22] = zVertex1;
	  pc_level2[23] = nFile1;

          pc_level2[24] = xPc1;
          pc_level2[25] = yPc1;
          pc_level2[26] = zPc1;
          pc_level2[27] = xPc1Geant;
          pc_level2[28] = yPc1Geant;
          pc_level2[29] = zPc1Geant;

          pc_level2[30] = xPc2min;  // cluster X value at PC2 for the closest PC2 match
          pc_level2[31] = yPc2min;
          pc_level2[32] = zPc2min;
          pc_level2[33] = xPc2Geant;
          pc_level2[34] = yPc2Geant;
          pc_level2[35] = zPc2Geant;

          pc_level2[36] = xPc3min;
          pc_level2[37] = yPc3min;
          pc_level2[38] = zPc3min;
          pc_level2[39] = xPc3Geant;
          pc_level2[40] = yPc3Geant;
          pc_level2[41] = zPc3Geant;

          pc_level2[42] = yPc1 - yPc1Geant;
          pc_level2[43] = zPc1 - zPc1Geant;

          //
          // Compute the Y distance error and the Z distance error based on projected line
          // The projected line is from the original vertex to PC3
          // In the XY plane the line has the form Y = aSlope*X  where aSlope is the XY slope
          // In the RZ plane the line has the form Z = bSlope*R + Z0EVENT where bSlope is the RZ slope
          // There are lines based on the GEANT values and lines based on the cluster values
          //
          
          //
          // First look at difference at PC1 based on GEANT line projections
          //
          Float_t aSlopeGeant = -99999.0;
          pc_level2[44] = -99999.0;
          if(yPc3Geant > -9000.0) {
            aSlopeGeant = yPc3Geant/xPc3Geant;
            pc_level2[44] = yPc1Geant - aSlopeGeant*xPc1Geant;
          } // check on valid yPc3Geant;

          Float_t bSlopeGeant = -99999.0;
          pc_level2[45] = -99999.0;
          if(zPc3Geant > -9000.0) {
            bSlopeGeant = (zPc3Geant - z0Event)/rPc3Geant;  // using true Z0 value
            pc_level2[45] = zPc1Geant - bSlopeGeant*rPc1Geant - z0Event;
          } // check on valid zPc3Geant;

          //
          // Next look at difference at PC1 based on Cluster line projections
          //
          Float_t aSlopeCluster = -99999.0;
          pc_level2[46] = -99999.0;
          if(yPc3min > -9000.0) {
            aSlopeCluster = yPc3min/xPc3min;
            pc_level2[46] = yPc1 - aSlopeCluster*xPc1;
          } // check on valid yPc3min;

          Float_t bSlopeCluster = -99999.0;
          pc_level2[47] = -99999.0;
          if(zPc3min > -9000.0) {
            bSlopeCluster = (zPc3min - zOrigin)/rPc3min;  // using BBC vertex value (GEANT for single particles)
            pc_level2[47] = zPc1 - bSlopeCluster*rPc1 - zOrigin;
          } // check on valid zPc3min;

          pc_level2[48] = pc1Type;
          pc_level2[49] = pc2Type;
          pc_level2[50] = pc3Type;

          PCLevel2->Fill(pc_level2);

        } // loop over accepted tracks

      }  // eventGood

    // Reset all data for this event
    //mainIter.print();
    mainIter.cd();
    if (mainIter.cd("DST")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("DCM")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("GEA")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("EVA")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("BBC")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("MVD")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("PAD")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("CRK")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("TEC")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("EMC")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("TOF")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("CGL")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("DCH")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("MOM")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("CGE")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }

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
