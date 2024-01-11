void camDstCheck(Int_t maxEvents=20, Int_t kFiles=1, const char *dstIFile="dstData.root",
                 Int_t debugOut=20, Int_t skipEvents=0, Float_t bbcZMin=-30.0, Float_t bbcZMax=+30.0, 
		 Int_t takePC1=3, Int_t takePC2=0, Int_t takePC3=0, Int_t crkFix=1, Int_t emcFix=1,
		 Float_t z0Input=-9999.0, Int_t flipPC1West==0, Int_t flipPC2==0, Int_t flipPC3West==0) {

/*
  DST evaluation macro
  Author: Charles F. Maguire
  Creation Date: May 19, 2001

  Input arguments
        maxEvents  total number events to be processed in a given input file
        kFiles     number of input files to be processed after the first input file, 
                   then the next input files are assumed to have names dstData01.root,
                   dstData02.root, etc.

        dstIFile   name of first DST input file
        debugOut   number of events for which to have debug printing

  Calling Map
        camDstIni.C and camDstNode.C are called for initialization, and re-initialization

  Method
        looks at correlated angular positions in PCs

  Qualifications
       There can be maximum of 99 input files.  After the first, these will have names
       dstDatann.root where nn goes from 01 to 99

  Output
       An output NTUPLE file camDstCheck.root is created 

  Revision history
    May 20, 2001     C.F. Maguire  Put number of Dch tracks into PC NTUPLEs
                                   Add summary counts for the other subsystems

    May 21, 2001     C.F. Maguire  Remove Dch count from individual subsystem logs
                                   for better clarity in differences check
                                   Put in check for dPhDchTrack Table

    June 11, 2001    C.F. Maguire  Describe the debugOut parameter

    June 13, 2001    C.F. Maguire  Add PC2, MVD, CRK, MuTr and MuID reports

    July 8, 2001     C.F. Maguire  Add Muon Arm tracks NTUPLE

    July 13, 2001    C.F. Maguire  Fix North South summary for TEC and TOF

    July 14, 2001    C.F. Maguire  Change event looping logic

    July 18, 2001    C.F. Maguire, Hugo Valle, Barbara Truett put in z0Beam
                                   to correct for collision Z vertex
                                   Put z0Beam and nTECTrack into NTUPLEs
				   Put in option for which PCs base to correlate
				   Put in check for East Arm (no PC2) in searches
				   Put in BBC min and max values for taking event
				   Put in protections against missing subsystems
				   Put in protections against divide by 0

    July 20, 2001    Hugo Valle, C.F. Maguire put in PC2 - PC3 Z0 determination
                                   Fix PC1 PC3 Z0 determination to look only in same Arm

    July 24, 2001    C.F. Maguire  Put in summary NTUPLE for checking V03 and V05 DSTs

    July 26, 2001    B. Truett, H. Valle, I. Ojha, C. Maguire  put in sectors for the
                                   NTUPLEs, fixed PCCluster for IPC=2 and IPC=3,
                                   put in IPC=2 for PCRaw NTUPLE, using fabs instead of abs

    July 27, 2001    C.F. Maguire  Add extra index variables and (R,Z) coordinates to
                                   PCZ0 NTUPLE; put in forced z0Input for Z0BEAM on
                                   single event studies; fix bug for PC3 based closest PC2 loop
				   
    July 28, 2001    C.F. Maguire  Add PC1/PC2/PC3 best straight tracks algorithm for getting
                                   Z0 from all three PCs in the West Arm; add "flip" flags for
                                   later "flip and swap" studies

    July 30, 2001    C.F. Maguire  Add results of PadVertexFunction checks to EVTSUM and PCZ0 NTUPLEs

*/

  if(kFiles>99) {
    cerr << "\n Number of input files " << kFiles;
    cerr << " exceeds maximum allowed value of 99" << endl;
    return;
  } // safety check

  const float DEGRAD = 57.295779513;

  static TFile *DstFile = 0;
  DstFile = new TFile("camDstCheck.root", "recreate", "Checking preco DSTs");

  const int EVENT_SUMMARY = 55;
  float event_summary[EVENT_SUMMARY];
  static TNtuple *EvtSum = 0;
  EvtSum = new TNtuple("EvtSum", "Event Summary",
		       "DCHEAST:DCHWEST:DCHNORTH:DCHSOUTH:"//
		       "PHDCHGOOD:PHDCHFAIL:"//
		       "PC1EAST:PC1WEST:PC1NORTH:PC1SOUTH:"//
		       "PC2EAST:PC2WEST:PC2NORTH:PC2SOUTH:"//
		       "PC3EAST:PC3WEST:PC3NORTH:PC3SOUTH:"//
		       "TECNORTH:TECSOUTH:TOFNORTH:TOFSOUTH:"//
		       "EMCEAST:EMCWEST:EMCNORTH:EMCSOUTH:"//
                       "CGLPC1:CGLPC2:CGLPC3:CGLTEC:CGLTOF:CGLEMC:"//
                       "ZPC1PC3E:ZPC1PC3W:ZPC1PC2W:ZPC2PC3W:ZPCCOMBN:ZPC123W:"//
		       "ZDCZVERT:ZDCENERS:ZDCENERN:ZDCNPMT:"//
		       "RAWTRIG:SCALTRIG:LIVETRIG:"//
		       "Z0EVENT:NTCTRACK:BBCNORTH:BBCSOUTH:BBCZVERT:"//
		       "NDCTRACK:NCLUSPC1:NCLUSPC3:IFILE:EVENT");

  const int PC_RAW = 18;
  float pc_raw[PC_RAW];
  static TNtuple *PCRaw= 0;
  PCRaw = new TNtuple("PCRaw", "PC Raw Coordinates",
                          "IPC:KRAW:ARM:SECTOR:SIDE:PADX:PADZ:PADTYPE:"//
                          "Z0EVENT:NTCTRACK:BBCNORTH:BBCSOUTH:BBCZVERT:"//
                          "NDCTRACK:NCLUSPC1:NCLUSPC3:IFILE:EVENT");

  const int PC_CLUSTER = 25;
  float pc_cluster[PC_CLUSTER];
  static TNtuple *PCCluster= 0;
  PCCluster = new TNtuple("PCCluster", "PC Cluster Coordinates",
                          "IPC:KCLUSTER:ARM:CLUSTERX:CLUSTERY:CLUSTERZ:"//
                          "THETA1:PHI1:THETA2:PHI2:THETA3:PHI3:"//
			  "SECTOR1:SECTOR2:SECTOR3:"//
                          "Z0BEAM:NTCTRACK:BBCNORTH:BBCSOUTH:BBCZVERT:"//
                          "NDCTRACK:NCLUSPC1:NCLUSPC3:IFILE:EVENT");

  const int PC_Z0 = 38;
  float pc_z0[PC_Z0];
  static TNtuple *PCZ0= 0;
  PCZ0 = new TNtuple("PCZ0", "PC Determination of Z0",
		     "IPCIN:KCLUSTER:ARM:CLUSTERX:CLUSTERY:CLUSTERZ:"//
		     "THETAIN:PHIIN:THETAOUT:PHIOUT:IPCOUT:Z0PC:"//
                     "SECTIN:SECTOUT:KPCIN:KPCOUT:RIN:ZIN:ROUT:ZOUT:PC2ZDIFF:PC2ZINTER:"//
		     "ZPC1PC3E:ZPC1PC3W:ZPC1PC2W:ZPC2PC3W:ZPCCOMBN:ZPC123W:"//
		     "Z0BEAM:NTCTRACK:BBCNORTH:BBCSOUTH:BBCZVERT:"//
		     "NDCTRACK:NCLUSPC1:NCLUSPC3:IFILE:EVENT");

  const int MUON_TRACK = 19;
  float muon_track[MUON_TRACK];
  static TNtuple *MuonTrack= 0;
  MuonTrack = new TNtuple("MuonTrack", "Muon Track Infomation",
                          "ITRACK:PX:PY:PZ:CHISQUARE:CHARGE:NHITS:PTOT:"//
                          "NMUOTRACK:Z0EVENT:NTCTRACK:BBCNORTH:BBCSOUTH:BBCZVERT:"//
                          "NDCTRACK:NCLUSPC1:NCLUSPC3:IFILE:EVENT");


  gROOT->Macro("camDstIni.C");
  char *dstDataxx = "dstData00.root";
  char digits[10] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'};

  Int_t firstZdc = 1;
  Int_t firstMvd = 1;
  Int_t firstDch = 1;
  Int_t firstPhd = 1;
  Int_t firstCrkHit = 1;
  Int_t firstCrkRaw = 1;
  Int_t firstCgl = 1;
  Int_t firstTec = 1;
  Int_t firstTof = 1;
  Int_t firstEmc = 1;
  Int_t firstMuo = 1;
  Int_t firstMui = 1;

  Int_t firstFlipPC1West = 1;
  Int_t firstFlipPC2 = 1;
  Int_t firstFlipPC3West = 1;

  //
  // Loop over sequential input files 
  //
  for (Int_t iFile=0; iFile<kFiles; iFile++) {
    if(iFile>0) {
      //
      // Determine new file name, after first file is finished
      //
      Int_t iUnit = iFile%10;
      Int_t iTen = iFile/10;
      dstDataxx[7] = digits[iTen];
      dstDataxx[8] = digits[iUnit];
      dstInFile = dstDataxx;

      gROOT->Macro("camDstNode.C");
      cout << "\n Opened DST input file " << dstInFile << endl;

    }  // check on file not being the first file

    // Time for the event loop
    Int_t eventNumber = 1; 
    Int_t eventLast = 0;
    while (eventNumber<=maxEvents && !eventLast) {

      if (eventNumber>skipEvents) {

        if (!dEventHeaderNode) {
          cout << "Dst Read: Could not find data node dEventHeader" << endl;
        } else {
          dEventHeaderWrapper* dEventHeader = (dEventHeaderWrapper*)dEventHeaderNode->getData();
        }

	UInt_t rawTrig = -1;
	UInt_t scaledTrig = -1;
	UInt_t liveTrig = -1;
	cout << "\n     Read event " << eventNumber;
	if(dEventHeaderNode && dEventHeader->RowCount()>0) {
	  rawTrig = dEventHeader->get_trigRaw(0,0);
	  cout << ", trigRaw[0] " << hex << rawTrig;
	  scaledTrig = dEventHeader->get_trigScaled(0,0);
	  cout << ", trigScaled[0] " << scaledTrig;
	  liveTrig = dEventHeader->get_trigLive(0,0);
	  cout << ", trigLive[0] " << liveTrig;
	  cout << dec;
	} // check on EventHeader being present
	cout << endl;

	// Obtain the BBC output and the PC Cluster nodes

        if (!dBbcOutNode) {
          cout << "Dst Read: Could not find data node dBbcOut" << endl;
        } else {
          dBbcOutWrapper* dBbcOut = (dBbcOutWrapper*)dBbcOutNode->getData();
        }

        if (!ZdcOutNode) {
	  if(firstZdc) {
	    cout << "Dst Read: Could not find data node ZdcOut" << endl;
	    firstZdc = 0;
	  }
	} else {
          ZdcOutv1* ZdcOut = (ZdcOutv1*)ZdcOutNode->getData();
        }

        if (!dMvdVertexOutNode) {
	  if(firstMvd) {
	    cout << "Dst Read: Could not find data node dMvdVertexOut" << endl;
	    firstMvd = 0;
	  }
        } else {
          dMvdVertexOutWrapper* dMvdVertexOut = (dMvdVertexOutWrapper*)dMvdVertexOutNode->getData();
        }

        if (!dCrkHitNode) {
	  if(firstCrkHit) {
	    cout << "Dst Read: Could not find data node dCrkHit" << endl;
	    firstCrkHit = 0;
	  }
        } else {
          dCrkHitWrapper* dCrkHit = (dCrkHitWrapper*)dCrkHitNode->getData();
        }

        if (!dCrkRawNode) {
	  if(firstCrkRaw) {
	    cout << "Dst Read: Could not find data node dCrkRaw" << endl;
	    firstCrkRaw = 0;
	  }
        } else {
          dCrkRawWrapper* dCrkRaw = (dCrkRawWrapper*)dCrkRawNode->getData();
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

        if (!dCglTrackNode) {
	  if(firstCgl) {
	    cout << "Dst Read: Could not find data node dCglTrack" << endl;
	    firstCgl = 0;
	  }
        } else {
          dCglTrackWrapper* dCglTrack = (dCglTrackWrapper*)dCglTrackNode->getData();
        }

       if (!dDchTracksNode) {
	 if(firstDch) {
	   cout << "Dst Read: Could not find data node dDchTracks" << endl;
	   firstDch = 0;
	 }
        } else {
          dDchTracksWrapper* dDchTracks = (dDchTracksWrapper*)dDchTracksNode->getData();
        }

       if (!dPHDchTrackNode) {
	 if(firstPhd) {
	   cout << "Dst Read: Could not find data node dPHDchTrack" << endl;
	   firstPhd = 0;
	 }
        } else {
          dPHDchTrackWrapper* dPHDchTrack = (dPHDchTrackWrapper*)dPHDchTrackNode->getData();
        }

       if (!dTecTrackNode) {
	 if(firstTec) {
	   cout << "Dst Read: Could not find data node dTecTrack" << endl;
	   firstTec = 0;
	 }
        } else {
          dTecTrackWrapper* dTecTrack = (dTecTrackWrapper*)dTecTrackNode->getData();
        }

       if (!dTofReconstructedNode) {
	 if(firstTof) {
          cout << "Dst Read: Could not find data node dTofReconstructed" << endl;
	  firstTof = 0;
	 }
      } else {
          dTofReconstructedWrapper* dTofReconstructed = (dTofReconstructedWrapper*)dTofReconstructedNode->getData();
       }

       if (!dEmcClusterLocalExtNode) {
	 if(firstEmc) {
	   cout << "Dst Read: Could not find data node dEmcClusterLocalExt" << endl;
	   firstEmc = 0;
	 }
       } else {
          dEmcClusterLocalExtWrapper* dEmcClusterLocalExt = (dEmcClusterLocalExtWrapper*)dEmcClusterLocalExtNode->getData();
       }

       if (!dMuoTracksNode) {
	 if(firstMuo) {
	   cout << "Dst Read: Could not find data node dMuoTracks" << endl;
	   firstMuo = 0;
	 }
       } else {
          dMuoTracksWrapper* dMuoTracks = (dMuoTracksWrapper*)dMuoTracksNode->getData();
       }

       if (!dMuiRoadsNode) {
	 if(firstMui) {
          cout << "Dst Read: Could not find data node dMuiRoads" << endl;
	  firstMui = 0;
	 }
       } else {
          dMuiRoadsWrapper* dMuiRoads = (dMuiRoadsWrapper*)dMuiRoadsNode->getData();
       }

        event_summary[EVENT_SUMMARY - 1] = eventNumber;
        event_summary[EVENT_SUMMARY - 2] = iFile;
	for(Int_t kSum=0; kSum<EVENT_SUMMARY-2; kSum++) {
	  event_summary[kSum] = 0;
	} // initialize summary NTUPLE

	Float_t bbcZVertex = dBbcOut->get_VertexPoint(0);

        if(eventNumber<=debugOut)  {
          cout << "\n Summary event " << eventNumber;
          cout << ", BBC Z0 = " << bbcZVertex;
	  if(ZdcOutNode && ZdcOut)
	    cout << ", ZDC Z0 = " << ZdcOut->get_Zvertex();
          if(dDchTracksNode)
	    cout << ", Dch = " << dDchTracks->RowCount();
          cout << ", PC1 = " << dPc1Cluster->RowCount();
          cout << ", PC2 = " << dPc2Cluster->RowCount();
          cout << ", PC3 = " << dPc3Cluster->RowCount();
          if(dCrkHitNode)
	    cout << ", CRK = " << dCrkHit->RowCount();
          if(dTecTrackNode)
	    cout << ", TEC = " << dTecTrack->RowCount();
          if(dTofReconstructedNode)
	    cout << ", TOF = " << dTofReconstructed->RowCount();
          if(dEmcClusterLocalExtNode)
	    cout << ", EMC = " << dEmcClusterLocalExt->RowCount();
	  if(dMuoTracksNode && dMuoTracks->RowCount())
	    cout << ", MUO = " << dMuoTracks->RowCount();
	  if(dMuiRoadsNode && dMuiRoads->RowCount())
	    cout << ", MUI = " << dMuiRoads->RowCount();
          cout << endl;
        }  // check on debug print for this event
	
	cout << endl;

	Float_t phiCut = 0.2;
        Float_t z0PC[6];
        Int_t fault[6];
	fault[0] = 0;
        PadVertexFunction(dPc1Cluster, dPc2Cluster, dPc3Cluster, phiCut, z0PC, fault);

	/*
	cout << "\n Return " << z0PC[0];  // PC1/PC3 East Arm
        cout << ",  " << z0PC[1];         // PC1/PC3 West Arm
        cout << ",  " << z0PC[2];         // PC1/PC2 West Arm
        cout << ",  " << z0PC[3];         // PC2/PC3 West Arm
        cout << ",  " << z0PC[4];         // all PCm/PCn combined
        cout << ",  " << z0PC[5];         // best PC1/PC2/PC3 West Arm tracks
	cout << endl;
	*/
	if(eventNumber<=debugOut && dMvdVertexOutNode && dMvdVertexOut->RowCount()>0) {
	  cout << " MVD report event " << eventNumber;
	  cout << ", X0 = " << dMvdVertexOut->get_vertex(0,0);
	  cout << ", Y0 = " << dMvdVertexOut->get_vertex(1,0);
	  cout << ", Z0 = " << dMvdVertexOut->get_vertex(2,0);
          cout << ", BBC Z0 = " << bbcZVertex;
          cout << endl; 
	} // MVD report

        if(eventNumber<=debugOut && dDchTracksNode && dDchTracks->RowCount()>0) {
          Int_t zZero = 0;
          Int_t nNorth = 0;
          Int_t nSouth = 0;
          Int_t nEast = 0;
          Int_t nWest = 0;
          for(Int_t iRow=0; iRow<dDchTracks->RowCount(); iRow++) {
            if(dDchTracks->get_point(0,iRow)>0)
              nWest++;
            else
              nEast++;

            if(dDchTracks->get_point(2,iRow)>0)
              nNorth++;
            else
              nSouth++;

            if(fabs(dDchTracks->get_point(2,iRow))<0.10)
              zZero++;

          } // loop over table rows
          cout << "  Dch report event " << eventNumber;
          cout << ", N = " << nNorth << ", S = " << nSouth; 
          cout << ", W = " << nWest << ", E = " << nEast;
	  event_summary[0] = nEast;
	  event_summary[1] = nWest;
	  event_summary[2] = nNorth;
	  event_summary[3] = nSouth;
          Float_t x0 = dDchTracks->get_point(0,0);
          Float_t y0 = dDchTracks->get_point(1,0);
          Float_t z0 = dDchTracks->get_point(2,0);
          Float_t theta = DEGRAD*acos(z0/sqrt(x0*x0 + y0*y0 + z0*z0));
          Float_t phi = DEGRAD*atan2(y0, x0);
          if(phi < -90.0)
            phi += 360.0;
          cout << ", th0 = " << theta << ", ph0 = " << phi;
          cout << ", BBC Z0 = " << bbcZVertex;
          cout << ", zZero = " << zZero;
          cout << endl; 
        } // check on Dch

        if(eventNumber<=debugOut && dPHDchTrackNode && dPHDchTrack->RowCount()>0) {
          Int_t iFail = 0;
          Int_t iSuccess = 0;
          for(Int_t iRow=0; iRow<dPHDchTrack->RowCount(); iRow++) {
            if(dPHDchTrack->get_ErrorCode(iRow) == 0) 
              iSuccess++;
            else
              iFail++;
          } // loop over Table rows
	  event_summary[4] = iSuccess;
	  event_summary[5] = iFail;
          cout << "  PHDch report event " << eventNumber;
          cout << ", successes = " << iSuccess << ", failures = " << iFail;
          /*
            The Theta0 and the Phi0 are always slightly different in ROOT3.00 ??
          cout << ", th0 = " << DEGRAD*dPHDchTrack->get_fittedTheta0(0); 
          cout << ", ph0 = " << DEGRAD*dPHDchTrack->get_fittedPhi0(0); 
          */
          cout << ", BBC Z0 = " << bbcZVertex;
          cout << endl; 
        } // check on PHDchTrack

        if(eventNumber<=debugOut && dPc1Cluster->RowCount()>0) {
          Int_t nNorth = 0;
          Int_t nSouth = 0;
          Int_t nEast = 0;
          Int_t nWest = 0;
          for(Int_t iRow=0; iRow<dPc1Cluster->RowCount(); iRow++) {
            if(dPc1Cluster->get_xyz(0,iRow)>0)
              nWest++;
            else
              nEast++;

            if(dPc1Cluster->get_xyz(2,iRow)>0)
              nNorth++;
            else
              nSouth++;
          } // loop over table rows
          cout << "  PC1 report event " << eventNumber;
          cout << ", N = " << nNorth << ", S = " << nSouth; 
          cout << ", W = " << nWest << ", E = " << nEast;
	  event_summary[6] = nEast;
	  event_summary[7] = nWest;
	  event_summary[8] = nNorth;
	  event_summary[9] = nSouth;
          Float_t x0 = dPc1Cluster->get_xyz(0,0);
          Float_t y0 = dPc1Cluster->get_xyz(1,0);
          Float_t z0 = dPc1Cluster->get_xyz(2,0);
          Float_t theta = DEGRAD*acos(z0/sqrt(x0*x0 + y0*y0 + z0*z0));
          Float_t phi = DEGRAD*atan2(y0, x0);
          if(phi < -90.0)
            phi += 360.0;
          cout << ", th0 = " << theta << ", ph0 = " << phi;
          cout << ", BBC Z0 = " << bbcZVertex;
          cout << endl; 
        } // check on PC1

        if(eventNumber<=debugOut && dPc2Cluster->RowCount()>0) {
          Int_t nNorth = 0;
          Int_t nSouth = 0;
          Int_t nEast = 0;
          Int_t nWest = 0;
          for(Int_t iRow=0; iRow<dPc2Cluster->RowCount(); iRow++) {
            if(dPc2Cluster->get_xyz(0,iRow)>0)
              nWest++;
            else
              nEast++;

            if(dPc2Cluster->get_xyz(2,iRow)>0)
              nNorth++;
            else
              nSouth++;
          } // loop over table rows
	  event_summary[10] = nEast;
	  event_summary[11] = nWest;
	  event_summary[12] = nNorth;
	  event_summary[13] = nSouth;
          cout << "  PC2 report event " << eventNumber;
          cout << ", N = " << nNorth << ", S = " << nSouth; 
          cout << ", W = " << nWest << ", E = " << nEast;
          Float_t x0 = dPc2Cluster->get_xyz(0,0);
          Float_t y0 = dPc2Cluster->get_xyz(1,0);
          Float_t z0 = dPc2Cluster->get_xyz(2,0);
	  Float_t d0 = sqrt(x0*x0 + y0*y0 + z0*z0);
	  Float_t theta = -888.0;
          Float_t phi = -888.0;
	  if(d0 > 0.0) {
	    theta = DEGRAD*acos(z0/d0);
	    phi = DEGRAD*atan2(y0, x0);
	    if(phi < -90.0)
	      phi += 360.0;
	  } // check against divide by 0

	  cout << ", th0 = " << theta << ", ph0 = " << phi;
	  cout << ", BBC Z0 = " << bbcZVertex;
          cout << endl; 
        } // check on PC2

        if(eventNumber<=debugOut && dPc3Cluster->RowCount()>0) {
          Int_t nNorth = 0;
          Int_t nSouth = 0;
          Int_t nEast = 0;
          Int_t nWest = 0;
          for(Int_t iRow=0; iRow<dPc3Cluster->RowCount(); iRow++) {
            if(dPc3Cluster->get_xyz(0,iRow)>0)
              nWest++;
            else
              nEast++;

            if(dPc3Cluster->get_xyz(2,iRow)>0)
              nNorth++;
            else
              nSouth++;
          } // loop over table rows
	  event_summary[14] = nEast;
	  event_summary[15] = nWest;
	  event_summary[16] = nNorth;
	  event_summary[17] = nSouth;
          cout << "  PC3 report event " << eventNumber;
          cout << ", N = " << nNorth << ", S = " << nSouth; 
          cout << ", W = " << nWest << ", E = " << nEast;
          Float_t x0 = dPc3Cluster->get_xyz(0,0);
          Float_t y0 = dPc3Cluster->get_xyz(1,0);
          Float_t z0 = dPc3Cluster->get_xyz(2,0);
          Float_t theta = DEGRAD*acos(z0/sqrt(x0*x0 + y0*y0 + z0*z0));
          Float_t phi = DEGRAD*atan2(y0, x0);
          if(phi < -90.0)
            phi += 360.0;
          cout << ", th0 = " << theta << ", ph0 = " << phi;
          cout << ", BBC Z0 = " << bbcZVertex;
          cout << endl; 
        } // check on PC3

	Int_t nTECTrack = 0;
        if(eventNumber<=debugOut && dTecTrackNode && dTecTrack->RowCount()>0) {
	  nTECTrack = dTecTrack->RowCount();
          Int_t nNorth = 0;
          Int_t nSouth = 0;
	  Float_t xInTEC = dTecTrack->get_xyzin(0,0);
	  Float_t yInTEC = dTecTrack->get_xyzin(1,0);

	  Float_t phiTemp1 =  DEGRAD*atan2(yInTEC, xInTEC);
	  if(phiTemp1 < -90.0)
	    phiTemp1 = 360.0 + phiTemp1;

          for(Int_t iRow=0; iRow<dTecTrack->RowCount(); iRow++) {
            Float_t zInTEC = dTecTrack->get_xyzin(2,iRow);
            if(zInTEC>0)
              nNorth++;
            else
              nSouth++;
          } // loop over table rows
	  event_summary[18] = nNorth;
	  event_summary[19] = nSouth;
          cout << "  TEC report event " << eventNumber;
          cout << ", N = " << nNorth << ", S = " << nSouth; 
          cout << ", ph0 = " << phiTemp1;
          cout << ", BBC Z0 = " << bbcZVertex;
          cout << endl; 
        } // check on TEC

        if(crkFix==0 && eventNumber<=debugOut && dCrkHitNode && dCrkHit->RowCount()>0) {
	  cout << "  CRK report Hits = " << dCrkHit->RowCount();
          cout << ", pmt1 = " << dCrkHit->get_pmt(0);
          cout << ", npe1 = " << dCrkHit->get_npe(0);
          cout << ", time1 = " << dCrkHit->get_time(0);
          if(dCrkHit->RowCount() != dCrkRaw->RowCount())
            cout << ", Raw = " << dCrkRaw->RowCount();
          if(dCrkRaw->RowCount()>0) {
            if(dCrkHit->get_pmt(0) != dCrkRaw->get_pmt(0))
	      cout << ", pmt1 = " << dCrkRaw->get_pmt(0);
	    cout << ", adc1 = " << dCrkRaw->get_adc(0);
            cout << ", tdc1 = " << dCrkRaw->get_tdc(0);
          } // check on CRK Raw
          cout << endl; 
        } // check on CRK Hits

        if(eventNumber<=debugOut && dTofReconstructedNode && dTofReconstructed->RowCount()>0) {
          Int_t nNorth = 0;
          Int_t nSouth = 0;
	  Float_t xInTOF0 = dTofReconstructed->get_xtof(0,0);
	  Float_t yInTOF0 = dTofReconstructed->get_xtof(1,0);
	  Float_t zInTOF0 = dTofReconstructed->get_xtof(2,0);

	  Float_t theta = DEGRAD*acos(zInTOF0/sqrt(xInTOF0*xInTOF0 + yInTOF0*yInTOF0 + zInTOF0*zInTOF0));
	  Float_t phiTemp1 =  DEGRAD*atan2(yInTOF0, xInTOF0);
	  if(phiTemp1 < -90.0)
	    phiTemp1 = 360.0 + phiTemp1;

          for(Int_t iRow=0; iRow<dTofReconstructed->RowCount(); iRow++) {
            Float_t zInTOF = dTofReconstructed->get_xtof(2,iRow);
            if(zInTOF>0)
              nNorth++;
            else
              nSouth++;
          } // loop over table rows
	  event_summary[20] = nNorth;
	  event_summary[21] = nSouth;
          cout << "  TOF report event " << eventNumber;
          cout << ", N = " << nNorth << ", S = " << nSouth; 
          cout << ", ph0 = " << phiTemp1;
	  cout << ", th0 = " << theta;
          cout << ", BBC Z0 = " << bbcZVertex;
          cout << endl; 
        } // check on TOF

        if(emcFix==0 && eventNumber<=debugOut && dEmcClusterLocalExtNode && dEmcClusterLocalExt->RowCount()>0) {
          Int_t nNorth = 0;
          Int_t nSouth = 0;
          Int_t nEast = 0;
          Int_t nWest = 0;
          for(Int_t iRow=0; iRow<dEmcClusterLocalExt->RowCount(); iRow++) {
            if(dEmcClusterLocalExt->get_xyz(0,iRow)>0)
              nWest++;
            else
              nEast++;

            if(dEmcClusterLocalExt->get_xyz(2,iRow)>0)
              nNorth++;
            else
              nSouth++;
          } // loop over table rows
	  event_summary[22] = nEast;
	  event_summary[23] = nWest;
	  event_summary[24] = nNorth;
	  event_summary[25] = nSouth;
          cout << "  EMC report event " << eventNumber;
          cout << ", N = " << nNorth << ", S = " << nSouth; 
          cout << ", W = " << nWest << ", E = " << nEast;
          Float_t x0 = dEmcClusterLocalExt->get_xyz(0,0);
          Float_t y0 = dEmcClusterLocalExt->get_xyz(1,0);
          Float_t z0 = dEmcClusterLocalExt->get_xyz(2,0);
          Float_t theta = DEGRAD*acos(z0/sqrt(x0*x0 + y0*y0 + z0*z0));
          Float_t phi = DEGRAD*atan2(y0, x0);
          if(phi < -90.0)
            phi += 360.0;
          cout << ", th0 = " << theta << ", ph0 = " << phi;
          cout << ", BBC Z0 = " << bbcZVertex;
          cout << endl; 
        } // check on EMC

        if(dMuoTracksNode && eventNumber<=debugOut && dMuoTracks->RowCount()>0) {
	  cout << " MUT report event = " << eventNumber << ", tracks = " << dMuoTracks->RowCount();
	  Float_t px0 = dMuoTracks->get_px(0,0);
	  Float_t py0 = dMuoTracks->get_py(0,0);
	  Float_t pz0 = dMuoTracks->get_pz(0,0);
	  Float_t pTot0 = sqrt(px0*px0 + py0*py0 + pz0*pz0);
	  cout << " first momentum = " << pTot0;
	  cout << ",  charge = " << dMuoTracks->get_charge(0);
          cout << ", BBC Z0 = " << bbcZVertex;
          cout << endl; 
	} // check on Muon Tracks

        if(dMuiRoadsNode && eventNumber<=debugOut && dMuiRoads->RowCount()>0) {
	  cout << " MUI report event = " << eventNumber << ",  roads = " << dMuiRoads->RowCount();
	  cout << ", hits0 = " <<  dMuiRoads->get_nhits(0);
          cout << ", quality0 = " << dMuiRoads->get_RoadQuality(0);
	  cout << ", last plane0 = " << dMuiRoads->get_last_plane(0);
	  cout << ", BBC Z0 = " << bbcZVertex;
          cout << endl;     
	} // check on MuID Roads

        Int_t nDchTrack = 0;
        Int_t nTecTrack = 0;
        Int_t nPc1Cluster = 0;
        Int_t nPc2Cluster = 0;
        Int_t nPc3Cluster = 0;
        Int_t nTofRec = 0;
        Int_t nEmcCluster = 0;
	if(dCglTrackNode) {
	  for(Int_t iCgl=0; iCgl<dCglTrack->RowCount(); iCgl++) {
	    if(dCglTrack->get_dctracksid(iCgl)>-1)
	      nDchTrack++;
	    if(dCglTrack->get_pc1clusid(iCgl)>-1)
	      nPc1Cluster++;
	    if(dCglTrack->get_pc2clusid(iCgl)>-1)
	      nPc2Cluster++;
	    if(dCglTrack->get_pc3clusid(iCgl)>-1)
	      nPc3Cluster++;
	    if(dCglTrack->get_tectrackid(iCgl)>-1)
	      nTecTrack++;
	    if(dCglTrack->get_tofrecid(iCgl)>-1)
	      nTofRec++;
	    if(dCglTrack->get_emcclusid(iCgl)>-1)
	      nEmcCluster++;
	  }  // loop over dCglTrack

	}  // check on CGL node

        if(nDchTrack>0 && eventNumber<=debugOut) {
	  event_summary[26] = nPc1Cluster;
	  event_summary[27] = nPc2Cluster;
	  event_summary[28] = nPc3Cluster;
	  event_summary[29] = nTecTrack;
	  event_summary[30] = nTofRec;
	  event_summary[31] = nEmcCluster;
          cout << "  Event " << eventNumber << " has associated: Dch = " << nDchTrack;
          cout << ", PC1 = " << nPc1Cluster;
          cout << ", PC2 = " << nPc2Cluster;
          cout << ", TEC = " << nTecTrack;
          cout << ", PC3 = " << nPc3Cluster;
          cout << ", TOF = " << nTofRec;
          cout << ", EMCal = " << nEmcCluster;
          cout << ", BBC Z0 = " << bbcZVertex;
          cout << endl;
        }

	Float_t z0Beam = bbcZVertex;
	if(bbcZVertex>50.0 || bbcZVertex<-50.0)
	  z0Beam = 0.0;

	if(z0Input>-300.0)
	  z0Beam = z0Input;

        pc_cluster[PC_CLUSTER - 1] = eventNumber;
        pc_cluster[PC_CLUSTER - 2] = iFile;

	pc_z0[PC_Z0 - 1] = eventNumber;
        pc_z0[PC_Z0 - 2] = iFile;

        pc_raw[PC_RAW - 1] = eventNumber;
        pc_raw[PC_RAW - 2] = iFile;

        muon_track[MUON_TRACK - 1] = eventNumber;
        muon_track[MUON_TRACK - 2] = iFile;

        Int_t nPc1 = dPc1Cluster->RowCount();
        Int_t nPc2 = dPc2Cluster->RowCount();
        Int_t nPc3 = dPc3Cluster->RowCount();

        pc_cluster[PC_CLUSTER - 3] = nPc3;
	pc_cluster[PC_CLUSTER - 4] = nPc1;

        event_summary[EVENT_SUMMARY - 3] = nPc3;
	event_summary[EVENT_SUMMARY - 4] = nPc1;

        pc_z0[PC_Z0 - 3] = nPc3;
	pc_z0[PC_Z0 - 4] = nPc1;

        pc_raw[PC_RAW - 3] = nPc3;
	pc_raw[PC_RAW - 4] = nPc1;

        muon_track[MUON_TRACK - 3] = nPc3;
	muon_track[MUON_TRACK - 4] = nPc1;

	pc_cluster[PC_CLUSTER - 5] = nDchTrack;
	event_summary[EVENT_SUMMARY - 5] = nDchTrack;
	pc_z0[PC_Z0 - 5] = nDchTrack;
	pc_raw[PC_RAW - 5] = nDchTrack;
	muon_track[MUON_TRACK - 5] = nDchTrack;

        pc_cluster[PC_CLUSTER - 6] = bbcZVertex;
        pc_cluster[PC_CLUSTER - 7] = dBbcOut->get_NhitPmtSouth(0);
        pc_cluster[PC_CLUSTER - 8] = dBbcOut->get_NhitPmtNorth(0);
        pc_cluster[PC_CLUSTER - 9] = nTECTrack;
        pc_cluster[PC_CLUSTER - 10] = z0Beam;

	event_summary[EVENT_SUMMARY - 6] = bbcZVertex;
        event_summary[EVENT_SUMMARY - 7] = dBbcOut->get_NhitPmtSouth(0);
        event_summary[EVENT_SUMMARY - 8] = dBbcOut->get_NhitPmtNorth(0);
        event_summary[EVENT_SUMMARY - 9] = nTECTrack;
        event_summary[EVENT_SUMMARY - 10] = z0Beam;
	event_summary[EVENT_SUMMARY - 11] = liveTrig;
	event_summary[EVENT_SUMMARY - 12] = scaledTrig;
	event_summary[EVENT_SUMMARY - 13] = rawTrig;

	pc_z0[PC_Z0 - 6] = bbcZVertex;
        pc_z0[PC_Z0 - 7] = dBbcOut->get_NhitPmtSouth(0);
        pc_z0[PC_Z0 - 8] = dBbcOut->get_NhitPmtNorth(0);
        pc_z0[PC_Z0 - 9] = nTECTrack;
        pc_z0[PC_Z0 - 10] = z0Beam;

	for(Int_t iz0PC=0; iz0PC<6; iz0PC++) {
          event_summary[32 + iz0PC] = z0PC[iz0PC];
	  pc_z0[PC_Z0 - 11 - iz0PC] = z0PC[5 - iz0PC];
	} // fill 6 values of z0PC array into PCZ0 NTUPLE

	if(ZdcOutNode && ZdcOut) {
	  event_summary[38] = ZdcOut->get_Zvertex();
	  event_summary[39] = ZdcOut->get_Energy(0);
	  event_summary[40] = ZdcOut->get_Energy(1);
	  event_summary[41] = ZdcOut->get_npmt();
	}
	else {
	  event_summary[38] = -999.0;
	  event_summary[39] = -999.0;
	  event_summary[40] = -999.0;
	  event_summary[41] = -999.0;
	} // check on ZDC storage

	EvtSum->Fill(event_summary);

        pc_raw[PC_RAW - 6] = bbcZVertex;
        pc_raw[PC_RAW - 7] = dBbcOut->get_NhitPmtSouth(0);
        pc_raw[PC_RAW - 8] = dBbcOut->get_NhitPmtNorth(0);
        pc_raw[PC_RAW - 9] = nTECTrack;
        pc_raw[PC_RAW - 10] = z0Beam;
  
        muon_track[MUON_TRACK - 6] = bbcZVertex;
        muon_track[MUON_TRACK - 7] = dBbcOut->get_NhitPmtSouth(0);
        muon_track[MUON_TRACK - 8] = dBbcOut->get_NhitPmtNorth(0);
        muon_track[MUON_TRACK - 9] = nTECTrack;
        muon_track[MUON_TRACK - 10] = z0Beam;
  
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
        // Store PC2 Raw information
        //
        pc_raw[0] = 2;
        for(Int_t jpc2=0; jpc2<nPc2; jpc2++) {
          pc_raw[1]= jpc2;
          pc_raw[2] = dPc2Raw->get_arm(jpc2);
          pc_raw[3] = dPc2Raw->get_sector(jpc2);
          pc_raw[4] = dPc2Raw->get_side(jpc2);
          pc_raw[5] = dPc2Raw->get_padx(jpc2);
          pc_raw[6] = dPc2Raw->get_padz(jpc2);
          pc_raw[7] = dPc2Raw->get_padtype(jpc2);

	  PCRaw->Fill(pc_raw);

	}  // loop over PC2 Raw

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
	// Search for closest clusters in the different PCs
	//

	if(takePC1 && bbcZVertex>bbcZMin && bbcZVertex<bbcZMax) {
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

	    Int_t iSector1 =  dPc1Cluster->get_sector(kPc1);
	    Int_t iSector2 = -999;
	    Int_t iSector3 = -999;

	    Float_t xPc1 = dPc1Cluster->get_xyz(0,kPc1);
	    Float_t yPc1 = dPc1Cluster->get_xyz(1,kPc1);
	    Float_t zPc1 = dPc1Cluster->get_xyz(2,kPc1) - z0Beam;  // put in beam reference frame
	    Float_t dPc1 = sqrt(xPc1*xPc1 + yPc1*yPc1 + zPc1*zPc1);
	    Float_t theta1 = -888.0;
	    Float_t phi1 = -888.0;
	    if(dPc1 > 0.0) {
	      theta1 = DEGRAD*acos(zPc1/dPc1);
	      phi1 = DEGRAD*atan2(yPc1, xPc1);
	      if(phi1 < -90.0)
		phi1 += 360.0;

	      if(phi1>90.0)
		iSector1 += 8;  // East Arm will have sectors 8 to 15 for PC1

	    } // check against divide by 0

	    // Search for closest PC2 cluster
	    Float_t theta2min = -9999.0;
	    Float_t phi2min = -9999.0;
	    Float_t delmin = +9999.0;
	    if(xPc1 > 0.0) {
	      for(Int_t kPc2=0; kPc2<nPc2; kPc2++) {
		Float_t xPc2 = dPc2Cluster->get_xyz(0,kPc2);
		Float_t yPc2 = dPc2Cluster->get_xyz(1,kPc2);
		Float_t zPc2 = dPc2Cluster->get_xyz(2,kPc2) - z0Beam;
		Float_t dPc2 = sqrt(xPc2*xPc2 + yPc2*yPc2 + zPc2*zPc2);
		Float_t theta2 = -888.0;
		Float_t phi2 = -888.0;
		if(dPc2 > 0.0) {
		  theta2 = DEGRAD*acos(zPc2/dPc2);
		  phi2 = DEGRAD*atan2(yPc2, xPc2);
		  if(phi2 < -90.0)
		    phi2 += 360.0;
		} // check against divide by 0

		if(fabs(theta1-theta2) + fabs(phi1-phi2) < delmin) {
		  iSector2 = dPc2Cluster->get_sector(kPc2);
		  theta2min = theta2;
		  phi2min = phi2;
		  delmin = fabs(theta1-theta2) + fabs(phi1-phi2);

		  if(phi2>90.0)
		    iSector2 += 4;  // for PC2 we should not have these sectors !!

		} // check for new minimum
	      } // closest PC2 search
    
	    }  // check for West Arm PC1, skip search in PC2 for East Arm PC1 base

	    // Search for closest PC3 cluster
	    Float_t theta3min = -9999.0;
	    Float_t phi3min = -9999.0;
	    delmin = +9999.0;
	    for(Int_t kPc3=0; kPc3<nPc3; kPc3++) {
	      Float_t xPc3 = dPc3Cluster->get_xyz(0,kPc3);
	      Float_t yPc3 = dPc3Cluster->get_xyz(1,kPc3);
	      Float_t zPc3 = dPc3Cluster->get_xyz(2,kPc3) - z0Beam;
	      Float_t dPc3 = sqrt(xPc3*xPc3 + yPc3*yPc3 + zPc3*zPc3);
	      Float_t theta3 = -888.0;
	      Float_t phi3 = -888.0;
	      if(dPc3 > 0.0) {
		theta3 = DEGRAD*acos(zPc3/dPc3);
		phi3 = DEGRAD*atan2(yPc3, xPc3);
		if(phi3 < -90.0)
		  phi3 += 360.0;
	      } // check against divide by 0

	      if(fabs(theta1-theta3) + fabs(phi1-phi3) < delmin) {
		iSector3 = dPc3Cluster->get_sector(kPc3);
		theta3min = theta3;
		phi3min = phi3;
		delmin = fabs(theta1-theta3) + fabs(phi1-phi3);

		if(phi3>90.0)
		  iSector3 += 4;  // for PC3 East we have 4 through 7

	      } // check for new minimum
	    } // closest PC3 search

	    pc_cluster[6] = theta1;
	    pc_cluster[7] = phi1;
	    pc_cluster[8] = theta2min;
	    pc_cluster[9] = phi2min;
	    pc_cluster[10] = theta3min;
	    pc_cluster[11] = phi3min;

	    pc_cluster[12] = iSector1;
	    pc_cluster[13] = iSector2;
	    pc_cluster[14] = iSector3;

	    PCCluster->Fill(pc_cluster);

	  } // loop over PC1 clusters looking for closest PC2 and PC3

	  if(takePC1>=2) {
	  //
	  // Determining the Z0 from PC1 and PC2, PC1 and PC3
	  //
	    pc_z0[0] = 1;
	    for(Int_t kPc1=0; kPc1<nPc1; kPc1++) {
	      pc_z0[1]= kPc1;
	      pc_z0[2] = dPc1Cluster->get_arm(kPc1);
	      pc_z0[3] = dPc1Cluster->get_xyz(0,kPc1);
	      pc_z0[4] = dPc1Cluster->get_xyz(1,kPc1);
	      pc_z0[5] = dPc1Cluster->get_xyz(2,kPc1);

	      Int_t iSector1 =  dPc1Cluster->get_sector(kPc1);
	    
	      Float_t xPc1 = dPc1Cluster->get_xyz(0,kPc1);
	      Float_t yPc1 = dPc1Cluster->get_xyz(1,kPc1);
	      Float_t rPc1 = sqrt(xPc1*xPc1 + yPc1*yPc1);
	      Float_t zPc1 = dPc1Cluster->get_xyz(2,kPc1);

	      if(flipPC1West && xPc1>0.0)
		zPc1 = -zPc1;

	      if(firstFlipPC1West && flipPC1West) {
		firstFlipPC1West = 0;
		cout << "\n Flipping Z of PC1 West" << endl;
	      }

	      Float_t dPc1 = sqrt(xPc1*xPc1 + yPc1*yPc1 + zPc1*zPc1);
	      Float_t theta1 = -888.0;
	      Float_t phi1 = -888.0;
	      if(dPc1 > 0.0) {
		theta1 = DEGRAD*acos(zPc1/dPc1);
		phi1 = DEGRAD*atan2(yPc1, xPc1);
		if(phi1 < -90.0)
		  phi1 += 360.0;

		if(phi1>90.0)
		  iSector1 += 8;  // East Arm will have sectors 8 to 15 for PC1

		pc_z0[6] = theta1;
		pc_z0[7] = phi1;

		//
		//  Outer PCs  PC2 in West, PC3 in East and West
		//
		for(Int_t iPcOut=1; iPcOut<3; iPcOut++) {
		  if(iPcOut==1 && xPc1>0.0) {
		    for(Int_t kPc2=0; kPc2<nPc2; kPc2++) {
		      Float_t xPc2 = dPc2Cluster->get_xyz(0,kPc2);
		      Float_t yPc2 = dPc2Cluster->get_xyz(1,kPc2);
		      Float_t rPc2 = sqrt(xPc2*xPc2 + yPc2*yPc2);
		      Float_t zPc2 = dPc2Cluster->get_xyz(2,kPc2);

		      if(flipPC2)
			zPc2 = -zPc2;  // flip North and South PC2 West

		      if(firstFlipPC2 && flipPC2) {
			firstFlipPC2 = 0;
			cout << "\n PC2 Z is being flipped" << endl;
		      }

		      Float_t dPc2 = sqrt(xPc2*xPc2 + yPc2*yPc2 + zPc2*zPc2);
		      Float_t theta2 = -888.0;
		      Float_t phi2 = -888.0;
		      if(dPc2 > 0.0) {
			theta2 = DEGRAD*acos(zPc2/dPc2);
			phi2 = DEGRAD*atan2(yPc2, xPc2);
			if(phi2 < -90.0)
			  phi2 += 360.0;

			Int_t iSectorOut = dPc2Cluster->get_sector(kPc2);
			if(phi2>90.0)
			  iSectorOut += 4;  // should not have these sectors for PC2
		      
			Float_t rzSlope = (zPc2 - zPc1)/(rPc2 - rPc1);
			Float_t pcZ0 = zPc2 - rzSlope*rPc2;

			pc_z0[8] = theta2;
			pc_z0[9] = phi2;
			pc_z0[10] = iPcOut + 1;  // will be 2 here for PC2
			pc_z0[11] = pcZ0;

			pc_z0[12] = iSector1;
			pc_z0[13] = iSectorOut;

			pc_z0[14] = kPc1;
			pc_z0[15] = kPc2;

			pc_z0[16] = rPc1;
			pc_z0[17] = zPc1;
			pc_z0[18] = rPc2;
			pc_z0[19] = zPc2;

                        pc_z0[20] = -999.0; // valid only for PC1/PC3 West Arm
                        pc_z0[21] = -999.0; // valid only for PC1/PC3 West Arm
		      
			PCZ0->Fill(pc_z0);  // PC1 PC2 Z0 determination

		      } // check against divide by 0

		    } // loop over PC2 clusters (West Arm only)

		  } // check on West Arm for PC1, and using PC2

		  if(iPcOut==2) {

		    for(Int_t kPc3=0; kPc3<nPc3; kPc3++) {
		      Float_t z0InterpPc2 = -999.0; // valid only for PC1/PC3 West Arm
                      Float_t zDiffPc2 = -999.0; // valid only for PC1/PC3 West Arm

		      Float_t xPc3 = dPc3Cluster->get_xyz(0,kPc3);

		      if(xPc1>0.0 && xPc3<0.0)
			continue;

		      if(xPc1<0.0 && xPc3>0.0)
			continue;

		      Float_t yPc3 = dPc3Cluster->get_xyz(1,kPc3);
		      Float_t rPc3 = sqrt(xPc3*xPc3 + yPc3*yPc3);
		      Float_t zPc3 = dPc3Cluster->get_xyz(2,kPc3);
		      Float_t dPc3 = sqrt(xPc3*xPc3 + yPc3*yPc3 + zPc3*zPc3);

		      if(flipPC3West && xPc3>0.0)
			zPc3 = -zPc3;  // flip North and South PC3 West

		      if(firstFlipPC3West && flipPC3West) {
			firstFlipPC3West = 0;
			cout << "\n Flipping Z0 of PC3 West" << endl;
		      }

		      Float_t theta3 = -888.0;
		      Float_t phi3 = -888.0;
		      if(dPc3 > 0.0) {
			theta3 = DEGRAD*acos(zPc3/dPc3);
			phi3 = DEGRAD*atan2(yPc3, xPc3);
			if(phi3 < -90.0)
			  phi3 += 360.0;

			Int_t iSectorOut = dPc3Cluster->get_sector(kPc3);
			if(phi3>90.0)
			  iSectorOut += 4;  // for PC3 East we have 4 through 7

			Float_t rzSlope = (zPc3 - zPc1)/(rPc3 - rPc1);
			Float_t pcZ0 = zPc3 - rzSlope*rPc3;

                        if(xPc1>0.0 && xPc3>0.0 && fabs(phi1-phi3)<0.50) {
			  Float_t zDiffMin = 99999.0; // initialize to large value;
                          for(Int_t kPc2=0; kPc2<dPc2Cluster->RowCount(); kPc2++) {

			    Int_t iSector2 = dPc2Cluster->get_sector(kPc2);  // should only be 0 to 3

			    if(iSector2 != iSectorOut)
			      continue;  // only look at same sectors (saves unneeded Phi angle check)

                            Float_t xPc2 = dPc2Cluster->get_xyz(0,kPc2);
                            Float_t yPc2 = dPc2Cluster->get_xyz(1,kPc2);
                            Float_t rPc2 = sqrt(xPc2*xPc2 + yPc2*yPc2);
                            Float_t zPc2 = dPc2Cluster->get_xyz(2,kPc2);

                            if(flipPC2)
                              zPc2 = -zPc2;  // flip North and South PC2 West

                            Float_t dPc2 = sqrt(xPc2*xPc2 + yPc2*yPc2 + zPc2*zPc2);
                            Float_t theta2 = -888.0;
                            Float_t phi2 = -888.0;
                            if(dPc2 > 0.0) {
                              theta2 = DEGRAD*acos(zPc2/dPc2);
                              phi2 = DEGRAD*atan2(yPc2, xPc2);
                              if(phi2 < -90.0)
                                phi2 += 360.0;

                              if(fabs(phi2 - 0.5*(phi1+phi3))<0.25) { 
                                zDiffPc2 = rzSlope*rPc2 + pcZ0 - zPc2;
				if(fabs(zDiffPc2)<zDiffMin) {
				  zDiffMin = fabs(zDiffPc2);
				  Float_t rzSlope2 = (zPc2 - zPc1)/(rPc2 - rPc1);
				  z0InterpPc2 = zPc2 - rzSlope2*rPc2;
				} // check on best zDiffPc2 value			
			      } // check on phi2 within window
                         
                            } // check on divide by 0

                          } // loop over West Arm PC2 clusters

                        }  //  check if in West Arm PC1 and PC3, and delta Phi < 0.25 degrees
		      
			pc_z0[8] = theta3;
			pc_z0[9] = phi3;
			pc_z0[10] = iPcOut + 1;  // will be 3 here for PC3
			pc_z0[11] = pcZ0;

			pc_z0[12] = iSector1;
			pc_z0[13] = iSectorOut;

			pc_z0[14] = kPc1;
			pc_z0[15] = kPc3;

			pc_z0[16] = rPc1;
			pc_z0[17] = zPc1;
			pc_z0[18] = rPc3;
			pc_z0[19] = zPc3;

                        pc_z0[20] = zDiffPc2; // vaild only for PC1/PC3 West Arm;
			pc_z0[21] = z0InterpPc2; // valid only for PC1/PC3 West Arm

			PCZ0->Fill(pc_z0);  // PC1 PC3 Z0 determination

		      } // check against divide by 0

		    } // loop over PC3 clusters (East and West Arms)

		  }  // using PC3

		} // loop over PC2 and PC3

	      } // check against divide by 0

	    } // looping over PC1 clusters

	  } // check on takePC1 = 2

	}  // check on taking PC1 for closest searches in PC2 and PC3


	//
	//Determining the Z0 from PC2 and PC3
	//
		
	pc_z0[0] = 2;
	if(takePC1==3) {
	  for(Int_t kPc2=0; kPc2<nPc2; kPc2++) {
	    pc_z0[1]= kPc2;
	    pc_z0[2] = dPc2Cluster->get_arm(kPc2);
	    pc_z0[3] = dPc2Cluster->get_xyz(0,kPc2);
	    pc_z0[4] = dPc2Cluster->get_xyz(1,kPc2);
	    pc_z0[5] = dPc2Cluster->get_xyz(2,kPc2);
	    
	    Float_t xPc2 = dPc2Cluster->get_xyz(0,kPc2);
	    Float_t yPc2 = dPc2Cluster->get_xyz(1,kPc2);
	    Float_t rPc2 = sqrt(xPc2*xPc2 + yPc2*yPc2);
	    Float_t zPc2 = dPc2Cluster->get_xyz(2,kPc2);

	    if(flipPC2)
	      zPc2 = -zPc2;

	    Float_t dPc2 = sqrt(xPc2*xPc2 + yPc2*yPc2 + zPc2*zPc2);
	    Float_t theta2 = -888.0;
	    Float_t phi2 = -888.0;
	    if(dPc2 > 0.0) {
	      theta2 = DEGRAD*acos(zPc2/dPc2);
	      phi2 = DEGRAD*atan2(yPc2, xPc2);
	      if(phi2 < -90.0)
		phi2 += 360.0;

	      Int_t iSectorIn = dPc2Cluster->get_sector(kPc2);
	      if(phi2>90.0)
		iSectorIn += 4;  // should not have these sectors for PC2

	      pc_z0[6] = theta2;
	      pc_z0[7] = phi2;
	      
	      //
	      //  Outer PCs  PC3 in West arm
	      //
	      Int_t iPcOut = 3;
	      for(Int_t kPc3=0; kPc3<nPc3; kPc3++) {
		Float_t xPc3 = dPc3Cluster->get_xyz(0,kPc3);
		if(xPc3<0.0)
		  continue;  // skip this East Arm PC3 cluster

		Float_t yPc3 = dPc3Cluster->get_xyz(1,kPc3);
		Float_t rPc3 = sqrt(xPc3*xPc3 + yPc3*yPc3);
		Float_t zPc3 = dPc3Cluster->get_xyz(2,kPc3);

		if(flipPC3West && xPc3>0.0)
		  zPc3 = -zPc3;

		Float_t dPc3 = sqrt(xPc3*xPc3 + yPc3*yPc3 + zPc3*zPc3);
		Float_t theta3 = -888.0;
		Float_t phi3 = -888.0;
		if(dPc3 > 0.0) {
		  theta3 = DEGRAD*acos(zPc3/dPc3);
		  phi3 = DEGRAD*atan2(yPc3, xPc3);
		  if(phi3 < -90.0)
		    phi3 += 360.0;

		  Int_t iSectorOut = dPc3Cluster->get_sector(kPc3);
		  if(phi3>90.0)
		    iSectorIn += 4;  // PC3 East is 4 to 7 sectors
		      
		  Float_t rzSlope = (zPc3 - zPc2)/(rPc3 - rPc2);
		  Float_t pcZ0 = zPc3 - rzSlope*rPc3;

		  pc_z0[8] = theta3;
		  pc_z0[9] = phi3;
		  pc_z0[10] = iPcOut;  // will be 3 here for PC3
		  pc_z0[11] = pcZ0;

		  pc_z0[12] = iSectorIn;
		  pc_z0[13] = iSectorOut;

		  pc_z0[14] = kPc2;
		  pc_z0[15] = kPc3;

		  pc_z0[16] = rPc2;
		  pc_z0[17] = zPc2;
		  pc_z0[18] = rPc3;
		  pc_z0[19] = zPc3;

                  pc_z0[20] = -999.0;  //  valid only for PC1/PC3 West Arm
                  pc_z0[21] = -999.0;  //  valid only for PC1/PC3 West Arm

		  PCZ0->Fill(pc_z0);  // PC2 PC3 Z0 determination

		} // check against divide by 0 (looking at dPc3 > 0)

	      } // loop over PC3 clusters 

	    } // check on dPc2 > 0 (divide by 0)

	  } // loop over West Arm PC2 

	}  // check if we want to have PC2/PC3 Z0 determination

	if(takePC2 && bbcZVertex>bbcZMin && bbcZVertex<bbcZMax) {
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

	    Int_t iSector1 = -999;
	    Int_t iSector2 = dPc2Cluster->get_sector(kPc2);
	    Int_t iSector3 = -999;

	    Float_t xPc2 = dPc2Cluster->get_xyz(0,kPc2);
	    Float_t yPc2 = dPc2Cluster->get_xyz(1,kPc2);
	    Float_t zPc2 = dPc2Cluster->get_xyz(2,kPc2) - z0Beam;
	    Float_t theta2 = DEGRAD*acos(zPc2/sqrt(xPc2*xPc2 + yPc2*yPc2 + zPc2*zPc2));
	    Float_t phi2 = DEGRAD*atan2(yPc2, xPc2);
	    if(phi2 < -90.0)
	      phi2 += 360.0;

	    if(phi2>90.0)
	      iSector2 += 4;  // PC2 does not have East Arm !!

	    // Search for closest PC1 cluster
	    Float_t theta1min = -9999.0;
	    Float_t phi1min = -9999.0;
	    Float_t delmin = +9999.0;
	    for(Int_t kPc1=0; kPc1<nPc1; kPc1++) {
	      Float_t xPc1 = dPc1Cluster->get_xyz(0,kPc1);
	      Float_t yPc1 = dPc1Cluster->get_xyz(1,kPc1);
	      Float_t zPc1 = dPc1Cluster->get_xyz(2,kPc1) - z0Beam;
	      Float_t theta1 = DEGRAD*acos(zPc1/sqrt(xPc1*xPc1 + yPc1*yPc1 + zPc1*zPc1));
	      Float_t phi1 = DEGRAD*atan2(yPc1, xPc1);
	      if(phi1 < -90.0)
		phi1 += 360.0;

	      if(fabs(theta1-theta2) + fabs(phi1-phi2) < delmin) {
		iSector1 = dPc1Cluster->get_sector(kPc1);
		theta1min = theta1;
		phi1min = phi1;
		delmin = fabs(theta1-theta2) + fabs(phi1-phi2);

		if(phi1>90.0)
		  iSector1 += 8;  // for PC1 East Arm has 8 to 15

	      } // check for new minimum
	    } // closest PC1 search

	    // Search for closest PC3 cluster
	    Float_t theta3min = -9999.0;
	    Float_t phi3min = -9999.0;
	    delmin = +9999.0;
	    for(Int_t kPc3=0; kPc3<nPc3; kPc3++) {
	      Float_t xPc3 = dPc3Cluster->get_xyz(0,kPc3);
	      Float_t yPc3 = dPc3Cluster->get_xyz(1,kPc3);
	      Float_t zPc3 = dPc3Cluster->get_xyz(2,kPc3) - z0Beam;
	      Float_t theta3 = DEGRAD*acos(zPc3/sqrt(xPc3*xPc3 + yPc3*yPc3 + zPc3*zPc3));
	      Float_t phi3 = DEGRAD*atan2(yPc3, xPc3);
	      if(phi3 < -90.0)
		phi3 += 360.0;

	      if(fabs(theta2-theta3) + fabs(phi2-phi3) < delmin) {
		iSector3 = dPc3Cluster->get_sector(kPc3);
		theta3min = theta3;
		phi3min = phi3;
		delmin = fabs(theta2-theta3) + fabs(phi2-phi3);

		if(phi3>90.0)
		  iSector3 += 4;  // for PC3 East we have 4 through 7

	      } // check for new minimum
	    } // closest PC3 search

	    pc_cluster[6] = theta1min;
	    pc_cluster[7] = phi1min;
	    pc_cluster[8] = theta2;
	    pc_cluster[9] = phi2;
	    pc_cluster[10] = theta3min;
	    pc_cluster[11] = phi3min;

	    pc_cluster[12] = iSector1;
	    pc_cluster[13] = iSector2;
	    pc_cluster[14] = iSector3;

	    PCCluster->Fill(pc_cluster);

	  } // loop over PC2 clusters looking for closest PC1 and PC3

	} // check on taking PC2 as a base cluster for closest searches

	if(takePC3 && bbcZVertex>bbcZMin && bbcZVertex<bbcZMax) {
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

	    Int_t iSector1 = -999;
	    Int_t iSector2 = -999;
	    Int_t iSector3 =  dPc3Cluster->get_sector(kPc3);
	    
	    Float_t xPc3 = dPc3Cluster->get_xyz(0,kPc3);
	    Float_t yPc3 = dPc3Cluster->get_xyz(1,kPc3);
	    Float_t zPc3 = dPc3Cluster->get_xyz(2,kPc3) - z0Beam;
	    Float_t theta3 = DEGRAD*acos(zPc3/sqrt(xPc3*xPc3 + yPc3*yPc3 + zPc3*zPc3));
	    Float_t phi3 = DEGRAD*atan2(yPc3, xPc3);
	    if(phi3 < -90.0)
	      phi3 += 360.0;

	    if(phi3>90.0)
	      iSector3 += 4;  // East Arm will have sectors 4 to 7 for PC3

	    // Search for closest PC1 cluster
	    Float_t theta1min = -9999.0;
	    Float_t phi1min = -9999.0;
	    Float_t delmin = +9999.0;
	    for(Int_t kPc1=0; kPc1<nPc1; kPc1++) {
	      Float_t xPc1 = dPc1Cluster->get_xyz(0,kPc1);
	      Float_t yPc1 = dPc1Cluster->get_xyz(1,kPc1);
	      Float_t zPc1 = dPc1Cluster->get_xyz(2,kPc1) - z0Beam;
	      Float_t theta1 = DEGRAD*acos(zPc1/sqrt(xPc1*xPc1 + yPc1*yPc1 + zPc1*zPc1));
	      Float_t phi1 = DEGRAD*atan2(yPc1, xPc1);
	      if(phi1 < -90.0)
		phi1 += 360.0;

	      if(fabs(theta1-theta3) + fabs(phi1-phi3) < delmin) {
		iSector1 = dPc1Cluster->get_sector(kPc1);
		theta1min = theta1;
		phi1min = phi1;
		delmin = fabs(theta1-theta3) + fabs(phi1-phi3);

		if(phi1>90.0)
		  iSector1 += 8;  // East Arm will have sectors 8 to 15 for PC1

	      } // check for new minimum
	    } // closest PC1 search
	    
	    Float_t theta2min = -9999.0;
	    Float_t phi2min = -9999.0;
	    if(xPc3 > 0.0) {
	      // Search for closest PC2 cluster
	      delmin = +9999.0;
	      for(Int_t kPc2=0; kPc2<nPc2; kPc2++) {
		Float_t xPc2 = dPc2Cluster->get_xyz(0,kPc2);
		Float_t yPc2 = dPc2Cluster->get_xyz(1,kPc2);
		Float_t zPc2 = dPc2Cluster->get_xyz(2,kPc2) - z0Beam;
		Float_t theta2 = DEGRAD*acos(zPc2/sqrt(xPc2*xPc2 + yPc2*yPc2 + zPc2*zPc2));
		Float_t phi2 = DEGRAD*atan2(yPc2, xPc2);
		if(phi2 < -90.0)
		  phi2 += 360.0;

		if(fabs(theta2-theta3) + fabs(phi2-phi3) < delmin) {
		  iSector2 = dPc2Cluster->get_sector(kPc2);
		  theta2min = theta2;
		  phi2min = phi2;
		  delmin = fabs(theta2-theta3) + fabs(phi2-phi3);

		  if(phi2>90.0)
		    iSector2 += 4;  // for PC2 we should not have these sectors !!

		} // check for new minimum
	      } // closest PC2 search

	    } // check for West Arm PC3, skip search over PC2 if not West Arm

	    pc_cluster[6] = theta1min;
	    pc_cluster[7] = phi1min;
	    pc_cluster[8] = theta2min;
	    pc_cluster[9] = phi2min;
	    pc_cluster[10] = theta3;
	    pc_cluster[11] = phi3;

	    pc_cluster[12] = iSector1;
	    pc_cluster[13] = iSector2;
	    pc_cluster[14] = iSector3;

	    PCCluster->Fill(pc_cluster);

	  } // loop over PC3 clusters looking for closest PC1 and PC2

	}  // check on taking PC3 as a base for closest PC1 and PC2 searches

	//
	// Fill Muon Tracker NTUPLE
	//

        if(dMuoTracksNode && dMuoTracks->RowCount()>0) {
          muon_track[MUON_TRACK - 11] = dMuoTracks->RowCount();
          for(Int_t kMuo=0; kMuo<dMuoTracks->RowCount(); kMuo++) {
            Float_t px0 = dMuoTracks->get_px(0,kMuo);
            Float_t py0 = dMuoTracks->get_py(0,kMuo);
            Float_t pz0 = dMuoTracks->get_pz(0,kMuo);
            Float_t pTot0 = sqrt(px0*px0 + py0*py0 + pz0*pz0);

            muon_track[0] = kMuo;
            muon_track[1] = px0;
            muon_track[2] = py0;
            muon_track[3] = pz0;
            muon_track[4] = dMuoTracks->get_chisquare(kMuo);
            muon_track[5] = dMuoTracks->get_charge(kMuo);
            muon_track[6] = dMuoTracks->get_nhits(kMuo);
            muon_track[7] = pTot0;

            MuonTrack->Fill(muon_track);

          } // loop over Muon Arm tracks

        }  // check on Muon Arm presence

      }  // check on skipEvents 

      if(eventNumber<maxEvents) {
        eventNumber++;
        if (!dstIn->read(dstNode)) {
          cerr << "\n Failure to read DST event " << eventNumber;
          cout << ", from input file " << iFile << endl;
          cout << "\n Finished input file " << iFile;
          cout << "  with events = " << --eventNumber;
          cout << endl;
          break;
        }  // read next event
      }
      else {
        cout << "\n Finished input file " << iFile;
        cout << "  with events = " << eventNumber;
        cout << endl;
        eventLast = 1;
      } // check on last event

    }  // loop over events

  } // loop over DST input files

  DstFile->Write();
  DstFile->Close();

}
