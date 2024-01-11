//************************************************************
// Analysis macro written by Akio Kiyomichi
//************************************************************

void tofMakeTree(Int_t maxEvents=0, 
		 const char *dstIFile="dstData.root", 
		 const char *rootOFile="toftree.root",
		 const char *runtotime="runtotime.txt") 
{
  Int_t minEvents=0;
  // Re Calculation Option  0:off, 1:on
  const Int_t bbcReCycle = 0;
  const Int_t tofReCycle = 0;
  //const Int_t tofGlobalT = 0;
  
  Int_t verbose = 7;
  Int_t eventNumber = 0;
  
  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libdgo.so");
  gSystem->Load("libRawDataCheck.so");
  gSystem->Load("libphgeo.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea.so");
  gSystem->Load("libheader.so");
  
  // Loading subsystem libraries
  gSystem->Load("libbbc.so");
  gSystem->Load("libzdc.so");
  gSystem->Load("libmvd.so");
  gSystem->Load("libpad.so");
  gSystem->Load("libEG.so");
  gSystem->Load("libEmcDynamic.so");
  gSystem->Load("libemcCalib.so");
  gSystem->Load("libemcOM.so");
  gSystem->Load("liblv1.so");
  gSystem->Load("libemc.so");
  gSystem->Load("libtof.so");
  gSystem->Load("libdch.so");
  gSystem->Load("libtec.so");
  gSystem->Load("libcgl.so");
  gSystem->Load("libcrk.so");
  gSystem->Load("libmom.so");
  gSystem->Load("libvtx.so");
  gSystem->Load("liblvl2.so");
  gSystem->Load("libmui.so");
  gSystem->Load("libmut.so");

  // Specify the geometry flags and magnetic field flags for each dataset
  Int_t bFieldFlag = 1;  // 0 is off, 1 is on
  Int_t geomFlag = 1;    // 0 is retracted, 1 is standard
  
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

  // Set up the modules
  PHRunHeaderRoot* PHRunHeaderRoot = new PHRunHeaderRoot();
  TofAddressObject* TofAddress = new TofAddressObject();
  TofGeometryObject* TofGeometry = new TofGeometryObject();
  TofCalibObject* TofCalib = new TofCalibObject();
  TofCalibrator* TofCalibrator = new TofCalibrator();

  // Book the DST checking histograms
  if (verbose>10) printf("Calling TofCalibrator Initialize\n");
  TofCalibrator->rootInit(rootOFile);
  TofCalibrator->setDebugLevel(0);  // 0:none, 1:little, 2:many
  //TofCalibrator->setTriggerCut(0x5002); // Year1 DATA
  TofCalibrator->setTriggerCut(0x100044); // Year2 DATA
  TofCalibrator->cleanPass();
  TofCalibrator->setPass(1);
  TofCalibrator->booking(TofAddress);
  //TofCalibrator->showPass();
  
  // Initialize the tables
  PHIODataNode<PHTable>* dRunHeaderNode = 
    (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dRunHeader");
  PHIODataNode<PHTable>* dEventHeaderNode = 
    (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dEventHeader");
  
  PhRootHistogramFactory::buildFactory();
  
  // Time for the event loop
  eventNumber = 0;
  while ((maxEvents == 0) || (eventNumber < maxEvents)) 
    {
      eventNumber++;
      if (eventNumber > 1) 
	{
	  if(!(dstIn->read(dstNode)))break;
	}
      printf("Read event %d\n",eventNumber);
      
      if(eventNumber < minEvents) continue;

      // Grab the data nodes
      if (!dRunHeaderNode) 
	{
	  cout << "Dst Read: Could not find data node dRunHeader" << endl;
	} 
      else 
	{
	  dRunHeaderWrapper* dRunHeader = 
	    (dRunHeaderWrapper*)dRunHeaderNode->getData();
	}
      if (!dEventHeaderNode) 
	{
	  cout << "Dst Read: Could not find data node dEventHeader" << endl;
	} 
      else 
	{
	  dEventHeaderWrapper* dEventHeader = 
	    (dEventHeaderWrapper*)dEventHeaderNode->getData();
	}
      
      // Set TOF Parameters
      if(eventNumber == 1)
	{
	  int runNumber = dRunHeader->get_run(0);
	  PHRunHeaderRoot->set_run(runNumber);
	  
	  RunToTime* runTime = RunToTime::instance();
	  PHTimeStamp ts;
	  ts = runTime->getBeginTime(runNumber);
	  
	  cout << "Trying to get TimeStamp for run # " << runNumber << endl;
	  PHTimeStamp startTime = ts ;
	  cout << "Found TimeStamp: "; startTime.print(); cout << endl;
	  
	  TimeStamp = startTime+30;
	  cout << "**** TimeStamp : " << TimeStamp <<endl;
	  
	  cout<<" run = "<<runNumber;
	  cout << " with TimeStamp " << TimeStamp << "\n";
	  
	  // Initialize the TOF Objects
	  if (verbose>10) printf("Calling TofAddress\n");
	  TofAddress->setTimeStamp(TimeStamp);
	  TofAddress->fetch();
      
	  if (verbose>10) printf("Calling TofGeometry\n");
	  TofGeometry->setTimeStamp(TimeStamp); 
	  // Julia's geometry set-up (alignment)
	  PHPoint wo(-41,0,0);
	  PHPoint eo(44,0,0);
	  PHVector Xaxis(1,0,0);
	  PHVector Yaxis(0,1,0);
	  PHFrame eastF(eo,Xaxis,Yaxis);
	  PHFrame westF(wo,Xaxis,Yaxis);
	  if (geomFlag == 0) TofGeometry->setEastCarriage(eastF);
	  TofGeometry->fetch();
	  
	  if (verbose>10) printf("Calling TofCalib\n");
	  TofCalib->setTimeStamp(TimeStamp);
	  TofCalib->fetch();
	  cout<<" run = "<<runNumber;
	  cout<<"  globalT = "<<TofCalib->getGlobalT()<<endl;
	}
      
      // Fill the DST checking histograms
      if (verbose>15) printf("Calling TofCalibrator\n");
      TofCalibrator->filling(topNode, TofAddress, TofGeometry, TofCalib);
    }
  // end event loop
  if (verbose>10) printf("Calling TofCalibrator->rootSave()\n");
  TofCalibrator->rootSave();
}
