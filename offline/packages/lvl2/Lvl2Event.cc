#include <iostream>
#include <string>

#include <recoConsts.h>

#include <phool.h>
#include <Lvl2Event.h>
#include <Lvl2Registry.h>
#include <Lvl2PrimitiveReadbackWrapper.h>
#include <Lvl2EventAccessor.h>
//#include <Lvl2AlgorithmWrapper.h>
#include <Lvl2Lvl1TriggerMask.h>
#include <Lvl2Logger.h>
#include <PHCompositeNode.h>
#include <getClass.h>
#include <Event.h>

#include <oEvent.h>
#include <EventTypes.h>
#include <oBuffer.h>
#include <packetConstants.h>
#include <Event.h>
#include <packet.h>
#include <packet_gl1.h>
#include <packet_lvl2decision.h>
#include <packet_lvl2primitive.h>
#include <phool.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHRawDataNode.h>

#include <Lvl2Out.h>
#include <Lvl2Outv1.h>
#include <Lvl2OutArray.h>
#include <Lvl2DecisionOut.h>
#include <TrigRunLvl1.h>
#include <TrigRunLvl2.h>
#include <TriggerUtilities.h>
#include <Run2Lvl2AlgorithmIndexList.h>

#define NSUBS 5
#define NSUBS1 3

// INSERT ALG HEADERS HERE:
// Run 2 triggers
#include <L2EmcHighPtTileTrigger.h>
#include <L2JPsiElectronTrigger.h>
#include <L2SingleElectronTrigger.h>
#include <L2SingleElectronTriggerNoPC3Cut.h>

// Run 3 triggers
#include <L2SnglElectronTrgLowOcupy.h>
#include <L2JPsiElecTrgLowOcupy.h>
#include <L2MutrDimuonNorthTrigger.h>
#include <L2MutrDimuonSouthTrigger.h>
#include <L2MutrSingleMuonNorthTrigger.h>
#include <L2MutrSingleMuonSouthTrigger.h>


// Run 4 triggers
// L2EmcHighPtTileTrigger already defined under Run 2
#include <L2EmcVeryHighPtTileTrigger.h>
#include <L2AuAuDiElectronTrigger.h>
#include <L2MutrDimuonNorthTrigger.h>
#include <L2MutrDimuonSouthTrigger.h>
#include <L2MuidDimuonSouthTrigger.h>
#include <L2MuidDimuonNorthTrigger.h>

// Run 5 pp triggers
#include <L2AuAuSingleElectronTrigger.h>
#include <L2MutrSingleMuonNorthTrigger.h>
#include <L2MutrSingleMuonSouthTrigger.h>
#include <L2EMuTrigger.h>

// Run 6 pp triggers
#include <L2AuAuDiElectronLowMassPairsTrigger.h>
#include <L2ElectronAcceptanceCutTrigger.h>

// test triggers
#include <L2Lvl1PhotonTrigEmulator.h>
#include <L2Lvl1DielecTrigEmulator.h>

// Run 10 peripheral events
#include <L2PeripheralEventTrigger.h>

// DECLARE ALGORITHM WRAPPERS HERE
// Run 2 triggers
L2EmcHighPtTileTriggerW this_L2EmcHighPtTileTrigger;
L2JPsiElectronTriggerW this_L2JPsiElectronTrigger;
L2SingleElectronTriggerW this_L2SingleElectronTrigger;
L2SingleElectronTriggerNoPC3CutW this_L2SingleElectronTriggerNoPC3Cut;

// Run 3 triggers
L2SnglElectronTrgLowOcupyW this_L2SnglElectronTrgLowOcupy;
L2JPsiElecTrgLowOcupyW this_L2JPsiElecTrgLowOcupy;

// Run 4 triggers
// L2EmcHighPtTileTriggerW already defined under Run 2
L2EmcVeryHighPtTileTriggerW this_L2EmcVeryHighPtTileTrigger;
L2AuAuDiElectronTriggerW this_L2AuAuDiElectronTrigger;
L2MutrDimuonNorthTriggerW this_L2MutrDimuonNorthTrigger;
L2MutrDimuonSouthTriggerW this_L2MutrDimuonSouthTrigger;
L2MuidDimuonNorthTriggerW this_L2MuidDimuonNorthTrigger;
L2MuidDimuonSouthTriggerW this_L2MuidDimuonSouthTrigger;

// Run 5 pp triggers
L2AuAuSingleElectronTriggerW this_L2AuAuSingleElectronTrigger;
L2MutrSingleMuonNorthTriggerW this_L2MutrSingleMuonNorthTrigger;
L2MutrSingleMuonSouthTriggerW this_L2MutrSingleSouthNorthTrigger;
L2EMuTriggerW this_L2EMuTrigger;

// Run 6 pp triggers
L2AuAuDiElectronLowMassPairsTriggerW this_L2AuAuDiElectronLowMassPairsTrigger;
L2ElectronAcceptanceCutTriggerW this_L2ElectronAcceptanceCutTrigger;

// test triggers
L2Lvl1PhotonTrigEmulatorW this_L2Lvl1PhotonTrigEmulator;
L2Lvl1DielecTrigEmulatorW this_L2Lvl1DielectronTrigEmulator;

// Run 10
L2PeripheralEventTriggerW this_L2PeripheralEventTrigger;

using namespace std;

typedef PHDataNode<Event>		EventNode_t;
typedef PHIODataNode<PHObject>	PHObjectNode_t;
typedef PHIODataNode<Lvl2DecisionOut>	Lvl2DecisionOutNode_t; 
typedef PHIODataNode<Lvl2Out>		Lvl2OutNode_t; 
typedef PHIODataNode<Lvl2OutArray>	Lvl2OutArrayNode_t; 

PHDWORD barr[BUFFERSIZE];


Lvl2Event::Lvl2Event() : dbAccessor_ptr(0), lvl2Control(0)
{
  // This version of the constructor is for backward compatibility 
  // it does not run Lvl2 triggers
  
  // The cast here is necessary because the compiler evidently considers
  // "False" to be close enough to no-argument that it calls this constructor
  // again without the type cast!

  Lvl2Event((bool) False);
}

Lvl2Event::Lvl2Event(bool run_triggers)  : dbAccessor_ptr(0), lvl2Control(0)
{
   // ADF - I find this preferable for development
  //Lvl2Logger<Lvl2LoggerImp>::addOStream(Lvl2Logger<Lvl2LoggerImp>::L2Logger::TRACE, std::cout);
  //Lvl2Logger<Lvl2LoggerImp>::addOStream(Lvl2Logger<Lvl2LoggerImp>::L2Logger::INFO, std::cout);
  //LogDisable(L2Logger::TRACE);
  //LogDisable(L2Logger::INFO);

  _trigRunLvl1Cal = NULL;
  _trigRunLvl2Cal = NULL;
  
  // Uncomment to make Lvl2 code quiet
  LogDisable(L2Logger::TRACE);
  LogDisable(L2Logger::INFO);  
  //LogDisable(L2Logger::WARNING);

  recoConsts *rc = recoConsts::instance();

  if(run_triggers==False) {
    // real data containing Lvl2 primitive packets written by DAQ, we only 
    // want to copy level 2 trigger packets over so we do not need a 
    // Lvl2Control object here
    
    cout << PHWHERE
	 << "Initializing level 2 only for copying level 2 packets to DST" 
	 << endl;
    return;
  }

  // The rest of the initialization is for running level 2 triggers on
  // PISA files, Run 2, Run 3, Run 4 or Run 5 real data
  // Note that we make a TrigRunLvl2Cal object on the run node in parallel
  // to the TrigRunLvl2 made by TrigReco

  // This is also done when we do not know if we want to run the triggers
  // or if we will find Lvl2 packets in the PRDF file. In the case where
  // we find lvl2 packets in the PRDF, the trigger setup made below and 
  // written to TrigRunLvl2Cal is not used. 

  cout << PHWHERE << "Initializing level 2 to run triggers" << endl;

  // This is the pointer that will be used for the event in memory
  // that level 2 will run on
  
  evt=0;

  // We need to set Lvl2RealData as the data type so that the real data
  // calibrations will be used by Lvl2 for both trigsims and real data
  // 
  Lvl2DataTypes thisDataType = Lvl2RealData;
  // DLW: these "set" methods are no longer available with the latest implementation
  // of the Lvl2EventAccessor class.  They are passed instead to the constructor of the
  // implementation object.
  //   Lvl2EventAccessor::setDataType(thisDataType);
  //   Lvl2EventAccessor::setRunType(Lvl2Run2);

  // This code tells Lvl2DBAccessor to use Objectivity, to get database 
  // constants for the appropriate year, and to NOT initialize and 
  // terminate the database connection ("False" argument).
  // The database connection is initialized and terminated using Pdbcal 
  // in offline.
  
  //bool objy_init = false; // DLW -- unused now??
  
  // initialize database
  dbAccessor_ptr = 0;
  if(rc->get_IntFlag("LVL2_USE_ASCII_DB") ) {
    
    // try initialize from directory
    cout << PHWHERE << " try initializing from ascii database" << endl;
    
    const char *db_directory = rc->get_CharFlag("LVL2_DB_DIR");
    if( !( db_directory && strlen( db_directory ) ) ) {
      cout << PHWHERE << " failed loading db directory name - punt" << endl;
      exit(1);
    }

    cout << PHWHERE << " try loading from db directory " << db_directory << endl;
    dbAccessor_ptr = new Lvl2DatabaseAccessorAscii( db_directory, "" );
     
  } else {
    cout << PHWHERE << " Lvl2 DB use does not work right now" << endl;
    cout << "Change your LVL2_USE_ASCII_DB Flag setting to 1 and read ASCII file" << endl;
    exit(1);
  } 

  // check pointer
  if(!dbAccessor_ptr) {
    cout << PHWHERE  " Failed to get a pointer to the database, exit!" << endl;
    exit(1);
  }

  lvl2Control = new Lvl2Control<>(dbAccessor_ptr);

  // Get a dump of all lvl2 modules
  cout << PHWHERE << "Dump of all Lvl2 modules:" << endl;
  lvl2Control->dumpAllModules(cout);
  cout << endl;

  bool doTrigSim = false;
  if(rc->get_IntFlag("LVL2_REAL_DATA") == 1) {
    cout << PHWHERE << "Initializing Lvl2 to run triggers on a PRDF file" << endl;
    doTrigSim = false;
    //Lvl2EventAccessor::setLvl2TrigSim(False);
  } else {
    cout << PHWHERE << "Initializing Lvl2 to run triggers on a PISA or simulated PRDF file" << endl;
    doTrigSim = true;
    //Lvl2EventAccessor::setLvl2TrigSim(True);
  }

  //  Provide the event control object with a prototype implementation
  //    object that implements the Offline desired methods
  //
  //bool doTrigSim = false;
  Lvl2EventAccessor::setPrototype(new Lvl2EventAccessorImplOffline(thisDataType,Lvl2Run2,doTrigSim));

  if(rc->get_IntFlag("LVL2_YEAR") == 2) {
    cout << PHWHERE << "Initializing Lvl2 to use Run 2 triggers" << endl;

    lvl2Control->addLvl1Trig(0, 0, 0);
    
    // ASSOCIATE TRIGGER WITH LEVEL1 BIT HERE 
    cout << "    Configuring " << this_L2EmcHighPtTileTrigger.getName() << endl; 
    lvl2Control->addLvl1TrigAlgorithm(0, this_L2EmcHighPtTileTrigger.getName(), 1);
    cout << "    Configuring " << this_L2JPsiElectronTrigger.getName()    << endl;
    lvl2Control->addLvl1TrigAlgorithm(0, this_L2JPsiElectronTrigger.getName(), 1);
    cout << "    Configuring " << this_L2SingleElectronTrigger.getName()    << endl;
    lvl2Control->addLvl1TrigAlgorithm(0, this_L2SingleElectronTrigger.getName(), 1);
    cout << "    Configuring " << this_L2SingleElectronTriggerNoPC3Cut.getName()    << endl;
    lvl2Control->addLvl1TrigAlgorithm(0, this_L2SingleElectronTriggerNoPC3Cut.getName(), 1);

    // Run on all data events
    L2AlgToGet = -1;
    
  }
  
  if(rc->get_IntFlag("LVL2_YEAR") == 3) {
    cout << PHWHERE << "Initializing Lvl2 to use Run 3 triggers" << endl;
    
    // Run on all data events
    L2AlgToGet = -1;
    
    
    if(rc->get_IntFlag("LVL2_REAL_DATA") != 1) {
      // PISA file
      lvl2Control->addLvl1Trig(0, 0, 0);
      lvl2Control->addLvl1TrigAlgorithm(0, this_L2JPsiElecTrgLowOcupy.getName(), 1);
      lvl2Control->addLvl1TrigAlgorithm(0, this_L2SnglElectronTrgLowOcupy.getName(), 1);
      
      // I did not add the run 3 Mutr triggers here yet, I am not sure 
      // if they work on PISA files (Tony)
      
    } else {
      // Real data file
      TriggerUtilities tu;
      
      _trigRunLvl1Cal = tu.getNewTrigRunLvl1();  
      _trigRunLvl2Cal = tu.getNewTrigRunLvl2();  
      
      //now fill these objects from the database
      // use this db info to configure lvl2Control
      // and finally extract the alg indexes from lvl2Control
      // to fully populate the TrigRunLvl2Cal object
      
      // get lvl2/lvl1 config setup info
      fillTrigRunCal(_trigRunLvl1Cal, _trigRunLvl2Cal);
      
      // perform a mapping procedure from the real lvl1
      // configuration lvl1 triggers to the Cal configuration
      // lvl1 triggers.  This should really be done when
      // a topNode is accessible, (which would require that this code
      // be moved into a "setup(topNode) function") but for now
      // I'll do a db lookup;
      // TrigRunLvl1 * realLvl1 runNumber:
      
      // configure lvl2Control with this info
      int l1added[32] = {32*0};
      
      cout << "Configuring Lvl2/Lvl1:" << endl;
      
      for (int j1 = 0; j1 < 32; j1++) // loop over lvl1's
	{
	  cout << "  Lvl1 bit: " << j1 << " Lvl1 algorithm: " << _trigRunLvl1Cal->get_lvl1_trig_name_bybit(j1) << endl;
	  for(int j2 = 0; j2 < 64; j2++) // loop over lvl2's
	    {
	      if (_trigRunLvl2Cal->get_lvl2_lvl1_assoc(j2, j1) == 1) {
		if (l1added[j1] == 0) { 
		  lvl2Control->addLvl1Trig(j1, 0, 0); 
		  // void Lvl2Control::addLvl1Trig(UINT inBitNumber, 
		  // UINT partition, UINT inForceN, 
		  // UINT initialMode=Lvl2EnabledReject);
		  l1added[j1] = 1;
		}
		
		cout << "     Adding Lvl2 bit: " << j2 << " algorithm: " << _trigRunLvl2Cal->get_lvl2_trig_name(j2) << endl;
		
		//  Now add the algorithm with the specified pre-scale
		//  don't stop for thrown exceptions
		try {
		  lvl2Control->addLvl1TrigAlgorithm(j1,_trigRunLvl2Cal->get_lvl2_trig_name(j2), _trigRunLvl2Cal->get_lvl2_lvl1_prescale(j2,j1));
		  
		} 
		catch (Lvl2Exception& e) {
		  cout << PHWHERE << "\n\tcaught Lvl2Exception:" 
		       << e.getDescription() << "\n\t...continuing" 
		       << endl;
		}
		
		catch (...) {
		  cout << PHWHERE << "\n\tcaught unknown exception:" 
		       << "\n\t...continuing" 
		       << endl;
		  
		}
		
	      } // if lvl2_lvl1_assoc
	    }// loop over lvl2's (j2)
	} // loop over lvl1;s (j1)
      
      
      // finally,  pick up alg indexes from 
      // lvl2Control and put them in _trigRunlvl2Cal.
      // With no second (runNumber) arg, fillLvl2AlgIndexes
      // defaults to using current lvl2Control
      // after this, _trigRunLvl2Cal is complete.
      int wasError = fillLvl2AlgIndexes(_trigRunLvl2Cal);  
      if (wasError < 0) { 
	std::cout << PHWHERE << "There was an error putting algorithm indices in TrigRunLvl2Cal, quit!" << std::endl;
	exit(1);
      }
      
    } // if running on a real data file
  } // LVL2_YEAR flag == 3
  
  
  ////////////////////////////////////////////////////////////////////////
  // RUN4 or later trigger configuration
  ////////////////////////////////////////////////////////////////////////

  if(rc->get_IntFlag("LVL2_YEAR") >= 4) {
    // used for real data, central arm PISA hits file, simulated PRDF
    
    ////////////////////////////////////////////////////////////////////////
    // get the trigger setup from a file, as in 
    // L2TestFramework.C.template_new, for Run 4 and later triggers
    ////////////////////////////////////////////////////////////////////////
    Lvl2String triggerSetupFile_str;
    if(rc->get_IntFlag("LVL2_YEAR") >= 4)
      {
	// returns null pointer if flag does not exist
	if(rc->get_CharFlag("TRIGSETUPFILE"))
	  triggerSetupFile_str = rc->get_CharFlag("TRIGSETUPFILE");
	else
	  triggerSetupFile_str = "Run4AuAuTriggerSetupFile";

	cout << PHWHERE << "Initializing Lvl2 using " << triggerSetupFile_str << endl;
      }

    //  Perform level-2 initialization    
    if(!initializeLevel2(triggerSetupFile_str)) {
      L2Log(L2Logger::FATAL) << "Lvl2Event: Caught unknown exception in Level-2 configuration." << Lvl2Logger::flush(); 
      L2Log(L2Logger::FATAL) << "Lvl2Event: terminating due to unknown exception." << Lvl2Logger::flush();
      exit(1);
    }
    
    
    ////////////////////////////////////////////////////////////////////////
    // Determine if we want to run only on events passing a particular Lvl2 
    // algorithm for Run 2 data - save the necessary information in L2AlgToGet
    ///////////////////////////////////////////////////////////////////////
    
    // This sets the default to running on all data events
    // If the flag is empty then we do all events
    L2AlgToGet = -1;
    
    const char *Run2Lvl2AlgoName = rc->get_CharFlag("Run2Lvl2AlgoName");
    if(!strcmp(Run2Lvl2AlgoName,"minbias"))
      {
	L2AlgToGet = 100;
	cout  << "Will run only on Lvl1 forced accepts" << endl;
      }
    for(int i=0;i<28;i++) 
      if(!strcmp(Run2Lvl2AlgoName,L2AlgorithmIndexList[i])) {
	L2AlgToGet = i;
	cout << "Will run only on Lvl2 algorithm accepted" 
	     << L2AlgorithmIndexList[i] << " ialg = " << L2AlgToGet << endl;
      }
    if(L2AlgToGet == -1)
      cout << "Will run on all data events in the file" << endl;
    
  }
  
  
  ///////////////////////////////////////////////////////////////////////
  //  Tell Level-2 that all triggers/algorithms have been defined
  ///////////////////////////////////////////////////////////////////////

  L2Log(L2Logger::INFO) << PHWHERE << "Calling Lvl2Control->configurationComplete() "<< Lvl2Logger::flush();  
  lvl2Control->configurationComplete();
  
  //  and enable rejection by the trigger. 

  L2Log(L2Logger::INFO) << PHWHERE << "Calling Lvl2Control->enableReject() " << Lvl2Logger::flush();  
  lvl2Control->enableReject(0);
  
  // Tell level-2 that we're going to run the trigger
  L2Log(L2Logger::INFO) << PHWHERE << "Calling Lvl2Control->start() " << Lvl2Logger::flush();  
  lvl2Control->start();

  // Now start a run on partition 0
  L2Log(L2Logger::INFO) << PHWHERE << "startPartition run" << Lvl2Logger::flush();  
  lvl2Control->startPartitionRun(0, 1000);
}

//_________________________________________________________________________________
void Lvl2Event::WriteTrigRunLvl2Cal(PHCompositeNode *topNode)
{	  
  // Add the TrigRunLvl2Cal object to the RUN node
  // The original TrigRunLvl2 is undisturbed
  
  // find the runNode
  PHNodeIterator iter(topNode);
  PHCompositeNode *runNode = NULL;
  runNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "RUN"));
  if(!runNode)
    {
      cout << PHWHERE << " Did not find a RUN node,  exit!"
	   << endl;
      exit(1);
    }
  
  PHIODataNode<PHObject> *trigrunlvl2cal =
    new PHIODataNode<PHObject>(_trigRunLvl2Cal,"TrigRunLvl2Cal","PHObject");
  runNode->addNode(trigrunlvl2cal);  
  
  return;
}

PHBoolean Lvl2Event::SaveToDST(PHCompositeNode *root)
{
  static const int verbose=0;

  // This version is called to transfer Lvl2 info from the PRDF to the DST. 
  // It only gets a pointer to the event object that contains the 
  // level 2 packets from the PRDF, and passes it to the other version 
  // of SaveToDST, which actually writes the packets to the objects on the 
  // DST node

  // If there are no Lvl2 packets in the PRDF, it does nothing and
  // returns False

  // Note that it always writes to the L2Decision and Lvl2OutArray nodes
  // since these names are reserved for level 2 info from the DAQ.

  Event *evt = findNode::getClass<Event>(root,"PRDF");
  if (!evt)
    {
      cout << PHWHERE << "PRDF node is empty or non existing!" << endl;
      return False;
    }

  // Now we need to check to see if the level 2 decision packet 
  // exists in the PRDF. If not, we return false

  if (!evt->existPacket(14050))
    {
      if (verbose)
	{
	  cout << PHWHERE
	       << "No level 2 decision packet in the PRDF file, do nothing"
	       << endl;
	  evt->identify();
	}
      return False;
  }

  // This defaults to writing to the nodes named L2Decision and Lvl2OutArray 
  SaveToDST(root,evt);

  return True;
}

//_______________________________________________________________________________
PHBoolean Lvl2Event::SaveToDST(PHCompositeNode *root, Event *evt, const char *lvl2decisionnodename, const char *lvl2outarraynodename)
{
  int verbose=0;
    
  // Called by:
  //       SaveToDST(PHCompositeNode *)
  // OR:   WritePisaLvl2ToDST(root);

  // This version is used to add level 2 data to the DST node during either 
  // the PRDF to DST chain (when it is called by the other version of 
  // SaveToDST), or when running triggers in the DAQ (when it is called by 
  // WritePisaLvl2ToDST) ----
  // In either case, the level 2 packets are in the event structure
  // pointed to by the calling argument "evt"

  PHNodeIterator iter(root);

  // Now find the DST node
  PHCompositeNode *dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode) {
    cout << PHWHERE << "failed to find DST node!" << endl;
    return False;
  }

  // The level 2 primitive data subnodes in the DST node are accessed through a Lvl2OutArray object

  Lvl2OutArray *lvl2outarray = 0;
  PHTypedNodeIterator<Lvl2OutArray> lvl2iter(dstNode);
  //Lvl2OutArrayNode_t *Lvl2OutNode = lvl2iter.find("Lvl2OutArray");
  Lvl2OutArrayNode_t *Lvl2OutNode = lvl2iter.find(lvl2outarraynodename);
  if (Lvl2OutNode) lvl2outarray = Lvl2OutNode->getData();  
  if (!lvl2outarray) {
    cout << PHWHERE << "failed to get Lvl2OutArray object named " << lvl2outarraynodename << endl;
    return False;
  }

  //L2Log(L2Logger::INFO) << PHWHERE << "Get the lvl2 decision packet" << Lvl2Logger::flush();

  // first get the decision packet
  Packet_lvl2decision *lvl2decisionpacket = (Packet_lvl2decision *)evt->getPacket(14050);
  if (lvl2decisionpacket) {
    Lvl2DecisionOut *decisionOut = 0;
    
    // Search for L2Decision Node and get decisionOut object
    PHTypedNodeIterator<Lvl2DecisionOut> l2iter(root);
    Lvl2DecisionOutNode_t *Lvl2DecisionOutNode = l2iter.find(lvl2decisionnodename);
    if (!Lvl2DecisionOutNode) {
      cout << PHWHERE << "L2Decision node named " << lvl2decisionnodename << " is missing." << endl;
      return False;
    }
    
    decisionOut = Lvl2DecisionOutNode->getData();
    if (!decisionOut) {
      cout << PHWHERE << "Could not getData from " << lvl2decisionnodename << endl;
      return False;
    }

    //-* copy the Lvl2 decision data into output Lvl2DecisionOut object
    decisionOut->setFullDecision( lvl2decisionpacket->getFinalDecisionMask() );
    decisionOut->setNumLevel1Triggers( lvl2decisionpacket->iValue(0,"NumLevel1Triggers") );
    for (unsigned int ialg=0; ialg<decisionOut->getMaxNumAlgorithms(); ialg++)
    decisionOut->setAlgorithmDecision(ialg, lvl2decisionpacket->iValue(ialg, "AlgorithmDecision"));

    for (unsigned int ilevel1=0; ilevel1<decisionOut->getMaxNumLvl1Triggers(); ilevel1++) {

      if(verbose>1)
	cout << "  Lvl1 trigger decision for ilevel1 = " << ilevel1 << " is " 
	     << lvl2decisionpacket->iValue(ilevel1,"Level1TriggerDecision") << endl;

      decisionOut->setLevel1TriggerDecision(ilevel1, lvl2decisionpacket->iValue(ilevel1,"Level1TriggerDecision"));

      // copy the lvl1algorithmdecision
      for (unsigned int ialg=0; ialg<decisionOut->getMaxNumAlgorithms(); ialg++)
      decisionOut->setLvl1AlgorithmDecision(ilevel1, ialg, lvl2decisionpacket->getLvl1AlgorithmDecision(ilevel1,ialg));
    }

    if(verbose>1) 
      cout << "Wrote level 2 decision to L2Decision node on DST" << endl;

    delete lvl2decisionpacket;

  }

  // now get the list of other lvl2primitive packets
  const int max_packets = 15000;
  Packet *pktList[max_packets];
  int numpackets = evt->getPacketList( pktList, max_packets );

  int nprim = 0;	// number of level2 primitives
  for (int ipkt=0; ipkt<numpackets; ipkt++) {
    int packetId = pktList[ipkt]->getIdentifier(); 

    //L2Log(L2Logger::INFO) << PHWHERE << "Packet = " << packetId
    //	  << Lvl2Logger::flush();

    if( (packetId > 14050) && (packetId < 14200) ) 
      {
	
	if(verbose>1)
	  L2Log(L2Logger::INFO) << PHWHERE << "     Primitive packetId: " << packetId
				<< Lvl2Logger::flush();
	
	try {
	  Packet_lvl2primitive *lvl2packet = (Packet_lvl2primitive *)pktList[ipkt];
	  //-* get the L2 primitive name
	  char lvl2name[100];
	  
	  unsigned int name_len = lvl2packet->getPrimitiveNameLength();
	  char *name_ptr = reinterpret_cast<char *>(lvl2packet->getPrimitiveDataPtr() - name_len);
	  
	  //L2Log(L2Logger::INFO) << PHWHERE << "     Primitive packet name: " << name_ptr
	  //      << Lvl2Logger::flush();
	  
	  // the name can be stored in the packet without the trailing '\0',
	  // so we put it in here to make a proper string.
	  for (unsigned int i=0; i<name_len*sizeof(PHDWORD); i++) lvl2name[i] = *(name_ptr++);
	  if (lvl2name[name_len*sizeof(PHDWORD)-1] != '\0') lvl2name[name_len*sizeof(PHDWORD)] = '\0';
	  
	  //L2Log(L2Logger::INFO) << PHWHERE << "     Lvl2 primitive name: " << lvl2name
	  //      << Lvl2Logger::flush();
	  
	  // in principle, the lvl2 name is "Algorithm" + "vX"
	  // we separate the name from the version before we write
	  // to disk
	  short version = 0;
	  char *p = lvl2name + strlen(lvl2name) - 1;
	  while ( isdigit(*p) ) p--;
	  if ( *p == 'v' || *p == 'V' ) {
	    *p = '\0';
	    version = (short)atoi( p+1 );	// get version
	  }
	  
	  //L2Log(L2Logger::INFO) << PHWHERE << "     Lvl2 primitive name: " << lvl2name
	  //      << Lvl2Logger::flush();
	  
	  //-* add primitive
	  lvl2outarray->AddPrimitive(nprim);
	  
	  lvl2outarray->setname(nprim, lvl2name);
	  lvl2outarray->setversion(nprim, version);
	  
	  //-* copy the primitive data into output Lvl2Out object
	  UINT dataLength = lvl2packet->getPrimitiveDataLength();
	  
	  PHDWORD* dataStart_ptr = lvl2packet->getPrimitiveDataPtr();
	  lvl2outarray->fill(nprim, dataLength, dataStart_ptr);
	  
	  if(verbose>0)
	    cout << "Wrote packet " << packetId << " named " 
		 << lvl2name << " with length " 
		 << dataLength << " to DST node" << endl;
	  
	  nprim++;
	} catch(Lvl2ProcessingRecoverable &e)
	  { cout << e.getDescription() << endl; }
	
      }
    
    delete pktList[ipkt];
  }
  
  lvl2outarray->set_npart(nprim);
  return True;
}

PHBoolean Lvl2Event::SetLvl2EventPointer(PHCompositeNode* topNode,const char *PRDFNodeName)
{
  int verbose=0;

  // Used only in PISA to DST or PISA to PRDF for level 2 trigger simulations
  // Make an event structure in memory containing the packets that we want to 
  // run level 2 triggers on, and give the pointer to level 2.
  // Reads the re-calibrated data from the PRDFNodeName = PRDFRECAL 
  // composite node, and then any data that do not need recalibration (eg. PC3)
  // from the node "PRDF"

  // Free the memory from the last event, if there was one
  if(evt) {
    delete evt;
    evt=0;
  }

  // the variable seq counts the number of events 
  static int seq=0;
  
  // these are the subsystems that we want to add to the event in memory
  // for later level 2 analysis. The first NSUBS1 will be in prdfNode1, the 
  // remainder will be in prdfNode2

  // Yes, I know that these packet numbers are sometimes larger than the actual
  // number of packets. But the BBC starts at 1000, the RICH starts at 6001 
  // and has gaps up to 6038, etc. This gets them all without much bookkeeping.
 
  static int npackets[NSUBS]={1,40,173,33,33};
  static const char *PRDF[]={"bbcPRDF","crkPRDF","emcPRDF","pc3PRDF","pc1PRDF"};

  // Make an event structure in memory that contains the reverse-calibrated
  // data packets
  
  //create a new oBuffer in which to assemble the event
  // How do we get run number? Does run number matter for level 2?
  // Also, how do we know what BUFFERSIZE to use? Ask Martin
  
  int runnumber=1;
  //int buffersize=16*8192;
  //PHDWORD barr[buffersize];

  oEvent *oe = new oEvent(barr,BUFFERSIZE, runnumber, DATAEVENT, seq);

  // Now we want to find the PRDF composite nodes that contain the data that we 
  // want to put into the event in memory

  PHNodeIterator iter(topNode);
  
  // First find the prdf composite node containing the reverse-calibrated data
  // for the level 2 trigger - this will normally be PRDFRECAL

  PHCompositeNode *prdfNode1 = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", PRDFNodeName));
  if(!prdfNode1) {
    cout << PHWHERE << "Composite node: " << PRDFNodeName << " not found - exit!" 
	 << endl;
    exit(1);
  }

  // Now iterate over the PRDFNodeName subnodes to get the data to
  // put in the packets
  
  PHNodeIterator nit1(prdfNode1);
  PHRawDataNode *snode=0;

  const char *psn;
  char pack[11];
  int pcount=0,pcount_keep=0;

  // loop over the subsystems of interest to level 2 in PRDFRECAL

  for(int isub=0;isub<NSUBS1;isub++) {

    pcount_keep=pcount;

    psn = &PRDF[isub][0];

    // loop over all packets for each subsystem
    // Note that it does not matter if we ask for one that does not exist,
    // we just want to cover all actual packet numbers.

    for(int jpack=0;jpack<npackets[isub];jpack++) {
      
      sprintf(pack,"%s%3.3d",psn,jpack);

      if(verbose>1)
	cout << "searching for the packet " << pack << endl;

      snode = static_cast<PHRawDataNode*>(nit1.findFirst("PHDataNode",pack));
      
      if (!snode) {
	continue;
      }
      pcount++;

      if(verbose>1)
	cout << "Found packet " << pack << endl;

      int bytes = oe->addUnstructPacketData(snode->getData(), 
					    snode->getLength(),
					    snode->getID(),
					    snode->getWordLength(), 
					    snode->getHitFormat());
      
      if(bytes <=0 )
	cout << PHWHERE << "Problem: " << bytes << " added to buffer for packet " 
	     << pack << endl;
    }
    if(verbose>1)
      cout <<  "    found " <<  pcount-pcount_keep << " packets for " << psn 
	   << " in node " << PRDFNodeName << endl;
  }

  // The PC1 and PC3 packets will be  read from the "PRDF" composite node, 
  // since those are OK for the level 2 analysis

  PHCompositeNode *prdfNode2 = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PRDF"));
  if(!prdfNode2) {
    cout << PHWHERE << "Composite node: " << "PRDF" << " not found - exit!" 
	 << endl;
    exit(1);
  }

  PHNodeIterator nit2(prdfNode2);

  // loop over the subsystems of interest to level 2 from the PRDF node

  for(int isub=NSUBS1;isub<NSUBS;isub++) {

    pcount_keep=pcount;

    psn = &PRDF[isub][0];

    // loop over all packets for each subsystem
    
    for(int jpack=0;jpack<npackets[isub];jpack++) {
      
      sprintf(pack,"%s%3.3d",psn,jpack);

      snode = static_cast<PHRawDataNode*>(nit2.findFirst("PHDataNode",pack));
      
      if (!snode) {
	continue;
      }
      pcount++;

      int bytes = oe->addUnstructPacketData(snode->getData(), 
					    snode->getLength(),
					    snode->getID(),
					    snode->getWordLength(), 
					    snode->getHitFormat());
      
      if(bytes <=0 )
	cout <<PHWHERE <<"Problem: " << bytes << " added to buffer for packet " 
	     << pack << endl;
    }
    cout <<  "    found " <<  pcount-pcount_keep << " packets for " << psn 
	 << " in node " << "PRDF" << endl;
  }

  // delete oe, leaving a full-blown event data structure behind in barr
  delete  oe; 
  evt = new A_Event(barr);
 
#ifdef DEBUG
  // Used for testing only
  cout << "List of packets in the event that Lvl2 will process:" << endl;
  Packet *plist[1000];
  evt->getPacketList(plist,1000);
  cout << "packet : length " << endl;
  for (int i=0;i<pcount;i++) {
    if(plist[i]->getDataLength()>0) {
      cout << plist[i]->getIdentifier() << " : " << plist[i]->getLength() 
	   << " ";
      if(i%5 == 0)
	cout << endl;
    }
  }
  cout << endl;
#endif  
  
  seq++;
  
  return True;
}

PHBoolean Lvl2Event::RunL2TriggersPRDFToDST(PHCompositeNode *root)
{
  // Used when running level 2 triggers on real data from a PRDF

  // Gets the event pointer for the event data on the PRDF, then calls the 
  // other version, with event pointer supplied. 
  // Note that the event pointer used here is local to this method only
  
  PHNodeIterator iter(root);

  // Find the node with the Event object; it is called "PRDF".
  PHNode *prdfNode = iter.findFirst("PHDataNode", "PRDF");
  if (!prdfNode)
    {
      cout << PHWHERE << " failed to find PRDF node!" << endl;
      return False;
    }

  // now get the event object from the prdf node
  EventNode_t *evtNode = static_cast<EventNode_t*>(prdfNode);
  Event *evt = evtNode->getData();
  if (!evt)
    {
      cout << PHWHERE << "PRDF node is empty!" 
           << endl;

      return False;
    }

  RunL2TriggersPRDFToDST(evt,root);

  return True;
}

//_______________________________________________________________________________
PHBoolean Lvl2Event::RunL2TriggersPRDFToDST(Event *evt, PHCompositeNode *root)
{
  int verbose=0;

  // This is the one that actually runs the triggers in offline 

  // It is called by:
  //     RunL2TriggersPRDFToDST(PHCompositeNode *root)
  // To output results it calls:
  //     WritePisaLvl2ToDST(root);
 
  recoConsts *rc = recoConsts::instance();

  PHNodeIterator iter(root);

  // Now find the DST node or create it if it doesn't exist
  PHCompositeNode *dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode) 
    {
      cout << PHWHERE << "Warning! Creating new DST node!" << endl;
      dstNode = new PHCompositeNode("DST");
      root->addNode(dstNode);
    }
  
  // Tell level 2 that the data type is real data. This is OK for simulated PRDF too.
  //Lvl2EventAccessor::setDataType(Lvl2RealData);

  // Now see if we want to run the level 2 triggers on this event
  //  Use lvl2EventAccessor to pass the event pointer to data accessors
  //  Lvl2EventAccessor::newEvent(evt);
  Lvl2EventAccessorImplOffline* access_ptr = dynamic_cast<Lvl2EventAccessorImplOffline*>(Lvl2EventAccessor::getImplPtr());

  // Tell level 2 that the data type is real data. This is OK for simulated PRDF too.
  access_ptr->setDataType(Lvl2RealData);

  access_ptr->newEvent(evt);

  // If this is not a data event, skip it - works OK for simulated PRDF
    
  if( evt->getEvtType() != 1 ) 
    {
      cout << " Lvl2Event::RunL2TriggersPRDFToDST: Not a data event, "
	   << "skip it: event type is " << evt->getEvtType() << endl;
      if(evt->getEvtType()==7) cout << "    This event was rejected at level 2" << endl;
      return False;
    }
  
  /////////////////////////////////////////////////////////
  // This is a data event that was not rejected at level 2
  // What was the Run 2 Lvl2 decision history for this event?
  //////////////////////////////////////////////////////////
  
  int nalgaccepted[2];
  int algacceptedlist[28];
  int lvl1acceptedlist[28];
  
  GetRun2Lvl2Decision(evt,nalgaccepted,algacceptedlist,lvl1acceptedlist);
  
  // Do we want to run on this event?
  // If L2AlgToGet is -1, it means that no special trigger was requested
  // and all data events will be processed
  
  int runit=1;
  if(L2AlgToGet != -1) 
    {
      runit = 0;
      if(L2AlgToGet == 100) 
	{
    
	  // We want Lvl1 forced accepts
	  if(nalgaccepted[1] > 0) 
	    {
	      runit = 1;
	      L2Log(L2Logger::INFO) << "Lvl1 bit " << nalgaccepted[1] 
				    << " was forced accepted in Run 2: run the Lvl2 triggers!" 
				    << Lvl2Logger::flush();
	      
	      // list the Run 2 Lvl2 triggers that fired, for information
	      if(nalgaccepted[0] > 0)
		for(int iacc=0;iacc<nalgaccepted[0];iacc++) 
		  L2Log(L2Logger::INFO) << "Lvl2 algorithm " 
					<< L2AlgorithmIndexList[algacceptedlist[iacc]]
					<< " with lvl1 bit " 
					<< lvl1acceptedlist[algacceptedlist[iacc]] 
					<< " was accepted in Run 2" 
					<< Lvl2Logger::flush();
	      
	      else L2Log(L2Logger::INFO) << "No Lvl2 algorithms fired in Run 2" << Lvl2Logger::flush();
	    }
	  
	} 
      else 
	{
    
	  // We want a particular Lvl2 trigger that was algorithm accepted in Run 2
	  for(int iacc=0;iacc<nalgaccepted[0];iacc++)
	    if(algacceptedlist[iacc] == L2AlgToGet) 
	      { 
		runit = 1;
		L2Log(L2Logger::INFO) << "Lvl2 algorithm " << L2AlgToGet << " "
				      << L2AlgorithmIndexList[L2AlgToGet] 
				      << " with lvl1 bit " 
				      << lvl1acceptedlist[iacc]
				      << " was accepted in Run 2: run the Lvl2 triggers!" 
				      << Lvl2Logger::flush();
	      }	
	  
	}
    }
  
  if(runit==0) 
    {
      L2Log(L2Logger::INFO) << " Requested trigger didn't fire in Run 2, skip the event" << Lvl2Logger::flush();
      cerr << " Requested trigger didn't fire in Run 2, skip the event" << endl;
      return False;
    }
  
  // OK, we are going to run the triggers, do it!
  // Instantiate the level-2 trigger mask
  Lvl2Control<>::TriggerMask lvl2TriggerMask;
  
  //  We're running on real data, look up the trigger words
  int nevt = evt->getEvtSequence();
  
  //  Level-2 uses a constant for the number of installed GL1 boards
  //    one 32-bit trigger mask for each
  
  PHDWORD dwTriggerMask[numLvl1TriggerBoards];
  for (UINT iword = 0; iword < numLvl1TriggerBoards; iword++) { dwTriggerMask[iword] = 0;}
  
  if(rc->get_IntFlag("LVL2_REAL_DATA") == 1) 
    {
      if(rc->get_IntFlag("LVL2_YEAR") == 2) 
	{
	  // trigger mask should be one
	  dwTriggerMask[0]=1;
	  lvl2TriggerMask = Lvl2Control<>::TriggerMask(dwTriggerMask);      
	}
      
      if(rc->get_IntFlag("LVL2_YEAR") > 2)
	{
	  // run 3, 4 or 5 real data, get gl1 word and use for trigger mask
	  // (FOR NOW ONLY 1 WORD IS IMPLEMENTED)
	  
	  Packet *p=evt->getPacket(14001);
	  if(p != 0) {
	    dwTriggerMask[0] = p->iValue(0, SCALEDTRIG);
	    lvl2TriggerMask = Lvl2Control<>::TriggerMask(dwTriggerMask);
	    
	    if(verbose>1)
	      {
		// dump dwTriggerMask 
		for( UINT i=0; i<numLvl1TriggerBoards; i++ )
		  cout << "Lvl2Event::RunL2TriggersPRDFToDST - dwTriggerMask[" << i 
		       << "]=" << dwTriggerMask[i] << endl;
	      }
	    
	    delete p;
	  } 
	  else 
	    std::cout << "Lvl2Event::RunL2TriggersPRDFToDST : event " << nevt 
		      << " has no GL1 bank (packet 14001 missing)." << std::endl;
	  
	}
      
    } 
  else 
    {

      // simulated PRDF - trigger mask should be one. Also set bit 2 
      // since this is the correct bit for RUN4 setup
      
      dwTriggerMask[0]= 1 | ( 1 << 2 );
      lvl2TriggerMask = Lvl2Control<>::TriggerMask(dwTriggerMask);      
      
      if(verbose>1)
	cout << "Set the trigger mask to " << dwTriggerMask[0] << endl;
    }
  
  std::set<UINT> strmIds; // These are used in the ATP, but i think can be ignored here -- DLW
  lvl2Control->accept(0, nevt, lvl2TriggerMask, decisionHistory, strmIds);
  
  // We write the level 2 trigger results to DST here 
  // Note that if there were Lvl2 primitives in the data PRDF, they will be
  // transferred to the L2Decision and Lvl2OutArray nodes in the DST, while
  // the new reults generated here will be written to L2DecisionCal and Lvl2OutArrayCal
  
  WritePisaLvl2ToDST(root);
  
  return True;
}

//___________________________________________________________________________
// Run level 2 triggers during PISA to DST chain
// Note that the event pointer is set by SetLvl2EventPointer()
PHBoolean Lvl2Event::RunL2TriggersPISAToDST(PHCompositeNode *root) {

  // This one creates a fake event in memory from the PISA input 
  // and runs the triggers on it

  // It calls WritePisaLvl2ToDST(root) to output the results to DST

  SetLvl2EventPointer(root,"PRDFRECAL");
  //SetLvl2EventPointer(root,"PRDF");

  // An event object has been created in memory by a call to the method
  // SetLvl2EventPointer, and the Event pointer evt now points to it

  cout << "RunL2TriggersPISAToDST: has event: " << endl;
  evt->identify();

  // Tell level 2 that the data type is run 2 real data
  // We do this because the PISA hits data have been decalibrated so
  // that they look like real data before being put into the Event object
  //Lvl2EventAccessor::setDataType(Lvl2RealData);
  
  //  Use lvl2EventAccessor to pass the event pointer to data accessors
  //Lvl2EventAccessor::newEvent(evt);
  Lvl2EventAccessorImplOffline* access_ptr = dynamic_cast<Lvl2EventAccessorImplOffline*>(Lvl2EventAccessor::getImplPtr());
  access_ptr->setDataType(Lvl2RealData);
  access_ptr->newEvent(evt);

  // Now execute the main level-2 accept, passsing a trigger mask of 1
  Lvl2Control<>::TriggerMask mask;
  mask.set(0);

  int nevt = evt->getEvtSequence();
  std::set<UINT> strmIds; // These are used in the ATP, but i think can be ignored here -- DLW
  lvl2Control->accept(0, nevt, mask, decisionHistory, strmIds);
  
  // Now output the level 2 results to the DST node

  WritePisaLvl2ToDST(root);
  
  return True;
}

//___________________________________________________________________
// Add the level 2 results from the PISA to DST or PRDF to DST chain to the DST node
PHBoolean Lvl2Event::WritePisaLvl2ToDST(PHCompositeNode *root)
{
  // What we do here is to make an Event structure in memory and 
  // add the level 2 packets to it. Then we transfer them to the L2DecisionCal
  // and Lvl2OutArrayCal objects on the DST in the same way as we would for the 
  // packets read from the PRDF, using the SaveToDST() method

  // first get the decision packet

  UINT packetNumber = decisionHistory.getPacketNumber();
  UINT length = decisionHistory.getOutputLengthDwords() + 6;
  UINT format = decisionHistory.getFormatNumber();
  
  // Make a buffer and fill it with the decision data
  PHDWORD* temp_buffer = new PHDWORD[length];
  decisionHistory.put(temp_buffer, temp_buffer+length);
  
  //  Add the decision packet to the output oevent
  
  int runnumber=1;
  //int buffersize=200*8192;
  //PHDWORD barr[buffersize];
  int seq = 1;

  oEvent *olvl2 = new oEvent(barr,BUFFERSIZE, runnumber, DATAEVENT, seq);

  olvl2->addUnstructPacketData(temp_buffer, length, 
  					  packetNumber, 4, format);
  delete[] temp_buffer;

  // now write the level 2 primitives to the olvl2 event

  Lvl2Registry theRegistry;
  
  for (Lvl2Registry::PrimitiveIter iter2 = theRegistry.getPrimitiveIterator(); iter2; iter2++)
    if (iter2->isWriteable() && iter2->wasModified()) {
      UINT packetNumber = iter2->getPacketNumber();
      UINT length = iter2->getOutputLength();
      UINT format = IDL2PRIMITIVE;
      
      PHDWORD* temp_buffer = new PHDWORD[length];
      
      //  Fill the new buffer with data
      
      iter2->write(temp_buffer);
      
      //  Add the new packet to the output oevent
      
      olvl2->addUnstructPacketData(temp_buffer, length, packetNumber, 4, format);
      delete[] temp_buffer;
    }
  
  // delete olvl2, leaving a full-blown event data structure behind in barr
  delete  olvl2; 
  
  Event *lvl2evt = new A_Event(barr);

  // L2Decision and Lvl2OutArray are reserved for the level 2 data from the DAQ
  // Use different names for the new level 2 data from running the triggers in
  // offline
  
  SaveToDST(root,lvl2evt,"L2DecisionCal","Lvl2OutArrayCal");
  
  delete lvl2evt;
  
  return True;
}

void Lvl2Event::DumpL2Statistics()
{
  lvl2Control->dumpAlgorithms(cout);
  lvl2Control->dumpPrimitives(cout);
  lvl2Control->dumpAccessors(cout);

  return;
}

void Lvl2Event::fillTrigRunCal(TrigRunLvl1 *trigRunLvl1Cal, TrigRunLvl2 *trigRunLvl2Cal)
{

  TriggerUtilities tu;

  recoConsts *rc = recoConsts::instance();
  int l2calRunno = rc->get_IntFlag("L2CALRUNNO");

  cout << "Lvl2Event::fillTrigRuncal: Get L2TestConfig for run " << l2calRunno
       << endl;

  tu.dbFillTrigRunObjects(l2calRunno, trigRunLvl1Cal, trigRunLvl2Cal);
  
  return;
}


int Lvl2Event::fillLvl2AlgIndexes(TrigRunLvl2 * trunl2, int runNumber)
{
  if (runNumber < 0)
    {

      map <string, int> indPlus1Map;
      // plus1 because uninitialized map entry is 0
      
      // Lvl2Registry is a singleton.  Lvl2Event class member lvl2Control 
      // must be associated with this Registry 
      // so by accessing the registry, we are accessing the values used
      // by lvl2Control
      Lvl2Registry theRegistry;
      Lvl2Registry::AlgorithmIter iter = theRegistry.getAlgorithmIterator();
      
      // filling a map with key of algName, value index +1 (hence indPlus1) for 
      // all algorithms
      while ( iter ) 
	{ 
	  indPlus1Map[ iter->getName() ] = iter->getInstanceIndex(0) + 1;
	  iter++;
	}
      
      for (int i = 0; i < 64; i++)
	{
	  string thisName = trunl2->get_lvl2_trig_name(i);
	  if (thisName.size() < 1) continue;
	  
	  int l2ind = indPlus1Map[thisName] - 1;
	  if (l2ind < 0)  
	    {
	      // algorithm is in database config, but not registered
	      // (and therefore not built) into this lvl2Control's
	      // environment  -- therefore there should be 
	      
	      cout << "warning!!! lvl2 bit " << thisName.c_str()
		   << " in database configuration but not built."
		   << "\n\t\tTherefore it cannot be run."
		   << endl;
	    }
	  else 	trunl2->set_lvl2_trig_bit(l2ind,i); 
	}
      return 0;
    } // if runNumber < 0
  
  //  else do database lookup for this information
  // if this fails Lvl2Decision info is useless so we must
  // return an error codde
  return 0;
}

//__________________________________________________________________________________________
// This is more or less a copy of Brian's trigger setup code from L2TestFramework.C.template_new
//  Initialize level-2 control structures
PHBoolean Lvl2Event::initializeLevel2(Lvl2String& triggerFileName)
{
  //_______________________________
  // Set up of algorithms/triggers
  return PHBoolean( setupTriggers(triggerFileName) );
}

//__________________________________________________________________________________________
PHBoolean Lvl2Event::setupTriggers(Lvl2String& triggerFileName)
{
  // For run 4 and Run 5 triggers
  int bitNumber[100]={0*100};
  Lvl2String algorithm[100];
  unsigned int scaledown[100]={0*100};
  
  // This code is taken from TriggerUtilities and modified 

  TriggerUtilities tu;
  
  _trigRunLvl2Cal = tu.getNewTrigRunLvl2();  

  // Needed for TrigRunLvl2Cal - initialize as we declare them

  int lvl1_lvl2_reject_enable[32]={32*0};
  int lvl1_lvl2_force_accept[32]={32*0};
  string lvl2_trigger_name[64];
  for(int i=0;i<64;i++) lvl2_trigger_name[i] = "";

  string lvl2_description = "Lvl2 OnCal";
  int lvl2_version = 0;
  int lvl2_run_enable = 1;
  int lvl2_reject_enable = 1;

  int lvl2_trigger_bit[64];
  int lvl2_lvl1_assoc[64][32];
  int lvl2_lvl1_prescale[64][32];

  for (int i = 0; i < 64; i++) lvl2_trigger_bit[i] = 0;
  for (int i = 0; i < 64; i++)
  for (int j = 0; j < 32; j++) {
	  lvl2_lvl1_assoc[i][j] = 0;
	  lvl2_lvl1_prescale[i][j] = 0;
	}

  int ilmax = 0;

  // NOTE:: *** We don't handle any exceptions here -- they'll be 
  // handled in the calling routine ***
  std::bitset<32*numLvl1TriggerBoards> bitDefined;
  
  // This allows a subset of the triggers to be run by putting
  // the setup in a file with the correct name in the running directory
  
  ifstream triggerDescrFile;

  triggerDescrFile.open(triggerFileName.c_str());
  if (!triggerDescrFile) 
    {
      recoConsts *rc = recoConsts::instance();

      cout << PHWHERE << "Did not find local file named " <<  triggerFileName.c_str() 
	   << ", will use the default from AFS directory: " <<  rc->get_CharFlag("LVL2_DB_DIR")
	   << endl;

      // Clear the failure flag
      triggerDescrFile.clear();

      triggerFileName = "/" + triggerFileName;
      triggerFileName = rc->get_CharFlag("LVL2_DB_DIR") + triggerFileName;
      
      triggerDescrFile.open(triggerFileName.c_str());
      if(!triggerDescrFile) 
	{
	  cout << PHWHERE << "Failed to open " << triggerFileName.c_str() << " - no more options, quit!" 
	       << endl;
	  exit(1);
	}
    }
  
  // OK, if we got to here we have the trigger setup file open
  
  int il=0;
  
  std::cout << "Setting up Lvl2/Lvl1 configuration from file: " <<  triggerFileName.c_str() << std::endl;
  
  while (!triggerDescrFile.eof()) {
    triggerDescrFile >> bitNumber[il] >> algorithm[il] >> scaledown[il];

    // the file read can read blank lines - they will have bitNumber zero, which is not possible for real data
    if(!(bitNumber[il]==0 && algorithm[il]=="") )
      {
	cout << " bit number " << bitNumber[il] << " algorithm " << algorithm[il] << endl;
	il++;
      }
  }
  ilmax=il;
  cout << " ilmax = " << ilmax << endl;
  
  
  
  // So now we have the trigger setup, apply it
  for(int il=0;il<ilmax;il++) 
    {
      cout << " bit number " << bitNumber[il] << " algorithm " << algorithm[il] << endl;
      //if (bitNumber[il] > 0) 
	{
	  std::cout << "Lvl2Event: Lvl1 bit number " 
		    << bitNumber[il] << " Lvl2 algorithm " 
		    << algorithm[il] << " Lvl2 scaledown " 
		    << scaledown[il] << std::endl;
	  
	  if (!bitDefined.test(bitNumber[il])) 
	    {
	      //
	      //  Define the level-1 trigger bit to partition 0 (always here) 
	      //  with zero forced accepts
	      //
	      
	      lvl2Control->addLvl1Trig(bitNumber[il], 0, 0);
	      bitDefined.set(bitNumber[il]);
	    }
	  
	  //  Now add the algorithm with the specified pre-scale
	  lvl2Control->addLvl1TrigAlgorithm(bitNumber[il], algorithm[il], scaledown[il]);
	  std::cout << "Lvl2Event:: Added algorithm " << algorithm[il] 
		    << " to bit " << bitNumber[il] << std::endl;
	}
    }
  
  // Get the Lvl2 setup from Lvl2Control now for TrigRunLvl2cal
  
  // Lvl2Registry is a singleton.  Lvl2Event class member lvl2Control 
  // must be associated with this Registry 
  // so by accessing the registry, we are accessing the values used
  // by lvl2Control
  Lvl2Registry theRegistry;
  Lvl2Registry::AlgorithmIter iter = theRegistry.getAlgorithmIterator();
  
  // filling a map with key of algName, value index +1 (hence indPlus1) for 
  // all algorithms
  while ( iter ) 
    { 
      string thisName = iter->getName();
      int lvl2bit = iter->getInstanceIndex(0);
      
      //cout << "   lvl2bit " << lvl2bit << " name " << thisName.c_str() << endl;
      
      // Find the lvl1 bit and scaledown
      
      int lvl1bit = 0;
      int lvl2scale = 0;
      
      for(int ind = 0;ind < ilmax;ind++) {
        /* cout << "     -- testing for ind = " << ind << " algorithm " << algorithm[ind].c_str() << endl; */
	
        if(strcmp(algorithm[ind].c_str(),thisName.c_str())==0) {
	  // This is the input list entry corresponding to this iter
	  
	  lvl1bit = bitNumber[ind];
	  lvl2scale = scaledown[ind];
	  
	  lvl2_trigger_name[lvl2bit]=thisName.c_str();
	  lvl2_trigger_bit[lvl2bit]=lvl2bit;
	  
	  lvl2_lvl1_prescale[lvl2bit][lvl1bit]=lvl2scale;
	  lvl2_lvl1_assoc[lvl2bit][lvl1bit]=1;
	  lvl1_lvl2_reject_enable[lvl1bit]=1;
	  
	  /*
	    cout << "          lvl2bit " << lvl2bit 
	    << " Found match for lvl1bit " << lvl1bit << " lvl2scale " 
	    << lvl2scale << " name " << thisName.c_str() << endl;
	  */
	}
      }
      iter++;
    }
  
  // Now completely fill in the _TrigRunLvl2Cal Object using the arrays made above
  if (_trigRunLvl2Cal) {
    _trigRunLvl2Cal->set_lvl2_description(const_cast <char *> (lvl2_description.c_str()));
    _trigRunLvl2Cal->set_lvl2_version(lvl2_version);
    _trigRunLvl2Cal->set_lvl2_run_enable(lvl2_run_enable);
    _trigRunLvl2Cal->set_lvl2_reject_enable(lvl2_reject_enable);
    
    for (int lvl1bit = 0; lvl1bit < 32; lvl1bit++)
      _trigRunLvl2Cal->set_lvl1_lvl2_force_accept(lvl1_lvl2_force_accept[lvl1bit], lvl1bit);
    
    for (int lvl2bit = 0; lvl2bit < 64; lvl2bit++) {
      _trigRunLvl2Cal->set_lvl2_trig_name(const_cast <char *> (lvl2_trigger_name[lvl2bit].c_str()), lvl2bit);
      _trigRunLvl2Cal->set_lvl2_trig_bit(lvl2_trigger_bit[lvl2bit], lvl2bit);
      
      for (int lvl1bit = 0; lvl1bit < 32; lvl1bit++) {
	_trigRunLvl2Cal->set_lvl2_lvl1_prescale(lvl2_lvl1_prescale[lvl2bit][lvl1bit], lvl2bit, lvl1bit);
	_trigRunLvl2Cal->set_lvl2_lvl1_assoc(lvl2_lvl1_prescale[lvl2bit][lvl1bit], lvl2bit, lvl1bit);
      }
    }
    _trigRunLvl2Cal->dump_info(_trigRunLvl2Cal);
  }


return true;
}

//__________________________________________________________________________________________
PHBoolean Lvl2Event::GetRun2Lvl2Decision(Event *evt, int nalgaccepted[], int algacceptedlist[], int lvl1acceptedlist[])
{
  nalgaccepted[0] = 0;
  nalgaccepted[1] = 0;

  // Set up some variables for reading the Lvl2 decision from Run 2 data
  // L2AlgorithmIndexList is defined in "Lvl2AlgorithmIndexList.h"

  const int numLvl2Algs = 32; 
  Lvl2Decision TheDecision[32];

  // First unpack the lvl2 decision packet, storing results in 
  // a Lvl2DecisionHistory
  Lvl2DecisionHistory DAQdecisionHistory;

  Packet* myptr = evt->getPacket(DAQdecisionHistory.getPacketNumber());

  if (myptr != 0) 
  {  
    // Fill DAQdecisionHistory

    L2Log(L2Logger::TRACE) << "Got a pointer to the Lvl2 decision packet" << Lvl2Logger::flush();

    Packet_lvl2decision* decisionPacket_ptr = 
      static_cast<Packet_lvl2decision*> (myptr);
    
    UINT itrig, ialg;
    const UINT * lvl1decisionArray =  
          decisionPacket_ptr->getDecisionArray(); 
    
    // the 0 in iValue(0 is insignificant--iVal here doesn't look at this
    
    
    // should we be looping over the 
    // " Number of Level-1 triggers: " << _numLvl1Triggers?
    
    UINT numl2trigs, numl1trigs = Packet_lvl2decision::MaxNumLvl1Triggers;
    numl2trigs = numl1trigs; // for now
    
    for (itrig = 0; itrig < numl1trigs; itrig++)
    {
      if (lvl1decisionArray[itrig] != 0) 
      {        
	DAQdecisionHistory.appendLvl1TriggerDecision(itrig, lvl1decisionArray[itrig]);
        //  Now get the algorithms for this level-1 trigger

        for (ialg = 0; ialg < numl2trigs; ialg++)
        {
          const UINT * thisLvl12AlgDecisionArray = 
          decisionPacket_ptr->getLvl1AlgorithmDecisionArray(itrig);
          if (thisLvl12AlgDecisionArray[ialg] != 0) 
          {
            Lvl2Decision thisAlgDecision(thisLvl12AlgDecisionArray[ialg]);
            DAQdecisionHistory.appendAlgorithmDecision(itrig, ialg, thisAlgDecision);
          }
        }
      }
    }
    
    // we need to delete the packet pointers, right?
    delete myptr;

    // Now extract the desired Lvl2 information from DAQdecisionHistory

    Lvl2Array<Lvl2DecisionHistory::ALGDECISIONVEC, numLvl2Algs> 
    lvl2DecisionVector = DAQdecisionHistory.getLvl2DecisionVector();
  
    std::vector<Lvl2Decision> &lvl1DecisionVector = DAQdecisionHistory.getLvl1DecisionVector();
  
    for(UINT ibit=0; ibit< numLvl1TriggerBits; ibit++) 
    {
      if(!lvl1DecisionVector[ibit].isEmpty())
      {      
        if(lvl1DecisionVector[ibit].wasExecuted()) 
          L2Log(L2Logger::TRACE) << "      Lvl1 bit " << ibit << " was Set" << Lvl2Logger::flush();
        if(lvl1DecisionVector[ibit].wasForced())
        {
          L2Log(L2Logger::INFO) << "      Lvl1 bit " << ibit << " was Forced" << Lvl2Logger::flush();
          nalgaccepted[1] = ibit;
        }
        for(unsigned int ialg=0;ialg < lvl2DecisionVector[ibit].size();ialg++)
        {
          TheDecision[lvl2DecisionVector[ibit][ialg].first] = lvl2DecisionVector[ibit][ialg].second;
	  if(TheDecision[lvl2DecisionVector[ibit][ialg].first].wasAlgorithmAccepted())
          {
            nalgaccepted[0]=nalgaccepted[0]+1;
	    algacceptedlist[nalgaccepted[0]-1] = ialg;
            lvl1acceptedlist[nalgaccepted[0]-1] = ibit;

            // Was this the trigger that caused the event to be accepted?

	    if(TheDecision[lvl2DecisionVector[ibit][ialg].first].wasAccepted())
            {
              //L2Log(L2Logger::INFO) << "       ----  This Lvl2 algorithm caused the accept" 
	      //<< Lvl2Logger::flush();
            }
          }
        }
      }
    }
  }
  else
  {
    L2Log(L2Logger::TRACE) << "No Lvl2 decision packet found" << Lvl2Logger::flush();
    return false;
  }

  return true;
}






