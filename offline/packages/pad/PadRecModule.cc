// Class: PadRecModule (implementation)
//
// Created by: Paul B Nilsson
//
// Description: PC Cluster Reconstruction
//
// Details: This is the Pad Chamber cluster reconstruction
//          module class that contains the interface to
//          the cluster reconstruction algorithm

#include <phool.h>
#include <PadRecModule.hh>
#include <PadRec.hh>
#include <iostream>
#include <list>

static int simprdfToDSTPrintMod = 1; // information message
static int simprdfToDSTMod = 0;  // simPRDF-to-DST flag for Module
static int pisaToDSTMod = 0;  // PISA-to-DST flag for Module
static int pisaToDSTPrintMod = 1; // information message

using namespace std;

PadRecModule::PadRecModule(short pc)
{
  eventNumber = 0;  // Fix for uninitialized variable...TKH
  debug = 0;
  splitMax = 100;
  padTypeLimit = 2;
  simprdfToDSTMod = 0;
  mode = 0;
  pisaToDSTMod = 0;
  if (pc < 0)
    {
      if (pc < -2)
        {
          pc = -(pc + 2);
          simprdfToDSTMod = 1;
          if (simprdfToDSTPrintMod)
            {
              simprdfToDSTPrintMod = 0;
	      // cout "PadRecModule in simPRDF-to-DST mode for PC" << pc + 1 << endl;
            } // print information message
        } // check on simPRDF-to-DST mode
      else
        {
          pc = -pc;
          pisaToDSTMod = 1;
          if (pisaToDSTPrintMod)
            {
              pisaToDSTPrintMod = 0;
	      // cout << "PadRecModule in PISA-to-DST mode for PC" << pc + 1 << endl;
            } // print information message
        } // check on PISA-to-DST mode
    }

  // Create main pad cluster reconstruction object
  // PadRec *padRecObj = new PadRec(pc,topNode);
  padRecObj = 0;
  if (pisaToDSTMod)
    {
      padRecObj = new PadRec( -pc);
    }
  else if (simprdfToDSTMod)
    {
      padRecObj = new PadRec( -pc - 2);
    }
  else
    {
      padRecObj = new PadRec(pc);
    }

  return;
}

PadRecModule::~PadRecModule()
{
  delete padRecObj;
}

//********************************************************
// Name:       event
//
// Definition: 'main' function of the class called from phool
// 
// History:    4/26/00 Original by P. Nilsson
//********************************************************
PHBoolean
PadRecModule::event(short pc,
                    padDetectorGeo* refGeo,
                    PHCompositeNode* topNode)
{
  PHBoolean state = True;

  short parList[14];
  parList[0] = eventNumber;
  parList[1] = debug;
  parList[2] = splitMax;
  parList[3] = mode;
  parList[4] = padTypeLimit;
  parList[5] = oneW;
  parList[6] = oneZ;
  parList[7] = oneL;
  parList[8] = twoW;
  parList[9] = twoZ;
  parList[10] = twoL;
  parList[11] = threeW;
  parList[12] = threeZ;
  parList[13] = threeL;
  padRecObj->setParameters(&parList[0]);

  padRecObj->init_event(pc, topNode); // init counters, get I/O tables
 
  // Fetch the cells for all sectors
  if ( !(padRecObj->getCells()) )
    {
      // Create cluster lists
      list<Cell> cluster;
      for (short sector = 0; sector < numberOfSectors[pc]; sector++)
        {
          // Set up a new sector
          padRecObj->setSector(sector, refGeo);
	  
          while (padRecObj->getNumberOfCells() > 0)
            {
              // Get one cluster at the time from the main cell list
              cluster = padRecObj->getCluster();
	      
              // Create a fake cluster (for testing purposes - run on only one sector)
              // cluster = padRecObj->fakeCluster();

              // Process the cluster. If necessary, split it into subclusters,
              // calculate and store the hit positions.
              padRecObj->processCluster(cluster, refGeo);
	      
              // Uncomment next line if fakeCluster is used
              // break;
            }
          // Delete the main cell list for the current sector
          padRecObj->clear();
        }
    }
  else
    {
      state = False;
    }
  
  // Dump some statistics
  if (state && (debug == 2))
    {
      padRecObj->showStat();
    }
  
  return state;
}


//********************************************************
// Name:       setClusterSize
//
// Definition: Limits of cluster sizes. These variables set
//             the sizes of the cluster boxes.
//
// History:    06/29/00 Original by P. Nilsson
//             05/01/01 Only the one* and two* are used from now on
//********************************************************
void 
PadRecModule::setClusterSizes(short onePW, short onePZ, short onePL, 
			      short twoPW, short twoPZ, short twoPL, 
			      short threePW, short threePZ, short threePL)
{
  oneW = onePW; // Box height
  oneZ = onePZ; // Box width
  oneL = onePL; // Maximum number of allowed cells within a one particle box

  twoW = twoPW;
  twoZ = twoPZ;
  twoL = twoPL;

  threeW = threePW;
  threeZ = threePZ;
  threeL = threePL;
}











