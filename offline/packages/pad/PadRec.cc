// Class: PadRec (implementation)
//
// Created by: Paul B Nilsson, paul.nilsson@kosufy.lu.se
//
// Description: PC Cluster Reconstruction
//
// Details: This is the Pad Chamber cluster class that
//          contains the cluster reconstruction algorithm
//
//          Fired pads are first converted to cells. These
//          are in turn grouped into clusters (groups) of
//          cells. The criteria for this is that when the
//          cells have close neighbors (cell coodinates <= 1)
//          they are stored in a new cluster object.
//           When the cells have been grouped together, the
//          average hit position is calculated and transformed
//          into the global PHENIX coordinate system.

#include <PadRec.hh>
#include <padDetectorGeo.hh>
#include <dPadRawWrapper.h>
#include <dPadRawClusWrapper.h>
#include <dPadClusterWrapper.h>

#include <iostream>

using namespace std;

static int pisaToDSTRec = 0;  // PISA-to-DST flag for Rec
static int pisaToDSTPrintRec = 1; // information message
static int simprdfToDSTRec = 0;  // simPRDF-to-DST flag for Rec
static int simprdfToDSTPrintRec = 1; // information message

//********************************************************
// Name:       PadRec
//
// Definition: Default Constructor for PadRec
//
// History:    4/26/00 Original by P. Nilsson
//********************************************************

PadRec::PadRec(short pc)
{
  if(pc < 0) {
    if (pc < -2) {
      pc = -(pc+2);
      simprdfToDSTRec = 1;
      if(simprdfToDSTPrintRec) {
	simprdfToDSTPrintRec = 0;
	cout << "\n\n   PadRec in simPRDF-to-DST mode for PC" << pc+1 << "\n" << endl;
      } // print information message
    } // check on simPRDF-to-DST mode
    else {
      pc = -pc;
      pisaToDSTRec = 1;
      if(pisaToDSTPrintRec) {
	pisaToDSTPrintRec = 0;
	// cout << "\n\n   PadRec in PISA-to-DST mode for PC" << pc+1 << "\n" << endl;
      } // print information message
    } // check on PISA-to-DST mode
  }

  short stat;

  // Initialize
  if ((stat = PadRec::init(pc)))
    cout << "PadRec: init error " << stat << endl;

} // End method PadRec::PadRec


//********************************************************
// Name:       ~PadRec
//
// Definition: Default Destructor for PadRec
//
// History:    4/12/00 Original by P. Nilsson
//********************************************************

PadRec::~PadRec(void)
{
  if (outfile) outfile.close();

} // End method PadRec::~PadRec


//********************************************************
// Name:       setParameters
//
// Definition: Read in the parameter list from *par.C/PadRecModule::event
// 
// History:    6/29/00 Original by P. Nilsson
//********************************************************

void PadRec::setParameters(short parList[])
{
  if (parList[1] == 1) cout << "PadRec::setParameters called" << endl;

  short tmp;

  tmp = parList[0];
  eventNumber = (tmp >= 0) ? tmp : 0;
  tmp = parList[1];
  debug = ( (tmp >= 0) && (tmp <= 5) ) ? tmp : 1;
  tmp = parList[2];
  splitMax = ( (tmp >= 1) && (tmp <= 10000)) ? (unsigned int)tmp : 100;
  tmp = parList[3];
  mode = ( (tmp >= 0) && (tmp <= 1)) ? tmp : 1;
  tmp = parList[4];
  padTypeLimit = ( (tmp >= 1) && (tmp <= 3) ) ? tmp : 2;
  tmp = parList[5];
  oneW = ( (tmp > 0) && (tmp <= 10) ) ? tmp : 4;
  tmp = parList[6];
  oneZ = ( (tmp > 0) && (tmp <= 10) ) ? tmp : 4;
  tmp = parList[7];
  oneL = ( (tmp > 0) && (tmp <= 100) ) ? tmp : 11;
  tmp = parList[8];
  twoW = ( (tmp > 0) && (tmp <= 20) ) ? tmp : 8;
  tmp = parList[9];
  twoZ = ( (tmp > 0) && (tmp <= 20) ) ? tmp : 8;
  tmp = parList[10];
  twoL = ( (tmp > 0) && (tmp <= 150) ) ? tmp : 16;
  tmp = parList[11];
  threeW = ( (tmp > 0) && (tmp <= 100) ) ? tmp : 100;
  tmp = parList[12];
  threeZ = ( (tmp > 0) && (tmp <= 100) ) ? tmp : 100;
  tmp = parList[13];
  threeL = ( (tmp > 0) && (tmp <= 100) ) ? tmp : 100;

  // Open hit file
  if (debug == 4)
    {
      outfile.open("padrec.dat",ios::app);
      if (!outfile)
  	{
  	  cerr << "ERROR: unable to open output file" << endl;
  	  exit(-1);
  	}
    }
  else if (debug == 5)
    {
      outfile.open("birdseye.dat",ios::app);
      if (!outfile)
	{
	  cerr << "ERROR: unable to open output file" << endl;
	  exit(-1);
	}
    }
}


//********************************************************
// Name:       showStat
//
// Definition: Dump some statistics
// 
// History:    6/29/00 Original by P. Nilsson
//********************************************************

void PadRec::showStat(void) const
{
  cout << "----------------------------------------" << endl;
  cout << "Statistics for PC" << currentPc+1 << "\n" << endl;
  for (int i = 0; i < numberOfSectors[currentPc]; i++)
    {
      cout << "Sector #" << i << endl;
      cout << ": Number of cells   : " << numberOfCells[i] << endl;
      cout << ": Number of clusters: " << numberOfClusters[i] << endl;
    }
}


//********************************************************
// Name:       setSector
//
// Definition: Prepare a new sector
// 
// History:    6/28/00 Original by P. Nilsson
//********************************************************

void PadRec::setSector(short s, padDetectorGeo* refGeo)
{
  currentSector = s;
  // To handle data correctly (as in putting the global hits in the correct place)
  // we need to correct the sector numbering, or actually succumb to it.
  // The sector numbering isn't used for anything else in the reconstruction code
  // than to reconstruct the right sector.

  if (currentSector < numberOfSectors[currentPc]/2)
    {
      PadRec::setArm(0); // East arm
      currentSectorReduced = currentSector;
    }
  else
    {
      PadRec::setArm(1); // West arm
      currentSectorReduced = currentSector - numberOfSectors[currentPc]/2;
    }

  if (currentSectorReduced == 0)
    {
      // Extract the pad chamber panels for one arm at the time
      // num = 8/4/4 (number of volumes) when geometry is read from ascii file
      // num = 8/8/8 (number of volumes) when geometry is read from database
      fromDatabase = refGeo->isFromDatabase();

      if (currentPc == 0)
	refGeo->get_pc1Geo(currentArm,&pcPanel[0]);
      else if (currentPc == 1)
	refGeo->get_pc2Geo(currentArm,&pcPanel[0]);
      else
	refGeo->get_pc3Geo(currentArm,&pcPanel[0]);
    }
}



//*********************************************************
// Name:       getCells
//
// Definition: Get cells (private)
// 
// History:    1 oct. 2003 Original by Henrik Tydesjo
//             (This is a combination of former methods 
//             getPads and getCells, to speed up the code.)
//
// Comment December 31, 2003 (C.F. Maguire)
//         For simulations the fromDatabase is set to False
//         even though the simulation geometry is being read from Objectivity
//         If I set fromDatabase to True, then the cluster reconstruction doesn't work
//         We need to understand why this is happening
//         Previously, the fromDatabase was uninitialized for simulations
//         So if it was zero, then the code would work as if fromDatabase were False
//         This code was being used with fromDatabase uninitialized for simulations.
//
//*********************************************************

short PadRec::getCells(void)
{
  short stat = 0, id, arm, side, sector, padx, padz, padtype, entries;
  short tempz,tempw;
  short zm,wm;
  short chsize=2; if (currentPc==0) {chsize=1;}
  short possible_cells[2][numberOfSectors[currentPc]][cellsAlongWire][cellsAcrossWire[0]][chsize];
  short cellType[2][numberOfSectors[currentPc]][cellsAlongWire][cellsAcrossWire[0]][chsize];
  short cellID[2][numberOfSectors[currentPc]][cellsAlongWire][cellsAcrossWire[0]][chsize];
  memset(&possible_cells, 0, sizeof(possible_cells));
  memset(&cellType, 0, sizeof(cellType));

  // Get number of entries (fired pads)
  if (dPcXRaw)
    {
      entries = dPcXRaw->RowCount();
    }
  else
    {
      entries = 0;
    }

  // Loop over the pads
  if (entries > 0) {
    for (short i = 0; i < entries; i++) {
      // Get list entry
      id = dPcXRaw->get_id(i);
      arm = dPcXRaw->get_arm(i);
      side = dPcXRaw->get_side(i);
      sector = dPcXRaw->get_sector(i);
      padx = dPcXRaw->get_padx(i);
      padz = dPcXRaw->get_padz(i);
      padtype = dPcXRaw->get_padtype(i);

      // Use sector interval [0,15](PC1), etc, instead of 2x[0,7](PC1), ...
      sector += arm*numberOfSectors[currentPc]/2;

      // Get "mid-pixel-cell" (z,w) for this pad
      zm=padz-1;
      wm=wmap[padz][padx];
      short offsetWire=0;
      if (padx>19) {
	offsetWire=1;
	wm-=cellsAcrossWire[0];
      }

      // where are the possibly hit cells?
      for (short w=wm-1;w<=wm+1;w++) {
	if (w>=0 && w<cellsAcrossWire[0]) {
	  for (short z=zm-1;z<=zm+1;z++) {
	    if (z>=0 && z<cellsAlongWire) {
	      // fill sim.id array, for the cell coordinates of middle pixel (3 for each pad)
	      if (z==zm) {cellID[side][sector][z][w][offsetWire]=id;}

	      possible_cells[side][sector][z][w][offsetWire]++;
	      cellType[side][sector][z][w][offsetWire]+=padtype;

	      if (possible_cells[side][sector][z][w][offsetWire]==3 && (3-(cellType[side][sector][z][w][offsetWire]))>=padTypeLimit) {
		// A cell has been found !!!!!!!!!!!!!!!!!!!!

		// Rotate and translate z-axis when necessary
		if (side == 0) { // South side
		  tempz = cellsAlongWire - 1 - z; 
		}
		else { // North side
		  tempz = cellsAlongWire + z;
		}

		tempw = w + cellsAcrossWire[0]*offsetWire;
		// Rotate and translate w-axis when necessary
		if (side == 0)                      // South side
		  tempw = (cellsAcrossWire[currentPc] - 1) - tempw;

		// PC3 West has its motherboard flipped (in real PRDF mode)
		if ( (!pisaToDSTRec && !simprdfToDSTRec && 
		      currentPc == 2 && arm == 1)) {
		  tempw = (cellsAcrossWire[currentPc] - 1) - tempw;
		}
		// PC3 (in sim PRDF mode)
		if (simprdfToDSTRec && currentPc == 2 && arm == 1) {
		  if (tempw < cellsAcrossWire[currentPc]/2)
		    tempw = tempw + cellsAcrossWire[currentPc]/2;
		  else
		    tempw = tempw - cellsAcrossWire[currentPc]/2;
		}
		// PC2 (in sim mode)
		if ( (pisaToDSTRec || simprdfToDSTRec) && currentPc == 1) {
		  tempw = (cellsAcrossWire[currentPc] - 1) - tempw;
		}
		
		// Store the found cell in the cell list
		Cell newCell;
		newCell.z = tempz;
		newCell.w = tempw;
		newCell.celltype=cellType[side][sector][z][w][offsetWire];
		// Get the simulation id from the middle pixel
		// (same for all)
		newCell.id = cellID[side][sector][z][w][offsetWire];
		cells[sector].push_back(newCell);
		numberOfCells[sector]++;
	      }
	    }
	  }
	}
      }
    }
  }
  else
    stat = -1;
  return stat;
} // End method PadRec::getCells


//********************************************************
// Name:       dumpCells
//
// Definition: Dump the list
// 
// History:    4/12/00 Original by P. Nilsson
//             3/11/00 Added file output
//********************************************************

void PadRec::dumpCells(list<Cell>& cluster)
{
  for (CI ci = cluster.begin(); ci != cluster.end(); ++ci)
    {
      if (outfile)
	outfile << "w = " << (*ci).w << " z = " << (*ci).z << " id = " << (*ci).id << endl;
      else
	cout << "w = " << (*ci).w << " z = " << (*ci).z << " id = " << (*ci).id << endl;
    }
} // End method PadRec::dumpCells


//********************************************************
// Name:       fakeCluster
//
// Definition: Create a fake cluster for testing purposes
//             (this method should only be used by the
//             developer)
// 
// History:    4/17/00 Original by P. Nilsson
//********************************************************

list<Cell> PadRec::fakeCluster(void)
{
  // Test cluster: w,z,celltype, ,,, , ...
  // Big block of 3x8 cells
  const short fixed = 21;
  short clusterCoords[fixed*3] = {
    4,22,0, 5,22,0, 6,22,0, 4,23,0, 5,23,0, 6,23,0,
    4,24,0, 5,24,0, 6,24,0, 4,25,0, 5,25,0, 6,25,0,
    4,26,0, 5,26,0, 6,26,0, 4,27,0, 5,27,0, 6,27,0,
    4,28,0, 5,28,0, 6,28,0 };

  list<Cell> cluster;
  Cell newCell;
  short j = 0;
  for (short i = 0; i < fixed; i++)
    {
      newCell.w = clusterCoords[j++];
      newCell.z = clusterCoords[j++];
      newCell.celltype = clusterCoords[j++];
      newCell.id = 0;
      cluster.push_back(newCell);
    }

  return cluster;
} // End method PadRec::fakeCluster


//********************************************************
// Name:       getCluster
//
// Definition: Get a cluster from the cell list by
//             identifying all cells that are neighbors
// 
// History:    4/12/00 Original by P. Nilsson
//********************************************************

list<Cell> PadRec::getCluster(void)
{
  short z0, w0, z, w;

  Cell newCell;
  list<Cell> newCluster;

  // Get the first cell from the list, store it and erase it
  CI ci = cells[currentSector].begin();
  w0 = (*ci).w;
  z0 = (*ci).z;
  newCell.w = w0;
  newCell.z = z0;
  newCell.celltype = (*ci).celltype;
  newCell.id = (*ci).id;
  newCluster.push_back(newCell);
  cells[currentSector].erase(ci);

  // Find the neighbors to all members of the new cluster
  CI nci = newCluster.begin();
  while (nci != newCluster.end())
    {
      w0 = (*nci).w;
      z0 = (*nci).z;
      ci = cells[currentSector].begin();

      // Find the neighbours to this cell and store them in the same list
      while (ci != cells[currentSector].end())
	{
	  w = (*ci).w;
	  z = (*ci).z;

	  if (isNeighbor(abs(w0-w),abs(z0-z)))
	    {
	      // Add cell to the new cluster
	      newCell.w = w;
	      newCell.z = z;
	      newCell.id = (*ci).id;
	      newCell.celltype = (*ci).celltype;
	      newCluster.push_back(newCell);

	      // Remove cell from the list
	      cells[currentSector].erase(ci--);
	    }
	  ci++;
	}
      nci++;
    }

  numberOfClusters[currentSector]++;
  return newCluster;
} // End method PadRec::getCluster


//********************************************************
// Name:       getDimensions
//
// Definition: Return the dimension parameters of a cluster
//
// History:    4/18/00 Original by P. Nilsson
//********************************************************

void PadRec::getDimensions(list<Cell>& cluster,
			   short& maxW, short& minW, short& maxZ, short& minZ) const
{
  maxW = 0;
  minW = 999;
  maxZ = 0;
  minZ = 999;

  for (CI ci = cluster.begin(); ci != cluster.end(); ci++)
    {
      maxW = max((*ci).w,maxW);
      minW = min((*ci).w,minW);
      maxZ = max((*ci).z,maxZ);
      minZ = min((*ci).z,minZ);
    }

} // End method PadRec::getDimensions


//********************************************************
// Name:       getNumberOfParticles
//
// Definition: Estimate the number of particles in a cluster
//             by measuring the size and the number of
//             member cells
//
// History:    04/12/00 Original by P. Nilsson
//             03/15/01 Reduced max nr of particles to 3
//********************************************************

short PadRec::getNumberOfParticles(list<Cell>& cluster)
{
  short num = 1, numCells;
  short maxW, minW, maxZ, minZ;
  short wLength, zLength;

  PadRec::getDimensions(cluster, maxW, minW, maxZ, minZ);

  // Set the dimensions of the cluster
  wLength = maxW - minW + 1;
  zLength = maxZ - minZ + 1;

  numCells = PadRec::getNumberOfCells(cluster);

  // Estimate the number of particles that created the cluster
  if ((wLength <= oneW) && (zLength <= oneZ))
    num = (numCells <= oneL) ? 1 : 2;
  else if ((wLength <= twoW) && (zLength <= twoZ))
    num = (numCells <= twoL) ? 2 : 3;
  else
    num = 3;

  return num;
} // End method PadRec::getNumberOfParticles


//*********************************************************
// Name:       processCluster
//
// Definition: Split a cluster recursively into sub clusters
//             (if necessary)
//
// History:    04/16/00 Original splitCluster by P. Nilsson
//             05/26/00 Recursive version -> processCluster
//             07/09/00 Bug fix: big cluster are better handled
//             11/02/00 Too big clusters are now thrown away
//             12/21/00 Removed a forgotten dumpCells call
//             01/02/01 Cluster overlapping removed
//             01/02/01 Algorithm compacted. Too big clusters = 1 particle
//             01/03/16 Bug fix: 1-particle-cluster level fixed
//*********************************************************

void PadRec::processCluster(list<Cell>& cluster, padDetectorGeo* refGeo)
{
  short nParts = 1;
  PHBoolean split;

  // Out-comment these lines if you want to write complete
  // hit information to file (also set debug to 4)
  // if (debug == 4)
  //  {
  //    if (outfile) outfile << "New cluster:" << endl;
  //    PadRec::dumpCells(cluster);
  //  }

  if (cluster.size() <= splitMax)
    {
      // mode == 1: Split large clusters when necessary
      nParts = (mode == 1) ? getNumberOfParticles(cluster) : 1;
    }
  else
    {
      // Cluster is very large (e.g. the whole ROC is firing)
      // don't bother to split it
      nParts = 1;
    }

  // When nParts is equal to one, we are ready. Calculate the hit position.
  // If it's bigger than one, it will be recursively split into smaller
  // clusters until nParts is equal to one.
  if (nParts == 1)
    {
      // Determine and store the hit position
      PadRec::setPosition(cluster, refGeo);

      // Erase and delete the cluster
      cluster.clear();
    }
  else
    {
      short sideLH, sideLV;
      short maxW, minW, maxZ, minZ;
      short wLength, zLength, maxZsub, maxWsub;
      list<Cell> subCluster;
      Cell newCell;

      // Loop until all sub clusters have been processed
      while (cluster.size() > 0)
	{
	  nParts = PadRec::getNumberOfParticles(cluster);

	  if (nParts == 1)
	    {
	      // Determine and store the hit position
	      PadRec::setPosition(cluster, refGeo);

	      // Erase and delete the cluster
	      cluster.clear();
	    }
	  else
	    {
	      PadRec::getDimensions(cluster, maxW, minW, maxZ, minZ);

	      // Set the dimensions of the cluster
	      wLength = maxW - minW + 1;
	      zLength = maxZ - minZ + 1;
	      sideLH = PadRec::sideLength((float) wLength, (float) nParts);
	      sideLV = PadRec::sideLength((float) zLength, (float) nParts);

	      // Maximum wire number of sub cluster to be extracted
	      maxWsub = minW + sideLH - 1;
	      // Maximum z number of sub cluster to be extracted
	      maxZsub = minZ + sideLV - 1;

	      // Loop over the cells in the cluster
	      for (CI ci = cluster.begin(); ci != cluster.end(); ci++)
		{
		  // Decide if the current cell should be a part of the new sub cluster
		  // Use the dPadCluster table to determine the split order;
		  // e.g. the cluster XXXXX will be split either into XX + XXX or XXX + XX
		  // (avoid systematic error)

		  split = kFALSE;
		  if (wLength >= zLength) // Split horizontally
		    {
		      if ((dPcXCluster->RowCount())%2 == 0)
			{
			  if ((*ci).w <= maxWsub) split = kTRUE;
			}
		      else
			{
			  if ((*ci).w < maxWsub) split = kTRUE;
			}
		    }
		  else // Split vertically
		    {
		      if ((dPcXCluster->RowCount())%2 == 0)
			{
			  if ((*ci).z <= maxZsub) split = kTRUE;
			}
		      else
			{
			  if ((*ci).z < maxZsub) split = kTRUE;
			}
		    }

		  if (split)
		    {
		      // Build a new cell
		      newCell.w = (*ci).w;
		      newCell.z = (*ci).z;
		      newCell.id = (*ci).id;
		      newCell.celltype = (*ci).celltype;

		      // Keep track of the split clusters (change sign of the celltype)
		      if (newCell.celltype > 0) newCell.celltype = -newCell.celltype;

		      // Save the extracted cell in the new sub cluster
		      subCluster.push_back(newCell);

		      // Erase the used cell
		      cluster.erase(ci--);
		    }
		} // end loop over cells

	      // Process the extracted sub cluster
	      PadRec::processCluster(subCluster, refGeo);
	    } // end else
	}
    }
} // End method PadRec::processCluster


//********************************************************
// Name:       getkPadID
//
// Definition: Get the simulation id for a given pixel
//             Principle: find the closest cell to the hit
//             This is only necessary for simulation. As
//             of Jan 2001, it is not possible to determine
//             if the data is simulated or real inside the
//             hit reconstruction algorithm. This consumes
//             valuable time..
//
// History:    5/31/00  Original by P. Nilsson
//             12/21/00 Removed a few junk lines and vars
//********************************************************

short PadRec::getkPadID(list<Cell>& cluster, double wrec, double zrec)
{
  short idcand = 0;
  double dw, dz, d2, d2min;

  d2min = 999.0;
  for (CI ci = cluster.begin(); ci != cluster.end(); ++ci)
    {
      dw = wrec - (double)(*ci).w;
      dz = zrec - (double)(*ci).z;
      d2 = dw*dw + dz*dz;
      if (d2 < d2min)
	{
	  d2min = d2;
	  idcand = (*ci).id;
	}
    }

  return idcand;
} // End method PadRec::getkPadID


//********************************************************************
// Name:       setPosition
//
// Definition: Calculate and store position of the reconstructed hit
//
// History:    04/17/00 Original by P. Nilsson
//             03/11/00 Bug fix: int instead of short for w,z variables
//             01/05/01 Bug fix: local coordinate system fixed
//             01/05/01 Junk comments removed
//             04/20/01 Read coordinates from database + major cleanup
//********************************************************************

void PadRec::setPosition(list<Cell>& cluster, padDetectorGeo* refGeo)
{
  int numCells = 0, w = 0, z = 0;
  short id, cw, cz, module;
  short maxW, minW, maxZ, minZ;
  double aasep, clsep, zgap;
  double wMean, zMean, dxyz[3], temp[3];
  double zMeanTemp, upperScale, lowerScale;
  float wLength, zLength, cellsAlongWireScale;
  PHPoint sectorOrigin, p0, p1, p2, p3, dispP3P1, dispP2P0, globalHit;
  PHVector baseP3P1, baseP2P0, connect;
  size_t index, rindex;

  // For local coord. system transformations only
  // double pxlen, pxsep, wside, wcent, xoff, zoff;

  // Get the mean value of the cell coordinates
  int numCellsNegType = 0;
  for (CI ci = cluster.begin(); ci != cluster.end(); ci++)
    {
      w += (int)(*ci).w;
      z += (int)(*ci).z;
      numCells++;
      if ( (*ci).celltype < 0 )
	{
	  numCellsNegType++; // from a split cluster
	}
    }
  wMean = (double)w/numCells;
  zMean = (double)z/numCells;
  if (numCellsNegType > 0)
    // this condition could/should probably be stricter
    // if ( numCellsNegType == numCells)
    // but I'm not 100% sure of that from looking at the code. /DS
    {
      numCells = - numCells;
    }

  // Which pad/pixel is closest to the reconstructed hit?
  cw = PadRec::near(wMean);
  cz = PadRec::near(zMean);

  // Set currentSide variable
  PadRec::setSide(cz);

  // Get the simulation id for the center pixel
  id = getkPadID(cluster, wMean, zMean);

  // Restore the gap between the PC2/3 modules (0 in PC1)
  refGeo->get_z0Gap(&temp[0]);

  // zgap = (currentSide == 1) ? temp[currentPc] : 0.0;
  zgap = (fromDatabase) ? 0.0 : temp[currentPc];

  // Wire chamber parameters
  refGeo->get_anodeSpacing(&temp[0]); // Anode-anode separation
  aasep = temp[currentPc];
  refGeo->get_cellSpacing(&temp[0]);  // Cell separation
  clsep = temp[currentPc];

  // For local coord. system transformation only
  // refGeo->get_pixelLength(&temp[0]);  // Pixel length (along wire)
  // pxlen = temp[currentPc];
  // refGeo->get_pixelSpacing(&temp[0]); // Pixel separation
  // pxsep = temp[currentPc];
  // refGeo->get_sideWidth(&temp[0]);    // Side pixel length
  // wside = temp[currentPc];
  // refGeo->get_centerWidth(&temp[0]);  // Center pixel length
  // wcent = temp[currentPc];
  // refGeo->get_xOffset(&temp[0]); // Offset in x to the local coordinate system
  // xoff = temp[currentPc];
  // refGeo->get_zOffset(&temp[0]); // Offset in z to the local coordinate system
  // zoff = temp[currentPc];

  // Extract the defining points from the sector panel
  // When the geometry is read from the database, we have access
  // to 4 survey points per chamber (8 per PC2/3 sector = 2 PC2/3 chambers)
  // When the geometry is read from the padGeometry.txt ascii file, we have access
  // to 4 survey points per PC1 chamber and PC2/3 sector (2 chambers)
  // See web documentation for explanation of indices and numbering at
  // http://www.kosufy.lu.se/phenix/geometry.shtml
  if ((fromDatabase) && (currentPc > 0))
    module = currentSectorReduced + currentSide*padDetectorGeo::CGLMAXSECTPERARM;
  else
    module = currentSectorReduced;
  p0 = pcPanel[module].getPoint(0);
  p1 = pcPanel[module].getPoint(1);
  p2 = pcPanel[module].getPoint(2);
  p3 = pcPanel[module].getPoint(3);
  sectorOrigin = pcPanel[module].getCenter();

  // Calculate the errors [local coordinate system]
  PadRec::getDimensions(cluster, maxW, minW, maxZ, minZ);
  zLength = (float)(maxZ - minZ + 1);
  wLength = (float)(maxW - minW + 1);
  dxyz[0] = aasep*wLength/sqrt(12.);
  dxyz[1] = aasep*wLength/sqrt(12.);
  dxyz[2] = clsep*zLength*2/sqrt(12.);

  // Alternative: Convert to local coordinate system: z' -> z, w' -> y (cm)
  // double dx,dy,dz;
  // dx = p1.getX() - p0.getX();
  // dy = p1.getY() - p0.getY();
  // dz = p1.getZ() - p0.getZ();
  // Offsets to the local coordinate system (calculate from survey measurements)
  // xoff = -0.5*sqrt(dx*dx + dy*dy + dz*dz);
  // zoff = p0.getZ();
  // dxyz[0] = 0;
  // dxyz[1] = wMean*aasep + wside + 0.5*wcent + pxsep + xoff;
  // dxyz[2] = zMean*(pxlen + pxsep) + 0.5*pxlen + zoff;

  //////////////////////////////////////
  // Convert to global coordinate system
  //////////////////////////////////////

  // Define base vectors
  baseP3P1 = (PHVector)(p3 - p1);
  baseP2P0 = (PHVector)(p2 - p0);

  // Scale factors
  upperScale = baseP3P1.length() - zgap;
  lowerScale = baseP2P0.length() - zgap;

  baseP3P1.normalize();
  baseP2P0.normalize();

  // Treat one chamber at the time when applicable
  if (currentSide == 0)
    zMeanTemp = zMean;
  else
    {
      zMeanTemp = ((fromDatabase) && (currentPc > 0)) ? 
	zMean - cellsAlongWire : zMean;
    }

  if ((fromDatabase) && (currentPc > 0))
    cellsAlongWireScale = 1.*cellsAlongWire;
  else
    cellsAlongWireScale = 2.*cellsAlongWire;

  // Move along base vectors
  dispP3P1 = p1 + baseP3P1*(upperScale*(zMeanTemp + 0.5)/cellsAlongWireScale);
  dispP2P0 = p0 + baseP2P0*(lowerScale*(zMeanTemp + 0.5)/cellsAlongWireScale);

  // Correct for the gap between the two sides of PC2/3
  if (zMean >= cellsAlongWire)
  {
    dispP3P1 = dispP3P1 + baseP3P1*zgap;
    dispP2P0 = dispP2P0 + baseP2P0*zgap;
  }

  // Define the connection between the upper and lower base vectors
  connect = (PHVector)(dispP3P1 - dispP2P0);

  // Move along the connection vector to the hit
  globalHit = dispP2P0 + connect*((wMean + 0.5)/cellsAcrossWire[currentPc]);


  // Store mean values in phool tables
  index = dPcXCluster->RowCount();
  dPcXCluster->set_id(index,index);
  dPcXCluster->set_arm(index,currentArm);
  dPcXCluster->set_sector(index,currentSectorReduced);
  dPcXCluster->set_xyz((size_t)0,index,globalHit.getX());
  dPcXCluster->set_xyz((size_t)1,index,globalHit.getY());
  dPcXCluster->set_xyz((size_t)2,index,globalHit.getZ());
  dPcXCluster->set_dxyz((size_t)0,index,dxyz[0]);
  dPcXCluster->set_dxyz((size_t)1,index,dxyz[1]);
  dPcXCluster->set_dxyz((size_t)2,index,dxyz[2]);
  dPcXCluster->set_wire(index,cw);
  dPcXCluster->set_cell(index,cz);
  dPcXCluster->set_type(index,numCells);
  dPcXCluster->SetRowCount(index+1);

  // rindex is equal to index (above)
  rindex = dPcXRawClus->RowCount();
  dPcXRawClus->set_clusid(rindex,index);
  dPcXRawClus->set_rawid(rindex,id);
  dPcXRawClus->SetRowCount(rindex+1);

  // Out-comment these lines if you want to write complete
  // hit information to file
  //  if (debug > 0)
  //    {
  //      if (debug == 3)
  //	{
  //	  cout << "index   : " << index << endl;
  //	  cout << "id      : " << id << endl;
  //	  cout << "arm     : " << currentArm << endl;
  //	  cout << "sector r: " << currentSectorReduced << endl;
  //	  cout << "sector  : " << currentSector << endl;
  //	  cout << "x       : " << globalHit.getX() << endl;
  //	  cout << "y       : " << globalHit.getY() << endl;
  //	  cout << "z       : " << globalHit.getZ() << endl;
  //	  cout << "cw      : " << cw << endl;
  //	  cout << "cz      : " << cz << endl;
  //	  cout << "wMean   : " << wMean << endl;
  //	  cout << "zMean   : " << zMean << endl;
  //	  cout << "numCells: " << numCells << endl;
  //	  cout << "-----------------------" << endl;
  //	}
  //      else if (debug == 4)
  //	{
  //	  outfile << eventNumber << " " << currentPc << " " << currentArm 
  //		  << " " << currentSide << " " << currentSectorReduced 
  //		  << " " << globalHit.getX() << " " << globalHit.getY() 
  //		  << " " << globalHit.getZ() << " " << zMean << " " 
  //		  << wMean << endl;
  //	}
  //      else if (debug == 5)
  //	{
  //	  outfile << eventNumber << " " << currentPc+1 << " " 
  //		  << currentSector+1 << " " << globalHit.getX() << " " 
  //		  << globalHit.getY() << " " << globalHit.getZ() << " " << endl;
  //	}
  //    }
} // End method PadRec::setPosition




//********************************************************
// Name:       init
//
// Definition: Initialize (private)
// 
// History:    4/26/00 Original by P. Nilsson
//********************************************************

short PadRec::init(short pc)
{
  short stat = 0;

  // Default settings
  PadRec::setPadChamber(pc);
  eventNumber = 0;
  debug = 0;
  splitMax = 100;
  mode = 1;
  padTypeLimit = 2;
  oneW = 4;
  oneZ = 4;
  oneL = 11;
  twoW = 8;
  twoZ = 8;
  twoL = 16;
  threeW = 100; // These are not used anymore
  threeZ = 100;
  threeL = 100;

  return stat;
} // End method PadRec::init

short PadRec::init_event(short pc, PHCompositeNode* topNode)
{
  short stat = 0;
  // Get the top node iterator
  PHNodeIterator topIter(topNode);

  // Find the input tables ------------------------------------------------

  if (pc == 0)
    {
      dPcXRawNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc1Raw"));
      dPcXRawClusNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc1RawClus"));
    }
  else if (pc == 1)
    {
      dPcXRawNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc2Raw"));
      dPcXRawClusNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc2RawClus"));
    }
  else if (pc == 2)
    {
      dPcXRawNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc3Raw"));
      dPcXRawClusNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc3RawClus"));
    }
  else 
    {
      cerr << __FILE__ << ":" << __LINE__
           << " wrong pc " << pc << " input " << endl;
      return -1;
    }

  if (!dPcXRawNode)
    {
      cout << "PadRec::event ERROR: dPc" << pc+1 << "Raw not found." << endl;
      stat = -1;
    }
  if (!dPcXRawClusNode)
    {
      cout << "PadRec::event ERROR: dPc" << pc+1 << "RawClus not found." << endl;
      stat = -1;
    }
  if (dPcXRawNode)
    {
      dPcXRaw = static_cast<dPadRawWrapper*>(dPcXRawNode->getData());
    }
  else
    {
      dPcXRaw = NULL;
    }
  if (dPcXRawClusNode)
    {
      dPcXRawClus = static_cast<dPadRawClusWrapper*>(dPcXRawClusNode->getData());
    }
  else
    {
      dPcXRawClus = NULL;
    }

  // Output tables ----------------------------------------------------------

  if (currentPc == 0)
    dPcXClusterNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc1Cluster"));
  else if (currentPc == 1)
    dPcXClusterNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc2Cluster"));
  else if (currentPc == 2)
    dPcXClusterNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc3Cluster"));
  if (!dPcXClusterNode)
    {
      cout << "PadRec::event ERROR: dPc" << currentPc+1 << "Cluster not found." << endl;
      stat = -1;
      return stat;
    }
  dPcXCluster = static_cast<dPadClusterWrapper*>(dPcXClusterNode->getData());

  // Keep track of the number of cells and clusters per sector
  for (int i = 0; i < numberOfSectors[currentPc]; i++)
    {
      numberOfCells[i] = 0;
      numberOfClusters[i] = 0;
    }

  // Fill map array : padz,padx -> cell w
  this->fill_Map_padxz_to_cellw();
  
  return stat;
} // End method PadRec::init_event


void PadRec::fill_Map_padxz_to_cellw()
{
  for (short padx=0;padx<40;padx++) {
    for (short padz=0;padz<108;padz++) {
      wmap[padz][padx]=3*(padx%20)+wireAdjust[(padz-1+3)%3] + (padx/20)*58;
    }
  }
}
