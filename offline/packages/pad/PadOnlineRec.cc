//---------------------------------------------------------------  
// Description: Implementation of PadOnlineRec class 
//---------------------------------------------------------------- 

#include "Event.h"
#include "packet.h"
#include "PadOnlineRec.h"  
#include "phool.h"

#include <cmath>
#include <cstdlib>
#include <cstring>
#include <iostream>

// couldn't fit 2 arrays into the namespace in the header file, so I put them
// here instead :=)

// event counter numbers:
const int EVT_NR_TYPE[5] = { 0x1, 0x9, 0x5, 0xd, 0x3}; 
// i.e. 0001, 1001, 0101, 1101, 0011 - seems suspicious?: 
// Look at the 3 MS bits in the nibble, and reverse them: 
// 000, 001, 010, 011, 100 - that looks more in order 0-4 :=)

// HV division info; 4 divisions per chamber, and different for the different layers
const short nwiresPerHVSector[12] = { 9, 20, 20, 9, // PC1 
				      24, 34, 34, 24, // PC2
				      29, 29, 29, 29 }; // PC3

using namespace std;
using namespace PadOnlineRecPar;

// constructor 
PadOnlineRec::PadOnlineRec(short ipacketid) 
{ 
  packetid = ipacketid; 
  decodePacketid();
  init();
}
 
// initialization routine on start-up
void PadOnlineRec::init() 
{
  if ( layer==1 ) {
    pc2check = 1;
  }
  else {
    pc2check = 0;
  }

  // *** for event address check
  // set the badrocEvtNR array to zero (good as default)
  nrbadrocEvtNR=0;
  for (int i=0;i<MAX_ROCS;i++) {
    badrocEvtNR[i]=0;
  }
  
  diagonalAdj = 1; // allow diagonal adj. cells to be treated as neighbours
  
  reset_counters(); // standard counters, has to be reset event-by-event
  reset_rest(); // init/reset all pad, cell, cluster counters and arrays

  // Reset unpacking mask for bad channels (TGL level is the finest level we use here)
  for(int k=0; k<NPADZ; k++) {
    mask[k] = 0xfffff; // all bits to be looked at by default
  }

  // init padpossdiff for offline style cell rec.
  for (unsigned int zmod3 = 0; zmod3<3; zmod3++)
    {
      // first possibility is the same for all zmod3 values
      padpossdiff[zmod3][0][0] = 0;
      padpossdiff[zmod3][0][1] = 0;
      switch (zmod3)
	{
	case 0:    
	  padpossdiff[zmod3][1][0] = 0;
	  padpossdiff[zmod3][1][1] = 1;
	  padpossdiff[zmod3][2][0] = 1;
	  padpossdiff[zmod3][2][1] = 1;
	  break;
	case 1:    
	  padpossdiff[zmod3][1][0] = 0;
	  padpossdiff[zmod3][1][1] = -1;
	  padpossdiff[zmod3][2][0] = 1;
	  padpossdiff[zmod3][2][1] = 0;
	  break;
	case 2:    
	  padpossdiff[zmod3][1][0] = -1;
	  padpossdiff[zmod3][1][1] = -1;
	  padpossdiff[zmod3][2][0] = -1;
	  padpossdiff[zmod3][2][1] = 0;
	  break;
	default:
	  cerr << PHWHERE 
	       << " ERROR No such number. How did this happen?\n";
	  break;
	}
    }
}

//********************************************************************** 
int PadOnlineRec::processEvent(Event * evt) 
{
  // reset main counters
  reset_counters();

  Packet * p = evt->getPacket(packetid);
  if (p) 
    {
      iFlag=1; // we have read the right packet this event
      numberProcEvents++;

      // *** check for bad event addresses
      // 1. check which event counter values occur, and how many times
      for (int ROC = 0; ROC<NROCS; ROC++) {
	int ctrval =  p->iValue(8-ROC,"CHECKCTR") & 0xfffff;
	for (int ROW = 0; ROW<NROWS; ROW++) {
	  if (badrocEvtNR[ROC*NROWS+ROW]==0) { // check that the ROC is not bad
	    int bitshift=16-ROW*4;
	    int segctrval=(ctrval>>bitshift & 0xf);

	    int itype = 0;
	    bool match = false;
	    while( itype<NBUFF && !match) { 
	      if (segctrval==EVT_NR_TYPE[itype]) {
		nrCorrEvtNR[itype]++;
		match = true;
	      }
	      itype++;
	    }
	  }
	}
      }
      
      // 2. which event # was the most common one? Assign it to EvtNR..
      int tmpmax = 0;
      EvtNR = -1;
      for (int i=0;i<NBUFF;i++) {
	if (nrCorrEvtNR[i]>tmpmax) {
	  tmpmax=nrCorrEvtNR[i];
	  EvtNR=i;
	}
      }
      // *** end of event counter checking

      // ok, let's then start the reconstruction by unpacking the data..
      // 1. Get all the 20-bit data-word info
      int nw = 0; // number of words
      p->fillIntArray(padsRaw,NPADZ,&nw);

      // 2. unmask the known problematic ones (set via SetBadRoc method)
      for (int i = 0; i < NPADZ; i++) {
	/* // Debug block
	if (padsRaw[i]>0 || mask[i]!=0xfffff) {
	  cout << PHWHERE 
	       << " packetid " << packetid
	       << " word " << i 
	       << " raw " << hex << padsRaw[i]
	       << " mask " << mask[i] << dec
	       << endl;
	}
	*/
	padsOn[i] = padsRaw[i] & mask[i];
      }

      // 3. PC2 layers require some extra care, and un-scrambling of data.. 
      if ( pc2check==1 ) {
	// PC2 has a channel swap in real data
	// that we should account for
	for (int i = 0; i < NPADZ; i++) {
	  int padcol = 11 - i%12;
	  if ( (padcol == 6) || (padcol == 7) )
	    {
	      // switching 2:11 and 2:13 for PC2 (holes 19 and 44)
	      if (padcol == 6)
		{
		  padsOn[i] = (padsRaw[i] & mask[i]) & 0xddddd; // only take the other channels
		  if (i > 0) { // to help coverity...
		    padsOn[i] += ( (padsRaw[i-1] & mask[i-1]) & 0x88888 ) >>  0x2;
		    // and add some from the other (shifted)
		  } // end if for coverity...
		}
	      else // padcol == 7
		{
		  padsOn[i] = (padsRaw[i] & mask[i]) & 0x77777; // only take the other channels
		  if (i < (NPADZ-1) ) { // to help coverity...
		    padsOn[i] += ( (padsRaw[i+1] & mask[i+1]) & 0x22222 ) << 0x2;
		    // and add some from the other (shifted)
		  } // end if for coverity...
		}
	    }
	}
      }

      int nrpads=0;
      // 4. ok, we now have the 20-bit data word info. Let's now get the 
      // individual pad info too for the sum counting. We did the PC2 swap above already..

      for (int iword=0; iword<NPADZ; iword++) {
	if (padsOn[iword] > 0) {
#ifdef DEBUG 
	  cout << PHWHERE << " iword " << iword 
	       << " padsOn[iword] " << padsOn[iword] << endl;
#endif
	  for (int j=0; j<NPADX; j++) { // data is contained in lowest 20 bits (20 = NPADX)
	    int k = (NPADZ-iword)*NPADX - j - 1;
	    int ival = ( (padsOn[iword] >> j) & 0x1 );
	    	    
	    if (ival) { // this pad fired
	      padK[nrpads] = k;
	      nrpads++;
	      fired_pad[k]++;
	    }
	  }
	}
      }
    
#ifdef DEBUG 
      cout << "nrpads= " << nrpads << endl;
#endif
      // 5. We have all the pad information we need. Now, let's call the 
      // cell reconstruction and clusters, if there were enough fired pads and cells respectively
      numberOfFiredPads=nrpads;
      cells.clear(); // zero/clear cell list

      if ( numberOfFiredPads > 2 ) { // otherwise, we can not possible have any cells

	pads_to_cells();

	numberOfFiredCells = cells.size();

	if ( numberOfFiredCells > 0 ) {
	  // let's also do the clustering then
	  cells_to_clust();
	}
      }
      
      // 6. And we are done with the reconstruction for this event and packet..
      delete p;      
         
    } // if (p)    	
  else { 
    // ******** extra careful if the packet does not exist... *********
    EvtNR=-2;
    return -1;
  }
      
  return 0;
}
    
//********************************************************************** 
void PadOnlineRec::pads_to_cells() 
{
  // cell reconstruction copied from Lvl2: lvl2_distribution/accessors/L2PadHitFinder.C
  // Use the full 20-bit data words to find possible cell-candidates
  // Then call getCells method for a final check and to calculate position of cell-candidates 
  unsigned int nw = 0;
  int oldval = 0;
  int oldpos = 0;
  int pos;    
  int matchp,matchm; // plus and minus step matches

  memset(padsLocal, 0, sizeof(padsLocal));

  for (int iword=0; iword<NPADZ; iword++) 
    {
      if (padsOn[iword] > 0) // only want non-zero words 
	{
	  int data = padsOn[iword];
	  pos = NPADZ - 1 - iword; // pos starts in the middle of the sector (z=0)  
	  // prepare for clustering
	  if (nw == 0) {
	    padsLocal[nw] = data;
	    nw = 1;   
	  }
	  else // see if this word next to the old one, and have bit overlaps
	    {
	      matchp = data << 0x1;
	      matchm = data >> 0x1;
	      if ( (pos == (oldpos-1) ) && 
		   ( ( (oldval & data) > 0 ) ||
		     ( (oldval & matchp) > 0 ) ||
		     ( (oldval & matchm) > 0 ) ) )
		{ // this has a pad overlap with the previous one
		  // not necessarily true that they can form a cell together
		  padsLocal[nw] = data;
		  nw++;
		}
	      else // no overlap: calc. probable hit coord and clear 
		{
		  if (nw>2) // can not have a valid hit otherwise 
		    {
		      getCells(oldpos,nw);
		    }
		  padsLocal[0] = data;
		  nw = 1; // reset
		}
	    }
	  oldpos = pos;
	  oldval = data;
	}
    }
  
  // did we miss the last one?
  if (nw>2) // can not have a valid hit otherwise 
    {
      getCells(oldpos,nw);
      nw = 0;
    }
  
  return;
}

//********************************************************************** 
void PadOnlineRec::getCells(int oldpos, unsigned int nw)
{
  // overlapping datawords with cell candidates -> calculated cells/positions
  int match;
  int cellz, cellwire, zmod3;
  int jw, jword, jbit;
  int possible[3];
  
  // start at the _last_ one, this is the one with the lowest k
  jword = nw-1;
  
  // the last cells are rec. when we start at
  // 2 (the first word is 0)
  while ( jword > 1 ) 
    {
      if (padsLocal[jword] > 0)
	{ 
	  jbit = 0;
	  match = 0x1;
	    
	  // find first non-zero-bit
	  while ( (jbit<NPADX) && ( !(padsLocal[jword] & match) ) )
	    {
	      jbit++; 
	      match = 0x1 << jbit;
	    }

	  padsLocal[jword] -= match; 
	  // only subtract from the first of the three words
	  // - a pad can take part in many cells
	    
	  // which bits should we check in the next word?
	  // that depends on the z-pos
	  cellz = oldpos + nw - 1 - jword; 
	  zmod3 = cellz%3;
	    
	  int j;
	  for (j = 0; j < 3; j++)
	    {
	      possible[j] = 1; 
	      // clear this separately since we might want to 
	      // exploit the correlations between the possibilities
	      // later (upgrade). If one doesn't work, the next one 
	      // might not work either. 
	    }

	  for (j = 0; j < 3; j++)
	    {
	      if (possible[j])
		{ // we have a chance that this cell is on
		  jw = 0; // to the 2nd word 
		  while ( (jw < 2) && (possible[j]) )
		    {
		      int jbitnew = jbit - padpossdiff[zmod3][j][jw];
		      if ( (jbitnew > -1) && (jbitnew < NPADX) )
			{  
			  match = 0x1 << jbitnew;
			    
			  if (!(padsLocal[jword-jw-1] & match)) 
			    // this next word doesn't have the right pad on  
			    {
			      possible[j] = 0; // this cell is not on
			    }
			}
		      else 
			{
			  possible[j] = 0; // there is no such cell
			}
		      jw++;
		    }

		  if (possible[j]) // all pads were on
		    {
		      // we have ourselves a cell and already know cellz
		      cellwire = ( ((NPADX-1-jbit)*3 + 
				    padpossdiff[zmod3][j][0] + 
				    padpossdiff[zmod3][j][1])*3 )/3; 
    
		      cells.push_back(Cell(cellwire, cellz));
		    }
		}
	    } // j: possible comb.
	}
      else 
	{
	  jword--; // try the next one
	}
    }

  return;
}

//********************************************************************** 
int PadOnlineRec::cells_to_clust()
{
  // Take the list of cells we have found and turn them into groups of cells, or 'cluster'
  int  ncells;
  numberOfClusters = 0;

  ncells = numberOfFiredCells;

  if ( ncells<=0 )
    return ncells;

  // algorithm from PadRec by PN
    
  // turn the 'cells' list into clusters
  Cell newCell;
  list<Cell> newCluster;
  short z0, w0, z, w, nincluster;
  int avew, avez;
  
  list<Cell> cells_local; // this is the local/copy list of cells that will be 
  // modified as part of the clustering
  CI iter = cells.begin();
  for(iter = cells.begin(); iter != cells.end(); iter++) {
    cells_local.push_back(*iter);
  }


#ifdef DEBUG 
  cout << "Setup completed. ncells = " << ncells << " " << cells.size() << endl;
#endif

  while (cells_local.size()>0) { // while we still have something to do clustering with..
    
#ifdef DEBUG
    cout << "ncells = " << cells_local.size() << endl;
#endif

    // Get the first cell from the list, store it and erase it
    CI ci = cells_local.begin();
    w0 = (*ci).w;
    z0 = (*ci).z;
    newCell.w = w0;
    newCell.z = z0;
    
    newCluster.clear();
    newCluster.push_back(newCell);
    cells_local.erase(ci);
    
    nincluster = 1; // size of newCluster is 1..
    avez = z0;
    avew = w0;
    
    // Find the neighbors to all members of the new cluster
    CI nci = newCluster.begin();
    
    while (nci != newCluster.end())
      {
	w0 = (*nci).w;
	z0 = (*nci).z;
	ci = cells_local.begin();
	
	// Find the neighbours to this cell and store them in the same list
	// added a cut so that we don't make too large clusters
	while ( (ci != cells_local.end()) && (nincluster<MAX_CLUSTERSIZE) )
	  {
	    w = (*ci).w;
	    z = (*ci).z;
	    
	    if (cell_adjacent(z0, w0, z, w))
	      {
		// Add cell to the new cluster
		newCell.w = w;
		newCell.z = z;
		newCluster.push_back(newCell);
		avez += z;
		avew += w;
		nincluster++;
		
		// Remove cell from the list
		cells_local.erase(ci--);
	      }
	    ci++;
	  }
	nci++;
      }
    
    // newCluster has been formed and grabbed all its neighbours. 
    // Now, it's time for us to store it, if possible
#ifdef DEBUG 
    cout << "cluster info: i sect size z wire\n";
#endif
    if (numberOfClusters<MAX_CLUSTERS) {
      cluster[numberOfClusters].size = nincluster; // should be eq to newCluster.size()
      if (nincluster!=0) {
	cluster[numberOfClusters].sectz = avez/(1.0*nincluster);
	cluster[numberOfClusters].sectwire = avew/(1.0*nincluster);
#ifdef DEBUG 
	cout << numberOfClusters << " "
	     << cluster[numberOfClusters].size << " "
	     << cluster[numberOfClusters].sectz << " "
	     << cluster[numberOfClusters].sectwire << endl;
#endif
      }
    }
    numberOfClusters++;
  }

#ifdef DEBUG
  cout << "numberOfClusters = " << numberOfClusters << endl;
#endif
  return 0;
}
//********************************************************************** 
short PadOnlineRec::getNumberFiredPads() 
{ 
  // This is the number of fired pads in one event! 
  return numberOfFiredPads;    
}
/* end getNumberFiredPads() */
//********************************************************************** 
short PadOnlineRec::getFiredPad(short i) 
{ // zero-suppressed array of which pads fired in the event
  if ( (i>-1) && (i<numberOfFiredPads) )
    return padK[i];
  else {
    cerr << PHWHERE << " ERROR Argument out of bounds" << endl; 
    return -1;
  }
}  
/* end getFiredPad() */
//********************************************************************** 
short PadOnlineRec::getNumberFiredCells() 
{ // number of cells that fired in this event
  return numberOfFiredCells;  
}  
/* end getNumberFiredCells() */
//********************************************************************** 
short PadOnlineRec::getFiredCellInfo(int sizeArray, int *zArray, int *wireArray)
{  
  // here we'll happily fill the arrays we're fed - it's up to the user to check that
  // the arrays are not too small.. but we'll help with a simple check up-front rather than just seg fault..
  if ( (unsigned int)sizeArray < cells.size() ) {
    cerr << PHWHERE << " ERROR - underdimensiond array" << endl; 
    return -1;
  }

  // go over the cell list and fill the arrays
  CI iter = cells.begin();
  Cell c;
  int n = 0;
  for(iter = cells.begin(); iter != cells.end(); iter++) {
    c = *iter;
    zArray[n] = c.z;
    wireArray[n] = c.w;
    n++;
  }
  return n;
}  
/* end getFiredCellInfo() */

//********************************************************************** 
short PadOnlineRec::getFiredCellSectInfo(int sizeArray, int *zArray, int *wireArray)
{  
  // here we'll happily fill the arrays we're fed - it's up to the user to check that
  // the arrays are not too small.. but we'll help with a simple check up-front rather than just seg fault..
  if ( (unsigned int)sizeArray < cells.size() ) {
    cerr << PHWHERE << " ERROR - underdimensiond array" << endl; 
    return -1;
  }
  
  CI iter = cells.begin();
  Cell c;
  int n = 0;
  for(iter = cells.begin(); iter != cells.end(); iter++) {
    c = *iter;
    short zval = c.z;
    
    // the z and wire info in this routine is on a sector level, not on the FEM level
    // we thus may need to rotate and translate a bit
 
    if (side==0) // reverse z 
      zval = (NCELLZ-1) - zval;
    else // add on to this side
      zval += NCELLZ;

    zArray[n] = zval;
    wireArray[n] = getSectWire(c.w);
    n++;
  }
  return n;

}  
/* end getFiredCellSectInfo() */
//********************************************************************** 
void PadOnlineRec::print() 
{ 
  // Print the parameter information 
  cout << "PACKETID= " << packetid << endl;  
  if (iFlag==0) {
    cout << "The object is ready to take data but has not done so yet"
	 << " or a reset has been made \n";
    }
  else {
    cout << PHWHERE << "Number of fired pads= " << numberOfFiredPads << endl  
	 << "Number of fired cells= " << numberOfFiredCells << endl;  
  }
}  
/* end print() */
//********************************************************************** 
void PadOnlineRec::reset_counters() 
{ // this is all taken care of inside processEvent, i.e. users do not need to call this explicitly
  iFlag = 0;
  numberOfFiredPads=0;
  numberOfFiredCells=0;
  numberOfClusters=0;
  
  for (int i=0; i<NBUFF; i++) {
    nrCorrEvtNR[i]=0;
  }
}  
/* end reset_counters() */

//********************************************************************** 
void PadOnlineRec::reset_rest() 
{ 
  // also accumulated counters over all events since last full resets
  numberProcEvents=0;
  memset(fired_pad, 0, sizeof(fired_pad));
  for (int irow = 0; irow<NROWS; irow++) {
    for (int icol = 0; icol<NROCS; icol++) {
      for (int itgl = 0; itgl<NTGLS; itgl++) { 
	tgl_hits[irow][icol][itgl] = 0;
      }
    }
  }

  
  // for good measure reset the arrays and lists too
  // cells
  cells.clear();
  // clusters
  for (int i=0; i<MAX_CLUSTERS; i++) {
    cluster[i].sectz = 0.0;
    cluster[i].sectwire = 0.0;
    cluster[i].size = 0;
  }

}  
/* end reset_rest() */
//********************************************************************** 
int PadOnlineRec::getNumberOfTimesPadFired(short k)
{ 
  // Get number of times this pad fired
  // This is accumulated over all events analysed 
  if ( (k>=0) && (k<MAX_PADS) ) 
    return fired_pad[k];
  else {
    cerr << PHWHERE << " ERROR getNumberOfTimesPadFired variables out of bounds.." << endl; 
    return -1; 
  } 
} 
/* end getNumberNumberOfTimesPadFired(short k) */
//**********************************************************************
short PadOnlineRec::setBadROC(int grow, int gcol, int badtgltype[NTGLS])
{
  short tgl,padcol;
  short iword, move;
  int match;
  if ( (grow<NROWS) && (grow>-1) && (gcol<NROCS) && (gcol>-1) ) {
    // ok, the indices are within the allowed range
    // loop over all channels in this ROC
    for (tgl=0; tgl<NTGLS; tgl++) {
      if (badtgltype[tgl]!=0) { // mask out all TGLs that have some non-zero error flag, 1 or 2 
	// a TGL covers a 4x4 grid of channels/pads
	move = grow*4; // index of first bit in padrow from TGL
	match = (0x000f0000 >> move); // these are the bits we want to mask out
	for (padcol=0; padcol<4; padcol++) { 
	  // Then we want figure out which word and bit we should manipulate
	  // Use the following 2 identities:
	  // k = (gcol*12+padcol)*NPADX + grow*4+padrow
	  // k = (NPADZ-iword)*NPADX - j - 1 = (NPADZ-iword-1)*NPADX + NPADX - j - 1;
	  // <=>
	  // gcol*12+padcol = NPADZ-iword-1
	  // grow*4+padrow = NPADX - j - 1;
	  // j = NPADX - (grow*4+padrow) - 1; // the bit
	  iword = NPADZ - (gcol*12+tgl*4+padcol) - 1;
	  // remove the relevant bit from the mask
	  // could probably do this subtraction more elegantly with a NOT operator or something
	  // but want to keep the code readable too :=)
	  if ( (mask[iword] & match) == match) { // not already masked off
	    mask[iword] -= match; // ok, subtract it off then
	  }
	}
      }
    }

    // *** for event address check
    if (badtgltype[0]!=0) {
      badrocEvtNR[gcol*NROWS+grow]=1;
      nrbadrocEvtNR++;
    } 

    return 0;
  } // indices within limits 
  else 
    return -1;
}

//********************************************************************** 
void PadOnlineRec::decodePacketid() 
{ 
  // some help constants here:
  const short FIRSTPACKET = 4001;
  const short NFEMSPERSIDE = 8;
  const short NFEMSPERPLANE = 16;
  const short NFEMSPERLAYER = 32;
  const short NHVSECTORS = 32; // per plane

  if ( (packetid>4000) && (packetid<4097) ) 
    {
#ifdef DEBUG 
      cout << "Decoding packetid " << packetid << endl;
#endif
      short packetred = packetid-FIRSTPACKET;
      layer = packetred/NFEMSPERLAYER;
      arm = 1 - (packetred%NFEMSPERLAYER)/NFEMSPERPLANE;
      side = (packetred%NFEMSPERPLANE)/NFEMSPERSIDE;
      subsector = packetred%NFEMSPERSIDE; // same as sector for PC1, sector/2 for PC2/3

      hvsectorstart = 0;
      if (layer==0) // PC 1
	hvsectorstart += 4*subsector;
      else  // PC2/3
	{
	  hvsectorstart += 4*(subsector/2);
	  if (side==1) 
	    hvsectorstart += NHVSECTORS/2;
	}  

      modsubsect = subsector%2;
      offset = 0;      
      if (layer>0) {
	if ( ((arm==side) && (modsubsect!=0)) ||
	     ((arm!=side) && (modsubsect==0)) ) offset=1;
    
	if ( (layer==2) && (arm==1) )  // PC3 West has MB facing IP
	  offset=1-offset; // should be the other way around..
      }
    }
  else {
    cerr << "PadOnlineRec::decodePacketid ERROR You should change your packetid " << packetid << " before attemtping to rec. data" << endl;
    layer = -1;
    arm = -1;
    side = -1;
    subsector = -1;
    hvsectorstart = -1;
    offset = -1;
    modsubsect = -1;
  }
  return;
}  
/* end decodePacketid() */
//********************************************************************** 
bool PadOnlineRec::cell_adjacent(short z1, short wire1, short z2, short wire2)
{
  // returns true if 2 cells are adjacent.. 
  short zdiff, wirediff;
  
  zdiff=abs(z1-z2); 
  if (zdiff > 1) return false;
  wirediff=abs(wire1-wire2); 
  if (wirediff>1) return false;

  if ((zdiff<=1) && (wirediff<=1)) {
    if ( (diagonalAdj==-1) && (zdiff==1) && (wirediff==1) ) 
      return false; // diagonally adjacent cells in 
    // the same cluster is not allowed
    else
      return true;
  }
  else
    return false;   
}
//********************************************************************** 
int PadOnlineRec::getHVSector(int ip, int wire)
{
  short wireval = wire;
  short sectwire = getSectWire(wireval);

  int addition = 0;
  while ( (sectwire>=0) && (addition<4) ) {
    sectwire -= nwiresPerHVSector[layer*4+addition];
    addition++; 
  }
  int hvsector = ip + hvsectorstart + addition - 1;
  
  return hvsector;
}
/* end  */
//********************************************************************** 
int PadOnlineRec::getHVSector(int ip, float wire)
{
  wire += 0.49999; // not 0.5..
  short wireval = (short) wire;
  short sectwire = getSectWire(wireval);

  int addition = 0;
  while ( (sectwire>=0) && (addition<4) ) {
    sectwire -= nwiresPerHVSector[layer*4+addition];
    addition++; 
  }
  int hvsector = ip + hvsectorstart + addition - 1;
  
  return hvsector;
}
//********************************************************************** 
int PadOnlineRec::getSectWire(int wire)
{
  short wireval = wire;
      
  if ( (layer==0) && (side==0) ) // reverse wire 
    wireval = (NWIRES-1) - wireval;
      
  if (layer>0) {
    wireval+=offset*NWIRES;
    if (side==0) // reverse wire 
      wireval = (2*NWIRES-1) - wireval;
  }

  return wireval;
}

//**********************************************************************               
void PadOnlineRec::Fill_TGL_hits(Event *evt) 
{
  Packet * p = evt->getPacket(packetid);
  if (p) {
    // PC2 swap doesn't cross TGL boundaries so realk vs k doesn't matter for bad TGL search
    // so just keep the one index..
    int n = 0;
    int n_per_roc = 12;
    int iword, j, ival, col, grow, gcol, tgl;
    
    for (iword=0; iword<NPADZ; iword++) {
      if (padsRaw[iword] > 0) {
	col = NPADZ - iword - 1;
	gcol = col/n_per_roc;
	// 4 columns of pads for each TGL
	tgl = ( col%n_per_roc )  / 4;
	
	for (j=0; j<NPADX; j++) { // data is contained in lowest 20 bits (20 = NPADX)
	  ival = ( (padsRaw[iword] >> j) & 0x1 );
	  
	  if (ival) { // this pad fired
	    grow = (NPADX - j - 1) / 4;
	    tgl_hits[grow][gcol][tgl] ++;
	    n++;
	  }
	}
      }
    }
#ifdef DEBUG
  cout << PHWHERE << " packetid " << packetid << " nfired " << n << endl;
#endif
  delete p;
  } // packet was in the data
  return;
}
//**********************************************************************               
int PadOnlineRec::Get_TGL_hits(int grow, int gcol, int tgl)
{
  return tgl_hits[grow][gcol][tgl];
}
//********************************************************************** 
