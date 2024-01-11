//--------------------------------------------------------
//
// Class: PadRec (definition)
//
// Created by: Paul B Nilsson
//
// Description: PC Cluster Reconstruction
//
// Details: This is the Pad Chamber cluster class that
//          contains the cluster reconstruction algorithm
//
//--------------------------------------------------------

#ifndef __PADREC_HH__
#define __PADREC_HH__

#include <phool.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h> 
#include <PHIODataNode.h> 
#include <PHNode.h>
#include <PHTable.hh>
#include <PHPanel.h>
#include <PadRecCell.hh>
#include <padDetectorGeo.hh>

#include <list>
#include <cmath>
#include <fstream>

class padDetectorGeo;
class dPadRawWrapper;
class dPadRawClusWrapper;
class dPadClusterWrapper;

// Cell type
//struct Cell { short w, z, celltype, id; };
typedef PadRecCell Cell;

// STL cell list iterator
typedef std::list<Cell>::iterator CI;

// Phool table node
typedef PHIODataNode<PHTable> TableNode_t;

// Definitions
static const short numberOfPads = 2160;
static const short numberOfSectors[3] = { 16, 8, 8 };
static const short numberOfPadWires[3] = { 20, 40, 40 };
static const short wireAdjust[3] = { 0, -1, 1 };
static const short cellsAlongWire = 106;
static const short padsAlongWire = 108;
static const short cellsAcrossWire[3] = { 58, 116, 116 };

class PadRec {

 public:

  PadRec(short); // Default Constructor
  PadRec(const PadRec &) { };       // Copy Constructor
  virtual ~PadRec(void);                    // Destructor

  // Set and get methods
  void setParameters(short *);
  void setSector(short, padDetectorGeo *);
  inline void setPadChamber(short pc) { currentPc = pc; }
  inline void setArm(short arm) { currentArm = arm; }
  inline void setSide(short cz) { currentSide = (cz < cellsAlongWire) ? 0 : 1; }
  inline short getNumberOfCells(void) { return (cells[currentSector].size()); };
  inline short getNumberOfCells(std::list<Cell>& li) { return (li.size()); };

  // Main reconstruction algorithm methods
  std::list<Cell> getCluster(void);             // Find a cluster in a hitlist
  std::list<Cell> fakeCluster(void);            // Create a fake cluster (for debugging)
  short getCells(void);                    // Convert pads to cells
  short getNumberOfParticles(std::list<Cell>&); // Estimation of #particles in a cluster
  void dumpCells(std::list<Cell>&);             // Dump the list (for debugging)
  void showStat(void) const;               // Dump some statistics
  inline void clear(void)                  // Erase and delete the main cell list
    { cells[currentSector].clear(); };
  void processCluster(std::list<Cell>&, padDetectorGeo *); // Process a cluster
  void setPosition(std::list<Cell>&, padDetectorGeo *);    // Determine hit position
  void fill_Map_padxz_to_cellw();

  //Evt-by-evt init
  short init_event(short, PHCompositeNode *); 

 private:

  // Pointers to the phool tables
  dPadRawWrapper     *dPcXRaw;
  dPadRawClusWrapper *dPcXRawClus;
  dPadClusterWrapper *dPcXCluster;
  TableNode_t *dPcXRawNode;
  TableNode_t *dPcXRawClusNode;
  TableNode_t *dPcXClusterNode;

  std::list<Cell> cells[16];                   // Main cell lists for all sectors
  short debug;                            // Global debug flag
  short mode;                             // Cluster split flag
  short currentPc;                        // 0,1,2 for PC1,2,3
  short currentArm;                       // 1 for West, 0 for East
  short currentSector;                    // 0-15 for PC1, 0-7 for PC2/3
  short currentSectorReduced;             // 0-7 for PC1, 0-3 for PC2/3
  short currentSide;                      // 0 for South, 1 for North
  short padTypeLimit;                     // Require at least x number of normal
  short wmap[108][40];

  short oneW, oneZ, oneL, twoW, twoZ, twoL, threeW, threeZ, threeL;
  short numberOfCells[16], numberOfClusters[16];
  short eventNumber;
  unsigned short splitMax;

  // Output file with hit information
  std::ofstream outfile;

  short init(short);         // Initialize
  short getkPadID(std::list<Cell>&, double, double); // Get simulation pixel id
  inline short near(double a) const             // Round off method
    { return ((short)(a + 0.5) ); };

  // Logical database variable. True if geometry is read from database
  // and False if geometry is read from the padGeometry.txt ascii file
  PHBoolean fromDatabase;

  // The pcPanel objects correspond to real pad chamber volumes in the global frame
  // and are used here to build the sector frames
  PHPanel pcPanel[padDetectorGeo::PC1MAXSECTPERARM];

  // Pointer to the geometron object (to be used for coordinate transformations)
  // Clusters dimensions
  void getDimensions(std::list<Cell>&, short&, short&, short&, short&) const;
  inline short sideLength(float length, float nparts) const
    { return ( (short)ceil( (length/nparts - 0.000001) ) ); };

  // Checks if two cells are neighbours.
  // - Do not allow diagonals: (default setting)
  // inline short isNeighbor(short dw, short dz) const
  //   { return ((((dw <= 1) && (dz <= 1)) && !((dw == 1) && (dz == 1))) ? 1 : 0); }
  // - Allow diagonals: (we will try this for a while)
  inline short isNeighbor(short dw, short dz) const
    { return ( ((dw <= 1) && (dz <= 1)) ? 1 : 0); }

  // Pads-to-cell conversion methods (From D. Silvermyr)
  inline short padWire(short z, short w) const
    { return (wireAdjust[(z + 999)%3] + ((w - wireAdjust[(z + 999)%3] + 1)/3)*3); };
  inline short padWireToPadx(short z, short w) const
    { return ((w - wireAdjust[(z + 999)%3])/3); };
};

#endif
