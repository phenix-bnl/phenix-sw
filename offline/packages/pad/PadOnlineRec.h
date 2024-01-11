#ifndef PADONLINEREC_H 
#define PADONLINEREC_H 

//---------------------------------------------------------------------------  
// Description: Header for PadOnlineRec class 
//---------------------------------------------------------------------------- 

#include <list>

namespace PadOnlineRecPar 
{
  // some helpful constants, per FEM or chamber-half
  const short NWIRES = 58;
  const short NCELLZ = 106; // along wires
  const short NPADX = 20; // across wires
  const short NPADZ = 108; // along z/wires
  const short NROWS = 5; // 5 rows in an FEM
  const short NROCS = 9; // 9 ROCs in a row
  const short NTGLS = 3; // 3 TGLs in a ROC
  const short NCHAN = 16; // 16 channels in a TGL
  const short MAX_PADS = 2160; // 108 * 20
  const short MAX_ROCS = NROWS*NROCS;
  
  const int NBUFF = 5; // length of event counter cycle

  // maximum number of clusters
  const short MAX_CLUSTERS = 500; 
  const short MAX_CLUSTERSIZE = 50;

}; // end of namespace

// from PadRec: PN
// Cell type
struct Cell { 
  short w, z; 
  Cell(short inW = 0, short inZ = 0) : w(inW), z(inZ) {}
};

// STL cell list iterator
typedef std::list<Cell>::iterator CI;

class Event;

/** 
    This is an OnlineRec PAD class. 
    @memo PAD OnlineRec Class 
*/ 
class PadOnlineRec {  
  
 public:
  
  // Constructor 
  PadOnlineRec(short ipacketid);  
  
  // Destructor
  virtual ~PadOnlineRec() { return; }
  
  // public member functions 
  virtual void init();
  
  // the main routine, where the unpacking of the data happens.. 
  virtual int processEvent(Event * evt);
  
  // dataword->pads->cells->"cluster" 
  void pads_to_cells();
  // help method from data words to cells
  void getCells(int oldpos, unsigned int nw);
  // takes all cells and reconstructs clusters
  int cells_to_clust();
  
  // some simple result feedback routines, for pads/channels 
  short getNumberFiredPads(); 
  short getFiredPad(short i);

  // cell info
  short getNumberFiredCells(); 
  // cell locations, local coord
  short getFiredCellInfo(int sizeArray, int *zArray, int *wireArray);
  // also local coord but on sector instead of FEM level
  short getFiredCellSectInfo(int sizeArray, int *zArray, int *wireArray);

  // a routine to print information about our FEM / packets
  void print(); 
  // reset routine for the main counters
  void reset_counters();
  // "reset the rest" routine, incl. arrays with accumulated/total info
  void reset_rest();

  // info about how many events we analyzed 
  int getNumberProcEvents() { return numberProcEvents; } 

  int getNumberOfTimesPadFired(short k); // accumulated since last full reset

  // set bad ROC routines
  short setBadROC(int grow, int gcol, int badtgltype[PadOnlineRecPar::NTGLS]);

  // calc. layer,arm,side, subsector once and for all: useful help indices
  void decodePacketid();

  // the object must be able to tell us who he is..
  short getPacketid() const { return packetid; }

  // reconstruction feedback, cluster info 
  short getNumberClusters() const { return numberOfClusters; }  
  short getClusterSize(short i) const 
    { return ( ((i>=0) && (i<numberOfClusters)) ? cluster[i].size : -1); }
  float getClusterSectZ(short i) const
    { return ( ((i>=0) && (i<numberOfClusters)) ? cluster[i].sectz : -1); }
  float getClusterSectWire(short i) const
    { return ( ((i>=0) && (i<numberOfClusters)) ? cluster[i].sectwire : -1); }
  
  // help routine for clustering
  bool cell_adjacent(short z1, short wire1, short z2, short wire2);  
  void doNotAllowDiagonalAdjCells() { diagonalAdj=-1; }
  void allowDiagonalAdjCells() { diagonalAdj=1; }

  // translation routines, used for info on sector level
  int getHVSector(int ip, int wire);
  int getHVSector(int ip, float wire);  
  int getSectWire(int wire);

  // *** methods for event address check
  int getEvtNR(void){return EvtNR;}
  int getnrCorrEvtNR(int for_evtnr){return nrCorrEvtNR[for_evtnr];}
  int getnrbadrocEvtNR(void){return nrbadrocEvtNR;}
  void setnrbadrocEvtNR(int value){nrbadrocEvtNR=value;}

  // TGL checking, used in calibrations
  void Fill_TGL_hits(Event *evt);
  int Get_TGL_hits(int grow, int gcol, int tgl);

 protected: 
 
  // our FEM must know who he is.. 
  short packetid;
  // derived from the packetid
  short layer,arm,side,subsector,hvsectorstart;
  short modsubsect,offset;

  // *** variables for the event address check
  // most occuring (possible) evtnr for this packet
  int EvtNR;
  // the number of occurances of evtnrs
  int nrCorrEvtNR[PadOnlineRecPar::NBUFF];
  // badroc status of all rocs (1=bad,0=good)
  int badrocEvtNR[PadOnlineRecPar::MAX_ROCS];
  // number of bad rocs
  int nrbadrocEvtNR;

  // necessary to transform indices? PC2 has some scrambled channels..
  short pc2check;

  // some raw numbers: safe to look at (hard to get wrong) 
  short numberOfFiredPads;
  short numberOfFiredCells;

  // Status of the FEM (-1: not ready to take data, 0: ready 1: has read) 
  short iFlag; 
 
  // counter for the number of events we processed
  int numberProcEvents;

  // help variable for cell reconstruction
  int padpossdiff[3][3][2]; // indices are zmod3,poss,0/1 diff 
  // 0 is the diff between the 1st and 2nd
  // 1 is the diff between the 2nd and 3rd

  // pad/channel counters
  unsigned int padK[PadOnlineRecPar::MAX_PADS]; // zero suppressed, this event only
  unsigned int fired_pad[PadOnlineRecPar::MAX_PADS]; // sum over all events since last reset

  // container for cells in current event
  std::list<Cell> cells;

  // info on clusters in the current event
  short numberOfClusters;
  typedef struct 
  { 
    // where?
    float sectz; 
    float sectwire; 
    // how big?
    short size; 
  } clust; 
  clust cluster[PadOnlineRecPar::MAX_CLUSTERS];

  // to select whether we allow diagonal adjacent cells or not 
  short diagonalAdj;

  // for unpacking; data-words in the current event
  // 20-bit word info
  int padsRaw[PadOnlineRecPar::NPADZ]; // raw info
  int mask[PadOnlineRecPar::NPADZ];   // mask array
  int padsOn[PadOnlineRecPar::NPADZ]; // result from raw & mask
  int padsLocal[PadOnlineRecPar::NPADZ]; // tmp storage of padsOn info in cell rec.

  // counter for total hits on TGL, since last reset
  int tgl_hits[PadOnlineRecPar::NROWS][PadOnlineRecPar::NROCS][PadOnlineRecPar::NTGLS];

};  
 
#endif 
