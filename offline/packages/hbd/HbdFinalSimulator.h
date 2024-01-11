#ifndef __HBDFINALSIMULATOR_H__
# define __HBDFINALSIMULATOR_H__
# include "SubsysReco.h"

#include "HbdFinalSimSupport.h"
#include "hbdDetectorGeo.hh"

class PHCompositeNode;
class HbdGhitList    ;
class HbdBlobList ;
class HbdCellList ;
class HbdHitList ;
class TFile;
class TH1;
//
// Global definitions
//

# define HBDMAXGTRACKS   60000  // all tracks in the event
# define HBDMAXGHITS     50000 // all hits in the event
# define HBDMAXGTRACKHITS  1000 // max hits per one track

# define NY              72   // eight sectors
# define NY1             9     // N cells along Y
# define NZ              19    // N cells along Z (not independent of Y)
# define halfZ           14.5
# define halfY           9

#define NSECT            12     
#define NPADS            192 

# define HBDMAXGCONTRIB      10     //
# define HBDMAXGCLUST        300
# define HBDMAXGCLUSTLENGTH  100
# define HBDMAXGSPLIT        10

class HbdFinalSimulator : public SubsysReco
{

  public:
  
  HbdFinalSimulator();
  virtual ~HbdFinalSimulator() {}
  
   
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode) {return 0;}
  int Reset(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  void Print(const std::string& what="") const {}

  // Analysis methods
  int fillGhitList(PHCompositeNode *topNode);          // Fill GEANT hits from the PISA tree
  int PulseSimAndfillCell();  // Pulse Simulator and filling HbdCell
  
  protected:

  //---------------------------------------
  /*! 
  use direct reference and not pointer so that default constructor
  is called in parent object constructor, and deletion is automatically
  performed in parent object destructor
  */
  //HbdFinalSimSupport simsupport;
  hbdDetectorGeo hbdgeo;
  //double * pointer;
  double z_loc,y_loc;
  short sect_loc,side_loc;

  //---------------------------------------

  // Data nodes
  HbdGhitList *d_ghit;
  HbdBlobList *d_blob;
  HbdCellList *d_cell;
  HbdHitList  *d_hit;
  
  // Utility to find the svx Nodes.
  int CreateNodeTree(PHCompositeNode *topNode);
  void GetNodes(PHCompositeNode *topNode);
  
  // Internal global variables
  int pisaNumEntries;  // Total number of ntuple entries
  int pisaEntryIndex;  // Ntuple entry being read in
  char *pisaInFile;    // Name of the input PISA ROOT file
  TFile *pisaInTFile;  // Pointer to the input PISA ROOT file
  
  // PISA AncHbd ntuple variables
  int PISAeventSeq;
  int PISAnHits;
  double PISAz0Event,PISAbImpact;
  int nGTracks;
  int gTrackList[HBDMAXGTRACKS];
  
  // Counters
  int nHbdGhits;      // Number of HbdGhit objects filled
  int nHbdBlobs;      // Number of HbdBlob objects filled
  int nHbdCells;      // Number of HbdCell objects filled
  int nHbdHits;       // Number of HbdHit objects filled
  
  // Arrays
  short TRK_ACCEPT[HBDMAXGTRACKS];
  short TRK_SRT[7][HBDMAXGTRACKS];
  int   TRK_INT[2][HBDMAXGTRACKS];
  double TRK_FLT[8][HBDMAXGTRACKS];
  
  int   HIT_SRT[5][HBDMAXGHITS];
  double HIT_FLT_C[3][HBDMAXGHITS];
  double HIT_FLT_P[4][HBDMAXGHITS];
  double HIT_FLT_T[3][HBDMAXGHITS];
  
  int   HITORDER[HBDMAXGTRACKHITS];
  int   NOHITORDER[HBDMAXGTRACKHITS];
  int   PHOTONORDER[HBDMAXGTRACKHITS];
  
  double contrib_profile[HBDMAXGCONTRIB][200];
  double contrib_shaped_profile[HBDMAXGCONTRIB][200];
  double sum_shaped_profile[2][200];
  
  short contrib_trail[HBDMAXGCONTRIB][200];
  
  short cell_trail[NSECT][NPADS][300];
  short cell_end[NSECT][NPADS];
  
  double cell_contrib_ampl[NSECT][NPADS][4];
  short cell_contrib_index[NSECT][NPADS][3];
  short cell_time[NSECT][NPADS];
  
  short cell_fire[NSECT][NPADS];
  
  double cluster_amplitude[HBDMAXGCLUST][HBDMAXGCONTRIB];
  double sum_cluster_amplitude[HBDMAXGCLUST];
  double cluster_time[HBDMAXGCLUST];
  double cluster_position[HBDMAXGCLUST][2];
  double cluster_position_unf_cog[HBDMAXGCLUST][2];
  
  double cluster_position_cog[HBDMAXGCLUST][3];
  double cluster_rms[HBDMAXGCLUST][2];
  short cluster_index[HBDMAXGCLUST][HBDMAXGCONTRIB];
  
  short clust[HBDMAXGCLUST][HBDMAXGCLUSTLENGTH][2];
  short cluster_size[HBDMAXGCLUST];
  double clust_charge[HBDMAXGCLUST][HBDMAXGCLUSTLENGTH];
  
  double cluster_contrib_ampl [HBDMAXGCLUSTLENGTH];
  short cluster_contrib_index[HBDMAXGCLUSTLENGTH];
  short max_trail[HBDMAXGSPLIT],end_trail[HBDMAXGSPLIT];
  
  int   hit_int[HBDMAXGHITS][9];
  double hit_float[HBDMAXGHITS][27];
  
  double shaper[300];                               // shaper response function immitation.
  
  int Verbose;
  
  // Histograms
  TH1 *shaper_f;     // Shaper function
  TH1 *sgn1;
};

#endif /* __HBDFINALSIMULATOR_H__ */

