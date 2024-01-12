#ifndef _HISTMUT_H
#define _HISTMUT_H

/*
 * histMut.h
 * $Id: QAMut.h,v 1.7 2013/03/02 16:27:50 imazu Exp $
 */

#include <TH1.h>
#include <TH2.h>
#include <TProfile.h>
#include <TNtuple.h>

#include "SubsysReco.h"

class PHCompositeNode;
class TMutHit;

class QAMut: public SubsysReco
{
  public:
  
  //! constructor
  QAMut(const char *name = "QAMut"): 
    SubsysReco(name),
    _counter(0),
    _run_number(0)
  {}
  
  //! destructor
  virtual ~QAMut() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  
  enum {
    NUMARM = 2,
    NUMSTATION = 3,
    NUMOCTANT = 8,
    NUMHALFOCT = 2,
    NUMGAP = 3,
    MAXNUMPLANE = 6,
    NUMCATH = 2,
    MINCLUSWID = 0,
    MAXCLUSWID = 25,
    MAXNUMPACKET = 192,
    MAXNUMSTRIPS = 160,
    NUMSAMPLES = 4,
    HISTMAXFITNSTRIPS = 160,
    HISTMAXFITCHARGE = 350,
    HISTMAXPEAKCHARGE = 200,
    HISTMAXWIDTH = 20,
    HISTMAXNTRACKS = 30
  };
  
  protected:
		 
  //! fills histograms at cluster level
  void fill_mut_clusters_hists( PHCompositeNode* top_node );
  
  //! fills histograms at track level
  void fill_mut_tracks_hists( PHCompositeNode* top_node );
  
  //! fills histograms at strip/packet level
  void fill_mut_packets_hists( PHCompositeNode* top_node );
  
  //! fill plane level histograms
  void fill_mut_plane_hists( PHCompositeNode *top_node );
  
  //! fills vtx histograms
  void fill_vtx_hists( PHCompositeNode* top_node );

  private:
  
  int _counter;
  int _run_number;

  // constants for booking of histogram
  TH1F *MutNumCathClustersSt[NUMARM][NUMSTATION];
  TH1F *MutCathClustQPeakSt[NUMARM][NUMSTATION][NUMOCTANT][NUMGAP];
  TProfile *MutHitsPerPlaneSt[NUMARM][NUMSTATION];
  TProfile *MutHitsPerPacketArm[NUMARM];
  TProfile *MutAmuErrorPerPacketArm[NUMARM];
  //TH1F* MutTrkChisq[NUMARM];
  TH1F* MutTrkChisqNdf[NUMARM];
  TH1F* MutClusChisq[NUMARM];
  TH1F* VtxChisqNdf[NUMARM];
  
  // Coordinate for radiograph
  TH2F *MutHitCoord[2][3][3];
  TH1F *MutCathClustWidth[NUMARM][NUMSTATION];
  TH1F *MutCathClustQPeak[NUMARM];
  TH1F *MutNumTracks[NUMARM];
  TH1F *MutTrackMom[NUMARM];
  TH1F *MutAdcPulses[NUMARM][NUMSTATION][NUMOCTANT][NUMGAP];

  // for plane-by-plane QA 
  TH1F *MutQTotPlane[NUMARM][NUMSTATION][NUMOCTANT][NUMGAP][NUMCATH];
  TH1F *MutPkClszPlane[NUMARM][NUMSTATION][NUMOCTANT][NUMGAP][NUMCATH];
  TH1F *MutPkWPlane[NUMARM][NUMSTATION][NUMOCTANT][NUMGAP][NUMCATH];
  TH1F *MutQSubQPkPlane[NUMARM][NUMSTATION][NUMOCTANT][NUMGAP][NUMCATH];
  TH1F *MutQPk20QPk2Plane[NUMARM][NUMSTATION][NUMOCTANT][NUMGAP][NUMCATH];
  TH1F *MutTimePkPlane[NUMARM][NUMSTATION][NUMOCTANT][NUMGAP][NUMCATH];
  TH1F *MutQ20Q2Plane[NUMARM][NUMSTATION][NUMOCTANT][NUMGAP][NUMCATH];
  TH1F *MutClszPlane[NUMARM][NUMSTATION][NUMOCTANT][NUMGAP][NUMCATH];
  TH1F *MutWPlane[NUMARM][NUMSTATION][NUMOCTANT][NUMGAP][NUMCATH];

  double get_calibed( TMutHit* mut_hit, const int& i );
  static Double_t pol2fit( Double_t* x, Double_t *par );
  std::vector< double > get_fitpar( TMutHit* mut_hit );

/*  #ifdef DEBUG
  TNtuple *MutHotStrips;
  TNtuple *MutGoodStrips;
  TNtuple *MutCathClustQFitNtuple;
  #endif*/
  
  
};
#endif
