
#ifndef __FvtxMCMatch_H__
#define __FvtxMCMatch_H__

#include <vector>
#include <TObject.h>

// A ROOT-ized object for filling rows of a match-eval
// TTree.

class FvtxMCMatch : public TObject 
{
public:
  FvtxMCMatch() {}
  virtual ~FvtxMCMatch() {}

  void clear();

  //private:

  int event;
  int mc_track_id;
  int idtrack;                          // PID of the MC track
  int idparent;                         // PID of the MC track's parent
  int itparent;                         // track id of the MC track's parent
  int arm;                              // Arm where this track exits
  double ptot_orig;                     // total MC momentum
  double vtx_orig[3];                   // Position of MC vertex
  int nMCHits;                          // Number of FvtxMCHits generated by track
  std::vector<int> mcHitIds;            // Ids of FvtxMCHits generated by this mc track
  int nCoords;                          // Number of FvtxCoords associated with track
  std::vector<int> coordIds;            // Ids of FvtxCoords associated with track
  int nstaHit;                          // Number of stations hit by the MC track
  double phi_start;
  double phi_end;
  double mc_r_slope;
  double mc_r_offset;
  int nreco;                            // Number of reco tracks associated with this track
  int nghost;                           // Number of associated tracks that are tagged as ghosts
  std::vector<int> reco_track_ids;      // List of reco track ids associated with this track
  std::vector<double> reco_phi_start;   // phi start of first hit
  std::vector<double> reco_phi_end;     // phi end of first hit
  std::vector<double> reco_r_slope;     // Road slope
  std::vector<double> reco_r_offset;    // Road offset
  std::vector<double> reco_r_chi2;      // chi2 from road
  std::vector<double> reco_r_slopeFit;  // Fitted slope
  std::vector<double> reco_r_offsetFit; // Fitted offset
  std::vector<double> reco_r_chi2Fit;   // Chi2 from fit
  std::vector<int> reco_nshared;        // Number of shared hits this reco track has
  std::vector<int> reco_ghost;          // ghost flag for each reco track

  ClassDef(FvtxMCMatch,1);
};

#endif