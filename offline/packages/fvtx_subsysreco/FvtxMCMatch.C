
#include <FvtxMCMatch.h>

void
FvtxMCMatch::clear()
{
  event = -1;
  mc_track_id = -9999;
  idtrack = -9999;
  idparent = -9999;
  itparent = -9999;
  arm = -1;
  ptot_orig = -1.0;
  std::fill(vtx_orig,vtx_orig+3,0.0);
  nMCHits = 0;
  mcHitIds.clear();
  nCoords = 0;
  coordIds.clear();
  nstaHit = 0;
  phi_start = 0.0;
  phi_end = 0.0;
  mc_r_slope = 0.0;
  mc_r_offset = 0.0;
  nreco = 0;
  nghost = 0;

  reco_track_ids.clear();
  reco_phi_start.clear();
  reco_phi_end.clear();
  reco_r_slope.clear();
  reco_r_offset.clear();
  reco_r_chi2.clear();
  reco_r_slopeFit.clear();
  reco_r_offsetFit.clear();
  reco_r_chi2Fit.clear();
  reco_nshared.clear();
  reco_ghost.clear();

  return;
}

ClassImp(FvtxMCMatch);
