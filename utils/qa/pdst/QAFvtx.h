#ifndef _QAFVTX_H
#define _QAFVTX_H

/*
 * histMut.h
 * $Id: QAFvtx.h,v 1.3 2012/10/25 22:12:04 slash Exp $
 */

#include <TH2.h>
#include <TH3.h>
#include <FVTXOO.h>

#include "SubsysReco.h"

class PHCompositeNode;
class TFvtxTrk;

class QAFvtx: public SubsysReco
{
  public:
  
  //! constructor
  QAFvtx(const char *name = "QAFvtx"): 
    SubsysReco(name),
    _counter(0)
  {}
  
  //! destructor
  virtual ~QAFvtx() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  
  protected:
		 
  // fill hits related histograms
  void fill_fvtx_hits_hists( PHCompositeNode* top_node );

  // fill coordinates related histograms
  void fill_fvtx_coord_hists( PHCompositeNode* top_node );

  // fill tracks related histograms
  void fill_fvtx_tracks_hists( PHCompositeNode* top_node );

  void fill_fvtx_residuals_hist(TFvtxTrk* trk);

  private:
  
  int _counter;

  // hit histograms
  TH3F* FVTX_hit[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE]; // strip vs sector*2+column vs station
  TH3F* FVTX_hit_q[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE]; // strip vs sector*2+column vs station weighted by the hit charge
  
  //coordinate histograms
  TH3F* FVTX_coord[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE]; // strip vs sector*2+column vs station
  TH3F* FVTX_coord_size[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE]; // cluster size vs sector*2+column vs station
  TH3F* FVTX_coord_q[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE]; // total charge vs sector*2+column vs station
  TH3F* FVTX_coord_xy[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE]; // X vs Y vs station
  TH3F* FVTX_coord_dw[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE]; // track-hit distance vs sector vs station

  // track histograms
  TH3F* FVTX_trk_nhits_chi2[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE]; // nhits vs chi^2/ndf vs sector
  TH2F* FVTX_trk_phi_theta[FVTXOO::MAX_ARM];  // phi vs theta

};
#endif
