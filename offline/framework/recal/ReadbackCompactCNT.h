#ifndef __READBACKCOMPACTCNT_H__
#define __READBACKCOMPACTCNT_H__

#include "Recalibrator.h"
#include <string>

class RecoverTrackProjections;
class RecoverTrackLineProjections;
class RecoverTrackPathLengths;
class RecoverTrackHits;
class RecoverDchHits;
class RecoverPadHits;
class RecoverTofeHits;
class RecoverTofwHits;
class RecoverCrkHits;
class RecoverTecHits;
class RecoverAccHits;
class RecoverHbdHits;
class RecoverEmcHits;
class RecoverSvxHits;
class SvxCompactToDST;
class CreateCNT;
class FillCNT_TrackProjections;
class FillCNT_TrackPathLengths;
class FillCNT_TrackHits;
class FillCNT_DchHits;
class FillCNT_TofeHits;
class FillCNT_TofwHits;
class FillCNT_PadHits;
class FillCNT_CrkHits;
class FillCNT_TecHits;
class FillCNT_AccHits;
class FillCNT_EmcHits;
class FillCNT_EmcPc3;
class PHCompositeNode;

class ReadbackCompactCNT : public Recalibrator {

 public:
  ReadbackCompactCNT(const std::string& name="ReadbackCompactCNT");
  virtual ~ReadbackCompactCNT();

  int isValidRun(const int runno) const;

  int Init(PHCompositeNode *topNode); // called during intialization
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 private:
  RecoverTrackProjections *fRecoverTrackProjections;
  RecoverTrackLineProjections *fRecoverTrackLineProjections;
  RecoverTrackPathLengths *fRecoverTrackPathLengths;
  RecoverTrackHits *fRecoverTrackHits;
  RecoverDchHits *fRecoverDchHits;
  RecoverPadHits *fRecoverPadHits;
  RecoverTofeHits *fRecoverTofeHits;
  RecoverTofwHits *fRecoverTofwHits;
  RecoverCrkHits *fRecoverCrkHits;
  RecoverTecHits *fRecoverTecHits;
  RecoverAccHits *fRecoverAccHits;
  RecoverHbdHits *fRecoverHbdHits;
  RecoverEmcHits *fRecoverEmcHits;
  RecoverSvxHits *fRecoverSvxHits;
  SvxCompactToDST *fSvxCompactToDST;
  CreateCNT *fCreateCNT;
  FillCNT_TrackProjections *fFillTrackProjections;
  FillCNT_TrackPathLengths *fFillTrackPathLengths;
  FillCNT_TrackHits *fFillTrackHits;
  FillCNT_DchHits *fFillDchHits;
  FillCNT_TofeHits *fFillTofeHits;
  FillCNT_TofwHits *fFillTofwHits;
  FillCNT_PadHits *fFillPadHits;
  FillCNT_CrkHits *fFillCrkHits;
  FillCNT_TecHits *fFillTecHits;
  FillCNT_AccHits *fFillAccHits;
  FillCNT_EmcHits *fFillEmcHits;
  FillCNT_EmcPc3 *fFillEmcPc3;

};

#endif /* __READBACKCOMPACTCNT_H__ */
