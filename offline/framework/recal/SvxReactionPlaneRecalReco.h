#ifndef __SVXREACTIONPLANERECALRECO_H__
#define __SVXREACTIONPLANERECALRECO_H__

#include "Recalibrator.h"

class RpSumXYObject;
class ReactionPlaneCalibv1;

class SvxSegmentList;

class SvxReactionPlaneRecalReco : public Recalibrator
{
  public:
    SvxReactionPlaneRecalReco();
    virtual ~SvxReactionPlaneRecalReco(){}
    
    //  Standard methods
    virtual int Init(PHCompositeNode *topNode);
    virtual int InitRun(PHCompositeNode *topNode);
    virtual int process_event(PHCompositeNode* topNode);
    virtual int isValidRun(const int runno) const;

    virtual void setCorrectForPro98(bool flag) { m_correctForPro98 = flag; }

  private:
    int  correctSvxPro98(RpSumXYObject* sumxy); // correction for cluster Qvector
    int  correctSegPro98(RpSumXYObject* sumxy); // correction for segment Qvector


    int  AddSvxRpSumXY( RpSumXYObject* sumxy );
    int  AddSegRpSumXY( RpSumXYObject* sumxy, SvxSegmentList *seglist=NULL );
    int  AddFvtxRpSumXY(RpSumXYObject* sumxy );

    bool doRecentering(const int detid, const int imul, const int iz, RpSumXYObject *sumxy);

    bool applyRecentering(const int detid, const unsigned int ikind, const unsigned int ihar, 
                          const int icent, const int iz, RpSumXYObject *sumxy);
   
  private:
    ReactionPlaneCalibv1* rpcalibv1;

    int m_RunNumber;
    int m_event;
    int m_skip;

    bool  m_correctForPro98;

  private:
    int        m_nth;
    static int m_nmodule;
};

#endif /* __SVXREACTIONPLANERECALRECO_H__ */
