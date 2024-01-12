#ifndef __SVXREACTIONPLANEMERGERECO_H__
#define __SVXREACTIONPLANEMERGERECO_H__

#include "Recalibrator.h"

#include <PHTimeServer.h>

class RpSumXYObject;
class ReactionPlaneCalibv1;

class SvxReactionPlaneMergeReco : public Recalibrator
{
  public:
    SvxReactionPlaneMergeReco();
    virtual ~SvxReactionPlaneMergeReco(){}
    
    //  Standard methods
    virtual int Init(PHCompositeNode *topNode);
    virtual int InitRun(PHCompositeNode *topNode);
    virtual int process_event(PHCompositeNode* topNode);
    virtual int isValidRun(const int runno) const;

  private:
    int  mergeRpSumXY(RpSumXYObject* sumxy, RpSumXYObject* cntsumxy);

    int  CreateNodeTree(PHCompositeNode* topNode);

  private:
    ReactionPlaneCalibv1* rpcalibv1;

    int m_RunNumber;
    int m_event;
    int m_skip;

    PHTimeServer::timer m_timer;   ///< Timer

  private:
    static int m_nmodule;
    int        m_nth;
};

#endif /* __SVXREACTIONPLANEMERGERECO_H__ */
