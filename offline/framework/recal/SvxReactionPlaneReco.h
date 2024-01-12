#ifndef __SVXREACTIONPLANERECO_H__
#define __SVXREACTIONPLANERECO_H__

#include "Recalibrator.h"

class RpSumXYObject;
class ReactionPlaneObject;

class ReactionPlaneCalibv1;


class SvxReactionPlaneReco : public Recalibrator
{
  public:
    SvxReactionPlaneReco();
    virtual ~SvxReactionPlaneReco();
    
    virtual int  Init(PHCompositeNode* topNode);
    virtual int  InitRun(PHCompositeNode* topNode);
    virtual int  process_event(PHCompositeNode* topNode);

    virtual int  isValidRun(const int runno) const;
  
  protected:
    virtual bool CreateNodeTree(PHCompositeNode* topNode);

    virtual bool doFlattening(int detid, int imul, int iz, RpSumXYObject *sumxy);

    virtual bool applyFlattening(int detid, int ikind, int ihar, int imul, int izps, RpSumXYObject *sumxy);

  
  
  private:
    ReactionPlaneObject*  d_rp;
    ReactionPlaneCalibv1* rpcalibv1;
    
    int m_RunNumber;
    int m_event;
    int m_skip;

  private:
    int         m_nth;
    static int  m_nmodule;
};

#endif
