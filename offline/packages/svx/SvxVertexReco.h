#ifndef __SVXVERTEXRECO_H__
#define __SVXVERTEXRECO_H__

#include <SubsysReco.h>
#include "SvxRecoTracks.h"
#include "SvxTracker.h"
#include "TH3I.h"
#include <vector>


class SvxSegmentList;

/**
 * @brief  [NEED A BETTER EXPLANATION]  A SubsysReco module to reconstruct vertex
 */
class SvxVertexReco : public SubsysReco
{
  public:
    SvxVertexReco();
    ~SvxVertexReco(){}
    
    int Init(PHCompositeNode *topNode){return 0;}
    int InitRun(PHCompositeNode *topNode){return 0;}
    int process_event(PHCompositeNode *topNode);
    int End(PHCompositeNode *topNode){return 0;}
    
    void setVerbosity(int verb);
    
  private:
    
    bool inBoundary(float center, float halfwidth, float pos);
    void findMaxBin(TH3I &hist);
    void findCell(TH1I &hx, TH1I &hy, TH1I &hz);
    void intersectCube(float centerx, float centery, float centerz, float halfwidthx, float halfwidthy, float halfwidthz);
    void intersectCubeFromSide(float centerx, float centery, float centerz, float halfwidthx, float halfwidthy, float halfwidthz);
    bool stepCell(TH3I &hist);
    std::vector<float> getBinCenter3D(TH1I &hx, TH1I &hy, TH1I &hz, int binx, int biny, int binz);
    
    float totalProbReco2(float *ver);
    
    void sortHighMom();
    
    TH3I cube;
    TH1I cubex, cubey, cubez;
    float vertex[3];
    float ex,ey,ez,x0,y0,z0;
    int binx,biny,binz;
    float isectx,isecty,isectz;
    int iplane;
    bool intersected;
    
    SvxRecoTracks reco;
    SvxTracker trkr;
    
    SvxSegmentList *d_segment;
    
    std::vector<int> highmomlist;
    
    unsigned int maxtracks;
    
    int verbosity;
    
    std::vector<float> probvec;
    
};

inline void SvxVertexReco::setVerbosity(int verb)
{
  verbosity=verb;
}

#endif
