#ifndef __SVXRPSUMXYRECO_H__
#define __SVXRPSUMXYRECO_H__

#include "SubsysReco.h"
#include "RpRecoConst.h"

class BbcCalib;
class BbcGeo;

/**
 * @brief  The reconstruction module of the VTX reaction plane 
 *
 * Created on 11/16/2011 by Takashi Hachiya.
 */



class RpSumXYObject;

class SvxRpSumXYReco : public SubsysReco
{
  public:
    //SvxRpSumXYReco(const std::string &name ="SVXRPSUMXYRECO", int flag=0);
    SvxRpSumXYReco(const std::string &name ="SVXRPSUMXYRECO", RpRecoConst::RPRECO flag=RpRecoConst::DEF);
    
    virtual ~SvxRpSumXYReco();
    int InitRun(PHCompositeNode *topNode);
    int process_event(PHCompositeNode *topNode);
    int End(PHCompositeNode *topNode);


    /**
     * Choose which vertex is used in the eta calculation
     *   Possible values:
     *   - 0: Choose most precise vertex available. (default)
     *   - 1: Use "SVX_PRECISE" : The vertex determined by the VTX
     *   - 2: Use "SVX" : The seed vertex determined by the VTX
     *   - 3: Use "BBC" : The vertex determined by the BBC's
     */
    virtual void setVertexFlag(const int flag=0) { m_vertexflag = flag; }

   
  protected:
    int CreateNodeTree(PHCompositeNode* topNode);  
   
    RpSumXYObject* m_rpsumxy;

  private:
    float getZVertex(PHCompositeNode *topNode);
   
  private:
    BbcCalib *m_bbccalib;
    BbcGeo   *m_bbcgeo;
    
    const static int nhar=6;
    const static int nfvt=40;
    
    int m_eventNumber;
    int calflag;
    int m_vertexflag;
};

#endif
