#ifndef __SVXVERTEXCOMBINER_H__
#define __SVXVERTEXCOMBINER_H__

#include <phool.h>
#include <SubsysReco.h>
#include <PHTimeServer.h>
#include <vector>

class VtxOut;



class SvxVertexCombiner : public SubsysReco
{

public :

  /**
   * Basic constructor. Set hard-coded values for some member variables.
   *
   * @todo Explain hard-coded numbers in constructor. cut0 are cuts made on the first iteration. cut (w/o 0) are
   *       made on subsquent iterations. I think these form a search window?
   */
  SvxVertexCombiner(const std::string &name = "SVXVERTEXCOMBINER");


  /**
   * Basic destructor. Delete SvxVertexTrack objects.
   */
  virtual ~SvxVertexCombiner();


  /**
   * No functionality.
   */
  int Init(PHCompositeNode *topNode)    { return 0; }


  /**
   * 
   */
  int InitRun(PHCompositeNode *topNode);


  /**
   * Do all the work for each event.
   */
  int process_event(PHCompositeNode *topNode);



  /**
   * No functionality.
   */
  int End(PHCompositeNode *topNode)     { return 0; }

  /**
   * Define the sigma of the beam spot independently for x and y 
   */

  void setBeamSpotSize(double sigx, double sigy) { m_beamspotsizex = sigx; m_beamspotsizey = sigy;}

 protected:
  double m_beamspotsizex;
  double m_beamspotsizey;





};

#endif
