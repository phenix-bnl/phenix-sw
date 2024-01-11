#ifndef __MMPCEXUNPACKPISA_H__
#define __MMPCEXUNPACKPISA_H__

/**
 * @class  mMpcExUnpackPISA
 * @author ngrau@augie.edu
 * @date   July 2015
 * @brief  Module run during pisaToDST that takes the NCCPisaHit object and creates a TMpcExGeaHitContainer.
 */

#include "SubsysReco.h"
class PHCompositeNode;
class TFile; 
class TH2D; 

class mMpcExUnpackPISA : public SubsysReco {

 public:

  //! constructor
  mMpcExUnpackPISA();

  //! destructor
  virtual ~mMpcExUnpackPISA();

  //! unpacks the NCCPisaHit and fills a TMpcExGeaHitContainer
  int process_event(PHCompositeNode *topNode);

  int Init         (PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);

 private:

  int makeHisto;
  TFile *outputfile; 
  TH2D *_histo; 
  TH2D *_histoD; 

};

#endif /* __MMPCEXUNPACKPISA_H__ */
