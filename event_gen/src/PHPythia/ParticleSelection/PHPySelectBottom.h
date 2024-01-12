#ifndef __PHPYSELECTBOTTOM_H__
#define __PHPYSELECTBOTTOM_H__

#include "SubsysReco.h"

/**

Example Reco Module for Selecting only certain particles
to write out on PYTHIA events.

We recommend that you derive your trigger from this class, and simply
override the ParticleCut() method.  

*/

class PHCompositeNode;
class PHPythiaHeader;
class PHPythiaContainer;
class TMCParticle;

class PHPySelectBottom: public SubsysReco
{
 public:
  PHPySelectBottom(const std::string &name = "PHPySelectBottom");
  virtual ~PHPySelectBottom();

  // Override this method to make your particle selection
  virtual int ParticleCut( TMCParticle *part );
  
  // Methods Derived from SubsysReco
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  
 protected:
  PHPythiaContainer *phpythia;
    
 private:
  int m_bottom_id;
};

#endif	/* __PHPYSELECTBOTTOM_H__ */
