#ifndef __PHPYSELECTEFROMOHF_H__
#define __PHPYSELECTEFROMOHF_H__

#include "SubsysReco.h"

/**

Example Reco Module for Selecting only certain particles
to write out on PYTHIA events.

We recommend that you derive your trigger from this class, and simply
override the ParticleCut() method.

D. McGlinchey - 1/22/2014
This class is meant to select electrons from open heavy flavor (bottom/charm) hadrons.
Only the electron and it's direct parents are saved.

*/

class PHCompositeNode;
class PHPythiaHeader;
class PHPythiaContainer;
class TMCParticle;
class TRandom3;

class PHPySelectEfromOHF: public SubsysReco
{
 public:
  PHPySelectEfromOHF(const std::string &name = "PHPySelectEfromOHF");
  virtual ~PHPySelectEfromOHF();

  // Override this method to make your particle selection
  virtual int ParticleCut( TMCParticle *part );

  // Methods Derived from SubsysReco
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  void set_zvtx_width(float zwidth) {zvtx_width = zwidth;}; //in cm

 protected:
  PHPythiaContainer *phpythia;
  PHPythiaHeader *phpythiaheader;

  TRandom3* rnd;
  float zvtx_width;

};

#endif	/* __PHPYSELECTEFROMOHF_H__ */
