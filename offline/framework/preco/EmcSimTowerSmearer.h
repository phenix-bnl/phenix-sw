#ifndef __EMCSIMTOWERSMEARER_H__
#define __EMCSIMTOWERSMEARER_H__

#include "SubsysReco.h"

#include <string>

class TF1;

/** Smear energies of the simulated EMCAL towers.
 */

class EmcSimTowerSmearer : public SubsysReco
{
 public:

  /** @param simuTopNode is the top node under which the
      emcTowerContainer might be found.
      @param percentageOfSmearing is the sigma of the gaussian
      distribution of the constant term smearing we apply.
      @param iseed might control the internal random generator
      used. zero means initialize it properly (i.e. different
      at each call). Non zero should only be used if you want
      to test it several times with the same random numbers).
   */
  EmcSimTowerSmearer(const char* simuTopNode="TOP",
		     double percentageOfSmearing=0.058,
		     unsigned int iseed=0);
  
  virtual ~EmcSimTowerSmearer();

  /** Initialize the random number generator and the smearing
      function used.
  */
  int Init(PHCompositeNode*);

  /** Apply the smearing to all the towers in the emcTowerContainer
      object to be found under fSimuTopNode.
  */
  int process_event(PHCompositeNode*);

 private:
  std::string fSimuTopNode;
  TF1* fEnergyResolutionConstantTerm;
  double fConstantTerm;
  unsigned int fIseed;
};

#endif
