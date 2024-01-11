#ifndef __MMPCEXDIGITIZEHITS_H__
#define __MMPCEXDIGITIZEHITS_H__

/**
 * @class  mMpcExDigitizeHits
 * @author ngrau@augie.edu
 * @date   July 2015
 * @brief  Uses the measured minipad-by-minipad pedestals and Gaussian widths to create a realistic digitized signal including noise from an input set of GEANT minipad energies. The incoming GEANT hit enerties are digitized assuming 150 keV for most probable landau energy loss.
 */

#include <SubsysReco.h>
class PHCompositeNode;
class TRandom3;
class TFile; 
class TH2D; 

class mMpcExDigitizeHits : public SubsysReco {

 public:

  //! constructor -- with the number of below above the 
  //! pedestal to suppress, which emulates the DCM zero
  //! suppression
  mMpcExDigitizeHits();

  //! destructor
  virtual ~mMpcExDigitizeHits();

  //! Applies known pedestal, width, and mip to each
  //! channel to digitize PISA energy into an ADC count
  int process_event(PHCompositeNode *topNode);

  int Init         (PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);

 private:

  //! Calculates the integer digitized channel for an input 
  //! energy including the pedestal mean position and width 
  //! and the mip position assuming the mip is 150 keV.
  float make_adc(float mip, float mean, float width, float energy, float *esat);

  //! random number generator
  TRandom3 *_dice;

  int makeHisto;
  TFile *outputfile; 
  TH2D *_histo_low; 
  TH2D *_histo_high; 
  TH2D *_histo_low_dcm; 
  TH2D *_histo_high_dcm; 


};

#endif /* __MMPCEXDIGITIZEHITS_H__ */
