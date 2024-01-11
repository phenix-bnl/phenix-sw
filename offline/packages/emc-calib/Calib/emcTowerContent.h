#ifndef __EMCTOWERCONTENT_H__
#define __EMCTOWERCONTENT_H__

#include "PHObject.h"
#include <iostream>

/** (ABC) EMCAL tower raw and/or calibrated data.

You generally obtain such object from an emcTowerContainer.
Then you can access tower characteristics, being raw ones (the 5 DC samples), ped-subtracted ones (ADC and TDC), or calibrated ones (Energy,ToF).

\note
Actual implementation may not store all the information. Please use the hasXXX() methods to check this (e.g. hasRaw(), hasDC(), hasCalib()).

@ingroup calibration
@ingroup interface
@ingroup clustering
 */

class emcTowerContent : public PHObject 
{
public:

  virtual ~emcTowerContent();

  /// Make a copy of this tower.
  virtual emcTowerContent* clone() const { warning("clone"); return 0; }

  /// Make an empty copy of this tower (i.e. copy only the type).
  virtual emcTowerContent* create() const { warning("create") ; return 0; }

  /** ADC value (i.e. pre-post-pedestals for the selected gain, 
      either low or high).
   */
  virtual int ADC(void) const { warning("ADC"); return 0; }

  /** AMU cell that gave the pre sample [0-63]
   */
  virtual int AMUPre(void) const { warning("AMUPre"); return 0; }

  /** AMU cell that gave the post sample [0-63]
   */
  virtual int AMUPost(void) const { warning("AMUPost"); return 0; }

  /** AMU cell that gave the tac sample [0-63]
   */
  virtual int AMUTAC(void) const { warning("AMUTAC"); return 0; }

  ///
  virtual int BeamClock(void) const { warning("BeamClock"); return 0; }

  /// Tower channel number within the FEM [0-143]
  virtual int Channel(void) const { warning("Channel"); return 0; }

  /// A word indicating some error (e.g. sample too low or too high).
  virtual int DataError(void) const { warning("DataError"); return 0; }
  
  /// Tower energy in GeV.
  virtual float Energy(void) const { warning("Energy"); return 0; }

  /** A word indicating the status of the tower itself and of its
      neighbours (5x5). See also emcBadModules class for more
      info.
  */
  virtual unsigned int ErrorNeighbours(void) const 
  { warning("ErrorNeighbours"); return 0; }

  /// FEM number of this tower [0-171 for data, >= 172 for monitoring.
  virtual int FEM(void) const { warning("FEM"); return 0; }

  /** @name Actual features.
      Test if this tower actually has some feature.
  */
  //@{
  /// Whether this tower has Energy and ToF available.
  virtual bool hasCalib(void) const { return false; }

  /// Whether this tower has ADC and TDC values available.
  virtual bool hasDC(void) const { return false; }

  /** Whether this tower has the gain (which was used to normalize
      the ADC value to take into account time evolution) available.
  */  
  virtual bool hasGain(void) const { return false; }

  /** Whether we can set the merged/simulated status
   */
  virtual bool hasMerSimStatus(void) const { return false; }

  /// Whether this tower has raw samples availalble.
  virtual bool hasRaw(void) const { return false; }

  /** Tells whether this tower has a reference (from monitoring
      system information available.
  */
  virtual bool hasReference(void) const;
  //@}

  /** @name Possible features.
      Test if this tower *might* have some feature.
      (to test if the tower actually has it, use the hasXXX
      methods).
  */
  //@{
  
  /// Whether this tower can hold calibrated (energy,tof) values.
  virtual bool canHaveCalib() const { return false; }

  /// Whether this tower can hold DC (ADC,TDC) values.
  virtual bool canHaveDC() const { return false; }

  /// Whether this tower can hold Gain value.
  virtual bool canHaveGain() const { return false; }

  /// Whether this tower can have the Merged/Simulated status.
  virtual bool canHaveMerSimStatus() const { return false; }

  /** Whether this tower can hold raw (HGPre,HGPost,LGPre,LGPost,TAC)
      samples.
  */
  virtual bool canHaveRaw() const { return false; }
  //@}

  /// High gain value (pedestal subtracted).
  virtual int HG(void) const { warning("HG"); return 0; }

  /// High gain post sample (raw).
  virtual int HGPost(void) const { warning("HGPost"); return 0; }

  /// High gain pre sample (raw).
  virtual int HGPre(void) const { warning("HGPre"); return 0; }

  /// HGPre()-HGPost().
  virtual int HGPP(void) const;

  /// Gain for this tower.
  virtual float Gain(void) const { warning("Gain"); return 0; }

  /// From PHObject.
  virtual void identify(std::ostream& os=std::cout) const;

  /// Whether this tower is a simulated one.
  virtual bool isSimulated(void) const 
  { warning("isSimulated"); return false; }

  /// Whether this tower was merged (i.e. embedded).
  virtual bool isMerged(void) const { warning("isMerged"); return false; }

  /// Whether this tower is a reference one.
  virtual bool isReference(void) const;

  /// From PHObject.
  virtual int isValid() const;

  /// Whether raw samples are compatible with an empty tower.
  virtual bool isZero(void) const { warning("isZero"); return 0; }

  /// Low gain (pedestal subtracted) value.
  virtual int LG(void) const { warning("LG"); return 0; }

  /// Low gain post sample.
  virtual int LGPost(void) const { warning("LGPost"); return 0; }

  /// Low gain pre sample.
  virtual int LGPre(void) const { warning("LGPre"); return 0; }

  /// LGPre()-LGPost()
  virtual int LGPP(void) const;

  /// Output to stream.
  virtual void print(std::ostream& /*out*/=std::cout, int /*level*/=0) const 
  { warning("print"); }

  /// Pointer to reference (if available, see hasReference()).
  virtual emcTowerContent* Reference(void) const;

  /** Set the "DC" values : ADC and TDC.
      Only useable if hasDC returns true.
  */
  virtual void SetADCTDC(int /*adc*/, int /*tdc*/, int /*hg*/=0, int /*lg*/=0) 
  { warning("SetADCTDC"); }

  /** Set the calibrated values : energy and tof.
      Only useable if hasCalib returns true.
  */
  virtual void SetCalibrated(float /*energy*/, float /*tof*/) 
  { warning("SetCalibrated"); }

  /// Set the data Error.
  virtual void SetDataError(int /*dataerror*/) { warning("SetDataError"); }

  /// Set the id of this tower, by means of the pair (fem,channel).
  virtual void SetID(int /*fem*/, int /*channel*/) { warning("SetID"); }

  /// Set the gain.
  virtual void SetGain(float /*gain*/) { warning("SetGain"); }

  /// Set the deadmap and warnmap.
  virtual void SetNeighbours(unsigned int /*_error*/, 
			     unsigned int /*_warning*/) 
  { warning("SetNeighbours"); }

  /** Set the merged and sim status. 
      Only useable if isMerSimStatus returns true.
  */
  virtual void SetMerSimStatus(bool /*ismerged*/, bool /*issimulated*/)
  { warning("SetMerSimStatus"); }

  /** Set the raw sample values.
      Only useable if hasRaw returns true.
  */
  virtual void SetRaw(int /*hgpost*/, int /*hgpre*/, 
		      int /*lgpost*/, int /*lgpre*/,
		      int /*tac*/,
		      int /*amupre*/=0,
		      int /*amupost*/=0,
		      int /*amutac*/=0,
		      int /*beamclock*/=0) { warning("SetRaw"); }

  /// Set the simulated energy fraction.
  virtual void SetSimFrac(float /*simfrac*/) { warning("SetSimFrac"); }

  /// Set the (corrected) tof.
  virtual void SetToF(float /*tof*/) { warning("SetTOF"); }

  /** Fraction of simulated energy (0=real tower, 1=simulated, in between
      for a merged, i.e. embedded, tower. */
  virtual float SimFrac() const { return 0; }

  /// TAC sample value.
  virtual int TAC(void) const { warning("TAC"); return 0; }

  /// TDC value.
  virtual int TDC(void) const { warning("TDC"); return 0; }

  /// Get the ToF value (ns).
  virtual float ToF(void) const { warning("TOF"); return 0; }

  /// Uncorrected ToF value (ns)
  virtual float UncorrectedToF() const { warning("UncorrectedToF"); return 0; }

  /// TowerID (computed from (fem,channel) pair).
  virtual int TowerID(void) const { warning("TowerID"); return 0; }    

  /// Alias for TowerID.
  int TowerId(void) const { return TowerID(); }

  /// Another alias for TowerID.
  int towerid(void) const { return TowerID(); }

  /// Warnmap (see also ErrorNeighbours()).
  virtual unsigned int WarnNeighbours(void) const 
  { warning("WarnNeighbours"); return 0; }  

  /// Reset the raw samples.
  virtual void Zero(void) { warning("Zero"); }

  void ShutUp(const int i = 1);

protected:
  /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
  emcTowerContent& operator=(const emcTowerContent& rhs);

private:

  void warning(const char*) const;

  ClassDef(emcTowerContent,0) // EMCAL Tower data (ABC)

};

inline 
std::ostream& operator<<(std::ostream& out, const emcTowerContent& tc)
{
  tc.print(out); 
  return out;
}

#endif
