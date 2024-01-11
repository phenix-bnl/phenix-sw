#ifndef __EMCCALIBRATIONDATAHELPER_H__
#define __EMCCALIBRATIONDATAHELPER_H__

#include <map>
#include <set>
#include <string>
#include <vector>

#ifndef __PHTIMESTAMP_H__
#include "PHTimeStamp.h"
#endif
#ifndef __EMCMANAGEABLE_H__
#include "emcManageable.h"
#endif

class emcCalFEM;
class emcCalibrationData;
class emcFEMList;

/** Helper class to give access to various calibration data.
 *
 *  This class is the key one to be used if you ever want to directly
 *  access (read) emcal calibration data (e.g. from the ROOT prompt).
 *
\code
gSystem->Load("libPgCalInstance.so");
gSystem->Load("libemcOMpg.so");
int runnumber = 80312;
emcCalibrationDataHelper cdh(runnumber); // this may take a little while...
emcCalFEM* gain_for_W0 = cdh.getCalibration(0,"Gains");
gain_for_W0->Print();
\endcode
 * @ingroup calibration
 * @ingroup helper
 */

class emcCalibrationDataHelper
{
public:
  /** The initall flag = true is MANDATORY if you use Objy as a source
      problem. */
  emcCalibrationDataHelper(int runnumber, bool initall=true,
			   emcManageable::EStorage source=emcManageable::kDB_Pg,
			   const char* emcalparts="emcal");

  /** This is the recommended way of using it : if you already know
      both the runnumber and the timestamp, please provide both !
  */
  emcCalibrationDataHelper(int runnumber, const PHTimeStamp& ts,
			   bool initall=true,
			   emcManageable::EStorage source=emcManageable::kDB_Pg,
			   const char* emcalparts="emcal");

  ~emcCalibrationDataHelper();

#ifndef __CINT__
  /** get calibration object for a given fem and a given flavour.
      e.g. Gains, HLRatios, Pedestals5, LCTofs, WalkTofs, TacPeds.*/
  const emcCalFEM* getCalibration(int femAbsolutePosition, 
				  const std::string& whatKind);
  
  /** same as above with no bound checking. It's also assumed
      that collect from DB has been done already. */
  const emcCalFEM* getCalibrationFast(int femAbsolutePosition,
				      const std::string& whatKind);

  ///
  const emcCalibrationData* getCalibrationData(const std::string& what, 
					       size_t number=0);

#endif

  const emcCalFEM* getCalibration(int femAbsolutePosition,
				  const char* whatKind);

  /** same as above with no bound checkind and collect from DB assumed
      to be done already. */
  const emcCalFEM* getCalibrationFast(int femAbsolutePosition,
				      const char* whatKind);

  const emcCalibrationData* getCalibrationData(const char* what,
					       size_t number=0);

  // get energy calibration for a given tower.

  float getEnergyCalibration(int towerid);
  float getEnergyCalibrationFast(int towerid);

  /** Return the gain base line factor for a given sector.
      Base line is defined as ... FIXME

      @param isector sector number (0-7, online convention)
      @param what can be value or error
      @param details format is "trimmed percent:xmin|xmax:-n1:-n2..."
      where trimmed percent is an integer giving the trimming percentage
      to compute a trimmed mean.
      if xmin is there, the gain value used for mean computation is that
      of the beginning of the validity period.
      if x is there, the gain used is that of the beginning of the *run*.
      if xmax is there, gain used is that of the end of the validity period.
      if -n_i is there, this fem (referenced by its absolute position), will
      not be used in the computation of mean.
      @param reallySilent suppress \e all screen outputs
  */
  float getGainBaseLine(int isector,
			const char* what="value",
			const char* details="0:xmax:-3:-9:-15:0",
			bool reallySilent=false);

  const char* gainBaseLineCalculationDetails(int isector) const; 
  
  int runNumber(void) const { return fRunNumber; }

  PHTimeStamp timeStamp(void) const { return fTimeStamp; }

  /// The list of FEM that object has been configured to use.
  emcFEMList* femlist() const { return fFemList; }
  void print();

private:

  void ctor(int runnumber,
	    const PHTimeStamp& ts, 
	    bool initall,
	    emcManageable::EStorage source,
	    const char* parts);

  emcCalFEM* collect(int femAbsolutePosition, const std::string& whatKind);
  emcCalibrationData* collectCalibData(const std::string& what, 
				       size_t number); 

  void initAll(void);
  void initECalAtT0(bool normalizationON);

  void patch(emcCalFEM& calfem, const std::string& how);

 public:

  void initGainBaseLine(int isector, const char* details);

  // To give access from ROOT prompt
  emcManageable::EStorage source(const char* what)
  { return source(std::string(what)); }

  emcManageable::EStorage source(const std::string& what);

  bool setSource(const char* what, emcManageable::EStorage datasource)
  { return setSource(std::string(what),datasource); }

  bool setSource(const std::string& what, emcManageable::EStorage datasource);

private:

  emcCalibrationDataHelper(const emcCalibrationDataHelper&);
  emcCalibrationDataHelper& operator=(const emcCalibrationDataHelper&);

#ifndef __CINT__
private:
  typedef std::map<std::string, std::vector<emcCalFEM*> > TMAP;
  typedef std::map<std::string, std::vector<emcCalFEM*> >::iterator TMAPITER;
  typedef std::map<std::string, std::vector<emcCalibrationData*> > TCMAP;
  typedef std::map<std::string, 
		   std::vector<emcCalibrationData*> >::iterator TCMAPITER;

  TMAP fData;
  TCMAP fCalibData;
  PHTimeStamp fTimeStamp;
  std::map<std::string,emcManageable::EStorage> fSources;
  class Flavour
  {
  public:
    typedef std::set<int>::const_iterator const_iterator;

    Flavour(const std::string& name) :
      fName(name) {}

    void append(int ifem) { fFems.insert(ifem); }
    std::string name() const { return fName; }
    const_iterator begin() const { return fFems.begin(); }
    const_iterator end() const { return fFems.end(); }
    size_t size() const { return fFems.size(); }

  private:
    std::string fName;
    std::set<int> fFems;
  };

  std::vector<Flavour> fKnownFlavours;
  std::vector<float> fECalAtT0;

  emcFEMList* fFemList;

  class BaseLine
  {
  public:
    BaseLine(float value=0, float error=0, float skew=0, float kurt=0)
      : fValue(value), fError(error), fSkewness(skew), fKurtosis(kurt) {}

    float value() const { return fValue; }
    float error() const { return fError; }
    float skewness() const { return fSkewness; }
    float kurtosis() const { return fKurtosis; }

  private:
    float fValue;
    float fError;
    float fSkewness;
    float fKurtosis;
  };

  typedef std::map<int,BaseLine> TBLMAP;
  typedef std::map<int,BaseLine>::iterator TBLMAPITER;

  TBLMAP fGainBaseLine;
  std::map<int,std::string> fGainBaseLineCalculationDetails;
  bool fInitAll;

#endif
  int fRunNumber;
  int fMaxNumberOfFEMs;
};

#endif
