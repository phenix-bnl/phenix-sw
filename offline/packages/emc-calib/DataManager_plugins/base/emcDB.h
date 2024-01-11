#ifndef __EMCDB_H__
#define __EMCDB_H__

#include <string>
#include <vector>
#ifndef __PHTIMESTAMP_H__
#include "PHTimeStamp.h"
#endif
#ifndef __EMCMANAGEABLE_H__
#include "emcManageable.h"
#endif

class emcTracedFEM;
class PdbCalBank;
class PdbBankManager;

/** Usefull tools to access EMCAL databases. 
    Used to produce the emcDB program.

    <a href="http://www.phenix.bnl.gov/~aphecetc/Computing/Tutorials/tutorial-emcdb.html">HOWTO use the emcDB program</a>
 */

//_____________________________________________________________________________
class emcDB {

public:
  /// default ctor.
  emcDB(const char* dbms="Pg",
	const char* dbname="calibrations",
	bool interactive=false, PHTimeStamp* ts=0,
	PHTimeStamp* tsend=0);

  /// dtor.
  ~emcDB();

  /// Set debug to yes or no.
  void Debug(bool value=true) { fDebug = value; }

  /// Dump a given database, e.g. calib.emc.Gains
  bool Dump(const std::string fulldbname);
  /// same as above.
  bool Dump(const char* fulldbname) { return Dump(std::string(fulldbname)); }

#ifndef __CINT__
  /// Compare a bank from Objy with the same object fetch from file.
  int Compare(PdbCalBank& bank, const std::string& directory, 
	      const std::string& flavour);
#endif

  /// Generate a temporary file with runnumber->timestamps relationship.
  void MakeRunTimes(int minrunnumber);

  /// Set the bankID to be used for next searches.
  void SetBankID(int bankid) { fBankID = bankid; }

  /// Set configuration filename (only needed for pedestals).
  void SetConfigurationFile(const std::string& filename) {
    fConfigurationFile = filename; }

  /// Same as above.
  void SetConfigurationFile(const char* filename) {
    SetConfigurationFile(std::string(filename));
  }

  /// Set the directories to look for comparisons.
  void SetDirectories(std::vector<std::string>& directories) {
    fDirectories = directories;
  }

  /// Set the end-of-validity interval for next searches.
  void SetEndInterval(const PHTimeStamp& endAfter,
			const PHTimeStamp& endBefore);

  /// Set the insert date interval for next searches.
  void SetInsertInterval(const PHTimeStamp& insertAfter,
			 const PHTimeStamp& insertBefore);

  /// Set the start-of-validity interval for next searches.
  void SetStartInterval(const PHTimeStamp& startAfter,
			const PHTimeStamp& startBefore);

  /// Read a directory with calibration files and write them into DB.
  bool Update(const std::string directory);

  /** Special case of update for PbGl Gains.
      where directory structure is slightly more complicated.*/
  bool UpdatePbGlGains(const std::string top_directory);

  /// Another special case for Initial Calibrations.
  bool UpdateInitialCalibration(const std::string top_directory);

  /// Yet Another Special case for Reject List
  bool UpdateRejectList(const std::string top_directory);

  /// Yet Another Special case for TofSectorOffset
  bool UpdateTofSectorOffset(const std::string dir, int runnumber);

  /// Get the version of this class.
  static std::string Version(void);

#ifndef __CINT__
private:

  int AbsolutePosition(const std::string& femName);

  void Abort(const std::string& method, const std::string& message);

  PdbBankManager* BankManager();

  emcManageable::EStorage destination() const;

  void DumpContent(PdbCalBank&, int nchannels);

  void CompareToDirectories(PdbCalBank&,const std::string&);

  PHTimeStamp EndOfValidity(void) const;
  bool Error(const std::string method, const std::string message);
  int GetPinNumber(int absPosition);
  void initDBMS(const char* dbms); 
  void ParseFileName(const std::string filename, size_t& ifem, size_t& pin);
  int PedestalVersion(const std::string cdir);
  bool Quit(void);
  bool ReadConfiguration(bool debug);
  void Split(const std::string& str, std::vector<std::string>& split);
  void UpdateXValue(emcTracedFEM& fem, int value);
#endif

private:
  std::string fDBMS;
  std::string fDbname;
  bool fInteractive; 
  PHTimeStamp* fForceDate;
  PHTimeStamp* fForceEndDate;
  int fBankID;
  bool fDebug;
  PHTimeStamp fInsertAfter;
  PHTimeStamp fInsertBefore;
  PHTimeStamp fStartAfter;
  PHTimeStamp fStartBefore;
  PHTimeStamp fEndAfter;
  PHTimeStamp fEndBefore;
  bool fStartIntervalGiven;
  bool fEndIntervalGiven;
  bool fInsertIntervalGiven;
  std::vector<std::string> fDirectories;
  std::string fConfigurationFile;
  std::vector<int> fPinNumbers;
};

#endif
