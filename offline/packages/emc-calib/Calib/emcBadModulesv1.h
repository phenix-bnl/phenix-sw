#ifndef __EMCBADMODULESV1_H__
#define __EMCBADMODULESV1_H__

#include <vector>
#include <map>
#include <string>
#ifndef __EMCBADMODULES_H__
#  include "emcBadModules.h"
#endif
#ifndef __PHTIMESTAMP_H__
#  include "PHTimeStamp.h"
#endif
#ifndef __EMCMANAGEABLE_H__
#  include "emcManageable.h"
#endif
#ifndef __EMCDATASTORAGEMAP_H__
#  include "emcDataStorageMap.h"
#endif

/**
   Implementation of emcBadModules.

   The QA "online" information comes from calib.emc.QAs database (typically
   filled by onlcal, while the QA "physics" information comes from 
   calib.emc.RejectList (typically filled after we started the first analysis
   on the data, i.e. we always exclude some more towers offline than online).

@ingroup calibration

 */

class emcBadModulesv1 : public emcBadModules
{

public:

  /**@name Constructors
   * In each ctor, the init flag has the following meaning :
   * - true => whole EMCAL Q&A information is retrieved.
   * - false => dead/warnmap are computed only when you request 
   * information for a given channel AND you do not use the
   * *Fast methods. This currently can NOT be
   * used if you're reading from Objy, as Objy accesses have to
   * be grouped at the beginning of the program.
   */

  //@{

  /// Default ctor : everything is perfect.
  emcBadModulesv1();

  /// Ctor from files.
  emcBadModulesv1(const char* directory, 
		emcBadModules::EInformationOrigin origin = emcBadModules::kAll,
		  bool init=true,
		  const char* sectors="emcal");

  /** Ctor with potentially different sources for QAs and RejectList.
      By default, both are read from Pg
   */
  emcBadModulesv1(const PHTimeStamp& ts,
		  emcBadModules::EInformationOrigin origin=emcBadModules::kAll,
		  const emcDataStorageMap& source = emcDataStorageMap(emcManageable::kDB_Pg),
		  bool init = true,
		  const char* sectors="emcal");
  //@}

  /// Copy ctor.
  emcBadModulesv1(const emcBadModulesv1&);

  emcBadModulesv1& operator=(const emcBadModulesv1&);

  emcBadModulesv1* clone(void) const { return new emcBadModulesv1(*this); }

  /// Dtor.
  virtual ~emcBadModulesv1();

  /**@name Retrieve dead/warn neighbour information.
     The 'Fast' methods do not do any check, i.e. no
     bound checking nor check whereas information is available/uptodate or not
   */
  //@{ 
  /// Get deadneighbours information for the tower referenced by towerID.
  unsigned int Deadmap(int towerID);
  ///
  unsigned int DeadmapFast(int towerID) const;
  
  /// Get warnneighbours information for the tower referenced by towerID.
  unsigned int Warnmap(int towerID);
  ///
  unsigned int WarnmapFast(int towerID) const;
  //@}
  
  /**@name Access to underlying separate information (Q&A from online,
     Q&A from physics). The 'Fast' methods do not do any check, i.e. no
     bound checking nor check whereas information is available/uptodate or not.
  */
  //@{

  ///
  unsigned int Error(emcBadModules::EInformationOrigin source, int towerID);
  ///
  unsigned int ErrorFast(emcBadModules::EInformationOrigin source, 
			 int towerID) const;
  ///
  unsigned int Warning(emcBadModules::EInformationOrigin source, int towerID);
  ///
  unsigned int WarningFast(emcBadModules::EInformationOrigin source, 
			   int towerID) const;

  //@}

  void identify(std::ostream& os=std::cout) const;

  int isValid() const;

  /// Reset ==> all is perfect.
  void Reset();

  /// Print one tower info (or all if towerid=-1).
  std::ostream& Print(int towerid=-1, std::ostream& out=std::cout);

  /// Number of towers.
  unsigned int size(void) const { return fErrorMap.size(); }

private:

  ///
  void Allocate(void);

  /// Collect Q&A obtained from physics.
  std::string CollectPhysicsQA(void);

  /// Collect Q&A obtained from online (all FEMs).
  std::string CollectOnlineQA(void);

  /// Collect Q&A obtained from online (one FEMs).
  std::string CollectOnlineQA(size_t ifem);

  void Collect(size_t ifem);

  /** From all Q&A (online and physics), compute the dead and warn
      neighbour maps for a given fem.*/
  void ComputeMaps(size_t ifem);

  /// Compute dead and warn maps for all FEMs.
  void ComputeMaps(const char* sectors="emcal");

  /// Return in vector fems the ids of FEMs that are neighbours of ifem.
  void GetListOfNeighbourFEMs(size_t ifem, std::vector<size_t>& fems);

  /** Returns true if towerID can be used as an index for our
      internal storage elements (fErrorRaw, ...).*/
  bool IsValid(int towerID) const;

  /** Insure that we get all the information collected (either from
      files or from DB) in order to get sensible return values from
      Error* or Warning* methods. */
  void update(int towerID);

  /// Print info about one tower.
  std::ostream& PrintOne(int towerid, std::ostream& out=std::cout);

#ifndef __CINT__
private:

  /// Used by copy ctor and assignment operator.
  void copyTo(emcBadModulesv1&) const;

  emcDataStorageMap fDataSource;
  emcBadModules::EInformationOrigin fOrigin;
  std::string fDirectory;
  PHTimeStamp fTimeStamp;
  bool fIsRejectListAlreadyRead;

  std::vector<unsigned int> fErrorMap;
  std::vector<unsigned int> fWarnMap;

  std::vector<bool> fCollectedFEMs;
  std::vector<bool> fComputedFEMs;

  std::map<EInformationOrigin, std::vector<unsigned int> > fErrorRaw;
  std::map<EInformationOrigin, std::vector<unsigned int> > fWarnRaw;

  typedef 
  std::map<EInformationOrigin, std::vector<unsigned int> >::const_iterator 
  RawMapIterator;

  static const unsigned int fMASK_Ampl_Physics    = 0x0000F;
  static const unsigned int fMASK_Ampl_Online     = 0x40000;
  static const unsigned int fMASK_Ampl_OnlineWarn = 0x00000;

  static const unsigned int fMASK_TOF_Physics     = 0x000F0;
  static const unsigned int fMASK_TOF_Online      = 0x00000; 
  static const unsigned int fMASK_TOF_OnlineWarn  = 0x00000;

#endif

  ClassDef(emcBadModulesv1,1)
};

#endif
