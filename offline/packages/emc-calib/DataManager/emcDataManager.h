#ifndef __EMCDATAMANAGER_H__
#define __EMCDATAMANAGER_H__


#include <iosfwd>
#include <string>

#ifndef __PHTIMESTAMP_H__
#include "PHTimeStamp.h"
#endif

class emcManageable;

/**
 * (Singleton) Handles read/write of EMCAL objects to/from DB/ASCII files.
 * The basic idea is that all data traffic to/from a database (Objy, Pg, or even some ascii files)
 * should go through this object.
 * The interface of the Data Manager (DM) has 2 main methods : \c Read and \c Write.
 * The \c Read method is used to retrieve objects from a datasource (mainly Postgres or ASCII file, nowadays)
 * usually by packets of 144 items (but this is driven by the object to be read really).
 * The \c Write method can write objects of any size which is usually (but it's not mandatory) 
 * a multiple of 144 items
 * (as long as the underlying plugin is able to do the job, of course...)
 *
 * It is of prime importance to realize that the DM does \e nothing by itself. It uses Object Managers 
 * (OM or plugins hereafter) to handle Read/Write requests.
 * DM and OM can only handle objects which derive from type \c emcManageable.
 * OM are added to the DM via a plug-in mechanism 
 * (see documentation on \c emcObjectManager class for more information).
 *
 * @author Laurent Aphecetche
 * @ingroup datamanager
 */

class emcDataManager
{
 private:
  /// not to be used. This class is a singleton.
  emcDataManager();

 public:

  /// dtor.
  ~emcDataManager() {}
 
  /// Get the unique instance of this class.
  static emcDataManager* GetInstance();

  /** Shows the list of the supported object's type 
      (i.e. the plug-in list).
    */
  void ls(const char* opt="");

  /** \deprecated Collect 1 big manageable valid for all the supermodules 
      in the current configuration.
      @param object Object you want to collect (must be a subclass of
      emcManageable).
      @param when The recovery time.
      @ingroup deprecated
      Has been deprecated because handling of collections is better done
      elsewhere. That's not the job of the datamanager, after all...
   */
  emcManageable* Collect(const emcManageable& object, const PHTimeStamp& when);

  /** Get the directory where to put the calibration data files.
      (in case calibration data has to go on files instead of DB).
   */
  const char* GetDestinationDir(void) { return fDestinationDir.c_str(); }

  /** Get the directory where to find the calibration data files.
      (in case calibration data has to be fetched from files instead of DB).
   */
  const char* GetSourceDir(void) { return fSourceDir.c_str(); }

  /// Get the verbose level
  static int GetVerboseLevel(void) { return fVerboseLevel; }

  /** Read a manageable object from a given source.
      (specified by the obj itself).
      @param object Object you want to read (must be a subclass of
      emcManageable)
      @param tSearch The recovery time
      @param id An identifier of your object (e.g. FEMcode).\\
      This id might be used to compute a Bank Identifier (this bankid\\
      is the actual id which will be stored in the DB). 
      To be deprecated (new one = Read(object,runnumber) ?) ?
      Not obvious how to deprecate it until the underlying technology (pdbcal)
      still relies on timestamps and not on run numbers...
  */
  bool Read(emcManageable& object, 
	    const PHTimeStamp& tSearch,
	    int id=-1);

   /** Read a manageable object from a given source 
      (specified by the obj itself) valid for a given run.
      @param object Object you want to read (must be a subclass of
      emcManageable)
      @param runnumber The recovery runnumber
   */
  bool Read(emcManageable& object, int runnumber);

  /** Special read method to retrieve shadowed banks. 
      Same parameters as for Read method, plus version which is :
      0 for latest version (completely equivalent of Read)
      -1 for previous version, -2 for previous to previous, etc...
   */
  bool ReadPreviousVersion(emcManageable& object, 
			   const PHTimeStamp& time_stamp,
			   int id,
			   int version=0);

  /** Reset DM and underlying plugins. Call this method if you would like
      to have the same behavior of the DM as if it was freshly created.
  */
  void Reset(void);

  /** Set the directory where to put the calibration data files.
      (in case calibration data has to go on files instead of DB).
   */
  void SetDestinationDir(const char* path) { fDestinationDir=path; }

  /** Set the directory where to find the calibration data files.
      (in case calibration data has to be fetched from files instead of DB).
   */
  void SetSourceDir(const char* path) { fSourceDir=path; }

  /// Set the verbose level 
  static void SetVerboseLevel(int level=0) { fVerboseLevel = level; }

  /** Write a manageable object to a given destination.
      Destination is driven by the object itself (i.e. the object knows
      where it should go).
      @param object Object to write to db.
      @param tStart Only starting time for the validity period is given.
      Value of ending time is chosen by the OM.
      It might be 'now' (e.g. for the Gains), 'infinity' (e.g. for
      the Pedestals), or whatever the plugins decides.
      @param id As in the read method
   */
  bool Write(const emcManageable& object,
	     const PHTimeStamp& tStart = PHTimeStamp(0),
	     int id=-1);

private:
  static int fVerboseLevel;
  std::string fSourceDir;
  std::string fDestinationDir;
};

/// output operator.
std::ostream& operator << (std::ostream&, const emcDataManager&);

#endif
