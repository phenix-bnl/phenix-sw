#ifndef __EMCRAWDATAACCESSOR_H__
#define __EMCRAWDATAACCESSOR_H__

/** (Singleton) Adapter class which encapsulates access to
    EmcDynamicData and EMCalFEE classes. 
 
    The purpose of this object
    is to avoid to have to deal with those two (too) closely related objects
    in many places as it is the case now.
 
    Please note that you have 2 options to access the events :
 
    1) Use the SetDataSource(Eventiterator*) and GetNextEvent() methods
    to loop over events by yourself.
 
    2) Use the processEvent(Event*) to process an external event (use this
    method e.g. in an offline reconstruction chain).
 
    To be deprecated as soon as online code does not need it anylonger.
 
    @author: L. Aphecetche.
*/

#ifndef __EMCFEM_H__
#include "EmcFEM.h"
#endif
#include <string>

// the EMCalFEE object would be better named 'emcDataExtractor'
typedef EMCalFEE emcDataExtractor;

class EmcDynamicData;
class emcRawDataAccessor;
class emcRawDataObject;
class Event;
class PHTimeStamp;
class emcConfigurationFile;

class emcRawDataAccessor
{
public:

  ~emcRawDataAccessor();

  /**@name Access methods to underlying objects. */
  //@{
  /** FIXME ? Temporary solution (?).
      The methods (GetDataExtractor and GetDynamicData) should be replaced 
      by a true set of redirection methods (e.g. this object
      should offer the wanted methods of both EMCalFEE and EmcDynamicData.
      That's what an adapter should do.) But it works like it is now...
      So what ?
  */
  emcDataExtractor* GetDataExtractor(void)
  {
    return fDataExtractor;
  }
  ///
  EmcDynamicData* GetDynamicData(void)
  {
    return fDynamicData;
  }
  //@}

  /**@name Getting an instance of the RDA (Raw Data Accessor). */
  //@{

  /// Get instance by specifying configuration filename
  static emcRawDataAccessor* GetInstance(int& status,
					 const char* configfilename);

  /** Get instance by specifying a TimeStamp : config. will be fetch from DB
      if config. exists for this TimeStamp, AND if relevant DataManager plugin exist
      in memory (i.e. relevant library has been loaded) -
  */
  static emcRawDataAccessor* GetInstance(const PHTimeStamp& ts);

  /** Get instance : return value will be null if you haven't
      called one of the the other GetInstance() methods before. */
  static emcRawDataAccessor* GetInstance(void);

  //@}
  /// removes instance and resets counter to zero - be sure about what you are doing
  static void RemoveInstance(void);

  /// Release instance
  static void ReleaseInstance(void);

  /// Get next event
  int GetNextEvent(void);

  /// Get access to the Raw Data Object
  emcRawDataObject* GetRawDataObject(void);

  /// Process one external event (usefull for offine chain)
  static int processEvent(Event* evt)
  {
    return fDataExtractor->processEvent(evt);
  }

  /// Set the Eventiterator pointing to the data source
  void SetDataSource(Eventiterator* it);

public:

  FEMlimits lim;

private:

  emcRawDataAccessor(int& status,
		     const char* configfilename);
  emcRawDataAccessor(int& status, emcConfigurationFile& configFile);

private:

  static std::string fConfigFileName;
  static int fCount;
  static EmcDynamicData * fDynamicData;
  static EMCalFEE       * fDataExtractor;
  static Eventiterator  * fEventiterator;
  static emcRawDataAccessor * fInstance;
  static emcRawDataObject   * fRDO;
};

#endif
