#ifndef __EMCOBJECTMANAGER_H__
#define __EMCOBJECTMANAGER_H__

#include <emcNamed.h>

class emcManageable;
class PHTimeStamp;
class emcDataManager;

/**
 * (ABC) An object manager is an emcDataManager plug-in.
 *
 * An Object Manager handles objects of type emcManageable.
 *
 * Object Manager typically is responsible for reading/writing objects
 * to a database (filesystem, i.e. ascii files can sometimes be considered 
 * as a database).
 *
 * How to write a plugin ?
 *
 * Let's assume you are
 * writing a plugin to handle objects of type Nut (Nut must be
 * a subclass of emcManageable). You can of course add to an
 * existing plugin the possibility to handle this new object type.
 * But here, we will consider that you are writing a brand new
 * object manager from scratch.
 *
 * 1. You create a new class, say emcOMNut, which must have
 *    in its hierarchy an emcObjectManager. If you want
 *    your OM to access Objy, let's derive from emcObjyObjectManager.
 *
 * 2. Somewhere in the source file emcOMNut.C, 
 *    you \e must declare a (static) object of class emcOMNut, e.g.
 *    in the beginning of your .C file : 
 *
 *    emcOMNut gMyGlobalemcOMNutObject("name","title");
 *
 * 3. Your emcOMNut class must announce which services it provides.
 *    If it can Read Nut objects from the DB, you must override
 *    the emcObjectManager method CanRead(emcManageable& obj) 
 *    method to return true in case obj is of type Nut.
 *    Then you must override the Read method to do the actual job.
 *    Same story with Write and Collect if you need them. Even if
 *    you do nothing with them, you must override them as they
 *    are pure virtual member functions of emcObjectManager.
 *    Also the Reset method is supposed to put the plugin in the same
 *    state as if it was newly created : you must override this one also.
 *    Nothing is better than examples : have a look at the class
 *    emcOMCalFEM to see how it was done for emcCalFEM objects.
 *
 * 4. You make a library with your emcOMNut and Nut classes.
 *
 * Whenever your library will be loaded, the DM will be aware of your
 * ObjectManager automatically (that's the plugin mechanism!).
 *
 * @author L. Aphecetche
 * @ingroup datamanager
 * @ingroup interface
 */

class emcObjectManagerRegister;

class emcObjectManager : public emcNamed
{

public:

  /// default ctor.
  emcObjectManager(const char* name = "", const char* title = "");

  /// dtor.
  virtual ~emcObjectManager();

  /** \deprecated Can we collect such object ? 
      @return false by default.
   */
  virtual bool CanCollect(const emcManageable&) const
  {
    return false;
  }

  /** Can we handle (read or write or collect) a given object type ? */
  virtual bool CanHandle(const emcManageable& object) const;

  /** Can we write a given object type ? 
      @return false by default.
  */
  virtual bool CanWrite(const emcManageable& /*object*/) const
  {
    return false;
  }

  /** Can we read a given object type ? 
      @return false by default.
   */
  virtual bool CanRead(const emcManageable& /*object*/) const
  {
    return false;
  }

  /** \deprecated Collect a manageable from the DB.
      See the DM interface for parameters.
      Unless allowed explicitely by a particular Object Manager,
      in general the returned pointer (<b>should not be deleted</b>) by
      the receiver !
      @ingroup deprecated 
      Has been deprecated because handling of collections is better done
      elsewhere. That's not the job of the datamanager, after all...
  */
  virtual emcManageable* Collect(const emcManageable& /*object*/,
				 const PHTimeStamp& /*when*/)
  {
    return 0;
  }


  /// Get the DataManager instance.
  static emcDataManager* DataManager(void);
  /// as above.
  static emcDataManager* DM(void)
  {
    return DataManager();
  }

  /** Read a manageable object from the DB.
      See the DM interface for parameters.
  */
  virtual bool Read(emcManageable& object,
		    const PHTimeStamp& time_stamp,
		    int id) = 0;


  /** Read a manageable object from the DB.
      See the DM interface for parameters.
  */
  virtual bool Read(emcManageable& /*object*/, int /*runnumber*/)
  {
    return false;
  }

  /** Special read method to retrieve shadowed banks. */
  virtual bool ReadPreviousVersion(emcManageable& object,
				   const PHTimeStamp& time_stamp,
				   int id,
				   int /*version*/ = 0)
  {
    return Read(object, time_stamp, id);
  }

  /** Reset the OM.
      After a call to Reset, the OM must behave as if it was newly created.
  */
  virtual void Reset(void) = 0;

  /** Write a manageable object to the DB.
      See the DM interface for parameters.
  */
  virtual bool Write(const emcManageable& object,
		     const PHTimeStamp& tStart,
		     int id = -1) = 0;
};

#endif
