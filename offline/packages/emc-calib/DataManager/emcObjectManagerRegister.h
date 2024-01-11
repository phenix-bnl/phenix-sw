#ifndef __EMCOBJECTMANAGERREGISTER_H__
#define __EMCOBJECTMANAGERREGISTER_H__

#include <string>
#include <map>

class emcObjectManager;

typedef std::map<std::string, emcObjectManager*> emcObjectManagerMap;

/** (Singleton) Registry to keep track of all the DataManager plug-ins.
 * @author Laurent Aphecetche
 * @ingroup datamanager
 */

//___________________________________________________________________________
class emcObjectManagerRegister
{
private:
  emcObjectManagerMap fMap;

private:
  static emcObjectManagerRegister* fInstance;
  static int fCount;

private:
  /// ctor not to be used as this is a singleton.
  emcObjectManagerRegister();

public:
  /** Get THE instance of this class */
  static emcObjectManagerRegister* GetInstance(void);

  /// dtor.
  virtual ~emcObjectManagerRegister();

  /** Register an ObjectManager */
  virtual void AddObjectManager(emcObjectManager*);

  /** Remove an ObjectManager from the registry */
  virtual void RemoveObjectManager(emcObjectManager*);

  /** Get an ObjectManager by name */
  virtual emcObjectManager* GetObjectManager(const char*);

  /** FIXME we should not return the map but provide couple of method
      to iterate over the registered ObjectManager instead, or have
      an external iterator ?
  */
  virtual const emcObjectManagerMap& GetObjectManagerMap(void)
    {
      return fMap;
    }
};

#endif
