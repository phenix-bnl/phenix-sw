#include <emcObjectManager.h>
#include <emcObjectManagerRegister.h>
#include <emcManageable.h>
#include <emcDataManager.h>
#include <iostream>
#include <cassert>

//_____________________________________________________________________________
emcObjectManager::emcObjectManager(const char* name, const char* title)
  : emcNamed(name, title)
{
  emcObjectManagerRegister::GetInstance()->AddObjectManager(this);
}

//_____________________________________________________________________________
emcObjectManager::~emcObjectManager()
{
  emcObjectManagerRegister::GetInstance()->RemoveObjectManager(this);
}

//_____________________________________________________________________________
bool 
emcObjectManager::CanHandle(const emcManageable& object) const
{
  return
    CanWrite(object) ||
    CanRead(object) ||
    CanCollect(object);
}

//_____________________________________________________________________________
emcDataManager* 
emcObjectManager::DataManager(void)
{
  static emcDataManager* dm = emcDataManager::GetInstance();

  if (dm == 0)
    {
      std::cerr << "<E> Cannot obtain an instance of Data Manager ?!" 
		<< std::endl;
      assert (0 == 1);
    }
  return dm;
}
