#include <emcObjectManager.h>
#include <emcObjectManagerRegister.h>

#include <iostream>

using namespace std;

emcObjectManagerRegister* emcObjectManagerRegister::fInstance = 0 ;
int emcObjectManagerRegister::fCount = 0 ;

//___________________________________________________________________________
emcObjectManagerRegister::emcObjectManagerRegister()
{
  // this ctor is protected because this class is a singleton
  // Access an instance of this object using the GetInstance() method
  // instead.
  
}

//___________________________________________________________________________
emcObjectManagerRegister::~emcObjectManagerRegister()
{
  fCount-- ;
  if (fCount==0) 
    {
       delete fInstance ;
       fInstance = 0;
    }
}

//___________________________________________________________________________
void emcObjectManagerRegister::AddObjectManager(emcObjectManager* objmgr)
{
  string name = objmgr->GetName() ;
  if (!name.empty()) {
    fMap[objmgr->GetName()] = objmgr ;
  }
  else {
    cout << "<E> emcObjectManagerRegister::AddObjectManager - objmgr not added" << endl ;
  }
}

//___________________________________________________________________________
emcObjectManagerRegister* emcObjectManagerRegister::GetInstance(void)
{
  if (!fInstance) {
    fInstance = new emcObjectManagerRegister() ;
  }
  fCount++ ;
  return fInstance ;
}

//___________________________________________________________________________
emcObjectManager* emcObjectManagerRegister::GetObjectManager(const char* name)
{
  emcObjectManager* objmgr = 0 ;
  
  if (fMap.find(name) != fMap.end()) {
    
    // FIXME. We should return here a new object created from the one
    // in the map, i.e. we should implement a emcObjectManager::Clone() method
    // and return fMap[name]->Clone() instead of just fMap[name].
    
    objmgr = fMap[name] ;
  }
  return objmgr ;
}

//_____________________________________________________________________________
void emcObjectManagerRegister::RemoveObjectManager(emcObjectManager* obj)
{
  map<string, emcObjectManager*>::iterator it ;
  bool kRemoved = false ;



  for ( it = fMap.begin() ; it != fMap.end(); it++ ) {
    if ( it->second == obj ) {
      fMap.erase(it) ;
      kRemoved = true ;
      break;
    }
  }
  if (!kRemoved) {
    cerr << __FILE__ << ":" << __LINE__ << " Cannot find object manager " << obj->GetName() << endl ;
  }
}
