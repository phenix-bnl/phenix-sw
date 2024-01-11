// $Id: emcDataManager.C,v 1.13 2007/04/04 22:23:02 phnxemc Exp $

#include "emcDataManager.h"
#include "emcManageable.h"
#include "emcObjectManager.h"
#include "emcObjectManagerRegister.h"
#include <iostream>
#include <cstdlib>

/**
   Here are some implementation notes.
 
   The Read,Write and Create methods are implemented the very same way.
   So we could probably have make a couple of methods to iterate over
   the object manager register, and use those methods in Read, Write
   and Create instead of having 3 times the same kind of loop.
   But we still don't know if this class will have to run in a
   multi-threaded environment. If so, we might want to think a little
   more about what this implies for a single looping mechanism.
   I've not give it a though yet. Just have to...
*/

int emcDataManager::fVerboseLevel = 0 ;

using namespace std;

//_____________________________________________________________________________
emcDataManager::emcDataManager()
{
  char* dir = getenv("EMCAL_DATA_COMMON");
  if (dir)
    {
      SetDestinationDir(dir);
      SetSourceDir(dir);
    }
  else
    {
      fDestinationDir = "";
      fSourceDir = "";
    }
}

//_____________________________________________________________________________
emcDataManager* emcDataManager::GetInstance()
{
  static emcDataManager* _instance = new emcDataManager() ;
  return _instance ;
}

//_____________________________________________________________________________
emcManageable* emcDataManager::Collect(const emcManageable& object,
                                       const PHTimeStamp& when)
{
  // very basic implementation. We loop over the registered
  // object manager, and we find one that can handle object.

  emcManageable* value = 0 ;

  emcObjectManagerMap cmap =
    emcObjectManagerRegister::GetInstance()->GetObjectManagerMap() ;
  emcObjectManagerMap::const_iterator it ;

  if ( cmap.empty() )
    {
      cout << "<emcDM::Collect> Plugin map is empty.  Sorry." << endl ;
    }
  else
    {
      for ( it = cmap.begin() ; it != cmap.end() && value == 0 ; it++)
        {
          emcObjectManagerMap::value_type entry = *it ;
          if ( entry.second->CanCollect(object) )
            {
              value = entry.second->Collect(object, when) ;
            }
        }
    }
  return value ;
}

//_____________________________________________________________________________
void emcDataManager::ls(const char*)
{
  cout << (*this) ;
}

//_____________________________________________________________________________
bool emcDataManager::Read(emcManageable& object,
                          const PHTimeStamp& time_stamp,
                          int bankID)
{
  // very basic implementation. We loop over the registered
  // object manager, and we find one that can handle object.

  bool kIsRead = false ;
  bool kFoundPlugin = false ;

  emcObjectManagerMap cmap =
    emcObjectManagerRegister::GetInstance()->GetObjectManagerMap() ;
  emcObjectManagerMap::const_iterator it ;

  if ( cmap.empty() )
    {
      cout << "<emcDM::Read> Plugin map is empty.  Sorry." << endl ;
    }
  else
    {
      for ( it = cmap.begin() ; it != cmap.end() && !kIsRead ; it++)
        {
          emcObjectManagerMap::value_type entry = *it ;
	  if ( GetVerboseLevel() >= 10 )
	    {
	      cout << "emcDataManager::Read : trying plugin "
		   << entry.second->GetName()
		   << ":" 
		   << entry.second->GetTitle()
		   << endl;
	    }

          if ( entry.second->CanRead(object) )
            {
              kFoundPlugin = true ;
              kIsRead = entry.second->Read(object, time_stamp, bankID) ;
            }
        }
    }

  if (!kFoundPlugin)
    {
      cerr << "<EMC-ERROR-from-DataManager> " << "Read : I did not find "
	   << "a suitable plugin for this object : name=" << object.GetName()
	   << " title=" << object.GetTitle()
	   << " classname=" << object.GetClassName() << endl ;
      cerr << "<EMC-ADVICE> a) Check the list of available plugins by : " 
	   << endl
	   << " emcDataManager* dm = emcDataManager::GetInstance() ; " << endl
	   << " dm->ls() ; " << endl ;
      cerr << "<EMC-ADVICE> b) Check that the object you'd like to read " 
	   << endl
	   << " has the correct source set, e.g. " << endl
	   << " cout << object.GetSource() ; " 
	   << " object.SetSource(emcManageable::kFile_ASCII) " << endl
	   << " or object.SetSource(emcManageable::kDB_Objy) " << endl
	   << " (note that some plugins might not support all sources)" 
	   << endl ;
    }
  return kIsRead ;
}

//_____________________________________________________________________________
bool emcDataManager::Read(emcManageable& object, int runnumber)
{
  // very basic implementation. We loop over the registered
  // object manager, and we find one that can handle object.

  bool kIsRead = false ;
  bool kFoundPlugin = false ;

  emcObjectManagerMap cmap =
    emcObjectManagerRegister::GetInstance()->GetObjectManagerMap() ;
  emcObjectManagerMap::const_iterator it ;

  if ( cmap.empty() )
    {
      cout << "<emcDM(emcManageable& object, int runnumber)::Read> Plugin map is empty.  Sorry." << endl ;
    }
  else
    {
      for ( it = cmap.begin() ; it != cmap.end() && !kIsRead ; it++)
        {
          emcObjectManagerMap::value_type entry = *it ;
          if ( entry.second->CanRead(object) )
            {
              kFoundPlugin = true ;
              kIsRead = entry.second->Read(object, runnumber) ;
            }
        }
    }
  if (!kFoundPlugin)
    {
      cerr << "<EMC-ERROR-from-DataManager> " << "Read : I did not find "
	   << "a suitable plugin for this object : name=" << object.GetName()
	   << " title=" << object.GetTitle()
	   << " classname=" << object.GetClassName() << endl ;
      cerr << "<EMC-ADVICE> a) Check the list of available plugins by : " << endl
	   << " emcDataManager* dm = emcDataManager::GetInstance() ; " << endl
	   << " dm->ls() ; " << endl ;
      cerr << "<EMC-ADVICE> b) Check that the object you'd like to read " << endl
	   << " has the correct source set, e.g. " << endl
	   << " cout << object.GetSource() ; object.SetSource(emcManageable::kFile_ASCII) " << endl
	   << " or object.SetSource(emcManageable::kDB_Objy) " << endl
	   << " (note that some plugins might not support all sources)" << endl ;
    }
  return kIsRead ;
}

//_____________________________________________________________________________
bool emcDataManager::ReadPreviousVersion(emcManageable& object,
					 const PHTimeStamp& time_stamp,
					 int bankID,
					 int version)
{
  // very basic implementation. We loop over the registered
  // object manager, and we find one that can handle object.

  bool kIsRead = false ;
  bool kFoundPlugin = false ;

  emcObjectManagerMap cmap =
    emcObjectManagerRegister::GetInstance()->GetObjectManagerMap() ;
  emcObjectManagerMap::const_iterator it ;

  if ( cmap.empty() )
    {
      cout << "<emcDMI::ReadPreviousVersion> Plugin map is empty. Sorry." << endl ;
    }
  else
    {
      for ( it = cmap.begin() ; it != cmap.end() && !kIsRead ; it++)
        {
          emcObjectManagerMap::value_type entry = *it ;
          if ( entry.second->CanRead(object) )
            {
              kFoundPlugin = true ;
              kIsRead = entry.second->ReadPreviousVersion
		(object, time_stamp, bankID, version) ;
            }
        }
    }
  if (!kFoundPlugin)
    {
      cerr << "<EMC-ERROR-from-DataManager> "
	   << "ReadPreviousVersion : I did not find "
	   << "a suitable plugin for this object : name=" << object.GetName()
	   << " title=" << object.GetTitle()
	   << " classname=" << object.GetClassName() << endl ;
      cerr << "<EMC-ADVICE> a) Check the list of available plugins by : " << endl
	   << " emcDataManager* dm = emcDataManager::GetInstance() ; " << endl
	   << " dm->ls() ; " << endl ;
      cerr << "<EMC-ADVICE> b) Check that the object you'd like to read " << endl
	   << " has the correct source set, e.g. " << endl
	   << " cout << object.GetSource() ; object.SetSource(emcManageable::kFile_ASCII) " << endl
	   << " or object.SetSource(emcManageable::kDB_Objy) " << endl
	   << " (note that some plugins might not support all sources)" << endl ;
    }
  return kIsRead ;
}

//_____________________________________________________________________________
void emcDataManager::Reset(void)
{
  // very basic implementation. We loop over the registered
  // object managers, and we call their Reset() method

  emcObjectManagerMap cmap =
    emcObjectManagerRegister::GetInstance()->GetObjectManagerMap() ;
  emcObjectManagerMap::const_iterator it ;

  for ( it = cmap.begin() ; it != cmap.end() ; it++)
    {
      emcObjectManagerMap::value_type entry = *it ;
      entry.second->Reset() ;
    }
}

//_____________________________________________________________________________
bool emcDataManager::Write(const emcManageable& object,
                           const PHTimeStamp& tStart,
                           int bankID)
{
  // very basic implementation. We loop over the registered
  // object manager, and we find one that can handle object.
  // We then ask him to write object.

  emcObjectManagerMap cmap =
    emcObjectManagerRegister::GetInstance()->GetObjectManagerMap() ;

  bool written = false ;

  emcObjectManagerMap::const_iterator it ;

  if ( cmap.empty() )
    {
      cout << "<emcDM:Write> Plugin map is empty. Sorry." << endl ;
    }
  else
    {
      for ( it = cmap.begin() ; it != cmap.end() && !written ; it++)
        {
          emcObjectManagerMap::value_type entry = *it ;
          if ( entry.second->CanWrite(object) )
            {
              written = entry.second->Write(object, tStart, bankID) ;
            }
        }
    }
  return written ;
}

//_____________________________________________________________________________
ostream& operator << (ostream& out, const emcDataManager&)
{
  emcObjectManagerMap cmap = emcObjectManagerRegister::GetInstance()->GetObjectManagerMap() ;
  emcObjectManagerMap::const_iterator it ;

  if ( cmap.empty() )
    {
      out << "<ostream& operator << (ostream& out, const emcDataManager&)> Plugin map is empty. Sorry." << endl ;
    }
  else
    {
      out << "<I> Here are my plug-ins to handle objects : " << endl ;
      for ( it = cmap.begin() ; it != cmap.end() ; it++)
        {
          emcObjectManagerMap::value_type entry = *it ;
          out << "  -> " << entry.second->GetName()
          << " " << entry.second->GetTitle() << endl ;
        }
    }
  return out ;
}



