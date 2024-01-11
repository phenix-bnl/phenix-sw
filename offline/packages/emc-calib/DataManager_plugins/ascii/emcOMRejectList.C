#include "emcObjectManager.h"
#include "emcManageable.h"
#include "phool.h"

/** DM plugin to R/W emcRejectlist from/to ASCII file. 
    @ingroup dmplugins
*/

class emcOMRejectList : public emcObjectManager
{
public:
  emcOMRejectList(const char* name, const char* title);
  virtual ~emcOMRejectList();

  bool CanRead(const emcManageable&) const;
  bool CanWrite(const emcManageable&) const;

  using emcObjectManager::Read;

  /** Should describe here the file format.
      @ingroup dmasciifiles
   */
  bool Read(emcManageable& object,
	    const PHTimeStamp& time_stamp,
	    int id);
  
  void Reset() {}

  bool Write(const emcManageable& object,
	     const PHTimeStamp& tdummy,
	     int dummy = -1);
  
private:
  class changeName
  {
  public:
    changeName(const char* name)
    {
      name_ = emcManageable::GetStorageName(emcManageable::kFile_ASCII);
      name_ += ":";
      name_ += name;
    }
    
    const char* c_str() const
    {
      return name_.c_str();
    }
    
  private:
    std::string name_;
  };
};

#include "emcRejectList.h"
#include "emcDefines.h"
#include <iostream>
#include "EmcIndexer.h"
#include "emcDataManager.h"
#include <fstream>
#include <sstream>
#include <cstdio>
#include "TSystem.h"

//_____________________________________________________________________________
emcOMRejectList::emcOMRejectList(const char* name, const char* title)
  : emcObjectManager(changeName(name).c_str(),title)
{  
}

//_____________________________________________________________________________
emcOMRejectList::~emcOMRejectList()
{
}

//_____________________________________________________________________________
bool
emcOMRejectList::CanRead(const emcManageable& object) const
{
  if ( object.GetSource() != emcManageable::kFile_ASCII )
    {
      return false;
    }
  const emcRejectList* test = dynamic_cast<const emcRejectList*>(&object);

  if (test)
    {
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
bool
emcOMRejectList::CanWrite(const emcManageable& object) const
{
  if ( object.GetDestination() != emcManageable::kFile_ASCII )
    {
      return false;
    }
  const emcRejectList* test = dynamic_cast<const emcRejectList*>(&object);

  if (test)
    {
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
bool
emcOMRejectList::Read(emcManageable& object,
		      const PHTimeStamp& /*time_stamp*/,
		      int /*id*/)
{ 
  emcRejectList& rl = static_cast<emcRejectList&>(object);
  
  if ( DM()->GetVerboseLevel() ) 
    {
      std::cout << EMC_INFO_MSG << " : Entering " << GetName()
		<< "::Read" << std::endl;
    }

  emcDataManager* dm = emcDataManager::GetInstance();
 
  std::ostringstream str; 
  char* stmpdir = gSystem->ExpandPathName(dm->GetSourceDir());
  str << stmpdir << "/RejectList/reject.list";
  delete[] stmpdir;

  std::ifstream in(str.str().c_str());
  if ( !in ) 
    {    
      std::cerr << "<E> Cannot open file " << str.str().c_str() << std::endl;
      return false;
    }

  char line[255];
  
  int sector,z,y;
  int amplError,tofError,amplWarning,tofWarning;

  while (in.getline(line, 255, '\n'))
    {
    
    if (line[0]=='#') 
      {
	continue; // skip comment lines
      }

    if ( 7 != sscanf(line, "%d %d %d %d %d %d %d", &sector, &z, &y, 
		     &amplError, &tofError, &amplWarning, &tofWarning))
      {	
	continue; // skip incomplete lines
      }

    if ( DM()->GetVerboseLevel()>=10 )
      {
	printf("%d %d %d %d %d %d %d\n", sector, z, y, 
	       amplError, tofError, amplWarning, tofWarning);
      }

    if (EmcIndexer::IsValid(sector, z, y))
      {
	int towerid = EmcIndexer::getTowerId(sector, z, y);
	rl.set_or(towerid,amplError,amplWarning,tofError,tofWarning);
      }
    else
      {
	std::cerr << "<E> emcOMRejectList::ReadFromFile : no such tower : "
	     << "sector=" << sector << " z=" << z << " y=" << y
	     << std::endl;
      }
  }
  in.close();

  return true;
}

bool 
emcOMRejectList::Write(const emcManageable& object,
		       const PHTimeStamp&,
		       int)
{
  const emcRejectList& rl = static_cast<const emcRejectList&>(object);
  
  if ( DM()->GetVerboseLevel() ) 
    {
      std::cout << EMC_INFO_MSG << " : Entering " << GetName()
		<< "::Write" << std::endl;
    }

  emcDataManager* dm = emcDataManager::GetInstance();
 
  std::ostringstream str; 
  char* stmpdir = gSystem->ExpandPathName(dm->GetDestinationDir());
  str << stmpdir << "/RejectList";
  delete[] stmpdir;

  void* pdir = gSystem->OpenDirectory(str.str().c_str());
  if (!pdir)
    {
      if ( DM()->GetVerboseLevel() )
	{
	  std::cout << PHWHERE << " Trying to create dir "
		    << str.str() << std::endl;
	}
      int bad = gSystem->mkdir(str.str().c_str(),true);
      if (bad)
	{
	  std::cerr << PHWHERE << " Could not create directory "
		    << str.str()
		    << std::endl;
	  gSystem->FreeDirectory(pdir);
	  return false;
	}
    }

  gSystem->FreeDirectory(pdir);

  str << "/reject.list";
  
  std::ofstream out(str.str().c_str());
  if ( !out ) 
    {    
      std::cerr << "<E> Cannot create file " << str.str().c_str() << std::endl;
      return false;
    }

  out << "# Written by DataManager plug-in=[" << GetName() 
      << "] on " << PHTimeStamp() << std::endl;

  for ( size_t towerid = 0; towerid < 144*172; ++towerid ) 
    {
      if ( rl.nonZero(towerid) )
	{
	  //sscanf(line, "%d %d %d %d %d %d %d", &sector, &z, &y, 
	  //		     &amplError, &tofError, &amplWarning, &tofWarning))		 
	  int sector,z,y;
	  EmcIndexer::decodeTowerId(towerid,sector,z,y);
	  out << sector << " " << z << " " << y << " "
	      << rl.AmplitudeError(towerid) << " "
	      << rl.TimingError(towerid) << " "
	      << rl.AmplitudeWarning(towerid) << " "
	      << rl.TimingWarning(towerid)
	      << std::endl;
	}
    }

  out.close();
  return true;
}

namespace 
{
  emcOMRejectList gemcOMRejectList("emcOMRejectList","Read EMCAL reject list");
}
