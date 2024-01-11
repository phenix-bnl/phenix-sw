#include "dirfilemanip.h"
#include "TSystem.h"
#include "emcDataManager.h"
#include <fstream>

bool createDirectory(const std::string& dir)
{
  char* stmp = gSystem->ExpandPathName(dir.c_str());
  std::string sdir = stmp;
  delete[] stmp;

  void* pdir = gSystem->OpenDirectory(sdir.c_str());
  if (!pdir)
    {
      emcDataManager* dm = emcDataManager::GetInstance();
      if ( dm->GetVerboseLevel() )
        {
          std::cout << __FILE__ << " Trying to create directory "
		    << sdir << std::endl;
        }
      // The trailing slash makes ROOT returning failure = -1
      // though the directory *is* created... Strange, but we
      // have to live with it, so remove any traling slash.
      if ( sdir[sdir.size()-1] == '/' )
	{
	  sdir = sdir.substr(0,sdir.size()-1);
	}

      int bad = gSystem->mkdir(sdir.c_str(), true);
      if (bad)
        {
          std::cerr << __FILE__ << " Could not create directory "
		    << sdir << std::endl;
	  return false;
        }
      else
        {
          if ( dm->GetVerboseLevel() )
            {
              std::cout << sdir << " successfully created." << std::endl;
            }
	  return true;
        }
    }
  else
    {
      gSystem->FreeDirectory(pdir);
      return true;
    }
}

bool checkFile(const std::string& file)
{
  std::ifstream in(file.c_str());
  return (in.good());
}

std::string expand(const std::string& dir)
{
  char* stmp = gSystem->ExpandPathName(dir.c_str());
  std::string rv(stmp);
  delete[] stmp;
  return rv;
}
