#include "utils.h"
#include <vector>
#include <string>
#include "TDirectory.h"
#include "TH1.h"
#include <cassert>
#include "TObject.h"   
#include "dEmcGeaTrackWrapper.h"   
#include <iostream>

using namespace std;

//_____________________________________________________________________________
void splitPath(const std::string& path,
	       std::vector<std::string>& paths)
{
  // Given a path e.g. /Cut##/OK/C#/V#, will return 
  // a vector of string with Cut#, 

  paths.clear();

  std::string str = path;

  if ( str.empty() )
    {
      return;
    }

  static std::vector<size_t> slashes_pos;

  slashes_pos.clear();

  if ( str[0] != '/' ) 
    { 
      str.insert(str.begin(),'/');
    }

  if ( str[str.size()-1] != '/' ) 
    {
      str.push_back('/');
    }

  for (size_t i = 0 ; i < str.size() ; i++) 
    {
      if ( str[i] == '/' ) 
	{ 
	  slashes_pos.push_back(i);
	}
    }
  
  if ( slashes_pos.size() > 0 ) 
    {
      for (size_t i = 0 ; i < slashes_pos.size()-1 ; i++) 
	{
	  paths.push_back(str.substr(slashes_pos[i]+1,
				     slashes_pos[i+1]-slashes_pos[i]-1));
	}
    }  

}

//_____________________________________________________________________________
bool mkpath(TDirectory* dir, const std::string& path)
{
  static vector<std::string> paths;

  splitPath(path,paths);

  TDirectory* currentdir = dir;

  for ( size_t i = 0 ; i < paths.size() ; i++) {

    currentdir->cd();

    currentdir = dynamic_cast<TDirectory*>(gDirectory->Get(paths[i].c_str()));
    if (!currentdir) 
      {
	currentdir = gDirectory->mkdir(paths[i].c_str());
      }
    assert(currentdir!=0);
  }

  return true;
}

//_____________________________________________________________________________
bool mkpath(TDirectory* dir, const std::string& path, const std::string& cent_class_title )
{
  static vector<std::string> paths;

  splitPath(path,paths);

  TDirectory* currentdir = dir;

  for ( size_t i = 0 ; i < paths.size() ; i++) {

    currentdir->cd();

    currentdir = dynamic_cast<TDirectory*>(gDirectory->Get(paths[i].c_str()));
    if (!currentdir) 
      {
	if (paths[i].size()<4) // should be a directory (like e.g. "C0", ..., "C10")
	  {
	    currentdir = gDirectory->mkdir(paths[i].c_str(),cent_class_title.c_str());
	  }
	else
	  {
	    currentdir = gDirectory->mkdir(paths[i].c_str());
	  }
	assert(currentdir!=0);
      }
  }

  return true;
}

//_____________________________________________________________________________
bool PathIsInDir(const std::string& path, TDirectory* dir)
{
  // This is to avoid annoying ROOT message when a directory does not exist 
  // in Cd(), so we provide this small method to check whereas
  // a path exists under a directory, but without issuing error message
  // in case of failure (just returning false in this case).

  TDirectory* dirsave = gDirectory;

  static std::vector<std::string> paths;

  paths.clear();
  splitPath(path,paths);

  bool ok = true;

  TDirectory* cdir = dir;

  for ( size_t i = 0 ; i < paths.size() && ok ; i++) {

    cdir->cd();

    cdir = dynamic_cast<TDirectory*>(cdir->Get(paths[i].c_str()));
    if ( !cdir ) {
      ok = false; 
    }
  }

  dirsave->cd();

  return ok;
}

//_____________________________________________________________________________
TH1* GetHisto(TDirectory* dir, const std::string& histoname, 
	      const std::string& where)
{
  // Try to find histogram named histoname into directory dir, under
  // path=where (where e.g. = "/Cut##/OK/C#/V#").

  TH1* rv = 0;

  bool ok = PathIsInDir(where,dir);

  if (ok) {

    // Path is in dir, we can safely (i.e. without getting ROOT error message
    // on stdout) cd into it.
    //    dir->cd();
    ok = dir->cd(where.c_str());
    assert(ok==true);
    TObject* obj = gDirectory->Get(histoname.c_str());
    if ( obj ) {
      rv = dynamic_cast<TH1*>(obj);
      if ( !rv ) {
	std::cerr << "GetHisto : object " << histoname << " is not a TH1" << std::endl;
      }
    }
  }
  return rv;
}

//_____________________________________________________________________________
void
dump(dEmcGeaTrackWrapper* d)
{
  std::cout << std::string(50,'-') << std::endl;

   size_t ntr = (size_t) d->RowCount();

   for ( size_t i = 0 ; i < ntr ; ++i )
     {
       printf("i=%2d idparent=%3d pid=%3d anclvl=%d xyz=(%e,%e,%e) ptr=%d\n",
 	     i,
 	     d->get_idparent(i),
 	     d->get_pid(i),
 	     d->get_anclvl(i),
 	     d->get_xyz(0,i),
 	     d->get_xyz(1,i),
 	     d->get_xyz(2,i),
 	     d->get_parent_ptr(i)
 	     );
	     
     }
}
