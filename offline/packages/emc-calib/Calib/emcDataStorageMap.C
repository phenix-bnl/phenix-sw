#include "emcDataStorageMap.h"

#include <string>
#include <vector>

namespace
{
  std::vector<std::string> split(const std::string& x, char sep)
  {
    std::string str = x;

    std::vector<std::string> rv;
    std::vector<int> pos;

    if ( str[0] != sep ) 
    { 
      str.insert(str.begin(),sep);
    }

    if ( str[str.size()-1] != sep ) 
    {
      str.push_back(sep);
    }


    for ( std::string::size_type i = 0; i < str.size(); ++i )
      {
	if ( str[i] == sep )
	  {
	    pos.push_back(i);
	  }
      }

    if ( pos.size() > 0 )
      {
	for ( std::vector<int>::size_type i = 0; i < pos.size()-1; ++i )
	  {
	    rv.push_back(str.substr(pos[i]+1,pos[i+1]-pos[i]-1));
	  }
      }
    return rv;
  }
}

//_____________________________________________________________________________
emcDataStorageMap::emcDataStorageMap(emcManageable::EStorage def)
  : fDefaultStorage(def)
{
}

//_____________________________________________________________________________
void
emcDataStorageMap::clear()
{
  fMap.clear();
}

//_____________________________________________________________________________
bool
emcDataStorageMap::empty() const
{
  return fMap.empty();
}

//_____________________________________________________________________________
std::vector<std::string>
emcDataStorageMap::knownStorages() const
{
  std::vector<std::string> rv;

  TMAP::const_iterator it;
  TMAP::const_iterator end = fMap.end();
  for ( it = fMap.begin(); it != end; ++it )
    {
      rv.push_back(it->first);
    }  
  return rv;
}

//_____________________________________________________________________________
bool
emcDataStorageMap::parse(const std::string& x)
{
  bool error = false;
  emcDataStorageMap rv;
  std::vector<std::string> pairs = split(x,',');
  for ( size_t i = 0; i < pairs.size(); ++i )
    {
      // check the pairs
      std::vector<std::string> p = split(pairs[i],':');
      if ( p.size() != 2 )
	{
	  error = true;
	  break;
	}
      else
	{
	  rv.storage(p[0],p[1]);
	}
    }  
  if (error)
    {
      return false;
    }
  else
    {
      *this = rv;
      return true;
    }
}

//_____________________________________________________________________________
emcManageable::EStorage
emcDataStorageMap::storage() const
{
  return fDefaultStorage;
}

//_____________________________________________________________________________
emcManageable::EStorage
emcDataStorageMap::storage(const std::string& what) const
{
  if ( empty() ) return fDefaultStorage;
  
  TMAP::const_iterator it = fMap.find(what);
  if ( it != fMap.end() )
    {
      return it->second;
    }
  else
    {
      return emcManageable::kNone;
    }
}

//_____________________________________________________________________________
emcManageable::EStorage
emcDataStorageMap::storage(const char* what) const
{
  return storage(std::string(what));
}

//_____________________________________________________________________________
bool
emcDataStorageMap::storage(const std::string& what, 
			   const std::string& storagename)
{
  emcManageable::EStorage value = emcManageable::GetStorage(storagename.c_str());
  TMAP::const_iterator it = fMap.find(what);
  if ( it == fMap.end() )
    {
      return fMap.insert(TMAP::value_type(what,value)).second;
    }
  else
    {
      emcManageable::EStorage old = it->second;
      fMap[what] = value;
      if ( old != storage(what) )
	{
	  std::cout << "<WARNING> emcDataStorageMap::storage(" << what
		    << "," << storagename << ") : changing from "
		    << emcManageable::GetStorageName(old) << " to " 
		    << emcManageable::GetStorageName(storage(what))
		    << std::endl;
	}
    }
  return true;
}

//_____________________________________________________________________________
bool
emcDataStorageMap::storage(const char* what, const char* storagename)
{
  return storage(std::string(what),std::string(storagename));
}

//_____________________________________________________________________________
void
emcDataStorageMap::print(std::ostream& out) const
{
  TMAP::const_iterator it;
  TMAP::const_iterator end = fMap.end();
  for ( it = fMap.begin(); it != end; ++it )
    {     
      out << it->first << " : " 
	  << emcManageable::GetStorageName(it->second)
	  << " (" << it->second << ")" << std::endl;
    }
}

//_____________________________________________________________________________
std::ostream& operator<<(std::ostream& out, const emcDataStorageMap& d)
{
  d.print(out);
  return out;
}
