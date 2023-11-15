#include <TOAD.h>
#include <sys/stat.h>
#include <string>
#include <boost/tokenizer.hpp>

#include <cstdlib>
#include <cstring>
#include <iostream>

using namespace std;
using namespace boost;

std::string
TOAD::location(const std::string& logical_name)
{

  std::string notfound = "";

  if (verb > 0) {
    cout << "TOAD : Looking for file = " << logical_name << endl;
  }

  // Check for NULL string...
  if (logical_name.compare("") == 0)
  {
    return notfound;
  }

  char * pathlist;
  pathlist = getenv("TSEARCHPATH");
  if (verb > 0) {
    cout << "TOAD : TSEARCHPATH list = " << pathlist << endl;
  }
  if (pathlist!=NULL)
  {
    string temp = string(pathlist);
    char_separator<char> sep(":");
    tokenizer< char_separator<char> > tok(temp, sep);

    for (tokenizer< char_separator<char> >::iterator beg=tok.begin(); beg!=tok.end();++beg)
    {
      string temp2 = string(*beg)+"/share/"+package_name+"/"+string(logical_name);

      if (verb > 0) {
        cout << "TOAD : Looking in " << temp2 << endl;
      }
      pfn = localSearch(temp2);
      if (!pfn.empty())
      {
        return pfn;
      }
    }
  }

  if (verb > 0) {
    cout << "TOAD : File not found" << endl;
  }
  return notfound;

} 

std::string
TOAD::localSearch(const string &logical_name)
{
  struct stat64 stbuf;
  const char * found = "";
  if (stat64(logical_name.c_str(), &stbuf) != -1)
    {
      if ((stbuf.st_mode & S_IFMT) == S_IFREG)
        {
          return logical_name.c_str();
        }
    }
  return found;
}

