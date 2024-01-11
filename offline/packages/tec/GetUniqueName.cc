//INCLUDECHECKER: Removed this line: #include "GetUniqueName.h"

//INCLUDECHECKER: Removed this line: #include <TString.h>
#include <TROOT.h>

#include <cstdlib>
//INCLUDECHECKER: Removed this line: #include <cstring>
using namespace std;

/**
 * GetUniqueName returns a name that is unique with regards to
 * ROOT objects. The returned name will be of the form
 * (baseName) (positive integer).
 * <p>
 * The caller is responsible for deleting the memory of the
 * returned C string.
 * 
 * @param baseName Valid prefix for a variable name, such as "hist".
 * @param baseNumber Starting point for suffix; the more unique, the better.
 * @return A name unique in ROOT.
 */
char* GetUniqueName(const char* baseName, int baseNumber)
{
  Int_t number = abs(baseNumber);
  Bool_t notUnique = kTRUE;
  char* name = 0;

  while (notUnique) {
    const char* cname;
    TString nameString(baseName);
    nameString += number;

    cname = (const char*) nameString;
    if (!gROOT->FindObject(cname)) {
      notUnique = kFALSE;

      //All of the work with cname must be
      //done here, before nameString goes
      //out of scope.
      name = new char[strlen(cname) + 1];
      strcpy(name, cname);
    }
    else {
      number++;
    }
  }

  return name;
}
