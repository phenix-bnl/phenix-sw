#include "emcConfigurationFile.h"
#include <fstream>
#include <cassert>

emcConfigurationFile::emcConfigurationFile(const char* filename)
  : fCurrent(0),fStart(0)
{
  fEnd.setToFarFuture();
  std::ifstream in(filename);
  if (!in) {
    fIsValid = false;
  }
  else {
    fContent = "";
    char c;
    int n = 0;
    while ( in.get(c) ) {
      fContent += c;
      n++;
    }
    assert(n!=0);
    fIsValid = true;
  }
}

bool emcConfigurationFile::IsValid(const PHTimeStamp& cwhen) const
{
  PHTimeStamp& when = const_cast<PHTimeStamp&>(cwhen);
  return ( fIsValid && when.isInRange(fStart,fEnd) );
}

bool emcConfigurationFile::GetLine(char* s, int size)
{  
  if ( fCurrent >= static_cast<int>(GetSize()) ) { 
    s[0] = '\0'; 
    return false; 
  }

  int i;
  int pos = 0;

  for ( i = 0; i < size-1; i++ ) {
    pos = i + fCurrent;
    if ( pos >= static_cast<int>(GetSize())-1 ) {
      s[i] = '\0'; 
      fCurrent = pos+1;
      return true;
    }
    else {
      s[i] = fContent[pos];
      if ( s[i] == '\n' ) { s[i+1] = '\0'; fCurrent = pos+1; return true; }
    }
  }

  fCurrent = pos + 1;
  s[size-1] = '\0';
  return true;
}

