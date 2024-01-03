#include <PdbParameter.hh>
#include <phool.h>

#include <cstdlib>
#include <cstring>
#include <iostream>

using namespace std;

PdbParameter::PdbParameter()
{
  thePar = 0;
  setName("UNKNOWN");
}

PdbParameter::PdbParameter(const float value)
{
  thePar = value;
  setName("UNKNOWN");
}

PdbParameter::PdbParameter(const float value, const char *name)
{
  thePar = value;
  setName(name);
}

void PdbParameter::print() const
{
  cout << theName << ": " << thePar << endl;
}

void PdbParameter::setName(const char *name)
{

  // this construct protects against not zero terminated strings
  // it copies only a fixed number of chars into the theName array
  // (sizeof(theName)-1 leaves space for a zero terminated char
  // at the end. strlen has problems when used with non zero
  // terminated strings and might crash
  // this code might still crash in the cout of name, but that is
  // already in the exit() part
  // Flawfinder: ignore signals flawfinder to ignore this line, strncpy does not zero terminate the string
  // if the length is exceeded, the termination is done in the line afterwards where the last
  // character is set to \0
  strncpy(theName, name, ( sizeof(theName) - 1));  /* Flawfinder: ignore */
  theName[( sizeof(theName) - 1)] = '\0';
  if (strncmp(theName, name, sizeof(theName)))
    {
      cout << "Name exceeds maximum length of "
	   << (sizeof(theName) - 1)
	   << " characters or is not zero terminated" << endl;
      cout << "Max length name: " << theName << endl;
      cout << "There is no point in continuing, fix your code and try again" << endl;
      cout << "Name used (code might crash now when printing out not zero terminated string): " << name << endl;
      exit(1);
    }

}
