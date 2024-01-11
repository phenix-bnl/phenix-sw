#include "emcNamed.h"
#include <iostream>

//___________________________________________________________________________
emcNamed::emcNamed(const char* name, const char* title, const char* classname)
{
  fName = name;
  fTitle = title;
  fClassName = classname;
}

//___________________________________________________________________________
emcNamed::emcNamed(const emcNamed& named)
{
  named.Copy(*this);
}

//___________________________________________________________________________
emcNamed& 
emcNamed::operator=(const emcNamed& named)
{
  if (this != &named)
    {
      named.Copy(*this);
    }
  return *this;
}

//___________________________________________________________________________
emcNamed::~emcNamed()
{}

//_____________________________________________________________________________
void
emcNamed::NameIt(const std::string& name, const std::string& title,
                 const std::string& classname)
{
  SetName(name);
  SetTitle(title);
  SetClassName(classname);
}

//___________________________________________________________________________
void 
emcNamed::Copy(emcNamed& named) const
{
  // Copy this to named
  named.SetName(fName);
  named.SetTitle(fTitle);
  named.SetClassName(fClassName);
}

//___________________________________________________________________________
std::ostream& operator << (std::ostream& out, const emcNamed& rhs)
{
  out << "emcNamed: name='" << rhs.GetName() << "' title='" << rhs.GetTitle()
      << " classname=[" << rhs.GetClassName() << "]";
  return out;
}
