#include <PdbCalBankSave.h>
#include <phool.h>
#include <iostream>

ClassImp(PdbCalBankSave)

using namespace std;

static int nowarning = 0;

void
PdbCalBankSave::Reset()
{
  cout << PHWHERE << "ERROR Reset() not implemented by daughter class" << endl;
  return ;
}

void
PdbCalBankSave::identify(ostream& os) const
{
  os << "identify yourself: virtual PdbCalBankSave Object" << endl;
  return ;
}

int
PdbCalBankSave::isValid() const
{
  cout << PHWHERE << "isValid not implemented by daughter class" << endl;
  return 0;
}

int
PdbCalBankSave::AddBank(const std::string& /*name*/, const int& /*rid*/)
{
  warning("AddBank(const std::string &name, const int &rid)");
  return -1;
}

void
PdbCalBankSave::warning(const char *funcname) const
{
  if (! nowarning)
    {
      cout << "Using virtual function PdbCalBankSave::" << funcname << endl;
    }
    return ;
  }
