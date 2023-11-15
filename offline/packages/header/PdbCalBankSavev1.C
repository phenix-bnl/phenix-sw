#include "PdbCalBankSavev1.h"
#include <phool.h>
#include <iostream>

ClassImp(PdbCalBankSavev1)

using namespace std;

void
PdbCalBankSavev1::Reset()
{
  return ;
}

void
PdbCalBankSavev1::identify(ostream& os) const
{
  os << "PdbCalBankSavev1 identify:" << endl;
   map<string,set<int> >::const_iterator bankiter;
   for (bankiter = banksave.begin(); bankiter != banksave.end(); ++bankiter)
     {
       os << "BankName: " << bankiter->first << endl;
       set<int>:: const_iterator siter;
       for (siter = (bankiter->second).begin(); siter != (bankiter->second).end(); ++siter)
 	{
 	  os << "rid: " << *siter << endl;
 	}
     }

  return ;
}

int
PdbCalBankSavev1::isValid() const
{
  if (banksave.empty())
    {
      return 0;
    }
  return 1;

}

int
PdbCalBankSavev1::AddBank(const std::string &name, const int &rid)
{
  map<string, set<int> >::iterator bankiter = banksave.find(name);
  if (bankiter != banksave.end())
    {
      (bankiter->second).insert(rid);
    }
  else
    {
      set<int> newbank;
      newbank.insert(rid);
      banksave[name] = newbank;
    }

  return 0;
}
