#include <VariableArrayInt.h>

ClassImp(VariableArrayInt)

using namespace std;

VariableArrayInt::VariableArrayInt(const unsigned int idval)
{
  id = idval;
  nVal = 0;
  sval = 0;
  return;
}

VariableArrayInt::~VariableArrayInt()
{
  Reset();
  return;
}

void
VariableArrayInt::identify(ostream &os) const
{
  os << "contain " << nVal << " values" << endl;
  for (unsigned int i = 0; i < nVal; i++)
    {
      os << "n: " << i << " val: " << sval[i] << endl;
    }
  return;
}

void
VariableArrayInt::set_val(const vector<int> &vec)
{
  nVal = vec.size();
  sval = new int[nVal];
  vector<int>::const_iterator iter;
  unsigned int i=0;
  for (iter = vec.begin(); iter != vec.end(); iter++)
    {
      sval[i++] = *iter;
    }
  return;
}

void
VariableArrayInt::Reset()
{
  delete [] sval;
  sval = 0;
  nVal = 0;
  return;
}
