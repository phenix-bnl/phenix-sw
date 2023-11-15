#include "PreviousEventv1.h"

using namespace std;

ClassImp(PreviousEventv1)

PreviousEventv1::PreviousEventv1()
{
  Reset();
  return ;
}

void
PreviousEventv1::Reset()
{
  for (int i = 0; i < 3;i++)
    {
      clockticks[i] = -1;
    }
  return ;
}

void
PreviousEventv1::identify(ostream& out) const
{
  out << "identify yourself: I am an PreviousEventv1 Object" << endl;
  for (int i = 0; i < 3; i++)
    {
      if (clockticks[i] >= 0)
	{
	  out << " delta of N-" << (i + 1) << " Event from this event: "
	      << clockticks[i] << " clockticks, "
	      << (clockticks[i]*0.106) << " microseconds)" << endl;
	}
    }
  return ;
}

int
PreviousEventv1::isValid() const
{
    return ((clockticks[0] > -1) ? 1 : 0);
}

