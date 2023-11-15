#include <RunHeaderv3.h>

#include <cstdlib>
#include <iostream>

using namespace std;

ClassImp(RunHeaderv3)

RunHeaderv3::RunHeaderv3()
{
  currentInner = -9999;
  return;
}

void RunHeaderv3::Reset()
{
  // we don't want to reset the run header for each event
  // so just return here
  return;
}

void RunHeaderv3::identify(ostream& out) const
{
  out << "identify yourself: I am an RunHeaderv3 Object" << endl;
  identifyv3(out);
  return;
}

void RunHeaderv3::identifyv3(ostream& out) const
{
  identifyv2(out);
  if (abs(currentInner) < 2300)
    {
      out << "Inner Coil Magnet: Off" << endl;
    }
  else
    {
      out << "Inner Coil Magnet: " << currentInner << " Amps";
     if (currentInner > 0)
	{
	  out << " (Normal Field)" << endl;
	}
      else
	{
	  out << " (Reversed Field)" << endl;
	}
    }
  return;
}

