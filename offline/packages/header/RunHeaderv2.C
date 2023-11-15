#include <RunHeaderv2.h>

#include <cstdlib>
#include <iostream>

using namespace std;

ClassImp(RunHeaderv2)

  RunHeaderv2::RunHeaderv2()
{
  RunNumber = 0;
  TimeStart = 0;
  TimeStop = 0;
  currentNorth = -9999;
  currentSouth = -9999;
  currentCentral = -9999;
  return ;
}

void RunHeaderv2::Reset()
{
  // we don't want to reset the run header for each event
  // so just return here
  return ;
}

void RunHeaderv2::identifyv2(ostream& out) const
{
  out << "Run no: " << RunNumber << endl;
  out << "Started at: " << ctime(&TimeStart);
  out << "Ended at:   " << ctime(&TimeStop);
  out << "Magnet Currents: " << endl;

  if (abs(currentCentral) < 1600)
    {
      out << "Central Magnet: Off" << endl;
    }
  else
    {
      out << "Central Magnet: " << currentCentral << " Amps";
      if (currentCentral > 0)
	{
	  out << " (Normal Field)" << endl;
	}
      else
	{
	  out << " (Reversed Field)" << endl;
	}
    }
  if (abs(currentSouth) < 2200)
    {
      out << "South Muon Magnet: Off" << endl;
    }
  else
    {
      out << "South Muon Magnet: " << currentSouth << " Amps";
      if (currentSouth > 0)
	{
	  out << " (Normal Field)" << endl;
	}
      else
	{
	  out << " (Reversed Field)" << endl;
	}
    }
  if (abs(currentNorth) < 2800)
    {
      out << "North Muon Magnet: Off" << endl;
    }
  else
    {
      out << "North Muon Magnet: " << currentNorth << " Amps";
      if (currentNorth > 0)
	{
	  out << " (Normal Field)" << endl;
	}
      else
	{
	  out << " (Reversed Field)" << endl;
	}
    }
  return ;
}

void RunHeaderv2::identify(ostream& out) const
  {
    out << "identify yourself: I am an RunHeaderv2 Object" << endl;
    identifyv2(out);
    return ;
  }

int RunHeaderv2::isValid() const
  {
    return ((TimeStart) ? 1 : 0); // return 1 if TimeStart not zero
  }
