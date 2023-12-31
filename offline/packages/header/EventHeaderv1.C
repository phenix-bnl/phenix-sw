#include "EventHeaderv1.h"

using namespace std;

ClassImp(EventHeaderv1)

EventHeaderv1::EventHeaderv1()
{
  Reset();
  return;
}

void EventHeaderv1::Reset()
{
  TimeStamp = 0;
  EvtSequence = -999999;
  EvtType = -999999;
  return;
}

void EventHeaderv1::identify(ostream& out) const
{
  out << "identify yourself: I am an EventHeaderv1 Object" << endl;
  out << "Event no: " << EvtSequence 
      << ", Type: " << EvtType 
      << ", ATP arrival time: " << ctime(&TimeStamp)
      << endl;

  return;
}

int EventHeaderv1::isValid() const
{
  return((TimeStamp) ? 1:0); // return 1 if TimeStamp is not zero
}

