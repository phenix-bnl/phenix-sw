#include "BadEvent.h"

using namespace std;

BadEvent::BadEvent()
{
  runno = 0;
  eventno = 0;
  segmentid = -1;
  trigraw = 0;
  triglive = 0;
  trigscaled = 0;
  crossing = -1;
  reason = "";
  cvstag = "";
  tickstolastevent = 0;
}

void
BadEvent::identify(ostream &os) const
{
  os << "Run: " << Run() 
     << ", Event: " << Event()
     << ", Segment: " << SegmentId()
     << hex
     << ", RawTrig: 0x" << TrigRaw()
     << ", LiveTrig: 0x" << TrigLive()
     << ", ScaledTrig: 0x" << TrigScaled() 
     << dec
     << ", Crossing: " << Crossing() 
     << ", Reason: " << Reason() 
     << ", CvsTag: " << CvsTag() 
     << ", Ticks to last evt: " << TicksToLastEvent() 
     << endl;
  return;
}
