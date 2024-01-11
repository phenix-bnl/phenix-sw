#include <lpcEvent.hh>
#include <lpcMap.h>
#include <lpcRawv2.h>

#include <PHTimeStamp.h>
#include <getClass.h>
#include <Event.h>

#include <cstdlib>
#include <iostream>

using namespace std;

lpcEvent::lpcEvent()
{
  lpcraw = 0;
  memset(lpc_pmt_map,0,sizeof(lpc_pmt_map));
  packet_id[0] = 16001;
  packet_id[1] = 16002;  //{South, North}
}

int 
lpcEvent::setRawData (PHCompositeNode *topNode)
{
  Event *evt = findNode::getClass<Event>(topNode,"PRDF");
      if (!evt)
        {
          cout << PHWHERE << "NULL Event Pointer" << endl;
          return -1;
        }
  int iret = setRawData(evt);
  return iret;
}

int 
lpcEvent::setRawData (Event * event)
{
  if ( !lpcraw )
    {
      lpcraw = new lpcRawv2();
    }

  // Get the relevant packets from the Event object and transfer the
  // data to the subsystem-specific table.
  Packet *p;
  
  for (int iarm=0; iarm<2; iarm++)
    {
      p = event->getPacket(packet_id[iarm]);
      
      if ( p == 0 )
	{
	  return -1; 
	}
      else
	{
	  for (int pmt_number=0; pmt_number<2; pmt_number++)
	    {
	      // High gain
	      lpcraw->AddlpcRawHit( p->iValue(lpc_pmt_map[iarm][pmt_number],1),
				    p->iValue(lpc_pmt_map[iarm][pmt_number],3),
				    p->iValue(lpc_pmt_map[iarm][pmt_number],0),
				    pmt_number+2*iarm );
	      /*
	      // Low gain
	      lpcraw->AddlpcRawHit( p->iValue(lpc_pmt_map[iarm][pmt_number],2),
				    p->iValue(lpc_pmt_map[iarm][pmt_number],4),
				    p->iValue(lpc_pmt_map[iarm][pmt_number],0),
				    pmt_number+2*iarm );
	      */
// 	      cout << "lpc_pmt_map["<< iarm << "][" << pmt_number
// 		   << "]: " << lpc_pmt_map[iarm][pmt_number] << endl;
	    }
	  delete p;
	}
    }

  return 0;

}

void 
lpcEvent::setCalibDataAll (const PHTimeStamp & time)
{
  lpcMap lpcmap;
  unsigned int size;
  int *lpc_pmt_map_tmp = lpcmap.FillChannelMap(time,size);
  unsigned int n=0;
  for (int i=0;i<2;i++)
    {
      for (int j=0; j<2;j++)
	{
	  lpc_pmt_map[i][j] = lpc_pmt_map_tmp[n];
	  n++;
	}
    }
  if (n != size) 
    {
      cout << PHWHERE << " channel map mismatch detected, exiting" << endl;
      exit(1);
    }
  delete [] lpc_pmt_map_tmp;
}
