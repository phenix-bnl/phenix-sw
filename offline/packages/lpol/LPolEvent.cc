#include <LPolEvent.hh>
#include <LPolMap.h>
#include <LPolRawv1.h>

#include <PHTimeStamp.h>
#include <getClass.h>
#include <Event.h>

#include <cstdlib>
#include <iostream>

using namespace std;

LPolEvent::LPolEvent()
{
  lpolraw = 0;
  memset(lpol_pmt_map,0,sizeof(lpol_pmt_map));
  packet_id = 13001;
  // packet_id[0] = 16001;
  //  packet_id[1] = 16002;  //{South, North}
}

int 
LPolEvent::setRawData (PHCompositeNode *topNode)
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
LPolEvent::setRawData (Event * event)
{
  if ( !lpolraw )
    {
      lpolraw = new LPolRawv1();
    }

  // Get the relevant packets from the Event object and transfer the
  // data to the subsystem-specific table.
  Packet *p;

  p = event->getPacket(packet_id);
      
  if ( p == 0 )
    {
      return -1; 
    }
  else
    {
      for (int pmt_number=0; pmt_number<POL_N_PMT; pmt_number++)
	{
	  // High gain
	  lpolraw->AddLPolRawHit( p->iValue(lpol_pmt_map[pmt_number]),
				  p->iValue(lpol_pmt_map[pmt_number],"T1"),
				  p->iValue(lpol_pmt_map[pmt_number],"T2"),
				  pmt_number);
	  /*
	  // Low gain
	  lpolraw->AddLPolRawHit( p->iValue(lpol_pmt_map[iarm][pmt_number],2),
	  p->iValue(lpol_pmt_map[iarm][pmt_number],4),
	  p->iValue(lpol_pmt_map[iarm][pmt_number],0),
	  pmt_number+2*iarm );
	  */
	  // 	      cout << "lpol_pmt_map["<< iarm << "][" << pmt_number
	  // 		   << "]: " << lpol_pmt_map[iarm][pmt_number] << endl;
	}
      delete p;
    }
  

  return 0;

}

void 
LPolEvent::setCalibDataAll (const PHTimeStamp & time)
{
  LPolMap lpolmap;
  unsigned int size;
  int *lpol_pmt_map_tmp = lpolmap.FillChannelMap(time,size);
  unsigned int n=0;
  for (int i=0;i<4;i++)
    {
      //      lpol_pmt_map[i] = lpol_pmt_map_tmp[i];
      lpol_pmt_map[i] = 40+i; //Based on channel mapping in the local pol wiki. 2015.06.25 minjung

      n++;
    }
  if (n != size) 
    {
      cout << PHWHERE << " channel map mismatch detected, exiting" << endl;
      exit(1);
    }
  delete [] lpol_pmt_map_tmp;
}
