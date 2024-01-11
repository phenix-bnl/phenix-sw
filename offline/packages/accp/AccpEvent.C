
//INCLUDECHECKER: Removed this line: #include "phool.h"
//INCLUDECHECKER: Removed this line: #include "PHDataNode.h"
#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
//INCLUDECHECKER: Removed this line: #include "PHTypedNodeIterator.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"

#include "Event.h"
//INCLUDECHECKER: Removed this line: #include "Accp.h"
//INCLUDECHECKER: Removed this line: #include "AccpRaw.h"
#include "AccpRawv1.h"
#include "AccpEvent.h"

#include <iostream>

ClassImp(AccpEvent)

using namespace std;

typedef PHIODataNode<AccpRaw> AccpRawNode_t;
typedef PHDataNode<Event>     EventNode_t;

PHBoolean AccpEvent::setRawData (Event *event)
{
  Packet *p = NULL;

  if ( !accpraw )
    {
      accpraw = new AccpRawv1();
    }

  p = event->getPacket(ACCP_PACKET_ID);

  if ( p == 0 )
    return False;
  
  
  for (int i = 0; i < ACCP_NCH; i++)
    {	  
      accpraw->SetTdc(i, p->iValue(i,0));
      accpraw->SetAdcHPost (i, p->iValue(i,1));
      accpraw->SetAdcLPost (i, p->iValue(i,2));
      accpraw->SetAdcHPre (i, p->iValue(i,3));
      accpraw->SetAdcLPre (i, p->iValue(i,4));
    }
  
  delete p;
 
  return True;

}

int AccpEvent::event(PHCompositeNode *topNode)
{

  PHNodeIterator iter(topNode);
  Event *evt       = NULL;
  
  // PHTypedNodeIterator<AccpOut> accpiter(topNode);
  // need to put AccpOutNode

  EventNode_t *EventNode = 
    dynamic_cast<EventNode_t*> (iter.findFirst("PHDataNode","PRDF"));

  if (!EventNode)
    {
      cout << PHWHERE
           << "[ERROR] PRDF Node missing in AccpEvent" << endl;
      return 0;
    }
  
  evt = EventNode->getData();
  if (!evt)
    {
      cout << PHWHERE 
           << "[ERROR] NULL Event Pointer in AccpEvent" << endl;
      return 0;
    }


  PHTypedNodeIterator<AccpRaw> accprawiter(topNode);
  AccpRawNode_t *AccpRawNode = accprawiter.find("AccpRaw");
  if (AccpRawNode)
    {
      accpraw = AccpRawNode->getData();
      if (!accpraw)
        {
          cout << PHWHERE << " Unable to get AccpRaw, is Node missing?" << endl;
          return 1;
        }
    }
  else
    {
      cout << PHWHERE << " AccpRaw Node is missing" << endl;
      return 0;
    }
    
  setRawData (evt);

  return 1;

} 

int AccpEvent::print()
{
  cout << " channel : time : adchpost : adchpre : adclpost : adclpre " << endl;
  for(int i=0; i<ACCP_NCH; i++)
    {
      cout << i << " : " 
           << accpraw->GetTdc(i) << " : " 
           << accpraw->GetAdcHPost(i) << " : "
           << accpraw->GetAdcHPre(i) << " : "
           << accpraw->GetAdcLPost(i) << " : "
           << accpraw->GetAdcLPre(i) << endl;
    }

  return 1;
}
