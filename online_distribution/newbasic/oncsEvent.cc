#include <oncsEvent.h>
#include <oncsStructures.h>
#include <oncsCollection.h>
#include <oncsSubConstants.h>

// the constructor first ----------------
oncsEvent::oncsEvent (int *data)
{ 
  is_data_type = 0;
  EventData = (oncsevtdata_ptr) data;
}

oncsEvent::~oncsEvent ()
{ 
  if (is_data_type) delete [] (int *) EventData;
}

// the info-type calls
unsigned int 
oncsEvent::getEvtLength()
{
  return EventData->evt_length;
}

int 
oncsEvent::getEvtType()
{
  return EventData->evt_type;
}

int 
oncsEvent::getEvtSequence()
{
  return EventData->evt_sequence;
}

int 
oncsEvent::getRunNumber()
{
  return EventData->run_number;
}

// PHTimeStamp *
// oncsEvent::getTimeStamp() const
// {
//   return 0;
// }


void oncsEvent::identify (OSTREAM &os) const
{ 
  os << std::dec << " -- Event " << SETW(5)  << EventData->evt_sequence;

  os << " Run: "  << SETW(5)  << EventData->run_number;

  os << " length: "     << SETW(5)  <<EventData->evt_length;

  os << " type: "      << SETW(2)  << EventData->evt_type 
     << " (" << get_evt_mnemonic(EventData->evt_type) << ")"
     <<  std::endl;


};

int oncsEvent::convert()
{
  if (is_data_type) return -1;

  int *tmp;

  tmp = new int[getEvtLength()];
  int *from= (int *)  EventData;
  int *to=tmp;
  for (unsigned int k=0; k< getEvtLength(); k++) 
    { 
      *to++ = *from++;
    }

  EventData = (oncsevtdata_ptr) tmp;

  is_data_type = 1;
  return 0;

}

int oncsEvent::is_pointer_type() const
{
  if (is_data_type) return 0;
  else return 1;
}


Packet* 
oncsEvent::getPacket (const int id, const int hitFormat)
{
  return getPacket (id);
}

// getSubevent (int)
Packet* 
oncsEvent::getPacket (const int id)
{
  int i;
  subevtdata_ptr sevt_ptr;

  int  datalength = EventData->evt_length - EVTHEADERLENGTH;

  // loop through the subevents and see if we locate the id

  for (i=0; i<datalength; i+=  EventData->data[i])
    { 

      // each data[i] is the start of a subevent;
      // we map it on a subevent_ptr

      sevt_ptr = (subevtdata_ptr) &EventData->data[i];

      // now we see what type of subevent we are supposed
      // to return

      if ( sevt_ptr->sub_id == id) 
	{
	  switch ( sevt_ptr->sub_decoding )
	    {
	    case (ID4EVT):
	      return new 
		oncsSub_id4evt( sevt_ptr);
	      break;
      
	    case (ID2EVT):
	      return new 
		oncsSub_id2evt( sevt_ptr );
	      break;
      
	    case (IDCSTR):
	      return new 
		oncsSub_idcstr( sevt_ptr );
	      break;
      
	    case (IDHAMMOND):
	      return new 
		oncsSub_idhammond( sevt_ptr );
	      break;
      
	    case (IDSAM):
	      return new 
		oncsSub_idsam( sevt_ptr );
	      break;
      
	    case (IDDCFEM):
	      return new 
		oncsSub_iddcfem( sevt_ptr );
	      break;

	    case (IDTECFEM):
	      return new 
		oncsSub_idtecfem( sevt_ptr );
	      break;
      
	    case (IDMIZNHC):
	      return new 
		oncsSub_idmiznhc( sevt_ptr );
	      break;
      
	    case (IDSIS3300):
	      return new 
		oncsSub_idsis3300( sevt_ptr );
	      break;
      
	    case (IDCAENV792):
	      return new 
		oncsSub_idcaenv792( sevt_ptr );
	      break;
      
      	    case (IDFIFOBOARD):
	      return new 
		oncsSub_idfifoboard( sevt_ptr );
	      break;
      
      
      	    case (IDRCPETDATA):
	      return new 
		oncsSub_idrcpetdata( sevt_ptr );
	      break;
      
      
      	    case (IDBSPETDATA):
	      return new 
		oncsSub_idbspetdata( sevt_ptr );
	      break;
      

	    default:
	      switch (sevt_ptr->sub_type)
		{
		case 1:
		  return  new oncsSubevent_w1(sevt_ptr);
		  break;
		case 2:
		  return new oncsSubevent_w2(sevt_ptr);
		  break;
		case 4:
		  return new oncsSubevent_w4(sevt_ptr);
		  break;
		default:
		  return new oncsSubevent_w4(sevt_ptr); 
		}
	      
	      return 0;
	    }
	}
    }
  return 0;
}

int 
oncsEvent::getPacketList( Packet* sl[], const int ne)
{
  int i,entries;
  subevtdata_ptr sevt_ptr;

  int  datalength = EventData->evt_length - EVTHEADERLENGTH;

  entries = 0;
  // loop through the subevents and see if we locate the id

  for (i=0; i<datalength; i+=  EventData->data[i])
    { 

      // each data[i] is the start of a subevent;
      // we map it on a subevent_ptr

      sevt_ptr = (subevtdata_ptr) &EventData->data[i];
      switch ( sevt_ptr->sub_decoding )
	{
	case (ID4EVT):
	  sl[entries++] = 
	    new oncsSub_id4evt( sevt_ptr);
	  break;

	case (ID2EVT):
	  sl[entries++] = 
	    new oncsSub_id2evt( sevt_ptr );
	  break;
      
	case (IDCSTR):
	  sl[entries++] = 
	    new oncsSub_idcstr( sevt_ptr );
	  break;
      
	case (IDHAMMOND):
	  sl[entries++] = 
	    new oncsSub_idhammond( sevt_ptr );
	  break;
	  
	case (IDSAM):
	  sl[entries++] = 
	    new oncsSub_idsam( sevt_ptr );
	  break;
	  

	case (IDDCFEM):
	  sl[entries++] = 
	    new oncsSub_iddcfem( sevt_ptr );
	  break;

	case (IDTECFEM):
	  sl[entries++] = 
	    new oncsSub_idtecfem( sevt_ptr );
	  break;
      
	case (IDMIZNHC):
	  sl[entries++] = 
	    new oncsSub_idmiznhc( sevt_ptr );
	  break;
      

	case (IDSIS3300):
	  sl[entries++] = 
	    new oncsSub_idsis3300( sevt_ptr );
	  break;

	case (IDCAENV792):
	  sl[entries++] = 
	    new oncsSub_idcaenv792( sevt_ptr );
	  break;
      
	case (IDFIFOBOARD):
	  sl[entries++] = 
	    new oncsSub_idfifoboard( sevt_ptr );
	  break;
      
	case (IDRCPETDATA):
	  sl[entries++] = 
	    new oncsSub_idrcpetdata( sevt_ptr );
	  break;
      
	case (IDBSPETDATA):
	  sl[entries++] = 
	    new oncsSub_idbspetdata( sevt_ptr );
	  break;
            
	default:

      switch (sevt_ptr->sub_type)
	{
	case 1:
	  sl[entries++] = new oncsSubevent_w1(sevt_ptr);
	  break;
	case 2:
	  sl[entries++] = new oncsSubevent_w2(sevt_ptr);
	  break;
	case 4:
	  sl[entries++] = new oncsSubevent_w4(sevt_ptr);
	  break;
	default:
	  sl[entries++]= new oncsSubevent_w4(sevt_ptr); 
	}


	}

    }
  return entries;
}




// existSubevent (const int)

int  
oncsEvent::existPacket (const int id)
{
  int i;
  subevtdata_ptr sevt_ptr;

  int  datalength = EventData->evt_length - EVTHEADERLENGTH;

  // loop through the subevents and see if we locate the id

  for (i=0; i<datalength; i+=  EventData->data[i])
    { 

      // each data[i] is the start of a subevent;
      // we map it on a subevent_ptr

      sevt_ptr = (subevtdata_ptr) &EventData->data[i];

      // now we see what type of subevent we are supposed
      // to return

      if ( sevt_ptr->sub_id == id) return 1;

    }
  return 0;
}


// the Copy routine
int  
oncsEvent::Copy (int * array, const unsigned int length, int *nw, const char *what)
{
  if (length<EventData->evt_length)
    {
      *nw = 0;
      return -1;
    }
  int *to = array;
  int *from= (int *) EventData;
  for (unsigned int i=0; i<EventData->evt_length; i++) *to++ = *from++;
  *nw=EventData->evt_length;
  return 0;
}
