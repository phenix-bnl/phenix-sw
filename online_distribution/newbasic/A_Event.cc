#include <A_Event.h>

#include <EventTypes.h>

#include <packet_collection.h>

#include <packet.h>
#include <Cframe.h>
#include <framePackets.h>
#include <dataBlock.h>

#include <frameRoutines.h>
#include <frameHdr.h>


// the constructor first ----------------
A_Event::A_Event (PHDWORD *data)
{ 
  // set the framlist pointer to 0 just in case.
  framelist = 0;


  hasMap = 0;
  // we always make a pointer-based event.
  is_data_type = 0;
  errorcode = 0;

  // cast the pointer to the EventData pointer
  EventData = (evtdata_ptr) data;
  if ( getEvtType() & CORRUPTEVENTMASK)
    {
      errorcode=1;
    }
  else
    {
      errorcode = updateFramelist();
    }
}

A_Event::A_Event (int *data)
{ 
  // set the framlist pointer to 0 just in case.
  framelist = 0;

  hasMap = 0;
  // we always make a pointer-based event.
  errorcode = 0;
  is_data_type = 0;
  
  // cast the pointer to the EventData pointer
  EventData = (evtdata_ptr) data;
  if ( getEvtType() & CORRUPTEVENTMASK)
    {
      errorcode=1;
    }
  else
    {
      errorcode = updateFramelist();
    }

}

A_Event::~A_Event()
{
  delete [] framelist;
  if (is_data_type) delete [] (PHDWORD *) EventData;
  pmap.clear();

}


// the info-type calls
unsigned int 
A_Event::getEvtLength()
{
  return EventData->evt_length;
}

int 
A_Event::getEvtType()
{
  return EventData->evt_type;
}

int 
A_Event::getEvtSequence()
{
  return EventData->evt_sequence;
}

int 
A_Event::getRunNumber()
{
  return EventData->run_number;
}

#ifdef WIN32
const ULONGLONG  ticOffset = 35067168000000000UL;
const ULONGLONG  ticFactor = 10000000;
#else
const  unsigned long long  ticOffset = 35067168000000000ULL;
const   unsigned long long ticFactor = 10000000;
#endif


time_t A_Event::getTime() const 
{
  if ( EventData->time == -1)
     {
       return (time_t) EventData->date;
     }
  else
    {

      unsigned long long  x,y,z;
      x = (unsigned int)  EventData->time;
      y = (unsigned int)  EventData->date;
      z = y | ( x<<32 );
      time_t t =  (z - ticOffset) / ticFactor;
      return t;
    }
}

// PHTimeStamp *
// A_Event::getTimeStamp() const 
// {

//   //  COUT << "time is " << std::hex << EventData->time << std::endl;
//   //  COUT << "date is " << std::hex << EventData->date << std::dec << std::endl;

//   if ( EventData->time == -1)
//     {

//       PHTimeStamp *p = new  PHTimeStamp( (time_t) EventData->date );
//       PHTimeStamp *b = new  PHBigTimeStamp( *p, EventData->run_number,  EventData->evt_sequence );
//       delete p;
//       return b;
//     }
//   else
//     {
//       phtime_t x,y,z;
//       x = (unsigned int)  EventData->time;
//       y = (unsigned int)  EventData->date;
//       z = y | ( x<<32 );
//       PHTimeStamp *p = new  PHTimeStamp(0);
//       p->setBinTics(z);
//       //     COUT << "time stamp is " << std::hex << z << std::dec << std::endl; 
//       return p;
//     }
  
// }


void A_Event::listFrame ( const int id, OSTREAM &os) const
{
  PHDWORD *fp;
  PHDWORD *pp;
  int i = 0;
  int j = 0;

  if (id == 0) 
    {
      
      while ( ( fp = framelist[i++]) )
	{
	  dumpFrame(fp, os);
	  //os << "Frame " << ++j << "align length: "<< getFrameAlignLength(fp) << std::endl;
	}
    }
  else // find the frame for a given packet
    {
      
      while ( (fp = framelist[i++]) )
	{
	  j++;
	  if ( ( pp = findFramePacketId (fp, id) ) !=  ptrFailure) 
	    {
	      dumpFrame(fp, os);
	      //os << "Frame " << j << "align length: "<< getFrameAlignLength(fp) << std::endl;
	      return;
	    }
	}
    }
}

void
A_Event::listHistory(const int id, OSTREAM &os) const
{
  os << "History Block: " << std::endl;

  int i = 0;
  if (id == 0) 
    {      
      while ( PHDWORD* fp = framelist[i++] ) 
	{
	  PHDWORD* h_ptr = findFrameHistoryStart(fp);
	  UINT len = getFrameHistoryLength(fp);
	  dumpBlock(h_ptr, len, os);
	}
    }
  else // find the frame for a given packet
    {      
      int j = 0;
      while ( PHDWORD* fp = framelist[i++] )
	{
	  j++;
	  if ( findFramePacketId(fp, id) != ptrFailure ) 
	    {
	      PHDWORD* h_ptr = findFrameHistoryStart(fp);
	      UINT len = getFrameHistoryLength(fp);
	      dumpBlock(h_ptr, len, os);
	      return;
	    }
	}
    }
  return;
}

void
A_Event::listError( const int id, OSTREAM& os ) const
{
  os << "Error Block: " << std::endl;
  int i = 0;
  if (id == 0) 
    {      
      while ( PHDWORD* fp = framelist[i++] ) 
	{
	  PHDWORD* ptr = findFrameErrorStart(fp);
	  UINT len = getFrameErrorLength(fp);
	  dumpBlock(ptr, len, os);
	  dumpErrorBlock(fp,os);
	}
    }
  else // find the frame for a given packet
    {      
      int j = 0;
      while ( PHDWORD* fp = framelist[i++] )
	{
	  j++;
	  if ( findFramePacketId(fp, id) != ptrFailure ) 
	    {
	      PHDWORD* ptr = findFrameErrorStart(fp);
	      UINT len = getFrameErrorLength(fp);
	      dumpBlock(ptr, len, os);
	      return;
	    }
	}
    }
}

void A_Event::dumpFrame(PHDWORD *fp, OSTREAM &os)
{
  // DLW: for SEQ number and code, there is no corresponding routine in Cframe.h, 
  // so I directly unpack the field.  This will break if the frame format ever changes...
  //
  os << "Frame length:       " << std::dec << getFrameLength(fp) << std::endl;
  os << "Frame mark:         " << std::hex << getFrameMark(fp) << std::dec << std::endl;
  os << "Frame Hdr version:  " << getFrameHdrVersion(fp) << std::endl;
  os << "Frame Hdr length:   " << getFrameHdrLength(fp) << std::endl ;
  os << "Frame Status:       " << getFrameStatus(fp) << std::endl;
  os << "Frame Seq Number:   " << (((*(fp+3))&0xff000000)>>24) << std::endl;
  os << "Frame Seq Code:     " << (((*(fp+3))&0x00ff0000)>>24) << std::endl;
  os << "Frame Source id:    " << getFrameSourceId(fp) << std::endl;
  os << "Frame data type:    " << getFrameDataType(fp) << std::endl;
  os << "Frame type:         " << getFrameType(fp) << std::endl;
  os << "Frame Error Length: " << getFrameErrorLength(fp) << std::endl;
  os << "Frame Hist Length:  " << getFrameHistoryLength(fp) << std::endl;
  os << "Frame align length: " << getFrameAlignLength(fp) << std::endl;
  os << "Frame padding:      " << getFramePadding(fp) << std::endl;
  unsigned int i = 0;
  PHDWORD *p = findFrameAlignBlock(fp);
  for (i = 0; i<  getFrameAlignLength(fp); i++ )
    {
      os << "  - Alignment word " << SETW(2) << i << ":  0x" ; 

      os.fill('0');
      os << SETW(8) << std::hex << *p++ << std::dec << std::endl;  
      os.fill (' ');
    }
  os << std::endl;
}

void
A_Event::dumpErrorBlock(PHDWORD *fp, OSTREAM &os)
{
  PHDWORD* ptr = findFrameErrorStart(fp);
  UINT len = getFrameErrorLength(fp);
  UINT nerr = calcNumErrorsV1(len);
  if ( nerr == 0 ) return;

  errorEntryV1* p = reinterpret_cast<errorEntryV1*>(ptr);
  for (UINT i=0; i<nerr; ++i)
    {
      errorEntryV1& e = *p;
      os << "ErrorEntry " << i << ": ";
      os << "severity: " << (int) e.severity << " "
	 << "deviceType: " << (int)e.deviceType << " "
	 << "deviceId: " << std::dec << e.deviceId << " "
	 << "errorCode: " << e.errorCode << " "
	 << "detectCode: " << e.detectCode << " "
	 << "addData: (" << std::hex << e.addData[0] << "," << std::hex << e.addData[1] << ")"
	 << std::dec
	 << std::endl;
      p++;
    }
  os << std::endl;
}

void
A_Event::dumpBlock(PHDWORD* p, UINT len, OSTREAM& os, const int how)
{
  if ( len == 0 ) { 
    os << "   (empty)\n" << std::endl;
    return; 
  }

  unsigned int j;
  switch (how)
    {
    case (EVT_HEXADECIMAL):
      j = 0;
      while (1)
	{
	  os << SETW(5) << j << " |  ";
	  for ( UINT l=0; l<4; l++ )
	    {
	      if ( j >= len ) break;
	      os << std::hex << SETW(8) << p[j++] << " " ;
	    }
	  if ( j >= len ) break;
	  os << std::dec << std::endl;
	}
      break;
		
    case (EVT_DECIMAL):
      j = 0;
      while (1)
	{
	  os << std::dec << SETW(5) << j << " |  ";
			 
	  for ( UINT l=0; l<6; l++ )
	    {
	      os << SETW(10) << p[j++] << " ";
	      if ( j >= len ) break;
	    }
	  if ( j >= len ) break;
	  os << std::endl;
	}
      break;
	 
    default: 
      break;
    }
  os << std::endl << std::endl;

}

unsigned int A_Event::getFrameEntry(const char *what, const int id, const int index) const
{
  
  PHDWORD *fp;
  PHDWORD *pp;
  int i = 0;
  int j = 0;

  if (id == 0) 
    {
      
      while ( ( fp = framelist[i++]) )
	{
	  return getFrameValue( what,fp,index);
	}
    }
  else // find the frame for a given packet
    {
      
      while ( (fp = framelist[i++]) )
	{
	  j++;
	  if ( ( pp = findFramePacketId (fp, id) ) !=  ptrFailure) 
	    {
	      return getFrameValue( what,fp,index);
	    }
	}
    }
  return 0;
}




unsigned int A_Event::getFrameValue(const char *what, PHDWORD *fp, const unsigned int index) const
{

  if ( strcmp(what,"FRAMELENGTH") == 0)     return getFrameLength(fp);
  else if  ( strcmp(what,"FRAMEMARK") ==0)  return  getFrameMark(fp);
  else if  ( strcmp(what,"FRAMEHDRVERSION") == 0)  return getFrameHdrVersion(fp);
  else if  ( strcmp(what,"FRAMEHDRLENGTH") == 0)  return  getFrameHdrLength(fp);
  else if  ( strcmp(what,"FRAMESTATUS") == 0) return  getFrameStatus(fp);
  else if  ( strcmp(what,"FRAMESOURCEID") ==0 )return  getFrameSourceId(fp);
  else if  ( strcmp(what,"FRAMEDATATYPE") == 0) return getFrameDataType(fp);
  else if  ( strcmp(what,"FRAMETYPE") == 0) return  getFrameType(fp);
  else if  ( strcmp(what,"FRAMEALIGNLENGTH") == 0) return getFrameAlignLength(fp);
  else if  ( strcmp(what,"FRAMEALIGNMENTWORD") == 0)
    {
      PHDWORD *p = findFrameAlignBlock(fp);
      if ( index >= getFrameAlignLength(fp) ) return 0;
      return p[index];
    }  

  return 0;
}


// getSubevent (int)
Packet* 
A_Event::getPacket (const int id)
{
  return getPacket(id,0);
}

#if !defined(SunOS) && !defined(OSF1)

int A_Event::createMap()
{
  int i = 0;
  PHDWORD *fp;
  PHDWORD *pp;

  if ( ! framelist ) 
    {
      errorcode = -3;
    }

  if (errorcode)  return 0;

  unsigned int pos_in_event;

  if (!hasMap)
    {
      
      while ( (fp = framelist[i++]) )
	{
	  pp = findFramePacketIndex (fp, 0);
	  

	  while ( pp  !=  ptrFailure) 
	    {

	      pos_in_event = ( int )(  pp - ( PHDWORD *) EventData );
	      
	      //	      std::cout << "pos in event = " << pos_in_event <<
	      //	"  packet length = " << *pp << " ev. length= " <<   getEvtLength() << std::endl;

	      if (pp && *pp > getEvtLength() - pos_in_event ) 
		{
		  std::cout << "Found wrong packet length " << *pp
			    << std::hex << "(0x" << *pp << ")" << std::dec 
			    << " packet Id: " <<  getPacketId(pp)  
			    << " Event: " << getEvtSequence() 
			    << " EvtLength: " << getEvtLength()
			    << " PosInEvent: " << pos_in_event
			    << std::endl;
		  errorcode =-2;
		  break;
		}
	      if ( pp != 0 && *pp == 0) 
		{
		  std::cout << "found 0-length packet" << std::endl;
		  errorcode =-1;
		  break;
		}


	      pmap[getPacketId(pp)] = pp;
	      // std::cout << "Packet id " << getPacketId(pp) << std::endl;

	      pp =  findNextFramePacket(fp, pp);

	    }
	}
      hasMap = 1;
    }
  return 0;
}


Packet* 
A_Event::getPacket (const int id, const int hitFormat)
{
  
  PHDWORD *pp;


  if (!hasMap) createMap();
  if ( errorcode) return 0;


  pp = pmap[id];
  if (!pp) return 0;

  return makePacket(pp,hitFormat);
}

#else

// the STL-Free solaris version

Packet* 
A_Event::getPacket (const int id, const int hitFormat)
{
  
  int i = 0;
  PHDWORD *fp;
  PHDWORD *pp;
  UINT ids = id;
  int wanted_hitformat;

  while ( fp = framelist[i++] )
    {
      if ( ( pp = findFramePacketId (fp, ids) ) !=  ptrFailure) 
	{
	  return makePacket(pp,hitFormat);
	}
    }
  return 0;
}
#endif

int 
A_Event::getPacketList( Packet* sl[], const int ne)
{
  int i = 0;
  PHDWORD *fp;
  PHDWORD *pp;

  if (!hasMap) createMap();
  if ( errorcode) return 0;

  int entries = 0;
  
  while ( (fp = framelist[i++]) )
    {

      pp = findFramePacketIndex (fp, 0);

      while ( pp  !=  ptrFailure) 
	{
	  if (getPacketStructure(pp) == Unstructured)
	    {
		  sl[entries++] = makePacket(pp,0);
		  //  sl[entries-1]->identify();
		  
	    }
	  
	  if (entries >= ne) return ne;
	  if ( (pp =  findNextFramePacket(fp, pp)) == ptrFailure)
            {
              break;
            }
	  if (*pp > getEvtLength()) 
	    {
	      std::cout << "Found wrong packet length " << *pp << std::endl;
	      break;
	    }
	  if ( pp != 0 && *pp == 0) 
	    {
	      std::cout << "found 0-length packet" << std::endl;
	      //  PHDWORD *x = pp - 10;
	      // std::cout << "--------------------------" << std::endl;
	      // for (i=0; i< 20; i++)
	      //	{
	      //	  std::cout << i << "  " << x << "  " << std::hex <<*x++ << std::dec << std::endl;
	      //	}
	      // std::cout << "--------------------------" << std::endl;
 

	      break;
	    }
	}
    }
  return entries;
}




Packet *A_Event::makePacket(PHDWORD *pp, const int hitFormat)
{

  int wanted_hitformat;

  if (getPacketStructure(pp) != Unstructured) return 0;
	  

  if (hitFormat)  wanted_hitformat = hitFormat;
  else wanted_hitformat  = getUnstructPacketHitFormat(pp);

  switch (wanted_hitformat)
    {

      // pbsc "32 channel format"

    case IDPBSC_DCMS:
    case IDPBGL_DCMS:
      return new Packet_emc_dcms(pp);
      break;

    case IDEMC_DCM32:
    case IDPBGL_DCM32: 
      return new Packet_emc_dcm32(pp);
      break;

    case IDPBSC_DCMZS:
    case IDPBGL_DCMZS:
      return new Packet_emc_dcm0(pp);
      break;

      // Sergei, don't take this out without asking me
    case IDEMC_FPGA:
    case IDEMC_FPGA0SUP:
    case IDFCAL_FPGA: 
    case IDFCAL_FPGA0SUP:
    case IDEMC_SHORTREFERENCE:
    case IDEMC_SHORTREFERENCE0SUP:

      return new Packet_emc_fpga(pp);
      break;

    case IDEMC_FPGASHORT: 
    case IDEMC_FPGASHORT0SUP:

      return new Packet_emc_fpgashort(pp);
      break;

     
    case IDEMC_FPGA3WORDS: 
    case IDEMC_FPGA3WORDS0SUP:

      return new Packet_emc_fpga3words(pp);
      break;

     
    case IDFCAL_FPGA3:
    case IDFCAL_FPGA0SUP3:
      return new Packet_fcal_fpga(pp);
      break;
      
      // This will give you a compatibility packet
      // designed to bring the 192 channels as before. 
    case IDEMC_OLDSTYLE:
      return new Packet_emc_oldstyle(pp);
      break;

    case IDEMC_REFERENCE:
    case IDEMC_REFERENCE0SUP:
      return new Packet_emc_reference(pp);
      break;


    case IDTOF_DCM16:
      return new Packet_tof_dcm16(pp);
      break;

    case IDMUTC_DCM3:
      return new Packet_mutc_dcm3(pp);
      break;

    case IDBBC_FPGA0SUP:
    case IDBBC_FPGA:
      return new Packet_bbc_fpga(pp);
      break;

    case IDTOF_FPGA0SUP:
    case IDTOF_FPGA:
    case IDTOFW_FPGA0SUP:
    case IDTOFW_FPGA:
      return new Packet_tof_fpga(pp);
      break;

    case IDMUTC_FPGA0SUP:
    case IDMUTC_FPGA:
      return new Packet_mutc_fpga(pp);
      break;

    case IDMUTC_15_FPGA:
      return new Packet_mutc_15_fpga(pp);
      break;

    case IDMUTC_FPGASHORT:
    case IDMUTC_FPGASHORTSUP:
      return new Packet_mutc_fpgashort(pp);
      break;

    case IDMUTC_FPGANEW:
    case IDMUTC_FPGANEWSUP:
    case IDMUTC_FPGANEWSUPV6:
      return new Packet_mutc_fpganew(pp);
      break;

    case IDPC_FPGA0SUP:
      return new Packet_pc_fpga(pp);
      break;

    case IDPC_FPGA:
      return new Packet_pc_dcm3(pp);
      break;

    case IDMVD_FPGA:
      return new Packet_mvd_fpga(pp);
      break;

    case IDMVD_FPGA0SUP:
    case IDMVD_PED_FPGA0SUP:
      return new Packet_mvd_fpga(pp);
      break;

    case IDRICH_FPGA0SUP:
    case IDRICH_FPGA:
      return new Packet_rich_fpga(pp);
      break;

    case IDMUID_FPGA:
    case IDMUID_FPGA0SUP:
    case IDEMCRICH_LL1:
      return new Packet_muid_fpga(pp);
      break;

    case IDZDC_FPGA:
    case IDZDC_FPGA0SUP:
      return new Packet_zdc_fpga(pp);
      break;

    case 50400:
    case IDHBD_FPGA:
    case IDHBD_FPGA0SUP:
    case IDFOCAL_FPGATEST:
      return new Packet_hbd_fpga(pp);
      break;

    case IDHBD_FPGASHORT:
    case IDHBD_FPGASHORT0SUP:
      return new Packet_hbd_fpgashort(pp);
      break;

    case IDHBD_FPGA3SAMPLES:
    case IDHBD_FPGA3SAMPLES0SUP:
      return new Packet_hbd_fpga3samples(pp);
      break;


    case IDRXNP_FPGASHORT:
    case IDRXNP_FPGASHORT0SUP:
      return new Packet_rxnp_fpga(pp);
      break;
      
    case IDPXL_DCM0:
    case IDVTXP_FPGA:
      return new Packet_vtxp_fpga(pp);
      break;

    case IDVTXS_FPGA:
      return new Packet_vtxs_fpga(pp);
      break;

    case IDMPCEX_FPGA:
    case IDMPCEX_FPGA0SUP:
      return new Packet_mpcextest_fpga(pp);
      break;

    case IDFVTX_DCM0:
    case IDFVTX_SIM:
      return new Packet_fvtx_dcm0(pp);
      break;

      //    case IDMUTRIG_FPGA:
      //return new Packet_mutrig_fpga(pp);
      //      break;

    case IDNTCT0_FPGA:
    case IDNTCT0_FPGA0SUP:
      return new Packet_ntct0_fpga(pp);
      break;

    case IDCDEVPOLARIMETER:
      return new Packet_cdevpolarimeter(pp);
      break;

    case IDCDEVPOLARIMETERTARGET:
      return new Packet_cdevpoltarget(pp);
      break;

    case IDCDEVIR:
      return new Packet_cdevir(pp);
      break;


    case IDCDEVWCMHISTORY:
      return new Packet_cdevwcm(pp);
      break;
      //EJD add cdev packet defs
    case IDCDEVBPM:
      return new Packet_cdevbpm(pp);
      break;

    case IDCDEVDVM:
      return new Packet_cdevdvm(pp);
      break;

    case IDCDEVRING:
      return new Packet_cdevring(pp);
      break;

    case IDCDEVRINGPOL:
      return new Packet_cdevring(pp);
      break;

    case IDCDEVRINGFILL:
      return new Packet_cdevring(pp);
      break;

    case IDCDEVRINGNOPOL:
      return new Packet_cdevringnopol(pp);
      break;

    case IDCDEVBUCKETS:
      return new Packet_cdevbuckets(pp);
      break;

      //mlp 10/27/03 added this - the SIS is a straight array of numbers.
    case IDCDEVSIS:
      return new Packet_id4evt(pp);
      break;

    case IDCDEVMADCH:
      return new Packet_cdevmadch(pp);
      break;

    case ID4SCALER:
      return new Packet_id4scaler(pp);
      break;

    case IDRICH_LL1:
      return new Packet_rich_ll1(pp);
      break;
    case IDMVD_LL1:
      return new Packet_mvd_ll1(pp);
      break;
    case IDBBC_LL1:
      return new Packet_bbc_ll1(pp);
      break;
    case IDTOF_LL1:
      return new Packet_tof_ll1(pp);
      break;
    case IDMUIDH_LL1:
      return new Packet_muid_hor_ll1(pp);
      break;
    case IDMUIDV_LL1:
      return new Packet_muid_ver_ll1(pp);
      break;
    case IDNTCZDC_LL1:
      return new Packet_ntczdc_ll1(pp);
      break;

    case IDBIG_LL1:
      return new Packet_big_ll1(pp);
      break;

    case IDERT_E_LL1:
    case IDERT_W_LL1:
      return new Packet_ert_ll1(pp);
      break;

    case IDPBGL_LL1:
      return new Packet_pbgl_ll1(pp);
      break;
    case IDPBSC_LL1:
      return new Packet_pbsc_ll1(pp);
      break;
    case IDGL1:
      return new Packet_gl1(pp);
      break;

    case IDGL1P:
      return new Packet_gl1p(pp);
      break;

    case IDGL1_EVCLOCK:
      return new Packet_gl1_evclocks(pp);
      break;

    case IDGL1PSUM:
    case IDGL1PSUMOBS:
      return new Packet_gl1psum(pp);
      break;

    case IDL2DECISION:
      return new Packet_lvl2decision(pp);
      break;
    case IDL2PRIMITIVE:
      return new Packet_lvl2primitive(pp);
      break;
    case ID4EVT:
      return new Packet_id4evt(pp);
      break;
    case ID2EVT:
      return new Packet_id2evt(pp);
      break;
      
    case IDCSTR:
      return new Packet_idcstr(pp);
      break;
      
    case IDSTARSCALER:
      return new Packet_starscaler(pp);
      break;

    case IDBBC_DCM0:
      return new Packet_bbc_dcm0(pp);
      break;
    case IDMVD_DCM0:
      return new Packet_mvd_dcm0(pp);
      break;
    case IDDCH_DCM0:
      return new Packet_dch_dcm0(pp);
      break;
    case IDPC_DCM0:
      return new Packet_pc_dcm0(pp);
      break;
    case IDTEC_DCM0:
      return new Packet_tec_dcm0(pp);
      break;
    case IDRICH_DCM0:
      return new Packet_rich_dcm0(pp);
      break;
    case IDTOF_DCM0:
      return new Packet_tof_dcm0(pp);
      break;
    case IDPBSC_DCM0:
      return new Packet_emc_dcm0(pp);
      break;
    case IDPBGL_DCM0:
      return new Packet_emc_dcm0(pp);
      break;
    case IDMUTA_DCM0:
      return new Packet_muta_dcm0(pp);
      break;
    case IDMUTC_DCM0:
      return new Packet_mutc_dcm0(pp);
      break;
    case IDMUID_DCM0:
      return new Packet_muid_dcm0(pp);
      break;
    case IDZDC_DCM0:
      return new Packet_zdc_dcm0(pp);
      break; 

    case IDSTRIP_DCM0:
      return new Packet_strip_dcm0(pp);
      break;

    case IDMUTRG_DCM0:
      return new Packet_mutrg_dcm0(pp);
      break;

     case IDRPC_DCM0:
       return new Packet_rpc_dcm0(pp);
       break;
     
    case IDRPC_PROTO:
    case IDRPC_PROTO0SUP:
      return new Packet_rpc_proto(pp);
      break;

    case IDRPC_FPGA:
    case IDRPC_FPGA0SUP:
      return new Packet_rpc_fpga(pp);
      break;


    case IDBBC_DCM1:
      return new Packet_bbc_dcm1(pp);
      break;
    case IDMVD_DCM1:
      return new Packet_mvd_dcm1(pp);
      break;

      // that's alright: we return a dcm0 packet here
    case IDDCH_DCM1:
      return new Packet_dch_dcm0(pp);
      break;

    case IDPC_DCM1:
      return new Packet_pc_dcm1(pp);
      break;
    case IDTEC_DCM1:
      return new Packet_tec_dcm1(pp);
      break;
    case IDRICH_DCM1:
      return new Packet_rich_dcm1(pp);
      break;
    case IDTOF_DCM1:
      return new Packet_tof_dcm1(pp);
      break;
    case IDPBSC_DCM1:
      return new Packet_emc_dcm0(pp);
      break;
    case IDPBGL_DCM1:
      return new Packet_emc_dcm0(pp);
      break;
    case IDMUTA_DCM1:
      return new Packet_muta_dcm1(pp);
      break;
    case IDMUTC_DCM1:
      return new Packet_mutc_dcm1(pp);
      break;
    case IDMUID_DCM1:
      // no mistake. dcm0 handles both IDMUID_DCM0 and IDMUID_DCM1 formats
      return new Packet_muid_dcm0(pp);
      break;
    case IDZDC_DCM1:
      return new Packet_zdc_dcm1(pp);
      break;
      
    case IDBBC_DCM2:
      return new Packet_bbc_dcm2(pp);
      break;
    case IDMVD_DCM2:
      return new Packet_mvd_dcm2(pp);
      break;
    case IDDCH_DCM2:
      return new Packet_dch_dcm2(pp);
      break;
    case IDPC_DCM2:
      return new Packet_pc_dcm2(pp);
      break;
    case IDTEC_DCM2:
      return new Packet_tec_dcm2(pp);
      break;
    case IDRICH_DCM2:
      return new Packet_rich_dcm2(pp);
      break;
    case IDTOF_DCM2:
      return new Packet_tof_dcm2(pp);
      break;
    case IDMUTA_DCM2:
      return new Packet_muta_dcm2(pp);
      break;
    case IDMUTC_DCM2:
      return new Packet_mutc_dcm2(pp);
      break;
    case IDZDC_DCM2:
      return new Packet_zdc_dcm2(pp);
      break;

    case IDCDEVDESCR:
      return new Packet_idcdevdescr(pp);
      break;



    default:
      switch (getUnstructPacketWordSize (pp))
	{
	case 1:
	  return new Packet_w1(pp);
	  break;
	case 2:
	  return new Packet_w2(pp);
	  break;
	case 4:
	  return new Packet_w4(pp);
	  break;
	default:
	  return new Packet_w4(pp); 
	}
    }
  
  return 0;
}



void  
A_Event::identify (OSTREAM &os) const
{ 
  os << " -- Event "   << EventData->evt_sequence;

  os << " Run: "  << SETW(5)  << EventData->run_number;

  os << " length: "     << SETW(5)  <<EventData->evt_length;

  os << " frames: " << SETW(3) << NumberFrames;

  os << " type: "      << SETW(2)  << EventData->evt_type ;

  os << " (" << get_evt_mnemonic(EventData->evt_type );


  if ( ( EventData->evt_type & CORRUPTEVENTMASK ) )
  {
    os << " *Corrupt* "; 
  }
  os << ") ";

  time_t x = getTime();
  os << x;
  
  os << std::endl;

  //PHTimeStamp *p = getTimeStamp();
  // os << *p << std::endl;
  //delete p;


};



int  
A_Event::existPacket (const int id)
{
#if !defined(SunOS) && !defined(OSF1)

  PHDWORD *pp;

  if (!hasMap) 
    {
      createMap();
    }

  pp = pmap[id];

  if (!pp) 
    {
      return 0;
    }
  return 1;
#else
  return 0;
#endif
}



// the Copy routine
int  
A_Event::Copy (int * array, const unsigned int length, int *nw, const char *what)
{
  if (length< getEvtLength() )
    {
      *nw = 0;
      return -1;
    }
  char *to = (char *) array;
  char *from;
  unsigned int l;
  if ( strcmp (what, "DATA") ==0  )
    {
      from= (char *)  &EventData->data[0];
      l = getEvtLength() - EVTHEADERLENGTH;
    }
  else
    {
      from= (char *) EventData;
      l = getEvtLength();
    }
  //  for (i=0; i<l ; i++) *to++ = *from++;
  //
  *nw = l;
  memcpy (to, from, l*4);
  return 0;
}

int A_Event::convert()
{
  if (is_data_type) return -1;

  PHDWORD *tmp;

  tmp = new PHDWORD[getEvtLength()];
  PHDWORD *from= (PHDWORD *)  EventData;
  PHDWORD *to=tmp;
  for (unsigned int k=0; k< getEvtLength(); k++) 
    { 
      *to++ = *from++;
    }

  delete [] framelist;
  EventData = (evtdata_ptr) tmp;
  updateFramelist();

  is_data_type = 1;
  pmap.clear();
  hasMap = 0;
  return 0;

}

int A_Event::is_pointer_type() const
{
  if (is_data_type) return 0;
  else return 1;
}


int A_Event::updateFramelist()
{
  // go through the data and see how may, 
  // if any, frames we have got.
  int max_index = EventData->evt_length - EVTHEADERLENGTH;
  int index = 0;
  int number_of_frames = 0;
  int flength;
  int cont;
  int i;
  // count the number of frames, and allocate a properly-sized vector
  // with pointers to it. 
  cont = 1;
  while (index < max_index && cont)
    {
      PHDWORD *f = &EventData->data[index];
      // so here we believe that we point to a frame start
      if (  validFrameHdr(f) )
	{
	  number_of_frames++;
	  flength = getFrameLength (f);
	  index += flength;
	}
      else 
	{
	  COUT << "invalid frame header, frame nr " << number_of_frames << " index = " << index <<  std::endl; //mlpd
	  COUT << " -- Event*"   << EventData->evt_sequence;

	  COUT << " Run: "  << SETW(5)  << EventData->run_number;

	  COUT << " length: "     << SETW(5)  <<EventData->evt_length;
	  
	  COUT << " type: "      << SETW(2)  << EventData->evt_type 
	       << " (" << get_evt_mnemonic(EventData->evt_type) << ") ";
	  COUT << std::endl;

	  for (i=0; i< 8; i++) 
	    COUT << i << "  " << std::hex << f[i] << std::dec << std::endl;

	  return -1;

	  // COUT << "I will continue anyway" << std::endl;
	  // cont = 0;
	}
    }

  // ok, so many frames. get the vector.
  framelist = new PHDWORD *[number_of_frames +1];
  
  // NumberFrames is a class data member.
  NumberFrames =  number_of_frames;

  // now we go through once more and remember the pointers to where the 
  // frames start
  index = 0;
  int ifn = 0;
  while (index < max_index && cont)
    {
      PHDWORD *f = &EventData->data[index];
      if (  validFrameHdr(f) )
	{
	  framelist[ifn++] = f;
	  flength = getFrameLength (f);
	  index += flength;
	}
      else 
	{
	  COUT << "invalid frame header, frame nr " << ifn << " index = " << index <<  std::endl; //mlpd
	  for (i=0; i< 8; i++) 
	    COUT << i << "  " << std::hex << f[i] << std::dec << std::endl;


	  cont = 0;
	}
    }

  //we terminate the list of frames with a 0.
  framelist[ifn] = 0;
  return 0;
}

int A_Event::getErrorCode()
{
#if !defined(SunOS) && !defined(OSF1)
  createMap();
#endif
  return errorcode;
}
