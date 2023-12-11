///////////////////////////////////////////////////////////////////////////
//
//  packet_lvl2decision.cc
//
//    Implementation of the lvl2 decision history packet reading class.
//
//  $Log: packet_lvl2decision.cc,v $
//  Revision 1.7  2010/09/21 19:37:50  phoncs
//  DLW: change name of DWORD to PHDWORD
//
//  Revision 1.6  2005/11/21 16:47:26  phoncs
//  DLW: remove 'std::' string
//
//  Revision 1.5  2004/01/23 01:37:57  purschke
//  mlp -- took out the namespace std:: in packet.h.
//         good practice.
//
//  Revision 1.4  2002/08/20 21:01:53  pinkenbu
//  Bugfix in unpacking routine
//
//  Revision 1.3  2002/03/23 00:06:12  jfrantz
//  added ddump info for algDecision
//
//  Revision 1.2  2001/08/27 13:56:29  phnxlvl1
//  major revisions to remove explicit dependence on Lvl2Decision.h
//
//  Revision 1.1  2001/08/23 16:00:12  cole
//  First versions
//
//
///////////////////////////////////////////////////////////////////////////


#include <packet_lvl2decision.h>
#include <string.h>

Packet_lvl2decision::Packet_lvl2decision(PACKET_ptr data) : Packet_w4 (data),
							    _unpacked(false)
{
  for (UINT itrig = 0; itrig < MaxNumLvl1Triggers; itrig++)
  {
    _lvl1Decision[itrig] = 0;
    for (UINT ialg = 0; ialg < MaxNumAlgorithms; ialg++)
    {
      _lvl1AlgorithmDecision[itrig][ialg] = 0;
    }
  }


  for (UINT ialg = 0; ialg < MaxNumAlgorithms; ialg++)
  {
    _algorithmDecision[ialg] = 0;
  }
}

int* Packet_lvl2decision::decode ( int *nwout)
{
  unpack();
  *nwout = 0;
  return 0;
}

void Packet_lvl2decision::unpack()
{
  const PHDWORD* read_ptr = findPacketDataStart(packet);

  _finalDecision = *read_ptr++;
  _numLvl1Triggers = *read_ptr++;

  for (UINT ilvl1 = 0; ilvl1 < _numLvl1Triggers; ilvl1++)
  {
    UINT ibit = *read_ptr++;
    _lvl1Decision[ibit] = *read_ptr++;

    UINT nalg = *read_ptr++;
    for (UINT ialg = 0; ialg < nalg; ialg++)
    {
      UINT algNumber = *read_ptr++;
      UINT decision = *read_ptr++;

      _lvl1AlgorithmDecision[ibit][algNumber] = decision;
      _algorithmDecision[ialg] = decision;
    }
  }

  _unpacked = true;

}

//  ich is ignored for obvious reasons
//
int Packet_lvl2decision::iValue(const int ich, const char *what)
{
  if (!_unpacked) unpack();

  if (strcmp(what, "FullDecision") == 0) 
  {
	return iValue(ich, FullDecision);
  }
  else if (strcmp(what, "NumLevel1Triggers") == 0)
  {
	return iValue(ich, NumLevel1Triggers);
  }
  else if (strcmp(what, "Level1TriggerDecision") == 0)
  {
	return iValue(ich, Level1TriggerDecision);
  }
  else if (strcmp(what, "AlgorithmDecision") == 0)
  {
	return iValue(ich, AlgorithmDecision);
  }
  else return 0;
}

int Packet_lvl2decision::iValue(const int ich, const  int what)
{
  if (!_unpacked) unpack();

  switch (what) 
  {
  case FullDecision:
  {
    return _finalDecision;
  }
  case NumLevel1Triggers:
  {
    return _numLvl1Triggers;
  }
  case Level1TriggerDecision:
  {
    return _lvl1Decision[ich];
  }
  case AlgorithmDecision:
  {
    return _algorithmDecision[ich];
  }
  default:
    {
      return 0;
    }
  }
}


void Packet_lvl2decision::dump ( OSTREAM &os)
{
  if (!_unpacked) unpack();

  this->identify(os); 

  //  Start with the full decision
  //
  os << "Final Level2 decision: " << _finalDecision
     << ", Number of Level-1 triggers: " << _numLvl1Triggers
     << std::endl;

  for (UINT itrig = 0; itrig < MaxNumLvl1Triggers; itrig++)
  {
    if (_lvl1Decision[itrig] != 0) {
      os << "Level1 Trigger bit: " << itrig
	 << ", Decision: " << std::hex << _lvl1Decision[itrig] << std::dec << std::endl;
       
      //  Now get the algorithms for this level-1 trigger
      // 
      for (UINT ialg = 0; ialg < MaxNumAlgorithms; ialg++)
      {
	if (_lvl1AlgorithmDecision[itrig][ialg] != 0) {
	  os << "Algorithm index: " << ialg 
	     << ", std::decision: " << std::hex << _lvl1AlgorithmDecision[itrig][ialg] << std::dec << std::endl;
	}
      }
    }
  }

  os << "summary of lvl2 algorithm std::decisions as Algorithm(Decision) :" << std::endl;
  for (UINT ilvl2 = 0; ilvl2 < MaxNumAlgorithms; ilvl2++) 
    {
      os << " " << ilvl2 <<"(" << std::hex << _algorithmDecision[ilvl2] << std::dec
	 << ")";
    }

  os << std::endl;
}








