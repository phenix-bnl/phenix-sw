///////////////////////////////////////////////////////////////////////////
//
//  packet_lvl2primitive.cc
//
//    Implementation of the lvl2 primitive packet reading class.
//
//  $Log: packet_lvl2primitive.cc,v $
//  Revision 1.13  2010/09/21 19:37:50  phoncs
//  DLW: change name of DWORD to PHDWORD
//
//  Revision 1.12  2008/03/24 22:05:05  pinkenbu
//  fix bad selection for fillIntArray what parameter
//
//  Revision 1.11  2007/06/05 15:30:08  phoncs
//  DLW: move the last endl to the right place
//
//  Revision 1.10  2007/06/05 15:25:31  phoncs
//  DLW: add std::endl to end of output for packet words
//
//  Revision 1.9  2005/09/28 00:26:25  ratcap
//
//  mlp -- added a few new mem function and fixed lvlprimitive
//
//  Revision 1.8  2004/01/23 01:37:57  purschke
//  mlp -- took out the namespace std:: in packet.h.
//         good practice.
//
//  Revision 1.7  2002/05/31 22:15:43  purschke
//  mlp -- went through the insure report and tried to fix
//  every little problem there is, unused variables, dead statements,
//  all. It'll probably take another round to complete, but it should get
//  rid of most warnings.
//
//  Revision 1.6  2001/10/03 18:25:49  phnxlvl1
//  (Brian) Remove use of vector
//
//  Revision 1.5  2001/10/03 18:24:21  phoncs
//  jfrantz-- same as other changes (re-intrept cast, etc)
//
//  Revision 1.4  2001/09/26 22:52:22  phoncs
//  updated so it matches online monitor (mchiu)
//
//  Revision 1.1  2001/08/23 04:56:27  phnxlvl1
//  Temporarily added to local directory because they're not in the newbasic that we're using
//
//  Revision 1.2  2001/07/30 05:21:16  phoncs
//  Many bug fixes to make ddump work on lvl2 primitives
//
//  Revision 1.1  2001/07/28 23:50:27  cole
//  First version in cvs
//
//
///////////////////////////////////////////////////////////////////////////


#include "packet_lvl2primitive.h"
#include <string.h>
#include <vector>

int *Packet_lvl2primitive::decode ( int *nwout)
{
  //  Since the format of the data is pre-proscribed by the primitive streamer
  //    which will actually do the unpacking, all we do here is copy the data
  //    word for word
  //
  unsigned long length = getDataLength();
  PHDWORD* output_ptr = new PHDWORD[length];

  const PHDWORD* start_ptr = findPacketDataStart(packet);
  docopy(start_ptr, start_ptr + length, output_ptr);

  *nwout = length;
  return reinterpret_cast<int*>(output_ptr);
}

//  ich is ignored for obvious reasons
//
int Packet_lvl2primitive::iValue(const int ich, const char *what)
{
  if (strcmp(what, "DataLength") == 0) 
  {
	return iValue(ich, DataLength);
  }
  else if (strcmp(what, "NameLength") == 0)
  {
	return iValue(ich, NameLength);
  }
  else if (strcmp(what, "DataPtr") == 0)
  {
	return iValue(ich, DataPtr);
  }
  else return 0;

}

void *  Packet_lvl2primitive::pValue(const int ich)
{
  return (findPacketDataStart(packet) + getPrimitiveNameLength());
}

int Packet_lvl2primitive::iValue(const int ich, const  int what)
{
  switch (what) 
  {
  case DataLength:
  {
	return getPrimitiveDataLength();
  }
  case DataPtr:
  {
    return 0;
  }
  case NameLength:
  {
	return getPrimitiveNameLength();	
  }
  default:
    {
      return 0;
    }
  }
}


/*int Packet_lvl2primitive::getArraylength(const char * what="")
{
  if (strcmp(what, "Name")) return getPrimitiveNameLength();
  else if (strcmp(what, "Data")) return getPrimitiveDataLength();
  else return 0;
}
*/

int Packet_lvl2primitive::fillIntArray(int destination[],
                                       const int length,
                                       int * nwout,
                                       const char * what)
{
  unsigned int* data_ptr;
  unsigned int nlength;
  if (strcmp(what, "Name") == 0)
    {
      nlength = getPrimitiveNameLength();
      if (nlength > length)
        {
          *nwout = 0;
          return -1;
        }

      //  The name length is the first word in the packet, the name follows
      //
      data_ptr = reinterpret_cast<unsigned int*>( findPacketDataStart(packet) + 1);
    }
  else if (strcmp(what, "Data") == 0)
    {
      nlength = getPrimitiveDataLength();
      if (nlength > length)
        {
          *nwout = 0;
          return -1;
        }
      data_ptr = getPrimitiveDataPtr();
    }
  else if (strcmp(what, "DATA") == 0)
    {
      nlength = getDataLength();
      if (nlength > length)
        {
          *nwout = 0;
          return -1;
        }
      data_ptr = reinterpret_cast<unsigned int*>( findPacketDataStart(packet));
    }
  else
    {
      std::cout << "Packet_lvl2primitive::fillIntArray Unknown option " << what << std::endl;
      return -1;
    }
  docopy(data_ptr, data_ptr + nlength, reinterpret_cast<unsigned int*>(destination));
  *nwout = nlength;

  return 0;
}


void Packet_lvl2primitive::dump ( OSTREAM &os)
{

  this->identify(os); 

  //  Start by dumping the name. We keep a static vector that we resize as needed.
  //
  static char* stringVec = new char[1024];

  // We tack on a zero at the end for good measure. The string will be padded with
  //   zeros but if the name ends on a word boundary it will have no zero.
  //
  unsigned int nameLengthBytes = getPrimitiveNameLength()*4;
  unsigned int vecLength =  nameLengthBytes + 1;
  if (vecLength > 1024) 
  {
    std::cout << "Packet_lvl2primitive::dump: unable to dump packet because name is too long (> 1024 bytes)" << std::endl;
  }

  PHDWORD* nameDword_ptr = findPacketDataStart(packet) + 1;
  char* name_ptr = reinterpret_cast<char*>(nameDword_ptr);

  docopy(name_ptr, name_ptr + nameLengthBytes, &stringVec[0]);
  stringVec[vecLength - 1] = 0;

  os << "Level-2 Primitive:: " << reinterpret_cast<char*>(&stringVec[0]) << std::endl;

  // Now dump the max number and actual number of copies
  //
  unsigned int* data_ptr = getPrimitiveDataPtr();
  unsigned int nMax = *data_ptr++;
  unsigned int nActual = *data_ptr++;

  os << "  Maximum copies = " << nMax << ", actual copies = " << nActual << std::endl;

  //  Now loop through the copies 
  //
  for (unsigned int iprim = 0; iprim < nActual; iprim++)
  {
    unsigned int index = *data_ptr++;
    unsigned int length = *data_ptr++;
    os << "\n" << " Copy # " << index << ", length = " << length << " DWords" << std::endl;
    
    os << std::hex;
    for (unsigned int ivalue = 0; ivalue < length; ivalue++) {
      os << *data_ptr++ << " ";
    }

    os << std::dec;
  }
  os << std::endl;
}








