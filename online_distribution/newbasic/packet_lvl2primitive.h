///////////////////////////////////////////////////////////////////////////
//
//  packet_lvl2primitive.h
//
//    Header file defining the interface to the lvl2 primitive packet
//    reading class.
//
//  $Log: packet_lvl2primitive.h,v $
//  Revision 1.6  2005/09/28 00:26:25  ratcap
//
//  mlp -- added a few new mem function and fixed lvlprimitive
//
//  Revision 1.5  2001/09/30 21:56:11  phoncs
//  jfrantz--fixed packet_lvl2primitive.h commit by Mickey on NT by adding reinterpret casts
//
//  Revision 1.4  2001/09/26 22:52:02  phoncs
//  getPrimitiveDataLength() was off by 1 (mchiu)
//
//  Revision 1.1  2001/08/23 04:56:35  phnxlvl1
//  Temporarily added to local directory because they're not in the newbasic that we're using
//
//  Revision 1.2  2001/07/30 05:16:10  phoncs
//  Many bug fixes to make ddump work on lvl2 primitives
//
//  Revision 1.1  2001/07/28 23:50:05  cole
//  First version in cvs
//
//
///////////////////////////////////////////////////////////////////////////

#ifndef __PACKET_LVL2PRIMITIVE_H__
#define __PACKET_LVL2PRIMITIVE_H__

#include <packet_w124.h>

//  The level-2 primitive data are written in long (32-bit) words so
//    we inherit from Packet_w4
//
#ifndef __CINT__
class WINDOWSEXPORT Packet_lvl2primitive : public Packet_w4 {
#else
class  Packet_lvl2primitive : public Packet_w4 {
#endif

public:

  //  Enumeration of the few "what's" implemented for lvl2primitives
  //
  enum What {DataLength, NameLength, Name, Data, DataPtr};

  Packet_lvl2primitive(PACKET_ptr data) : Packet_w4 (data)
  {}
  
  ~Packet_lvl2primitive()
  {}

  int getArraylength(const char * what="")
  {
    if (strcmp(what, "Name")) return getPrimitiveNameLength();
    else if (strcmp(what, "Data")) return getPrimitiveDataLength();
    else return 0;
  }

  int fillIntArray (int destination[],    // the data go here 
			       const int length,            // space we have in destination
			       int * nw,                    // words actually used
			       const char * what="");       // type of data (see above)

  int  iValue(const int channel, const char *what);
  int  iValue(const int channel, const int what);
  void  *  pValue(const int channel);
  void dump ( OSTREAM& );

  unsigned int getPrimitiveLength() const
  {
    return getDataLength();
  }

  unsigned int* getPrimitivePtr() const
  {
    return  reinterpret_cast<unsigned int*>(findPacketDataStart(packet));
  }

  unsigned int getPrimitiveDataLength() const
  {
    return getDataLength() - getPrimitiveNameLength() - 1;	
  }

  unsigned int getPrimitiveNameLength() const
  {
    return *(reinterpret_cast<unsigned int*>(findPacketDataStart(packet)));
  }

  unsigned int* getPrimitiveDataPtr() const
  {
    return reinterpret_cast<unsigned int*>
      (findPacketDataStart(packet) + 1 + getPrimitiveNameLength());
  }
protected:

  virtual int *decode (int *);

 private:

  //  We will use std::copy when it's available everywhere
  //
  template<class T> void docopy(const T* start, const T* end, T* out)
  {
    for (const T* ptr = start; ptr < end; ptr++)
    {
      *out++ = *ptr;
    } 
  }
};

#endif /* __PACKET_LVL2PRIMITIVE_H__ */

















