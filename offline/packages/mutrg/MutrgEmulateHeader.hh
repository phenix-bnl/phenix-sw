#ifndef __MUTRGEMULATEHEADER__
#define __MUTRGEMULATEHEADER__

#include <string>

class PHCompositeNode;
class EventHeader;
class MutrgHeaderArray;

class MutrgEmulateHeader{
public:
  MutrgEmulateHeader(void);
  virtual ~MutrgEmulateHeader(void){;}

  int Init(PHCompositeNode *top_node);
  int InitRun(PHCompositeNode *top_node);
  int ProcessEvent(PHCompositeNode *top_node);

  void SetPacketFormat(unsigned short val){packet_format=val;}
  void SetMrgFormat(unsigned short val){mrg_format=val;}
  void SetEventWidth(unsigned short val){event_width=val;}

  const char *ClassName(void){return class_name.c_str();}

public:
  // should be in packetConsts.h
  static const unsigned short PACKET_FORMAT_ZERO;
  static const unsigned short PACKET_FORMAT_NOZERO;

  static const int PACKET_TO_ARM[8]; // [packet]
  static const int PACKET_TO_OCTANT[8][2]; // [packet][octant]
  static const unsigned short PACKET_TO_DET_ID[8];
  static const unsigned short PACKET_TO_MOD_ID[8];

protected:
  std::string class_name;

  unsigned short packet_format;
  unsigned short mrg_format;
  unsigned short event_width;

  EventHeader *evt_header;
  MutrgHeaderArray *mutrg_headers;
};

#endif /* __MUTRGEMULATEHEADER__ */
