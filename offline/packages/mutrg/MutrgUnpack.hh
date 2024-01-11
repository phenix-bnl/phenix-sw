#ifndef __MUTRGUNPACK__
#define __MUTRGUNPACK__

/*
packet_format=
 791 : early version
1291 : w/o zero suppression
1391 : w/  zero suppression
*/

#include <vector>
#include <string>

class Packet;
class MutrgHeader;

class MutrgUnpack{
public:
  MutrgUnpack(void);
  virtual ~MutrgUnpack(void){;}
  void Reset(void);

  int Unpack(Packet *packet);
  int UnpackF0(Packet *packet,int nheader,int nfooter);
  int UnpackF1(Packet *packet,int nheader,int nfooter);
  int CheckPacket(Packet *packet,int *data);

  int FillHeader(MutrgHeader *mutrg_header);
  int FillHeaderF0(MutrgHeader *mutrg_header);
  int FillHeaderF1(MutrgHeader *mutrg_header);
  int CheckHeaderSize(int nheader,int nfooter);

  void SetHeader(const std::vector<unsigned int> &header1){header=header1;}
  void SetFooter(const std::vector<unsigned int> &footer1){footer=footer1;}
  void SetData(const std::vector<unsigned int> &data1){data=data1;}
  void SetPacketFormat(int num){packet_format=num;}

  const std::vector<unsigned int>& GetHeader(void){return header;}
  const std::vector<unsigned int>& GetFooter(void){return footer;}
  const std::vector<unsigned int>& GetData(void){return data;}
  int GetPacketFormat(void){return packet_format;}

  const char* ClassName(void){return class_name.c_str();}

protected:
  std::string class_name;

  static const int MAX_DATA_LENGTH;
  std::vector<unsigned int> header;
  std::vector<unsigned int> footer;
  std::vector<unsigned int> data;
  int packet_format;
};

#endif /* __MUTRGUNPACK__ */
