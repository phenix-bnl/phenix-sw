#ifndef __MUTRGHEADER_V1__
#define __MUTRGHEADER_V1__

#include "MutrgHeader.hh"

class MutrgHeader_v1 : public MutrgHeader{
public:
  MutrgHeader_v1(void);
  virtual ~MutrgHeader_v1(void){;}
  virtual void Reset(void);

  virtual unsigned short GetPacketID(void){return packet_id;}
  virtual unsigned short GetPacketFormat(void){return packet_format;}
  virtual unsigned short GetEventNumber(void){return event_number;}
  virtual unsigned short GetFlagWord(void){return flag_word;}
  virtual unsigned short GetDetectorID(void){return detector_id;}
  virtual unsigned short GetModuleAddress(void){return module_address;}
  virtual unsigned short GetClockCounter(void){return clock_counter;}
  virtual unsigned short GetUserWord(void){return user_word;}
  virtual bool GetParityOK(void){return parity_ok;}
  virtual unsigned short GetParityWord(void){return parity_word;}
  virtual unsigned short GetPacketNWord(void){return packet_nword;}
  virtual unsigned short GetMrgFormat(void){return mrg_format;}
  virtual unsigned short GetMrgError(void){return mrg_error;}
  virtual bool GetDcmifError(void){return dcmif_error;}
  virtual unsigned short GetEventWidth(void){return event_width;}
  virtual int GetArm(void){return arm;}
  virtual int GetOctant(int ioct);
  virtual void GetArmOctant(int &arm0,int &oct1,int &oct2);

  virtual void SetPacketID(unsigned short val){packet_id=val;}
  virtual void SetPacketFormat(unsigned short val){packet_format=val;}
  virtual void SetEventNumber(unsigned short val){event_number=val;}
  virtual void SetFlagWord(unsigned short val){flag_word=val;}
  virtual void SetDetectorID(unsigned short val){detector_id=val;}
  virtual void SetModuleAddress(unsigned short val){module_address=val;}
  virtual void SetClockCounter(unsigned short val){clock_counter=val;}
  virtual void SetUserWord(unsigned short val){user_word=val;}
  virtual void SetParityOK(bool flag){parity_ok=flag;}
  virtual void SetParityWord(unsigned short val){parity_word=val;}
  virtual void SetPacketNWord(unsigned short val){packet_nword=val;}
  virtual void SetMrgFormat(unsigned short val){mrg_format=val;}
  virtual void SetMrgError(unsigned short val){mrg_error=val;}
  virtual void SetDcmifError(bool val){dcmif_error=val;}
  virtual void SetEventWidth(unsigned short val){event_width=val;}
  virtual void SetArm(int arm0){arm=arm0;}
  virtual void SetOctant(int ioct,int oct);
  virtual void SetArmOctant(int arm0,int oct1,int oct2);

protected:
  unsigned short packet_id;
  unsigned short packet_format;
  unsigned short event_number;
  unsigned short flag_word;
  unsigned short detector_id;
  unsigned short module_address;
  unsigned short clock_counter;
  unsigned short user_word;
  bool parity_ok;
  unsigned short parity_word;
  unsigned short packet_nword;
  unsigned short mrg_format;
  unsigned short mrg_error;
  bool dcmif_error;
  unsigned short event_width;
  int arm;
  int octant[MutrgPar::NOCTANT_IN_PACKET];

  ClassDef(MutrgHeader_v1,1)
};

#endif /* __MUTRGHEADER_V1__ */
