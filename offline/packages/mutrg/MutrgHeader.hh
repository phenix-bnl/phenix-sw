#ifndef __MUTRGHEADER__
#define __MUTRGHEADER__

#include "TObject.h"

#include "MutrgPar.hh"

class MutrgHeader : public TObject{
public:
  MutrgHeader(void){;}
  virtual ~MutrgHeader(void){;}
  virtual void Reset(void)=0;

  virtual unsigned short GetPacketID(void)=0;
  virtual unsigned short GetPacketFormat(void)=0;
  virtual unsigned short GetEventNumber(void)=0;
  virtual unsigned short GetFlagWord(void)=0;
  virtual unsigned short GetDetectorID(void)=0;
  virtual unsigned short GetModuleAddress(void)=0;
  virtual unsigned short GetClockCounter(void)=0;
  virtual unsigned short GetUserWord(void)=0;
  virtual bool GetParityOK(void)=0;
  virtual unsigned short GetParityWord(void)=0;
  virtual unsigned short GetPacketNWord(void)=0;
  virtual unsigned short GetMrgFormat(void)=0;
  virtual unsigned short GetMrgError(void)=0;
  virtual bool GetDcmifError(void)=0;
  virtual unsigned short GetEventWidth(void)=0;
  virtual int GetArm(void)=0;
  virtual int GetOctant(int ioct)=0;
  virtual void GetArmOctant(int &arm0,int &oct1,int &oct2)=0;

  virtual void SetPacketID(unsigned short val)=0;
  virtual void SetPacketFormat(unsigned short val)=0;
  virtual void SetEventNumber(unsigned short val)=0;
  virtual void SetFlagWord(unsigned short val)=0;
  virtual void SetDetectorID(unsigned short val)=0;
  virtual void SetModuleAddress(unsigned short val)=0;
  virtual void SetClockCounter(unsigned short val)=0;
  virtual void SetUserWord(unsigned short val)=0;
  virtual void SetParityOK(bool flag)=0;
  virtual void SetParityWord(unsigned short val)=0;
  virtual void SetPacketNWord(unsigned short val)=0;
  virtual void SetMrgFormat(unsigned short val)=0;
  virtual void SetMrgError(unsigned short val)=0;
  virtual void SetDcmifError(bool val)=0;
  virtual void SetEventWidth(unsigned short val)=0;
  virtual void SetArm(int arm0)=0;
  virtual void SetOctant(int ioct,int oct)=0;
  virtual void SetArmOctant(int arm0,int oct1,int oct2)=0;

protected:
  ClassDef(MutrgHeader,1)
};

#endif /* __MUTRGHEADER__ */
