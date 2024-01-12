#ifndef __PHHijingHeaderV3_H__
#define __PHHijingHeaderV3_H__

#include <phool.h>
#include <PHObject.h>
#include <PHHijingHeaderV2.h>

class PHHijingHeaderV3 : public PHHijingHeaderV2
{
public:

  PHHijingHeaderV3();
  virtual ~PHHijingHeaderV3() {}

  virtual void SetNp(const int val) { np = val; }
  virtual void SetNt(const int val) { nt = val; }
  virtual int GetNp() const { return np; }
  virtual int GetNt() const { return nt; }

private:

  int np;
  int nt;

  ClassDef (PHHijingHeaderV3,1)
};

#endif	// __PHPYTHIAHEADER_H__
