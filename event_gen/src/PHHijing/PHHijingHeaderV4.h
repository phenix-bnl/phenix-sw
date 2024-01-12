#ifndef __PHHijingHeaderV4_H__
#define __PHHijingHeaderV4_H__

#include <phool.h>
#include <PHObject.h>
#include <PHHijingHeaderV3.h>

class PHHijingHeaderV4 : public PHHijingHeaderV3
{
public:

  PHHijingHeaderV4();
  virtual ~PHHijingHeaderV4() {}

  virtual void SetRP(const float val) { rp = val; }
  virtual float GetRP() const { return rp; };
private:

  float rp;

  ClassDef (PHHijingHeaderV4,1)
};

#endif	// __PHPYTHIAHEADER_H__
