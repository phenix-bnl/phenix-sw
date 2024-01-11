#ifndef MPCEXRAWHIT_H
#define MPCEXRAWHIT_H

#include <PHObject.h>

class MpcExRawHit: public PHObject
{
 public:
  virtual ~MpcExRawHit() {}
  virtual void Reset() {}
  virtual unsigned int fillfromvector(const std::vector<unsigned int> &vec) {return -1;}
  virtual unsigned int getnhits() const {return 0;};
  virtual unsigned int gethit (const unsigned int index) const {return 0x0;};

  virtual unsigned int getid (const unsigned int index) const {return 0x0;};
  virtual unsigned int getadcs (const unsigned int index) const {return 0x0;};

  virtual unsigned int getarm (const unsigned int index) const {return 0x0;};
  virtual unsigned int getpkt (const unsigned int index) const {return 0x0;};
  virtual unsigned int getchipmap (const unsigned int index) const {return 0x0;};
  virtual unsigned int getchain (const unsigned int index) const {return 0x0;};
  virtual unsigned int getchip (const unsigned int index) const {return 0x0;};
  virtual unsigned int getmicromodule (const unsigned int index) const {return 0x0;};
  virtual unsigned int getrocbond (const unsigned int index) const {return 0x0;};
  virtual unsigned int gethadc (const unsigned int index) const {return 0x0;};
  virtual unsigned int getladc (const unsigned int index) const {return 0x0;};
  virtual unsigned int getOnlineKey (const unsigned int index) const {return 0x0;};

 protected:
  MpcExRawHit() {}
 private:
  ClassDef(MpcExRawHit,1)

};

#endif
