#ifndef MPCEXEVENTHEADER_H
#define MPCEXEVENTHEADER_H

#include <PHObject.h>

class MpcExEventHeader: public PHObject
{
 public:
  virtual ~MpcExEventHeader() {}
  virtual void Reset() {}
  virtual unsigned int setStack(int multi) {return 0x0;}
  virtual unsigned int setStatephase(const std::vector<unsigned short> &vec) {return 0;}
  virtual unsigned int setCellIDs(const std::vector<unsigned short> &vec) {return 0;}
  virtual unsigned int setPARSTTime(const std::vector<unsigned short> &vec) {return 0;}
  
  virtual unsigned int getStatephaseSize() const { return 0;}
  virtual unsigned int getCellIDsSize() const { return 0;}

  virtual unsigned int getStack() const {return 0x0;}
  virtual unsigned int getStatephase(const unsigned int index) const {return 0x0;}
  virtual unsigned int getCellIDs(const unsigned int index) const {return 0x0;}
  virtual unsigned int getPARSTTime(const unsigned int index) const {return 0x0;}

  virtual unsigned int getStatephaseArm(const unsigned int index) const { return 0x0;}
  virtual unsigned int getStatephasePkt(const unsigned int index) const { return 0x0;}
  virtual unsigned int getStatephaseValue(const unsigned int index) const { return 0x0;}

  virtual unsigned int getCellIDsArm(const unsigned int index) const { return 0x0;}
  virtual unsigned int getCellIDsPkt(const unsigned int index) const { return 0x0;}
  virtual unsigned int getCellIDsSVXID(const unsigned int index) const { return 0x0;}
  virtual unsigned int getCellIDsValue(const unsigned int index) const { return 0x0;}

 protected:
  MpcExEventHeader() {}
	 
 private:
  ClassDef(MpcExEventHeader,1)
};

#endif
