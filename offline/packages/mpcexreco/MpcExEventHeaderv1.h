#ifndef MPCEXEVENTHEADERV1_H
#define MPCEXEVENTHEADERV1_H

#include "MpcExEventHeader.h"

#include <vector>

class MpcExEventHeaderv1: public MpcExEventHeader
{
 public:
  MpcExEventHeaderv1();
  virtual ~MpcExEventHeaderv1();
  void Reset() {}
  unsigned int setStack(int multi) {stack = multi; return 0;}
  unsigned int setStatephase(const std::vector<unsigned short> &vec);
  unsigned int setCellIDs(const std::vector<unsigned short> &vec);
  
  unsigned int getStatephaseSize() const { return statephases.size();};
  unsigned int getCellIDsSize() const { return cellid.size();};

  unsigned int getStack() const 
  { 
    if (stack>4) std::cout << "MpcExEventHeader: Invalid stack value = " << (int) stack << std::endl;
    return stack;
  }
  unsigned int getStatephase(const unsigned int index) const;
  unsigned int getCellIDs(const unsigned int index) const;

  unsigned int getStatephaseArm(const unsigned int index) const;
  unsigned int getStatephasePkt(const unsigned int index) const;
  unsigned int getStatephaseValue(const unsigned int index) const;

  unsigned int getCellIDsArm(const unsigned int index) const;
  unsigned int getCellIDsPkt(const unsigned int index) const;
  unsigned int getCellIDsSVXID(const unsigned int index) const;
  unsigned int getCellIDsValue(const unsigned int index) const;

 protected:
  unsigned char stack;
  std::vector<unsigned short> statephases;
  std::vector<unsigned short> cellid;

 private:
  ClassDef(MpcExEventHeaderv1,1)
};

#endif
