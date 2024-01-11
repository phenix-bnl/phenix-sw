#ifndef __DCHRAWTABLEv1_H
#define __DCHRAWTABLEv1_H

#include <iostream>
#include "DchRawTable.hh"

class DchRawOutv1;
class TClonesArray;

class DchRawTablev1 : public DchRawTable
{
 public:
  DchRawTablev1();
  DchRawTablev1(int);
  virtual ~DchRawTablev1();

  int AddRaw(DchRawOutv1* hit);
  int Expand(int);
  void Clear(Option_t *option="");
  void Reset();
  void identify(std::ostream& os = std::cout) const;
  int Entries() { return entries;}
  int Size();

  //----------------------------------
  DchRawOutv1* getRaw(int i);
  TClonesArray* getRaws(){return DchRaws;}
  
  short getId(int i);
  int   getGlobal(int i) ;
  short getArm(int i);					
  short getPlane(int i);					
  short getCell(int i);					
  short getSide(int i);
  int   getTime(int i);
  short getEdge(int i);

  void  setId(int i, short val);
  void  setGlobal(int i, int val) ;
  void  setArm(int i, short val);					
  void  setPlane(int i, short val);					
  void  setCell(int i, short val);					
  void  setSide(int i,short val);
  void  setTime(int i, int val);
  void  setEdge(int i, short val);
  //--------------------------------

protected: 
  int entries;
  TClonesArray *DchRaws;
  ClassDef(DchRawTablev1,1)
};

#endif
