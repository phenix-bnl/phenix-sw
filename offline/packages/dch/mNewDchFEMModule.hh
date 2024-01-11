#ifndef __MNEWDCHFEMMODULE_H__
#define __MNEWDCHFEMMODULE_H__

#include <dDchRaw.h>
#include <dDchFEM.h>
#include <dDchNibbleGhit.h>
#include <dDchGhitRaw.h>
#include <PHTable.hh>
#include <phool.h>

class PHDchAddressObject;
class PHCompositeNode;

class mNewDchFEMModule
{
public:
  mNewDchFEMModule();
  virtual ~mNewDchFEMModule(){}
  PHBoolean event(PHCompositeNode *);   

  void  print(const short) const;
  void  writeData(const short nibble, const short value);
  short readData(const short nibble) const;    

private:
  short insertEdges(const short leadTime, const short trailTime,const short ghitid);
  void  addRelation(const short, const short);
  void  replaceRelation(const short, const short);

  void insertRawHits();
  void initialize();

  PHDchAddressObject*     dchAddressObject;

  short theCounter;
  short theChannel;
  short index[2][2][20][2];

  TABLE_HEAD_ST  dDchRaw_h;
  DDCHRAW_ST *dDchRaw;
  TABLE_HEAD_ST  dDchGhitRaw_h;
  DDCHGHITRAW_ST *dDchGhitRaw;
  TABLE_HEAD_ST dDchFEM_h;
  DDCHFEM_ST *dDchFEM;
  TABLE_HEAD_ST dDchNibbleGhit_h;
  DDCHNIBBLEGHIT_ST *dDchNibbleGhit;

};
#endif /*__MNEWDCHFEMMODULE_H__*/












