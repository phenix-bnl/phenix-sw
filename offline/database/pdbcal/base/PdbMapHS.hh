//  Declaration of class PdbMapHS
//  Purpose: User defined storage class for TEC
//  Author: lebedev

#ifndef __PDBMAPHS_HH__
#define __PDBMAPHS_HH__

#include "PdbCalChan.hh"
#ifndef __CINT__
#include <cstddef>
#endif

class PdbMapHS : public PdbCalChan 
{
public:

  enum { arm=0, sector=1, side=2, plane=3, firstwire=4, lastwire=5, crate=6,
	 slot=7, psaddress=8, firstchannel=9, lastchannel=10, flag=11};
 
  PdbMapHS();
  PdbMapHS(int arm, int sector, int side,
	   int plane, int firstwire, int lastwire, int crate,
	   int slot, int psaddress, int firstchannel, 
	   int lastchannel, int flag);
  PdbMapHS(const PdbMapHS &);

  virtual ~PdbMapHS();

  size_t getNdim() const { return nDim;}
  int  getParameter(size_t) const;
  const  char* getParName(size_t) const;

  void setParameter(size_t, int);
  void setAllParameters(int, int, int, int, int, int,
			int, int, int, int, int, int);

  virtual void print() const;

private:
  size_t nDim;
  int iArm;
  int iSector;
  int iSide;
  int iPlane;
  int iFirstWire;
  int iLastWire;
  int iCrate;
  int iSlot;
  int iPSAddress;
  int iFirstChannel;
  int iLastChannel;
  int iFlag;

  ClassDef(PdbMapHS,1);
};

#endif /* __PDBMAPHS_HH__ */
