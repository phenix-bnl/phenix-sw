#ifndef __PDBMUTRGRPCTRK_HH__
#define __PDBMUTRGRPCTRK_HH__

#include "PdbCalChan.hh"

#include <string>

class PdbMutrgRpcTrk : public PdbCalChan {
public:
  PdbMutrgRpcTrk();
  virtual ~PdbMutrgRpcTrk(){;}

  void Reset();

  void SetArm(int value){arm=value;}
  void SetOctant(int value){octant=value;}
  void SetStripMutrg(int station,int strip);
  void SetModuleRpc(int station,int radseg,int module);
  void SetStripRpc(int station,int radseg,int position,int strip);

  int GetArm(){return arm;}
  int GetOctant(){return octant;}
  int GetStripMutrg(int station);
  int GetModuleRpc(int station,int radseg);
  int GetStripRpc(int station,int radseg,int position);

  void WriteFormat(std::ostream&os) const;
  void Write(std::ostream& os, const std::string& delim=",") const;
  using TObject::Write;
  virtual void print() const;

protected:
  int CheckStationMutrg(int station);
  int CheckStationRpc(int station);
  int CheckRadsegRpc(int radseg);
  int CheckStripPositionRpc(int position);

public:
  enum{
    NSTATION=3,
    STATION_RPC0=0,
    STATION_RPC2=2,
    NRADSEG=3,
    NSTRIP_RPC=5
  };

protected:
  int arm;
  int octant;

  int strip_mutrg[NSTATION]; // [st0,st1,st2]

  int module_rpc0[NRADSEG];   // [3radsegs]
  int module_rpc2[NRADSEG];   // [3radsegs]
  int strip_rpc0[NRADSEG][NSTRIP_RPC]; // [3radsegs][left,center,right]
  int strip_rpc2[NRADSEG][NSTRIP_RPC]; // [3radsegs][left,center,right]

  ClassDef(PdbMutrgRpcTrk,1);
};

#endif /* __PDBMUTRGRPCTRK_HH__ */
