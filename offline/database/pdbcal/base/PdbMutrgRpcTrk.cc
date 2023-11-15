#include "PdbMutrgRpcTrk.hh"

#include <iomanip>
#include <iostream>

using namespace std;

//////////////////////////////////////////////////////////////////////////

PdbMutrgRpcTrk::PdbMutrgRpcTrk(){
  Reset();
}

//////////////////////////////////////////////////////////////////////////

void PdbMutrgRpcTrk::Reset(){
  arm=-1;
  octant=-1;
  for(int i=0; i<NSTATION; i++){strip_mutrg[i]=-1;}
  for(int i=0; i<NRADSEG; i++){
    module_rpc0[i]=-1;
    module_rpc2[i]=-1;
    for(int j=0; j<NSTRIP_RPC; j++){
      strip_rpc0[i][j]=-1;
      strip_rpc2[i][j]=-1;
    }
  }
  return;
}

//////////////////////////////////////////////////////////////////////////////

void PdbMutrgRpcTrk::SetStripMutrg(int station,int strip){
  if(CheckStationMutrg(station)){return;}
  strip_mutrg[station]=strip;
  return;
}

/////////////////////////////////////////////////////////////////////////////

void PdbMutrgRpcTrk::SetModuleRpc(int station,int radseg,int module){
  if(CheckStationRpc(station)){return;}
  if(CheckRadsegRpc(radseg)){return;}

  int *module_rpc[3]={module_rpc0,NULL,module_rpc2};
  module_rpc[station][radseg]=module;

  return;
}

/////////////////////////////////////////////////////////////////////////////

void PdbMutrgRpcTrk::SetStripRpc(int station,int radseg,int position,int strip){
  if(CheckStationRpc(station)){return;}
  if(CheckRadsegRpc(radseg)){return;}
  if(CheckStripPositionRpc(position)){return;}

  int (*strip_rpc[3])[NSTRIP_RPC]={strip_rpc0,NULL,strip_rpc2};
  strip_rpc[station][radseg][position]=strip;

  return;
}

/////////////////////////////////////////////////////////////////////////////

int PdbMutrgRpcTrk::GetStripMutrg(int station){
  if(CheckStationMutrg(station)){return -1;}
  return strip_mutrg[station];
}

/////////////////////////////////////////////////////////////////////////////

int PdbMutrgRpcTrk::GetModuleRpc(int station,int radseg){
  if(CheckStationRpc(station)){return -1;}
  if(CheckRadsegRpc(radseg)){return -1;}

  int *module_rpc[3]={module_rpc0,NULL,module_rpc2};
  return module_rpc[station][radseg];
}

/////////////////////////////////////////////////////////////////////////////

int PdbMutrgRpcTrk::GetStripRpc(int station,int radseg,int position){
  if(CheckStationRpc(station)){return -1;}
  if(CheckRadsegRpc(radseg)){return -1;}
  if(CheckStripPositionRpc(position)){return -1;}

  int (*strip_rpc[3])[NSTRIP_RPC]={strip_rpc0,NULL,strip_rpc2};
  return strip_rpc[station][radseg][position];
}

/////////////////////////////////////////////////////////////////////////////

void PdbMutrgRpcTrk::WriteFormat(std::ostream& os) const {
  os << "# arm oct mutrg_strip(st0,st1,st2)";
  os << " rpc_st0 (module outer_stripx5)x3radsegs";
  os << " rpc_st2 (module outer_stripx5)x3radsegs";
  return;
}

/////////////////////////////////////////////////////////////////////////////

void PdbMutrgRpcTrk::Write(std::ostream& os, const std::string& delim) const {
  os << arm << delim << octant;
  for(int ist=0; ist<NSTATION; ist++){
    os << delim << setw(3) << strip_mutrg[ist];
  }

  os << delim << 0;
  for(int irad=0; irad<NRADSEG; irad++){
    os << delim << setw(1) << module_rpc0[irad];
    for(int ipos=0; ipos<NSTRIP_RPC; ipos++){
      os << delim << setw(2) << strip_rpc0[irad][ipos];
    }
  }

  os << delim << 2;
  for(int irad=0; irad<NRADSEG; irad++){
    os << delim << setw(1) << module_rpc2[irad];
    for(int ipos=0; ipos<NSTRIP_RPC; ipos++){
      os << delim << setw(2) << strip_rpc2[irad][ipos];
    }
  }

  return;
}

/////////////////////////////////////////////////////////////////////////////

void PdbMutrgRpcTrk::print() const {
  WriteFormat(cout);
  cout << endl;
  Write(cout);
  cout << endl;
  return;
}

/////////////////////////////////////////////////////////////////////////////

int PdbMutrgRpcTrk::CheckStationMutrg(int station){
  if(station<0 || station>=NSTATION){
    printf("Error - PdbMutrgRpcTrk : MuTRG station number must be 0 - 2\n");
    return -1;
  }

  return 0;
}

/////////////////////////////////////////////////////////////////////////////

int PdbMutrgRpcTrk::CheckStationRpc(int station){
  if(station!=0 && station!=2){
    printf("Error - PdbMutrgRpcTrk : RPC station number must be 0 or 2\n");
    return -1;
  }

  return 0;
}

//////////////////////////////////////////////////////////////////////////////

int PdbMutrgRpcTrk::CheckRadsegRpc(int radseg){
  if(radseg<0 || radseg>=NRADSEG){
    printf("Error - PdbMutrgRpcTrk : RPC radseg number must be 0 - 2\n");
    return -1;
  }

  return 0;
}

/////////////////////////////////////////////////////////////////////////////

int PdbMutrgRpcTrk::CheckStripPositionRpc(int position){
  if(position<0 || position>=NSTRIP_RPC){
    printf("Error - PdbMutrgRpcTrk : RPC strip position must be 0 - 2\n");
    return -1;
  }

  return 0;
}

/////////////////////////////////////////////////////////////////////////////
