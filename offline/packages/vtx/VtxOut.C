
#include <PHPoint.h>
#include <PHObject.h>
#include <VtxReturncodes.h>
#include <VtxOut.h>

#include <cmath>
#include <iostream>

ClassImp(VtxOut)

using namespace std;

//_____________________________________________________________
VtxOut* VtxOut::clone() const
{
  cerr << "VtxOut::clone() not implemented by daughter class. Sorry"
       << endl;
  return 0;
}

//_____________________________________________________________
int VtxOut::isValid() const
{
  cout << "VtxOut: isValid() not implemented by daughter class" << endl;
  return 0;
}

//_____________________________________________________________
void VtxOut::identify(ostream& out) const
{
  out << "virtual VtxOut object" << endl;
  return ;
}

//_____________________________________________________________
void VtxOut::Reset()
{
  cout << "ERROR VtxOut: Reset() not implemented by daughter class" << endl;
  return ;
}

//_____________________________________________________________
void VtxOut::FillFromClass(const VtxOut *vtxout)
{
  virtual_warning("FillFromClass(const VtxOut *vtxout)");
  return ;
}

//_____________________________________________________________
int VtxOut::DeepCopy(const VtxOut *vtxout)
{
  virtual_warning("DeepCopy(const VtxOut *vtxout)");
  return -1;
}

//_____________________________________________________________
void 
VtxOut::AddVtx(const char *name, const float *vertex, const float *vertexerr, const short int order)
{
  virtual_warning("AddVtx(const char *name, const float *vertex, const float *vertexerr, const short int order)");
  return ;
}

void 
VtxOut::AddVtx(const char *name, const float *vertex, const float *vertexerr, const VTX::Order order)
{
  virtual_warning("AddVtx(const char *name, const float *vertex, const float *vertexerr, const VTX::Order order)");
  return ;
}

//_____________________________________________________________
int VtxOut::set_BbcVtx(const float vtx, const float vtxerr)
{
  virtual_warning("set_BbcVtx(const float vtx, const float vtxerr)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::set_BbcVtx(const float vtx)
{
  virtual_warning("set_BbcVtx(const float vtx)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::set_MvdVtx(const PHPoint *vtx, const PHPoint *vtxerr)
{
  virtual_warning("set_MvdVtx(const PHPoint *vtx, const PHPoint *vtxerr)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::set_MvdVtx(const PHPoint *vtx, const PHPoint *vtxerr, const float conflevel)
{
  virtual_warning("set_MvdVtx(const PHPoint *vtx, const PHPoint *vtxerr, const float conflevel)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::set_MvdVtx(const PHPoint *vtx)
{
  virtual_warning("set_MvdVtx(const PHPoint *vtx)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::set_MuonVtx(const float vtx, const float vtxerr)
{
  virtual_warning("set_MuonVtx(const float vtx, const float vtxerr)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::set_MuonVtx(const float vtx)
{
  virtual_warning("set_MuonVtx(const float vtx)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::set_NtcVtx(const float vtx, const float vtxerr)
{
  virtual_warning("set_NtcVtx(const float vtx, const float vtxerr)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::set_CglVtx(const float vtx, const float vtxerr)
{
  virtual_warning("set_NtcVtx(const float vtx, const float vtxerr)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::set_PadVtx(const float vtx, const float vtxerr)
{
  virtual_warning("set_PadVtx(const float vtx, const float vtxerr)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::set_ZdcVtx(const float vtx, const float vtxerr)
{
  virtual_warning("set_ZdcVtx(const float vtx, const float vtxerr)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::set_ZdcVtx(const float vtx)
{
  virtual_warning("set_ZdcVtx(const float vtx)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::set_DefaultVtx(const PHPoint *vtx, const PHPoint *vtxerr)
{
  virtual_warning("set_DefaultVtx(const PHPoint *vtx, const PHPoint *vtxerr)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::set_Vtx(const PHPoint *vtx)
{
  virtual_warning("set_Vtx(const PHPoint *vtx)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
PHPoint VtxOut::get_Vertex() const
{
  PHPoint invalid(NAN, NAN, NAN);
  virtual_warning("get_Vertex()");
  return invalid;
}

//_____________________________________________________________
PHPoint VtxOut::get_Vertex(const char *name) const
{
  PHPoint invalid(NAN, NAN, NAN);
  virtual_warning("get_Vertex", name);
  return invalid;
}

//_____________________________________________________________
PHPoint VtxOut::get_VertexError() const
{
  PHPoint invalid(NAN, NAN, NAN);
  virtual_warning("get_VertexError()");
  return invalid;
}

//_____________________________________________________________
PHPoint VtxOut::get_VertexError(const char *name) const
{
  PHPoint invalid(NAN, NAN, NAN);
  virtual_warning("get_VertexError", name);
  return invalid;
}

//_____________________________________________________________
float VtxOut::get_ZVertex() const
{
  virtual_warning("get_ZVertex()");
  return NAN;
}

//_____________________________________________________________
float VtxOut::get_ZVertex(const char *name) const
{
  virtual_warning("get_Vertex", name);
  return NAN;
}

//_____________________________________________________________
float VtxOut::get_ZVertexError() const
{
  virtual_warning("get_ZVertexError()");
  return NAN;
}

//_____________________________________________________________
float VtxOut::get_ZVertexError(const char *name) const
{
  virtual_warning("get_ZVertexError", name);
  return NAN;
}

//_____________________________________________________________
PHPoint VtxOut::get_MvdVertex() const
{
  PHPoint invalid(NAN, NAN, NAN);
  virtual_warning("get_MvdVertex()");
  return invalid;
}

//_____________________________________________________________
PHPoint VtxOut::get_MvdVertexError() const
{
  PHPoint invalid(NAN, NAN, NAN);
  virtual_warning("get_MvdVertexError()");
  return invalid;
}

//_____________________________________________________________
int VtxOut::get_VtxList() const
{
  virtual_warning("get_VtxList()");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::use_Vertex(const PHPoint *vtx, const PHPoint *vtxerr)
{
  virtual_warning("use_Vertex(const PHPoint *vtx, const PHPoint *vtxerr)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
int VtxOut::use_Vertex(const char *subsystem)
{
  virtual_warning("use_Vertex(const char *subsystem)");
  return VTX_INVALID_INT;
}

//_____________________________________________________________
bool VtxOut::isVtx(const char *name) const
{
  virtual_warning("isVtx",name);
  return false;
}

//_____________________________________________________________
const char *VtxOut::which_Vtx() const
{
  virtual_warning("*which_Vtx()");
  return "NONE";
}

//_____________________________________________________________
void VtxOut::virtual_warning(const char *funcsname) const
{
  cout << "VtxOut::" << funcsname << " is virtual, doing nothing" << endl;
  return ;
}

//_____________________________________________________________
void VtxOut::virtual_warning(const char *funcsname, const char *param) const
{
  cout << "VtxOut::" << funcsname << "(" << param << ") is virtual, doing nothing" << endl;
  return ;
}
