#include <PHPoint.h>
#include <VtxOutv4.h>
#include <VtxBits.h>

#include <cstring>
#include <iostream>

ClassImp(VtxOutv4)

using namespace std;

VtxOutv4::VtxOutv4()
{
  Reset(); // Init all data members
  return ;
}

void VtxOutv4::identify(ostream& out) const
{
  PHPoint vtx, vtxerr;

  out << "identify yourself: I am a VtxOutv4 object, list of vertices:" << endl;
  if (VtxList & VTX::BBCBIT)
    {
      out << "Bbc: z = " << BbcVtx << ", dz = " << BbcVtxErr << endl;
    }
  if (VtxList & VTX::ZDCBIT)
    {
      out << "Zdc: z = " << ZdcVtx << ", dz = " << ZdcVtxErr << endl;
    }
  if (VtxList & VTX::MUONBIT)
    {
      out << "Unused Muon: z = " << MuonVtx << ", dz = " << MuonVtxErr << endl;
    }
  if (VtxList & VTX::NTCBIT)
    {
      out << "Ntc: z = " << NtcVtx << ", dz = " << NtcVtxErr << endl;
    }
  if (VtxList & VTX::SETVTXBIT)
    {
      out << "Forced Vtx: x = " << DefaultVtx[0] << ", dx = " << DefaultVtxErr[0]
	  << ", y = " << DefaultVtx[1] << ", dy = " << DefaultVtxErr[1]
	  << ", z = " << DefaultVtx[2] << ", dz = " << DefaultVtxErr[2]
	  << endl;
    }
  if (isValid())
    {
      vtx = get_Vertex();
      vtxerr = get_VertexError();
      cout << "Vertex of choice: " << which_Vtx() << endl;
      out << "x = " << vtx.getX() << ", dx = " << vtxerr.getX()
	  << ", y = " << vtx.getY() << ", dy = " << vtxerr.getY()
	  << ", z = " << vtx.getZ() << ", dz = " << vtxerr.getZ()
	  << endl;
    }
else
    {
      out << "No vertex found by any subsystem" << endl;
    }

  return ;
}

void VtxOutv4::Reset()  // reset data members (called by phool node reset)
{
  short i;
  VtxList = 0;
  BbcVtx = VTX_INVALID_FLOAT;
  BbcVtxErr = VTX_INVALID_FLOAT;
  MuonVtx = VTX_INVALID_FLOAT;
  MuonVtxErr = VTX_INVALID_FLOAT;
  NtcVtx = VTX_INVALID_FLOAT;
  NtcVtxErr = VTX_INVALID_FLOAT;
  ZdcVtx = VTX_INVALID_FLOAT;
  ZdcVtxErr = VTX_INVALID_FLOAT;
  for (i = 0;i < 3;i++)
    {
      DefaultVtx[i] = 0;
      DefaultVtxErr[i] = 10;
    }
  DefaultVtxErr[2] = 40;
  return ;
}

int VtxOutv4::isValid() const
{
  return ((VtxList&~VTX::MUONBIT)?1:0); // muons are not valid vertex yet
}

PHPoint VtxOutv4::get_Vertex() const
{
  int ivtx;
  ivtx = ChooseVtx();
  PHPoint vertex;
  vertex.setX(DefaultVtx[0]);
  vertex.setY(DefaultVtx[1]);
  switch (ivtx)                 // and try to fill z here
    {
    case BBCVTX:
      vertex.setZ(BbcVtx);
      break;
    case ZDCVTX:
      vertex.setZ(ZdcVtx);
      break;
    case NTCVTX:
      vertex.setZ(NtcVtx);
      break;
    default:
      vertex.setZ(DefaultVtx[2]);
      break;
    }
  return vertex;
}

PHPoint VtxOutv4::get_VertexError() const
{
  int ivtx;
  PHPoint vertexerror;
  ivtx = ChooseVtx();
  vertexerror.setX(DefaultVtxErr[0]);
  vertexerror.setY(DefaultVtxErr[1]);
  switch (ivtx)
    {
    case BBCVTX:
      vertexerror.setZ(BbcVtxErr);
      break;
    case ZDCVTX:
      vertexerror.setZ(ZdcVtxErr);
      break;
    case NTCVTX:
      vertexerror.setZ(NtcVtxErr);
      break;
    default:
      vertexerror.setZ(DefaultVtxErr[2]);
      break;
    }
  return vertexerror;
}

/*
here we set the order in which the vertices are used:
1. Set Vertex
2. Bbc
3. Zdc
4. Ntc
*/
int VtxOutv4::ChooseVtx() const
{
  if (VtxList & VTX::SETVTXBIT)
    {
      return (SETVTX);
    }
  if (VtxList & VTX::BBCBIT)
    {
      return (BBCVTX);
    }
  if (VtxList & VTX::ZDCBIT)
    {
      return (ZDCVTX);
    }
  if (VtxList & VTX::NTCBIT)
    {
      return (NTCVTX);
    }
  return (VTX_INVALID_INT);
}

float VtxOutv4::get_ZVertex() const
{
  switch (ChooseVtx())
    {
    case BBCVTX:
      return (BbcVtx);
    case ZDCVTX:
      return (ZdcVtx);
    case NTCVTX:
      return (NtcVtx);
    default:
      return (DefaultVtx[2]);
    }
}

float VtxOutv4::get_ZVertexError() const
{
  switch (ChooseVtx())
    {
    case BBCVTX:
      return (BbcVtxErr);
    case ZDCVTX:
      return (ZdcVtxErr);
    case NTCVTX:
      return (NtcVtxErr);
    default:
      return (DefaultVtxErr[2]);
    }
}


int VtxOutv4::set_BbcVtx(const float vtx, const float vtxerr)
{
  BbcVtx = vtx;
  BbcVtxErr = vtxerr;
  VtxList |= VTX::BBCBIT;
  return (VTX_SUCCESS_INT);
}

int VtxOutv4::set_NtcVtx(const float vtx, const float vtxerr)
{
  NtcVtx = vtx;
  NtcVtxErr = vtxerr;
  VtxList |= VTX::NTCBIT;
  return (VTX_SUCCESS_INT);
}

int VtxOutv4::set_MuonVtx(const float vtx, const float vtxerr)
{
  MuonVtx = vtx;
  MuonVtxErr = vtxerr;
  VtxList |= VTX::MUONBIT;
  return (VTX_SUCCESS_INT);
}

int VtxOutv4::set_ZdcVtx(const float vtx, const float vtxerr)
{
  ZdcVtx = vtx;
  ZdcVtxErr = vtxerr;
  VtxList |= VTX::ZDCBIT;
  return (VTX_SUCCESS_INT);
}

bool VtxOutv4::isBbcVtx() const
{
  return VtxList&VTX::BBCBIT;
}

bool VtxOutv4::isMuonVtx() const
{
  return VtxList&VTX::MUONBIT;
}

bool VtxOutv4::isNtcVtx() const
{
  return VtxList&VTX::NTCBIT;
}

bool VtxOutv4::isZdcVtx() const
{
  return VtxList&VTX::ZDCBIT;
}

// the apropriate bit is set if it was set by filling the vertex with
// &= which also removes all other bits thus making sure the chosen
// one is the only one to be considered
int VtxOutv4::use_Vertex(const char *subsystem)
{
  if (!strcmp(subsystem, "BBC"))
    {
      VtxList &= VTX::BBCBIT;
    }
  else if (!strcmp(subsystem, "ZDC"))
    {
      VtxList &= VTX::ZDCBIT;
    }
  else if (!strcmp(subsystem, "NTC"))
    {
      VtxList &= VTX::NTCBIT;
    }
  else
    {
      cout << PHWHERE << " invalid subsystem string: " << subsystem << endl;
      cout << "Valid subsystems are BBC, NTC, ZDC" << endl;
      return (VTX_INVALID_INT);
    }
  return (VTX_SUCCESS_INT);
}

int VtxOutv4::use_Vertex(const PHPoint *vtx, const PHPoint *vtxerr)
{
  set_DefaultVtx(vtx, vtxerr);
  VtxList |= VTX::SETVTXBIT;
  return (VTX_SUCCESS_INT);
}

int VtxOutv4::set_DefaultVtx(const PHPoint *vtx, const PHPoint *vtxerr)
{
  DefaultVtx[0] = vtx->getX();
  DefaultVtx[1] = vtx->getY();
  DefaultVtx[2] = vtx->getZ();
  DefaultVtxErr[0] = vtxerr->getX();
  DefaultVtxErr[1] = vtxerr->getY();
  DefaultVtxErr[2] = vtxerr->getZ();
  return (VTX_SUCCESS_INT);
}


const char *VtxOutv4::which_Vtx() const
{
  switch (ChooseVtx())
    {
      case MVDVTX:
        return ("MVD");
        break;
      case PADVTX:
        return ("PAD");
        break;
      case BBCVTX:
        return ("BBC");
        break;
      case NTCVTX:
        return ("NTC");
        break;
      case ZDCVTX:
        return ("ZDC");
        break;
      case SETVTX:
        return ("FORCED");
        break;
      case VTX_INVALID_INT:
        return ("NONE");
        break;
      default:
        cout << PHWHERE << " ERROR ChooseVtx returned funky value!!!!";
        return ("STRANGE");
        break;
    }
}
