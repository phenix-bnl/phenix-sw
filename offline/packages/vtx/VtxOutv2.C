#include <VtxOutv2.h>
#include <VtxBits.h>

#include <cmath>
#include <cstring>
#include <iostream>

ClassImp(VtxOutv2)

using namespace std;

VtxOutv2::VtxOutv2()
{
  Reset(); // Init all data members
  return ;
}

void VtxOutv2::identify(ostream& out) const
{
  PHPoint vtx, vtxerr;

  out << "identify yourself: I am a VtxOutv2 object, list of vertices:" << endl;
  if (VtxList & VTX::MVDBIT)
    {
      out << "MVD: x = " << MvdVtx[0] << ", dx = " << MvdVtxErr[0]
	  << ", y = " << MvdVtx[1] << ", dy = " << MvdVtxErr[1]
	  << ", z = " << MvdVtx[2] << ", dz = " << MvdVtxErr[2]
	  << ", ConfLevel = " << MvdConfLevel
	  << endl;
    }
  if (VtxList & VTX::PADBIT)
    {
      out << "Pad: z = " << PadVtx << ", dz = " << PadVtxErr << endl;
    }
  if (VtxList & VTX::BBCBIT)
    {
      out << "Bbc: z = " << BbcVtx << ", dz = " << BbcVtxErr << endl;
    }
  if (VtxList & VTX::ZDCBIT)
    {
      out << "Zdc: z = " << ZdcVtx << ", dz = " << ZdcVtxErr << endl;
    }
  if (VtxList & VTX::SETVTXBIT)
    {
      out << "Forced Vtx: x = " << DefaultVtx[0] << ", dx = " << DefaultVtxErr[0]
	  << ", y = " << DefaultVtx[1] << ", dy = " << DefaultVtxErr[1]
	  << ", z = " << DefaultVtx[2] << ", dz = " << DefaultVtxErr[2]
	  << endl;
    }
  if (get_VtxList())
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

void VtxOutv2::Reset()  // reset data members (called by phool node reset)
{
  int i;
  VtxList = 0;
  BbcVtx = VTX_INVALID_FLOAT;
  BbcVtxErr = VTX_INVALID_FLOAT;
  for (i = 0;i < 3;i++)
    {
      MvdVtx[i] = VTX_INVALID_FLOAT;
      MvdVtxErr[i] = VTX_INVALID_FLOAT;
    }
  MvdConfLevel = VTX_INVALID_FLOAT;
  PadVtx = VTX_INVALID_FLOAT;
  PadVtxErr = VTX_INVALID_FLOAT;
  ZdcVtx = VTX_INVALID_FLOAT;
  ZdcVtxErr = VTX_INVALID_FLOAT;
  DefaultVtx[0] = 0.;
  DefaultVtx[1] = 0.;
  DefaultVtx[2] = 0.;
  DefaultVtxErr[0] = 10.;
  DefaultVtxErr[1] = 10.;
  DefaultVtxErr[2] = 40.;
  return ;
}

int VtxOutv2::isValid() const
{
  if (ChooseVtx() == VTX_INVALID_INT)
    {
      return 0;
    }
  else
    {
      return 1;
    }
}


PHPoint VtxOutv2::get_Vertex() const
{
  int ivtx;
  ivtx = ChooseVtx();
  PHPoint vertex;
  if (ivtx == MVDVTX)
    {
      vertex.setX(MvdVtx[0]);
      vertex.setY(MvdVtx[1]);
      vertex.setZ(MvdVtx[2]);
      return vertex;
    }
  else if (ivtx == SETVTX)
    {
      vertex.setX(DefaultVtx[0]);
      vertex.setY(DefaultVtx[1]);
      vertex.setZ(DefaultVtx[2]);
      return vertex;
    }
  vertex.setX(DefaultVtx[0]);  // here we fill in measured values
  vertex.setY(DefaultVtx[1]);
  switch (ivtx)                 // and try to fill z here
    {
    case PADVTX:
      vertex.setZ(PadVtx);
      break;
    case BBCVTX:
      vertex.setZ(BbcVtx);
      break;
    case ZDCVTX:
      vertex.setZ(ZdcVtx);
      break;
    default:
      vertex.setX(VTX_INVALID_FLOAT);
      vertex.setY(VTX_INVALID_FLOAT);
      vertex.setZ(VTX_INVALID_FLOAT);
      break;
    }
  return vertex;
}

PHPoint VtxOutv2::get_VertexError() const
{
  int ivtx;
  PHPoint vertexerror;
  ivtx = ChooseVtx();
  if (ivtx == MVDVTX)
    {
      vertexerror.setX(MvdVtxErr[0]);
      vertexerror.setY(MvdVtxErr[1]);
      vertexerror.setZ(MvdVtxErr[2]);
      return vertexerror;
    }
  else if (ivtx == SETVTX)
    {
      vertexerror.setX(DefaultVtxErr[0]);
      vertexerror.setY(DefaultVtxErr[1]);
      vertexerror.setZ(DefaultVtxErr[2]);
      return vertexerror;
    }
  vertexerror.setX(DefaultVtxErr[0]);
  vertexerror.setY(DefaultVtxErr[1]);
  switch (ivtx)
    {
    case PADVTX:
      vertexerror.setZ(PadVtxErr);
      break;
    case BBCVTX:
      vertexerror.setZ(BbcVtxErr);
      break;
    case ZDCVTX:
      vertexerror.setZ(ZdcVtxErr);
      break;
    default:
      vertexerror.setX(VTX_INVALID_FLOAT);
      vertexerror.setY(VTX_INVALID_FLOAT);
      vertexerror.setZ(VTX_INVALID_FLOAT);
      break;
    }
  return vertexerror;
}

float VtxOutv2::get_ZVertex() const
{
  PHPoint vertex;
  vertex = get_Vertex();
  return vertex.getZ();
}

float VtxOutv2::get_ZVertexError() const
{
  PHPoint vertexerr;
  vertexerr = get_VertexError();
  return vertexerr.getZ();
}

/*
here we set the order in which the vertices are used:
1. Set Vertex
2. Bbc
3. Zdc
4. Mvd
   Pad (is not filled)
*/
int VtxOutv2::ChooseVtx() const
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
  if (VtxList & VTX::MVDBIT)
    {
      if (MvdConfLevel > 0.9)
	{
	  if (VtxList & VTX::BBCBIT)
	    {
	      if (fabs(MvdVtx[2] - BbcVtx) < 1.)
		{
		  return (MVDVTX);
		}
	    }
	  if (VtxList & VTX::ZDCBIT)
	    {
	      if (fabs(MvdVtx[2] - ZdcVtx) < 1.)
		{
		  return (MVDVTX);
		}
	    }
	}
    }
  if (VtxList & VTX::PADBIT)
    {
      return (PADVTX);
    }
  return (VTX_INVALID_INT);
}

bool VtxOutv2::isMvdVtx() const
{
  if (VtxList & VTX::MVDBIT)
    {
      if (MvdConfLevel > 0.9)
	{
	  if (VtxList & VTX::BBCBIT)
	    {
	      if (fabs(MvdVtx[2] - BbcVtx) < 1.)
		{
		  return true;
		}
	    }
	  if (VtxList & VTX::ZDCBIT)
	    {
	      if (fabs(MvdVtx[2] - ZdcVtx) < 1.)
		{
		  return true;
		}
	    }
	}
    }
  return false;
}

int VtxOutv2::set_MvdVtx(const PHPoint *vtx, const PHPoint *vtxerr, const float conflevel)
{
  MvdVtx[0] = vtx->getX();
  MvdVtx[1] = vtx->getY();
  MvdVtx[2] = vtx->getZ();
  MvdVtxErr[0] = vtxerr->getX();
  MvdVtxErr[1] = vtxerr->getY();
  MvdVtxErr[2] = vtxerr->getZ();
  MvdConfLevel = conflevel;
  VtxList |= VTX::MVDBIT;
  return (VTX_SUCCESS_INT);
}

int VtxOutv2::set_DefaultVtx(const PHPoint *vtx, const PHPoint *vtxerr)
{
  DefaultVtx[0] = vtx->getX();
  DefaultVtx[1] = vtx->getY();
  DefaultVtx[2] = vtx->getZ();
  DefaultVtxErr[0] = vtxerr->getX();
  DefaultVtxErr[1] = vtxerr->getY();
  DefaultVtxErr[2] = vtxerr->getZ();
  return (VTX_SUCCESS_INT);
}

int VtxOutv2::use_Vertex(const PHPoint *vtx, const PHPoint *vtxerr)
{
  set_DefaultVtx(vtx, vtxerr);
  VtxList |= VTX::SETVTXBIT;
  return (VTX_SUCCESS_INT);
}

int VtxOutv2::use_Vertex(const char *subsystem)
{
  if (!strcmp(subsystem, "MVD"))
    {
      VtxList &= VTX::MVDBIT;
    }
  else if (!strcmp(subsystem, "BBC"))
    {
      VtxList &= VTX::BBCBIT;
    }
  else if (!strcmp(subsystem, "ZDC"))
    {
      VtxList &= VTX::ZDCBIT;
    }
  else if (!strcmp(subsystem, "PAD"))
    {
      VtxList &= VTX::PADBIT;
    }
  else
    {
      cout << PHWHERE << " invalid subsystem string: " << subsystem << endl;
      cout << "Valid subsystems are BBC, MVD, PAD, ZDC" << endl;
      return (VTX_INVALID_INT);
    }
  return (VTX_SUCCESS_INT);
}

const char *VtxOutv2::which_Vtx() const
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

