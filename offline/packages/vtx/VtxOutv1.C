#include <iostream>
#include "VtxOutv1.h"
#include "VtxBits.h"

ClassImp(VtxOutv1)

using namespace std;

VtxOutv1::VtxOutv1()
{
  Reset(); // Init all data members
  return ;
}

void VtxOutv1::identify(ostream& out) const
{
  PHPoint vtx, vtxerr;

  out << "identify yourself: I am a VtxOutv1 object, list of vertices:" << endl;
  if (VtxList & VTX::MVDBIT)
    {
      out << "MVD: x = " << MvdVtx[0] << ", dx = " << MvdVtxErr[0]
	  << ", y = " << MvdVtx[1] << ", dy = " << MvdVtxErr[1]
	  << ", z = " << MvdVtx[2] << ", dz = " << MvdVtxErr[2]
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
  if (!VtxList)
    {
      out << "No vertex found by any subsystem" << endl;
    }

  if (get_VtxList())
    {
      vtx = get_Vertex();
      vtxerr = get_VertexError();
      cout << "Vertex of choice:";
      switch (ChooseVtx())
	{
	case MVDVTX:
	  out << " MVD";
	  break;
	case PADVTX:
	  out << " PAD";
	  break;
	case BBCVTX:
	  out << " BBC";
	  break;
	case ZDCVTX:
	  out << " ZDC";
	  break;
	default:
	  out << "ERROR ChooseVtx returned funky value!!!!";
	  break;
	}
      out << endl;
      out << "x = " << vtx.getX() << ", dx = " << vtxerr.getX()
	  << ", y = " << vtx.getY() << ", dy = " << vtxerr.getY()
	  << ", z = " << vtx.getZ() << ", dz = " << vtxerr.getZ()
	  << endl;
    }
  return ;
}

void VtxOutv1::Reset()  // reset data members (called by phool node reset)
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
  PadVtx = VTX_INVALID_FLOAT;
  PadVtxErr = VTX_INVALID_FLOAT;
  ZdcVtx = VTX_INVALID_FLOAT;
  ZdcVtxErr = VTX_INVALID_FLOAT;
  return ;
}

PHPoint VtxOutv1::get_Vertex() const
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
  vertex.setX(0.);             // here we might want to fill in measured values
  vertex.setY(0.);
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

PHPoint VtxOutv1::get_VertexError() const
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
  vertexerror.setX(10.);
  vertexerror.setY(10.);
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

/*
here we set the order in which the vertices are used:
1. Mvd
2. Pad (for no filed runs)
3. Bbc
4. Zdc
*/
int VtxOutv1::ChooseVtx() const
{
  if (VtxList & VTX::MVDBIT)
    {
      return (MVDVTX);
    }
  if (VtxList & VTX::PADBIT)
    {
      return (PADVTX);
    }
  if (VtxList & VTX::BBCBIT)
    {
      return (BBCVTX);
    }
  if (VtxList & VTX::ZDCBIT)
    {
      return (ZDCVTX);
    }
  return (VTX_INVALID_INT);
}

float VtxOutv1::get_ZVertex() const
{
  switch (ChooseVtx())
    {
    case MVDVTX:
      return (MvdVtx[2]);
    case PADVTX:
      return (PadVtx);
    case BBCVTX:
      return (BbcVtx);
    case ZDCVTX:
      return (ZdcVtx);
    default:
      return (VTX_INVALID_FLOAT);
    }
}

float VtxOutv1::get_ZVertexError() const
{
  switch (ChooseVtx())
    {
    case MVDVTX:
      return (MvdVtxErr[2]);
    case PADVTX:
      return (PadVtxErr);
    case BBCVTX:
      return (BbcVtxErr);
    case ZDCVTX:
      return (ZdcVtxErr);
    default:
      return (VTX_INVALID_FLOAT);
    }
}


PHPoint VtxOutv1::get_MvdVertex() const
{
  PHPoint vertex(MvdVtx[0], MvdVtx[1], MvdVtx[2]);
  return vertex;
}

PHPoint VtxOutv1::get_MvdVertexError() const
{
  PHPoint vertexerror(MvdVtxErr[0], MvdVtxErr[1], MvdVtxErr[2]);
  return vertexerror;
}


int VtxOutv1::set_BbcVtx(const float vtx, const float vtxerr)
{
  BbcVtx = vtx;
  BbcVtxErr = vtxerr;
  VtxList |= VTX::BBCBIT;
  return (VTX_SUCCESS_INT);
}

int VtxOutv1::set_MvdVtx(const PHPoint *vtx, const PHPoint *vtxerr)
{
  MvdVtx[0] = vtx->getX();
  MvdVtx[1] = vtx->getY();
  MvdVtx[2] = vtx->getZ();
  MvdVtxErr[0] = vtxerr->getX();
  MvdVtxErr[1] = vtxerr->getY();
  MvdVtxErr[2] = vtxerr->getZ();
  VtxList |= VTX::MVDBIT;
  return (VTX_SUCCESS_INT);
}

int VtxOutv1::set_PadVtx(const float vtx, const float vtxerr)
{
  PadVtx = vtx;
  PadVtxErr = vtxerr;
  VtxList |= VTX::PADBIT;
  return (VTX_SUCCESS_INT);
}

int VtxOutv1::set_ZdcVtx(const float vtx, const float vtxerr)
{
  ZdcVtx = vtx;
  ZdcVtxErr = vtxerr;
  VtxList |= VTX::ZDCBIT;
  return (VTX_SUCCESS_INT);
}

bool VtxOutv1::isBbcVtx() const
  {
    return VtxList&VTX::BBCBIT;
  }

bool VtxOutv1::isPadVtx() const
  {
    return VtxList&VTX::PADBIT;
  }

bool VtxOutv1::isZdcVtx() const
  {
    return VtxList&VTX::BBCBIT;
  }

bool VtxOutv1::isMvdVtx() const
  {
    return VtxList&VTX::MVDBIT;
  }
