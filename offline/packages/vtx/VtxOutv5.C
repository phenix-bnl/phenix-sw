#include <PHPoint.h>
#include <VtxOutv5.h>
#include <VtxBits.h>

#include <cstring>
#include <iostream>

ClassImp(VtxOutv5)

using namespace std;

VtxOutv5::VtxOutv5()
{
  Reset(); // Init all data members
  return;
}

VtxOutv5&
VtxOutv5::operator=(const VtxOut &source)
{
  if ( this != &source ) 
    {
      const VtxOutv5 *bla = dynamic_cast<const VtxOutv5 *> (&source);
      if (bla)
	{
	  //      bla->copyTo(*this);
	}
      else
	{
	  cout << PHWHERE << "no = operator between different objects" << endl;
	}
    }
  return *this;
}

void 
VtxOutv5::identify(ostream& out) const
{
  PHPoint vtx, vtxerr;

  out << "I am a VtxOutv5 object. My list of vertexes:" << endl;
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
  if (VtxList & VTX::CGLBIT)
    {
      out << "Cgl: z = " << CglVtx << ", dz = " << CglVtxErr << endl;
    }
  if (VtxList & VTX::SETVTXBIT)
    {
      out << "Forced Vtx" 
	  << ": x = " << DefaultVtx[0] << ", dx = " << DefaultVtxErr[0]
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

  return;
}

void 
VtxOutv5::Reset()  // reset data members (called by phool node reset)
{
  VtxOutv4::Reset();

  CglVtx = VTX_INVALID_FLOAT;
  CglVtxErr = VTX_INVALID_FLOAT;

  return;
}

PHPoint 
VtxOutv5::get_Vertex() const
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
    case CGLVTX:
      vertex.setZ(CglVtx);
      break;
    default:
      vertex.setZ(DefaultVtx[2]);
      break;
    }

  return vertex;
}

PHPoint 
VtxOutv5::get_VertexError() const
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
    case CGLVTX:
      vertexerror.setZ(CglVtxErr);
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
int VtxOutv5::ChooseVtx() const
{
  if (VtxList & VTX::CGLBIT)
    {
      return CGLVTX;
    }

  return VtxOutv4::ChooseVtx();
}

float 
VtxOutv5::get_ZVertex() const
{
  if (ChooseVtx() == CGLVTX)
    {
      return CglVtx;
    }
  
  return VtxOutv4::get_ZVertex();
}

float 
VtxOutv5::get_ZVertexError() const
{
  if (ChooseVtx() == CGLVTX)
    {
      return CglVtxErr;
    }
  
  return VtxOutv4::get_ZVertexError();
}

int 
VtxOutv5::set_CglVtx(const float vtx, const float vtxerr)
{
  CglVtx = vtx;
  CglVtxErr = vtxerr;
  VtxList |= VTX::CGLBIT;
  return VTX_SUCCESS_INT;
}

bool 
VtxOutv5::isCglVtx() const
{
  return VtxList & VTX::CGLBIT;
}

// The apropriate bit is set if it was set by filling the vertex with
// &= which also removes all other bits thus making sure the chosen
// one is the only one to be considered
int 
VtxOutv5::use_Vertex(const char *subsystem)
{
  if (!strcmp(subsystem, "CGL"))
    {
      VtxList &= VTX::CGLBIT;
      return VTX_SUCCESS_INT;
    }

  return VtxOutv4::use_Vertex(subsystem);
}

const char *
VtxOutv5::which_Vtx() const
{
  if (ChooseVtx() == CGLVTX)
    {
      return "CGL";
    }

  return VtxOutv4::which_Vtx();
}
