#include "VtxOutv6.h"
#include "VtxSnglVtxv1.h"

#include <PHPoint.h>

#include <TClass.h>
#include <TClonesArray.h>

#include <cmath>
#include <set>

ClassImp(VtxOutv6)

using namespace std;

// default value of TClonesArray: 4 covers almost all events
static const int VTXNOUTS = 4;

VtxOutv6::VtxOutv6()
{
  VtxSngl = new TClonesArray("VtxSnglVtxv1", VTXNOUTS);
}

VtxOutv6::VtxOutv6(const VtxOutv6& source)
{
  VtxSngl = NULL;
  source.copyTo(*this);
}

int VtxOutv6::DeepCopy(const VtxOut *vtxout)
{
  const VtxOutv6 *vtx6 = dynamic_cast<const VtxOutv6 *> (vtxout);
  if (vtx6)
    {
      if (this != vtx6)
        {
          vtx6->copyTo(*this);
          return 0;
        }
      cout << PHWHERE << "do not deepdcopy object to itself, doing nothing" << endl;
      return 0;
    }
  else
    {
      cout << PHWHERE << "no copy between different objects, deepcopy from " << endl;
      vtxout->identify();
      cout << "to" << endl;
      identify();
    }
  return -1;
}


void
VtxOutv6::copyTo(VtxOutv6& dest) const
{
  if (! dest.VtxSngl)
    {
      dest.VtxSngl = new TClonesArray(VtxSngl->GetClass()->GetName(), VTXNOUTS);
    }
  else
    {
      dest.Reset();
    }
  dest.VtxSngl->ExpandCreate(VtxSngl->GetLast() + 1);
  for ( int i = 0; i <= VtxSngl->GetLast(); ++i )
    {
      VtxSnglVtx *snglvtxin = static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(i));
      VtxSnglVtx *snglvtxout = static_cast<VtxSnglVtx *> (dest.VtxSngl->UncheckedAt(i));
      *snglvtxout = *snglvtxin;
    }
  dest.VtxOrder = VtxOrder;
  return ;
}

VtxOutv6::~VtxOutv6()
{
  if (VtxSngl)
    {
      VtxSngl->Delete();
      delete VtxSngl;
      VtxSngl = NULL;
    }
  return ;
}

void VtxOutv6::identify(ostream& os) const
{
  os << "identify yourself: VtxOutv6 Object" << endl;
  identifyv6(os);
  return ;
}

void VtxOutv6::identifyv6(ostream& os) const
{
  map<int, int>::const_iterator iter;
  os << "No of stored Vtxs: " << VtxSngl->GetLast() + 1 << endl;
  for (iter = VtxOrder.begin(); iter != VtxOrder.end(); ++iter)
    {
      VtxSnglVtx *tmp = static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(iter->second));
      os << tmp->Name()
	 << ": x " << tmp->Vtx(0) << ", dx " << tmp->VtxErr(0)
	 << ", y " << tmp->Vtx(1) << ", dy " << tmp->VtxErr(1)
	 << ", z " << tmp->Vtx(2) << ", dz " << tmp->VtxErr(2)
	 << endl;
    }
  iter = VtxOrder.begin();
  if (iter != VtxOrder.end())
    {
      os << "Chosen Vertex: ";
      VtxSnglVtx *tmp = static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(iter->second));
      os << tmp->Name()
	 << " with x " << tmp->Vtx(0) << ", dx " << tmp->VtxErr(0)
	 << ", y " << tmp->Vtx(1) << ", dy " << tmp->VtxErr(1)
	 << ", z " << tmp->Vtx(2) << ", dz " << tmp->VtxErr(2)
	 << endl;
    }
  else
    {
      os << "No Vertex found" << endl;
    }
  // in case someone added a vertex which does not take part in vtx selection
  if ( static_cast<int>(VtxOrder.size()) != VtxSngl->GetLast() + 1)
    {
      set<int> tmpset;
      for (iter = VtxOrder.begin(); iter != VtxOrder.end(); ++iter)
	{
	  tmpset.insert(iter->second);
	}
      for (int i = 0; i <= VtxSngl->GetLast(); i++)
	{
	  if (tmpset.find(i) == tmpset.end())
	    {
	      os << "Vertex which is not used: ";
	      VtxSnglVtx *tmp = static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(i));
	      os << tmp->Name()
		 << " with x " << tmp->Vtx(0) << ", dx " << tmp->VtxErr(0)
		 << ", y " << tmp->Vtx(1) << ", dy " << tmp->VtxErr(1)
		 << ", z " << tmp->Vtx(2) << ", dz " << tmp->VtxErr(2)
		 << endl;
	    }
	}
    }
  return ;
}

void VtxOutv6::Reset()
{
  VtxSngl->Delete();
  VtxOrder.clear();
  // deflate TClonesArray again if size is larger than default
  if (VtxSngl->GetSize() > VTXNOUTS)
    {
      VtxSngl->Expand(VTXNOUTS);
    }
  return ;
}

int VtxOutv6::isValid() const
{
  return ((VtxOrder.size() > 0) ? 1 : 0);
}

//_____________________________________________________________
void VtxOutv6::AddVtx(const char *name, const float *vertex, const float *vertexerr, const VTX::Order order)
{
  short int short_order = order;
  AddVtx(name,vertex,vertexerr,short_order);
}
//_____________________________________________________________
void VtxOutv6::AddVtx(const char *name, const float *vertex, const float *vertexerr, const short int order)
{
  int ivtx = VtxSngl->GetLast() + 1;

  // Expand Vertex TClonesArray if neccessary
  if (ivtx == VtxSngl->GetSize())
    {
      VtxSngl->Expand((VtxSngl->GetSize() + VTXNOUTS));
    }

  TClonesArray &vtxsngl = *VtxSngl;
  new(vtxsngl[ivtx]) VtxSnglVtxv1(name, vertex, vertexerr, order);
  // if order <= 0 the vertex does not take part in vertex selection
  if (order > 0)
    {
      VtxOrder[order] = ivtx;
    }
  return ;
}

PHPoint VtxOutv6::get_Vertex() const
{
  PHPoint vertex;
  map<int, int>::const_iterator iter = VtxOrder.begin();
  if (iter != VtxOrder.end())
  {
    vertex.setX((static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(iter->second)))->Vtx(0));
    vertex.setY((static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(iter->second)))->Vtx(1));
    vertex.setZ((static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(iter->second)))->Vtx(2));
  } else {
    vertex.setX(NAN);
    vertex.setY(NAN);
    vertex.setZ(NAN);
  }
  
  return vertex;
}

PHPoint VtxOutv6::get_Vertex(const char *name) const
{
  PHPoint vertex;
  for (int i = 0; i <= VtxSngl->GetLast(); i++)
  {
    VtxSnglVtx *tmp = static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(i));
    if (!strcmp(tmp->Name(), name))
    {
      vertex.setX(tmp->Vtx(0));
      vertex.setY(tmp->Vtx(1));
      vertex.setZ(tmp->Vtx(2));
      return vertex;
    }
  }
  
  vertex.setX(NAN);
  vertex.setY(NAN);
  vertex.setZ(NAN);
  return vertex;
}

PHPoint VtxOutv6::get_VertexError() const
{
  PHPoint vertexerr;
  map<int, int>::const_iterator iter = VtxOrder.begin();
  if (iter != VtxOrder.end())
    {
      vertexerr.setX((static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(iter->second)))->VtxErr(0));
      vertexerr.setY((static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(iter->second)))->VtxErr(1));
      vertexerr.setZ((static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(iter->second)))->VtxErr(2));
    }
  else
    {
      vertexerr.setX(NAN);
      vertexerr.setY(NAN);
      vertexerr.setZ(NAN);
    }
  return vertexerr;
}

PHPoint VtxOutv6::get_VertexError(const char *name) const
{
  PHPoint vertexerror;
  for (int i = 0; i <= VtxSngl->GetLast(); i++)
    {
      VtxSnglVtx *tmp = static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(i));
      if (!strcmp(tmp->Name(), name))
	{
	  vertexerror.setX(tmp->VtxErr(0));
	  vertexerror.setY(tmp->VtxErr(1));
	  vertexerror.setZ(tmp->VtxErr(2));
	  return vertexerror;
	}
    }
  vertexerror.setX(NAN);
  vertexerror.setY(NAN);
  vertexerror.setZ(NAN);
  return vertexerror;
}


float VtxOutv6::get_ZVertex() const
{
  float vertex = NAN;
  map<int, int>::const_iterator iter = VtxOrder.begin();
  if (iter != VtxOrder.end())
    {
      vertex = (static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(iter->second)))->Vtx(2);
    }
  return vertex;
}

float VtxOutv6::get_ZVertexError() const
{
  float vertexerr = NAN;
  map<int, int>::const_iterator iter = VtxOrder.begin();
  if (iter != VtxOrder.end())
    {
      vertexerr = (static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(iter->second)))->VtxErr(2);
    }
  return vertexerr;
}

const char *VtxOutv6::which_Vtx() const
{
  map<int, int>::const_iterator iter = VtxOrder.begin();
  if (iter != VtxOrder.end())
  { return (static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(iter->second)))->Name(); }
  return "NONE";
}

float VtxOutv6::get_ZVertex(const char *name) const
{
  for (int i = 0; i <= VtxSngl->GetLast(); i++)
    {
      VtxSnglVtx *tmp = static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(i));
      if (!strcmp(tmp->Name(), name))
	{
	  return tmp->Vtx(2);
	}
    }
  return NAN;
}

float VtxOutv6::get_ZVertexError(const char *name) const
{
  for (int i = 0; i <= VtxSngl->GetLast(); i++)
    {
      VtxSnglVtx *tmp = static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(i));
      if (!strcmp(tmp->Name(), name))
	{
	  return tmp->VtxErr(2);
	}
    }
  return NAN;
}

bool VtxOutv6::isVtx(const char *name) const
{
  for (int i = 0; i <= VtxSngl->GetLast(); i++)
  {
    VtxSnglVtx *tmp = static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(i));
    if (!strcmp(tmp->Name(), name)) { return true; }
  }
  return false;
}
