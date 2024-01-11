#include <map>

#include <TClass.h>
#include <TClonesArray.h>

#include <phool.h>
#include <VtxOutv7.h>
#include <VtxSnglVtxv2.h>

#include <iostream>

ClassImp(VtxOutv7)

using namespace std;

// default value of TClonesArray: 4 covers almost all events
static const int VTXNOUTS = 4;

VtxOutv7::VtxOutv7()
{
  // the ctor of v6 creates its TClonesArray, delete it again here
  if (VtxSngl)
    {
      delete VtxSngl;
    }
  VtxSngl = new TClonesArray("VtxSnglVtxv2", VTXNOUTS);
}

VtxOutv7::VtxOutv7(const VtxOutv7& source)
{
  VtxSngl = NULL;
  source.copyTo(*this);
}

VtxOutv7::~VtxOutv7()
{
  if (VtxSngl)
    {
      VtxSngl->Delete();
      delete VtxSngl;
      VtxSngl = NULL;
    }
  return ;
}

void VtxOutv7::identify(ostream& os) const
{
  os << "identify yourself: VtxOutv7 Object" << endl;
  identifyv6(os);
  for (int i=0; i<VtxSngl->GetEntriesFast(); i++)
    {
      VtxSnglVtx *snglvtxin = static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(i));
      snglvtxin->identify(os);
    }
  return;
}

int VtxOutv7::DeepCopy(const VtxOut *vtxout)
{
  const VtxOutv7 *vtx7 = dynamic_cast<const VtxOutv7 *> (vtxout);
  if (vtx7)
    {
      if (this != vtx7)
	{
	  vtx7->copyTo(*this);
          return 0;
	}
      cout << PHWHERE << "do not deepdcopy object to itself" << endl;
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

//_____________________________________________________________
void VtxOutv7::copyTo(VtxOutv7& dest) const
{
  if (! dest.VtxSngl)
  { 
    dest.VtxSngl = new TClonesArray(VtxSngl->GetClass()->GetName(), VTXNOUTS);    
  } else {
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
  return;
}

//_______________________________________________________________________________________________
void VtxOutv7::AddVtx(const char *name, const float *vertex, const float *vertexerr, const VTX::Order order)
{
  short int short_order = order;
  AddVtx(name, vertex, vertexerr, short_order);
  return ;
}

void 
VtxOutv7::AddVtx(const char *name, const float *vertex, const float *vertexerr, short int order)
{

  // overwrite existing vertex if one by this name is already present
  for (int i = 0; i <= VtxSngl->GetLast(); i++)
  {
    VtxSnglVtx *tmp = static_cast<VtxSnglVtx *> (VtxSngl->UncheckedAt(i));
    if (!strcmp(tmp->Name(), name))
    {
      tmp->Vtx(0,vertex[0]);
      tmp->Vtx(1,vertex[1]);
      tmp->Vtx(2,vertex[2]);
      tmp->VtxErr(0,vertexerr[0]);
      tmp->VtxErr(1,vertexerr[1]);
      tmp->VtxErr(2,vertexerr[2]);
      tmp->SubSystem(order);
      if (order > 0) { VtxOrder[ (int) order ] = i;}
      return;
    }
  }
  
  int ivtx = VtxSngl->GetLast()+1;

  // Expand Vertex TClonesArray if neccessary
  if (ivtx == VtxSngl->GetSize())
  {
    VtxSngl->Expand((VtxSngl->GetSize()+VTXNOUTS));
  }
 
  TClonesArray &vtxsngl = *VtxSngl;
  new(vtxsngl[ivtx]) VtxSnglVtxv2(name, vertex, vertexerr, order);
  
  // if order <= 0 the vertex does not take part in vertex selection
  if (order > 0) { VtxOrder[ (int)order ] = ivtx; }
  
  return ;
}

