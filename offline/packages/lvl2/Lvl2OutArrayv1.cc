#include <Lvl2OutArrayv1.h>
#include <Lvl2SnglOutv1.h>
#include <phool.h>

#include <iostream>
#include <cstdlib>

ClassImp(Lvl2OutArrayv1)

using namespace std;

static const unsigned int MAXPRIMITIVES = 200;

Lvl2OutArrayv1::Lvl2OutArrayv1()
{
  nL2Prim = 0;
  L2Prim = new TClonesArray("Lvl2SnglOutv1", MAXPRIMITIVES);
}

Lvl2OutArrayv1::~Lvl2OutArrayv1()
{
  if (L2Prim) delete L2Prim;
}

void Lvl2OutArrayv1::AddPrimitive(const unsigned int iprim)
{
  TClonesArray &a = *L2Prim;
  new (a[iprim]) Lvl2SnglOutv1();
}

void Lvl2OutArrayv1::RemovePrimitive(const unsigned int iprim)
{
  L2Prim->RemoveAt(iprim);
}

void Lvl2OutArrayv1::Reset()
{
  if ( L2Prim && nL2Prim>0 )
    {
      // The "C" option tels TClonesArray to call the Lvl2SnglOutv2
      // Clear method. This is needed to free the memory "newed" in
      // Lvl2SnglOutv2::Fill(). Tony Frawley, jan 28 2004.
      
      L2Prim->Clear("C");
    }
  //set_TClonesArraySize(nL2Prim);
  nL2Prim = 0;
}

int Lvl2OutArrayv1::isValid() const
{
  if (nL2Prim >= 0) return 1;
  return 0;
}

void Lvl2OutArrayv1::identify(ostream &os) const
{
  os << "identify yourself: Lvl2OutArrayv1 object" << endl;
}

int Lvl2OutArrayv1::set_TClonesArraySize(const unsigned int iprim)
{
  if ( iprim > MAXPRIMITIVES ) L2Prim->Expand(MAXPRIMITIVES);
  return iprim;
}

const char *Lvl2OutArrayv1::getname(const unsigned int iprim) const
{
  Lvl2SnglOutv1 *out = (Lvl2SnglOutv1 *)L2Prim->At(iprim);
  return ((out) ? out->getname() : "");
}

short Lvl2OutArrayv1::getversion(const unsigned int iprim) const
{
  Lvl2SnglOutv1 *out = (Lvl2SnglOutv1 *)L2Prim->UncheckedAt(iprim);
  return ((out) ? out->getversion() : 0);
}

void Lvl2OutArrayv1::setversion(const unsigned int iprim, short v)
{
  Lvl2SnglOutv1 *out = (Lvl2SnglOutv1 *)L2Prim->At(iprim);
  if (out) out->setversion(v);
  else     cout << PHWHERE << "ERROR no Lvl2SnglOutv1 object found" << endl;
}

Int_t Lvl2OutArrayv1::getdatalength(const unsigned int iprim) const
{
  Lvl2SnglOutv1 *out = (Lvl2SnglOutv1 *)L2Prim->At(iprim);
  return ((out) ? out->getdatalength() : -9999);
}

PHDWORD *Lvl2OutArrayv1::getdata(const unsigned int iprim) const
{
  Lvl2SnglOutv1 *out = (Lvl2SnglOutv1 *)L2Prim->At(iprim);
  return ((out) ? out->getdata() : 0);
}

void Lvl2OutArrayv1::setname(const unsigned int iprim, const char *src_ptr)
{
  Lvl2SnglOutv1 *out = (Lvl2SnglOutv1 *)L2Prim->At(iprim);
  if (out) out->setname(src_ptr);
  else     cout << PHWHERE << "ERROR no Lvl2SnglOutv1 object found" << endl;
}

short Lvl2OutArrayv1::getendianism(const unsigned int iprim) const
{
  Lvl2SnglOutv1 *out = (Lvl2SnglOutv1 *)L2Prim->At(iprim);
  return ((out) ? out->getendianism() : 0);
}

void Lvl2OutArrayv1::setendianism(const unsigned int iprim, short e)
{
  Lvl2SnglOutv1 *out = (Lvl2SnglOutv1 *)L2Prim->At(iprim);
  if (out) out->setendianism(e);
  else     cout << PHWHERE << "ERROR no Lvl2SnglOutv1 object found" << endl;
}

void Lvl2OutArrayv1::fill(const unsigned int iprim, UINT n, PHDWORD *src)
{
  Lvl2SnglOutv1 *out = (Lvl2SnglOutv1 *)L2Prim->At(iprim);
  if (out) out->fill(n, src);
  else     cout << PHWHERE << "ERROR no Lvl2SnglOutv1 object found" << endl;
}

int Lvl2OutArrayv1::find(const char *primitive) const
{
  int nentries = nL2Prim;
  int len = strlen(primitive);
  for (int i=0; i<nentries; i++)
    {
      if ( strncmp(primitive, getname(i), len) == 0 )
        {
          return i;	// found primitive
        }
    }
  return -1;	// failed to find primitive
}

//  GetPrimitive:
//  This method will check the array for the presence of any of the
//  registered Lvl2Primitives. This will enable the primitives to be
//  by the standard Lvl2Primitive pointers such as
//  L2MuiPairsP mupairs(0);
//  this returns 0 on failure

int Lvl2OutArrayv1::GetPrimitive(const char *primitive)
{

  cout << "Lvl2OutArrayv1::GetPrimitive is deprecated" << endl;
  exit(-1);
  return 0;
}


// To replace the contents of an this object with 
// copies of another's contents
void Lvl2OutArrayv1::ReplaceContents(Lvl2OutArray * replacer)  
{
  // I (this) am the replacee
  // so first reset myself
  Reset(); 
 
  for (int iprim=0; iprim<replacer->get_npart(); iprim++)
    {
      //-* add primitive
      AddPrimitive(iprim);
      setname(iprim, replacer->getname(iprim));
      setversion(iprim, replacer->getversion(iprim));
      
      //-* copy the primitive data into output Lvl2Out object
      UINT dataLength = replacer->getdatalength(iprim);
      PHDWORD* dataStart_ptr = replacer->getdata(iprim);
      fill(iprim, dataLength, dataStart_ptr);
    } // iprim
  
  // not sure why this isn't taken care of internally
  set_npart(replacer->get_npart());
  //done
}

// Dumps out the name of all primtives in data
void Lvl2OutArrayv1::dump_info()  
{
 
  cout << "For this event, Lvl2OutArray contains the following primitives:" 
       << endl;
  for (int iprim=0; iprim< get_npart(); iprim++)
    {
      cout << getname(iprim) << endl;
    } // iprim
  
}
