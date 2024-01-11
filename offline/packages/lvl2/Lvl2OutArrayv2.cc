#include <iostream>
#include <Lvl2OutArrayv2.h>
#include <Lvl2SnglOutv2.h>
#include <phool.h>
#include <recoConsts.h>

#include <cstdlib>

ClassImp(Lvl2OutArrayv2);

using namespace std;

static const unsigned int MAXPRIMITIVES = 200;

Lvl2OutArrayv2::Lvl2OutArrayv2()
{
  recoConsts *rc = recoConsts::instance();
  if (rc->FlagExist("LVL2_INITIALIZE"))
    {
      if (rc->get_IntFlag("LVL2_INITIALIZE"))
        {
          cout << "No lvl2 initializing anymore" << endl;
          exit(-1);
        }
    }
  nL2Prim = 0;
  L2Prim = new TClonesArray("Lvl2SnglOutv2", MAXPRIMITIVES);
}

Lvl2OutArrayv2::~Lvl2OutArrayv2()
{
  if (L2Prim) delete L2Prim;
}

void Lvl2OutArrayv2::AddPrimitive(const unsigned int iprim)
{
  TClonesArray &a = *L2Prim;
  new (a[iprim]) Lvl2SnglOutv2();
}

void Lvl2OutArrayv2::RemovePrimitive(const unsigned int iprim)
{
  L2Prim->RemoveAt(iprim);
}

void Lvl2OutArrayv2::Reset()
{
  if ( L2Prim && nL2Prim>0 )
    {
      // The "C" option tels TClonesArray to call the Lvl2SnglOutv2
      // Clear method. This is needed to free the memory "newed" in
      // Lvl2SnglOutv2::Fill(). Tony Frawley, jan 28 2004.
      
      L2Prim->Clear("C");
    }
  nL2Prim = 0;
}

int Lvl2OutArrayv2::isValid() const
{
  if (nL2Prim >= 0) return 1;
  return 0;
}

void Lvl2OutArrayv2::identify(ostream &os) const
{
  os << "identify yourself: Lvl2OutArrayv2 object" << endl;
}

int Lvl2OutArrayv2::set_TClonesArraySize(const unsigned int iprim)
{
  if ( iprim > MAXPRIMITIVES ) L2Prim->Expand(MAXPRIMITIVES);
  return iprim;
}

void Lvl2OutArrayv2::fill(const unsigned int iprim, UINT n, PHDWORD *src)
{
  Lvl2SnglOut *out = (Lvl2SnglOut *)L2Prim->At(iprim);
  if (out) out->fill(n, src);
  else     cout << PHWHERE << "ERROR no Lvl2SnglOut object found" << endl;
}

int Lvl2OutArrayv2::find(const char *primitive) const
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

int Lvl2OutArrayv2::GetPrimitive(const char *primitive)
{

  cout << "Lvl2OutArrayv1::GetPrimitive is deprecated" << endl;
  exit(-1);
  return 0;
}


// To replace the contents of an this object with 
// copies of another's contents
void Lvl2OutArrayv2::ReplaceContents(Lvl2OutArray * replacer)  
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
void Lvl2OutArrayv2::dump_info()  
{
 
  cout << "For this event, Lvl2OutArray contains the following primitives:" 
       << endl;
  for (int iprim=0; iprim< get_npart(); iprim++)
    {
      cout << getname(iprim) << endl;
    } // iprim
  
}
