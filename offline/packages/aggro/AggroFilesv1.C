#include "AggroFilesv1.h"
//INCLUDECHECKER: Removed this line: #include "AggroSnglFilesv1.h"
#include <iostream>

ClassImp(AggroFilesv1)

using std::cout;
using std::cerr;
using std::endl;

#define AGGRONFILES 40

  // First we implement the "standard functions"...
AggroFilesv1::AggroFilesv1()
{
  nFiles = 0;
  Files  = new TClonesArray("AggroSnglFilesv1",AGGRONFILES);
}

AggroFilesv1::AggroFilesv1(const AggroFilesv1& rhs)
{
  Files=0;
  rhs.copyto(*this);
}

AggroFilesv1& 
AggroFilesv1::operator=(const AggroFilesv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
AggroFilesv1::copyto(AggroFilesv1& dest) const
{
  delete dest.Files;
  dest.Files = new TClonesArray("AggroSnglFilesv1",nFiles);
  dest.nFiles = nFiles;
  for ( unsigned int i = 0; i < nFiles; ++i ) 
    {
      AggroSnglFilesv1* src = static_cast<AggroSnglFilesv1*>
	(get_files(i));
      if ( src ) 
	{
	  dest.AddFiles(i);
	  AggroSnglFilesv1* d = static_cast<AggroSnglFilesv1*>
	    (dest.get_files(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

AggroFilesv1* 
AggroFilesv1::clone() const
{
  return new AggroFilesv1(*this);
}

AggroFilesv1::~AggroFilesv1()
{
  Files->Clear();
  delete Files;
  return;
}

AggroSnglFilesv1* AggroFilesv1::get_files (const unsigned int itrk) const {
  AggroSnglFilesv1 *Particle = (AggroSnglFilesv1 *) GetFiles()->UncheckedAt(itrk);
  return Particle;
}


void AggroFilesv1::identify(std::ostream& os) const
{
  os << "identify yourself: AggroFilesv1 Object\n"
     << "No of Files: " << nFiles << std::endl;
  return;
}

void AggroFilesv1::Reset()
{
 Files->Clear();
 if (nFiles>AGGRONFILES)
   {
     Files->Expand(AGGRONFILES);
   }
 nFiles = 0;
 return;
}

int AggroFilesv1::isValid() const
{
  return((nFiles>0) ? 1 : 0);
}

int AggroFilesv1::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > AGGRONFILES)
    {
      Files->Expand(nhits);
     }
  return nhits;
}

void  AggroFilesv1::AddFiles(const unsigned int itrk)
{
  TClonesArray &Particle = *Files;
  new(Particle[itrk]) AggroSnglFilesv1();
  return;
}

AggroSnglFilesv1* 
AggroFilesv1::AddFiles(const unsigned int itrk,
				const AggroSnglFiles& track)
{
  const AggroSnglFilesv1* test = dynamic_cast<const AggroSnglFilesv1*>
    (&track);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type AggroSnglFilesv1"
	   << endl;
      return 0;
    }

  return new((*Files)[itrk]) AggroSnglFilesv1(*test);
}


void  AggroFilesv1::RemoveFiles(const unsigned int itrk)
{
  Files->RemoveAt(itrk);
  return;
}


