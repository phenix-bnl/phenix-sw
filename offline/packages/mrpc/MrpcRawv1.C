#include <MrpcPar.h>
#include <MrpcRawv1.h>
#include <iostream>

ClassImp(MrpcRawv1)

using std::cout;
using std::endl;

static const unsigned int MRPCNRAW = MRPC_NCH_TOTAL;

MrpcRawv1::MrpcRawv1()
{
  nRaw = 0;
  MrpcRawHits  = new TClonesArray("MrpcSnglRawv1",MRPCNRAW);
}

MrpcRawv1::MrpcRawv1(const MrpcRawv1& rhs)
{
  MrpcRawHits = 0;
  rhs.copyto(*this);
}

MrpcRawv1& MrpcRawv1::operator=(const MrpcRawv1& rhs)
{
  if(this != &rhs)
    {
      rhs.copyto(*this);
    }
  return *this;
}

void MrpcRawv1::copyto(MrpcRawv1& dest) const
{
  delete dest.MrpcRawHits;
  dest.MrpcRawHits  = new TClonesArray("MrpcSnglRawv1",nRaw);
  dest.nRaw = nRaw;
  for(unsigned int i=0;i<nRaw;++i){
    MrpcSnglRawv1* src = static_cast<MrpcSnglRawv1*>(get_raw(i));
    if( src )
      {
	dest.AddRaw(i);
	MrpcSnglRawv1* d = static_cast<MrpcSnglRawv1*>(dest.get_raw(i));
	*d = *src;
      }
    else
      {
	cout << PHWHERE << "src particle is null ?" << endl;
      }
  }
}

MrpcRawv1* MrpcRawv1::clone() const
{
  return new MrpcRawv1(*this);
}

MrpcRawv1::~MrpcRawv1()
{
  if (MrpcRawHits)
    {
      MrpcRawHits->Clear();
      delete MrpcRawHits;
    }
  return;
}

void MrpcRawv1::Reset()
{
  MrpcRawHits->Clear();
  if(nRaw>MRPCNRAW)
    {
      MrpcRawHits->Expand(MRPCNRAW);
    }
  nRaw = 0;
  
  return;
}

int MrpcRawv1::isValid() const
{
  return ( (nRaw>0) ? 1 : 0);
}

void MrpcRawv1::identify(std::ostream& os) const
{
  os << "identify yourself: MrpcRawv1 Object " << std::endl;
  os << "No of Raws: " << nRaw << std::endl;
   
  return;
}

int MrpcRawv1::set_TClonesArraySize(const unsigned int nch)
{
  if(nch > MRPCNRAW)
    {
      MrpcRawHits->Expand(nch);
    }

  return nch;
}

void MrpcRawv1::AddRaw(const unsigned int ich)
{
  TClonesArray& raw = *MrpcRawHits;
  new(raw[ich]) MrpcSnglRawv1();

  return;
}

void MrpcRawv1::RemoveRaw(const unsigned int ich)
{
  MrpcRawHits->RemoveAt(ich);

  return;
}

MrpcSnglRawv1* MrpcRawv1::AddRaw(const unsigned int ich, const MrpcSnglRaw& sngl)
{
  const MrpcSnglRawv1* raw = dynamic_cast<const MrpcSnglRawv1*>(&sngl);

  if(!raw)
    {
      cout << PHWHERE << " sngl is not of type MrpcSnglRawv1" << std::endl;
      return 0;
    }
  
  return new( (*MrpcRawHits)[ich] ) MrpcSnglRawv1(*raw);
}

MrpcSnglRawv1* MrpcRawv1::get_raw(const unsigned int ich) const
{
  MrpcSnglRawv1* sngl = (MrpcSnglRawv1*) GetRaw()->UncheckedAt(ich);
  return sngl;
}

// set ===============
void MrpcRawv1::set_slatid(const int iraw, const int val)
{
  MrpcSnglRaw* sngl = (MrpcSnglRawv1*) GetRaw()->UncheckedAt(iraw);

  if(sngl) sngl->set_slatid(val);
  else
    {
      std::cout << PHWHERE << "Error no MrpcRawv1 object found" << std::endl; 
    }
  
  return;
}

void MrpcRawv1::set_t3(const int iraw, const int ich, const int val)
{
  MrpcSnglRaw* sngl = (MrpcSnglRawv1*) GetRaw()->UncheckedAt(iraw);
  
  if(sngl) sngl->set_t3(ich, val);
  else
    {
      std::cout << PHWHERE << "Error no MrpcRawv1 object found" << std::endl; 
    }
  
  return;
}

void MrpcRawv1::set_t4(const int iraw, const int ich, const int val)
{
  MrpcSnglRaw* sngl = (MrpcSnglRawv1*) GetRaw()->UncheckedAt(iraw);
  
  if(sngl) sngl->set_t4(ich, val);
  else
    {
      std::cout << PHWHERE << "Error no MrpcRawv1 object found" << std::endl; 
    }
  
  return;
}

void MrpcRawv1::set_q1(const int iraw, const int ich, const int val)
{
  MrpcSnglRaw* sngl = (MrpcSnglRawv1*) GetRaw()->UncheckedAt(iraw);
  
  if(sngl) sngl->set_q1(ich, val);
  else
    {
      std::cout << PHWHERE << "Error no MrpcRawv1 object found" << std::endl; 
    }
  
  return;
}

void MrpcRawv1::set_q3(const int iraw, const int ich, const int val)
{
  MrpcSnglRaw* sngl = (MrpcSnglRawv1*) GetRaw()->UncheckedAt(iraw);
  
  if(sngl) sngl->set_q3(ich, val);
  else
    {
      std::cout << PHWHERE << "Error no MrpcRawv1 object found" << std::endl; 
    }
  
  return;
}

void MrpcRawv1::set_qvc(const int iraw, const int ich, const int val)
{
  MrpcSnglRaw* sngl = (MrpcSnglRawv1*) GetRaw()->UncheckedAt(iraw);
  
  if(sngl) sngl->set_qvc(ich, val);
  else
    {
      std::cout << PHWHERE << "Error no MrpcRawv1 object found" << std::endl; 
    }
  
  return;
}

void MrpcRawv1::set_t3_dig(const int iraw, const int ich, const int val)
{
  MrpcSnglRaw* sngl = (MrpcSnglRawv1*) GetRaw()->UncheckedAt(iraw);
  
  if(sngl) sngl->set_t3_dig(ich, val);
  else
    {
      std::cout << PHWHERE << "Error no MrpcRawv1 object found" << std::endl; 
    }
  
  return;
}

void MrpcRawv1::set_t4_dig(const int iraw, const int ich, const int val)
{
  MrpcSnglRaw* sngl = (MrpcSnglRawv1*) GetRaw()->UncheckedAt(iraw);
  
  if(sngl) sngl->set_t4_dig(ich, val);
  else
    {
      std::cout << PHWHERE << "Error no MrpcRawv1 object found" << std::endl; 
    }
  
  return;
}


// get ============
int MrpcRawv1::get_slatid(const int iraw) const
{
  MrpcSnglRaw* sngl = MrpcRawv1::get_raw(iraw);
  
  return ( (sngl) ? sngl->get_slatid() : -9999 );
}

int MrpcRawv1::get_t3(const int iraw, const int ich) const
{
  MrpcSnglRaw* sngl = MrpcRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_t3(ich) : -9999 );
}

int MrpcRawv1::get_t4(const int iraw, const int ich) const
{
  MrpcSnglRaw* sngl = MrpcRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_t4(ich) : -9999 );
}

int MrpcRawv1::get_q1(const int iraw, const int ich) const
{
  MrpcSnglRaw* sngl = MrpcRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_q1(ich) : -9999 );
}

int MrpcRawv1::get_q3(const int iraw, const int ich) const
{
  MrpcSnglRaw* sngl = MrpcRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_q3(ich) : -9999 );
}

int MrpcRawv1::get_qvc(const int iraw, const int ich) const
{
  MrpcSnglRaw* sngl = MrpcRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_qvc(ich) : -9999 );
}

int MrpcRawv1::get_t3_dig(const int iraw, const int ich) const
{
  MrpcSnglRaw* sngl = MrpcRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_t3_dig(ich) : -9999 );
}

int MrpcRawv1::get_t4_dig(const int iraw, const int ich) const
{
  MrpcSnglRaw* sngl = MrpcRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_t4_dig(ich) : -9999 );
}
