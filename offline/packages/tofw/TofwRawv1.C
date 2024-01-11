#include <TofwPar.h>
#include <TofwRawv1.h>
#include <TClonesArray.h>

ClassImp(TofwRawv1)

using std::cout;
using std::endl;

static const unsigned int TOFWNRAW = TOFW_NSTRIP_TOTAL;

TofwRawv1::TofwRawv1()
{
  nRaw = 0;
  TofwRawHits  = new TClonesArray("TofwSnglRawv1",TOFWNRAW);
}

TofwRawv1::TofwRawv1(const TofwRawv1& rhs)
{
  TofwRawHits = 0;
  rhs.copyto(*this);
}

TofwRawv1& TofwRawv1::operator=(const TofwRawv1& rhs)
{
  if(this != &rhs)
    {
      rhs.copyto(*this);
    }
  return *this;
}

void TofwRawv1::copyto(TofwRawv1& dest) const
{
  delete dest.TofwRawHits;
  dest.TofwRawHits  = new TClonesArray("TofwSnglRawv1",nRaw);
  dest.nRaw = nRaw;
  for(unsigned int i=0;i<nRaw;++i){
    TofwSnglRawv1* src = static_cast<TofwSnglRawv1*>(get_raw(i));
    if( src )
      {
	dest.AddRaw(i);
	TofwSnglRawv1* d = static_cast<TofwSnglRawv1*>(dest.get_raw(i));
	*d = *src;
      }
    else
      {
	cout << PHWHERE << "src particle is null ?" << endl;
      }
  }
}

TofwRawv1* TofwRawv1::clone() const
{
  return new TofwRawv1(*this);
}

TofwRawv1::~TofwRawv1()
{
  if (TofwRawHits)
    {
      TofwRawHits->Clear();
      delete TofwRawHits;
    }
  return;
}

void TofwRawv1::Reset()
{
  TofwRawHits->Clear();
  if(nRaw>TOFWNRAW)
    {
      TofwRawHits->Expand(TOFWNRAW);
    }
  nRaw = 0;
  
  return;
}

int TofwRawv1::isValid() const
{
  return ( (nRaw>0) ? 1 : 0);
}

void TofwRawv1::identify(std::ostream& os) const
{
  os << "identify yourself: TofwRaw Object " << std::endl;
  os << "No of Raws: " << nRaw << std::endl;
   
  return;
}

int TofwRawv1::set_TClonesArraySize(const unsigned int nch)
{
  if(nch > TOFWNRAW)
    {
      TofwRawHits->Expand(nch);
    }

  return nch;
}

void TofwRawv1::AddRaw(const unsigned int ich)
{
  TClonesArray& raw = *TofwRawHits;
  new(raw[ich]) TofwSnglRawv1();

  return;
}

void TofwRawv1::RemoveRaw(const unsigned int ich)
{
  TofwRawHits->RemoveAt(ich);

  return;
}

TofwSnglRawv1* TofwRawv1::AddRaw(const unsigned int ich, const TofwSnglRaw& sngl)
{
  const TofwSnglRawv1* raw = dynamic_cast<const TofwSnglRawv1*>(&sngl);

  if(!raw)
    {
      cout << PHWHERE << " sngl is not of type TofwSnglRaw" << std::endl;
      return 0;
    }
  
  return new( (*TofwRawHits)[ich] ) TofwSnglRawv1(*raw);
}

TofwSnglRawv1* TofwRawv1::get_raw(const unsigned int ich) const
{
  TofwSnglRawv1* sngl = (TofwSnglRawv1*) GetRaw()->UncheckedAt(ich);
  return sngl;
}

// set ===============
void TofwRawv1::set_boxid(const int iraw, const int val)
{
  TofwSnglRaw* sngl = (TofwSnglRawv1*) GetRaw()->UncheckedAt(iraw);

  if(sngl) sngl->set_boxid(val);
  else
    {
      std::cout << PHWHERE << "Error no TofwRawv1 object found" << std::endl; 
    }
  
  return;
}

void TofwRawv1::set_chamberid(const int iraw, const int val)
{
  TofwSnglRaw* sngl = (TofwSnglRawv1*) GetRaw()->UncheckedAt(iraw);

  if(sngl) sngl->set_chamberid(val);
  else
    {
      std::cout << PHWHERE << "Error no TofwRawv1 object found" << std::endl; 
    }
  
  return;
}

void TofwRawv1::set_stripid(const int iraw, const int val)
{
  TofwSnglRaw* sngl = (TofwSnglRawv1*) GetRaw()->UncheckedAt(iraw);

  if(sngl) sngl->set_stripid(val);
  else
    {
      std::cout << PHWHERE << "Error no TofwRawv1 object found" << std::endl; 
    }
  
  return;
}


void TofwRawv1::set_t3(const int iraw, const int ich, const int val)
{
  TofwSnglRaw* sngl = (TofwSnglRawv1*) GetRaw()->UncheckedAt(iraw);
  
  if(sngl) sngl->set_t3(ich, val);
  else
    {
      std::cout << PHWHERE << "Error no TofwRawv1 object found" << std::endl; 
    }
  
  return;
}

void TofwRawv1::set_t4(const int iraw, const int ich, const int val)
{
  TofwSnglRaw* sngl = (TofwSnglRawv1*) GetRaw()->UncheckedAt(iraw);
  
  if(sngl) sngl->set_t4(ich, val);
  else
    {
      std::cout << PHWHERE << "Error no TofwRawv1 object found" << std::endl; 
    }
  
  return;
}

void TofwRawv1::set_q1(const int iraw, const int ich, const int val)
{
  TofwSnglRaw* sngl = (TofwSnglRawv1*) GetRaw()->UncheckedAt(iraw);
  
  if(sngl) sngl->set_q1(ich, val);
  else
    {
      std::cout << PHWHERE << "Error no TofwRawv1 object found" << std::endl; 
    }
  
  return;
}

void TofwRawv1::set_q3(const int iraw, const int ich, const int val)
{
  TofwSnglRaw* sngl = (TofwSnglRawv1*) GetRaw()->UncheckedAt(iraw);
  
  if(sngl) sngl->set_q3(ich, val);
  else
    {
      std::cout << PHWHERE << "Error no TofwRawv1 object found" << std::endl; 
    }
  
  return;
}

void TofwRawv1::set_tvc(const int iraw, const int ich, const float val)
{
  TofwSnglRaw* sngl = (TofwSnglRawv1*) GetRaw()->UncheckedAt(iraw);
  
  if(sngl) sngl->set_tvc(ich, val);
  else
    {
      std::cout << PHWHERE << "Error no TofwRawv1 object found" << std::endl; 
    }
  
  return;
}

void TofwRawv1::set_qvc(const int iraw, const int ich, const float val)
{
  TofwSnglRaw* sngl = (TofwSnglRawv1*) GetRaw()->UncheckedAt(iraw);
  
  if(sngl) sngl->set_qvc(ich, val);
  else
    {
      std::cout << PHWHERE << "Error no TofwRawv1 object found" << std::endl; 
    }
  
  return;
}


// get ============
int TofwRawv1::get_stripid(const int iraw) const
{
  TofwSnglRaw* sngl = TofwRawv1::get_raw(iraw);
  
  return ( (sngl) ? sngl->get_stripid() : -9999 );
}

int TofwRawv1::get_chamberid(const int iraw) const
{
  TofwSnglRaw* sngl = TofwRawv1::get_raw(iraw);
  
  return ( (sngl) ? sngl->get_chamberid() : -9999 );
}

int TofwRawv1::get_boxid(const int iraw) const
{
  TofwSnglRaw* sngl = TofwRawv1::get_raw(iraw);
  
  return ( (sngl) ? sngl->get_boxid() : -9999 );
}

int TofwRawv1::get_t3(const int iraw, const int ich) const
{
  TofwSnglRaw* sngl = TofwRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_t3(ich) : -9999 );
}

int TofwRawv1::get_t4(const int iraw, const int ich) const
{
  TofwSnglRaw* sngl = TofwRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_t4(ich) : -9999 );
}

int TofwRawv1::get_q1(const int iraw, const int ich) const
{
  TofwSnglRaw* sngl = TofwRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_q1(ich) : -9999 );
}

int TofwRawv1::get_q3(const int iraw, const int ich) const
{
  TofwSnglRaw* sngl = TofwRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_q3(ich) : -9999 );
}

float TofwRawv1::get_tvc(const int iraw, const int ich) const
{
  TofwSnglRaw* sngl = TofwRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_tvc(ich) : -9999 );
}

float TofwRawv1::get_qvc(const int iraw, const int ich) const
{
  TofwSnglRaw* sngl = TofwRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_qvc(ich) : -9999 );
}
