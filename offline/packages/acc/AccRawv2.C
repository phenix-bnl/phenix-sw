
#include "Acc.h"
#include "AccRawv2.h"

ClassImp(AccRawv2)

using std::cout;
using std::endl;

static const unsigned int ACCNRAW = ACC::ACC_NCH;

AccRawv2::AccRawv2()
{
  nRaw = 0;
  AccRawHits  = new TClonesArray("AccSnglRawv2",ACCNRAW);
}

AccRawv2::AccRawv2(const AccRawv2& rhs)
{
  AccRawHits = 0;
  rhs.copyto(*this);
}

AccRawv2& AccRawv2::operator=(const AccRawv2& rhs)
{
  if(this != &rhs){
    rhs.copyto(*this);
  }
  return *this;
}

void AccRawv2::copyto(AccRawv2& dest) const
{
  delete dest.AccRawHits;
  dest.AccRawHits  = new TClonesArray("AccSnglRawv2",nRaw);
  dest.nRaw = nRaw;
  for(unsigned int i=0;i<nRaw;++i){
    AccSnglRawv2* src = static_cast<AccSnglRawv2*>(get_raw(i));
    if( src ){
      dest.AddRaw(i);
      AccSnglRawv2* d = static_cast<AccSnglRawv2*>(dest.get_raw(i));
      *d = *src;
    }
    else{
      cout << PHWHERE << "src particle is null ?" << endl;
    }
  }
}

AccRawv2* AccRawv2::clone() const
{
  return new AccRawv2(*this);
}

AccRawv2::~AccRawv2()
{
  if (AccRawHits)
    {
      AccRawHits->Clear();
      delete AccRawHits;
    }
  return;
}

void AccRawv2::Reset()
{
  AccRawHits->Clear();
  if(nRaw>ACCNRAW){
    AccRawHits->Expand(ACCNRAW);
  }
  nRaw = 0;

  return;
}

int AccRawv2::isValid() const
{
  return ( (nRaw>0) ? 1 : 0);
}

void AccRawv2::identify(std::ostream& os) const
{
  os << "identify yourself: AccRawv2 Object " << std::endl;
  os << "No of Raws: " << nRaw << std::endl;
   
  return;
}

int AccRawv2::set_TClonesArraySize(const unsigned int nch)
{
  if(nch > ACCNRAW){
    AccRawHits->Expand(nch);
  }

  return nch;
}

void AccRawv2::AddRaw(const unsigned int ich)
{
  TClonesArray& raw = *AccRawHits;
  new(raw[ich]) AccSnglRawv2();

  return;
}

void AccRawv2::RemoveRaw(const unsigned int ich)
{
  AccRawHits->RemoveAt(ich);

  return;
}

AccSnglRawv2* AccRawv2::AddRaw(const unsigned int ich, const AccSnglRaw& sngl)
{
  const AccSnglRawv2* raw = dynamic_cast<const AccSnglRawv2*>(&sngl);

  if(!raw){
    cout << PHWHERE << " sngl is not of type AccSnglRawv2" << std::endl;
    return 0;
  }

  return new( (*AccRawHits)[ich] ) AccSnglRawv2(*raw);
}

AccSnglRawv2* AccRawv2::get_raw(const unsigned int ich) const
{
  AccSnglRawv2* sngl = (AccSnglRawv2*) GetRaw()->UncheckedAt(ich);
  return sngl;
}

void AccRawv2::set_boxid(const int iraw, const int val)
{
  AccSnglRaw* sngl = (AccSnglRawv2*) GetRaw()->UncheckedAt(iraw);

  if(sngl) sngl->set_boxid(val);
  else{
    std::cout << PHWHERE << "Error no AccRawv2 object found" << std::endl; 
  }

  return;
}

void AccRawv2::set_adc(const int iraw, const int ipmt, const int val)
{
  AccSnglRaw* sngl = (AccSnglRawv2*) GetRaw()->UncheckedAt(iraw);

  if(sngl) sngl->set_adc(ipmt, val);
  else{
    std::cout << PHWHERE << "Error no AccRawv2 object found" << std::endl; 
  }

  return;
}

void AccRawv2::set_tdc(const int iraw, const int ipmt, const int val)
{
  AccSnglRaw* sngl = (AccSnglRawv2*) GetRaw()->UncheckedAt(iraw);

  if(sngl) sngl->set_tdc(ipmt, val);
  else{
    std::cout << PHWHERE << "Error no AccRawv2 object found" << std::endl; 
  }

  return;
}

void AccRawv2::set_adcpost(const int iraw, const int ipmt, const int val)
{
  AccSnglRaw* sngl = (AccSnglRawv2*) GetRaw()->UncheckedAt(iraw);

  if(sngl) sngl->set_adcpost(ipmt, val);
  else{
    std::cout << PHWHERE << "Error no AccRawv2 object found" << std::endl; 
  }

  return;
}

void AccRawv2::set_adcpre(const int iraw, const int ipmt, const int val)
{
  AccSnglRaw* sngl = (AccSnglRawv2*) GetRaw()->UncheckedAt(iraw);

  if(sngl) sngl->set_adcpre(ipmt, val);
  else{
    std::cout << PHWHERE << "Error no AccRawv2 object found" << std::endl; 
  }

  return;
}

int AccRawv2::get_boxid(const int iraw) const
{
  AccSnglRaw* sngl = AccRawv2::get_raw(iraw);

  return ( (sngl) ? sngl->get_boxid() : -9999 );
}

int AccRawv2::get_adc(const int iraw, const int ipmt) const
{
  AccSnglRaw* sngl = AccRawv2::get_raw(iraw);

  return ( (sngl) ? sngl->get_adc(ipmt) : -9999 );
}

int AccRawv2::get_tdc(const int iraw, const int ipmt) const
{
  AccSnglRaw* sngl = AccRawv2::get_raw(iraw);

  return ( (sngl) ? sngl->get_tdc(ipmt) : -9999 );
}

int AccRawv2::get_adcpost(const int iraw, const int ipmt) const
{
  AccSnglRaw* sngl = AccRawv2::get_raw(iraw);

  return ( (sngl) ? sngl->get_adcpost(ipmt) : -9999 );
}

int AccRawv2::get_adcpre(const int iraw, const int ipmt) const
{
  AccSnglRaw* sngl = AccRawv2::get_raw(iraw);

  return ( (sngl) ? sngl->get_adcpre(ipmt) : -9999 );
}
