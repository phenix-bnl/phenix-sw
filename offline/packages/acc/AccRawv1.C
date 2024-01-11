
#include "Acc.h"
#include "AccRawv1.h"

ClassImp(AccRawv1)

using std::cout;
using std::endl;

static const unsigned int ACCNRAW = ACC::ACC_NCH;

AccRawv1::AccRawv1()
{
  nRaw = 0;
  AccRawHits  = new TClonesArray("AccSnglRawv1",ACCNRAW);
}

AccRawv1::AccRawv1(const AccRawv1& rhs)
{
  AccRawHits = 0;
  rhs.copyto(*this);
}

AccRawv1& AccRawv1::operator=(const AccRawv1& rhs)
{
  if(this != &rhs){
    rhs.copyto(*this);
  }
  return *this;
}

void AccRawv1::copyto(AccRawv1& dest) const
{
  delete dest.AccRawHits;
  dest.AccRawHits  = new TClonesArray("AccSnglRawv1",nRaw);
  dest.nRaw = nRaw;
  for(unsigned int i=0;i<nRaw;++i){
    AccSnglRawv1* src = static_cast<AccSnglRawv1*>(get_raw(i));
    if( src ){
      dest.AddRaw(i);
      AccSnglRawv1* d = static_cast<AccSnglRawv1*>(dest.get_raw(i));
      *d = *src;
    }
    else{
      cout << PHWHERE << "src particle is null ?" << endl;
    }
  }
}

AccRawv1* AccRawv1::clone() const
{
  return new AccRawv1(*this);
}

AccRawv1::~AccRawv1()
{
  AccRawHits->Clear();
  return;
}

void AccRawv1::Reset()
{
  AccRawHits->Clear();
  if(nRaw>ACCNRAW){
    AccRawHits->Expand(ACCNRAW);
  }
  nRaw = 0;

  return;
}

int AccRawv1::isValid() const
{
  return ( (nRaw>0) ? 1 : 0);
}

void AccRawv1::identify(std::ostream& os) const
{
  os << "identify yourself: AccRawv1 Object " << std::endl;
  os << "No of Raws: " << nRaw << std::endl;
   
  return;
}

int AccRawv1::set_TClonesArraySize(const unsigned int nch)
{
  if(nch > ACCNRAW){
    AccRawHits->Expand(nch);
  }

  return nch;
}

void AccRawv1::AddRaw(const unsigned int ich)
{
  TClonesArray& raw = *AccRawHits;
  new(raw[ich]) AccSnglRawv1();

  return;
}

void AccRawv1::RemoveRaw(const unsigned int ich)
{
  AccRawHits->RemoveAt(ich);

  return;
}

AccSnglRawv1* AccRawv1::AddRaw(const unsigned int ich, const AccSnglRaw& sngl)
{
  const AccSnglRawv1* raw = dynamic_cast<const AccSnglRawv1*>(&sngl);

  if(!raw){
    cout << PHWHERE << " sngl is not of type AccSnglRawv1" << std::endl;
    return 0;
  }

  return new( (*AccRawHits)[ich] ) AccSnglRawv1(*raw);
}

AccSnglRawv1* AccRawv1::get_raw(const unsigned int ich) const
{
  AccSnglRawv1* sngl = (AccSnglRawv1*) GetRaw()->UncheckedAt(ich);
  return sngl;
}

void AccRawv1::set_boxid(const int iraw, const int val)
{
  AccSnglRaw* sngl = (AccSnglRawv1*) GetRaw()->UncheckedAt(iraw);

  if(sngl) sngl->set_boxid(val);
  else{
    std::cout << PHWHERE << "Error no AccRawv1 object found" << std::endl; 
  }

  return;
}

void AccRawv1::set_adc(const int iraw, const int ipmt, const int val)
{
  AccSnglRaw* sngl = (AccSnglRawv1*) GetRaw()->UncheckedAt(iraw);

  if(sngl) sngl->set_adc(ipmt, val);
  else{
    std::cout << PHWHERE << "Error no AccRawv1 object found" << std::endl; 
  }

  return;
}

void AccRawv1::set_tdc(const int iraw, const int ipmt, const int val)
{
  AccSnglRaw* sngl = (AccSnglRawv1*) GetRaw()->UncheckedAt(iraw);

  if(sngl) sngl->set_tdc(ipmt, val);
  else{
    std::cout << PHWHERE << "Error no AccRawv1 object found" << std::endl; 
  }

  return;
}

void AccRawv1::set_adcpost(const int iraw, const int ipmt, const int val)
{
  AccSnglRaw* sngl = (AccSnglRawv1*) GetRaw()->UncheckedAt(iraw);

  if(sngl) sngl->set_adcpost(ipmt, val);
  else{
    std::cout << PHWHERE << "Error no AccRawv1 object found" << std::endl; 
  }

  return;
}

void AccRawv1::set_adcpre(const int iraw, const int ipmt, const int val)
{
  AccSnglRaw* sngl = (AccSnglRawv1*) GetRaw()->UncheckedAt(iraw);

  if(sngl) sngl->set_adcpre(ipmt, val);
  else{
    std::cout << PHWHERE << "Error no AccRawv1 object found" << std::endl; 
  }

  return;
}

int AccRawv1::get_boxid(const int iraw) const
{
  AccSnglRaw* sngl = AccRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_boxid() : -9999 );
}

int AccRawv1::get_adc(const int iraw, const int ipmt) const
{
  AccSnglRaw* sngl = AccRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_adc(ipmt) : -9999 );
}

int AccRawv1::get_tdc(const int iraw, const int ipmt) const
{
  AccSnglRaw* sngl = AccRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_tdc(ipmt) : -9999 );
}

int AccRawv1::get_adcpost(const int iraw, const int ipmt) const
{
  AccSnglRaw* sngl = AccRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_adcpost(ipmt) : -9999 );
}

int AccRawv1::get_adcpre(const int iraw, const int ipmt) const
{
  AccSnglRaw* sngl = AccRawv1::get_raw(iraw);

  return ( (sngl) ? sngl->get_adcpre(ipmt) : -9999 );
}
