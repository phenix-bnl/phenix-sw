#include "emcEmbedEvent.h"
#include <cmath>
#include "phool.h"

ClassImp(emcEmbedEvent)

//_____________________________________________________________________________
emcEmbedEvent::emcEmbedEvent()
{
}

//_____________________________________________________________________________
emcEmbedEvent::~emcEmbedEvent()
{
}

//_____________________________________________________________________________
double
emcEmbedEvent::mass(unsigned int i) const
{
  double e = energy(i);
  double p = momentum(i);
  return sqrt(e*e - p*p);
}

//_____________________________________________________________________________
double
emcEmbedEvent::momentum(unsigned int i) const
{
  double px_ = px(i);
  double py_ = py(i);
  double pz_ = pz(i);

  return sqrt(px_*px_+py_*py_+pz_*pz_);
}

//_____________________________________________________________________________
double
emcEmbedEvent::pt(unsigned int i) const
{
  double px_ = px(i);
  double py_ = py(i);
  return sqrt(px_*px_+py_*py_);
}

//_____________________________________________________________________________
void
emcEmbedEvent::warning(const char* method) const
{
  std::cerr << PHWHERE << " virtual method " << method << std::endl;
}
