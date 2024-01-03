#include "ParticleProperty.h"

ParticleProperty::ParticleProperty() :
  itsMass(0),
  itsWidth(0),
  itsID(0),
  itsCharge(0)
{}
//___________________________________________________________
void ParticleProperty::Set(const int ID, const double mass, const double width,
    const int charge)
{
  itsID     = ID;
  itsMass   = mass;
  itsWidth  = width;
  itsCharge = charge;
}

