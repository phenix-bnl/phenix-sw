#ifndef InitializeYCERES_h
#define InitializeYCERES_h

class TH1;
class ParticlePropertyList;

TH1* InitializeYCERES(const int ParticleID, const ParticlePropertyList& PPList);
double y_shift(const double Aproj, const double Atarg, const double bpar);

#endif
