#ifndef InitializeM_h
#define InitializeM_h

class TH1;
class ParticlePropertyList;

TH1* InitializeM(const int ParticleID, double mass_min, double mass_max,
    const ParticlePropertyList& PPList);
double GounarisSakurai(const double mass, const double vmass, const double vwidth, const double emass);
double Lorentz(const double mass, const double vmass, const double vwidth);

#endif
