#ifndef InitializePtPHENIX_h
#define InitializePtPHENIX_h

class TH1;
class ParticlePropertyList;

TH1* InitializePtPHENIX(const int setup, const int ParticleID,
    const double f_c, const double f_p0, const double f_a,
    const double f_b, const double f_n,
    const double t, const double w, const double A, const double p0,
    const double m, const double B, const double n,
    const ParticlePropertyList& PPList);

#endif
