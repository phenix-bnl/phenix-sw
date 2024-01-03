#ifndef GenerateFullEventFlow_h
#define GenerateFullEventFlow_h

class ParticleList;
class ParticleGeneratorList;
class ParticlePropertyList;
class TF1;

void GenerateFullEventFlow(ParticleList& PList, const int dnch_dy, const double zVTXmax,
    const ParticleGeneratorList& PGList,
    const ParticlePropertyList& PPList,
    TF1& f_dNdphi, const TF1& f_v2);

#endif
