#ifndef FillROOTObjects_h
#define FillROOTObjects_h

class ParticleList;
class ParticlePropertyList;

void FillROOTObjects(const int setup, const unsigned int Nevents, const double dNdy_pi0, const double N_coll,
    const ParticleList& PList,
    const ParticlePropertyList& PPList,
    const bool fill_primaries = false,
    const bool fill_singles   = false,
    const bool fill_pairs     = false,
    const bool fill_photons   = false
    );

#endif
