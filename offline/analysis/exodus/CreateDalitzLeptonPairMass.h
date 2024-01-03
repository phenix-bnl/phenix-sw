#ifndef CreateDalitzLeptonPairMass_h
#define CreateDalitzLeptonPairMass_h

class TH1F;
class Decay;
class ParticlePropertyList;

TH1F* CreateDalitzLeptonPairMass(const Decay& DDalitz, const ParticlePropertyList& PPList);

#endif
