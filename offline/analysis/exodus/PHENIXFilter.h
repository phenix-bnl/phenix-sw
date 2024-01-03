#ifndef PHENIXFilter_h
#define PHENIXFilter_h

//-----------------------------------------------------------------------------
//
//  Check whether a particle is in PHENIX central arm acceptance
//  modify to allow different field settings:  5/23/06 AD
//  controled by new flag fieldSetting, also hand over ptcut and vtxcut
//
//  RETURN VALUE:
//  in acceptance:     return 0,1
//  not in acceptance: return <0
//
//  INPUT:
//
//  int fieldSetting
//  1: ++ field as used for run-4/5/6
//  2:  + field used earlier
//  3: +- field for running with HBD
//  4: 
//  5: To filter into STAR acceptance
//
//  1 is default value, determined by Alberica Toia from Run-4
//  2 values taken from old acceptance filter
//  3 values determined from Run10 data
//  4 
//  5 STAR only has a 200MeV pT cut on the singles and a |Y|<1
//    (which is what exodus generates into).
//
//  double pt_cut in GeV
//  double vtx_cut in cm
//
//  Particle,ParticlePropertyList
//-----------------------------------------------------------------------------

class Particle;
class ParticlePropertyList;

int PHENIXFilter(const int fieldSetting, const double pt_cut, const double vtx_cut,
    const Particle& PParticle, const ParticlePropertyList& PPList);

#endif
