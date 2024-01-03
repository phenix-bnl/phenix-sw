#ifndef ElectronInPHENIXAcceptance_h
#define ElectronInPHENIXAcceptance_h

int ElectronInPHENIXAcceptance(const double px, const double py, const double pz,
    const double vtx, const double charge,
    const double min_pt_cut  = 0.15,
    const double vtx_cut  = 35.);

#endif
