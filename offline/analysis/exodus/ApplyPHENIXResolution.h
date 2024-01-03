#ifndef ApplyPHENIXResolution_h
#define ApplyPHENIXResolution_h

class Mom4;

// Apply smearing to `mom4`.
//
// The functional form of the smearing function is
//
//     sigma/p = A (x) B*p
//
// Default parameters correspond to pp@200GeV.
// update: now the default corresponds to Run8dAu -jkamin, 01Mar2012
//         check AN974 (authors: themann,drees,kamin)

//void ApplyPHENIXResolution(Mom4& mom4, const double A=0.007, const double B=0.01);//pp@200GeV (probably Run5?)
void ApplyPHENIXResolution(Mom4& mom4, const double A=0.011, const double B=0.0116);//Run8dAu

#endif
