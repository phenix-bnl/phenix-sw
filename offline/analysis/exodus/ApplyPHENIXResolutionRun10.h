#ifndef ApplyPHENIXResolutionRun10_h
#define ApplyPHENIXResolutionRun10_h

class Mom4;

// Apply smearing to `mom4`.
// Default parameters correspond to Run10 AuAu@200GeV.
//
// This is a more complete momentum resolution function developed by Y.Watanabe
// (U Tokyo), also see his slides from the HBD (electron) group meeting from
// 2012-07-15. It includes momentum-dependent angular smearing and a tail in
// the reconstructed |p| towards lower p. For the meaning of the parameters see
// the implementation.

void ApplyPHENIXResolutionRun10(Mom4& mom4,
    double p0      = -3.76e-03,
    double p1      = 1.594,
    double p2      = 1.123e-02,
    double p3      = 3.385,
    double s_c     = 8.882e-05,
    double s_b     = 2.101e-02,
    double s_a     = -2.995e-03,
    double sl_c    = 2.384,
    double sl_a    = 4.329,
    double r0      = 0.8965,
    double r1      = 4.686,
    double r2      = -0.003184,
    double phi_a   = 2.535e-04,
    double phi_b   = -5.092e-05,
    double phi_c   = 1.721e-03,
    double theta_a = 7.709e-04,
    double theta_b = 2.705e-04,
    double theta_c = 2.14e-03);

#endif
