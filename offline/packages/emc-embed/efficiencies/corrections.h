#ifndef __corrections_h__
#define __corrections_h__

// Current cutname = FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut

class corrections
{

public:

  static double acceptance(double pt, const char* type="value");

  static double efficiency(double pt, int centClass, const char* cut, 
			   const char* type);

  static double cutEfficiency(double pt, int centClass, 
			      const char* cut, const char* type);

  static void getCorrectionPointers(const char* cut,
				    double*& p0,
				    double*& p0err,
				    double*& p1,
				    double*& p1err);

  static void dumpCorrections(const char* cut);

  static double nonVertex(const char* type);
};

#endif
