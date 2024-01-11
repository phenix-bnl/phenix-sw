#ifndef __convertNormalization_h__
#define __convertNormalization_h__

#include <iosfwd>
#include "emcManageable.h"

class emcCalibrationData;
class PHTimeStamp;

void convertNormalization(emcManageable::EStorage fromWhere, 
			  PHTimeStamp* ts=0);

void convertSector(const emcCalibrationData& sector, 
		   const PHTimeStamp& when, 
		   std::ostream& out);

#endif
