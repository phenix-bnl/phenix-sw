#ifndef __PBSCCALIBRATIONDATA_H__
#define __PBSCCALIBRATIONDATA_H__


/** PbSc Calibrations Data.

		This is a data-base implementation dependent class. At this moment it accesses the calibration data files initially stored on EMCAL01 while calibrating PbSc Sectors in 902 area. Its duty is to load calibration data into protected area of the EmcSector objects designed explicitly for the fast access to static calibration data. Care should be taken about proper data mapping between event data which are initially structured in blocks of 144 channels (Items, Item can be tower or reference but we are using similarily structured electronics everywhere) 
	 \\
	 @author: E.Kistenev
	 \\
	 @date:  03/01/99

*/

#include <Rtypes.h>

class PbScCalibrationData
{

 public:

  PbScCalibrationData();
	virtual ~PbScCalibrationData();

	/// Returns Production Number (Idebntifier) of the PbSc SuperModule installed in position SMNumber (0-107) in PHENIX 
	int          getSMId(int SMNumber) {return PbScSMList[SMNumber];}

	/// Returns position in PHENIX of the PbSc SuperModule with production number SMId
	int          getSMNumber(int SMId);

	///
	char       * getPointerToFName(int SMNumber) { return fName[SMNumber];}

	/// Used only for interactive access to data on PMT's
	virtual void LoadPMTDataBase(const char *);
	Bool_t       getStatus(){return Status;}
 private:

	Bool_t Status;
	char * fName[109];
	int  * PbScSMList;
};

#endif
