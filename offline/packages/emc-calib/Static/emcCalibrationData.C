#include "emcCalibrationData.h"
#include <iostream>
#include <string>

using namespace std;

const float emcCalibrationData::fgInvalidFloat = -99999.9999;

//_____________________________________________________________________________
emcCalibrationData::emcCalibrationData(EType type, size_t number, size_t size)
  : fStart(0), fEnd(0), fType(type), fNumber(number),
    fValues(0), fErrors(0), fSize(size), fDimension(0)
{
  SetTypeAndSize(fType, fSize);
}

//_____________________________________________________________________________
emcCalibrationData::~emcCalibrationData()
{
  Reset();
}

//_____________________________________________________________________________
const char*
emcCalibrationData::GetCategory(void) const
{
  string cat;

  switch (fType)
    {
    case kUnknown:
      return "UNKNOWN";
    case kIniCal:
      return "generic.IniCal";
    case kTofSectorOffset:
      return "tof.sector.offsets";
    default:
      return "???";
    }
}

//_____________________________________________________________________________
float
emcCalibrationData::GetError(size_t index, size_t dim) const
{
  float rv = fgInvalidFloat;

  if ( index < fSize &&
       dim < fDimension )
    {
      rv = fErrors[index][dim];
    }
  return rv;
}

//_____________________________________________________________________________
const char*
emcCalibrationData::GetTypeName(EType type)
{
  // Type name should not contain blanks and should be short,
  // as it is used to infer the DB name.

  switch (type)
    {
    case kUnknown:
      return "UNKNOWN";
    case kIniCal:
      return "IniCal";
    case kTofSectorOffset:
      return "TofSectorOffset";
    default:
      return "???";
    }
}

//_____________________________________________________________________________
float
emcCalibrationData::GetValue(size_t index, size_t dim) const
{
  float rv = fgInvalidFloat;

  if ( index < fSize &&
       dim < fDimension )
    {
      rv = fValues[index][dim];
    }
  return rv;
}

//_____________________________________________________________________________
bool
emcCalibrationData::IsValid(const PHTimeStamp& cwhen) const
{
  PHTimeStamp& when = const_cast<PHTimeStamp&>(cwhen);
  if (when.isInRange(fStart, fEnd))
    return true;
  return false;
}

//_____________________________________________________________________________
void
emcCalibrationData::Print(int level) const
{
  cout << "Calibration type " << static_cast<int>(fType)
       << " (\"" << GetTypeName() << "\")"
       << " number " << fNumber
       << " has " << fSize << " entries." << endl;
  cout << " - is valid from " << fStart << " until "
       << fEnd << endl;
  if (level)
    {
      size_t i;
      for (i = 0;i < fSize;i++)
	{
	  cout << i << " ";
	  size_t j;
	  for (j = 0;j < fDimension;j++)
	    {
	      if (j)
		cout << " , ";
	      cout << GetValue(i, j) << " +- " << GetError(i, j);
	    }
	  cout << endl;
	}
    }
}

//_____________________________________________________________________________
void
emcCalibrationData::Reset(void)
{
  if (!fValues || !fErrors)
    return ;

  size_t i;

  for ( i = 0; i < fSize; i++ )
    {
      delete[] fValues[i];
      fValues[i] = 0;
      delete[] fErrors[i];
      fErrors[i] = 0;
    }

  delete[] fValues;
  delete[] fErrors;

  fValues = 0;
  fErrors = 0;
  fDimension = 0;
  fSize = 0;
  fStart.setTics(0);
  fEnd.setTics(0);
}

//_____________________________________________________________________________
bool
emcCalibrationData::Set(size_t index, float value, float error, size_t dim)
{
  bool rv = false;

  if ( index < fSize &&
       dim < fDimension )
    {
      fValues[index][dim] = value;
      fErrors[index][dim] = error;
      rv = true;
    }
  return rv;
}

//_____________________________________________________________________________
void
emcCalibrationData::SetTypeAndSize(EType type, size_t size)
{
  Reset();

  fType = type;
  fSize = size;

  fRange = 1;
  fDimension = 1;

  if (fType == kIniCal)
    {
      fDimension = 3;
      fRange = 8;
    }
  else if (fType == kTofSectorOffset)
    {
      fDimension = 5;
      // dim 0 = run,number of events
      // dim 1 = peak, width
      // dim 2 = gaus peak, gaus width
      // dim 3 = bbcT0, bbcT0rms
      // dim 4 = toft0, tofT0rms
      fRange = 8;
    }

  if (fSize)
    {

      fValues = new float * [fSize];
      fErrors = new float * [fSize];

      size_t i;
      size_t j;

      for ( i = 0; i < fSize; i++ )
        {
          fValues[i] = new float[fDimension];
          fErrors[i] = new float[fDimension];
          for ( j = 0; j < fDimension; j++ )
            {
              fValues[i][j] = fErrors[i][j] = 0.0;
            }
        }
    }
}





