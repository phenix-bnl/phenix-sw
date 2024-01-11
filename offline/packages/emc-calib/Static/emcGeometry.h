#ifndef __EMCGEOMETRY_H__
#define __EMCGEOMETRY_H__

#include <iostream>
#include <vector>
#ifndef __EMCMANAGEABLE_H__
#include "emcManageable.h"
#endif
#ifndef __SECGEOM_H__
#include "SecGeom.h"
#endif
#ifndef __PHTIMESTAMP_H__
#include "PHTimeStamp.h"
#endif

/** Geometry object that can be passed to DataManager.*/

class emcGeometry : public emcManageable
{

  /// output stream.
  friend std::ostream& operator<<(std::ostream& out, const emcGeometry& obj);

public:

  /// default ctor.
  emcGeometry();

#ifndef __CINT__
  /// ctor with given sectors.
  emcGeometry(const std::vector<SecGeom>& sectors)
  {
    Set(sectors);
  }
#endif

  /// copy ctor.
  emcGeometry(const emcGeometry& obj);

  /// assignment operator.
  emcGeometry& operator=(const emcGeometry& obj);

  /// dtor.
  virtual ~emcGeometry();

  /// Category = "geom.emc.Sectors"
  virtual const char* GetCategory(void) const
  {
    return "geom.emc.Sectors";
  }

  /// End-of-validity time.
  const PHTimeStamp& GetEndValTime(void) const
  {
    return fEnd;
  }

  /// Start-of-validity time.
  const PHTimeStamp& GetStartValTime(void) const
  {
    return fStart;
  }

  /// Tells if geometry is valid at a given time.
  bool IsValid(const PHTimeStamp& when) const;

  /// Returns the maximum number of sectors handled by this geometry.
  static size_t MaxNumberOfSectors(void)
  {
    return fgNumberOfSectors;
  }

  /// Actual number of sectors.
  size_t NumberOfSectors(void) const
  {
    return fSectors.size();
  }

  /// Output on screen.
  void Print(void) const
  {
    std::cout << *this << std::endl;
  }

  /// Only output the sector corners.
  void PrintCorners(void) const;

#ifndef __CINT__
  /// Itinialize geometry from sectors.
  void Set(const std::vector<SecGeom>& sectors)
  {
    fSectors = sectors;
  }
#endif

  /// Reset the object.
  void Reset(void);

  /// Get an handle to a sector, given its number.
  const SecGeom& Sector(size_t iS) const;

  // Set the validity period.
  void SetValidityPeriod(const PHTimeStamp& t1, const PHTimeStamp& t2)
    {
      fStart = t1;
      fEnd = t2;
    }

 protected:

  void Copy(emcGeometry&) const;

private:
  std::vector<SecGeom> fSectors;
  static size_t fgNumberOfSectors;
  PHTimeStamp fStart;
  PHTimeStamp fEnd;
};

#endif
