#ifndef __EMCQAS_H__
#define __EMCQAS_H__

#ifndef __EMCFEMTUPLE_H__
#include "emcFEMtuple.h"
#endif
#ifndef __EMCQAFEM_H__
#include "emcQAFEM.h"
#endif
#include <string>
#include <map>

typedef char INT8;

/** (OLD) Stores (online) Q&A information for several FEMs.
    \deprecated
    @ingroup oldemccalib
*/

class emcQAs : public emcFEMtuple
{
public:

  // MV 2001/12/04 deleted calls to SetDeadMask(), added fWarnMap;
  emcQAs() : emcFEMtuple(), fDeadMap(0), fWarnMap(0)
  {
    SetExtraRejectListFilename();
  }
  emcQAs(const char* name, const char* title, const char* classname)
    : emcFEMtuple(name, title, classname), fDeadMap(0), fWarnMap(0)
  {
    SetExtraRejectListFilename();
  }

  virtual ~emcQAs()
  {
    delete[] fDeadMap;
    delete[] fWarnMap;
  }

  bool BuildDeadMap(void);

  bool ApplyExtraRejectList(void);

  virtual const char* GetCategory(void) const
  {
    return "QAs";
  }

  Int_t GetDead(int ichannel);
  Int_t GetWarn(int ichannel); // MV 2001/12/08

  Int_t* GetDeadMap(void);
  Int_t* GetWarnMap(void); // MV 2001/12/08

  // MV 2001/12/04  INT32 GetDeadMask(void) const { return fMask; }

  INT32 GetError(int ichannel) const;

  const char* GetExtraRejectListFilename(void) const
  {
    return fExtraRejectListFilename.c_str();
  }

  INT32 GetWarning(int channel) const;

  static INT32 IamDeadMask(void)
  {
    return 0x400;
  }

  // MV 2001/12/04 noone should mess with the masks
  //  void SetDeadMask(INT32 amplMask=0x3, INT32 tofMask=0x0)
  //    {fMoniAmplMask=amplMask; fMoniTofMask=tofMask;}

  void SetExtraRejectListFilename(const char* filename = "" )
  {
    fExtraRejectListFilename = filename;
  }

  static bool WriteDataToFile(const char* producerName, int femCode,
			      const PHTimeStamp& tStart,
			      const PHTimeStamp& tEnd,
			      INT8* errors, INT8* warnings);
private:

  std::string fExtraRejectListFilename;

  Int_t* fDeadMap;
  Int_t *fWarnMap; // MV 2001/12/08

  // MV 2001/12/04 different masks for bad ToF and bad amplitude
  static const INT32 fMoniAmplMask;
  static const INT32 fMoniTofMask;
  static const INT32 fMoniAmplWarnMask;
  static const INT32 fMoniTofWarnMask;

  // MV 2001/12/04 The idea is to put all amplitude- and ToF-related errors
  // found from physics data into the additional reject list (ASCII) file.
  // When this file is read in, the error codes are put into the map.
  // The key of the map is the tower item id, the value is the error code
  // (bitmap). 4 most significant bits of this bitmap are for ToF-related
    // errors, 4 least significant bits are for amplitude-related errors.

    std::map<int, unsigned char> fPhysRejectMap;
    std::map<int, unsigned char> fPhysWarningMap;

    static unsigned char const fPhysAmplMask;
    static unsigned char const fPhysTofMask;

  };

#endif
