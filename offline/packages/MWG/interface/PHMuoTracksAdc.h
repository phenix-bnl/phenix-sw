#ifndef __PHMUOTRACKSADC__
#define __PHMUOTRACKSADC__

#include "PHObject.h"

class PHMuoTrackAdc;

class PHMuoTracksAdc : public PHObject{
public:
  PHMuoTracksAdc(const char *content_class="");
  virtual ~PHMuoTracksAdc(void);
  virtual PHMuoTracksAdc* Create(const char *content_class="");
  virtual void Reset(void);

  virtual PHMuoTrackAdc* Insert(void);
  virtual void Remove(PHMuoTrackAdc *trk_adc);
  virtual void Remove(int itrk);
  virtual unsigned int GetSize(void);
  virtual PHMuoTrackAdc* Get(int itrk);
  virtual const char* GetContentName(void);

  // Create latest version
  static PHMuoTracksAdc* NewPHMuoTracksAdc(void);

protected:
  TClonesArray *muotrks_adc;

  ClassDef(PHMuoTracksAdc,1)
};

#endif /* __PHMUOTRACKSADC__ */
