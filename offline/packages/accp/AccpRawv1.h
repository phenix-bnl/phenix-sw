#ifndef __ACCPRAWV1_H__
#define __ACCPRAWV1_H__

#include <iostream>
#include "Accp.h"
#include "AccpRaw.h"

class AccpRawv1 : public AccpRaw 
{
public:
  AccpRawv1();
  virtual ~AccpRawv1();

  void identify(std::ostream& os = std::cout) const
  {
    os << "identify yourself: I am a AccpRawv1 object" << std::endl;
    return;
  }

  int  isValid() const { return 1; }
  void Reset();

  void SetAdcHPost(int ich, int val ) { adchpost[ich]  = val; }
  void SetAdcHPre(int ich, int val ) { adchpre[ich]  = val; }
  void SetAdcLPost(int ich, int val ) { adclpost[ich]  = val; }
  void SetAdcLPre(int ich, int val ) { adclpre[ich]  = val; }

  void SetTdc(int ich, int val ) { tdc[ich] = val; } 

  int GetAdcHPost(int ich)  const { return adchpost[ich]; }
  int GetAdcHPre(int ich)  const { return adchpre[ich]; }
  int GetAdcLPost(int ich)  const { return adclpost[ich]; }
  int GetAdcLPre(int ich)  const { return adclpre[ich]; }

  int GetTdc(int ich) const { return tdc[ich]; }
 
protected:

  int adchpost[ACCP_NCH]; // high gain post-sample
  int adchpre[ACCP_NCH]; // high gain pre-sample
  int adclpost[ACCP_NCH]; // low gain post-sample
  int adclpre[ACCP_NCH]; // low gain pre-sample
  int tdc[ACCP_NCH];

  ClassDef(AccpRawv1,1)    
};

#endif
