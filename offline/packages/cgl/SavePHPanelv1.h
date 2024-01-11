#ifndef __SAVEPHPANELV1_H
#define __SAVEPHPANELV1_H

#include "TString.h"
#include "SavePHPanel.h"

class PHPanel;

class SavePHPanelv1 : public SavePHPanel
{
 public:

  SavePHPanelv1() {}
  virtual ~SavePHPanelv1() {}

  int AddPanel(const PHPanel *panel, const int iarm, const int index, const char *det);
  double GetPoint(const short int i, const short int index) const;
  const PHPanel *GetPanel() {return NULL;}
  const char *GetName() const { return Detector.Data(); }
  int GetArm() const {return arm;}

 protected:
  TString Detector;
  int arm;
  int Index;
  double p0[3];
  double p1[3];
  double p2[3];

  ClassDef(SavePHPanelv1,1)

};

#endif /* __SAVEPHPANELV1_H */
