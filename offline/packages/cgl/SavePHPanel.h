#ifndef __SAVEPHPANEL_H
#define __SAVEPHPANEL_H

#include "PHObject.h"

class PHPanel;

class SavePHPanel : public PHObject
{
 public:

  virtual ~SavePHPanel() {}

  virtual int AddPanel(const PHPanel *panel, const int iarm, const int index, const char *det) {return -1;}
  //virtual const PHPanel *GetPHPanel() const {return NULL;}
  virtual double GetPoint(const short int i, const short int index) const;
  virtual const char *GetName() const { return "NONE"; }
  virtual int GetArm() const {return -1;}
 
  ClassDef(SavePHPanel,1)
};

#endif /* __SAVEPHPANEL_H */
