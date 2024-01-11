#ifndef DIDLVL1LVL2FIRE_H
#define DIDLVL1LVL2FIRE_H

class PHCompositeNode;

class DidLvl1Lvl2Fire
{
 public:

  DidLvl1Lvl2Fire(const char *Lvl1Namein, const char *Lvl2Namein);
  bool GetDecision(PHCompositeNode *topnode);
  const char *GetLvl1Name() {return Lvl1Name;}
  const char *GetLvl2Name() {return Lvl2Name;}

 private:

  int got_names_lvl1;
  int got_names_lvl2;

  const char *Lvl1Name;
  const char *Lvl2Name;
  
  int lvl1trigbit;
  int lvl2trigbit;

  bool dbg;
  bool lvl1only;

};

#endif
