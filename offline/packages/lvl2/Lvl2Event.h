#ifndef __LVL2EVENT_H__
#define __LVL2EVENT_H__

#include <phool.h>
#include <Lvl2DBAccessor.h>
#include <Lvl2PostgresDBAccessor.h>
#include <Lvl2Control.h>
#include <phenixLvl2.h>

class TrigRunLvl1;
class TrigRunLvl2;
class PHCompositeNode;
class Event;

class Lvl2Event

#define BUFFERSIZE 1638400

{
public:
  
  Lvl2Event();
  Lvl2Event(bool pisa_file);
  virtual ~Lvl2Event() {}
  
  void WriteTrigRunLvl2Cal(PHCompositeNode *topNode);

  PHBoolean SaveToDST(PHCompositeNode *);
  PHBoolean SaveToDST(PHCompositeNode*, Event*, const char* = "L2Decision", const char* = "Lvl2OutArray");
  PHBoolean SetLvl2EventPointer(PHCompositeNode *,const char *PRDFNodeName);
  PHBoolean RunL2TriggersPISAToDST(PHCompositeNode *root);
  PHBoolean WritePisaLvl2ToDST(PHCompositeNode *root);
  PHBoolean SavePrdfLvl2ToDST(PHCompositeNode *root);
  PHBoolean RunL2TriggersPRDFToDST(PHCompositeNode *root);
  PHBoolean RunL2TriggersPRDFToDST(Event *evt, PHCompositeNode *root);
  void DumpL2Statistics();

  PHBoolean initializeLevel2(Lvl2String& triggerFileName);
  PHBoolean setupTriggers(Lvl2String& triggerFileName);
  PHBoolean GetRun2Lvl2Decision(Event *evt, int nalgaccepted[], int algacceptedlist[], int lvl1acceptedlist[]);
  
  // fills TrigRun objects for running in onCal / preco situations
  void fillTrigRunCal(TrigRunLvl1 *trigRunLvl1Cal, TrigRunLvl2 *trigRunLvl2Cal);
  

  // fills in the algorithm index for referencing the decision packet info 
  // returns < 0 if there was a problem
  // if given no second argument it uses it's own
  // class member lvl2Control to find the 
  // index mapping (for when running triggers inside Lvl2Event)
  // [In fact just accesses the global Lvl2Registry for lvl2Control]
  // if given a runNumber, looks in the database.
  int fillLvl2AlgIndexes(TrigRunLvl2 * trunl2, int runNumber = -1);
  

  int restoreConfigFromObjy(int runNum);

 private:
  Event *evt;     // Used only for event that the level 2 triggers are run on
  Lvl2DatabaseAccessorBase* dbAccessor_ptr;
  Lvl2Control<> *lvl2Control;
  Lvl2DecisionHistory decisionHistory;
  TrigRunLvl1 * _trigRunLvl1Cal;
  TrigRunLvl2 * _trigRunLvl2Cal;
  int L2AlgToGet;
  
};

#endif // __LVL2EVENT_H__


