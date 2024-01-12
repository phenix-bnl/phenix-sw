#ifndef __TRIGSELECT_H__
#define __TRIGSELECT_H__

#include <SubsysReco.h>
#include <TriggerHelper.h>
#include <TriggerUtilities.h>

#include <map>
#include <set>
#include <string>
#include <vector>

class TrigSelect: public SubsysReco
{
 public:
		 
	//! constructor
  TrigSelect(const std::string &name = "TRIGSELECT");
  
	//! destructor
	virtual ~TrigSelect() {}

	int InitRun(PHCompositeNode *topNode);
	//! event method
  int process_event(PHCompositeNode *topNode);
	
	//! print method
  void Print(const std::string&) const;

	//! 
  int AddTrigger(const char *name);
  int RemoveTrigger(const char *name);
  int SetReturnCode(const char *action = "DISCARD");
  int SetDefaultReturnCode(const char *action = "OKAY");
  int AddNoSaveTrigger(const TrigSelect *trigsel);
  int AddNoSaveTrigger(const std::string &name);
  int AddVetoTrigger(const std::string &name);
  int RemoveVetoTrigger(const char *name);
  void SetMaxFires(const int firelimit);
  unsigned int GetNFired() const {return nfired;}
  unsigned int GetNTrigSelected()  const {return nTrigSelected;}
  void GetTriggerNames(std::vector<std::string> &namevec) const;

 protected:

	//! trigger helper
	/*! 
		it is stored as a member here so that the initialize() method
		called in constructor is called only once. The SetNodes method 
		needed to keep local pointers to the needed nodes is called every 
		event
	*/
  TriggerHelper _trigger_helper;
	
	//! trigger map
  typedef std::map<std::string, unsigned int> TriggerMap;

	//! name of triggers to select
  TriggerMap _triggers;
  
	//! trigger set
  typedef std::set<std::string> TriggerSet;
	
	//! name of triggers to veto on
  TriggerSet _triggers_veto;
	
	//! name of triggers to not save
  TriggerSet _triggers_nosave;

        //! save trigger masks selected by not saving
  std::set<unsigned int> nosave_bits;

	//! return code if any Trigger selection is true
  int RetCode;
	
	//! return code if no Trigger selection is true
  int DefaultRetCode;
	
	//! counter on events for which triggers fired
  unsigned int nfired;
	
	//! max number of events on which triggers fired
  unsigned int maxfires;
	
	//! counter on number of events not vetoed by any trigger
  unsigned int n_not_vetoed;  

	//! counter on number of events where selected trigger fired (also veto)
  unsigned int nTrigSelected;

        //! bitmask for not saved triggers, so we can see if trigger bits of non vetoed trigs fired
  unsigned int nosave_trig_mask;

        //! check for first event
  int first;
};

#endif /* __TRIGSELECT_H__ */
