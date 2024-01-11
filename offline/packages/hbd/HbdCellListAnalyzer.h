//--------------------------------------------------- 
// Class: HbdCellListAnalyzer
//--------------------------------------------------- 


#ifndef __HBDCELLLISTANALYZER_HH__
#define __HBDCELLLISTANALYZER_HH__

#include <set>
#include <vector>

class HbdCellList;
class PHCompositeNode;

class SortUnitPadCharge {
	public:
		bool operator()(const float first, const float second){
			return first > second;
		}
};


class HbdCellListAnalyzer{ 

public:
  HbdCellListAnalyzer();                             // constructor
  virtual ~HbdCellListAnalyzer();                            // destructor
  void SetVerbose(int a){verbose = a;}
  int  Process(PHCompositeNode *);
  void Init();
  void Expand(HbdCellList *);
  void CalculateModuleProperty(HbdCellList *);
  void Subtract(HbdCellList *);
  
  float GetModuleMean(int a ){return charge[a];}
  float GetModuleRMS(int a ){return rms[a];}
  float GetModuleMedian(int a ){return median[a];}
  void setMCFlag (int flag = 0){mc_flag = flag;}
  
protected:
	int verbose;
	float bbcq;

	enum{
		ES0,
		ES1,
		ES2,
		ES3,
		ES4,
		ES5,
		EN0,
		EN1,
		EN2,
		EN3,
		EN4,
		EN5,
		WS0,
		WS1,
		WS2,
		WS3,
		WS4,
		WS5,
		WN0,
		WN1,
		WN2,
		WN3,
		WN4,
		WN5
	};
  float charge[24];
  float hitarea[24];
  float rms[24];
  float median[24];
  float unit_hip_high_threshold[24];
  float unit_threshold[24];
  std::set <int> hip[24];
  std::vector <float > unitpadcharge[24];
  int mc_flag;

}; 

#endif /* __HbdCellListAnalyzer_HH__ */

