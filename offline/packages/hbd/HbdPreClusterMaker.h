//--------------------------------------------------- 
// Class: HbdPreClusterMaker
//--------------------------------------------------- 


#ifndef HBDPRECLUSTERMAKER_HH__
#define HBDPRECLUSTERMAKER_HH__

#include "hbdDetectorGeo.hh"

#include <vector>

class HbdCellList;
class HbdFinalSimSupport;

struct PadsInCluster{
	int sector;
	int padnum;
	int padkey;
	float charge;
	float threshold;
};

class SortPads {
	public:
		bool operator()(const PadsInCluster& first, const PadsInCluster &second){
			return first.charge > second.charge;
		}
};

class FindPad {
	private:
		int t_padkey;

	public:
		FindPad(int p_) : t_padkey(p_){}
		bool operator()(const PadsInCluster& e){
			return e.padkey == t_padkey;
		}
};

class HbdPreClusterMaker{ 

public:
  HbdPreClusterMaker(int runnum);                           // constructor
  virtual ~HbdPreClusterMaker();                            // destructor
	void SetVerbose(int a){verbose = a;}
	void get_padkeys(int ,int ,double , double ,std::vector<PadsInCluster> &);
	void get_padkeys_phiframe(int,int,double ,double ,int ,std::vector<PadsInCluster> &);
	void get_padkeys_zframe(int, int, double ,double ,int ,std::vector<PadsInCluster> &);
	void get_padkeys_zphiframe(int, int, double ,double ,int ,std::vector<PadsInCluster> &);
	void get_padkeys_regular(int, int, int ,std::vector<PadsInCluster> &);
	void Print(int ,int, int );
  int region_find(int ,double ,double ,double ,double ,double ,double );
	double get_padx(){return pad_x;}
	double get_pady(){return pad_y;}
	int get_padtype(){return pad_type;}

protected:
	int verbose;
  hbdDetectorGeo hbdgeo;
  HbdFinalSimSupport *simsupport;
	float mypad_center[192][2];
	int mypad_type[192];
	double pad_x, pad_y;
	int pad_type;

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

	double r2d, d2r;
}; 

#endif /* __HbdPreClusterMaker_HH__ */

