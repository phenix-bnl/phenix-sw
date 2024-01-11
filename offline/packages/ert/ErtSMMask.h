#ifndef ERTSMMASK_h
#define ERTSMMASK_h
//================================================
/*
  Created by K.Okada
  Modified by H.Torii for the PostgreSQL.

  How to use...
  rcasxxxx % root
  root [] ErtSMMask m(92446);
  root [] m.Print();
  root [] m.FetchByRunNumber(87202);
  root [] m.Get(arm,sector,sm,triggertype);

  arm = 0 (West) 1(East)
  sector = 0, 1, 2, 3
  sm = 0 - 17 (for PbSc), 0 - 31 (for PbGl)
  triggertype = 0 (EMCal 4x4A, Photon)
                1 (EMCal 4x4B, Photon)
		2 (EMCal 4x4C, Photon)
		3 (EMCal 2x2, Electron)
		4 (RICH 4x5, Electron)

*/
//================================================

#define NARM 2
#define NSECTOR 4
#define NSM 32 
#define NTRIGTYP 5

class ErtSMMask{
 public:
   ErtSMMask(int runNumber=0);
   virtual ~ErtSMMask() {};

   // --- Basic Operator
   void Reset();
   bool FetchByRunNumber(int runNumber);
   void Print();

   // --- Accessor to the mask bits.
   int Set(int arm,int sector,int sm,int triggertype,int bit){
     Mask[arm*(NSECTOR*NSM)+sector*NSM+sm][triggertype]=bit;
     return bit;
   }
   int Get(int arm,int sector,int sm,int triggertype){
     return(Mask[arm*(NSECTOR*NSM)+sector*NSM+sm][triggertype]);
   }
   void SetVersion(int v){version=v;};
   void SetFirstRun(int r){first_run=r;};
   void SetLastRun(int r){last_run=r;};
   int GetVersion(){return version;};
   int GetFirstRun(){return first_run;};
   int GetLastRun(){return last_run;};

 private:
   int version;
   int first_run;
   int last_run;
   int Mask[NARM*NSECTOR*NSM][NTRIGTYP];
};

#endif
