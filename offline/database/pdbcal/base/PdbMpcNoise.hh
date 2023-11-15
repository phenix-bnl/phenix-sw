#ifndef __PDBMPCNOISE_HH__
#define __PDBMPCNOISE_HH__

#include <PdbCalChan.hh>
#include <phool.h>


class PdbMpcNoise : public PdbCalChan {
 public:
  PdbMpcNoise();
  virtual ~PdbMpcNoise(){}
  int get_N(const short ithr){ 
    if(ithr < 4) 
      return N_thr[ithr];
    else
      std::cout << PHWHERE << "incorrect threshold index\n";
  
    return -999;
  }
  int get_fee576(){ return fee576; }
  
  void set_N(const short ithr, int N){  
    if( ithr<4 )
      N_thr[ithr] = N;
    else
      std::cout << PHWHERE << "incorrect threshold index\n";
  }
  
  void set_fee576(const short ich) { fee576 =  ich; }
  void set(const int ich, const int N1, const int N2,
	   const int N3, const int N4){
    fee576 =  ich;
    N_thr[0] = N1;
    N_thr[1] = N2;
    N_thr[2] = N3;
    N_thr[3] = N4;
  }

  void set(const short ich, const int N1, const int N2,
	   const int N3){
    fee576 = ich;
    N_thr[0] = N1;
    N_thr[1] = N2;
    N_thr[2] = N3;
  }
  
  virtual void Reset();
  virtual void print() const;
  
private:
  short fee576;	// FEE Channel
  int N_thr[4]; //N hits at each of four thresholds

  ClassDef(PdbMpcNoise,1);

};

#endif /* __PDBMPCNOISE_HH__ */
