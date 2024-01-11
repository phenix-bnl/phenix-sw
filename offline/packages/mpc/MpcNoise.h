
#ifndef __MPCNOISE_H__
#define __MPCNOISE_H__

#include <string>
#include <PHTimeStamp.h>
#include <phool.h>
#include <PdbMpcNoise.hh>

// Data Structure for PdbMpcNoise array w/in MpcNoise object,
// | ch | Nthr1 | Nthr2 | Nthr3| Nthr4 |
//-----------------------------------
// |  0 |  1000 |  800  |  500 |  100  |
// |  1 |  1059 |  820  |  540 |  150  | 
// |  . |   .   |   .   |   .  |   .   |
// |  . |   .   |   .   |   .  |   .   |
// |  . |   .   |   .   |   .  |   .   |
// | 575|  1004 |  850  |  501 |  90   | 
// |    | thr1  | thr2  | thr3 | thr4  |
// |    | Run   | Ntot  |      |       |


class MpcNoise
{
public:

  MpcNoise();
  MpcNoise(const int runnumber);
  MpcNoise(const std::string& dbase_location);
  virtual ~MpcNoise() {}
  
  int getN(const int ch,const int thr) {
    if( thr<0 || thr>=MAXTHR )
      {
	std::cout << PHWHERE << "Invalid Threshold\n";
	return -999;
      }
    if( ch>=0 && ch<MAXCH+2 )
      return mpcPdbNoise[ch].get_N(thr);
    else if(ch == -1)  //thesholds
      return mpcPdbNoise[MAXCH].get_N(thr);  
    else if(ch == -2)  //runnumber
      return mpcPdbNoise[MAXCH+1].get_N(0);
    else if(ch == -3) // Nevents
      return mpcPdbNoise[MAXCH+1].get_N(1);
    else
      std::cout << PHWHERE << "Invalid Channel\n";
    return -999;
  }

  short getCh(const int ch) {
    if(ch>=0 && ch<MAXCH+2)
      // return (short) ch;  //this works too
      return mpcPdbNoise[ch].get_fee576();
    else
      std::cout << PHWHERE << "Invalid Channel\n";
    return -999;
  }

  //store thresholds in entry MAXCH=576, Nevents in MAXCH+1 in ch1,
  //runnumber in MAXCH+1 in ch0
  int getThresh(int thr) { return getN(-1,thr); }
  int getNtot() { return getN(-3,0); }
  int getRun() { return getN(-2,0);  }
  int getNumCh() { return MAXCH; }
  int getNumThr() { return MAXTHR; }

  void setN(const int ch, const int thr, const int N) {
    if( thr<0 || thr>=MAXTHR )
      {
	std::cout << PHWHERE << "Invalid Threshold\n";
	return;
      }
    if(ch>=0 && ch<MAXCH+2)
      mpcPdbNoise[ch].set_N(thr,N);
    else if(ch == -1)
      mpcPdbNoise[MAXCH].set_N(thr,N);
    else if(ch == -2)
      mpcPdbNoise[MAXCH+1].set_N(0,N);
    else if(ch == -3)
      mpcPdbNoise[MAXCH+1].set_N(1,N);
    else
      std:: cout << PHWHERE << "Incorrect channel setting\n";
  }

  void setThresh(const int thr, const int threshold){ setN(-1,thr,threshold); }
  void setNtot(const int Ntot){ setN(-3,0,Ntot); }
  void setRun(const int runnumber){ setN(-2,1,(int)runnumber);  }
  void setCh(const int ch, const short value) {
    if(ch>=0 && ch<MAXCH+2)
      // return (short) ch;  //this works too
      mpcPdbNoise[ch].set_fee576(value);
    else
      std::cout << PHWHERE << "Incorrect channel setting" << std::endl;
  }

  void setCh(const int ch) {
    if(ch>=0 && ch<MAXCH+2)
      // return (short) ch;  //this works too
      mpcPdbNoise[ch].set_fee576((short) ch);
    else
      std::cout << PHWHERE << "Incorrect channel setting" << std::endl;
  }

  
  int Download_Noise(const int& runnumber);
  int Download_Noise(const std::string& dbase_file);
  int Download_Noise(const PHTimeStamp& tstamp, const std::string& what);
  

  int StoreInDatabase(PHTimeStamp& tStart, const std::string& what, const std::string& username = "", const std::string& description = "");
  int StoreInDatabase(const int& runnumber, const std::string& what, const std::string& username, const std::string& description);

  //int  Download(const PHTimeStamp& tstamp, const std::string& what);
  //int  Download(const std::string& dbase_file, const std::string& what);
 
  void Dump(const std::string& what = "NOISE");
  
  PHTimeStamp *getStartTime() { return &StartTime; }
  PHTimeStamp *getEndTime() { return &EndTime; }

  void Reset();
  void Print();
  int IsValid(const int verbosity = 0) const;

private:

  static const int MAXTHR = 4;
  static const int MAXCH = 576;
  static const int MAXAMU = 64;

  int status;

  PHTimeStamp StartTime;        // calibration start and end time
  PHTimeStamp EndTime;

  PdbMpcNoise mpcPdbNoise[MAXCH+2];
  
 
};

#endif	// __MPCNOISE_H__

