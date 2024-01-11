//This code set the raw information for TofW
//ich=0 bottom ich=1 top

#ifndef __TOFWRAW_H_
#define __TOFWRAW_H_

#include <PHObject.h>
#include <phool.h>

#include <iostream>

class TofwSnglRaw;

class TofwRaw : public PHObject 
{
 public:
  virtual ~TofwRaw(){}

  virtual TofwRaw* clone() const{
    std::cout << "Clone method not implemented for your version of CentralTracks" << std::endl;
    return 0;
  }

  // the standard PHObject response functions
  virtual void Reset(){
    std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function " << std::endl;
    return;
  }

  int isValid() const{
    std::cout << PHWHERE << "isValid() not implemented by daughter function " << std::endl;
    return 0;
  }

  void identify(std::ostream& os=std::cout) const{
    os << "identify yourself: virtual MrpcRaw object " << std::endl;
    return;
  }

  // actual implementations of the set/get methods
  virtual void set_nraw (const unsigned int NRAW) {
    std::cout<<"TofwRaw::Error set_nraw not overridden"<<std::endl; 
    return;
  }

  virtual int  get_nraw () const {
    std::cout<<"TofwRaw::Error set_nraw not overridden"<<std::endl;
    return 0;
  }


  // routines to manipulate the particle array
  virtual int set_TClonesArraySize (const unsigned int nch){return 0;}
  virtual void AddRaw              (const unsigned int ich){return;}
  virtual void RemoveRaw           (const unsigned int ich){return;}

  virtual TofwSnglRaw* AddRaw      (const unsigned int ich, const TofwSnglRaw& sngl){return NULL;}
  virtual TofwSnglRaw* get_raw     (const unsigned int ich) const{
    std::cout << "Single Track return not implemented for your version of tracks" << std::endl;
    return 0;
  }

  
  virtual void set_boxid(const int iraw, const int val){warning("boxid");}
  virtual void set_chamberid(const int iraw, const int val){warning("chamberid");}
  virtual void set_stripid(const int iraw, const int val){warning("stripid");}
  virtual void set_t3(const int iraw, const int ich, const int val){warning("t3");}
  virtual void set_t4(const int iraw, const int ich, const int val){warning("t4");}
  virtual void set_q1(const int iraw, const int ich, const int val){warning("q1");}
  virtual void set_q3(const int iraw, const int ich, const int val){warning("q3");}
  virtual void set_tvc(const int iraw, const int ich, const float val){warning("tvc");}
  virtual void set_qvc(const int iraw, const int ich, const float val){warning("qvc");}
  
  
  
  virtual int get_boxid(const int iraw) const { return -9999; }
  virtual int get_chamberid(const int iraw) const { return -9999; }
  virtual int get_stripid(const int iraw) const { return -9999; }
  virtual int get_t3(const int iraw, const int ich) const { return -9999; }
  virtual int get_t4(const int iraw, const int ich) const { return -9999; }
  virtual int get_q1(const int iraw, const int ich) const { return -9999; }
  virtual int get_q3(const int iraw, const int ich) const { return -9999; }
  virtual float get_tvc(const int iraw, const int ich) const { return -9999; }
  virtual float get_qvc(const int iraw, const int ich) const { return -9999; }

 private:
  void warning(const char* field) const {
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout <<"TOFW RAW Offending field == " << field << std::endl;
  }

  ClassDef(TofwRaw,1)
};

#endif
