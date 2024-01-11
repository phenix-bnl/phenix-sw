#ifndef __FCLOUT__
#define __FCLOUT__

#include <iostream>
#include <PHObject.h>
#include <FclConsts.h>

class Packet;
class FclCalib;
class FclRaw;

class FclOut: public PHObject
{

 public:

  virtual ~FclOut() {};
  virtual void readData(Packet *packet, int iSide) {warning("readData()");}
  virtual void readData(FclRaw *data) {warning("readData()");}
  virtual void calibrateData(FclCalib &calib) {warning("calibrateData()");}

  virtual void Reset() {warning("Reset()");}

  virtual void identify(std::ostream& out = std::cout) const {out<<"virtual FclOut object"<<std::endl;}
  virtual int isValid() const {warning("isValid()"); return 0;}

  virtual void print() const {warning("print()");}

  // GETS:
  virtual int getSide() {
    warning("getSide()");
    return FCL_INVALID_INT;}

  virtual float getLowGain(int channel) {
    warning("getLowGain(int channel)");
    return FCL_INVALID_FLOAT;}

  virtual float getLowGain(int row, int col) {
    warning("getLowGain(int row,int col)");
    return FCL_INVALID_FLOAT;}

  virtual float getSumAll() {
    warning("getSumAll()");
    return FCL_INVALID_FLOAT;}
  
  virtual float getSumGrey() {
    warning("getSumGrey()");
    return FCL_INVALID_FLOAT;}
  

  //SETS:
  virtual int setLowGain(int channel, float value){
    warning("setLowGain(int channel, int value)");
    return FCL_INVALID_INT;}

  virtual int setSumAll(float sum) {
    warning("setSumAll(float sum)");
    return FCL_INVALID_INT;}
  
  virtual int setSumGrey(float sum) {
    warning("setSumGrey(float sum)");
    return FCL_INVALID_INT;}

  virtual int computeSums(){
    warning("setSumGrey(float sum)");
    return FCL_INVALID_INT;}
 
 private:

  void warning(const char *funcsname) const
    {
      std::cout << "BIG PROBLEM...FCL::" << funcsname << " is virtual, doing nothing" << std::endl;
      return ;
    }

  ClassDef(FclOut,1)
};


#endif 
