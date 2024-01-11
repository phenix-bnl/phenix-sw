#ifndef __FCLRAW__
#define __FCLRAW__

#include <iostream>
#include "PHObject.h"
#include "FclConsts.h"

class Packet;

class FclRaw: public PHObject
{

 public:

  virtual ~FclRaw() {};
  virtual void readData(Packet *packet, int iSide) {warning("readData()");}

  virtual void Reset() {warning("Reset()");}

  virtual void identify(std::ostream& out = std::cout) const {out<<"virtual FclRaw object"<<std::endl;}
  virtual int isValid() const {warning("isValid()"); return 0;}

  // GETS:
  virtual int getHighAdcPost(int channel) {
    warning("getHighAdcPost(int channel)");
    return FCL_INVALID_INT;}
  virtual int getHighAdcPre(int channel) {
    warning("getHighAdcPre(int channel)");
    return FCL_INVALID_INT;}
  virtual int getHighGain(int channel) {
    warning("getHighGain(int channel)");
    return FCL_INVALID_INT;}
  
  virtual float getHighGainCalib(int channel) {
    warning("getHighGainCalib(int channel)");
    return FCL_INVALID_FLOAT;}
  virtual float getLowGainCalib(int channel) {
    warning("getLowGainCalib(int channel)");
    return FCL_INVALID_FLOAT;}

  virtual int getSide() {
    warning("getSide()");
    return FCL_INVALID_FLOAT;}
  

  //SETS:
  virtual int setHighAdcPost(int channel, int value){
    warning("setHighAdcPost(int channel, int value)");
    return FCL_INVALID_INT;}
  virtual int setHighAdcPre(int channel, int value){
    warning("setHighAdcPre(int channel, int value)");
    return FCL_INVALID_INT;}

  virtual int getCalibration(){
    warning("getCalibration()");
    return FCL_INVALID_INT;}
  

 // GETS:
  virtual int getTdc(int channel) {
    warning("getTdc(int channel)");
    return FCL_INVALID_INT;}
  virtual int getLowAdcPost(int channel) {
    warning("getLowAdcPost(int channel)");
    return FCL_INVALID_INT;}
  virtual int getLowAdcPre(int channel) {
    warning("getLowAdcPre(int channel)");
    return FCL_INVALID_INT;}
  virtual int getLowGain(int) {
    warning("getLowGain(int)");
    return FCL_INVALID_INT;}
  
  //SETS:
  virtual  int setTdc(int channel, int value){
    warning("setTdc(int channel, int value)");
    return FCL_INVALID_INT;}
  virtual int setLowAdcPost(int channel, int value){
    warning("setLowAdcPost(int channel, int value)");
    return FCL_INVALID_INT;}
  virtual int setLowAdcPre(int channel, int value){
    warning("setLowAdcPre(int channel, int value)");
    return FCL_INVALID_INT;}

 private:

  void warning(const char *funcsname) const
    {
      std::cout << "BIG PROBLEM...FCL::" << funcsname << " is virtual, doing nothing" << std::endl;
      return ;
    }

  ClassDef(FclRaw,1)
};


#endif 
