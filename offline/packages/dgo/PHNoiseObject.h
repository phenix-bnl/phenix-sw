#ifndef PHNOISEOBJECT_H
#define PHNOISEOBJECT_H
 
#include <PdbParameter.hh>
#include <PHAddressObject.h>
#include <PHTimeStamp.h>

class PdbBankManager;  
class PdbApplication;  
class PdbCalBank;     

class PHNoiseObject { 

public: 
  PHNoiseObject();
  PHNoiseObject(PHAddressObject *add);  
  virtual ~PHNoiseObject();
 

  virtual PHAddressObject* getAddress() const { return address;}
  virtual void print();
  virtual void initialize() = 0;

 
  const PHTimeStamp getStartValTime() const;
  const PHTimeStamp getEndValTime()   const;

  virtual PHBoolean commit();
  virtual PHBoolean validate(PHTimeStamp &Tsearch); 
  virtual PHBoolean fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID) = 0;  
  virtual PHBoolean update(PHTimeStamp &Tstart, PHTimeStamp &Tstop,
                                 const char *calibname, PdbBankID , const char *descrip ) = 0;
  
  virtual const PdbCalChan* get(int index) const ;
  virtual int   getBankLength() ;

  virtual int   getNumberOfNoisyChannels() { return numberOfNoisyChannels; }
  virtual void  setNumberOfNoisyChannels(int num) { numberOfNoisyChannels = num; }
  
  virtual int   getNoiseParameters() { return noiseParameters; }
  virtual void  setNoiseParameters(int num) { noiseParameters = num; }

  virtual int   getNoiseThreshold() { return noiseThreshold; }
  virtual void  setNoiseThreshold(int num) { noiseThreshold = num; }
  
protected:
  
  const char* noiseFile;
  short committed;
  int numberOfNoisyChannels;
  int noiseParameters;
  int noiseThreshold;
  PHAddressObject *address;
  
  PdbBankManager *bankManager;
  PdbApplication *application;
  PdbCalBank     *noiseBank;   
  PHTimeStamp    start, stop;  
}; 

#endif /* PHNOISEOBJECT_H */ 
