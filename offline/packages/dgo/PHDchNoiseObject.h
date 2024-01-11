#ifndef PHDCHNOISEOBJECT_H
#define PHDCHNOISEOBJECT_H
 
#include <PHDchAddressObject.h>
#include <PHNoiseObject.h>
#include <DchBasicNoise.h>

class PHDchNoiseObject: public PHNoiseObject { 

public: 
  PHDchNoiseObject();
  PHDchNoiseObject(PHDchAddressObject *add);  
  virtual ~PHDchNoiseObject();
 
  void    clearAndDestroy() { noiseList.clearAndDestroy(); zeroArray();}
  virtual void initialize();
  virtual void zeroArray();
  PHBoolean setFileName(const char* noise,const char* effi="DchEfficiency.Real");
  
  virtual PHBoolean fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID);  
  virtual PHBoolean update(PHTimeStamp &Tstart, PHTimeStamp &Tstop,
                                 const char *calibname, PdbBankID , const char *descrip );

  virtual PHBoolean updateValidityTimeForLastBank(PHTimeStamp&, PHTimeStamp&, PHTimeStamp&, const char *, PdbBankID, int force = 0);
  virtual PHBoolean fetchNoiseFromFile();
  virtual PHBoolean writeNoiseInFile();
  void    appendNoise(DchBasicNoise* noise) { noiseList.append(noise);}
  void    setNoisyDeadArray(int ind,short val) {noisyDeadArray[ind] = val;}

  PHBoolean status(const int global) const;
  float     getEfficiency(const int global) const;
  DchBasicNoise* getNoise(int global);
  
private:
  DchBasicNoise* basicNoise;
  PHPointerList<DchBasicNoise> noiseList;
  short noisyDeadArray[numberOfArms*numberOfSides*numberOfPlanes*numberOfCells];
  float efficiencyArray[numberOfArms*numberOfSides*numberOfPlanes*numberOfCells];
  PHDchAddressObject *dchaddress;
  const char* effiFile;
  
}; 

#endif /* PHDCHNOISEOBJECT_H */ 
