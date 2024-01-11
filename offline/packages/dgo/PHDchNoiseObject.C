// Implementation of class file: PHDchNoiseObject.h
// Created by: Federica Messer at Mon Dec 27 13:11:37 1999
#include <PHDchNoiseObject.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>

#include <fstream>
#include <cstdlib>

using namespace std;

PHDchNoiseObject::PHDchNoiseObject():
  PHNoiseObject(),
  dchaddress(NULL)
{
  initialize();
}

PHDchNoiseObject::PHDchNoiseObject(PHDchAddressObject *add):
  PHNoiseObject(add),
  dchaddress(add)
{
  initialize();
}

void
PHDchNoiseObject::initialize()
{
  committed = 1;
  noiseParameters = 3;
  noiseThreshold = 50000; //dummy
  noiseList.clearAndDestroy();
  zeroArray();
}

//___________________________________________________________________
PHBoolean PHDchNoiseObject::setFileName(const char* noise, const char* effi)
{
  noiseFile = noise;
  effiFile = effi;
  if (!noiseFile || !effiFile)
  {
    cout << PHWHERE << "No filename given! " << endl;
    return False;
  } else {
      cout << "PHDchNoiseObject::setFileName - Going to read NOISE from: " << noiseFile << endl;
      cout << "PHDchNoiseObject::setFileName - Going to read EFFICIENCY from: " << effiFile << endl;
      return True;
    }
}

//___________________________________________________________________
void PHDchNoiseObject::zeroArray()
{
  fill(noisyDeadArray,noisyDeadArray+sizeof(noisyDeadArray)/sizeof(short),1);
  fill(efficiencyArray,efficiencyArray+sizeof(efficiencyArray)/sizeof(float),0.95);
}

PHDchNoiseObject::~PHDchNoiseObject()
{
  commit();
  noiseList.clearAndDestroy();
}

float
PHDchNoiseObject::getEfficiency(const int indexGlobal) const
{
  // hard cast to unsigned int  to make compiler happy - < 0 is handled
  if (indexGlobal < 0 || (unsigned int) indexGlobal >= sizeof(efficiencyArray)/sizeof(float))
    {
      cout << PHWHERE << " invalid index for efficiency array: " << indexGlobal << endl;
      exit(1);
    }
  float effi = efficiencyArray[indexGlobal];
  if (effi > 1. || effi < 0) {
    cout << PHWHERE << " Invalid Efficiency: " << effi << " at index " << indexGlobal << endl;
    exit(1);
  }
  return effi;
}

PHBoolean
PHDchNoiseObject::status(const int indexGlobal) const
{
  // if channel is in the list than is noisy !!!
  // hard cast to unsigned int  to make compiler happy - < 0 is handled
  if (indexGlobal < 0 || (unsigned int) indexGlobal >= sizeof(noisyDeadArray)/sizeof(short))
    {
      cout << PHWHERE << " invalid index for dead channel array: " << indexGlobal << endl;
      exit(1);
    }
  if (noisyDeadArray[indexGlobal] == 0) {
    return False;
  }

 return True;
}

DchBasicNoise* PHDchNoiseObject::getNoise(int indexGlobal)
{
  for (unsigned int i = 0; i < noiseList.length(); i++)
    {
      if (noiseList[i]->getGlobalIndex() == indexGlobal)
        {
          basicNoise = noiseList[i];
          return basicNoise;
        }
    }

  return 0;
}


PHBoolean
PHDchNoiseObject::update(PHTimeStamp &Tstart, PHTimeStamp &Tstop,
                         const char *calibname,
                         PdbBankID bankID,
                         const char *descrip )
{
  if (committed == 1)
    {
      if (!application->startUpdate())
        {
          PHMessage("PHNoiseObject", PHError, "Aborting ... Database not writable");
          application->abort();
        }
      else
        {
          committed = 0;
        }
    }
  noiseBank = bankManager->createBank("PdbParameterBank", 
				      bankID, descrip, 
				      Tstart, Tstop, calibname);
  start = Tstart;
  stop = Tstop;

  int length = noiseList.length() * noiseParameters; //
  noiseBank->setLength(length);

  PdbParameter *noiseValue;

  for (unsigned int i = 0; i < noiseList.length(); i++)
    {
      noiseValue = (PdbParameter*) & noiseBank->getEntry(i * noiseParameters);
      noiseValue->setParameter(noiseList[i]->getGlobalIndex());
      noiseValue->setName("Global Index");
      noiseValue = (PdbParameter*) & noiseBank->getEntry(i * noiseParameters + 1);
      noiseValue->setParameter(noiseList[i]->getCountsPerEvent());
      noiseValue->setName("Counts/Event");
      noiseValue = (PdbParameter*) & noiseBank->getEntry(i * noiseParameters + 2);
      noiseValue->setParameter(noiseList[i]->getStatus());
      noiseValue->setName("Status");
    }
  return True;
}


PHBoolean
PHDchNoiseObject::updateValidityTimeForLastBank(PHTimeStamp& Tstart, PHTimeStamp& Tstop,  PHTimeStamp& Tsearch, const char *calibname, PdbBankID bankID, int force )
{

 if (!fetch(Tsearch, calibname, bankID)) return False;

 if (stop == PHTimeStamp::PHFarFuture ||  force) {
   noiseBank->setEndValTime(Tstop);
   noiseBank->setStartValTime(Tstart);
   stop = Tstop;
 }else {
   PHMessage("PHDchNoise::updateValidityTimeForLastBank",PHWarning,"Tstop not  in Far Future");
 }
  return True;

}

PHBoolean
PHDchNoiseObject::fetch(PHTimeStamp &Tsearch, const char *calibname,PdbBankID bankID )
{
  if(committed == 1) {
    if(!application->startRead()) {
      PHMessage("PHNoiseObject",PHError, "Aborting ... Database not readable");
      application->abort();
    }else{
      committed =  0;
    }
  }
  noiseBank = bankManager->fetchBank("PdbParameterBank",bankID,calibname,Tsearch);

  if (!noiseBank) 
    {
      return False;
    }
  start = noiseBank->getStartValTime();
  stop  = noiseBank->getEndValTime();

  noiseList.clearAndDestroy();
  zeroArray();

  PdbParameter *noiseValue;

  int globalIndex;
  int length = noiseBank->getLength();
  numberOfNoisyChannels = length/noiseParameters;

  cout << "Start: " << start.getTics() << endl;
  cout << "Number of Dead: " << numberOfNoisyChannels << endl;
  for(int i=0; i < numberOfNoisyChannels; i++) {

    basicNoise = new DchBasicNoise();

    noiseValue =  (PdbParameter*)&noiseBank->getEntry(i*noiseParameters);
    globalIndex = (int) (noiseValue->getParameter() + 0.5); // security
    basicNoise->setGlobalIndex(globalIndex);

    noiseValue =  (PdbParameter*)&noiseBank->getEntry(i*noiseParameters+1);
    basicNoise->setCountsPerEvent(globalIndex);

    noiseValue =  (PdbParameter*)&noiseBank->getEntry(i*noiseParameters+2);
    basicNoise->setStatus(globalIndex);

    noiseList.append(basicNoise);
    noisyDeadArray[globalIndex] = 0;
  }
  delete noiseBank;
  noiseBank = 0;
  return True;

}

//___________________________________________________________________
PHBoolean PHDchNoiseObject::fetchNoiseFromFile()
{
  ifstream file;
  if (!noiseFile)
    {
      cout << "PHDchNoiseObject::fetchNoiseFromFile - running without dead map" << endl;
    }
  else
    {
      file.open(noiseFile);
      if (!file)
        {
          cout << "PHDchNoiseObject::fetchNoiseFromFile - could not open input dead map file " << noiseFile << " !!" << endl;
        }
      else
        {
          cout << "PHDchNoiseObject::fetchNoiseFromFile - Reading from file " << noiseFile << endl;


          float noiseLevel = 0.;
          int index;

          int previous = -1;
          noiseList.clearAndDestroy();
          zeroArray();
          index = -1;

          while (!file.eof())
            {
              file >> index;
              if (index == previous)
                {
                  cout << "PHDchNoiseObject::fetchNoiseFromFile - end of file reached" << endl;
                  break;
                }
              //if (flag==0 || flag==1 || flag==2)
              {  // bad channel
                basicNoise = new DchBasicNoise(index, noiseLevel, False);
                noisyDeadArray[index] = 0;
                noiseList.append(basicNoise);
                previous = index;
              }
            }
        }
    }
  if (! effiFile)
    {
      cout << "PHDchNoiseObject::fetchNoiseFromFile - running without efficiency file" << endl;
    }
  else
    {
      ifstream file2;
      file2.open(effiFile);
      if (!file2)
        {
          cout << "PHDchNoiseObject::fetchNoiseFromFile - could not open input file " << effiFile << " !!" << endl;
        }
      else
        {
          cout << "PHDchNoiseObject::fetchNoiseFromFile - Reading from file " << effiFile << endl;

          int total;
          float efficiency;
          int previous = -1;
          int index = -1;
          char parName1[20];
          char parName2[20];
          char parName3[20];
          while (!file2.eof())
            {
              file2 >> total >> parName1 >>  index >> parName2 >> efficiency >> parName3 ;
              if (index == previous)
                {
                  cout << "PHDchNoiseObject::fetchNoiseFromFile - End of file reached" << endl;
                  break;
                }

              efficiencyArray[index] = efficiency;
              previous = index;
            }
        }
    }
  return True;
}

PHBoolean
PHDchNoiseObject::writeNoiseInFile()
{
  int length = noiseList.length();
  for (int i = 0; i < length ; i++)
    {
      basicNoise = noiseList[i];
    }

  return True;
}
