#ifndef __PDBSVXSTRIPTHRESHOLD_HH__
#define __PDBSVXSTRIPTHRESHOLD_HH__

#include <string>
using std::string;

#include "PdbCalChan.hh"

class PdbSvxStripThreshold : public PdbCalChan
{
 public:
 
   PdbSvxStripThreshold();
   PdbSvxStripThreshold (const short layer, 
   		         const short ladder, 
   			 const short sensor, 
   			 const short sensorSection, 
   			 const short sensorReadout, 
   			 const short channel,
   			 const int thershold);
   PdbSvxStripThreshold (const PdbSvxStripThreshold &);

   virtual ~PdbSvxStripThreshold();
   
   void setAll (const short layer,
      	        const short ladder, 
	   	const short sensor,
	   	const short sensorSection, 
   		const short sensorReadout, 
   		const short channel,
   		const int threshold);

   int getThreshold()     {return iThreshold;}
   int getLayer()         {return iLayer;}
   int getLadder()        {return iLadder;}
   int getSensor()        {return iSensor;}
   int getSensorSection() {return iSensorSection;}
   int getSensorReadout() {return iSensorReadout;}
   int getChannel()       {return iChannel;}

        virtual void print() const;

 private:

   short iLayer;
   short iLadder;
   short iSensor;
   short iSensorSection;
   short iSensorReadout;
   short iChannel;
   int   iThreshold;
   
   ClassDef (PdbSvxStripThreshold, 1);
};

#endif

