// Declaration of class PdbSvxDeadMap
// Purpose: Lists SVX channels that are dead or hot (or possibly some other
//          non-functional status, but not at the time of this writing).
//          Normally functioning channels have no corresponding record -
//          to save space, only malfunctioning channels are listed.
// Author: Matthew Lockner (mlockner)

#ifndef __PDBSVXDEADMAP_HH__
#define __PDBSVXDEADMAP_HH__

#include <string>
using std::string;

#include "PdbCalChan.hh"

class PdbSvxDeadMap : public PdbCalChan
{
 public:
 
   enum { layer = 0, ladder = 1, sensor = 2, 
          sensorSection = 3, sensorReadout = 4, channel = 5, status = 6 };
          
   PdbSvxDeadMap();
   PdbSvxDeadMap (const short layer, 
   				  const short ladder, 
   				  const short sensor, 
   				  const short sensorSection, 
   				  const short sensorReadout, 
   				  const short channel,
   				  const short status);
   PdbSvxDeadMap (const PdbSvxDeadMap &);

   virtual ~PdbSvxDeadMap();
   
   short getParameter (const size_t index) const;
   const char* getParName (const size_t index) const;
   
   void setParameter (const size_t index, const short value);
   void setAllParameters (const short layer,
      				  const short ladder, 
	   				  const short sensor,
	   				  const short sensorSection, 
   					  const short sensorReadout, 
   					  const short channel,
   					  const short status);

	virtual void print() const;
	
 private:
   static const size_t nDim = 7;
 
   short iLayer;
   short iLadder;
   short iSensor;
   short iSensorSection;
   short iSensorReadout;
   short iChannel;
   short iStatus;
   
   ClassDef (PdbSvxDeadMap, 1);
};

#endif /* __PDBSVXDEADMAP_HH__ */

