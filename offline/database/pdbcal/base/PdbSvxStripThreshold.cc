#include "PdbSvxStripThreshold.hh"

#include <iostream>

PdbSvxStripThreshold::PdbSvxStripThreshold() 
{
	iLayer = 0;
	iLadder = 0;
	iSensor = 0;
	iSensorSection = 0;
	iSensorReadout = 0;
	iChannel = 0;
	iThreshold = 0;
}

PdbSvxStripThreshold::PdbSvxStripThreshold (const PdbSvxStripThreshold &rhs)
{
	iLayer = rhs.iLayer;
	iLadder = rhs.iLadder;
	iSensor = rhs.iSensor;
	iSensorSection = rhs.iSensorSection;
	iSensorReadout = rhs.iSensorReadout;
	iChannel = rhs.iChannel;
	iThreshold = rhs.iThreshold;
}

PdbSvxStripThreshold::PdbSvxStripThreshold (const short layer, 
			   		    const short ladder, 
   					    const short sensor, 
   					    const short sensorSection, 
   					    const short sensorReadout, 
   					    const short channel,
   					    const int threshold) 
{
	setAll (layer, ladder, sensor, sensorSection, sensorReadout, channel, threshold);
}

PdbSvxStripThreshold::~PdbSvxStripThreshold()
{
}
   
void PdbSvxStripThreshold::setAll (const short layer,
      			    const short ladder, 
			    const short sensor,
	   		    const short sensorSection, 
   			    const short sensorReadout, 
   			    const short channel,
   			    const int threshold) 
{
	iLayer = layer;
	iLadder = ladder;
	iSensor = sensor;
	iSensorSection = sensorSection;
	iSensorReadout = sensorReadout;
	iChannel = channel;
	iThreshold = threshold;
}

void PdbSvxStripThreshold::print() const
{
        std::cout << "Strip threshold = " << iThreshold 
                          << " for layer " << iLayer << ", "
                          << "ladder " << iLadder << ", "
                          << "sensor " << iSensor << ", "
                          << "sensor section " << iSensorSection << ", "
                          << "sensor readout " << iSensorReadout << ", "
                          << "channel " << iChannel 
                          << std::endl;
}

