//---------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  
//  Implementation of class PdbSvxDeadMap
//
//  Author: Matthew Lockner (mlockner)
//---------------------------------------------------------------------

#include "PdbSvxDeadMap.hh"

#include <iostream>

PdbSvxDeadMap::PdbSvxDeadMap() 
{
	iLayer = 0;
	iLadder = 0;
	iSensor = 0;
	iSensorSection = 0;
	iSensorReadout = 0;
	iChannel = 0;
	iStatus = 0;
}

PdbSvxDeadMap::PdbSvxDeadMap (const PdbSvxDeadMap &rhs)
{
	iLayer = rhs.iLayer;
	iLadder = rhs.iLadder;
	iSensor = rhs.iSensor;
	iSensorSection = rhs.iSensorSection;
	iSensorReadout = rhs.iSensorReadout;
	iChannel = rhs.iChannel;
	iStatus = rhs.iStatus;
}

PdbSvxDeadMap::PdbSvxDeadMap (const short layer, 
			   				  const short ladder, 
   							  const short sensor, 
   							  const short sensorSection, 
   							  const short sensorReadout, 
   							  const short channel,
   							  const short status) 
{
	setAllParameters (layer, ladder, sensor, 
					  sensorSection, sensorReadout, channel, status);
}

PdbSvxDeadMap::~PdbSvxDeadMap()
{
}
   
short PdbSvxDeadMap::getParameter (const size_t index) const 
{
	switch (index) {
	case layer:
		return iLayer;
	case ladder:
		return iLadder;
	case sensor:
		return iSensor;
	case sensorSection:
		return iSensorSection;
	case sensorReadout:
		return iSensorReadout;
	case channel:
		return iChannel;
	case status:
		return iStatus;
	default:
		return 0;
	}
}

const char* PdbSvxDeadMap::getParName (const size_t index) const 
{
	switch (index) {
	case layer:
		return "layer";
	case ladder:
		return "ladder";
	case sensor:
		return "sensor";
	case sensorSection:
		return "sensorSection";
	case sensorReadout:
		return "sensorReadout";
	case channel:
		return "channel";
	case status:
		return "status";
	default:
		return NULL;
	}
}
   
void PdbSvxDeadMap::setParameter (const size_t index, const short value) 
{
	switch (index) {
	case layer:
		iLayer = value;
		break;
	case ladder:
		iLadder = value;
		break;
	case sensor:
		iSensor = value;
		break;
	case sensorSection:
		iSensorSection = value;
		break;
	case sensorReadout:
		iSensorReadout = value;
		break;
	case channel:
		iChannel = value;
		break;
	case status:
		iStatus = value;
		break;
	default:
		std::cout << "PdbSvxDeadMap::setParameter - Index value = "
				  << index << " is out of range. [0.."
				  << nDim - 1 << "] is valid." << std::endl;
	}
}

void PdbSvxDeadMap::setAllParameters (const short layer,
      								  const short ladder, 
					   				  const short sensor,
	   								  const short sensorSection, 
   									  const short sensorReadout, 
   									  const short channel,
   									  const short status) 
{
	iLayer = layer;
	iLadder = ladder;
	iSensor = sensor;
	iSensorSection = sensorSection;
	iSensorReadout = sensorReadout;
	iChannel = channel;
	iStatus = status;
}

void PdbSvxDeadMap::print() const 
{
	std::cout << "Dead/Hot Channel Map Entry. "
			  << "Layer " << iLayer << ", "
			  << "ladder " << iLadder << ", "
			  << "sensor " << iSensor << ", "
			  << "sensor section " << iSensorSection << ", "
			  << "sensor readout " << iSensorReadout << ", "
			  << "channel " << iChannel << ", "
			  << "with status of "
			  << (iStatus == 0 ? "DEAD" : iStatus == 2 ? "HOT" : "NORMAL")
			  << "." 
			  << std::endl;
}

