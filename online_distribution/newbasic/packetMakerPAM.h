/* 
** packetMakerPAM.h
** 
** Author: $Author: purschke $  
**   Date: $Date: 2000/07/21 01:51:17 $ 
** 
** $Log: packetMakerPAM.h,v $
** Revision 1.1.1.1  2000/07/21 01:51:17  purschke
** mlp -- adding the new automakified "basic" module to CVS.
**
**
** Revision 1.4  1998/12/11 22:01:47  markacs
** (stephen markacs) adding log into cvs tags
** 
*/
//
//  packetMakerPAM.h 
//
//  creates a properly formatted frame containing packets
//  encapsulating the data contained in STAF tables
//  
//  requirements: 
//		1) Data for each packet must be contained in a contiguous block
//		

#include "phenixOnline.h"

#include "Cframe.h"
#include "Cpacket.h"
#include "CframeV1.h"
#include "CpacketV1.h"
#include "frameRoutines.h"
#include "packetRoutines.h"
#include "frameHdr.h"
#include "packetHdr.h"
#include "errorBlock.h"
#include "dataBlock.h"
#include <stdio.h>

///
class PacketDesc 
{
public:	
///
	DWORD *Location;
///
	DWORD  Length;
///
	DWORD  Format;
///
	DWORD  Id;
};

///
class PacketMakerTableList
{
private:
///
	int maxNumberOfTables;
///
	int numberOfTables;
///
	int totalLength;
///
	PacketDesc* packetList;
public:
///
	PacketMakerTableList(int);
///
	~PacketMakerTableList();
///
	int getMaxNumberOfTables(){return maxNumberOfTables;}
///
	int getNumberOfTables(){return numberOfTables;}
///
	int getTotalLength(){return totalLength;}
///
	addTableToList(DWORD* pointer, DWORD length, DWORD format, DWORD packetId);
///
	void dump();

///
	DWORD* getPacketLocation(int n){return packetList[n].Location;}
///
	DWORD getPacketLength(int n){return packetList[n].Length;}
///
	DWORD getPacketFormat(int n){return packetList[n].Format;}
///
	DWORD getPacketId(int n){return packetList[n].Id;}

};

///
class PacketMaker
{
private:
///
	DWORD* frameBuffer;
///
	DWORD  frameLength;
///
	PacketMakerTableList* packetMakerTableList;
public:
///
	PacketMaker(int,int,DWORD,PacketMakerTableList*);
/**	PacketMaker(int frameId, 
				int eventNumber,
				DWORD partitionVector,
				PacketMakerTableList* pmtl)	;
				*/
	~PacketMaker();
///
	DWORD* getFrameLocation(){return frameBuffer;}
///
	DWORD getTotalFrameLength(){return frameLength;}
///
	DWORD getNumberOfPackets(){return packetMakerTableList->getNumberOfTables();}
};

///
class frameHdrFailed
{
public:
///
	frameHdrFailed();
};


