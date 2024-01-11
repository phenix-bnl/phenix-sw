#include <strstream>
#include <fstream>
#include <iostream>

#include "MutGeomEnumerations.h"
#include "MutArm.h"
#include "MutStation.h"
#include "MutOctant.h"
#include "MutHalfOctant.h"
#include "MutGap.h"
#include "MutPlane.h"
#include "MutStrip.h"
#include "MutDCMChannelMap.h"

#include "SwapDCMChannels.h"


void SwapDCMChannels(char *file) 
{
  /*  File should contain two lines for each set of swapped cables.
      Cases in which 3 cables have been swapped can be handled by 
      first swapping one to its correct position, and then swapping 
      the remaining two to their correct positions.  If future debugging
      uncovers a more pathelogical situation, we can rewrite this function.

      Each pair of lines should contain the identification for the range of 
      strips to be swapped: stripBegin1 to stripEnd1 swaps with stripBegin2 
      to stripEnd2.  These numbers may be in ascending or descending order.
  */
  static const int bufsize = 256;
  char linebuf[bufsize];

  ifstream s(file);
  if (!s.good()){ 
    cout<<__FILE__<<__LINE__<<"Error opening file "<<file<<". \n";
    s.close();
    return;
  }

  PHTimeStamp Tsearch(2002,5,1,0,0,0,0);

  //MutArm *pArm =  new MutArm(South, Tsearch);
  //MutArm *pArm =  new MutArm(North, Tsearch);
  MutArm *pArm =  new MutArm(North);
  pArm->print();

  int arm1, sta1, oct1, hoct1, gap1, pla1, stripBegin1, stripEnd1;
  int arm2, sta2, oct2, hoct2, gap2, pla2, stripBegin2, stripEnd2;

  while(s.getline(linebuf,bufsize,'\n'))
  {
    istrstream stringbuf(linebuf,strlen(linebuf));
    stringbuf >> arm1 >> sta1 >> oct1 >> hoct1 >> gap1 >> pla1 >> stripBegin1 >> stripEnd1;
    s.getline(linebuf,bufsize,'\n');
    istrstream stringbuf2(linebuf,strlen(linebuf));
    stringbuf2 >> arm2 >> sta2 >> oct2 >> hoct2 >> gap2 >> pla2 >> stripBegin2 >> stripEnd2;

    //Does this make sense?
    if(pArm->getArm()!=arm1|| arm1!=arm2 || sta1!=sta2){
      cout<<__FILE__<<__LINE__<<"Error in file "<<file<<". \n";
      cout<<"File indicates that mutr cables from different arms or \n";
      cout<<"stations must be swapped!\n";
      continue;
    }

    int swapCount = abs(stripBegin1 - stripEnd1);
    if(swapCount!= abs(stripBegin2 - stripEnd2)){
      cout<<__FILE__<<__LINE__<<"  Error in file "<<file<<". \n";
      cout<<"File indicates that mutr cables of differing size must be swapped!\n";
      continue;
    }

    int sign1 = (stripEnd1 - stripBegin1)/swapCount;
    int sign2 = (stripEnd2 - stripBegin2)/abs(stripEnd2 - stripBegin2);

    MutPlane *pPla1 = pArm->f_pMutStations[sta1]->f_pMutOctants[oct1]->
      f_pMutHalfOctants[hoct1]->f_pMutGaps[gap1]->f_pMutPlanes[pla1];
    MutPlane *pPla2 = pArm->f_pMutStations[sta2]->f_pMutOctants[oct2]->
      f_pMutHalfOctants[hoct2]->f_pMutGaps[gap2]->f_pMutPlanes[pla2];

    cout<<"Swapping station"<<sta1<<" oct "<<oct1<<" half "<<hoct1<<" gap "<<gap1<<" "<<pPla1->name<<" strips "<<stripBegin1<<" to "<<stripEnd1<<endl;
    cout<<" with oct "<<oct2<<" half "<<hoct2<<" gap "<<gap2<<" "<<pPla2->name<<" strips "<<stripBegin2<<" to "<<stripEnd2<<endl;

    int tempPacketID1 =0;
    int tempDCMChannel1 =0;
    int tempPacketID2 =0;
    int tempDCMChannel2 =0;
    for(int i=0; i<swapCount+1; i++) {
      if(pPla1->f_pMutStrips[stripBegin1+i*sign1]&&
	 pPla2->f_pMutStrips[stripBegin2+i*sign2]) {
	tempPacketID1 = pPla1->f_pMutStrips[stripBegin1+i*sign1]->getPacket_ID();
	tempDCMChannel1 = pPla1->f_pMutStrips[stripBegin1+i*sign1]->getDCMChannel();
	tempPacketID2 = pPla2->f_pMutStrips[stripBegin2+i*sign2]->getPacket_ID();
	tempDCMChannel2 = pPla2->f_pMutStrips[stripBegin2+i*sign2]->getDCMChannel();
	pPla1->f_pMutStrips[stripBegin1+i*sign1]->setPacket_ID(tempPacketID2);
	pPla1->f_pMutStrips[stripBegin1+i*sign1]->setDCMChannel(tempDCMChannel2);
	pPla2->f_pMutStrips[stripBegin2+i*sign2]->setPacket_ID(tempPacketID1);
	pPla2->f_pMutStrips[stripBegin2+i*sign2]->setDCMChannel(tempDCMChannel1);
      }
      else { 
	cout<<pPla1->name<<" Strip "<< stripBegin1+i*sign1<<" or ";
	cout<<pPla2->name<<" Strip "<< stripBegin2+i*sign2<<" doesn't exist"<<endl;
      }
    }
  }
  s.close();

  ofstream outfile("North_channelsnew.out");
  if (!outfile){ 
    cout<<"Error creating file. \n";
    outfile.close();
    return;
  }
  MutPlane *pPlane;
  MutStrip *pStrip;
  int pkt, channel;

  for(int sta = Station1; sta<NumberOfStations; sta++){
    int NumberOfGaps=pArm->f_pMutStations[sta]->getNumberOfGaps();
    for(int oct=0; oct<NumberOfOctants; oct++) {
      for(int halfo=0; halfo<NumberOfHalfOctants; halfo++) {
	for(int gap=0; gap<NumberOfGaps; gap++) {
	  for(int pla=0; pla<NumberOfPlanes; pla=pla+2) {//only strip planes
	    pPlane=pArm->f_pMutStations[sta]->f_pMutOctants[oct]->
	      f_pMutHalfOctants[halfo]->f_pMutGaps[gap]->f_pMutPlanes[pla];
	    outfile<<"Station "<<sta<<", Octant "<<oct<<", HO"<<halfo<<", gap"<<gap<<
	      ", Cathode"<<pla<<endl;
	    for(int strp=0; strp<pPlane->getNumElements(); strp++) {
     	      pStrip = pPlane->f_pMutStrips[strp];
	      if(pStrip){
		pkt = pStrip->getPacket_ID();
		channel = pStrip->getDCMChannel();
		outfile<<strp<<" "<<pkt<<" "<<channel<<endl;
	      }
	    }
	  }
	}
      }
    }
  } 
  outfile.close();

  //PHTimeStamp Tstart(2001,5,1,0,0,0,0);
  PHTimeStamp Tstart(2002,5,1,0,0,0,0);
  PHTimeStamp Tstop(2002,2,1,0,0,0,0);
  Tstop.setToFarFuture();

  //This is the call that writes things back out to the database.
  //Uncomment this line only when your sure things are swapping correctly.
  //pArm->f_pMutStations[sta1]->updateDCMChannelMap(Tstart, Tstop);

}
