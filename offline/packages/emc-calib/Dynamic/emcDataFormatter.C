#include <emcDataFormatter.h>
#include <EmcIndexer.h>
#include <emcRawDataAccessor.h>
#include <emcRawDataObject.h>
#include <Event.h>
#include <EmcDynamicData.h>

using namespace std;
// **********************************************************************

// #define HG_PRE_OUT       0x4     // high gain pre-sample value is OUT-OF-RANGE
// #define HG_POST_OUT      0x8     // high gain post-sample value is OUT-OF-RANGE

// #define LG_PRE_OUT       0x40    // low gain pre-sample value is OUT-OF-RANGE
// #define LG_POST_OUT      0x80    // low gain post-sample value is OUT-OF-RANGE

// #define TAC_OUT          0x400   // TAC value is OUT-OF-RANGE
// #define CHANNEL_DISABLED 0x2000  // 

// #define HG_MIN           1024
// #define HG_MAX           4095
// #define LG_MIN           0
// #define LG_MAX           4095
// #define TAC_MIN          0
// #define TAC_MAX          4095


bool emcDataFormatter::fillRDO(Event * ev){
  //  cout<<"emcDataFormatter::fillRDO "<<endl;
  //  ev->identify();
  emcRawDataAccessor * rda = emcRawDataAccessor::GetInstance();
  emcRawDataObject   * rdo = rda->GetRawDataObject() ;
  EmcDynamicData     * fDynamicData = rda->GetDynamicData() ;
  const SuperModule  * SMMap = fDynamicData->getSMMap();
  struct emcChannelLongList ecl[144];
  int    nw;
  int    NumberOfChannels;

  // loop over packets
  for(int iSM=0;iSM<fDynamicData->getnSM(); iSM++){
    empty(rdo, iSM);
    int packetId = SMMap[iSM].packet;
    Packet * p = ev->getPacket(packetId);
    //    if(!p) cout<<"Packet "<<packetId<<" not found"<<endl;
    if(p) {
      int ctac = p->iValue(0,"AMU");
      int cpre = p->iValue(1,"AMU");
      int cpost= p->iValue(2,"AMU");
      rdo->SetCells(iSM, ctac, cpre, cpost);
      NumberOfChannels = p->fillIntArray ( (int *) ecl, sizeof(emcChannelLongList)*144, &nw, "SPARSE");



//       if(packetId>8100&&packetId<8176) {
// 	cout<<packetId<<" Channels "<<NumberOfChannels<<" HitFormat "<<p->getHitFormat()<<" Words "<<nw<<endl;
// 	cout<<packetId<<" "<<ctac<<" "<<cpre<<" "<<cpost<<endl;
//      }


      //     if(NumberOfChannels==0) {
      // 	for(int i=0;i<144;i++){
      // 	  cout<<i<<" "<<p->iValue(i,0)<<" "<<p->iValue(i,1)<<" "<<p->iValue(i,2)<<" "<<p->iValue(i,3)<<" "<<p->iValue(i,4)<<endl;
      // 	}
      //       }
      int start = iSM*144;
      delete p;
      if((packetId<=8172||packetId>=8176) ||
	 // beginning Run5 we have new FPGA code and similar data structures in all FEM's including Monitoring
	 (packetId>8172&&packetId<8176&&NumberOfChannels>72)) {

	//  All normal calorimeter data packets
	for (int i=0; i< NumberOfChannels; i++){
	  int errCode = 0;
	  int tac     = 4095-ecl[i].time;
	  int hg_pre  = 4095-ecl[i].highpre;
	  int hg_post = 4095-ecl[i].highpost;
	  int lg_pre  = 4095-ecl[i].lowpre;
	  int lg_post = 4095-ecl[i].lowpost;
	  if(tac    <=TAC_MIN||tac    >=TAC_MAX) errCode |= TAC_OUT;
	  if(hg_pre <=HG_MIN ||hg_pre >=HG_MAX)  errCode |= HG_PRE_OUT;
	  if(hg_post<=HG_MIN ||hg_post>=HG_MAX)  errCode |= HG_POST_OUT;
	  if(lg_pre <=LG_MIN ||lg_pre >=LG_MAX)  errCode |= LG_PRE_OUT;
	  if(lg_post<=LG_MIN ||lg_post>=LG_MAX)  errCode |= LG_POST_OUT;
	  rdo->Set(start+ecl[i].channel, tac, hg_post, lg_post, hg_pre, lg_pre, errCode);
	  // cout<<"Set twr "<<start+i<<" "<<ecl[i].channel<<" TAC "<<tac<<" HG-POST "<<hg_post<<" LG-POST "<<lg_post <<" HG-PRE "<<hg_pre<<" LG-PRE "<<lg_pre<<" ERR-CODE "<<hex<<errCode<<dec<<endl;
	}
      } else {
	// PBSC monitoring FEM's
	for (int i=0; i< NumberOfChannels; i++){
	  int errCode = 0;
	  int tac     = 4095-ecl[i].time;
	  int hg_pre  = 4095-ecl[i].highpre;
	  int hg_post = 4095-ecl[i].highpost;
	  int lg_pre  = 4095-ecl[i].lowpre;
	  int lg_post = 4095-ecl[i].lowpost;
	  if(tac    <=TAC_MIN||tac    >=TAC_MAX) errCode |= TAC_OUT;
	  if(hg_pre <=HG_MIN ||hg_pre >=HG_MAX)  errCode |= HG_PRE_OUT;
	  if(hg_post<=HG_MIN ||hg_post>=HG_MAX)  errCode |= HG_POST_OUT;
	  if(lg_pre <=LG_MIN ||lg_pre >=LG_MAX)  errCode |= LG_PRE_OUT;
	  if(lg_post<=LG_MIN ||lg_post>=LG_MAX)  errCode |= LG_POST_OUT;
	  int ch      = ecl[i].channel;
	  int loc     = ch%24 + (ch/24)*48;
	  rdo->Set(start+loc, tac, hg_post, lg_post, hg_pre, lg_pre, errCode);	
	  //	  cout<<"Set ref "<<i<<" "<<ecl[i].channel<<" "<<loc<<" TAC "<<tac<<" HG-POST "<<hg_post<<" LG-POST "<<lg_post <<" HG-PRE "<<hg_pre<<" LG-PRE "<<lg_pre<<" ERR-CODE "<<hex<<errCode<<dec<<endl;
	}
      }

    }
  }
  return true;
}

void emcDataFormatter::empty(emcRawDataObject * rdo, const int iSM){
  int start = iSM*144;
  for (int i=0; i<144; i++){
    rdo->Set(start+i, 0,0,0,0,0,CHANNEL_DISABLED);
  }
  rdo->resetAMUAddresses(iSM);
}









