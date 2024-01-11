#include "mErtPackModule.h"
#include "dErtDcmDataWrapper.h"
#include "dErtFemDataWrapper.h"
#include <PHIODataNode.h>
#include <packetConstants.h>

#include <cstdlib>
#include <iostream>
using namespace std;

typedef PHIODataNode<dErtFemDataWrapper> dErtFemDataNode_t;
typedef PHIODataNode<dErtDcmDataWrapper> dErtDcmDataNode_t;

//.....................................
PHBoolean mErtPackModule::event(PHCompositeNode* topNode)
{//.. 

   PHNodeIterator it(topNode);

   //.. get dErtFemData table ..
   //
   dErtFemDataWrapper* dErtFemData = 0;

   dErtFemDataNode_t *dertfemdatanode = static_cast<dErtFemDataNode_t *>(it.findFirst("PHIODataNode","dErtFemData"));
   if(dertfemdatanode) {
      dErtFemData = dertfemdatanode->getData();
   } else {
      cout<<" mErtPackModule: could not find table dErtFemData"<<endl;
    // scan build fix: we crash in the next line when this is a null pointer
    // so we might as well exit to stop getting mails about this
      exit(1);
   }

   //.. get dErtDcmData table ..
   //
   dErtDcmDataWrapper* dErtDcmData = 0;

   dErtDcmDataNode_t *dertdcmdatanode = static_cast<dErtDcmDataNode_t *>(it.findFirst("PHIODataNode","dErtDcmData"));
   if(dertdcmdatanode) {
      dErtDcmData = dertdcmdatanode->getData();
   } else {
      cout<<" mErtPackModule: could not find table dErtDcmData"<<endl;
    // scan build fix: we crash in the next line when this is a null pointer
    // so we might as well exit to stop getting mails about this
      exit(1);
   }

   for(int i = 0; i<nPacket; i++) {
     //..  all header and trailer value, assume 0  ...
     wordValue[i][0]=(1<<31); 
     wordValue[i][1]=(1<<31) | (1<< 20);
     wordValue[i][2]=(1<<31) | (2<< 20);
     wordValue[i][3]=(1<<31) | (3<< 20);
     wordValue[i][4]=(1<<31) | (4<< 20); 
     wordValue[i][125]=(1<<31) | (1<<24);
     wordValue[i][126]=(1<<31) | (1<<24) | (1<<20);
     wordValue[i][127]=(1<<31) | (1<<24) | (2<<20);
     wordValue[i][128]=(1<<31) | (1<<24) | (3<<20);
     wordValue[i][129]=(1<<31) | (1<<24) | (4<<20);
     wordValue[i][130]=(1<<31) | (1<<24) | (5<<20);
     wordValue[i][131]=(1<<31) | (1<<24) | (6<<20);
     wordValue[i][132]=(1<<31) | (1<<24) | (7<<20);
     wordValue[i][133]= (1<<31) | (1<<24) | (8<<20);
     wordValue[i][134]= 0xfff00000;
   }

   //...................... 
   int nwordArm0 = 0;
   int nwordArm1 = 0;
   for(unsigned int i= 0; i<dErtFemData->RowCount(); i++) {
     if(!dErtFemData->get_crate(i))  //..west arm 
         wordValue[0][5+nwordArm0++] = dErtFemData->get_Value(i) | 
				       (dErtFemData->get_Roc(i)<<24)|
				       (dErtFemData->get_word(i)<<20);
     else 
	 wordValue[1][5+nwordArm1++] = dErtFemData->get_Value(i) | 
                                       (dErtFemData->get_Roc(i)<<24)|
                                       (dErtFemData->get_word(i)<<20);

   }
   if(nwordArm0>120) cout<<" Error: roc*word should be less then 120 "<<endl; 

   for(int i = 0; i<nPacket; i++) {
     dErtDcmData->set_Nwords(i, nWordPerPacket); 
     dErtDcmData->set_packetID(i, packetID[i]);
     dErtDcmData->set_hitformat(i, IDEMCRICH_LL1);
     for(int j = 0; j<nWordPerPacket; j++) {
        dErtDcmData->set_word(j, i, wordValue[i][j]);
     }
   }
   dErtDcmData->SetRowCount(nPacket);

   return True;
}
