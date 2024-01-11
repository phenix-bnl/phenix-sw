#include "mTecPackModule.h"
#include "TecHit.hh"
#include "TecGHitList.hh"
#include "TecPacketList.hh"

#include "dTecFemDataWrapper.h"
#include "dTecGhitRawWrapper.h"
#include "dTecDcmDataWrapper.h"
#include "dTecGhitDcmWrapper.h"

#include "packetConstants.h"

#include "PHNodeIterator.h"
#include "PHIODataNode.h"
#include "PHTypedNodeIterator.h"

#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHIODataNode<dTecFemDataWrapper> dTecFemDataNode_t;
typedef PHIODataNode<dTecGhitRawWrapper> dTecGhitRawNode_t;
typedef PHIODataNode<dTecDcmDataWrapper> dTecDcmDataNode_t;
typedef PHIODataNode<dTecGhitDcmWrapper> dTecGhitDcmNode_t;
typedef PHIODataNode<TecGHitList> TecGHitListNode_t;
typedef PHIODataNode<TecPacketList> TecPacketListNode_t;

//----------------------------------------------------------

mTecPackModule::mTecPackModule()
{
  Verbose=0;
  Suppress=IDTEC_DCM1;	// (505) zero suppressed
  UseListSum=0;
  FillAllFEM=1;
  FillTecGhitDcm=0;
  FillTecPackets=0;
  Write2File=0;
}

mTecPackModule::~mTecPackModule(){}

PHBoolean mTecPackModule::event(PHCompositeNode* root) 
{
  cerr << "mTecPackModule ERROR: event(PHCompositeNode*) method" << endl <<
          "    is not implemented." << endl <<
          "    Use event(PHCompositeNode*, TecAddressObject* )" << endl <<
          "    method instead." << endl;
  return False;
}

PHBoolean 
mTecPackModule::event(PHCompositeNode* root,
		      TecAddressObject* TAO) 
{
  
  if(Verbose>0) cout << "mTecPackModule: Started..." << endl;
  
  //------ Get pointers to TecGHits and TecPackets -----------------------

  PHNodeIterator iter(root);
  PHCompositeNode* tecNode;

  PHTypedNodeIterator<TecGHitList> ghiter(root);
  TecGHitListNode_t *TecGHitListNode = ghiter.find("TecGHitList");
  TecGHitList* tecghitlist;

  if(TecGHitListNode) {
    tecghitlist = (TecGHitList*)TecGHitListNode->getData();
    if(Verbose>0)
      cout << "mTecPackModule: Found TecGHitList." << endl;
  }
  else {           // TecGHitList does not exist. Add it to node tree.
    if(Verbose>0)
      cout << "mTecPackModule: Can not find TecGHitList. "
           << "Adding TecGHitList to the node tree... (tecNode)" << endl;

    tecNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "TEC"));
    if (!tecNode) {
      cerr << "mTecPackModule ERROR: TEC node does not exist." << endl;
      return False;
    }
    else {
      if(Verbose>0) cout << "mTecPackModule: tecNode FOUND." << endl;
    }

// Add TecGHitList to tecNode
      tecghitlist = new TecGHitList();
      PHIODataNode<PHObject>* TecGHitListNodeNew = new
                PHIODataNode<PHObject>(tecghitlist,"TecGHitList","PHObject");
      tecNode->addNode(TecGHitListNodeNew);
      if(Verbose>0)
             cout << "mTecPackModule: TecGHitList added to tecNode." << endl;
  }


  PHTypedNodeIterator<TecPacketList> piter(root);
  TecPacketListNode_t *TecPacketListNode = piter.find("TecPacketList");
  TecPacketList* tecpacketlist;

  if(TecPacketListNode) {
    tecpacketlist = (TecPacketList*)TecPacketListNode->getData();
    if(Verbose>0)
      cout << "mTecPackModule: Found TecPacketList." << endl;
  }
  else {           // TecPacketList does not exist. Add it to node tree.
    if(Verbose>0)
      cout << "mTecPackModule: Can not find TecPacketList. "
           << "Adding TecPacketList to the node tree... (tecNode)" << endl;

    tecNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "TEC"));
    if (!tecNode) {
      cerr << "mTecPackModule ERROR: TEC node does not exist." << endl;
      return False;
    }
    else {
      if(Verbose>0) cout << "mTecPackModule: tecNode FOUND." << endl;
    }

// Add TecPacketList to tecNode
      tecpacketlist = new TecPacketList();
      PHIODataNode<PHObject>* TecPacketListNodeNew = new
                PHIODataNode<PHObject>(tecpacketlist,"TecPacketList","PHObject");
      tecNode->addNode(TecPacketListNodeNew);
      if(Verbose>0)
             cout << "mTecPackModule: TecPacketList added to tecNode." << endl;
  }

//------ Get pointers to the Tables ------------------------------------

  PHTypedNodeIterator<dTecFemDataWrapper> iFDN(root);
  PHTypedNodeIterator<dTecGhitRawWrapper> iGRN(root);
  PHTypedNodeIterator<dTecDcmDataWrapper> iDDN(root);
  PHTypedNodeIterator<dTecGhitDcmWrapper> iGDN(root);

  dTecFemDataNode_t *FDN = iFDN.find("dTecFemData");
  dTecGhitRawNode_t *GRN = iGRN.find("dTecGhitRaw");
  dTecDcmDataNode_t *DDN = iDDN.find("dTecDcmData");
  dTecGhitDcmNode_t *GDN = iGDN.find("dTecGhitDcm");
  if(!FDN) { 
    cerr << "mTecPackModule -> ERROR: dTecFemData table not found." << endl;
    return False;
  }
  if(!GRN) { 
    cerr << "mTecPackModule -> ERROR: dTecGhitRaw table not found." << endl;
    return False;
  }
  if(!DDN) { 
    cerr << "mTecPackModule -> ERROR: dTecDcmData table not found." << endl;
    return False;
  }
  if(!GDN) { 
    cerr << "mTecPackModule -> ERROR: dTecGhitDcm table not found." << endl;
    return False;
  }

  dTecFemDataWrapper* dTecFemData = FDN->getData();
  dTecGhitRawWrapper* dTecGhitRaw = GRN->getData();
  dTecDcmDataWrapper* dTecDcmData = DDN->getData();
  dTecGhitDcmWrapper* dTecGhitDcm = GDN->getData();

  int Nwords,icrate,ichan,islot,word0,sum,tmpsum=0,i,j;
  int ListSum[TECDCMLISTSUMSIZE],psadd[2],ipsadd,iarm=0,isect=0,iside=0,iwire;
  int pid,nok1,nok2,endloop;
  unsigned long tmp1,tmp2,tmp3,tmpword;
  unsigned long DCM[TECDCMSIZE],tmp,data[TECMAXTIMEBIN],sum2;
  static int CallNumber = 0;
  FILE *out_file=0;

// Check if debugging and zero-suppressing were requested 

  nok1=0; nok2=0;

  if(Verbose > 0) {
    cout << "mTecPack -> Started...      " << CallNumber << endl;
    cout << "mTecPack -> Number of entries in dTecFemData table = " 
         <<  dTecFemData->RowCount() << endl;
    cout << "mTecPack -> Number of entries in dTecGhitRaw table = "
	 << dTecGhitRaw->RowCount() << endl;
    cout << "mTecPack -> Number of TecGhits = "
	 << tecghitlist->getNHits() << endl;
    cout << "mTecPack -> IDTEC_DCM0 = "
	 << IDTEC_DCM0 << " " << IDTEC_DCM1 << " " << IDTEC_DCM2
	 << " " << IDDCM0OFFSET
	 << endl;
    cout << "mTecPack -> Suppress = " << Suppress << endl;
  }

  if(dTecGhitRaw->RowCount() == 0 && FillTecGhitDcm != 0) {
    printf("mTecPack -> ERROR: Filling of dTecGhitDcm was requested,\n");
    printf("    but dTecGhitRaw was not filled.\n");
    printf("    Make sure that mTecSlowSimModule->set_FillTecGhitRaw(1)\n");
    printf("    is set to 1.\n");
    return False;
  }

// open file for writing debug output 
  if(Write2File > 0) out_file = fopen("mtecpack.dat","w");

// Loop over all crates and slots 

  pid=1;
  for(icrate=0; icrate<TECMAXCRATE; icrate++)
  {
    for(islot=0; islot<TECMAXSLOT; islot++)
    {

// Prepare arrays 

      for(i=0; i<TECDCMSIZE; i++) DCM[i]=0;
      for(i=0; i<TECDCMLISTSUMSIZE; i++) ListSum[i]=0;
      sum=0;

// Loop over dTecFemData entries and fill DCM array 

      psadd[0]=15; psadd[1]=15;  // Initialize psadd with non-existing
                                 // addresses. psadd[i] will be equal to 15
                                 // if there is only one PreampShaper per
                                 // slot, or if wires corresponding to this
                                 // PS did not fire.

      for(i=0; i<(int)dTecFemData->RowCount(); i++)
      {
        if((icrate==(int)dTecFemData->get_crate(i)) && (islot==(int)dTecFemData->get_slot(i)))
        {
          ichan = dTecFemData->get_ichan(i);
          if(ichan<32)
          {
            psadd[0] = dTecFemData->get_psadd(i);
          }
          else
          {
            psadd[1] = dTecFemData->get_psadd(i);
          }
          for(j=0; j<TECMAXTIMEBIN; j++) sum += dTecFemData->get_amplitude(j,i);
          for(j=0; j<TECMAXTIMEBIN; j++) data[j] = dTecFemData->get_amplitude(j,i);
          for(j=0; j<TECMAXTIMEBIN; j++) ListSum[ichan] += 
                         (dTecFemData->get_amplitude(j,i) & 0x00000007);

// Find out iarm, isect, iside and iwire for this icrate, islot, ichan 

          ipsadd = dTecFemData->get_psadd(i);
          TAO->setHard(icrate,islot,ipsadd,ichan);
          iarm=TAO->getArm();
          isect=TAO->getSector();
          iside=TAO->getSide();
          iwire=TAO->getWire();

          word0 = TECDCMHEADERSIZE + ichan*TECDCMFEMSIZE;
          for(j=0; j<TECDCMFEMSIZE; j++)
          {
            tmp1 = data[j*4+0] +
                   data[j*4+1]*32 +
                   data[j*4+2]*32*32 +
                   data[j*4+3]*32*32*32;
            tmp2 = j*1024*1024;
            tmp3 = ichan*1024*1024*32;
            DCM[word0+j] = tmp1+tmp2+tmp3;
          } 
          if(Verbose>10) printf(" %d %d %d %d %d \n",
                         i,icrate,islot,ichan,tmpsum);
        } // icrate & islot if 
      } // i - end loop over dTecFemData entries 

// new DetectorID word. See http://www.inst.bnl.gov/~jack/MANUAL/page10.html 

      DCM[0] = 0x00004000 +
               0x80000000;

      DCM[1] = CallNumber +     // Event Number 
               0x80100000;

      DCM[2] = 0 +		// DetectorID
               islot +
               icrate*32 +
               psadd[0]*1024 +
               psadd[1]*16384 +
               0x00200000 +
               0x80000000;

      DCM[3] = Suppress +       // Flag Word = DCM formatting scheme 
               0x00300000 +
               0x80000000;

      DCM[4] = 0 +              // FEM Beam Clock Counter (8 bits) 
               0x00400000 +
               0x80000000;

      DCM[TECDCMHEADERSIZE+TECDCMDATASIZE] = 0x82000000 +   // User Word 
                  iarm +
                  isect*2 +
                  iside*8 +
                  icrate*16 +
                  islot*512 +
                  psadd[0]*1024*16 +
                  psadd[1]*1024*256;    // hardware address - lowest 22 bits  

// Calculate parity 
      sum2 = 0;

      DCM[TECDCMHEADERSIZE+TECDCMDATASIZE+1] = sum2 +                // Parity 
                  0x02100000 +
                  0x80000000;

      DCM[TECDCMHEADERSIZE+TECDCMDATASIZE+2] = 0xFFFFFFFF;           // Last Word - all bits on 

      if(UseListSum!=0) {	// List Summary - 64 words
        for(i=0; i<TECDCMLISTSUMSIZE; i++) { 
          DCM[i+TECDCMHEADERSIZE+TECDCMDATASIZE+TECDCMTRAILERSIZE] = 
               ListSum[i] +        
               0x80000000 +
               0x04000000 +
               i*1024*1024;
        }
      }

// Prepare the table
      for(i=0; i<TECDCMSIZE; i++) dTecDcmData->set_DCM(i,nok1,0);

// Write out all packets (default)

      TecPacket* tmppacket = new TecPacket();

      if((sum>0) || (FillAllFEM==1)) {

//*** Fill dTecDcmData table ***

        dTecDcmData->SetRowCount(nok1+1);
        dTecDcmData->set_scheme(nok1,Suppress);
        tmppacket->setScheme(Suppress);
        dTecDcmData->set_packetID(nok1,(5000 + pid));
        tmppacket->setPacketID(5000 + pid);
        tmpword = 0x05000000 +
                  iarm*4 +
                  isect*2*4 +
                  iside*8*4 +
                  icrate*16*4 +
                  islot*512*4 +
                  psadd[0]*1024*16*4 +
                  psadd[1]*1024*256*4;

        if(Write2File>0) fprintf(out_file," %2d %2d %2d %2d %2d      0x%lx     %5d\n", iarm,isect,iside,icrate,islot,tmpword, pid+5000);

// If zero suppression was requested fill only non-zero elements of DCM 

        if(Suppress==IDTEC_DCM0)  // pass-through mode 
        {
          Nwords = 0;
	  if(UseListSum!=0) { endloop = TECDCMSIZE; }
            else { endloop = TECDCMHEADERSIZE+TECDCMDATASIZE+TECDCMTRAILERSIZE; }
          for(i=0; i<endloop; i++) {
            dTecDcmData->set_DCM(i,nok1,DCM[i]);
            tmppacket->setFEM(i,DCM[i]);
            Nwords++;
          }
          dTecDcmData->set_Nwords(nok1,Nwords);
          tmppacket->setNWords(Nwords);
        }
        else              // zero-suppression mode 
        {
          if(Suppress==IDTEC_DCM1)
          {
            Nwords = 0;

// fill dTecDcmData header 
            for(i=0; i<TECDCMHEADERSIZE; i++) { 
              dTecDcmData->set_DCM(Nwords,nok1,DCM[i]);
              tmppacket->setFEM(Nwords,DCM[i]);
              Nwords++;
            }

// check data and fill dTecDcmData only if it is not 0 
            for(i=TECDCMHEADERSIZE; i<(TECDCMHEADERSIZE+TECDCMDATASIZE); i++) {
              tmp = (DCM[i] & 0x000FFFFF);
              if(tmp!=0) {
                dTecDcmData->set_DCM(Nwords,nok1,DCM[i]);
                tmppacket->setFEM(Nwords,DCM[i]);
                Nwords++;
              }
            }  

// fill trailer 
            for(i=(TECDCMHEADERSIZE+TECDCMDATASIZE); 
                i<(TECDCMHEADERSIZE+TECDCMDATASIZE+TECDCMTRAILERSIZE); i++) {
              dTecDcmData->set_DCM(Nwords,nok1,DCM[i]);
              tmppacket->setFEM(Nwords,DCM[i]);
              Nwords++;
            }

// fill list summary if requested
            if(UseListSum!=0) {
              for(i=(TECDCMHEADERSIZE+TECDCMDATASIZE+TECDCMTRAILERSIZE); 
                  i<TECDCMSIZE; i++) {
                     dTecDcmData->set_DCM(Nwords,nok1,DCM[i]);
                     tmppacket->setFEM(Nwords,DCM[i]);
                     Nwords++;
              }
            }

              dTecDcmData->set_Nwords(nok1,Nwords);
              tmppacket->setNWords(Nwords);

          }
          else
          {
            printf("mTecPack ERROR: Wrong DCM Formatting Scheme: %d\n",Suppress);
            printf("  Only %d and %d are supported.\n",IDTEC_DCM0,IDTEC_DCM1);
            return False;
          }
        } // Suppress 

        nok1++;

        if(FillTecPackets!=0) tecpacketlist->AddTecPacket(tmppacket);

        if(Verbose>1) cout << nok1 << " " << "Nwords = " << Nwords << endl;

//*** end filling dTecDcmData ***

      } // FillAllFEM if 

      delete tmppacket;

      pid++;

    } // islot 
  } // icrate 

// Fill relational table dTecGhitDcm if requested 

  if(FillTecGhitDcm != 0) // check if filling was requested 
  {
    for(i=0; i<(int)dTecGhitRaw->RowCount(); i++)
    {

      dTecGhitDcm->SetRowCount(nok2+1);

      dTecGhitDcm->set_ghitid(i,dTecGhitRaw->get_ghitid(i));
      dTecGhitDcm->set_binnum(i,dTecGhitRaw->get_binnum(i));
      dTecGhitDcm->set_fraction(i,dTecGhitRaw->get_fraction(i));
      for(j=0; j<(int)dTecFemData->RowCount(); j++)
      {
        if(dTecGhitRaw->get_rawid(i) == dTecFemData->get_id(j))
        {
          icrate = dTecFemData->get_crate(j);
          islot = dTecFemData->get_slot(j);
          ipsadd = dTecFemData->get_psadd(j);
          ichan = dTecFemData->get_ichan(j);

// Find out arm, sector, etc. 
          TAO->setHard(icrate,islot,ipsadd,ichan);
          iarm=TAO->getArm();
          isect=TAO->getSector();
          iside=TAO->getSide();
          iwire=TAO->getWire();
          dTecGhitDcm->set_crate(i,icrate);
          dTecGhitDcm->set_slot(i,islot);
          dTecGhitDcm->set_channel(i,ichan);
          dTecGhitDcm->set_arm(i,iarm);
          dTecGhitDcm->set_sector(i,isect);
          dTecGhitDcm->set_side(i,iside);
          dTecGhitDcm->set_wire(i,iwire);
          break;
        } // rawid if 
      } // j - loop over dTecFemData entries 

      nok2++;

    } // i - loop over dTecGhitRaw entries 
  } // end filling dTecGhitDcm 

  if(Verbose > 0)
  {
    cout << "mTecPack -> Finished" << endl;
    cout << "mTecPack -> Number of entries in dTecDcmData table = " 
	 << dTecDcmData->RowCount() << endl;
    cout << "mTecPack -> Number of TecPackets = "
	 << tecpacketlist->getNPackets() << endl;
  }
  CallNumber++;

  if(Write2File > 0) fclose(out_file);

  return True;
}

