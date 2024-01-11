#include <HbdDcmRaw.h>
#include <hbdDetectorGeo.hh>
#include <HbdCellList.h>
#include <HbdCell.h>
#include <HbdMiniCellList.h>
#include <HbdMiniCell.h>
#include <PHTimeStamp.h>
#include <Event.h>
#include <packet.h>
#include <recoConsts.h>

// They are DB classes
#include <PdbBankID.hh>
#include <PdbApplication.hh>
#include <PdbBankManager.hh>
#include <PdbCalBank.hh>

#include <cstdlib>
#include <fstream>
#include <iostream>

using namespace std;

HbdDcmRaw::HbdDcmRaw()
{
   fem_id[0]=22001;
   fem_id[1]=22002;
   fem_id[2]=22003;
   fem_id[3]=22004;
   fem_id[4]=22005;
   fem_id[5]=22006;
   fem_id[6]=22007;
   fem_id[7]=22008;
   fem_id[8]=22009;
   fem_id[9]=22010;
   fem_id[10]=22011;
   fem_id[11]=22012;

   p=NULL;

   NFEM=12;
   NMOD=4;
   NALLCHAN=NFEM*NMOD*NCHAN_PER_MOD;

   // default calibration. It must be wrong
   for(int i=0;i<NALLCHAN;i++){
      ped[i]=0.0;
      pep[i]=1.0;
      pedw[i]=1.0;
      pepw[i]=1.0;

      // By default, all pads are alive.
      Alive[i]=1;
   }

//   readCalibFromFile("CalibPar.txt");

   NSAMPLE = MAXSAMPLE;

   Nthsample=4;

   bPhysics = 1;
   verbosity=0;

   ChargeThreshold=0; // Dummy to keep things running...


   first_event=1;

   return;
}

int HbdDcmRaw::Init()
{
   recoConsts *rc = recoConsts::instance();
   PHTimeStamp t_setToSystemTime(2009,4,26,0,0,0);
   PHTimeStamp tNow = rc->get_TimeStamp();
   hbdDetectorGeo t;
   if(tNow.getTics()<100){
     t.fetchPad(t_setToSystemTime);}
   else t.fetchPad(tNow);
   
   for(int i=0;i<2304;i++){
      int seqsec,padid,arm,sec;
      t.getPadInfo(i,arm,sec,seqsec,padid);
      SeqSec[i] = seqsec;
      PadId[i] = padid-1; //pad numbers go from 0-191
   }

   return 0;
}   

int HbdDcmRaw::CheckAndPreserveEvent(Event* evt)
{
  if (verbosity) evt->identify();

  //  Reset the data every time to avoid user error...
  for(int padid = 0;padid<NALLCHAN; padid++){
     for(int samp=0; samp<NSAMPLE; samp++){
        AdcVal[padid][samp]=-100;
        AdcCorVal[padid][samp]=-100.0;
     }
     for(int i=0;i<3;i++) AdcSumVal[padid][i]=-100;
  }

  // If this is not a data event, skip it
  if( evt->getEvtType() != 1 )  return 0;

//  cout << "NMOD: " << NMOD <<", NFEM: " << NFEM << endl;

//  int runno;
  for(int fem_seq = 0; fem_seq<NFEM;fem_seq++){
     // If there is an existing packet pointer, free the memory
//     if(p){delete p; p=0;}
     p=0;

     // Get the next packet and length of it
     if(!(p=evt->getPacket(fem_id[fem_seq]))){ continue;}

     NsampleFromData[fem_seq] = p->iValue(0,"NRSAMPLES");

     // Read the packet data into an integer array of length wordsperpacket
//     runno = evt->getRunNumber();

     // Converting ADC channel to PadId
     for(int i=0;i<NMOD;i++){
        for(int j=0;j<NCHAN_PER_MOD;j++){

           int adcid;
           adcid=(fem_seq*NMOD+i)*NCHAN_PER_MOD+j;

           if(NsampleFromData[fem_seq]==12){
              for(int k=0;k<12;k++)
                 AdcVal[adcid][k]= p->iValue(i*NCHAN_PER_MOD+j,k);
           }

           for(int k=0;k<3;k++)
              AdcSumVal[adcid][k]= p->iValue(i*NCHAN_PER_MOD+j,100+k);
        }

        // Several hardware information
        beamclock[fem_seq][i] = p->iValue(i,"BCLK");
        physmod[fem_seq][i] = p->iValue(i,"MODULEID");
        lvl1trig[fem_seq][i] = p->iValue(i,"TRIGGER");
     }
     delete p;
  }

  return 0;
}

int HbdDcmRaw::GetEvent(Event* evt, HbdCellList* celllist )
{

  CheckAndPreserveEvent(evt);

  //
  // Check if the cell list is filled, and not deleted. 
  //
  //  The "physics" bit chooses whether to fill cells
  if (bPhysics)
    {
      int ncell_left = celllist->get_nCells();
      if(ncell_left!=0)
	{
	  if (verbosity) cout << "Remove cells: " << ncell_left << endl;
	  for(int i=0;i<ncell_left;i++) celllist->RemoveCell(i);
	  celllist->set_nCells(0);
	  if (verbosity) cout << "Removal done." << endl;
	}
      if(StoreToHbdCell(celllist)) return 1;
      else return 0;
    }

  return 0;
}

int HbdDcmRaw::GetEventToMiniCell(Event* evt, HbdMiniCellList* minicelllist )
{

  CheckAndPreserveEvent(evt);

  //
  // Check if the cell list is filled, and not deleted. 
  //
  //  The "physics" bit chooses whether to fill cells
  if (bPhysics)
    {
      int ncell_left = minicelllist->get_nCells();
      if(ncell_left!=0)
	{
	  if (verbosity) cout << "Remove cells: " << ncell_left << endl;
	  for(int i=0;i<ncell_left;i++) minicelllist->RemoveCell(i);
	  minicelllist->set_nCells(0);
	  if (verbosity) cout << "Removal done." << endl;
	}
      if(StoreToHbdMiniCell(minicelllist)) return 1;
      else return 0;
    }

  return 0;
}


int HbdDcmRaw::StoreToHbdCell(HbdCellList* celllist)
{
//  celllist = new HbdCellList();

  for(int adcid= 0; adcid<NALLCHAN; adcid++){

     // If this is disabled padid, don't include in CellList
     if(Alive[adcid]==0) continue;

     float netsignal = ( AdcSumVal[adcid][2]- AdcSumVal[adcid][0] )/2.0;

     netsignal = -1.0* netsignal; 
     if(netsignal<ChargeThreshold) continue;


//     cout << "Store another!" << endl;
     int ncell = celllist->get_nCells();
     celllist->AddCell(ncell);
     celllist->set_nCells(ncell+1);
     HbdCell *hbdcell = celllist->get_cell(ncell);

     //
     // These are temporary
//     hbdcell->set_row(0);
//     hbdcell->set_col(0);
     hbdcell->set_sector(SeqSec[adcid]);
     hbdcell->set_padnum(PadId[adcid]); 
     hbdcell->set_charge(netsignal);
//     hbdcell->set_npe   (netsignal/10.);
  }
  return 0;
}


int HbdDcmRaw::StoreToHbdMiniCell(HbdMiniCellList* minicelllist)
{
  for(short adcid= 0; adcid<NALLCHAN; adcid++){

     // If this is disabled padid, don't include in MiniCellList
     if(Alive[adcid]==0) continue;

     // This means that this channel is zero-suppressed
     if(AdcSumVal[adcid][0]==0 && AdcSumVal[adcid][2]==0) continue;

     short netsignal = (AdcSumVal[adcid][0] - AdcSumVal[adcid][2]);

     if(netsignal<(short)ChargeThreshold) continue;

     int ncell = minicelllist->get_nCells();
     minicelllist->AddCell(ncell);
     minicelllist->set_nCells(ncell+1);
     minicelllist->SetConvFactor(2.0);
     HbdMiniCell *hbdminicell = minicelllist->get_cell(ncell);

     hbdminicell->set_adcch(adcid); 
     hbdminicell->set_charge(netsignal);
  }
  return 0;
}


void HbdDcmRaw::ChanToPadId(int fem_seq, int modul, int chan, int& padid,int runno)
{
   //
   // THis is for proto-type only
   //
//   int order[8] = {3,2,1,0,7,6,5,4};
   int order[8] = {0,1,2,3,4,5,6,7};

   padid=(fem_seq*NMOD+modul)*NCHAN_PER_MOD+order[chan%8]+(chan/8)*8;
//   cout << "padid: " << padid << endl;


/*
   if(runno<0203650){

     //  By noon of 5/31/06 

     if(verbosity) cout << "Old padid to ADC channel Config" << endl; 

     if(modul==0){
       if(chan<4) padid+=12;
       else if(chan<16) padid -=4;
     }
   }
   else{

     //  After noon of 5/31/06 

     if(verbosity) cout << "New padid to ADC channel Config" << endl; 

     if(modul==0 && chan<16){
       int corr[16]={2,1,8,7,6,5,12,11,10,9,16,15,14,13,3,4};
       for(int i=0;i<16;i++){
          if(corr[i]-1==chan) padid=i;
       }
    }
  }
*/
}


void HbdDcmRaw::PadIdToPos(int padid, int& sec, int &col, int &row)
{
   //
   // They are for proto-type only 
   //
   int col_order[9] = {8,6,4,2,0,7,5,3,1};

   sec = padid/68;
   row =  padid/9;
   col =  col_order[padid%9];

//   cout << "row: " << row << "col: " << col<<endl;
}


int HbdDcmRaw::SetPacketManually(int *inpacket, int fem_seq, int nwords)
{
//  if(memcpy(packetdata[fem_seq],inpacket,sizeof(int)*nwords)){
//     wordsperpacket=nwords;
//     return 1;
//  }
  cout << "SetPacketManually no longer functions" << endl;
  return 1;
}


//
// Fetch calibration information from the database
//
int HbdDcmRaw::fetchCalibDB(const int run)
{
   const char *geomName = "calib.hbd.charge";
   PdbBankManager* bankManager = PdbBankManager::instance();
   PdbApplication* application = bankManager->getApplication();
   if(application->startRead()){
      PdbBankID bankID(1);
      PdbCalBank* hbdBank = bankManager->fetchBank("PdbHbdChargeBank", bankID, geomName, run);

      if(hbdBank){
            hbdBank->print();
            cout << "# of channels = " << hbdBank->getLength() << endl;
         delete hbdBank;
      }
   }
   return 0;
}


int HbdDcmRaw::updateCalibDB(PHTimeStamp& tStart, PHTimeStamp& tStop)
{

     const char *geomName = "calib.hbd.charge";
     const char *hbddescript = "HBD charge calibration";
     PdbBankManager* bankManager = PdbBankManager::instance();
     PdbApplication* application = bankManager->getApplication();
     cout << "opening db in update mode " << geomName << endl;

     if(application->startUpdate()){
        PdbBankID bankID(1); // bankID used at subsystem discretion.  Currently not for hbd.
        PdbCalBank* hbdBank = bankManager->createBank("PdbHbdChargeBank",
                                                      bankID,
                                                      hbddescript,
                                                      tStart,tStop,
                                                      geomName );
//        hbdBank->setLength(total);
//        if(debug) hbdBank->print();

        char username[20];
        cout << endl << "Please put your name" << endl;
        cin.getline(username,20);
        PHString UserName(username);
        hbdBank->setUserName(UserName);
        application->commit(hbdBank);
     }
     return 0;
}


void HbdDcmRaw::readCalibFromFile(const char *calfile)
{
   ifstream fp(calfile);

   if(fp){
      int padid;
      int nchan=0;
      float mean,offset;
      char dum[5];
      while(fp>>dum>>padid>>mean>>offset){
         ped[padid]=0.0;  pedw[padid]=0.0;
         pep[padid]=mean; pepw[padid]=0.0;
         cout << "Padid: " << padid << ", par: " << pep[padid] << endl;

         nchan ++;
      }
      cout << "successfully read out calibrations of " << nchan << " channels" << endl;
   }
   else { cout << "cannot open file: " << calfile << endl; exit(-1); }
}
