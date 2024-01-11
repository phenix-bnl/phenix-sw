// gSystem->Load("libPgCalInstance.so");
void maskread(const char *ffname,PdbErtSMMask *ertp);
void adddb(PdbErtSMMask *ertp);
void wait(int seconds);

void mask_read_putDB(const char *flist="./run8masks.txt")
{
 int brun,erun;
 char pdir[64],fname[256];
 char ffname[256];
 PdbErtSMMask *ert=new PdbErtSMMask();
 sprintf(pdir,"/phenix/WWW/trigger/pp/c-arm/Run3/Run8ROCmask/");

 ifstream flp;
 flp.open(flist);


 flp >> brun >> erun >> fname;
 while(flp.good())
 {
   sprintf(ffname,"%s%s",pdir,fname);
   cout << ffname << endl;

   ert->Reset();
   ert->SetVersion(0);		// 0: from mask file
   ert->SetFirstRun(brun);
   ert->SetLastRun(erun);

   maskread(ffname,ert);
//   ert->print();
   adddb(ert);

   wait(2);

   flp >> brun >> erun >> fname;
 }
 flp.close();
}




void maskread(const char *ffname,PdbErtSMMask *ertp)
{
 ifstream fp;

 char ctyp[64];
 char ctmp[256];
 int arm,sect,sm,erta,ertb,ertc,ert2x2,ertr;

 fp.open(ffname);
 
 fp.getline(ctmp,256);
 while(fp.good())
 {
   if(strstr(ctmp,"EMC"))
   {
     sscanf(ctmp,"%s%d%d%d %d%d%d%d",ctyp,&arm,&sect,&sm,&erta,&ertb,&ertc,&ert2x2);
//     cout << "EMC  " << arm*4+sect << " " << sm << " " << erta*1000+ertb*100+ertc*10+ert2x2 << endl;
     ertp->Set(arm,sect,sm,0,erta);
     ertp->Set(arm,sect,sm,1,ertb);
     ertp->Set(arm,sect,sm,2,ertc);
     ertp->Set(arm,sect,sm,3,ert2x2);
   }
   if(strstr(ctmp,"RICH"))
   {
     sscanf(ctmp,"%s%d%d%d %d",ctyp,&arm,&sect,&sm,&ertr);
//     cout << "RICH " << arm*4+sect << " " << sm << " " << ertr << endl;
     ertp->Set(arm,sect,sm,4,ertr);
   }
   fp.getline(ctmp,256);
 }

 fp.close();
}
     

/////////////////////////////////////////
void adddb(PdbErtSMMask *ertp)
{
 int brun,erun;
 PdbBankManager* bm = PdbBankManager::instance();
 PdbApplication* app = bm->getApplication();

 RunToTimePg::Register();
 RunToTime *run_time=RunToTime::instance();

 PdbBankID bankID;
 bankID.setInternalValue(0);

 if(app->startUpdate ())
   {
     brun=ertp->GetFirstRun();
     erun=ertp->GetLastRun();

     PHTimeStamp tStart= PHTimeStamp(*(run_time->getBeginTime(brun)));
     PHTimeStamp tStop= PHTimeStamp(*(run_time->getEndTime(erun)));

     cout << "tStart " << brun << " " << tStart.formatTimeString() << endl;
     cout << "tStop  " << erun << " " << tStop.formatTimeString() << endl;

     PdbCalBank *ertsmBank = bm->createBank("PdbErtSMMaskBank", bankID, "Run8 mask info",tStart,tStop,"calib.ertsmmask");

     ertsmBank->setLength(1);
   cout << "getLength() = " << ertsmBank->getLength () << endl;
     PdbErtSMMask *entry = (PdbErtSMMask *) & (ertsmBank->getEntry (0));
     *entry = *ertp;
     app->commit(ertsmBank);
   }
}


void wait(int seconds)
{
 time_t endtime;

 endtime=time(NULL) + seconds;
 while(time(NULL)<endtime){}
}


