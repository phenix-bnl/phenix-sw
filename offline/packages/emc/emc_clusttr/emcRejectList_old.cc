
#ifdef QA
  //===============================================================
  int ismz,ismy;
  int ism,itwr;
  int ierr,iwarning;
  char qadir[] = "/direct/phenix+data01/phnxemc/mirror/Data/Common/QA";
  char* qafile = new char[128];
  int nline =0;

  isect = 2;
  while(isect-- ){
    ismz = 6;
    while( ismz-- >0){
      ismy = 3;
      while( ismy-- >0 ){
	ism = ismz + ismy * 6;
	if( ism> 9 )
	  sprintf(qafile,"%s/W%dSM%2d.GAINS.QA",qadir,isect,ism);
	else
	  sprintf(qafile,"%s/W%dSM0%d.GAINS.QA",qadir,isect,ism);
	ifstream* pfin = new ifstream(qafile);
	if( pfin == 0 ) {
	  cerr<<" Can't open the file: "<<qafile<<endl;
	  exit(0);
	}
	while(pfin->get(tmpc)&&tmpc!='\n');  // TimeStamp
	while(pfin->get(tmpc)&&tmpc!='\n');  // TimeStamp
	while( *pfin>>itwr>>ierr>>iwarning){
	  iz = itwr%12 + ismz * 12;
	  iy = (int)(itwr/12) + ismy * 12;
	  frejtwr[isect][iz][iy] = frejtwr[isect][iz][iy] + ierr ;
	  nline++;
	}
	pfin->close();
      }
    }
  }
  delete qafile;
  cout<<" emcRejectList:: Reading "<<nline<<" QA channel"<<endl;



#endif

#ifdef QADM
  //===============================================================
  // Reading QA info from database.
  gSystem->Load("libPdbCal.so");
  gSystem->Load("/afs/rhic/phenix/users/aphecetc/install-27/lib/libEmcStatic.so");
  gSystem->Load("/afs/rhic/phenix/users/aphecetc/install-27/lib/libEmcDynamic.so");
  gSystem->Load("/afs/rhic/phenix/users/aphecetc/install-27/lib/libemcCalib.so") ;
  gSystem->Load("/afs/rhic/phenix/users/aphecetc/install-27/lib/libemcOM.so") ;

  char configfname[] = "/afs/rhic/phenix/users/kistenev/workarea/wb.conf";
  int status = 0 ;
  emcRawDataAccessor * rda = emcRawDataAccessor::GetInstance(status,configfname);
  emcDataManager* dm = emcDataManager::GetInstance() ;
  emcQAs qa ;
  emcQAs* pqa ;
  qa.SetSource( emcManageable::kFile_ASCII ) ;
  PHTimeStamp when(2000,8,3,0,0,0);
  dm->Collect(qa,when);


  pqa = dynamic_cast<emcQAs*>( dm->Collect(qa, when) ) ;
  cout<<" pqa->GetError(0) = "<<pqa->GetError(0)<<endl;
#endif QA
