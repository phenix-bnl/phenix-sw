//int MakeOutputParameters(char* inputfile="/common/standalonedata/calibdata/rc-0420300-ZDC-0.prdf", char* outpath="DSTOut"){
//char outfile[255];
int MakeOutputParameters(char* inputfile, char* outpath, char* outfile){

  string PRDFNAME = inputfile;
  cout<<"PRDF File Name: "<<PRDFNAME<<endl;
  
  int runnumber;
  int segnumber;
  int ppg=0;
  if(PRDFNAME.find("rc-")<10000) //standalone
    {
      ppg=0;
      string rn=PRDFNAME.substr(PRDFNAME.find("rc-")+3,7);
      runnumber=atoi(rn.c_str());
      segnumber=0;
      
      /* put those lines for standalone event in 2015 */
      char filetag[255];
      sprintf(filetag,"-0000%d-0000",runnumber);
      string FileTag(filetag);
      cout<<"standalone"<<endl;
    }
  else
    {
      if( PRDFNAME.find("_ppg_") <10000) //ppg trigger 
	ppg=1;
      else
	ppg=0;

      int length = PRDFNAME.size();
      string FileTag = PRDFNAME.substr(length-22,16);
      istringstream RUNNUMBER(FileTag.substr(1,10));
      RUNNUMBER >> runnumber;
      istringstream SEGNUMBER(FileTag.substr(12,4));
      SEGNUMBER >> segnumber;
      cout<<"big partition"<<endl;
    }

  cout<<"runnumber: "<<runnumber<<endl;
  cout<<"segnumber: "<<segnumber<<endl;
  cout<<"FileTag: "<<FileTag<<endl;
  
 
  string FILE("DST");
  string SYST="_ZDCSMDPOL";

  string EXTN=".root";

  if(ppg==0)
    sprintf(outfile,"%s/%s%s%s%s",outpath,FILE.c_str(),SYST.c_str(),FileTag.c_str(),EXTN.c_str() );
  else
    sprintf(outfile,"%s/%s%s%s_ppg%s",outpath,FILE.c_str(),SYST.c_str(),FileTag.c_str(),EXTN.c_str() );
  
  // output = OUTPATH + FILE + SYST + NUM + EXTN;
  cout<<"Making nDST file : "<<outfile<<endl;

  return runnumber;

}
