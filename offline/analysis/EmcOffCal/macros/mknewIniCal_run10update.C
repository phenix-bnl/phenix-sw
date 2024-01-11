// emc helper lusect
// sector 0,1,2,3 = W0,W1,W2,W3
// sector 4,5,6,7 = E2,E3,E0,E1


int readcoeff(int lusect,double coeff[]);
int twr2luid(int arm, int sect, int iy, int iz);


void mknewIniCal_run10update(const int lusect=0)
{
  //const int runnumber = 259576;
  //const int runnumber = 280076; //Run9pp500GeV
  //const int runnumber = 288014; //Run9pp200GeV
  const int runnumber = 308966; //Run10AuAu200GeV
  const int runnumber = 330693; //Run11pp500GeV
  int ntwr;
  double coeff[4608];
  
  ntwr=readcoeff(lusect,coeff);
  
  cout << "ntwr = " << ntwr << endl;
  
  
  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("libemcOMpg.so");
  gSystem->Load("libemcOMascii.so");
  
  //** extract IniCal
  emcCalibrationDataHelper cdh(runnumber,false);
  emcCalibrationData* s0=cdh.getCalibrationData("IniCal",lusect);
  
  
  //** modification 
  //PHTimeStamp time_begin(2009,1,1,0,0,0,0); //Run9pp500GeV
  //PHTimeStamp time_begin(2009,4,16,0,0,0,0); //Run9pp200GeV
  //PHTimeStamp time_begin(2010,1,1,0,0,0,0); //Run10AuAu200GeV
  PHTimeStamp time_begin(2011,1,1,0,0,0,0); //Run11pp500GeV
  PHTimeStamp time_end(2038,4,1,0,0,0,0);
  double oldvalue;
  
  for(int luid = 0; luid < ntwr; luid++)
    {
      //cout << luid << " " << coeff[luid] << endl;
      oldvalue=s0->GetValue(luid, 0);    // 0,1,2
      s0->Set(luid, oldvalue*coeff[luid], 0, 0);
      //s0->Set(luid, oldvalue, 0, 0);
    }
  
  // s0->Print();
  // s0->Print(1);
  
  
  //** output
  emcDataManager* dm = emcDataManager::GetInstance();
  //dm->SetDestinationDir("test.old");
  //dm->SetDestinationDir("test.new");
  //dm->SetDestinationDir("test");//comment out when for realz
  //s0->SetDestination(emcManageable::kFile_ASCII); //comment out when for realz
  s0->SetDestination(emcManageable::kDB_Pg); //comment out when testing
  s0->SetValidityPeriod(time_begin,time_end);
  dm->Write(*s0);
  
}

int readcoeff(int lusect,double coeff[])
{
  int arm,sect;
  int iarm,isect,iz,iy;
  int ntwr;
  double value;
  double value_err;
  ifstream fp;

  // emc helper armsect should be
  // sector 0,1,2,3 = W0,W1,W2,W3
  // sector 4,5,6,7 = E2,E3,E0,E1
  if( lusect>=0 && lusect<4 )
    {
      arm = 0;  // west
      sect = lusect;
    }
  else if( lusect>=4 && lusect<8 )
    {
      arm = 1;  // east
      switch( lusect )
	{
	case 4: sect = 2; break;
	case 5: sect = 3; break;
	case 6: sect = 0; break;
	case 7: sect = 1; break;
	}
    }
  else
    {
      cout << "invalid lusect = " << lusect << endl;
      return -1;
    } 

  string line;
  ntwr = 0;
  //fp.open("totcoeff_20080629.txt");
  fp.open("updated_coefficient.txt");
    
  while( getline(fp, line) )
    {
      int tmp_as, tmp_ypos, tmp_zpos;
      double tmp_coef;
      double tmp_coef_err;
      if( line.size()==0 ) break;
      if( line[0]=='#' ) continue;

      sscanf(line.c_str(), "%d  %d  %d  %lf  %lf",
             &tmp_as, &tmp_ypos, &tmp_zpos, &tmp_coef, &tmp_coef_err);

      // armsect in coef file should be
      // sector 0,1,2,3 = W0,W1,W2,W3
      // sector 4,5,6,7 = E0,E1,E2,E3
      iarm = tmp_as/4;
      isect = tmp_as%4;
      iy = tmp_ypos;
      iz = tmp_zpos;
      value = tmp_coef;

      /*
      sscanf(line.c_str(), "%d %d %d %d %lf",
	     &iarm, &isect, &iy, &iz, &value);
      */
      if(iarm == arm && isect == sect)
	{
	  int twrid = twr2luid(arm,sect,iy,iz);
	  //cout << twrid << endl;
	  coeff[twrid] = value;
	  cout <<  arm << " " << sect << " " << iy << " " << iz << " "
	       << twrid << " " << value<< endl;
	  ntwr++;
	}
    }
  fp.close();
  return ntwr;
}      

int twr2luid(int arm, int sect, int iy, int iz)
{
 int luid;
 if(arm == 0 || sect > 1)  // PbSc
   {
     luid = iy*72 + iz; 
   }
 else
   {
     luid = iy*96 + iz;
   }
 //cout << luid << endl;
 return luid;
}

     
 
