{
gSystem->Load("libfun4all");
gSystem->Load("libsvx");

  svxDetectorGeo* svxgeo = new svxDetectorGeo();
  svxgeo->Read_svxPISApar();

  //PHTimeStamp* Tsearch = new PHTimeStamp(2005,1,1,0,0,0);
  //PHTimeStamp* Tsearch = new PHTimeStamp(2009,10,1,0,0,0);
  //Tsearch->print(); cout << endl; 

//  cout << "start reading database..." << endl;
//  PHBoolean success2 = svxgeo->Fetch_svxPISApar(Tsearch);
//  cout << "   ...success = " << success2 << endl << endl;

  //svxgeo->Read_svxPISApar();
  
  //svxgeo->Write_svxPISApar();

  int ladary[4] = {10, 20, 16, 24};

  int sublay[2][24] = {{3,2, 4,3,2, 4,3,2, 2,3,4, 2,3,4, 2,3, 0,0,0,0,0,0,0,0}, 
                       {5,6,7, 5,6,7, 5,6,7, 5,6,7,  7,6,5, 7,6,5, 7,6,5, 7,6,5}};

  for(int ilayer=0; ilayer<4; ilayer++){
    for(int iladder=0; iladder<ladary[ilayer]; iladder++){
      float lad_cent[3];
      cout<<iladder<<" "<<ilayer<<" ";
      for(int ixyz=0; ixyz<3; ixyz++){
        lad_cent[ixyz] = svxgeo->get_SensorCenter(ilayer, iladder, 0, ixyz);
        cout<<lad_cent[ixyz]<<" ";
      }
      float radius = sqrt(lad_cent[0]*lad_cent[0] + lad_cent[1]*lad_cent[1]);
      cout<<" R= "<<radius<<" ";
      if(ilayer>1) cout<<"( "<<sublay[ilayer-2][iladder]<<" )";
      cout<<endl;
    }
  }
}

