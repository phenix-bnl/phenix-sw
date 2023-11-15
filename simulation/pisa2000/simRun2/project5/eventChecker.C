//
// Root Macro to test eventMaker output
//
{
  gROOT->Reset();

  char file[] = "test.out";
  ifstream fin(file);

  int i, j, event, nin, nout;
  int id, ist;
  Float_t px,py,pz,p0;
  Float_t x,y,z,t,mass;
  Float_t mt,rap;
  Float_t d1,d2;
  char line[100];

  TFile *f = new TFile("event.root","RECREATE");
  TNtuple *ntuple = new TNtuple("ntuple","data from oscar file","event:i:id:mass:px:py:pz:p0:mt:rap");


  if (fin) {

    // Read the 4 dummy lines
    for (i=0; i<4; i++) {
      fin.getline(line,255);
      cout << line << endl;
    }
  }
  else {
    cout << "Error: Unable to open input file " << file << endl;
  }

  // Loop over event blocks
  event = 0;
  char ch;
  while (fin.peek()!=EOF) {
    event++;    
    fin >> nin >> nout;
    //    cout << ch << event << " " << nin << " " << nout << endl;
    for (i=0 ; i<nout; i++) {
      fin >> j >> id >> ist;
      fin >> px >> py >> pz >> p0 >> mass;
      fin >>  x >>  y >>  z >> t;

//       if (i!=j) {
// 	cout << "Particle# out of sync: i = " << i << " j = " << j << endl;
//       return 1;
//       }

      mt  = sqrt(mass*mass+px*px+py*py);
      rap = 0.5 * log((p0+pz)/(p0-pz));
      ntuple->Fill(event,i,id,mass,px,py,pz,p0,mt,rap);
      //      cout << id << " " << px << endl;
    }

    fin >> d1 >> d2;
    if ((d1!=0.0)||(d2!=0.0)) {
      cout << "Error ending event block:" << d1 << " " << d2 << endl;
      return 1;
    }
    // Throw away the last newline, for while(fin.peek) test to work.
    fin.ignore(1,'\n');
  }

  fin.close();
  
  f->Write();
}
