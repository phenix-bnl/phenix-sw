void makerelgains() {

int dTecGeom_NumWires[6] = { 415, 415, 436, 447, 446, 468 };
int NARMS = 1;
int NSECTORS = 4;
int NPLANES = 6;
int NSIDES = 2;

float tmp,buff[25000];
int i,index,totnent,ibuf=0;
int is,ia,ip,iz,iw;

int mylim =9;

    for(ia=0; ia<NARMS; ia++) {
      for(is=0; is<NSECTORS; is++) {
        for(ip=0; ip<NPLANES; ip++) {
          for(iz=0; iz<NSIDES; iz++) {
            for(iw=0; iw<dTecGeom_NumWires[ip]; iw++) {

            index = is*NPLANES*NSIDES+ip*NSIDES+iz;
            buff[ibuf]=1.0;
             
// Zero gains for wires at the edges
              if(iw<mylim) buff[ibuf]=0.0;
              if((iw>dTecGeom_NumWires[ip]-mylim)) buff[ibuf]=0.0;

            ibuf++;

            } // end iw loop over wires
          } // end iz loop over z-sides
        } // end ip loop over planes
      } // end is loop over sectors
    } // end ia loop over arms

cout << ibuf << " entries in buffer filled." << endl;

totnent = ibuf;

const char *tecdb = "tecrelgain_database_1.txt";
ofstream file;
file.open(tecdb);

  for(int i=0; i<totnent; i++) {
    tmp=buff[i];
    file << tmp << endl;
  } 

  cout
  << "makerelgains:  TEC relative gains written to " << tecdb << endl
  << "            Total " << i << " " << ibuf << " rows."
  << endl;

}

