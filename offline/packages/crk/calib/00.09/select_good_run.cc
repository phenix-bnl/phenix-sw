#include <iostream>
#include <fstream>
main() {
  ifstream runlist("run.lst");
  int run, year, month, day, hour, min, sec;
  float Nped;

  while(!runlist.eof()) {
    runlist >> run >> year >> month >> day >> hour >> min >> sec >> Nped;
    if(Nped > 5000) {
      cout << run <<" "<<  year <<" "<< month <<" "<< day <<" ";
      cout << hour <<" " << min <<" "<< sec <<" "<< Nped <<endl;
    }
  }
}
