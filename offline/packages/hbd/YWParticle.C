#include <iostream>
#include "YWParticle.h"

using namespace std;

YWParticle::YWParticle(){
	verbose = 0;
	bbcq = 0;
	hbdpphi = 0;
	hbdpz = 0;
	hbdsize = 0;
	hbdpresize =0;
	hbddz = 0;
	hbddphi = 0;
	hbdcharge = 0;
	padtype = 0;
	hbdsector = 0;
	hbdpadnum =0;
	padcharge0 = 0;
	padcharge1 = 0;
	padcharge2 = 0;
	padcharge3 = 0;
}

YWParticle::~YWParticle(){
}
void YWParticle::Reset(){
	if(verbose) cout<<"Resetting particle"<<endl;
	bbcq = 0;
	mom = 0;
	pT = 0;
	phi0 = 0;
	hbdpphi = 0;
	hbdpz = 0;
	hbdsize = 0;
	hbdpresize =0;
	hbddz = 0;
	hbddphi = 0;
	hbdcharge = 0;
	padtype = 0;
	hbdsector = 0;
	hbdpadnum =0;
	padcharge0 = 0;
	padcharge1 = 0;
	padcharge2 = 0;
	padcharge3 = 0;
	padcharge4 = 0;
	padcharge5 = 0;
	padcharge6 = 0;
	padcharge7 = 0;
	padcharge8 = 0;
	padcharge9 = 0;
	padkey0 = 0;
	padkey1 = 0;
	padkey2 = 0;
	padkey3 = 0;
	padkey4 = 0;
	padkey5 = 0;
	padkey6 = 0;
	padkey7 = 0;
	padkey8 = 0;
	padkey9 = 0;
	bbcz =0;
	alpha =0;
	vertx =0;
	verty =0;
	padx = 0;
	pady = 0;
	locx = 0;
	locy = 0;
	for (int i=0; i<10; i++) used_pads[i] = -999;
	threshold = 999;
}
