#ifndef __YWPARTICLE_H__
#define __YWPARTICLE_H__
class YWParticle{
	public:
		YWParticle();
		~YWParticle();
		void Reset();
		void set_bbcq(float a){bbcq = a;}
		void set_bbcz(float a){bbcz = a;}
		void set_n0(int a){n0 = a;}
		void set_mom(float a){mom = a;}
		void set_pT(float a){pT = a;}
		void set_vertx(float a){vertx = a;}
		void set_verty(float a){verty = a;}
		void set_phi0(float a) {phi0 = a;}
		void set_alpha(float a){alpha = a;}
		void set_realflag(int a){realflag = a;}
		void set_hbdpphi(float a){ hbdpphi = a;}
		void set_hbddphi(float a){hbddphi = a;}
		void set_hbdpz(float a){hbdpz = a;}
		void set_hbddz(float a){hbddz = a;}
		void set_hbdpresize(int a){hbdpresize = a;}
		void set_hbdsize(int a){hbdsize = a;}
		void set_hbdcharge(float a){hbdcharge = a;}
		void set_padx(float a){padx = a;}
		void set_pady(float a){pady = a;}
		void set_locx(float a){locx = a;}
		void set_locy(float a){locy = a;}
		void set_padtype(int a){padtype = a;}
		void set_padcharge0(float a) { padcharge0 = a;}
		void set_padcharge1(float a) { padcharge1 = a;}
		void set_padcharge2(float a) { padcharge2 = a;}
		void set_padcharge3(float a) { padcharge3 = a;}
		void set_padcharge4(float a) { padcharge4 = a;}
		void set_padcharge5(float a) { padcharge5 = a;}
		void set_padcharge6(float a) { padcharge6 = a;}
		void set_padcharge7(float a) { padcharge7 = a;}
		void set_padcharge8(float a) { padcharge8 = a;}
		void set_padcharge9(float a) { padcharge9 = a;}
		void set_padkey0(int a) { padkey0 = a;}
		void set_padkey1(int a) { padkey1 = a;}
		void set_padkey2(int a) { padkey2 = a;}
		void set_padkey3(int a) { padkey3 = a;}
		void set_padkey4(int a) { padkey4 = a;}
		void set_padkey5(int a) { padkey5 = a;}
		void set_padkey6(int a) { padkey6 = a;}
		void set_padkey7(int a) { padkey7 = a;}
		void set_padkey8(int a) { padkey8 = a;}
		void set_padkey9(int a) { padkey9 = a;}
		void set_hbdsector(int a){hbdsector = a;}
		void set_hbdpadnum(int a){hbdpadnum = a;}
		void set_hbdside(int a){hbdside = a;}
		float get_bbcq(){return bbcq;}
		int get_n0(){return n0;}
		float get_bbcz(){return bbcz;}
		float get_vertx(){return vertx;}
		float get_verty(){return verty;}
		float get_phi0(){return phi0;}
		float get_alpha(){return alpha;}
		float get_mom(){return mom;}
		float get_pT(){return pT;}
		int get_realflag(){return realflag;}
		float get_hbdpphi(){return hbdpphi;}
		float get_hbddphi(){return hbddphi;}
		float get_hbdpz(){return hbdpz;}
		float get_hbddz(){return hbddz;}
		int get_hbdpresize(){return hbdpresize;}
		int get_hbdsize(){return hbdsize;}
		float get_hbdcharge(){return hbdcharge;}
		float get_padx() {return padx;}
		float get_pady() {return pady;}
		float get_locx() {return locx;}
		float get_locy() {return locy;}
		int get_padtype() {return padtype;}
		float get_padcharge0(){return padcharge0;}
		float get_padcharge1(){return padcharge1;}
		float get_padcharge2(){return padcharge2;}
		float get_padcharge3(){return padcharge3;}
		float get_padcharge4(){return padcharge4;}
		float get_padcharge5(){return padcharge5;}
		float get_padcharge6(){return padcharge6;}
		float get_padcharge7(){return padcharge7;}
		float get_padcharge8(){return padcharge8;}
		float get_padcharge9(){return padcharge9;}
		int get_padkey0(){return padkey0;}
		int get_padkey1(){return padkey1;}
		int get_padkey2(){return padkey2;}
		int get_padkey3(){return padkey3;}
		int get_padkey4(){return padkey4;}
		int get_padkey5(){return padkey5;}
		int get_padkey6(){return padkey6;}
		int get_padkey7(){return padkey7;}
		int get_padkey8(){return padkey8;}
		int get_padkey9(){return padkey9;}
		int get_hbdpadnum() {return hbdpadnum;}
		int get_hbdsector() {return hbdsector;}
		int get_hbdside() {return hbdside;}
		void SetVerbose(int a){verbose = a;}

        	void set_usedpad(int num, int key) {used_pads[num]=key;}
		int  get_usedpad(int num) {return used_pads[num];}

	private:
		int verbose;
		float bbcq;
		float bbcz;
		int n0;
		float mom;
		float pT;
		float alpha;
		float phi0;
		float vertx,verty;
		float hbdpphi;
		float hbdpz;
		float hbdcharge;
		int hbdsize;
		int hbdpresize;
		float hbddz;
		float hbddphi;
		float locx;
		float locy;
		float padx;
		float pady;
		int padtype;
		float padcharge0;
		float padcharge1;
		float padcharge2;
		float padcharge3;
		float padcharge4;
		float padcharge5;
		float padcharge6;
		float padcharge7;
		float padcharge8;
		float padcharge9;
		int padkey0;
		int padkey1;
		int padkey2;
		int padkey3;
		int padkey4;
		int padkey5;
		int padkey6;
		int padkey7;
		int padkey8;
		int padkey9;
		int hbdsector;
		int hbdpadnum;
		int hbdside;
		int realflag;
		int used_pads[10];
		float threshold;
};

//void copy_particle(YWParticle &, YWParticle &);
#endif
