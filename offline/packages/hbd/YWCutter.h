#ifndef __YWCUTTER_H__
#define __YWCUTTER_H__

class YWParticle;

typedef struct threshold_container{
	float weight_pad0;
	float th_par[24][2];
	int cluster_pads[5];
}threshold_container_t;

class YWCutter{
	public:
		YWCutter();
		virtual ~YWCutter();
		void Init();
		void Initialize_200GeV();
		void Initialize_62GeV();
                void Initialize_HIJING();
		void SetRejection(float a);
                float GetRejection(){return rejection;};
		bool isGoodProjection(YWParticle &);
		bool isCAElectron(YWParticle &);
		bool isHBDElectron(YWParticle &);
		bool RejectBothParticle(YWParticle &, YWParticle &);
		void SetVerbose(int a){verbose = a;}
		float get_threshold(float,float,float);
		void rotate(int, float, float, float &, float &);
		int _classifyArea(int, int, float, float, int &);
		void SetRunnumber(int run);
		void SetMCFlag(int mcflag);
		void SetEventRejection(int centrality);

		bool isSingleElectron(int centrality,int size,float charge);
		bool isMissingElectron(int centrality,int size,float charge, int neighbor);

		threshold_container_t container[23][6][25];

	protected:
		int verbose;
		float rejection;
		enum PadType{
			type0 = 0,
			type1 = 1,
			type2 = 2,	
			type3 = 3,	
			type4 = 4,	
			type11 = 5,	
			type12 = 6,	
			type31 = 7,	
			type32 = 8,	
			type33 = 9,	
			type34 = 10,	
			type35 = 11,	
			type36 = 12,	
			type37 = 13,	
			type38 = 14,	
			type39 = 15,	
			type40 = 16,	
			type41 = 17,	
			type42 = 18,	
			type61 = 19,	
			type62 = 20,	
			type63 = 21,	
			type64 = 22	
		};

		float hbdpz_cut;
		float hbdpphi_cut;
		float hbdcharge_cut;
		int hbdsize_cut;
		int runnumber;
		double r2d,d2r;
		int mc_flag;
};
#endif
