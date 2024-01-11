
//_____________________________________________________________________________
// 1st iteration (Saskia's weights)
//
//   Float_t p0[CentClasses] = {0.274359,0.274359,
//                  0.279511,0.279511,
//                  0.288906,0.288906,
//                  0.293015,0.293015,
//                  0.309597,0.309597,
//                  0.31598,0.31598,
//                  0.311112,0.311112,
//                  0.306987,0.306987,
//                  0.318363,0,0,
//                  0.297001};

//   Float_t p0_error[CentClasses] = {0.00564065,0.00564065,
//                 0.00506642,0.00506642,
//                 0.00462844,0.00462844,
//                 0.00498305,0.00498305,
//                 0.00536839,0.00536839,
//                 0.00522563,0.00522563,
//                 0.00610136,0.00610136,
//                 0.00315042,0.00315042,
//                 0.00330951,0,0,
//                 0.00558602};

//   Float_t p1[CentClasses] = {0.00640976,0.00640976,
//                  0.00856748,0.00856748,
//                  0.0119575,0.0119575,
//                  0.0109983,0.0109983,
//                  0.011084,0.011084,
//                  0.0114206,0.0114206,
//                  0.0139033,0.0139033,
//                  0.0127932,0.0127932,
//                  0.0125875,0,0,
//                  0.0134319};

//   Float_t p1_error[CentClasses] = {0.00113145,0.00113145,
//                 0.00129757,0.00129757,
//                 0.00123244,0.00123244,
//                 0.00107299,0.00107299,
//                 0.00135477,0.00135477,
//                 0.00128936,0.00128936,
//                 0.00140611,0.00140611,
//                 0.000788874,0.000788874,
//                 0.00103881,0,0,
//                 0.00136206};

//_____________________________________________________________________________

// 1st iteration (David's weights)
//  Float_t p0[CentClasses] = {0.267411,0.267411,
//                 0.274827,0.274827,
//                 0.28648,0.28648,
//                 0.297527,0.297527,
//                 0.305412,0.305412,
//                 0.30428,0.30428,
//                 0.303717,0.303717,
//                 0.306104,0.306104,
//                 0.316009,0,0,
//                 0.293709};
//  Float_t p0_error[CentClasses] = {0.00632552,0.00632552,
//                0.00539669,0.00539669,
//                0.00474452,0.00474452,
//                0.00450373,0.00450373,
//                0.0055097,0.0055097,
//                0.00584222,0.00584222,
//                0.00525652,0.00525652,
//                0.00317134,0.00317134,
//                0.00341669, 0,0,
//                0.00567475};

//  Float_t p1[CentClasses] = {0.00849988,0.00849988,
//                 0.00988458,0.00988458,
//                 0.012258,0.012258,
//                 0.00913931,0.00913931,
//                 0.0120343,0.0120343,
//                 0.0151813,0.0151813,
//                 0.0157988,0.0157988,
//                 0.0124634,0.0124634,
//                 0.0136905, 0,0,
//                 0.0133741};

//  Float_t p1_error[CentClasses] = {0.00148395,0.00148395,
//                0.00146577,0.00146577,
//                0.00129031,0.00129031,
//                0.000829277,0.000829277,
//                0.00143416,0.00143416,
//                0.00153401,0.00153401,
//                0.00141001,0.00141001,
//                0.000809206,0.000809206,
//                0.00108988,0,0,
//                0.00145479};

// 2nd iteration David's weights

Float_t p0[CentClasses] = {0.274508,0.274508, //  0-10%
			   0.276017,0.276017, // 10-20%
			   0.287278,0.287278, // 20-30%
			   0.292872,0.292872, // 30-40%
			   0.305886,0.305886, // 40-50%
			   0.304365,0.304365, // 50-60%
			   0.303592,0.303592, // 60-70%
			   0.306328,0.306328, // 70-80%
			   0.316178,0.316178, // 80-92%
			   0.305,   0.29401}; // 60-80% + 0-92%

Float_t p0_error[CentClasses] = {0.00853992,0.00853992, //  0-10%
				 0.00546053,0.00546053,
				 0.0047409, 0.0047409,
				 0.00503005,0.00503005, // 30-40%
				 0.00553572,0.00553572,
				 0.00590891,0.00590891,
				 0.00521469,0.00521469, // 60-70%
				 0.00317232,0.00317232,
				 0.00340667,0.00340667,
				 0.0062    ,0.00571287}; // 60-80% + 0-92%

Float_t p1[CentClasses] = {0.00770245,0.00770245, //  0-10%
			   0.0101855, 0.0101855,
			   0.0127449, 0.0127449,
			   0.0111319, 0.0111319,  // 30-40%
			   0.0125228, 0.0125228,
			   0.0157798, 0.0157798,
			   0.0163419, 0.0163419,  // 60-70%
			   0.0128199, 0.0128199,
			   0.0141241, 0.0141241,
			   0.0146   ,0.0140175}; // 60-80% + 0-92%

Float_t p1_error[CentClasses] = {0.00179456,0.00179456,
				 0.00149061,0.00149061,
				 0.00129878,0.00129878,
				 0.00109082,0.00109082,
				 0.00143748,0.00143748,
				 0.00155222,0.00155222,
				 0.00141149,0.00141149,
				 0.000802736,0.000802736,
				 0.00109309,0.00109309,
				 0.002     ,0.00144894}; // 60-80% + 0-92%



//_____________________________________________________________________________

// 1st iteration yesterday ... ;-)
//
//   Float_t p0[CentClasses] = {3.21237e-01, 3.21237e-01, // 10
//                  3.35306e-01, 3.35306e-01, // 20
//                  3.49344e-01, 3.49344e-01, // 30
//                  3.63564e-01, 3.63564e-01, // 40
//                  3.78570e-01, 3.78570e-01, // 50
//                  3.90351e-01, 3.90351e-01, // 60
//                  3.99867e-01, 3.99867e-01, // 70
//                  3.98510e-01, 3.98510e-01, // 80
//                  3.96293e-01, 0, 0,        // 92
//                  3.76377e-01};             // min.bias

//   Float_t p0_error[CentClasses] = {6.83773e-03, 6.83773e-03,
//                 6.22022e-03, 6.22022e-03,
//                 5.88363e-03, 5.88363e-03,
//                 6.01372e-03, 6.01372e-03,
//                 5.74787e-03, 5.74787e-03,
//                 5.74893e-03, 5.74893e-03,
//                 5.72576e-03, 5.72576e-03,
//                 5.64870e-03, 5.64870e-03,
//                 5.21520e-03, 0, 0,
//                 2.25104e-03};
//   Float_t p1[CentClasses]       = {1.27153e-02, 1.27153e-02,
//                 1.25462e-02, 1.25462e-02,
//                 1.34109e-02, 1.34109e-02,
//                 1.41243e-02, 1.41243e-02,
//                 1.28730e-02, 1.28730e-02,
//                 1.21340e-02, 1.21340e-02,
//                 1.17519e-02, 1.17519e-02,
//                 9.45437e-03, 9.45437e-03,
//                 1.36524e-02, 0, 0,
//                 1.16184e-02};
//   Float_t p1_error[CentClasses] = {1.44185e-03, 1.44185e-03,
//                 1.37585e-03, 1.37585e-03,
//                 1.37585e-03, 1.37585e-03,
//                 1.27293e-03, 1.27293e-03,
//                 1.20568e-03, 1.20568e-03,
//                 1.20466e-03, 1.20466e-03,
//                 1.19043e-03, 1.19043e-03,
//                 1.15841e-03, 1.15841e-03,
//                 1.09392e-03, 0, 0,
//                 4.69144e-04};