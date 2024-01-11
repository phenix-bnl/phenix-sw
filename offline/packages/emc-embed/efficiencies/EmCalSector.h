class EmCalSector{

  // r0: vector pointing from PHENIX origin to lower right corner
  //     of the sector when viewing from nominal vertex position 
  //     (= origin of 2d sector surface coordinate system)
  // y0: vector along the y-axis of the detector surface, 
  //     |y0| = 1 module length
  // z0: vector along the z-axis of the detector surface,
  //     |z0| = 1 module length

  float r0[3];          
  float y0[3];
  float z0[3];

  // the number of modules in y in z direction
  int nModY;
  int nModZ;

  // the modules length in cm
  float lModY;
  float lModZ;

  // matrices used for coordinate system transformation
  float A[3][3];
  float Ainv[3][3];

public:

  void  init(char* const);
  int  Hit(float*, float*);

};

