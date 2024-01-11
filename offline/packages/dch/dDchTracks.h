#ifndef __DDCHTRACKS_H__
#define __DDCHTRACKS_H__

typedef struct {
   short trackid;
   short arm;
   short side;
   float point[3];
   float err_point[3];
   float direction[3];
   float err_direction[3];
   short hits[40];
   short quality;
   float phi;
   float alpha;
   float beta;
   float betaNoVertex;
   float zed;
   float phi0;
   float theta0;
   float momentum;
} DDCHTRACKS_ST;
#endif /*__DDCHTRACKS_H__*/
