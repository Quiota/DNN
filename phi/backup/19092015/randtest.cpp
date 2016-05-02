#include <stdio.h>
#include <iostream>
#include "mkl.h"
#include "DTime.h"

using namespace std;
MKL_LONG status;
VSLStreamStatePtr stream;
int main()
{
   double tstart = dtime();
   vslNewStream(&stream, VSL_BRNG_SFMT19937, 777);
   int total = 1;
   for (int i=0;i<10000;i++){
       double* rnd_data = (double*)malloc(total * sizeof(double));
       status = vdRngUniform(VSL_RNG_METHOD_UNIFORM_STD, stream, total, rnd_data, -1.0, 1.0);
       cout<<"r:"<<rnd_data[0]<<",";
   }
   cout<<endl;
   vslDeleteStream(&stream);

   double tstop = dtime();
   cout<<"time:"<<(tstop-tstart)<<"(s)"<<endl;
}
