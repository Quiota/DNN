#include "ReadFile.h"
#include "DTime.h"

int main(int argc, char* argv[])
{
   std::vector<std::pair<int,int> > vec_offsets;
   double tstart, tstop;
   tstart = dtime();
   if (*argv[2]=='p'){
      matrix data;
      try{
         int nInputs = loadFiles(argv[1], data, vec_offsets);
      }
      catch(std::exception& e){
         std::cout<<e.what()<<std::endl;
	 throw e;
     }
   }
   else{ 
      std::vector<double> data;
      int nInputs = loadFiles(argv[1], data, vec_offsets);
   }
   tstop = dtime();
   std::cout<<(tstop-tstart)<<"(s)"<<std::endl;
}
