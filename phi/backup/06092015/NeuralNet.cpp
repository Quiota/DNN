#include "BackProp.h"
#include "ReadFile.h"
#include "DTime.h"
// NeuralNet.cpp : Defines the entry point for the console application.
//
int main(int argc, char* argv[])
{
        std::vector<double> trainData;
        matrix testData;
	std::vector<std::pair<int,int> > vec_nInputs;
	std::vector<std::pair<int,int> >::iterator it;
        std::vector<double> data;
        

        int nInputs;
        int nThreads=atoi(argv[1]);
        int batch_size=atoi(argv[2]);
	double tstart, tstop, ttime = 0.0;

        mkl_set_num_threads(nThreads); //set OMP_NUM_THREADS?
        tstart = dtime();
        nInputs = loadFiles(argv[3], trainData, vec_nInputs);
        tstop = dtime();
        std::cout<<"time to load files: " << (tstop-tstart) << "(s)"<<std::endl;
        //int offset_ = 0;
        /*for(it=vec_nInputs.begin() ; it < vec_nInputs.end(); it++) {
		std::cout<<it->first<<", "<<it->second<<std::endl;
                //std::cout<<trainData[offset_+0]<<","<<trainData[offset_+1]<<","<<trainData[offset_+2]<<std::endl;	
		//offset_ += it->first*it->second + 2;
        }*/
	if (0 >= nInputs) 
 	   return(0);
        std::cout<<"Num Inputs:"<<nInputs<<std::endl;      
	// defining a net with <numLayers> layers
	// the first layer is input layer i.e. simply holder for the input parameters
	// and has to be the same size as the no of input parameters
        const int numLayers = 6;
	int nSymbols = vec_nInputs.size();
        int lSz[numLayers] = {nInputs,1000,900,800,700,1};
	// Learing rate - beta
	// momentum - alpha
	// Threshhold - thresh (value of target mse, training stops once it is achieved)
	double beta = 0.3, alpha = 0.0, Thresh =  0.00001;
	// maximum no of iterations during training
	long num_iter = 2000000;
	// Creating the net
	CBackProp *bp;// = new CBackProp(numLayers, nInputs, batch_size, lSz, beta, alpha);
	
	std::cout<< std::endl <<  "Now training the network...." << std::endl;	

	for (long i=0; i<(int)num_iter/batch_size; i++)
	{
	        // randomly sample index	
		float *x = new float[batch_size*nInputs];
                float *t = new float[batch_size*nSymbols];

		for (int j=0; j<batch_size;j++){
	          int idx = (int)(rand()/vec_nInputs[0].first); 
		  int kidx=0; int kkidx = 0;
		  int offset=3;
 		  for (int k=0; k<nSymbols;k++){
		    if (k>0){
		      kidx += vec_nInputs[k-1].first*vec_nInputs[k-1].second+2; 
		      kkidx += vec_nInputs[k-1].second-offset; 
		    }
                    for (int l=0; l<(vec_nInputs[k].second-1);l++){
		      x[j*nInputs+ kkidx + l] = trainData[kidx +idx*vec_nInputs[k].second+ l+offset];
		   std::cout<<"x["<<(j*nInputs+ kkidx + l)<<"]:"<<trainData[kidx +idx*vec_nInputs[k].second+ l+offset]<< ",";
		   }
		   std::cout<<std::endl;
                   
                   t[j*nSymbols + k] = trainData[kidx +idx*vec_nInputs[k].second+ 2];
		   std::cout<<"t["<<j*nSymbols+k<<"]:"<<t[j*nSymbols+k]<<"=data["<<k<<","<<idx<<"]"<< std::endl;
		  }
		}
                std::cout<<"Back propagation for mini-batch iteration: "<<i<<std::endl;
		return(0);
		bp->bpgt(x, t); 
		//std::cout<<"target["<<idx<<"]: " << trainData[idx][nInputs]<<std::endl; 
		if( bp->mse(t) < Thresh) {
			std::cout << std::endl << "Network Trained. Threshold value achieved in " << i << " iterations." << std::endl;
			std::cout << "MSE:  " << bp->mse(t) 
				 <<  std::endl <<  std::endl;
			break;
		}
		if ( i%(num_iter/10) == 0 )
			std::cout<<  std::endl <<  "MSE:  " << bp->mse(t) 
				<< "... Training..." << std::endl;

	        if (i == (num_iter-1) )
		   std::cout << std::endl << i << " iterations completed..." 
		   << "MSE: " << bp->mse(t) << std::endl;  	
	}
        std::cout<<" Total time for training the network: "<< ttime << " seconds"<<std::endl;
        std::cout<<" Time per epoch of the back-propagation algorithm: "<< ttime/(double)num_iter << " seconds"<<std::endl;
	

	std::cout<< "Now using the trained network to make predctions on test data...." << std::endl << std::endl;	
	for (int i = 0 ; i < testData.size() ; i++ )
	{
		if ( i%(testData.size()/1000) == 0 ){
		   bp->ffwd((float*)testData[i].data());
                   //std::cout<<bp->Out(0)<<std::endl; 
                   std::cout << testData[i][0]<< "  " << testData[i][1]<< "  "  << testData[i][2]<< "....,   " << bp->Out(0) << std::endl;
		}
		/*for (int j =0; j < testData[i].size(); j++)
		   std::cout << testData[i][j]<< "  "; 
		std::cout<<std::endl;*/
	}

	return 0;
}


/*void write(const std::string& filename, matrix& data)
{
	int nRows = data.size();
	int nCols= data[0].size();
	std::ofstream out (filename.c_str(), std::ofstream::binary | std::ios::out);
        for (int i=0;i < data[0].size();i++)
          std::cout<< data[0][i]<< " ";
        for (matrix::iterator it= data.begin(); it != data.end(); ++it)
           out.write((char*)&it->front(), it->size() * sizeof(double));

        out.close();
}*/
