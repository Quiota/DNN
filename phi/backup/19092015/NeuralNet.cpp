#include "BackProp.h"
#include "FileIO.h"
#include "DTime.h"
// NeuralNet.cpp : Defines the entry point for the console application.
//
int main(int argc, char* argv[])
{
        matrix trainData;
	std::vector<std::pair<int,int> > vec_nInputs;
	std::vector<std::pair<int,int> >::iterator it;
        std::vector<double> data;
        

        int nInputs;
        int batch_size=atoi(argv[1]);
        int nThreads=atoi(argv[2]);
        omp_set_num_threads(nThreads);
        mkl_set_num_threads(nThreads);
	double tstart, tstop, ttime = 0.0;

        tstart = dtime();
        nInputs = loadFiles(argv[3], trainData, vec_nInputs);
        tstop = dtime();
        std::cout<<"time to load files: " << (tstop-tstart) << "(s)"<<std::endl;
        /*int k =0; 
        for(it=vec_nInputs.begin() ; it < vec_nInputs.end(); it++) {
		std::cout<<it->first<<", "<<it->second<<","<<trainData[k][2]<<std::endl;
                std::cout<<trainData[k][3]<<","<<trainData[k][4]<<","<<trainData[k][5]<<std::endl;	
		k++;
        }*/ 
	if (0 >= nInputs) 
 	   return(0);
        std::cout<<"Num Inputs:"<<nInputs<<std::endl;      
	// defining a net with <numLayers> layers
	// the first layer is input layer i.e. simply holder for the input parameters
	// and has to be the same size as the no of input parameters
        const int numLayers = 6;
	int nSymbols = vec_nInputs.size();
	const int nStates = 3;
        int nClasses = nSymbols * nStates;
        int lSz[numLayers] = {nInputs,1000,900,800,700,nClasses};
	// Learing rate - beta
	// momentum - alpha
	// Threshhold - thresh (value of target mse, training stops once it is achieved)
	double beta = 0.3, alpha = 0.0, Thresh =  0.00001;
	// maximum no of iterations during training
	long num_iter = 15000;
        int test_size = (int)(0.1*vec_nInputs[0].first);
	int train_size = vec_nInputs[0].first - test_size;

	float * t_hat = new float[nSymbols*test_size];
	float * x_hat = new float[nInputs*test_size];

	// prepare the test set
        std::cout<<"Preparing the test set of size:"<<test_size<<std::endl;      
        int offset = 3;
	int jidx = 0; 
        for (int idx=0;idx<test_size;idx++)
 	  for (int j=0; j<nSymbols;j++){
            t_hat[idx*nSymbols+j] = (int)trainData[j][idx*vec_nInputs[j].second+ 2]; 
            //std::cout<<"t_hat["<<j<<","<<idx<<"]:"<<t_hat[idx*nSymbols+j]<<",";
            if (j>0){ 
             jidx+= (vec_nInputs[j].second-1);
	    }
	    //std::cout<<std::endl;
            for(int k=0;k<(vec_nInputs[j].second-1);k++){	
                   x_hat[idx*nInputs+ jidx +k] = trainData[j][idx*vec_nInputs[j].second+ k + offset]; 
	     }
        }
	// Creating the net
        std::cout<<"Constructing network: " <<std::endl;
        tstart = dtime();
	CBackProp *bp = new CBackProp(numLayers, nInputs, batch_size, nSymbols, lSz, beta, alpha);
        tstop = dtime();
        std::cout<<"time to construct CBackProp: " << (tstop-tstart) << "(s)"<<std::endl;
	
	std::cout<< std::endl <<  "Now training the network...." << train_size<< std::endl;	
        int idx=0;
	float *x = new float[batch_size*nInputs];
        float *t = new float[batch_size*nClasses];

	for (long i=0; i<num_iter; i++)
	{
	        // randomly sample index	
		for (int j=0; j<batch_size;j++){
                   
	          idx = test_size + (int)(rand()%train_size); //train on observations from 0 to training size
		  int kidx=0;
 		  for (int k=0; k<nSymbols;k++){
                      if (k>0)
		          kidx+= vec_nInputs[k-1].second-offset; 
                      for (int l=0; l<(vec_nInputs[k].second-1);l++)
		         x[j*nInputs+ kidx + l] = trainData[k][idx*vec_nInputs[k].second+ l+offset];
		      //std::cout<<"x["<<(j*nInputs+ kidx + l)<<"]:"<<trainData[k][idx*vec_nInputs[k].second+l+offset]<< ",";
		      for (int m=0;m<nStates;m++) 
                         t[j*nClasses+ k*nStates + m] = 0.0;
		      int val =	(int)trainData[k][idx*vec_nInputs[k].second+ 2] +1; 
                      t[j*nClasses +k*nStates + val] = 1.0;

                      //std::cout<<"t["<<k<<","<<idx<<"]:"<<t[j*nSymbols+k]<<",";
		  }
		  //std::cout<<std::endl;
		} // batch
                std::cout<<"Back propagation for mini-batch iteration: "<<i<<std::endl;
		bp->bpgt(x, t); 
                //for (int n=0;n<nInputs;n++)
		//  std::cout<<"x["<<idx<<", "<<n<<"]: " << x[n]<<std::endl;
		if ( i%100 == 0 ){
 		  
		  tstart = dtime();
		  bp->change_depth(test_size);
		  bp->ffwd(x_hat);
 		  tstop = dtime();
                  ttime += tstop - tstart;
                  std::cout<<"> Feed forward network construction for test set:"<<(tstop-tstart)<<"(s)"<<std::endl;


                  std::cout<<"Calculating error rate: "<<i<<std::endl;
		  double error = bp->error_rate(t_hat,test_size);
		  std::cout<<"error_rate ["<<i<<"]:"<<error<<std::endl;
		  bp->change_depth(batch_size);
		}	
 
	} //end i
        std::cout<<" Total time for training the network: "<< ttime << " seconds"<<std::endl;
        std::cout<<" Time per epoch of the back-propagation algorithm: "<< ttime/(double)num_iter << " seconds"<<std::endl;
        delete x;
        delete t;	
        delete x_hat;
        delete t_hat;
        
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
