/* This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
 // Author: Matthew Dixon, Diego Klabjan, Jin Hoon Bang
 // Description: This file contains the entry point of the whole application as it implements the main method. Functionality ranging from the training, testing & initialization of the Deep Neural Network to writing the results on to the Hard Disk is performed by the code in this file.
 // Reference: Please cite as M. Dixon, D. Klabjan, and J. H. Bang. "Implementing Deep Neural Networks for Financial Market Prediction on the Intel Xeon Phi" at the Eighth Workshop on High Performance Computational Finance (WHPCF'15), held in conjunction with Supercomputing 2015, Austin, TX, November 2015. Available at SSRN: http://ssrn.com/abstract=2627258 or http://dx.doi.org/10.2139/ssrn.2627258.
 // Dependencies: Intel MKL Random Number generator and Intel MKL BLAS
 // Revision: 1.0

#include "BackProp.h"
#include "FileIO.h"
#include "DTime.h"
// NeuralNet.cpp : Defines the entry point for the console application.
//

/* Checks whether input string is a valid number. */
char checkNum(char *num)
{
    char *tmp;
    strtol(num, &tmp, 10);
    if (*tmp)
        return 0;
    return 1;
}

int main(int argc, char* argv[])
{
    if (argc != 4){
        std::cout<<"Invalid number of arguments. Please enter arguments in the format './DNN <batch-size> <num-threads> <file-list>'"<<std::endl;
        return 0;
    }
    else{
        if ((checkNum(argv[1]) == 0) || (checkNum(argv[2]) == 0)){
            std::cout<<"Invalid arguments entered. Please enter arguments in the format './DNN <batch-size> <num-threads> <file-list>'"<<std::endl;
            return 0;
        }
    }

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
        int k =0;
        for(it=vec_nInputs.begin() ; it < vec_nInputs.end(); it++) {
		std::cout<<it->first<<", "<<it->second<<","<<trainData[k][2]<<std::endl;
                std::cout<<trainData[k][3]<<","<<trainData[k][4]<<","<<trainData[k][5]<<std::endl;
		k++;
        }
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
	double alpha = 0.0, Thresh =  0.00001;
	// maximum no of iterations during training
        long num_epochs = 100;
	long num_iter = 200;
        int test_size = (int)(0.25*vec_nInputs[0].first);
	int train_size = vec_nInputs[0].first - test_size;
	int epoch_size = 15000;

	float * t_hat = new float[nSymbols*test_size];
	float * r_hat = new float[nSymbols*test_size];
	float * x_hat = new float[nInputs*test_size];

	// prepare the test set
        std::cout<<"Preparing the test set of size:"<<test_size<<std::endl;
        int offset = 3;
        for (int idx=0;idx<test_size;idx++){
	  int jidx = 0;
 	  for (int j=0; j<nSymbols;j++){
            t_hat[idx*nSymbols+j] = (int)trainData[j][idx*vec_nInputs[j].second+ 2];
            if (j>0){
             jidx+= (vec_nInputs[j-1].second-1);
	    }
            for(int k=0;k<(vec_nInputs[j].second-1);k++){
               double val = trainData[j][idx*vec_nInputs[j].second+ k + offset];
	       if (k==0)
		 r_hat[idx*nSymbols+ j]=val;
	       x_hat[idx*nInputs+ jidx +k] = val;
	     }
           }
        }
	// Creating the net

	std::cout<< std::endl <<  "Now training the network...." << train_size<< std::endl;
        int idx=0;
	float *x = new float[batch_size*nInputs];
        float *t = new float[batch_size*nClasses];
	float *indices = new float[epoch_size];
	for (double beta = 0.1; beta<1.0; beta+=0.2){

         srand((unsigned) time(0));
         std::cout<<"Constructing network: " <<std::endl;
         tstart = dtime();
	 //CBackProp *bp = new CBackProp(numLayers, nInputs, batch_size, nSymbols, lSz,beta);
	 CBackProp bp(numLayers, nInputs, batch_size, nSymbols, lSz, beta);
         tstop = dtime();
         std::cout<<"time to construct CBackProp: " << (tstop-tstart) << "(s)"<<std::endl;
         for (int e=0;e<num_epochs;e++)
	   for (int j=0; j<epoch_size;j++){
	      indices[j] = test_size+ (int)(rand()%train_size); //train on observations from 0 to training size
	      for (int i=0; i<num_iter; i++){
	        // randomly sample index
		for (int j=0; j<batch_size;j++){
	          idx =indices[(int)(rand()%epoch_size)]; //train on observations from 0 to training size
		  int kidx=0;
 		  for (int k=0; k<nSymbols;k++){
                      if (k>0)
		         kidx+= vec_nInputs[k-1].second-offset;
                      for (int l=0; l<(vec_nInputs[k].second-1);l++){
			 double val = trainData[k][idx*vec_nInputs[k].second+ l+offset];
			 if(std::isnan(val) || std::isinf(val))
		            x[j*nInputs+ kidx + l] = 0.0;
		         else
		            x[j*nInputs+ kidx + l] = val;
		      }
		      for (int m=0;m<nStates;m++)
                         t[j*nClasses+ k*nStates + m] = 0.0;
		      int val =	(int)trainData[k][idx*vec_nInputs[k].second+ 2] +1;
                      t[j*nClasses +k*nStates + val] = 1.0;

		  }
		} // batch
                //        std::cout<<"Back propagation for mini-batch iteration: "<<i<<std::endl;
		tstart = dtime();
                for (int si=0;si<10;si++){
		 //bp->bpgt(x, t);
		 bp.bpgt(x, t);
	        }
 		tstop = dtime();
                ttime += tstop - tstart;
                std::cout<<"> Back-propagation time: "<<(tstop-tstart)/10.0<<"(s)"<<std::endl;

		char* test = getenv("PHI_TEST");
	        if (test !=NULL){
		 if ( i%atoi(test) == 0 ){
                  std::cout<<"> Test: Feed forward network construction..."<<std::endl;
		  tstart = dtime();
		  //bp->change_depth(test_size);
		  //bp->ffwd(x_hat,true);
		  bp.change_depth(test_size);
		  bp.ffwd(x_hat,true);
		  writeResults(r_hat, t_hat, test_size, nSymbols);
 		  tstop = dtime();
                  ttime += tstop - tstart;
                  std::cout<<"> Feed forward network construction for test set:"<<(tstop-tstart)<<"(s)"<<std::endl;


                  //std::cout<<"Calculating error rate: "<<i<<std::endl;
		  //double error = bp->F1(t_hat,test_size,"micro");
		  double error = bp.F1(t_hat,test_size,"micro");
		  std::cout<<"F1 error ["<<i<<"]:"<<error<<std::endl;
		  //error = bp->error_rate(t_hat,test_size);
		  error = bp.error_rate(t_hat,test_size);
		  std::cout<<"Classification error ["<<i<<"]:"<<error<<std::endl;
		  //error = bp->cross_entropy(t_hat, test_size);
		  error = bp.cross_entropy(t_hat, test_size);
		  std::cout<<"Cross entropy error ["<<i<<"]:"<<error<<std::endl;
		  //bp->change_depth(batch_size);
		  bp.change_depth(batch_size);
		 }
		}

	  } //end i
	 }
        } //beta
        std::cout<<" Total time for training the back-propagation algorithm : "<< ttime << " seconds"<<std::endl;
        delete x;
        delete t;
        delete x_hat;
        delete r_hat;
        delete t_hat;
        delete indices;

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
