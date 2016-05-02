#include "BackProp.h"
#include "DTime.h"

//	initializes and allocates memory on heap
CBackProp::CBackProp(int nl,int ni,int batch_size_,int nSymbols_,int *sz,double b,double a):numi(ni),batch_size(batch_size_),nSymbols(nSymbols_),beta(b),alpha(a)
{
	double tstart, tstop = 0.0;

	//	set no of layers and their sizes
	numl=nl;
	lsize=new int[numl];

	for(int i=0;i<numl;i++){
		lsize[i]=sz[i];
	}

	//	allocate memory for output of each neuron
	out = new double*[numl];

	for(int i=0;i<numl;i++){
		out[i]=new double[lsize[i]*batch_size];
	}

	//	allocate memory for delta
	delta = new double*[numl];

	for(int i=1;i<numl;i++){
		delta[i]=new double[lsize[i]*batch_size];
	}

	//	allocate memory for weights
	weight = new double*[numl];
	//min_weight = new double*[numl];
	//max_weight = new double*[numl];

	for(int i=1;i<numl;i++){
		weight[i]=new double[lsize[i]*(lsize[i-1]+1)];
		//min_weight[i]=new double[lsize[i]*(lsize[i-1]+1)];
		//max_weight[i]=new double[lsize[i]*(lsize[i-1]+1)];
	}

	//	allocate memory for previous weights
	prevDwt = new double*[numl];

	for(int i=1;i<numl;i++){
		prevDwt[i]=new double[lsize[i]*(lsize[i-1]+1)];
	}

	//	seed and assign random weights
	//srand((unsigned)(time(NULL)));
        VSLStreamStatePtr stream;
        double* rnd_data;
        vslNewStream(&stream, VSL_BRNG_SFMT19937, 777); //SEED=777
	for(int i=1;i<numl;i++){
                int total = (lsize[i-1]+1)*lsize[i];
	        rnd_data = new double[total]; 
                tstart = dtime();	
		vdRngUniform(VSL_RNG_METHOD_UNIFORM_STD, stream, total, rnd_data, -1.0, 1.0);
                tstop = dtime();
	        std::cout<<"Layer "<<i<<" rng generation: "<< (tstop-tstart)<<"(s)"<<std::endl;
		for(int j=0;j<lsize[i];j++){
                        #pragma omp parallel for
			for(int k=0;k<(lsize[i-1]+1);k++){
				int jidx = k*lsize[i] + j;
				weight[i][jidx]=rnd_data[j*(lsize[i-1]+1)+k];
			        //min_weight[i][jidx] = -1.0; 	
			        //max_weight[i][jidx] = 1.0; 	
 			}
			#pragma omp barrier
                }
        }
        vslDeleteStream(&stream);
        delete rnd_data;
	//	initialize previous weights to 0 for first iteration
        tstart = dtime();	
	for(int i=1;i<numl;i++)
		for(int j=0;j<lsize[i];j++)
                        #pragma omp parallel for
			for(int k=0;k<(lsize[i-1]+1);k++){
				int jidx = k*lsize[i] + j;
				prevDwt[i][jidx]=(double)0.0;
			}
			#pragma omp barrier

        tstop = dtime();
	std::cout<<"Prev Weight Initialization: "<< (tstop-tstart)<<"(s)"<<std::endl;
// Note that the following variables are unused,
//
// delta[0]
// weight[0]
// prevDwt[0]

//  I did this intentionaly to maintains consistancy in numbering the layers.
//  Since for a net having n layers, input layer is refered to as 0th layer,
//  first hidden layer as 1st layer and the nth layer as output layer. And 
//  first (0th) layer just stores the inputs hence there is no delta or weigth
//  values corresponding to it.
}

void CBackProp::filterWeights(int i, int j, int k){
        return;
     	if (weight[i][k*lsize[i]+j] > max_weight[i][k*lsize[i]+j])
	   weight[i][k*lsize[i]+j] = max_weight[i][k*lsize[i]+j];
     	else if (weight[i][k*lsize[i]+j] < min_weight[i][k*lsize[i]+j])
	   weight[i][k*lsize[i]+j] = min_weight[i][k*lsize[i]+j];
}

CBackProp::~CBackProp()
{
	//	free out
	for(int i=0;i<numl;i++)
		delete[] out[i];
	delete[] out;

	//	free delta
	for(int i=1;i<numl;i++)
		delete[] delta[i];
	delete[] delta;

	//	free weight
	for(int i=1;i<numl;i++){
		delete[] weight[i];
		delete[] min_weight[i];
		delete[] max_weight[i];
	}
	delete[] weight;
	delete[] min_weight;
	delete[] max_weight;

	//	free prevDwt
	for(int i=1;i<numl;i++)
		delete[] prevDwt[i];
	delete[] prevDwt;

	//	free layer info
	delete[] lsize;
}

//	sigmoid function
double CBackProp::sigmoid(double in) const
{
		return (double)(1/(1+exp(-in)));
}

// error rate of confusion matrix
double CBackProp::error_rate(const float *tgt, int length) const{

   double error = 0.0;
   std::cout<<"In error_rate function"<<std::endl;
   //#pragma omp parallel for
   for (int i=0;i<length;i++){
       int k=0;
       for (int j=0;j<nSymbols*3;j+=3){
		int idx = i*lsize[numl-1] + j;
		//std::cout<<"idx="<<idx<<",";
		int val = max(&out[numl-1][idx],3)-1;
		//std::cout<<"val="<<val<<",";
		if (val==tgt[i*nSymbols + k])
                    error+=1.0; 
		k++;
	}
    }
    #pragma omp barrier
      
   return(error/(length*nSymbols));
}
int CBackProp::max(double* ar, int len) const
{
    int pos = 0;
    int max = 0; 
    for (int i=0;i<len;i++){
      if (ar[i]>=max){
	max = ar[i];
	pos = i;
      }
    }
    return(pos);
}
//	mean square error
double CBackProp::mse(float *tgt) const
{
	double mse=0;
        for(int i=0;i<batch_size;i++)
	   for(int j=0;j<lsize[numl-1];j++){
		int idx = i*lsize[numl-1] + j;
		mse+=(tgt[idx]-out[numl-1][idx])*(tgt[idx]-out[numl-1][idx]);
	}
	return mse/(2.0*batch_size);
}


//	returns i'th output of the net
double CBackProp::Out(int i) const
{
	return out[numl-1][i];
}
void CBackProp::change_depth(int depth){
	batch_size = depth;
        for(int i=0;i<numl;i++){
             delete[] out[i];
             out[i]=new double[lsize[i]*batch_size];
        }
}

// feed forward one set of input
void CBackProp::ffwd(float *in)
{
	double sum;
	std::cout<<"In ffwd function"<<std::endl;
	//	assign content to input layer
        #pragma omp parallel for 
        for(int i=0;i<batch_size;i++)
	  for(int j=0;j<lsize[0];j++){
	    if (std::isnan(in[i*lsize[0]+j]) || std::isinf(in[i*lsize[0]+j]))
		in[i*lsize[0]+j] =0.0;
             out[0][i*numi+j]=in[i*lsize[0]+j];  // output_from_neuron(i,j) Jth neuron in Ith Layer
	  }

	//	assign output(activation) value 
	//	to each neuron usng sigmoid func
  	double factor = 1.0;	
        //const MKL_INT one = 1.0;
        double alpha_ = 1.0, beta_ = 0.0; /* Scaling factors */
        char trans = 'N'; /* Transposition options */
        #pragma omp parallel for 
	for(int l=0;l<batch_size;l++)		// For each batch member
	     for(int i=1;i<numl;i++) {			// For each layer
               MKL_INT m = lsize[i];
               MKL_INT n = lsize[i-1]+1;
	       MKL_INT one = 1; 
	       int kidx = l*lsize[i-1];// +k;
               //double tstart = dtime();
               double* sum = new double[m];
               //dgemv(&trans, &m, &n, &alpha_, weight[i], &m, &out[i-1][kidx], &one, &beta_, sum, &one); 
               
		for(int j=0;j<lsize[i];j++){ // For each neuron in current layer
	         //       std::cout<<"dgemv Sum["<<i<<","<<j<<"]: "<<sum[j]<<std::endl;
			sum[j] =0.0;
			for(int k=0;k<lsize[i-1];k++){		// For input from each neuron in preceeding layer
			   /*if (std::isnan(out[i-1][kidx+k]) || std::isinf(out[i-1][kidx+k]))
			       std::cout<<"Error: out["<<i-1<<","<<kidx+k<<","<<l<<"]: isnan or isinf"<<std::endl;
			   else if (std::isnan(weight[i][k*lsize[i]+j]) ||std::isinf(weight[i][k*lsize[i]+j]) )
			       std::cout<<"Error: weight["<<i<<","<<k*lsize[i]+j<<","<<l<<"]: isnan or isinf"<<std::endl;
			   else*/
			     sum[j]+= out[i-1][kidx+k]*weight[i][k*lsize[i] + j];	// Apply weight to inputs and add to sum
			 }
			 //if (std::isnan(weight[i][lsize[i-1]*lsize[i]+j]) ||std::isinf(weight[i][lsize[i-1]*lsize[i]+j]) )
		     //std::cout<<"Error: bias weight["<<i<<","<<lsize[i-1]*lsize[i]+j<<","<<l<<"]: isnan or isinf"<<std::endl;
	              //   else	
		        sum[j]+=weight[i][lsize[i-1]*lsize[i] + j]; //w[i][j][lsize[i-1]]		// Apply bias
		//	if (std::isnan(sum[j]) || std::isinf(sum[j]))
		//        std::cout<<"Non-dgemv Sum["<<i<<","<<j<<"]: "<<sum[j]<<std::endl;
	       }	
	       int u =0;
               for (int j=0;j<lsize[i];j++){
	          int jidx = l*lsize[i] + j;
   	          if (i == (numl-1) && ( j%3==0 )){
		        //std::cout<<"Instrument ["<<u<<","<<j<<"]: "<< sum[j]<<","<<sum[j+1]<<","<<sum[j+1]<<std::endl;
	                factor = exp(sum[j])+exp(sum[j+1])+exp(sum[j+2]);
                        //double sum_check =0.0;
		        for (int k=0;k<3;k++){
			    out[i][jidx+k]=exp(sum[j+k])/factor;
			    //std::cout<<"out["<<u<<","<<k<<"]:"<<out[i][jidx+k]<<",";
			    //sum_check += out[i][jidx+k];
		        }
			u++;
		        //std::cout<<sum_check<<std::endl;
		 }
  		 else if (i <(numl-1))
		    out[i][jidx]=sigmoid(sum[j]);
	     }
	  } // end i
#pragma omp barrier	
}
//	backpropogate errors from output
//	layer uptill the first hidden layer
void CBackProp::bpgt(float* x,float *tgt)
{
	double sum;
        double tstart, tstop, ttime = 0.0;

	//	update output values for each neuron
  	tstart = dtime();
	ffwd(x);
        tstop = dtime();
        ttime += tstop - tstart;
        std::cout<<"> Feed forward network construction:"<<(tstop-tstart)<<"(s)"<<std::endl;

  	tstart = dtime();
	//	find delta for output layer
        #pragma omp parallel for 
        for(int i=0;i<batch_size; i++)	
	   for(int j=0;j<lsize[numl-1];j++){
		int idx = i*lsize[numl-1] + j;
	        //std::cout<<"tgt["<<idx<<"]="<<tgt[idx]<<"out["<<idx<<"]="<<out[numl-1][idx]<<std::endl;
                delta[numl-1][idx]=out[numl-1][idx]*
                (1-out[numl-1][idx])*(tgt[idx]-out[numl-1][idx]); 
	   }

         #pragma omp barrier
         tstop = dtime();
         std::cout<<"delta for output later "<<(tstop-tstart)<<"(s)"<<std::endl;
	//	find delta for hidden layers	
  	tstart = dtime();
        #pragma omp parallel for 
        for(int l=0;l<batch_size;l++)
	    for(int i=numl-2;i>0;i--){
		for(int j=0;j<lsize[i];j++){
			sum=0.0;
			for(int k=0;k<lsize[i+1];k++){
				int kidx = l*lsize[i+1] + k;
				sum+=delta[i+1][kidx]*weight[i+1][j*lsize[i+1]+k];
			}
			int jidx = l*lsize[i] + j;
			delta[i][jidx]=out[i][jidx]*(1-out[i][jidx])*sum;
		}
	}
	#pragma omp barrier

        tstop = dtime();
        ttime += tstop - tstart;
        std::cout<<">delta propagation:"<<(tstop-tstart)<<"(s)"<<std::endl;


	/*tstart = dtime();
	//	apply momentum ( does nothing if alpha=0 )
	for(int i=1;i<numl;i++){
	  for(int j=0;j<lsize[i];j++){
             #pragma omp parallel for 
    	     for(int k=0;k<lsize[i-1];k++){
			weight[i][k*lsize[i]+j]+=alpha*prevDwt[i][k*lsize[i]+j];
			filterWeights(i,j,k);
	 	}
		// is this necessary?
		weight[i][lsize[i-1]*lsize[i] +j]+=alpha*prevDwt[i][lsize[i-1]*lsize[i] + j];
		filterWeights(i,j,lsize[i-1]);
		}
	}
        tstop = dtime();
        ttime += tstop - tstart;
        std::cout<<">apply momentum:"<<(tstop-tstart)<<"(s)"<<std::endl;*/

	//	adjust weights usng steepest descent	

        /*`for (int l=0; l<batch_size;l++)
	   for(int i=1;i<numl;i++){
		for(int j=0;j<lsize[i];j++){
			int jidx = l*lsize[i] + j;
			#pragma omp parallel for
			for(int k=0;k<lsize[i-1];k++){
			        int kidx = l*lsize[i-1] + k;
				prevDwt[i][j][k]=beta*delta[i][jidx]*out[i-1][kidx];
				weight[i][j][k]+=prevDwt[i][j][k];
				filterWeights(i,j,k);
			}
			prevDwt[i][j][lsize[i-1]]=beta*delta[i][j];
			weight[i][j][lsize[i-1]]+=prevDwt[i][j][lsize[i-1]];
			filterWeights(i,j,lsize[i-1]);
		}*/


        //#pragma omp barrier
	
        double alpha_ = beta, beta_ = 1.0; // Scaling factors 
        char transa = 'T', transb = 'N'; // Transposition options 

        for(int i=1;i<numl;i++){
	  MKL_INT m = lsize[i-1] +1; 
	  MKL_INT n = lsize[i];
	  MKL_INT k = batch_size;
          tstart = dtime();
          dgemm(&transa, &transb, &m, &n, &k, &alpha_, out[i-1], &k, delta[i], &k, &beta_, weight[i], &m);
          tstop = dtime();
          ttime += tstop - tstart;
          /*for (int u=0; u<k*m;u++){
             if (weight[i][u]<-1.0) 
		weight[i][u]=-10.0;
             else if( weight[i][u] > 10.0)
		weight[i][u]=10.0;
	  }*/


          double gflops = (2.0e-9*m*n*k)/(tstop-tstart);
          std::cout<<">dgemm for weights update in layer("<<i<<"): "<<(tstop-tstart)<<"(s)"<<std::endl;
	  std::cout<<"GFlops/s: "<<gflops<<std::endl;
        }

        std::cout<<">Total time for bpgt: "<<ttime<<"(s)"<<std::endl;

}
