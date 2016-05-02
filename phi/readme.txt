This file provides instructions on how to run the Deep Neural Network (DNN) code for predicting financial markets.

==Compile on CPU==
To compile on the cpu type

make cpu

==Compile on Phi==

make phi

Note that the executable DNN must be copied on to the device. 

A list of all .so dependencies that must be copied onto the device are listed in
dependencies.txt.


==Running the code ==
Run the code (in training mode) with arguments

./DNN <batch-size> <num-threads> <file-list>

For example, using a batch-size=1024, num-threads=240 and a list of data files in list

./DNN 1024 240 list

list contains a filepath to a list of binary data files in the data folder. 

It is recommended to use at least 180 threads when running the code on the Intel Xeon Phi in order to exploit the hardware resources. It is further recommended that the batch-size be a minimum of the number of threads.

Note that it takes a considerable amount of time to train the model, the details of which are provided in the paper:

Dixon, Klabjan and Bang, Implementing Deep Neural Networks for Financial Market Prediction on the Intel Xeon Phi, Proceedings of the Eighth Workshop on High Performance Computational Finance, SC15, November 20th, 2015 
http://dl.acm.org/citation.cfm?id=2830556

For profiling purposes it may be easiest to terminate after one epoch of deep neural network training. 

In order to measure the accuracy of the prediction, a test function can be
called by setting the environment variable PHI_TEST to an integer value which
represents the frequency that the test is called. For example, setting
PHI_TEST to 100 will call the test function every 100 mini-batch iterations.
If the environment variable is not set then the test function is not called.

==Comments on the stability of the code==
Occasionally the CPU and Phi version of the code will exit with a segmentation
fault. The exact cause of this is correctly unknown. It is recommended that
the code be simply rerun if this occurs.

UPDATE: This has been fixed on 3/23/2016
