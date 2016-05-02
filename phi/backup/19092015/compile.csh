#!/usr/bin/csh
source /opt/intel/composer_xe_2015/bin/compilervars.sh intel64
source /opt/intel/impi/5.0.3.048/bin64/mpivars.sh
icc -O3 -mkl -openmp -mmic -o DNN.MIC NeuralNet.cpp BackProp.cpp FileIO.cpp DTime.cpp
scp -i ~/.ssh/id_rsa_IntelPhis DNN.MIC root@mic0:~/do_not_delete/.
#icc -O3 -mkl -openmp -mmic -o filetest.MIC filetest.cpp FileIO.cpp DTime.cpp
#scp -i ~/.ssh/id_rsa_IntelPhis filetest.MIC root@mic0:~/do_not_delete/.
#icc -O3 -mkl -openmp -mmic -o randtest.MIC randtest.cpp DTime.cpp
#scp -i ~/.ssh/id_rsa_IntelPhis randtest.MIC root@mic0:~/do_not_delete/.
