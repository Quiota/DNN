export LD_LIBRARY_PATH=/root/sbiswas
export KMP_AFFINITY=compact
export turbo=enabled

for ((mat_size=1000; mat_size<=50000; mat_size+=1000)); do
	for ((cores=1; cores<=60; cores*=2)); do
		for ((threads=1; threads<=4; threads++)); do
			
			conf=$cores'c,'$threads't';
			export KMP_PLACE_THREADS=$conf
			echo 'Experiment with matrix size='$mat_size' using '$cores' cores with '$threads' threads per core'
			echo 'Matrix Size = '$mat_size >> report2.txt
			echo 'Experiment with '$threads' threads/core on '$cores' cores' >> report2.txt
			./e_inplace.mic $mat_size >> report2.txt
			


		done

		if [ $cores -eq 32 ]
                then
                	cores=30
		fi

	done
done

