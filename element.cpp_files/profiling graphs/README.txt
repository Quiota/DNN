The "Matrix Sizes" folder compares the performance of each matrix size separately. In each graph, there are 4 plots, each one of them corresponding to the #threads/core (varied from 1 to 4) for that particular matrix size. The "cracked" folder in the "Matrix Sizes" folder contains graphs for the cases where the operation failed due to memory leak or other miscellaneous issues like segmentation fault.

From the "Matrix Sizes" folder, it can be observed that the overhaed is maximum usually for 4 threads/core and minimum for 1 thread/core. However, using 1 thread/core might cause a performance penalty.


The "Threads per Core" folder compares the performance for a given #threads/core value. In each graph, there are multiple plots each of which correspond to the different matrix sizes for that particular #threads/core value.

From the "Threads per Core" folder, it can be observed that the overhead is usually the maximum for small matrices (1000x1000)  and lesser for large matrix operations (17000x17000)