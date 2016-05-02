import numpy as np
from matplotlib import pyplot as plt
import copy
from mpl_toolkits.mplot3d import Axes3D



f = open('report2.txt', 'rb')
data = np.array(f.readlines())


sizes = range(1000, 20001, 1000)
res = dict.fromkeys(sizes)
mat_dict = None

cores = np.array([1, 2, 4, 8, 16, 32, 60])
threads = np.arange(1, 5)

mat_sz = 0
prev_mat_sz = 0
num_cores = 0
threads_per_core = 0
cell = dict.fromkeys(['operation', 'dgemm', 'diff', 'fraction'])
for k in cell.iterkeys():
    cell[k] = np.zeros(4)


for d in data:
    line = d.strip().split()
    
    if 'Matrix' in line:
        mat_sz = int(line[-1])

        if mat_sz != prev_mat_sz:
            res[prev_mat_sz] = copy.deepcopy(mat_dict)
            mat_dict = dict.fromkeys(cores)
            
            for k in mat_dict.iterkeys():
                mat_dict[k] = copy.deepcopy(cell)

            prev_mat_sz = mat_sz
            continue
        continue

    if 'cores' in line:
        num_cores = int(line[-2])
        threads_per_core_idx = int(line[2]) - 1

    if 'Total' in line:
        mat_dict[num_cores]['operation'][threads_per_core_idx] = float(line[-2])

    if 'DGEMM' in line:
        mat_dict[num_cores]['dgemm'][threads_per_core_idx] = float(line[-2])

    if 'Difference' in line:
        mat_dict[num_cores]['diff'][threads_per_core_idx] = float(line[-2])
        mat_dict[num_cores]['fraction'][threads_per_core_idx] = mat_dict[num_cores]['diff'][threads_per_core_idx] / mat_dict[num_cores]['operation'][threads_per_core_idx]

res[mat_sz] = copy.deepcopy(mat_dict)



#fig = plt.figure()
#ax = fig.add_subplot(111, projection='3d')
#plots = []
#color = np.zeros(3)
#cidx = 0
#for sz in sizes:
#    grph = []
#    for c in res[sz].iterkeys():
#        times = res[sz][c]['fraction']
#        if times[0] > 0.:
#            for i in xrange(times.shape[0]):
#                grph.append([c, i+1, times[i]])
#    grph = np.array(grph)
#    color[cidx%3] += .2
#    tmp = ax.scatter(grph[:, 0], grph[:, 1], grph[:, 2], c=tuple(color))
#    cidx += 1
#    plots.append(tmp)

#ax.set_xlabel('#Cores')
#ax.set_ylabel('#Threads/Core')
#ax.set_zlabel('Fractional time loss')
#ax.text2D(0.05, 0.95, 'Performance Loss Plot', transform=ax.transAxes)
#ax.legend(plots, ['1000x1000', '2000x2000', '3000x3000', '4000x4000', '5000x5000', '6000x6000', '7000x7000', '8000x8000', '9000x9000'])
#plt.show()

#for sz in sizes:
#    precs = []
#    losses = np.array([[res[sz][i]['fraction'][j] for i in cores] for j in xrange(4)])
#    for l in losses:
#        prec_tmp, = plt.plot(cores, l, '-o')
#        precs.append(prec_tmp)

#    plt.title('Loss Fraction vs #Cores for ' + str(sz) + 'x' + str(sz))
#    plt.xlabel('#Cores')
#    plt.ylabel('Loss Fraction')
#    plt.legend(precs, ['1 thread/core', '2 threads/core', '3 threads/core', '4 threads/core'])
#    plt.xticks(cores)
#    plt.show()

for tc in xrange(4):
    precs = []
    valid_sizes = range(1000, 8001, 1000)
    valid_sizes += [10000, 11000, 13000, 14000, 16000, 17000]
    losses = np.array([[res[sz][i]['fraction'][tc] for i in cores] for sz in valid_sizes])
    for l in losses:
        prec_tmp, = plt.plot(cores, l, '-o')
        precs.append(prec_tmp)

    plt.title('Loss Fraction vs #Cores for ' + str(tc + 1) + ' threads/core')
    plt.xlabel('#Cores')
    plt.ylabel('Loss Fraction')
    plt.legend(precs, [str(i) + 'x' + str(i) for i in valid_sizes])
    plt.xticks(cores)
    plt.show()

lol=0