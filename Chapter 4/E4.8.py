import numpy as np

import matplotlib.pyplot as plt

lr = 1 #learning rate

bias = 0.5 #value of bias

weights = [1, 0, bias] #weights generated in a list (3 weights in total for 2 neurons and the bias)

def Perceptron(input1, input2, output) :
   outputP = input1*weights[0] + input2*weights[1] + bias
   if outputP > 0 : #activation function (here Heaviside)
      outputP = 1
   else :
      outputP = 0
   error = output - outputP
   weights[0] += error * input1 * lr
   weights[1] += error * input2 * lr
   weights[2] += error




for i in range(2) :
     Perceptron(-1,-1,0)#p_1
     Perceptron(0,0,0)  #p_2
     Perceptron(-1,1,1) #p_3

print('Final weights : ', weights,'\n')

if weights[1] == 0:
    y = np.linspace(-2,2,10)
    x = (-weights[1]*y - weights[2]) / weights[0]

else :
        x = np.linspace(-5,5,10)
        y = (-weights[0]*x - weights[2]) / weights[1]

plt.scatter(-1, -1, color='red')
plt.scatter(0, 0, color='red')
plt.scatter(-1, 1, color='blue')

plt.plot(x, y, '-r', label='g(x)')
plt.title('Final decision boundary')
plt.xlabel('x', color='#1C2833')
plt.ylabel('y', color='#1C2833')
plt.legend(loc='upper left')
plt.grid()
plt.show()
