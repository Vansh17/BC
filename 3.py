# ************************************************ EXP 3
import numpy as np
import matplotlib.pyplot as plt
radius = 1

N = 100000   
X = np.random.uniform(low=-radius, high=radius, size=N) 
Y = np.random.uniform(low=-radius, high=radius, size=N)   


R = np.sqrt(X**2+Y**2);  

box_area =(2.0*radius)**2      
is_point_inside = R<radius
N_inside=np.sum(is_point_inside)
circle_area = box_area*N_inside/N

plt.scatter(X,Y, c=is_point_inside, s=5.0, edgecolors='none', cmap=plt.cm.Paired)  
plt.axis('equal')


print("Area of the circle = ", circle_area)
print("pi = ", circle_area/radius**2)
plt.show()