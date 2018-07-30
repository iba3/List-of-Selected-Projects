import math
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
import scipy.sparse
import scipy.sparse.linalg
import sys
import warnings

np.set_printoptions(precision=3,linewidth=180)

class Truss:
    """This Truss class solves for the beam forces of a specified truss. 
    The arguments are a jointsfile and a beamsfile. An optional argument
    is the outputfile name for plotting the truss. This class also identified
    under-determined and over-determined systems"""
 
    def __init__(self, jointsfile,beamsfile,plotoutputfile=None):
        """Initialization method that calls load function"""
        
        self.plotoutputfile=str(plotoutputfile)
        self.load_data(jointsfile,beamsfile)

    def load_data(self,jointsfile,beamsfile):
        """Loads in the various input files into numpy tables
        and creates self variables"""

        self.jointsxy=np.loadtxt(jointsfile,dtype=np.float64,usecols=(0,1,2))
        self.jointsfxy=np.loadtxt(jointsfile,dtype=np.float64,usecols=(3,4))
        self.jointsd=np.loadtxt(jointsfile,dtype=np.int64,usecols=(5,))
        self.beams=np.loadtxt(beamsfile,dtype=np.int64,usecols=(1,2))
        
        # Creating empty lists for self data 
        self.row=[]
        self.col=[]
        self.data=[]
        self.b=[]
        self.beamForces=[]
        numberJoints=self.jointsxy.shape[0]
        shapeSpa=(numberJoints*2,numberJoints*2)
        self.rowarray=np.empty(shape=shapeSpa)
        self.colarray=np.empty(shape=shapeSpa)
        self.dataarray=np.empty(shape=shapeSpa)
        self.barray=np.empty(shape=(numberJoints*2,1))

    def loadMatrices(self):
        """Creating sparse matrixes and solution array in order to
           solve. Each sparse matrix is filled with the coefficients
           (cos,sin or 1) in order to correspond with each unknown
           force"""

        # Creating column indexes for csr sparse format
        for i,row in enumerate(self.beams):
            self.col.extend(i for ind in range(4))

            # Creating row indices for csr sparse format
            j1=row[0]
            j2=row[1]
            j1starti=(j1-1)*2
            j2starti=(j2-1)*2
            self.row.extend([j1starti,j1starti+1,j2starti,j2starti+1])

            # Filling in data list with sin and cos values   
            cosVal=self.cosFactor(j1,j2)
            sinVal=self.sinFactor(j1,j2)
            self.data.extend([cosVal,sinVal,-cosVal,-sinVal])
        
        # Filling in the RX,RY in sparse matrix
        for i,val in enumerate(self.jointsd):
            if val != 0:
                colnum=max(self.col)+1
                self.col.extend([colnum,colnum+1])
                rownum=2*i
                self.row.extend([rownum,rownum+1])
                self.data.extend(1 for ind in range(2))

        # Converting lists into numpy array
        self.rowarray=np.array(self.row,dtype=np.int64)
        self.colarray=np.array(self.col,dtype=np.int64)
        self.dataarray=np.array(self.data,dtype=np.float64)

        # Creating sparse matrix    
        self.sparseM=scipy.sparse.csr_matrix((self.dataarray,(self.rowarray,self.colarray)))

        # Creating B Matrix
        for i,val in enumerate(self.jointsfxy):
            self.b.extend([val[0],val[1]])
        
        self.barray=np.array(self.b,dtype=np.float64)

        # Raising Runtime Error by checking the amount of equations vs unknowns
        unknowncount=2*np.sum(self.jointsd)+self.beams.shape[0]
        check=2*self.jointsxy.shape[0]

        if check < unknowncount:
            raise RuntimeError ('Truss geometry not suitable for static equilibrium analysis')

    def getSparseMatrix(self):
        """Allows user to view sparse matrix format"""

        sparseMCopy=self.sparseM.copy().todense()
        return sparseMCopy

    def getRHS(self):
        """Allows user to view rhs filled with external forces"""

        return self.barray.copy()

    def xSolver(self):
        """Obtains solution to 2D truss system"""
        
        # Catches warnings into error for unstable truss 
        warnings.filterwarnings('error')
        warnings.filterwarnings('ignore',category=DeprecationWarning)
        
        # Solves the solution to the 2D truss sytem
        try:
            self.x=scipy.sparse.linalg.spsolve(self.sparseM,self.barray)
        except Warning:
            raise RuntimeError('Cannot solve the linear system, unstable truss?')
 
    def beamForce(self):
        """Creating list of beam forces"""
  
        for i,force in enumerate(self.x):
            if i<len(self.beams):
                self.beamForces.append(force)

    def cosFactor(self,joint1,joint2):
        """Generating cosine coefficient factors"""

        x1=self.jointsxy[joint1-1,1]
        y1=self.jointsxy[joint1-1,2]
        x2=self.jointsxy[joint2-1,1]
        y2=self.jointsxy[joint2-1,2]
        dx=x2-x1
        hypdis=math.hypot(x2-x1,y2-y1)
        cosf=dx/hypdis
        return cosf

    def sinFactor(self,joint1,joint2):
        """Generating sine coefficient factors"""

        x1=self.jointsxy[joint1-1,1]
        y1=self.jointsxy[joint1-1,2]
        x2=self.jointsxy[joint2-1,1]
        y2=self.jointsxy[joint2-1,2]
        dy=y2-y1
        hypdis=math.hypot(x2-x1,y2-y1)
        sinf=dy/hypdis
        return sinf
       
    def PlotGeometry(self,plotoutputfile):
        """Plotting geometry of truss"""
        
        # Checking for optional third argument
        if self.plotoutputfile=="None":
            pass
        
        # Plotting/saving figure in specified file
        else:
            plt.figure(1)
            for i,val in enumerate(self.beams):
                ja=val[0]
                jb=val[1]
                x1=self.jointsxy[ja-1,1]
                y1=self.jointsxy[ja-1,2]
                x2=self.jointsxy[jb-1,1]
                y2=self.jointsxy[jb-1,2]
                plt.plot((x1,x2),(y1,y2))
                matplotlib.pyplot.axes().set_aspect('equal','datalim')
                plt.show()
                plt.savefig(self.plotoutputfile)

    def __repr__(self):
        """Class representation method to print out the
           beam number and force"""

        self.loadMatrices()
        self.xSolver()
        self.beamForce()
        self.PlotGeometry(self.plotoutputfile)
        string='Beam       Force\n'
        string+='________________\n'
        for i, val in enumerate(self.beamForces):
            string+='   %d   %9.3f\n' % (i+1,val)
        return string

