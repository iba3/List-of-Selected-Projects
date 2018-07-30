import copy
import glob
import math
import os
import sys

# Making Airfoil class
class Airfoil(object):
    """This creates an Airfoil class. This class computes the following 
    properties and associates it with an airfoil object: chord length,
    lift coefficient, and stagnation point. The class needs an inputdir
    as an input which will have the x and y coordinates of the airfoil 
    and the pressure coefficients for each panel of the airfoil at a 
    particular angle of attack."""

    def __init__(self,inputdir):
        """Initialization of class airfoil object"""
 
        #Creating self.variables that will be reused within Airfoil class
        self.__xylist=[]
        self.__chordlength=0
        self.__cl={}
        self.__stag={}
        self.__alpha2cp = {}
        self.__dirname=str(inputdir)
        self.load_name_data(inputdir)
        

    def load_name_data(self,inputdir):
        """Loading inputdir, assigning variables, and creating empty dictionaries"""

        if not os.path.isdir(inputdir):
            raise RuntimeError('Not a valid directory')

        fxy = open(os.path.join(inputdir, 'xy.dat'))

        if not os.path.isfile(os.path.join(inputdir, 'xy.dat')):
            raise RuntimeError("XY file not found. Needs to be of format: 'xy.dat' ")
    
        file_paths = glob.glob(os.path.join(inputdir, 'alpha*.dat'))
        
        if file_paths == []:
            raise RuntimeError("The required data files are not in the directory")

        # Opening all alpha files
        for file in file_paths:
            alpha = float(os.path.split(file)[-1][5:-4])
            self.__alpha2cp[alpha] = []
            f_alpha = open(file)
            
            # Converting each alpha into a string and appending to alpha2cp matrix
            for line in f_alpha.readlines()[1:]:
                cp = float(line.strip().split()[0])
                self.__alpha2cp[alpha].append(cp)
                if len(line.strip().split()) != 1:
                    raise RuntimeError("The file is missing a pressure coefficient")

            f_alpha.close()

        # Creating an XY list that stores each x,y as a set 
        for line in fxy.readlines()[1:]:
            linelist=line.split()
            xval=float(linelist[0])
            yval=float(linelist[1])
            self.__xylist.append((xval, yval))
            if len(linelist) != 2:
                raise RuntimeError ('XY.Dat file is missing an x or y entry')
        
       # Closing fxy files
        fxy.close() 

    def chordLength(self):
        """Finding chordlength of input directory. 
           Defined as distance from leading and trailing edge"""

        xmin=min(self.__xylist)
        xmax=max(self.__xylist)
        self.__chordlength=math.hypot(xmax[0]-xmin[0],xmax[1]-xmin[1])
        
    def get_chordLength(self):
        """User interactive function to get chordlength of Airfoil abject"""

        return copy.deepcopy(self.__chordlength)

    
    def clift(self):
        '''Calculates the lift coefficient for each angle of attack'''

        for keys,values in self.__alpha2cp.items():
            CX=0
            CY=0

            # Creating inner for loop that iterates through list of XY
            for index,xyvals in enumerate(self.__xylist[:-1]):
                dx=self.__xylist[index+1][0]-xyvals[0]
                dy=self.__xylist[index+1][1]-xyvals[1]
                dCX=-(dy*values[index]/self.__chordlength)
                dCY=(dx*values[index]/self.__chordlength)
                CX += dCX
                CY += dCY

            # Finding lift coefficient and adding its value to dictionary
            LC=CY*math.cos(math.radians(keys))-CX*math.sin(math.radians(keys))
            self.__cl[keys]=LC
           
        
    def get_cl(self):
        """Returns lift coefficient"""
        
        return copy.deepcopy(self.__cl)

    def stagpoint(self):
        """ Finds stagnation point for each angle of attack"""
 
        # Uses for loop to find highest cp and corresponding avg stagpoint
        for keys,values in self.__alpha2cp.items():
            maxPrescoef=max(values)
            ind=values.index(maxPrescoef)
            stgpt1=self.__xylist[ind]
            stgpt2=self.__xylist[ind+1]
            stagpointavg=((stgpt1[0]+stgpt2[0])/2,(stgpt1[1]+stgpt2[1])/2)
            self.__stag[keys]=(stagpointavg,maxPrescoef)

    def get_stagpoint(self):
        """Returns stagnation point for each angle of attack"""        

        return copy.deepcopy(self.__stag)

    def __repr__(self):

        #Runs class methods to create output for each object
        self.chordLength()
        self.clift()
        self.stagpoint()
        nameofdir=self.__dirname.upper()
        string = "Test case: {} {} \n \n ".format(nameofdir[0:4],nameofdir[4:8])
        string += " alpha       cl              stagnation pt\n"
        string += "  _____    ______     ______________________________\n"
        
        for keys,values in sorted(self.__cl.items()):

            stgx=self.__stag[keys][0][0]
            stgy=self.__stag[keys][0][1]
            stgp=self.__stag[keys][1]
             
            string += "%7.2f " % (keys)
            string += "  %7.4f    " % (values)
            string += " (%7.4f,  %7.4f)   %7.4f \n" % (stgx,stgy,stgp)

        return string

