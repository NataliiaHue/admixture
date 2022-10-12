#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May 25 15:06:52 2021

@author: huebler
"""
clusterOutput = list()
clusterOutput.append("Run;K;sample;LnProbData;meanLnLikelihood;varianceLnLikelihood;meanAlpha")
class Input_Container:
    def __init__(self):
        self.run=0
        self.k=0
        self.sample="test"
        self.lnProb=0.0
        self.meanLikely=0.0
        self.varianceLikely=0.0
        self.meanAlpha=0.0
        self.lines = []
    def set_Sample(self, sample):
        self.sample=sample
    def set_K(self, k):
        self.k =k
    def set_run(self, run):
        self.run=run
    def add_lnProb(self, lnProb):
        self.lnProb = lnProb
    def add_meanLikely(self, meanLikely):
        self.meanLikely = meanLikely
    def add_varianceLikely(self, varianceLikely):
        self.varianceLikely = varianceLikely
    def add_meanAlpha(self, meanAlpha):
        self.meanAlpha = meanAlpha
    def add_Line(self, line):
        self.lines.append(line)
    
def write_output_ancestry_table(ic,directory):
    #write output matrix for file
    try:
        f = open(directory, "w")
        for line in ic.lines:
            f.write(line)
    finally:    
        f.close()

def add_output_likelihood_table(ic):
    line = str(ic.run)+";"+str(ic.k)+";"+ic.sample+";"+str(ic.lnProb)+";"+str(ic.meanLikely)+";"+str(ic.varianceLikely)+";"+str(ic.meanAlpha)
    clusterOutput.append(line)
    
def go_through_file(filename):
   try:
       print(filename)
       with open(filename,'r') as fp:
           isTargetArray=False
           ic = Input_Container()
           line = fp.readline()
           name =filename.split("/")[-1]
           ic.set_Sample(name.split("sample_")[1].strip())
           ic.set_K(int(name.split("_")[3][1:]))
           ic.set_run(int(name.split("_")[4][3:]))
           while line:
          
               if "Estimated Ln Prob of Data" in line:
                   ic.add_lnProb(float(line.split('=')[1].strip()))
               if "Mean value of ln likelihood " in line:
                   ic.add_meanLikely(float(line.split('=')[1].strip()))
               if "Variance of ln likelihood" in line:
                   ic.add_varianceLikely(float(line.split('=')[1].strip()))
               if "Mean value of alpha" in line:
                   ic.add_meanAlpha(float(line.split('=')[1].strip()))  
               if "Inferred ancestry of individuals:" in line:
                    isTargetArray= True
               if isTargetArray:
                   if "Estimated Allele Frequencies in each cluster" in line:
                       isTargetArray=False
                       break
                   ic.add_Line(line)
               line = fp.readline()
   finally:
       fp.close()
       add_output_likelihood_table(ic)
       #write_output_ancestry_table(ic,"/Users/huebler/Desktop/phonology_K2-K8_matrices/"+name)

import os
directory = "/Applications/structure_console/outfiles_samples/"
files = os.listdir(directory)
for filename in files:
    if filename.find("outfile.txt" )== -1:
        go_through_file(directory+filename)
    
#writeo output for directory
try:
    f = open(directory+"outfile.txt", "w")
    for line in clusterOutput:
        f.write(line+"\n")
finally:    
    f.close()
