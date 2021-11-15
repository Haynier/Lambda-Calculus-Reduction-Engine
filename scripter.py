import os
import sys

if len(sys.argv) < 1:
    print ('Missing input file.')
else:
    inputFiles = sys.argv

    for inputFile in inputFiles:
        os.system('python3 parser.py ' + inputFile)

        f = open("reduc.sml","r")
        og = f.read()
        f.close()

        f = open("reducable.txt","r")
        sml = f.read()
        f.close()

        f = open("reduc.sml","a")
        f.write(sml)

        os.system('sml reduc.sml')

        f.truncate(0)
        f.write(og)
        f.close()

