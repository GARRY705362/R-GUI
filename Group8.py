import subprocess
a=input("Enter your name to proceed with analysis: ")
if (a=="gaurav"):
    #subprocess.call("D:\\scripting project\\library.R", shell=True)
    #subprocess.call("D:\\scripting project\\Net.R", shell=True)
    subprocess.call("D:\\scripting project\\Gaurav.R", shell=True)
elif(a=="naveen"):
    subprocess.call("D:\scripting project\\Naveen.R", shell=True)
elif (a=="seema"):
    subprocess.call("D:\scripting project\\Seema.R", shell=True)
elif(a=="goutham"):
    subprocess.call("D:\scripting project\\Goutham.R", shell=True)
else:
    print("Invalid User")
    
    
    
